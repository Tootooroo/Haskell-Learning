{-#LANGUAGE OverloadedStrings #-}
-- Merge Request Check

import Debug.Trace

import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

import Data.Aeson as Aeson
import Data.Maybe
import Data.Aeson.Types
import Data.List.NonEmpty
import Data.List.Split
import Data.Either

import System.Process as Process
import System.Posix.Unistd
import System.Unix.Directory
import System.Directory
import System.IO

import Control.Parallel.Strategies
import Control.Concurrent
import Control.DeepSeq
import Control.Monad.IO.Class

import Modules.ConfigReader
import List.Transformer

type MergeRequestId = Int
type MergeRequestState = String
type MergeReqInfo = (MergeRequestId, MergeRequestState)

type ProjName = String
type ProjApi  = String
type ProjUrl  = String
type ProjInfo = (ProjName, ProjApi, ProjUrl)

projInfoGetName :: ProjInfo -> ProjName
projInfoGetName (n, a, u) = n
projInfoGetApi :: ProjInfo -> ProjApi
projInfoGetApi (n, a, u) = a
projInfoGetUrl :: ProjInfo -> ProjUrl
projInfoGetUrl (n, a, u) = u

data ErrorCode = Code_error | Code_ok deriving (Show, Eq)
instance Enum ErrorCode where
  fromEnum code | code == Code_ok = 0
                | code == Code_error = -1
  toEnum num | num == 0 = Code_ok
             | num == -1 = Code_error

data StageId = Prepare | Retrive | Build | Test | Accept deriving(Eq, Show)

instance Enum StageId where
  fromEnum id | id == Prepare = 0
              | id == Retrive = 1
              | id == Build = 2
              | id == Test = 3
              | id == Accept = 4

  toEnum id | id == 0 = Prepare
            | id == 1 = Retrive
            | id == 2 = Build
            | id == 3 = Test
            | id == 4 = Accept
            | otherwise = error "key error"

data StageInfo = StageInfo { repoUrl    :: String,
                             mergeId    :: Int,
                             mergeState :: String,
                             workingDir :: String }

configFilePath :: String
configFilePath = "./Config.txt"

scriptDirPath :: String
scriptDirPath = "./scripts"

tempDirPath :: String
tempDirPath = "/home/aydenlin/"

gitUrl :: String
gitUrl = "http://gpon.git.com:8011/api/v4/projects/34/merge_requests?private_token=D_-yvMKXNJcqpQxZr_CU"

sourceUrl :: String
sourceUrl = "http://gpon.git.com:8011/gpon/olt.git"

-- unit is ms
discoveryInterval :: Int
discoveryInterval = 300000

main = do
  -- Spawn http client manager
  manager <- newManager defaultManagerSettings

  -- Configuration file load and parse
  configs <- loadConfig configFilePath
  print configs

  -- Daemon boot
  msg <- daemon_boot manager configs
  print msg

loadConfig :: String -> IO [[String]]
loadConfig configPath = do
  handle <- openFile configPath ReadMode
  contents <- hGetContents handle

  let config = fromRight (("error":[]):[]) $ parseConfig contents
  return config

daemon_boot :: Manager -> [[String]] -> IO String
daemon_boot manager configs = do
  msg <- reviewProcess manager windPath tempDirPath projInfos
  return msg
  where projs = listProjConfig configs
        projInfos  = [ (x, getAccess x, getUrl x) | x <- (fromJust projs) ]
        getAccess x = fromJust $ mrAccessApiConfig x configs
        getUrl    x = fromJust $ sourceUrlConfig x configs
        windPath = fromJust $ windRiverPathConfig configs
        tempDirPath = fromJust $ tempDirPathConfig configs

reviewProcess :: Manager -> String -> String -> [ProjInfo] -> IO String
reviewProcess m windRiverPath tempDirPath info = do
  openMrs <- openMrDiscovery
  codes <- dispatcher openMrs
  if elem Code_error codes
    then return "Error"
    else do num <- threadDelay discoveryInterval
            reviewProcess m windRiverPath tempDirPath info

  where accessApis = [ (projInfoGetUrl x, projInfoGetApi x) | x <- info ]
        openMrDiscovery = fold (flip (:)) [] id (openMrDiscovery_ m accessApis)

openMrDiscovery_ :: Manager -> [(String, String)] -> ListT IO (ProjUrl, [MergeReqInfo])
openMrDiscovery_ m [] = List.Transformer.empty
openMrDiscovery_ m (x:xs) = ListT $ do
  opens <- liftIO $ discovery m (snd x)
  if opens == []
    then return (Cons (fst x, []) (openMrDiscovery_ m xs))
    else return (Cons (fst x, opens) (openMrDiscovery_ m xs))

-- discovery_until will block until discovery open merge request
discovery_until :: Manager -> String -> Int -> IO [MergeReqInfo]
discovery_until manager url interval = do
  openStates <- discovery manager url
  if openStates /= []
    then return openStates
    else do num <- threadDelay 5
            discovery_until manager url interval

-- discovery will return list of tuple that represent open merge requests
discovery :: Manager -> String -> IO [MergeReqInfo]
discovery manager url = do
  request <- parseRequest url
  response <- httpLbs request manager

  let parsedJson = decode (responseBody response) :: Maybe [Object]
  if parsedJson == Nothing
    then return []
    else let mergeReqStates = [ fromJust $ mergeReqStateParse x | x <- (fromJust parsedJson) ]
             openMerges = Prelude.filter ((== "opened") . snd) mergeReqStates
         in return openMerges

mergeReqStateParse :: Object -> Maybe MergeReqInfo
mergeReqStateParse o = flip parseMaybe o $ \obj -> do
                  iid <- obj .: "iid"
                  state <- obj .: "state"
                  return (iid, state)

dispatcher :: [(ProjUrl, [MergeReqInfo])] -> IO [ErrorCode]
dispatcher [] = return (Code_ok:[])
dispatcher (x:xs) = do
  let mrPair = [ (fst x, y) | y <- (snd x) ]
  codes <- dispatcher_helper mrPair
  codes_ <- dispatcher xs
  return (codes ++ codes_)
  where mrList = snd x
        dispatcher_helper [] = return (Code_ok:[])
        dispatcher_helper (x:xs)  = do
          code <- taskSpawn x
          codes <- dispatcher_helper xs
          return (code:codes)

taskSpawn :: (ProjUrl, MergeReqInfo) -> IO ErrorCode
taskSpawn tArg = runEval $ do
    rpar (reviewFlow tArg)

reviewFlow :: (ProjUrl, MergeReqInfo) -> IO ErrorCode
reviewFlow (url, merInfo) = do
  -- First check is argument valid
  if snd merInfo /= "opened"
    then return Code_error
    else let info = StageInfo url (fst merInfo) (snd merInfo) ""
         in  stages info

stages :: StageInfo -> IO ErrorCode
stages info = do
    x0 <- return (Code_ok, info)
    x1 <- stage Prepare x0
    x2 <- stage Retrive x1
    x3 <- stage Build   x2
    xx <- stage Test    x3
    return (fst xx)

stage :: StageId -> (ErrorCode, StageInfo) -> IO (ErrorCode, StageInfo)
stage id (code, info)
    | code == Code_error = return (Code_error, info)
    | id == Prepare = stage_prepare info
    | id == Retrive = stage_retrive info
    | id == Build   = stage_build info
    | id == Test    = stage_test info

stage_prepare :: StageInfo -> IO (ErrorCode, StageInfo)
stage_prepare info = do
    path <- mkdtemp tempDirPath
    setCurrentDirectory path
    return (Code_ok, StageInfo (repoUrl info) (mergeId info) (mergeState info) path)

stage_retrive :: StageInfo -> IO (ErrorCode, StageInfo)
stage_retrive info = do
    callProcess "." ["git clone ", repoUrl info]
    return (Code_ok, info)

stage_build :: StageInfo -> IO (ErrorCode, StageInfo)
stage_build info = do
    setCurrentDirectory $ workingDir info ++ "olt/GBN/src/"
    callProcess "gcom_gpon_build_boot.bat" ["C:\\BuildTool"]
    callProcess "gcom_gpon_build_boot.bat" ["C:\\BuildTool"]
    return (Code_ok, info)

stage_test :: StageInfo -> IO (ErrorCode, StageInfo)
stage_test info = do
    return (Code_ok, info)

stage_accept :: StageInfo -> IO (ErrorCode, StageInfo)
stage_accept info = do
    return (Code_ok, info)
