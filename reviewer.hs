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
import Control.Exception

import Modules.ConfigReader
import List.Transformer

type MergeRequestId = Int
type MergeRequestState = String
type MergeReqInfo = (MergeRequestId, MergeRequestState)

type ProjName = String
type ProjApi  = String
type ProjUrl  = String
data ProjInfo = ProjInfo {
  projName :: ProjName,
  projApi  :: ProjApi,
  projUrl :: ProjUrl
}

projInfoGetName :: ProjInfo -> ProjName
projInfoGetName info = projName info
projInfoGetApi :: ProjInfo -> ProjApi
projInfoGetApi info = projApi info
projInfoGetUrl :: ProjInfo -> ProjUrl
projInfoGetUrl info = projUrl info

data DispatchInfo = DispatchInfo {
  baseDir_dis      :: String,
  projectName_dis  :: String,
  workInfo_dis     :: (ProjUrl, [MergeReqInfo]),
  tempDirPath_dis  :: String,
  manager_dis      :: Manager,
  acceptApi_dis    :: String
}

dispatchGetUrl :: DispatchInfo -> ProjUrl
dispatchGetUrl info = fst $ workInfo_dis info

dispatchGetMergeReqInfos :: DispatchInfo -> [MergeReqInfo]
dispatchGetMergeReqInfos info = snd $ workInfo_dis info

dispatchGetTempPath :: DispatchInfo -> String
dispatchGetTempPath info = tempDirPath_dis info

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

data StageInfo = StageInfo {
  baseDir    :: String,
  projectName :: String,
  repoUrl    :: String,
  mergeId    :: Int,
  mergeState :: String,
  workingDir :: String,
  manager    :: Manager,
  acceptApi  :: String
}

buildStagesFromDispatch :: DispatchInfo -> [StageInfo]
buildStagesFromDispatch info =
  [ StageInfo base (projectName_dis info) url (mrID x) (mrState x) dir | x <- dispatchGetMergeReqInfos info ]
  where mrID  info = fst info
        mrState info = snd info
        url = dispatchGetUrl info
        dir = dispatchGetTempPath info
        base = baseDir_dis info

configFilePath :: String
configFilePath = "./Config.txt"

scriptDirPath :: String
scriptDirPath = "./scripts"

tempDirPath :: String
tempDirPath = "/home/aydenlin/temp"

-- unit is ms
discoveryInterval :: Int
discoveryInterval = 300000

main = do
  -- Spawn http client manager
  manager <- newManager defaultManagerSettings

  -- Configuration file load and parse
  configs <- loadConfig configFilePath

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
  baseDirPath <- getCurrentDirectory
  msg <- reviewProcess manager baseDirPath windPath tempDirPath projInfos
  return msg
  where projs = listProjConfig configs
        projInfos  = [ ProjInfo x (getAccess x) (getUrl x) | x <- (fromJust projs) ]
        getAccess x = fromJust $ mrAccessApiConfig x configs
        getUrl    x = fromJust $ sourceUrlConfig x configs
        tempDirPath = fromJust $ tempDirPathConfig configs

reviewProcess :: Manager -> String -> String -> String -> [ProjInfo] -> IO String
reviewProcess m base acceptApi tempDirPath info = do
  openMrs <- openMrDiscovery
  if openMrs /= []
    then do codes <- dispatcher $ [DispatchInfo base (fst x) (snd x) tempDirPath m  | x <- openMrs ]
            if elem Code_error codes
              then return "Error"
              else do num <- threadDelay discoveryInterval
                      reviewProcess m base acceptApi tempDirPath info
    else do num <- threadDelay discoveryInterval
            reviewProcess m base acceptApi tempDirPath info

  where accessApis = [ (projInfoGetName x, (projInfoGetUrl x, projInfoGetApi x)) | x <- info ]
        openMrDiscovery = do
          openMrs <- fold (flip (:)) [] id (openMrDiscovery_helper m accessApis)
          return $ Prelude.filter ((/=[]) . snd . snd) openMrs
        openMrDiscovery_helper m [] = List.Transformer.empty
        openMrDiscovery_helper m (x:xs) = ListT $ do
          opens <- liftIO $ discovery m (snd . snd $ x)
          if opens == []
            then return (Cons (fst x, (fst . snd $ x, [])) (openMrDiscovery_helper m xs))
            else return (Cons (fst x, (fst . snd $ x, opens)) (openMrDiscovery_helper m xs))

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

dispatcher :: [DispatchInfo] -> IO [ErrorCode]
dispatcher [] = return (Code_ok:[])
dispatcher (x:xs) = do
  codes <- dispatcher_helper $ buildStagesFromDispatch x
  codes_ <- dispatcher xs
  return (codes ++ codes_)

  where dispatcher_helper [] = return (Code_ok:[])
        dispatcher_helper (x:xs)  = do
          code <- taskSpawn x
          codes <- dispatcher_helper xs
          return (code:codes)

taskSpawn :: StageInfo -> IO ErrorCode
taskSpawn tArg = runEval $ do
    rpar (reviewFlow tArg)

reviewFlow :: StageInfo -> IO ErrorCode
reviewFlow info = do
  -- First check is argument valid
  if mergeState info /= "opened"
    then return Code_error
    else stages info

stages :: StageInfo -> IO ErrorCode
stages info = do
    x0 <- return (Code_ok, info)
    x1 <- stage Prepare x0
    x2 <- stage Retrive x1
    x3 <- stage Build   x2
    x4 <- stage Test    x3
    xx <- stage Accept  x4
    return (fst xx)

stage :: StageId -> (ErrorCode, StageInfo) -> IO (ErrorCode, StageInfo)
stage id (code, info)
    | code == Code_error = return (Code_error, info)
    | id == Prepare = stage_prepare info
    | id == Retrive = stage_retrive info
    | id == Build   = stage_build info
    | id == Test    = stage_test info
    | id == Accept  = stage_accept info

stage_script_path :: StageId -> StageInfo -> String
stage_script_path id info
  | id == Prepare = pathTemplate "prepare"
  | id == Retrive = pathTemplate "retrive"
  | id == Build   = pathTemplate "build"
  | id == Test    = pathTemplate "test"
  | id == Accept  = pathTemplate "accept"
  where pathTemplate stage = (baseDir info) ++ "/" ++
          "scripts/" ++ (projectName info) ++ "/" ++ stage

stage_script_run :: StageId -> StageInfo -> IO ErrorCode
stage_script_run id info = do
  exists <- doesFileExist script
  if exists
    then do status <- try (callProcess script [""]) :: IO (Either SomeException ())
            case status of
              Left ex -> return Code_error
              Right () -> return Code_ok
    else return Code_error
  where script = stage_script_path id info

stage_command_run :: String -> [String] -> IO Bool
stage_command_run command args = do
  status <- try (callProcess command args) :: IO (Either SomeException ())
  case status of
    Left ex -> return False
    Right () -> return True

stage_prepare :: StageInfo -> IO (ErrorCode, StageInfo)
stage_prepare info = do
    path <- mkdtemp tempDirPath
    setCurrentDirectory path
    return (Code_ok, info)

stage_retrive :: StageInfo -> IO (ErrorCode, StageInfo)
stage_retrive info = do
  status1 <- stage_command_run "git" ["clone", url, projDirName]
  case status1 of
    True -> do setCurrentDirectory $ "./" ++ projDirName
               status2 <- stage_command_run "git" ["fetch", "origin", "merge-requests/" ++
                                                    (show mrID) ++ "/head:mr-origin-" ++ (show mrID)]
               status3 <- stage_command_run "git" ["checkout", "mr-origin-" ++ (show mrID)]

               case and [status2, status3] of
                 False -> return (Code_error, info)
                 True -> return (Code_ok, info)
    False -> return (Code_error, info)

  where url = repoUrl info
        projDirName = projectName info
        mrID = force $ mergeId info

stage_build :: StageInfo -> IO (ErrorCode, StageInfo)
stage_build info = do
  status <- stage_script_run Build info
  return (status, info)

stage_test :: StageInfo -> IO (ErrorCode, StageInfo)
stage_test info = do
  status <- stage_script_run Test info
  return (status, info)

stage_accept :: StageInfo -> IO (ErrorCode, StageInfo)
stage_accept info = do

  return (status, info)
