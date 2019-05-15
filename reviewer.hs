{-#LANGUAGE OverloadedStrings #-}
-- Merge Request Check

import Network.HTTP.Client
import Data.Aeson as Aeson
import Data.Maybe
import Data.Aeson.Types
import Data.List.NonEmpty
import Network.HTTP.Types.Status (statusCode)
import System.Process as Process
import System.Posix.Unistd

type MergeRequestId = Int
type MergeRequestState = String

gitUrl :: String
gitUrl = "http://gpon.git.com:8011/api/v4/projects/34/merge_requests?private_token=D_-yvMKXNJcqpQxZr_CU"

-- unit is second
discoveryInterval :: Int
discoveryInterval = 300

main = do
  -- Http client manager
  manager <- newManager defaultManagerSettings
  msg <- daemon_main manager gitUrl
  print msg

daemon_main :: Manager -> String -> IO String
daemon_main manager url = do
  openMerges <- discovery_until manager url discoveryInterval

  return "MergeRequest Reviewer exit normally."

-- discovery_until will block until discovery open merge request
discovery_until :: Manager -> String -> Int -> IO [(MergeRequestId, MergeRequestState)]
discovery_until manager url interval = do
  openStates <- discovery manager url
  if openStates /= []
    then return openStates
    else do num <- sleep 5
            discovery_until manager url interval

-- discovery will return list of tuple that represent open merge requests
discovery :: Manager -> String -> IO [(MergeRequestId, MergeRequestState)]
discovery manager url = do
  request <- parseRequest url
  response <- httpLbs request manager

  let parsedJson = decode (responseBody response) :: Maybe [Object]
  if parsedJson == Nothing
    then return []
    else let mergeReqStates = [ fromJust $ mergeReqStateParse x | x <- (fromJust parsedJson)]
             openMerges = [ x | x <- mergeReqStates, snd x == "opened" ]
         in return openMerges

mergeReqStateParse :: Object -> Maybe (MergeRequestId, MergeRequestState)
mergeReqStateParse o = flip parseMaybe o $ \obj -> do
                  iid <- obj .: "iid"
                  state <- obj .: "state"
                  return (iid, state)

dispatcher :: [(MergeRequestId, MergeRequestState)] -> Int

data StageId = Prepare | Retrive | Build | Compile | Test deriving(Eq, Show)
instance Enum StageId where
  fromEnum Prepare = 0
  fromEnum Retrive = 1
  fromEnum Build = 2
  fromEnum Compile = 3
  fromEnum Test = 4

  toEnum 0 = Prepare
  toEnum 1 = Retrive
  toEnum 2 = Build
  toEnum 3 = Compile
  toEnum 4 = Test
  toEnum otherwise = error "key error"

data StageInfo = StageInfo { url :: String }

reviewFlow :: (MergeRequestId, MergeRequestState) -> Int

stage :: StageId -> StageInfo -> Int
stage id info
  | id == Prepare = stage_prepare info

stage_prepare :: StageInfo -> Int
stage_retrive :: StageInfo -> Int
stage_build :: StageInfo -> Int
stage_compile :: StageInfo -> Int
stage_test :: StageInfo -> Int
