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

data ErrorCode = Code_error | Code_ok deriving (Show, Eq)
instance Enum ErrorCode where
  fromEnum code | code == Code_ok = 0
                | code == Code_error = -1
  toEnum num | num == 0 = Code_ok
             | num == -1 = Code_error

gitUrl :: String
gitUrl = "http://gpon.git.com:8011/api/v4/projects/34/merge_requests?private_token=D_-yvMKXNJcqpQxZr_CU"

sourceUrl :: String
sourceUrl = "http://gpon.git.com:8011/gpon/olt.git"

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

-- dispatcher :: [(MergeRequestId, MergeRequestState)] -> Int

data StageId = Prepare | Retrive | Build | Compile | Test deriving(Eq, Show)

instance Enum StageId where
  fromEnum id | id == Prepare = 0
              | id == Retrive = 1
              | id == Build = 2
              | id == Compile = 3
              | id == Test = 4

  toEnum id | id == 0 = Prepare
            | id == 1 = Retrive
            | id == 2 = Build
            | id == 3 = Compile
            | id == 4 = Test
            | otherwise = error "key error"

data StageInfo = StageInfo { url   :: String,
                             id    :: Int,
                             state :: String }

reviewFlow :: (MergeRequestId, MergeRequestState) -> ErrorCode
reviewFlow (id, state) =
  -- First check is argument valid
  if state /= "opened"
    then Code_error
    else let info = StageInfo sourceUrl id state
         in  stage Prepare info
             stage Retrive info
             stage Build   info
             stage Compile info
             stage Test    info
             Code_ok

-- stage :: StageId -> StageInfo -> Int
-- stage id info
  -- | id == Prepare = stage_prepare info
  -- | id == Retrive = stage_retrive info
  -- | id == Build   = stage_build info
  -- | id == Compile = stage_compile info
  -- | id == Test    = stage_test info

-- stage_prepare :: StageInfo -> Int
-- stage_retrive :: StageInfo -> Int
-- stage_build :: StageInfo -> Int
-- stage_compile :: StageInfo -> Int
-- stage_test :: StageInfo -> Int
