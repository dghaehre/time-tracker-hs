{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module File where

import            GHC.Generics
import            Data.Aeson
import            System.Directory
import            Data.Time
import qualified  Data.ByteString.Lazy as B

data StoredData = StoredData
  { projects  :: [Project] }
  deriving (Show, Read, Generic)

instance FromJSON StoredData
instance ToJSON StoredData

data Project = Project
  { name  :: String
  , jobs  :: [Job]
  }
  deriving (Show, Read, Generic)

instance FromJSON Project
instance ToJSON Project

data Job = Job
  { description :: String
  , startTime   :: UTCTime
  , endTime     :: UTCTime
  }
  deriving (Show, Read, Generic)

instance FromJSON Job
instance ToJSON Job

data Result' =
    Ok      -- Used to let the user know everything went fine
  | Finish  -- No message to user when process is finished
  | Err String

data FileError =
    NotFound
  | Other String

projectPath :: IO FilePath
projectPath = do
  h <- getHomeDirectory
  return $ h ++ "/.time-tracker.json"

getStoredData :: IO (Either FileError StoredData)
getStoredData = do
  p <- projectPath
  b <- doesFileExist p
  go b p
    where
      go :: Bool -> FilePath -> IO (Either FileError StoredData)
      go False _ = return $ Left NotFound
      go _ p' = do
        content <- B.readFile p'
        return $ case eitherDecode content of
          Left s -> Left (Other s)
          Right s -> Right s
      
createFile :: IO Result'
createFile = do
  p <- projectPath
  writeFile p "{\"projects\":[]}"
  return Ok

save :: StoredData -> IO ()
save d = do
  p <- projectPath
  B.writeFile p $ encode d
