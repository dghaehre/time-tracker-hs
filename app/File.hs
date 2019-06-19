{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module File where

import            System.IO
import            GHC.Generics
import            Data.Aeson
import            Data.Aeson.Types
import            Data.Text
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
  , startTime   :: String
  , endTime     :: String
  }
  deriving (Show, Read, Generic)

instance FromJSON Job
instance ToJSON Job

data Result' =
    Ok      -- Used to let the user know everything went fine
  | Finish  -- No message to user when process is finished
  | Err String

-- |
-- | TODO: Make dynamic
projectPath = "/Users/danielhaehre/.time-tracker.json"

-- |
-- | TODO: Check if file exist and try to create file if needed
getStoredData :: IO (Either String StoredData)
getStoredData = do
  content <- B.readFile projectPath
  return $ parseContent content
      
parseContent :: B.ByteString -> Either String StoredData 
parseContent = f . decode
  where
    f (Just v) = Right v
    f Nothing = Left "Could not parse content" 

save :: StoredData -> IO ()
save = B.writeFile projectPath . encode
