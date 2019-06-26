{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
module Main where

import System.IO
import System.Console.CmdArgs
import System.Console.CmdArgs.Implicit
import Data.Maybe

import File
import Display

data Argument = 
    Display { project :: String }
  | New     { project :: String }
  | Today   { project :: String }
  | Week    { project :: String }
  | List
  | Delete  { project :: String }
  | Start   { project :: String
            , comment :: String
            }
  deriving (Show, Data, Typeable)

data Operation =
    Display' StoredData
  | New' StoredData String
  | Today' StoredData String --
  | Week' StoredData String --
  | Error' String
  | Delete' StoredData String
  | List' StoredData
  deriving (Show)

display :: Argument
display = Display { project = def &= help "Project name" &= typ "name" }

today :: Argument
today = Today { project = def &= help "Project name" &= typ "name" }

week :: Argument
week = Week { project = def &= help "Project name" &= typ "name" }

new :: Argument
new = New 
  { project = def &= typ "<Project name>" &= argPos 0
  } &= help "Create new project"

start :: Argument
start = Start 
  { project = def &= typ "<Project name>" &= argPos 0
  , comment = def &= help "Comment" &= typ "comment" 
  }

list :: Argument
list = List &= help "List out stored projects."

delete :: Argument
delete = Delete
  { project = def &= typ "<Project name>" &= argPos 0
  } &= help "Delete project"

operation :: Argument -> Either String StoredData -> Operation
operation _ (Left s)            = Error' s
operation (New s) (Right p)     = New' p s
operation List (Right p)        = List' p
operation (Delete s) (Right p)  = Delete' p s
operation (Today s) (Right p)   = Today' p s
operation (Week s) (Right p)    = Week' p s
operation _ (Right p)           = Display' p

elemName :: String -> [Project] -> Bool
elemName s [] = False 
elemName s (x:xs) 
  | File.name x == s = True
  | otherwise = elemName s xs

getProjects :: String -> StoredData -> Maybe [Project]
getProjects p d
  | length byName > 0 = Just byName
  | p /= ""           = Nothing
  | otherwise         = Just $ projects d
  where
    byName = filter (\x -> File.name x == p) $ projects d

-- TODO
filterToday :: String -> String -> Bool
filterToday start end = True

-- TODO
filterWeek :: String -> String -> Bool
filterWeek start end = True

maybe' :: (a -> b) -> Maybe a -> Maybe b
maybe' f a
  | isJust a  = Just $ f $ fromJust a
  | otherwise = Nothing

getByTime :: (String -> String -> Bool) -> String -> StoredData -> Maybe [Project]
getByTime f n = maybe' (filter noJobs . map (filterJobs f)) . getProjects n
  where
    noJobs x = 0 < length (File.jobs x)

-- Remove jobs that is not relevant
filterJobs :: (String -> String -> Bool) -> Project -> Project
filterJobs f p = File.Project n filteredJobs
  where
    n             = File.name p
    filteredJobs  = filter isRelevant $ jobs p
    isRelevant j  = f (File.startTime j) (File.endTime j)

createNew :: StoredData -> String -> IO Result'
createNew d p 
  | elemName p (projects d) = return (Err "Project already exist")
  | otherwise = do
    _ <- save $ StoredData $ projects d ++ [ Project p [] ]
    return Ok

deleteProject :: StoredData -> String -> IO Result'
deleteProject d p
  | not exist = return (Err "Project already exist")
  | otherwise = do
    _ <- save $ StoredData $ filter remove $ projects d 
    return Ok
      where
        exist = elemName p (projects d)
        remove x = File.name x /= p

runOperation :: Operation -> IO Result'
runOperation (New' d s)     = createNew d s
runOperation (List' d)      = listProjects d
runOperation (Delete' d s)  = deleteProject d s
runOperation (Today' d s)   = displayToday $ getByTime filterToday s d
runOperation (Week' d s)    = displayWeek $ getByTime filterWeek s d
runOperation _              = return $ Err "Operation not ready"

record :: StoredData -> String -> String -> IO Result'
record d project job = do
  return $ Err "Operation not ready"
   --where
    --go

main :: IO ()
main = do
  input <- cmdArgs $ modes [new, start, display, list, delete, today, week]
    &= help "Track time spent on projects" 
    &= program "tt" 
    &= summary "Time tracker v0.1"
  d <- getStoredData
  res <- runOperation $ operation input d 
  displayEnd res
