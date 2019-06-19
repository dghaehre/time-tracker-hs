{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
module Main where

import System.IO
import System.Console.CmdArgs
import System.Console.CmdArgs.Implicit

import File
import Display

data Argument = 
    Display { project :: String }
  | New     { project :: String }
  | List
  | Delete  { project :: String }
  | Start   { project :: String
            , comment :: String
            }
  deriving (Show, Data, Typeable)

data Operation =
    Display' StoredData
  | New' StoredData String
  | Error' String
  | Delete' StoredData String
  | List' StoredData
  deriving (Show)

display :: Argument
display = Display { project = def &= help "Project name" &= typ "name" }

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
list = List  &= help "List out stored projects."

delete :: Argument
delete = Delete
  { project = def &= typ "<Project name>" &= argPos 0
  } &= help "Delete project"


operation :: Argument -> Either String StoredData -> Operation
operation _ (Left s)            = Error' s
operation (New s) (Right p)     = New' p s
operation List (Right p)        = List' p
operation (Delete s) (Right p)  = Delete' p s
operation _ (Right p)           = Display' p

elemName :: String -> [Project] -> Bool
elemName s [] = False 
elemName s (x:xs) 
  | File.name x == s = True
  | otherwise = elemName s xs

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
runOperation _              = return $ Err "Operation not ready"

record :: StoredData -> String -> String -> IO Result'
record d project job = do
  return Ok
   --where
    --go

main :: IO ()
main = do
  input <- cmdArgs $ modes [new, start, display, list, delete]
    &= help "Track time spent on projects" 
    &= program "tt" 
    &= summary "Time tracker v0.1"
  d <- getStoredData
  res <- runOperation $ operation input d 
  displayEnd res
