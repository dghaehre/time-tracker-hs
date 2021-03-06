{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
module Main where

import System.IO
import System.Console.CmdArgs
import Control.Concurrent
import Data.Maybe
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate


import File
import Display

data Argument =
    Display { project :: String }
  | New     { project :: String }
  | Today   { project :: String }
  | Week    { project :: String
            , byDay   :: Bool   }
  | Month   { project :: String
            , byDay   :: Bool
            , month  :: Int
            , year    :: Integer }
  | Delete  { project :: String }
  | List
  | Init
  | Start   { project :: String
            , comment :: String }
  deriving (Show, Data, Typeable)

data Operation =
    Display' StoredData
  | New' StoredData String
  | Today' StoredData String
  | Week' StoredData Bool String
  | Month' StoredData Bool String Int Integer
  | Error' String
  | Init'
  | Delete' StoredData String
  | List' StoredData
  | Start' StoredData String String
  deriving (Show)

display :: Argument
display = Display { project = def &= help "Project name" &= typ "name" }

today :: Argument
today = Today { project = def &= help "Project name" &= typ "name" }

week :: Argument
week = Week { project = def &= help "Project name" &= typ "name"
            , byDay = def &= help "Sorted by day" &= typ "by-day" }

month' :: Argument
month' = Month { project = def &= help "Project name" &= typ "name"
              , byDay = def &= help "Sorted by day" &= typ "by-day"
              , month = def &= help "Specify specific month" &= typ "month"
              , year = def &= help "Specify yearh" &= typ "year" }

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

initP :: Argument
initP = Init &= help "Create file in home folder."

delete :: Argument
delete = Delete
  { project = def &= typ "<Project name>" &= argPos 0
  } &= help "Delete project"

toErr :: FileError -> String
toErr NotFound = "Could not find file.\nRun 'tt init' to create file in home directory."
toErr (Other s) = s

operation :: Argument -> Either FileError StoredData -> Operation
operation Init _                    = Init'
operation _ (Left e)                = Error' $ toErr e
operation (New s) (Right p)         = New' p s
operation List (Right p)            = List' p
operation (Delete s) (Right p)      = Delete' p s
operation (Today s) (Right p)       = Today' p s
operation (Week s b) (Right p)      = Week' p b s
operation (Month s b m y) (Right p) = Month' p b s m y
operation (Start s c) (Right p)     = Start' p s c
operation _ (Right p)               = Display' p

elemName :: String -> [Project] -> Bool
elemName _ [] = False
elemName s (x:xs)
  | File.name x == s = True
  | otherwise = elemName s xs

getProjects :: String -> StoredData -> Maybe [Project]
getProjects p d
  | not (null byName) = Just byName
  | p /= ""           = Nothing
  | otherwise         = Just $ projects d
  where
    byName = filter (\x -> File.name x == p) $ projects d

filterToday :: UTCTime -> UTCTime -> UTCTime -> Bool
filterToday t s _ = utctDay t <= utctDay s

filterWeek :: UTCTime -> UTCTime -> UTCTime -> Bool
filterWeek t s _ = t' <= utctDay s
  where
    (y, m, _) = toWeekDate $ utctDay t
    t' = fromWeekDate y m 0

fst' :: (a, b, c) -> a
fst' (a, _, _) = a

-- TODO: handle first week in january..
filterMonth :: UTCTime -> Int -> Integer -> UTCTime -> UTCTime -> Bool
filterMonth t 0 0 s _ = t' <= utctDay s
  where
    (y', m, _) = toGregorian $ utctDay t
    t' = fromGregorian y' m 0
filterMonth t m y s _ = from <= utctDay s && to >= utctDay s
  where
    y' = if y == 0 then fst' $ toGregorian $ utctDay t else y
    m' = if m == 0 then 1 else m
    from  = fromGregorian y' m' 0
    to  = fromGregorian y' m' (gregorianMonthLength y' m')

maybe' :: (a -> b) -> Maybe a -> Maybe b
maybe' f a
  | isJust a  = Just $ f $ fromJust a
  | otherwise = Nothing

getByTime :: (UTCTime -> UTCTime -> Bool) -> String -> StoredData -> Maybe [Project]
getByTime f n = maybe' (filter noJobs . map (filterJobs f)) . getProjects n
  where
    noJobs x = not (null $ File.jobs x)

-- Remove jobs that is not relevant
filterJobs :: (UTCTime -> UTCTime -> Bool) -> Project -> Project
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

runOperation :: UTCTime -> Operation -> IO Result'
runOperation _ (New' d s)         = createNew d s
runOperation _ (List' d)          = listProjects d
runOperation _ (Delete' d s)      = deleteProject d s
runOperation t (Today' d s)       = displayToday $ getByTime (filterToday t) s d
runOperation t (Week' d b s)      = displayWeek b $ getByTime (filterWeek t) s d
runOperation t (Month' d b s m y) = displayMonth b $ getByTime (filterMonth t m y) s d
runOperation _ (Start' d s c)     = record' d s c
runOperation _ Init'              = createFile
runOperation _ (Error' e)         = return $ Err e
runOperation _ (Display' _)       = return $ Err "Display operation not ready"

record' :: StoredData -> String -> String -> IO Result'
record' d p job = do
  t <- getCurrentTime
  if elemName p (projects d)
  then go p job 0 t False
  else return $ Err "No such project"
   where
    isEnter :: Bool -> IO Bool
    isEnter False = return False
    isEnter True  = do
      c <- getChar
      return $ c == '\n'
    go :: String -> String -> NominalDiffTime -> UTCTime -> Bool -> IO Result'
    go s c _ s' True = saveJob s c s'
    go s c sec s' _ = do
      displayRecord s c sec
      threadDelay 1000000 --sleep for a million microseconds, or one second
      t' <- getCurrentTime
      input <- hReady stdin
      finish <- isEnter input
      go s c (t' `diffUTCTime` s') s' finish

saveJob :: String -> String -> UTCTime -> IO Result'
saveJob p j s = do
  displaySave p j s
  d <- getStoredData
  end <- getCurrentTime
  either (return . Err . toErr) (save' s end) d
    where
      addJob s' e' p'
        | File.name p' == p = Project (File.name p') (File.jobs p' ++ [ Job j s' e' ]) -- TODO
        | otherwise         = p'
      save' :: UTCTime -> UTCTime -> StoredData -> IO Result'
      save' s' end d = do
        save $ StoredData $ map (addJob s' end) $ projects d
        return Ok

main :: IO ()
main = do
  input <- cmdArgs $ modes [new, start, display, list, delete, today, week, month', initP]
    &= help "Track time spent on projects"
    &= program "tt"
    &= summary "Time tracker v0.1"
  d <- getStoredData
  t <- getCurrentTime
  res <- runOperation t $ operation input d
  displayEnd res
