module Display where

import System.Console.ANSI
import Data.Time.Format
import Data.Time.Clock

import File

displayEnd :: Result' -> IO ()
displayEnd Ok = do
  setSGR [SetColor Foreground Vivid Green]
  putStrLn "Success"
  return ()
displayEnd Finish = return ()
displayEnd (Err s) = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn s
  return ()

listProjects :: StoredData -> IO Result'
listProjects d = do
  setSGR [SetColor Foreground Vivid Green]
  putStrLn "Your current projects:"
  setSGR [SetColor Foreground Vivid Yellow]
  _ <- mapM_ (putStrLn . File.name) $ projects d
  return Finish

-- Get total amount of jobs and total time spent
totalJobs :: [Job] -> (Int, Int)
totalJobs j = (length j, sum $ map getSec $ map getTime j)

concatJobs :: [Project] -> [Job]
concatJobs = foldl (\b a -> jobs a ++ b) [] 

displayToday :: Maybe [Project] -> IO Result'
displayToday Nothing = return $ Err "No such project."
displayToday (Just p) = do
  setSGR [SetColor Foreground Vivid Green]
  putStr "Projects worked on today\n\n"
  displayBullet $ "Total jobs: " ++ show jobs
  displayBullet $ "Total time: " ++ showSec time
  putStrLn "------------------------------\n"
  displayJobs p
    where
      (jobs, time) = totalJobs $ concatJobs p

displayWeek :: Maybe [Project] -> IO Result'
displayWeek Nothing = return $ Err "No such project."
displayWeek (Just p) = do
  setSGR [SetColor Foreground Vivid Green]
  putStr "Projects worked on this week\n\n"
  displayBullet $ "Total jobs: " ++ show jobs
  displayBullet $ "Total time: " ++ showSec time
  putStrLn "------------------------------\n"
  displayJobs p
    where
      (jobs, time) = totalJobs $ concatJobs p

displayBullet :: String -> IO ()
displayBullet s = do
  setSGR [SetColor Foreground Vivid Yellow]
  putStr "* "
  setSGR [SetColor Foreground Vivid White]
  putStr $ s ++ "\n"
  return ()

displayRecord :: String -> String -> Int -> IO ()
displayRecord project comment sec = do
  clearScreen
  setCursorPosition 0 0
  setSGR [SetColor Foreground Vivid White]
  putStr "Working "
  setSGR [SetColor Foreground Vivid Green]
  putStr $ project ++ "  "
  setSGR [SetColor Foreground Vivid Yellow]
  putStr $ "" ++ showSec sec ++ "\n"
  putStrLn comment
  putStrLn ""
  setSGR [SetColor Foreground Vivid White]
  putStrLn "Press Enter to save job"
  return ()
    where
      c :: String -> String
      c "" = ""
      c c' = " - " ++ c'

showSec :: Int -> String
showSec sec = addZero h ++ "." ++ addZero m ++ "." ++ addZero s
  where
    addZero :: Int -> String
    addZero s' | s' < 10 = "0" ++ show s'
    addZero s' | otherwise = show s'
    s = sec `mod` 60
    m = (sec `div` 60) `mod` 60
    h = (sec `div` (60 * 60)) `mod` 60

getSec :: (String, String) -> Int
getSec (start, end) = (toSec end) - (toSec start)
  where
    toSec :: String -> Int
    toSec = f' . parseTimeM True defaultTimeLocale "%F %R:%S%Q %Z"
      where
        f' :: Maybe UTCTime -> Int
        f' Nothing  = 0
        f' (Just t) = read $ formatTime defaultTimeLocale "%s" t

getTime :: Job -> (String, String)
getTime j = (startTime j, endTime j)

displayJobs :: [Project] -> IO Result'
displayJobs p = do
  _ <- mapM_ d p
  return Finish
    where
      d :: Project -> IO ()
      d p = do
        let 
          jobs' = jobs p
          l = show $ length jobs'
        setSGR [SetColor Foreground Vivid Green]
        putStr $ File.name p
        setSGR [SetColor Foreground Vivid White]
        putStr $ " (" ++ l ++ ") - "
        setSGR [SetColor Foreground Vivid Yellow]
        putStrLn $ showSec (sum $ map getSec $ map getTime jobs')
        setSGR [SetColor Foreground Vivid White]
        _ <- mapM_ displayJob $ jobs p
        putStr "\n"
        return ()

displaySave :: String -> String -> IO ()
displaySave p j = do
  clearScreen
  setCursorPosition 0 0
  setSGR [SetColor Foreground Vivid Yellow]
  putStrLn "* Saving job"
  putStrLn ""
  return ()

displayJob :: Job -> IO ()
displayJob j = do
  displayBullet $ s ++ " (0) " ++ description j
    where
      s = showSec $ getSec $ getTime j

displayProject :: [Project] -> IO Result'
displayProject p = do
  putStrLn "Display project"
  return Finish
