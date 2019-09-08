module Display where

import System.Console.ANSI
import Data.Time.Clock

import File

type ProjectsWithDay = (String, [Project])

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
  putStr "Projects: "
  setSGR [SetColor Foreground Vivid White]
  putStrLn $ "(" ++ show (length (projects d)) ++ ")"
  _ <- mapM_ (putStrLn . File.name) $ projects d
  return Finish

-- Get total amount of jobs and total time spent
totalJobs :: [Job] -> (Int, NominalDiffTime)
totalJobs j = (length j, sum $ map getSec $ map getTime j)

concatJobs :: [Project] -> [Job]
concatJobs = foldl (\b a -> jobs a ++ b) []

-- TODO
sortByDay :: [Project] -> [ProjectsWithDay]
sortByDay _ = []

displayToday :: Maybe [Project] -> IO Result'
displayToday Nothing = return $ Err "No such project."
displayToday (Just p) = do
  setSGR [SetColor Foreground Vivid Green]
  putStr "Projects worked on today\n\n"
  displayBullet $ "Total jobs: " ++ show j
  displayBullet $ "Total time: " ++ showSec t
  putStrLn "------------------------------\n"
  displayJobs p
    where
      (j, t) = totalJobs $ concatJobs p

displayWeek :: Bool -> Maybe [Project] -> IO Result'
displayWeek _ Nothing = return $ Err "No such project."
displayWeek byDay (Just p) = do
  setSGR [SetColor Foreground Vivid Green]
  putStr "Projects worked on this week\n\n"
  displayBullet $ "Total jobs: " ++ show j
  displayBullet $ "Total time: " ++ showSec t
  putStrLn "------------------------------\n"
  display' byDay
    where
      (j, t) = totalJobs $ concatJobs p
      displayDay p' = do
        putStr $ "\n" ++ fst p' ++ "\n"
        displayJobs $ snd p'
      display' False = displayJobs p
      display' True = do
        r <- mapM displayDay $ sortByDay p
        return' r Finish
          where
            -- TODO semi ready
            return' :: [Result'] -> Result' -> IO Result'
            return' [] r' = return r'
            return' ((Err e):xs) (Err e') = return' xs (Err $ e ++ "|" ++ e')
            return' ((Err e):xs) _ = return' xs (Err e)
            return' (_:xs) (Err e) = return' xs (Err e)
            return' (x:xs) _ = return' xs x

displayMonth :: Maybe [Project] -> IO Result'
displayMonth Nothing = return $ Err "No such project."
displayMonth (Just p) = do
  setSGR [SetColor Foreground Vivid Green]
  putStr "Projects worked on this month\n\n"
  displayBullet $ "Total jobs: " ++ show j
  displayBullet $ "Total time: " ++ showSec t
  putStrLn "------------------------------\n"
  displayJobs p
    where
      (j, t) = totalJobs $ concatJobs p

displayBullet :: String -> IO ()
displayBullet s = do
  setSGR [SetColor Foreground Vivid Yellow]
  putStr "* "
  setSGR [SetColor Foreground Vivid White]
  putStr $ s ++ "\n"
  return ()

displayRecord :: String -> String -> NominalDiffTime -> IO ()
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

showSec :: NominalDiffTime -> String
showSec sec = addZero h ++ "." ++ addZero m ++ "." ++ addZero s
  where
    sec' = round sec :: Int
    addZero s' | s' < 10 = "0" ++ show s'
    addZero s' | otherwise = show s'
    s = sec' `mod` 60
    m = (sec' `div` 60) `mod` 60
    h = sec' `div` (60 * 60)

getSec :: (UTCTime, UTCTime) -> NominalDiffTime
getSec (a, b) = b `diffUTCTime` a

  {--
getSec (start, end) = (toSec end) - (toSec start)
  where
    toSec :: String -> Int
    toSec = f' . parseTimeM True defaultTimeLocale "%F %R:%S%Q %Z"
      where
        f' :: Maybe UTCTime -> Int
        f' Nothing  = 0
        f' (Just t) = read $ formatTime defaultTimeLocale "%s" t
        --}

getTime :: Job -> (UTCTime, UTCTime)
getTime j = (startTime j, endTime j)

displayJobs :: [Project] -> IO Result'
displayJobs p = do
  _ <- mapM_ d p
  return Finish
    where
      d :: Project -> IO ()
      d p' = do
        let
          jobs' = jobs p'
          l = show $ length jobs'
        setSGR [SetColor Foreground Vivid Green]
        putStr $ File.name p'
        setSGR [SetColor Foreground Vivid White]
        putStr $ " (" ++ l ++ ") - "
        setSGR [SetColor Foreground Vivid Yellow]
        putStrLn $ showSec (sum $ map getSec $ map getTime jobs')
        setSGR [SetColor Foreground Vivid White]
        _ <- mapM_ displayJob $ jobs p'
        putStr "\n"
        return ()

displaySave :: String -> String -> UTCTime -> IO ()
displaySave _ _ start = do
  end <- getCurrentTime
  clearScreen
  setCursorPosition 0 0
  setSGR [SetColor Foreground Vivid Yellow]
  putStrLn "Saving"
  displayBullet $ "Time spent: " ++ showSec (getSec (start, end))
  putStrLn ""
  return ()

displayJob :: Job -> IO ()
displayJob j = do
  displayBullet $ s ++ " (0) " ++ description j
    where
      s = showSec $ getSec $ getTime j

displayProject :: [Project] -> IO Result'
displayProject _ = do
  putStrLn "Display project"
  return Finish
