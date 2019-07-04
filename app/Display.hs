module Display where

import System.Console.ANSI

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

-- TODO
-- Get total amount of jobs and total time spent
totalJobs :: [Job] -> (Int, Int)
totalJobs j = (length j, 0)

concatJobs :: [Project] -> [Job]
concatJobs = foldl (\b a -> jobs a ++ b) [] 

displayToday :: Maybe [Project] -> IO Result'
displayToday Nothing = return $ Err "No such project."
displayToday (Just p) = do
  setSGR [SetColor Foreground Vivid Yellow]
  putStrLn $ "Today (" ++ show jobs ++ ") - " ++ show time ++ "\n"
  displayJobs p
    where
      (jobs, time) = totalJobs $ concatJobs p

displayWeek :: Maybe [Project] -> IO Result'
displayWeek Nothing = return $ Err "No such project."
displayWeek (Just p) = do
  setSGR [SetColor Foreground Vivid Green]
  putStrLn $ "This week: " ++ (show jobs) ++ "\n" 
  displayJobs p
    where
      (jobs, time) = totalJobs $ concatJobs p

displayRecord :: String -> String -> Int -> IO ()
displayRecord project comment sec = do
  clearScreen
  setCursorPosition 0 0
  setSGR [SetColor Foreground Vivid Green]
  putStrLn $ "| Running " ++ project ++ c comment
  setSGR [SetColor Foreground Vivid Yellow]
  putStrLn $ "| " ++ showSec sec
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

-- TODO
getSec :: (String, String) -> Int
getSec (start, end) = 0

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
        setSGR [SetColor Foreground Vivid Yellow]
        putStrLn $ File.name p ++ " (" ++ l ++ ") - " ++ showSec (sum $ map getSec $map getTime jobs')
        setSGR [SetColor Foreground Vivid White]
        _ <- mapM_ displayJob $ jobs p
        return ()

displaySave :: String -> String -> IO ()
displaySave p j = do
  clearScreen
  setCursorPosition 0 0
  setSGR [SetColor Foreground Vivid Yellow]
  putStrLn "| Saving job"
  putStrLn ""
  return ()

displayJob :: Job -> IO ()
displayJob j = do
  putStrLn " Job"
        

displayProject :: [Project] -> IO Result'
displayProject p = do
  putStrLn "Display project"
  return Finish
