module Display where

import            System.Console.ANSI
import            Data.Time.Clock
import            Data.Time.Calendar
import            Data.Time.Format
import qualified  Data.Map.Lazy        as M

import File

type ProjectsWithDay = M.Map Day [Project]

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
  mapM_ (putStrLn . File.name) $ projects d
  return Finish

-- Get total amount of jobs and total time spent
totalJobs :: [Job] -> (Int, NominalDiffTime)
totalJobs j = (length j, sum $ map (getSec . getTime) j)

concatJobs :: [Project] -> [Job]
concatJobs = foldl (\b a -> jobs a ++ b) []

sortByDay :: [Project] -> ProjectsWithDay
sortByDay ps = go ps $ M.fromList []
  where
    go :: [Project] -> ProjectsWithDay -> ProjectsWithDay
    go ps' d = foldl (\d' p -> M.unionWith (++) d' $ createWithDay p) d ps'

createWithDay :: Project -> ProjectsWithDay
createWithDay p = M.fromListWith concat' jobs'
  where
    projectName = name p
    concat' :: [Project] -> [Project] -> [Project]
    concat' a b = [Project projectName $ concatJobs a ++ concatJobs b]
    jobs' = map addDay $ jobs p 
    addDay j = (utctDay $ startTime j, [Project projectName [j]])

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

showDay :: Day -> IO ()
showDay d = do
  setSGR [SetColor Foreground Vivid Yellow]
  putStr " ** "
  setSGR [SetColor Foreground Vivid Blue]
  putStr $ formatTime defaultTimeLocale "%A" d
  setSGR [SetColor Foreground Vivid Yellow]
  putStr " **\n"
  setSGR [SetColor Foreground Vivid White]
  putStr $ "   " ++ show d ++ "\n"

displayWeek :: Bool -> Maybe [Project] -> IO Result'
displayWeek _ Nothing = return $ Err "No such project."
displayWeek byDay (Just p) = do
  setSGR [SetColor Foreground Vivid Green]
  putStr "Projects worked on this week\n\n"
  displayBullet $ "Total jobs: " ++ show j
  displayBullet $ "Total time: " ++ showSec t
  putStrLn "------------------------------\n"
  if byDay then displayByDay p else displayJobs p
    where
      (j, t) = totalJobs $ concatJobs p

displayMonth :: Bool -> Maybe [Project] -> IO Result'
displayMonth _ Nothing = return $ Err "No such project."
displayMonth byDay (Just p) = do
  setSGR [SetColor Foreground Vivid Green]
  putStr "Projects worked on this month\n\n"
  displayBullet $ "Total jobs: " ++ show j
  displayBullet $ "Total time: " ++ showSec t
  putStrLn "------------------------------\n"
  if byDay then displayByDay p else displayJobs p
    where
      (j, t) = totalJobs $ concatJobs p

displayBullet :: String -> IO ()
displayBullet s = do
  setSGR [SetColor Foreground Vivid Yellow]
  putStr "* "
  setSGR [SetColor Foreground Vivid White]
  putStr $ s ++ "\n"
  return ()

displayByDay :: [Project] -> IO Result'
displayByDay p = do
  r <- mapM displayDay $ M.toList $ sortByDay p
  return' r Finish
    where
      displayDay p' = do
        putStr "\n"
        showDay (fst p')
        putStr "\n"
        displayBullet $ "Total jobs: " ++ show j
        displayBullet $ "Total time: " ++ showSec t
        putStr "\n"
        displayJobs $ snd p'
          where
            (j, t) = totalJobs $ concatJobs $ snd p'
      return' :: [Result'] -> Result' -> IO Result'
      return' [] r' = return r'
      return' (Err e:xs) (Err e') = return' xs (Err $ e ++ "|" ++ e')
      return' (Err e:xs) _ = return' xs (Err e)
      return' (_:xs) (Err e) = return' xs (Err e)
      return' (x:xs) _ = return' xs x

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
    addZero s' = show s'
    s = sec' `mod` 60
    m = (sec' `div` 60) `mod` 60
    h = sec' `div` (60 * 60)

getSec :: (UTCTime, UTCTime) -> NominalDiffTime
getSec (a, b) = b `diffUTCTime` a

getTime :: Job -> (UTCTime, UTCTime)
getTime j = (startTime j, endTime j)

displayJobs :: [Project] -> IO Result'
displayJobs p = do
  mapM_ d p
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
        putStrLn $ showSec (sum $ map (getSec . getTime) jobs')
        setSGR [SetColor Foreground Vivid White]
        mapM_ displayJob $ foldl addCounter [] (jobs p')
        putStr "\n"
        return ()
          where
            addCounter :: [(Job, Int, NominalDiffTime)] -> Job -> [(Job, Int, NominalDiffTime)] 
            addCounter l j
              | j `elem` map fst' l  = map (insert j) l
              | otherwise           = (j, 1, getSec $ getTime j) : l
            insert j' (a, b, c)
              | a == j'             = (a, b + 1, getSec (getTime j') + c)
              | otherwise           = (a, b, c)
            fst' (a, b, c) = a

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

displayJob :: (Job, Int, NominalDiffTime) -> IO ()
displayJob (j, i, n) = displayBullet $ showSec n ++ s' i ++ description j
    where
      s' i'
        | i' <= 1 = "     " 
        | otherwise = " (" ++ show i' ++ ") "

displayProject :: [Project] -> IO Result'
displayProject _ = do
  putStrLn "Display project"
  return Finish
