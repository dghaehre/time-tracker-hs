module Display where
-- TODO: make output look nice

import File

displayEnd :: Result' -> IO ()
displayEnd Ok = do
  putStrLn "Success"
  return ()
displayEnd Finish = return ()
displayEnd (Err s) = do
  putStrLn s
  return ()

listProjects :: StoredData -> IO Result'
listProjects d = do
  putStrLn "Your current projects:"
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
  putStrLn $ "Today (" ++ show jobs ++ ") - " ++ show time ++ "\n"
  displayJobs p
    where
      (jobs, time) = totalJobs $ concatJobs p

displayWeek :: Maybe [Project] -> IO Result'
displayWeek Nothing = return $ Err "No such project."
displayWeek (Just p) = do
  putStrLn $ "This week: " ++ (show jobs) ++ "\n" 
  displayJobs p
    where
      (jobs, time) = totalJobs $ concatJobs p

displayJobs :: [Project] -> IO Result'
displayJobs p = do
  _ <- mapM_ d p
  return Finish
    where
      d :: Project -> IO ()
      d p = do
        let l = show $ length $ jobs p
        putStrLn $ File.name p ++ " (" ++ l ++ ") - " ++ "0"
        _ <- mapM_ displayJob $ jobs p
        return ()

displayJob :: Job -> IO ()
displayJob j = do
  putStrLn $ File.description j

displayProject :: [Project] -> IO Result'
displayProject p = do
  putStrLn "Display project"
  return Finish
