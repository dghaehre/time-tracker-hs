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
