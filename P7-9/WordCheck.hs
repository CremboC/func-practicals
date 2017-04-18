import System.Environment
import Data.Char

main  =  do [dictName] <- getArgs
            wordset <- readFile dictName >>= return . lines
            text  <- getContents
            mapM_ (check wordset) (zip [1..] (lines text))

check :: [String] -> (Int,String) -> IO ()
check ws (no,line)  =  case nonws of
                       [] -> return ()
                       _  -> putStrLn (show no ++" "++unwords nonws)
  where
  nonws  =  [w | w <-  words (purge line),
                       not (map toLower w `elem` ws)]
  purge  =  map (\c -> if isAlpha c || elem c "-'" 
                       then c else ' ') 
