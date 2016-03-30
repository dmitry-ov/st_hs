import System.Directory
import Data.List

main' :: IO ()
main' = do
    putStr "Substring: "
    line <- getLine
    if null line
    then putStrLn "Canceled"
    else  do
        list <- getDirectoryContents "."
        mapM_ deleteFiles $ filter (stringExists line) list

deleteFiles file = do
   putStrLn $ "Removing file: " ++ file
   removeFile file

stringExists "" wordToCheckAgainst = False
stringExists wordToCheckFor "" = False
stringExists wordToCheckFor wordToCheckAgainst | length wordToCheckAgainst < length wordToCheckFor = False
                                               | length wordToCheckAgainst == length wordToCheckFor = wordToCheckAgainst == wordToCheckFor
                                               | take (length wordToCheckFor) wordToCheckAgainst == wordToCheckFor = True
                                               | otherwise = stringExists wordToCheckFor (tail wordToCheckAgainst)