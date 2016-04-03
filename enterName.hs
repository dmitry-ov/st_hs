module Main where

main :: IO ()
main = do
    putStrLn "What is your name ?"
    putStr "Name: "
    name <- getLine
    if (name == "")
    then main
    else putStrLn $ "Hi, " ++ name ++ "!"
