module Message where

message :: Bool -> String -> IO()
message bool text
    |bool = putStrLn text
    |otherwise = putStrLn text


