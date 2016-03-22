module Main where

main :: IO ()
main = putStrLn $ show result

result :: Integer
result = foldr (*) 1 [1..(10^5)]
