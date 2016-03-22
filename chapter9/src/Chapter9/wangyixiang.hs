-- Basic Input and Output
main2 = do 
    putStrLn "wangyixiang"
    place <- getLine
    let year = (length place) * 10
    putStrLn $ "It's " ++ place ++ " distance is " ++ show year

