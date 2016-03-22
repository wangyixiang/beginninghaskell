allNumbers = allNumbersFrom 1
allNumbersFrom n = n: allNumbersFrom (n+1)


data TimeMachine = TM {manufacturer :: String, year :: Integer } deriving (Eq, Show)

timeMachinesFrom :: String -> Integer -> [TimeMachine]
timeMachinesFrom mf y = TM mf y : timeMachinesFrom mf (y+1) 


yxtake _ [] = []
yxtake 0 l = []
yxtake n (x:xs) = x: yxtake (n-1) xs

yxsum [] = 0
yxsum (x:xs) = x + ( yxsum xs)

fibonacci = 0 : 1: zipWith (+) fibonacci (tail fibonacci)

yxfib 0 = 0
yxfib 1 = 1
yxfib n = yxfib (n-1) + yxfib (n-2)

fibonacci2 = map fst $ iterate (\(n, n1)->(n1,n+n1)) (0,1)
