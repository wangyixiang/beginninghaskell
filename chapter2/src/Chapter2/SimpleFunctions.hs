-- using || && and or
-- || && 与 and or的定义是不一样的，要注意
usingandorinfixr = True || (False && True)
usingandor = or [False, and [True, False]]

-----------------------------------------------
firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

(+++) :: [a] -> [a] -> [a]
lst1 +++ lst2 = if null lst1 then lst2 else head lst1 : tail lst1 +++ lst2

list1 ++++ list2 = case list1 of
			[] -> list2
			x:xs -> x: xs ++++ list2

--addv3 :: [a] -> [a] -> [a]
addv3 [] list2 = list2
addv3 (x:xs) list2 = x:(addv3 xs list2)

reverse2 :: [a] -> [a]
reverse2 lst = if null lst then lst else reverse2 (tail lst) +++ (head lst:[])

-----------------------------------------------
yxmaxmin :: Ord a => [a] -> (a, a)
yxmaxmin list = let h = head list in
		if null (tail list) then (h, h)
		else if h > h_max then (h, h_min)
		else if h < h_min then (h_max, h)
		else (h_max, h_min) 
		where
			t = yxmaxmin (tail list)
			h_max = fst t
			h_min = snd t

maxmin :: Ord a => [a] -> (a, a)
maxmin list = let h = head list in
		if null (tail list) then (h, h)
		else (
		if h > t_max then h else t_max,
		if h < t_min then h else t_min
		)
		where  t = maxmin (tail list); t_max = fst t; t_min = snd t

--yxversion
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci ( n - 1) + (fibonacci ( n - 2))

--pattern marching and guard
mfibonacci 0 = 0
mfibonacci 1 = 1
mfibonacci n | otherwise = let (f1, f2) = ( mfibonacci (n-1), mfibonacci (n-2)) in (f1 + f2)

--just guard
mmfib n
	| n == 0 = 0
	| n == 1 = 1
	| otherwise = mmfib(n-2) + (mmfib(n-1))

	
