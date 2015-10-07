{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
import Data.Char
data Client = GovOrg String | Company String Integer Person String | Individual Person Bool
		deriving Show

data Person = Person String String Gender
		deriving Show

data Gender = Male | Female | Unknown
		deriving Show

--Simple Pattern Marching
clientName :: Client -> String
clientName client = case client of
			GovOrg name	-> name
			Company name id person resp -> name
			Individual person _ -> 
				case person of
					Person fName lName gender -> fName ++ "  " ++ lName

clientNamev2 :: Client -> String
clientNamev2 client = case client of
			GovOrg name -> name
			Company name _ _ _ -> name
			Individual (Person fname lname _) _ -> fname ++ " " ++ lname

companyName :: Client -> Maybe String
companyName client = case client of
			Company name _ _ _ -> Just name
			_	-> Nothing


--mathematical functions writing way
clientNamev3 :: Client -> String
clientNamev3 (GovOrg name) = name
clientNamev3 (Company name _ _ _) = name
clientNamev3 (Individual (Person fname lname _) _) = fname ++ " " ++ lname

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)


sortedv1 :: Ord a => [a] -> Bool
sortedv1 [] = True
sortedv1 [_] = True
sortedv1 (x:y:ys) = x < y && sortedv1 (y:ys)

--using 'as pattern'
sortedv2 :: Ord a => [a] -> Bool
sortedv2 [] = True
sortedv2 [_] = True
sortedv2 (x:r@(y:_)) = x < y && sortedv2 r


--binomialCoefficient :: Int Int -> Int
binomialCoefficient _ 0 = 1
binomialCoefficient x y | x==y = 1 
binomialCoefficient n k = binomialCoefficient (n-1) (k-1) + binomialCoefficient (n-1) k

ackermann 0 n = n + 1
ackermann m 0 = ackermann (m-1) 1
ackermann m n = ackermann (m-1) (ackermann m (n-1))

--view pattern
{-# LANGUAGE ViewPatterns #-}
specialClient :: Client -> Bool
specialClient (clientNamev3 -> "wang yixiang") = True
specialClient  _ = False


data ClientR = GovOrgR { clientRName :: String}
		| CompanyR { clientRName :: String
			, companyId :: Integer
			, person :: PersonR
			, duty :: String }
		| IndividualR { person :: PersonR }
		deriving Show

data PersonR = PersonR { firstName :: String
			,lastName :: String
			} deriving Show


greetv1 :: ClientR -> String
greetv1 GovOrgR { clientRName = c } = "Welcome"
greetv1 CompanyR { clientRName = c } = "Hello " ++ c
greetv1 IndividualR { person = PersonR { firstName = c}} = "Hi " ++ c
-- record puns
{-# LANGUAGE NamedFieldPuns #-}
greetv2 :: ClientR -> String
greetv2 GovOrgR { clientRName } = "Welcome"
greetv2 CompanyR { clientRName } = "Hello " ++ clientRName
greetv2 IndividualR { person = PersonR { firstName}} = "Hi " ++ firstName
-- two dots
{-# LANGUAGE RecordWildCards #-}
greetv3 :: ClientR -> String
greetv3 GovOrgR { .. } = "Welcome"
greetv3 CompanyR { .. } = "Hello " ++ clientRName
greetv3 IndividualR { person = PersonR {..}} = "Hi " ++ firstName

-- recording updating
-- 很明显，下面的pattern matching 是一个非穷尽的
nameInCapitals :: PersonR -> PersonR
nameInCapitals p@(PersonR { firstName = initial:rest }) = 
	let newName = (toUpper initial):rest
	in p { firstName = newName}
