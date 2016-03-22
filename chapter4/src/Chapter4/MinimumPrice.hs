data TravelGuide = TravelGuide { title::String, authors::[String], price::Double} 
	deriving (Show, Eq, Ord)

instance Ord TravelGuide where 
	(TravelGuide t1 a1 p1) <= (TravelGuide t2 a2 p2) = p1 < p2 || (p1 == p2 && ( t1 < t2 || (t1 == t2 && a1 <= a2)))
