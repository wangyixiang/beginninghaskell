{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module KMean where
import Data.List
import Data.Default
import qualified Data.Map as M

class (Default v, Ord v) => Vector v where
	distance :: v -> v -> Double
	centroid :: [v] -> v

instance Vector (Double, Double) where
  distance (a, b) (c,d) = sqrt $ (a-c)^2 + (b-d)^2
  centroid lst = let (u, v) = foldr (\(a,b) (c,d) -> (a+c,b+d)) (0.0, 0.0) lst 
		     n = fromIntegral $ length lst
	in (u / n, v / n)

class Vector v => Vectorizable e v where
	toVector :: e -> v

instance Vectorizable (Double, Double) (Double, Double) where
	toVector = id


--yxfun p m = let chosenCentroid = minimumBy (\x y -> compare (distance x $ toVector p) (distance y $ toVector p) in M.adjust (p:) 

clusterAssignmentPhase :: (Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase centroids points =   
  let initialMap = M.fromList $ zip centroids (repeat []) 
  in foldr 
  (\p m -> let chosenCentroid = 
		minimumBy 
	      	(\x y -> compare ( distance x $ toVector p) ( distance y $ toVector p)) 
	      	centroids 
	   in M.adjust (p:) chosenCentroid m) 
  initialMap points

newCentroidPhase :: (Vector v, Vectorizable e v) => M.Map v [e] -> [(v,v)]
newCentroidPhase = M.toList . fmap (centroid . map toVector)


initializeSimple :: Int -> [e] -> [(Double,Double)]
initializeSimple 0 _ = []
initializeSimple n v = (fromIntegral n::Double, fromIntegral n::Double) : initializeSimple (n-1) v

shouldStop :: (Vector v) => [(v,v)] -> Double -> Bool
shouldStop centroids threshold = foldr (\(x,y) s -> s + distance x y) 0.0 centroids < threshold

kMeans :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v])  -- initialization function
                                       -> Int                  -- number of centroids
                                       -> [e]                  -- the information
                                       -> Double               -- threshold
                                       -> [v]                  -- final centroids
kMeans i k points = kMeans' (i k points) points
 
kMeans' :: (Vector v, Vectorizable e v) => [v] -> [e] -> Double -> [v]
kMeans' centroids points threshold =
  let assignments     = clusterAssignmentPhase centroids points
      oldNewCentroids = newCentroidPhase assignments
      newCentroids    = map snd oldNewCentroids
   in if shouldStop oldNewCentroids threshold
      then newCentroids
      else kMeans' newCentroids points threshold

genCluster :: M.Map (Double,Double) [(Double,Double)]
--genCluster :: (Vector v, Vectorizable e v) => [e] -> M.Map v [e]
genCluster = let es = ([(1,2),(3,4),(5,6)]::[(Double,Double)])
		 f x = toVector x::(Double,Double)
	in M.fromList $ zip (map f es) (repeat [])

