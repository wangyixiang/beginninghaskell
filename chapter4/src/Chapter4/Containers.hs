module Chapter4.Containers where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Tree as T
yxMap = M.fromList [("hello",3),("bye",4)]
yxMap2 = M.fromList [("hello",13),("bye",5),("welcome",6)]
yxmv1 = M.null M.empty
yxmv2 = M.null $ yxMap
yxmv3 = M.member "hello" $ yxMap
yxmv4 = M.lookup "hello" $ yxMap
yxmv5 = M.lookup "welcome" $ yxMap
yxmv6 = M.findWithDefault 0 "welcome" $ yxMap
yxmv7 = M.delete "hello" $ yxMap
yxmv8 = M.adjust (+7) "hello" $ yxMap

yxTree = T.Node 1 [ T.Node 2 [ T.Node 3 [],T.Node 4 [], T.Node 5 [] ], T.Node 6 []]

yxPreOrder :: (a -> b) -> T.Tree a -> [b]
yxPreOrder f (T.Node v subTrees) = let traverseTree trees = concat $ map (yxPreOrder f) trees
				in f v : (traverseTree subTrees)

