{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax#-}


module Set3_monad_version where

import MCPrelude
import Set4

instance Monad [] where
    bind [] _ = []
    bind (a:as) f = f a ++ bind as f
    return a = [a]

----------------------------------------
-- GENERATING COMBINATIONS
----------------------------------------
--
-- generate the cartesian product for two input lists
allPairs :: [a] -> [b] -> [(a,b)]
allPairs v1 v2 = flattened 
    where rev1 = reverse v1 -- do this so we get ascending order for fst of tuple
          crunch = foldl (\accX x -> (map (\y -> (x,y)) v2) : accX) [] rev1
          flattened = concat crunch

-- implemented using recursion instead
allPairs' :: [a] -> [b] -> [(a,b)]
allPairs' [] v2 = []
allPairs' v1 [] = []
allPairs' [x] [y] = [(x,y)]
allPairs' [x] (y:ys) = (x,y) : allPairs' [x] ys
allPairs' (x:xs) [y] = (x,y) : allPairs' xs [y]
allPairs' (x:xs) (y:ys) = cartesianProduct
    where cartesianProduct = allPairs' [x] (y:ys) ++ allPairs' xs (y:ys)


----------------------------------------
-- POKER HANDS
----------------------------------------
data Card = Card Int String

instance Show Card where
    show (Card a b) = show a ++ b

-- using previous allPairs' func
allCards :: [Int] -> [String] -> [Card]
allCards cNums cClasses = 
    let permutations = allPairs' cNums cClasses
        cards = map (\(cNum, cType) -> Card cNum cType) permutations
    in cards

-- implementation only using recursion
allCards' :: [Int] -> [String] -> [Card]
allCards' [] v2 = []
allCards' v1 [] = []
allCards' [x] [y] = [Card x y] 
allCards' [x] (y:ys) = Card x y : allCards' [x] ys
allCards' (x:xs) [y] = Card x y : allCards' xs [y]
allCards' (x:xs) (y:ys) = cartesianProduct
    where cartesianProduct = allCards' [x] (y:ys) ++ allCards' xs (y:ys)


----------------------------------------
-- GENERALIZING PAIRS AND CARDS
----------------------------------------
-- look at allPairs' and allCards' function and find the 
-- differences and similarities. abstract a pattern out of this
-- and call this func
-- allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
-- allCombs f leftVals [] = []
-- allCombs f [] rightVals = []
-- -- allCombs f [x] [y] = [f x y]
-- allCombs f [x] (y:ys) = f x y : allCombs f [x] ys
-- allCombs f (x:xs) [y] = f x y : allCombs f xs [y]
-- allCombs f (x:xs) (y:ys) =
--     let cartesianProduct = allCombs f [x] (y:ys) ++ allCombs f xs (y:ys)
--     in cartesianProduct

-- using monad abstraction, the allCombs simply becomes this
allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs = liftM2


-- now we can redo allPairs' and allCards' to use this pattern which
-- should vastly simplify them
allPairs'' :: [a] -> [b] -> [(a,b)]
allPairs'' v1 v2 = allCombs (,) v1 v2

-- Card is just a function taking 2 args
allCards'' :: [Int] -> [String] -> [Card]
allCards'' ranks suits = allCombs Card ranks suits

-- how about the cartesian product of 3 items
-- allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
-- allCombs3 f v1 v2 [] = []
-- allCombs3 f v1 [] v3 = []
-- allCombs3 f [] v2 v3 = []
-- allCombs3 f [x] [y] [z] = [f x y z]
-- allCombs3 f [x] [y] (z:zs) = f x y z: allCombs3 f [x] [y] zs
-- allCombs3 f (x:xs) [y] [z] = f x y z: allCombs3 f xs [y] [z]
-- allCombs3 f [x] (y:ys) [z] = f x y z: allCombs3 f [x] ys [z]
-- allCombs3 f (x:xs) (y:ys) (z:zs) = products
--     where products = allCombs3 f [x] [y] (z:zs) ++
--                      allCombs3 f [x] ys (z:zs) ++
--                      allCombs3 f xs [y] (z:zs) ++ 
--                      allCombs3 f xs ys (z:zs)

-- using the monod abstraction, the allCombs3 func becomes
allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 = liftM3


----------------------------------------
-- COMBINATIONS OF MORE THINGS
----------------------------------------

-- i went way off on a tangent when trying to guess the type of the generalization 
-- pattern. also, this definition is not satisfactory b/c it's too restrictive; see the 
-- actual combStep defined below it to see why.
-- combStep :: [a -> b] -> [a] -> [b]
-- combStep [] vals = []
-- combStep funcs [] = []
-- combStep (f:fs) (x:xs) = f x : combStep fs xs
--
-- this is what we were after
-- combStep :: [a -> b] -> [a] -> [b]
-- combStep _ [] = []
-- combStep [] _ = []
-- combStep (f:fs) vals = map f vals ++ combStep fs vals

-- or using monadic abstraction, combStep becomes
combStep :: [a -> b] -> [a] -> [b]
combStep = ap 

allCombs' :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs' f v1 v2 = combStep  (combStep [f] v1) v2

allCombs3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3' f v1 v2 v3 = combStep (combStep (combStep [f] v1) v2) v3
