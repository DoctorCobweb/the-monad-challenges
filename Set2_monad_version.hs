{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax#-}

module Set2_monad_version where

import MCPrelude
import Set4

data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
    show Nothing = "Nothing"
    show (Just a) = "Just " ++ show a


instance Monad Maybe where
    bind Nothing _ = Nothing
    bind (Just a) f = f a
    return a = Just a


-- BUILD A LIBRARY OF THINGS THAT CAN FAIL
----------------------------------------
-- make some safe versions of serveral common functions usually
-- specified in prelude

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = return x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (x:xs) = return xs

-- lookup can fail for two reasons 
-- i) inputted empty list
-- ii) there's no tuple in the list having the lookup key
lookupMay :: Eq a => a -> [(a,b)] -> Maybe b
lookupMay key [] = Nothing
lookupMay key tupleList = if length results == 0
                          then Nothing
                          else return (snd (results !! 0))
    where results = filter ( \(x,y) -> x == key) tupleList




divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay numerator denominator = return (numerator / denominator)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay [x] = return x
maximumMay (x:xs) = if val > x
                    then maximumMay xs
                    else return x
    where (Just val) = maximumMay xs

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay [x] = return x
minimumMay (x:xs) = if val < x
                    then minimumMay xs
                    else return x
    where (Just val) = minimumMay xs


-- -- CHAINS OF FAILTING COMPUTATIONS
-- ----------------------------------------

queryGreek :: GreekData -> String -> Maybe Double
queryGreek gData key = blah
    where xs = lookupMay key gData
          d = xs `bind` tailMay `bind` maximumMay `bind` (\x -> return (fromIntegral x))
          n = xs `bind` headMay `bind` (\y -> return (fromIntegral y))


          -- LOW BROW VERSION
          -- can do it like this: more explicit
          div = n `bind` (\x -> d `bind` (\y -> divMay y x))

          -- HIGH BROW VERSION
          -- or use liftM and combine
          -- lift the function divMay to operate on monadic inputs, result is
          -- something like Maybe (Maybe Double). then use combine to get rid of
          -- the Maybe in front. thus giving Maybe Double
          blah = combine $ liftM2 divMay d n


-- -- -- CHAINING VARIATIONS
-- -- ----------------------------------------

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries salaries person1 person2 = tot 
    where salary1May = lookupMay person1 salaries
          salary2May = lookupMay person2 salaries
          tot = liftM2 (+) salary1May salary2May

-- need to construct Maybe context on-the-fly for a given value.
-- analogous to the 'return' func in a monad typeclass def mkMaybe :: a -> Maybe a
mkMaybe x = return x


-- -- TAILPROD
-- ----------------------------------------
tailProd :: Num a => [a] -> Maybe a
tailProd [x] = return x -- or Just 1 as said in monad challenges??
tailProd xs = tailMay xs `bind` (\tail -> return $ product tail) 

tailSum :: Num a => [a] -> Maybe a
tailSum [x] = return x
tailSum xs = tailMay xs `bind` (\tail -> return $ sum tail)

transMaybe :: Num a => [a] -> ([a] -> Maybe [b]) -> ([b] -> c) -> Maybe c
transMaybe xs f g = f xs `bind` (\vals -> return $ g vals) 

transMaybe' :: (a -> b) -> Maybe a -> Maybe b
transMaybe' f Nothing = Nothing
transMaybe' f (Just x) = return (f x)

tailProd2 :: Num a => [a] -> Maybe a
tailProd2 vals = transMaybe vals tailMay product

tailSum2 :: Num a => [a] -> Maybe a
tailSum2 vals = transMaybe vals tailMay sum

tailProd3 :: Num a => [a] -> Maybe a
tailProd3 vals = transMaybe' product $ tailMay vals

tailSum3 :: Num a => [a] -> Maybe a
tailSum3 vals = transMaybe' sum $ tailMay vals 

-- -- -- implement a min and max of tail using transMaybe' and
-- -- -- previously defined funcs, maximumMay and minimumMay
tailMax :: Ord a => [a] -> Maybe (Maybe a)
tailMax vals = transMaybe' maximumMay $ tailMay vals 

tailMin :: Ord a => [a] -> Maybe (Maybe a)
tailMin vals = transMaybe' minimumMay $ tailMay vals
