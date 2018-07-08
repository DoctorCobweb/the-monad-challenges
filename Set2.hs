{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax#-}

module Set2 where

import MCPrelude

-- THE MAYBE TYPE
----------------------------------------
data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
    show Nothing = "Nothing"
    show (Just a) = "Just " ++ show a


-- BUILD A LIBRARY OF THINGS THAT CAN FAIL
----------------------------------------
-- make some safe versions of serveral common functions usually
-- specified in prelude
headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (x:xs) = Just xs

-- lookup can fail for two reasons 
-- i) inputted empty list
-- ii) there's no tuple in the list having the lookup key
lookupMay :: Eq a => a -> [(a,b)] -> Maybe b
lookupMay key [] = Nothing
lookupMay key tupleList = if length results == 0
                          then Nothing
                          else Just (snd (results !! 0))
    where results = filter ( \(x,y) -> x == key) tupleList

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay numerator denominator = Just (numerator / denominator)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay [x] = Just x
maximumMay (x:xs) = if val > x
                    then maximumMay xs
                    else Just x
    where (Just val) = maximumMay xs

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay [x] = Just x
minimumMay (x:xs) = if val < x
                    then minimumMay xs
                    else Just x
    where (Just val) = minimumMay xs


-- CHAINS OF FAILTING COMPUTATIONS
----------------------------------------
queryGreek :: GreekData -> String -> Maybe Double
queryGreek gData key = mDiv
    where xs = lookupMay key gData
          mDenom = case xs 
                   of Nothing -> Nothing
                      _ -> case (\(Just queryResults) -> tailMay queryResults) xs
                           of Nothing -> Nothing
                              mTail -> case (\(Just tailVal) -> maximumMay tailVal) mTail
                                       of Nothing -> Nothing
                                          mTailMax -> mTailMax
                                
                        
                        
          mNumer = case xs
                   of Nothing -> Nothing
                      _ -> case (\(Just queryResults) -> headMay queryResults) xs
                           of Nothing -> Nothing
                              mHead -> mHead 

          -- BUG: for some reason i have to switch numerator and denominator in call to divMay, to get correct results
          -- I DONT KNOW WHY
          mDiv = case mNumer
                 of Nothing -> Nothing
                    jNumer -> case (\(Just numerator) -> numerator ) jNumer
                              of _ -> case mDenom
                                      of Nothing -> Nothing
                                         jDenom -> case (\(Just t) (Just b) -> divMay (fromIntegral b) (fromIntegral t)) jNumer jDenom 
                                                   of Nothing -> Nothing
                                                      result -> result


-- GENERALIZING CHAINS OF FAILURES
----------------------------------------
chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain f Nothing = Nothing
chain f (Just x) = f x

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link Nothing f = Nothing
link (Just x) f = f x

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 gData key = mFrac
    where mVals = lookupMay key gData
          mDenom = mVals `link` tailMay `link` maximumMay `link` (\x -> Just (fromIntegral x))
          mNumer = mVals `link` headMay `link` (\x -> Just(fromIntegral x))

          -- note this trickery using lambdas, partial application of diveMay and the `link` func
          mFrac = mDenom `link` (\x -> mNumer `link` (divMay x))

          

-- CHAINING VARIATIONS
----------------------------------------
salaries' :: [(String, Integer)]
salaries' =  [ ("alice", 105000)
             , ("bob", 90000)
             , ("carol", 85000)
             ]

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries salaries person1 person2 = totalSalMay
    where salary1May = lookupMay person1 salaries
          salary2May = lookupMay person2 salaries

          -- trickery again
          totalSalMay = salary1May `link` (\s1 -> salary2May `link` (\s2 -> mkMaybe (s1 + s2)) )

-- need to construct Maybe context on-the-fly for a given value.
-- analogous to the 'return' func in a monad typeclass def mkMaybe :: a -> Maybe a
mkMaybe x = Just x

-- advise typesignature
yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink f Nothing _ = Nothing
yLink f _ Nothing = Nothing
yLink f x y = x `link` (\xVal -> y `link` (\yVal -> mkMaybe (f xVal yVal) ) )

-- my attempt at making something work
yLink' :: Maybe a -> Maybe b -> (a -> b-> Maybe e) -> Maybe e
yLink' Nothing _ f = Nothing
yLink' _ Nothing f = Nothing
yLink' x y f = (\(Just xVal) -> (\(Just yVal) -> f xVal yVal ) y ) x 

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 salaries person1 person2 = totalSalMay
    where salary1May = lookupMay person1 salaries
          salary2May = lookupMay person2 salaries

          -- using the yLink abstraction it makes the code much easier to deal with Maybe context
          totalSalMay = yLink (+) salary1May salary2May


-- TAILPROD
----------------------------------------
tailProd :: Num a => [a] -> Maybe a
tailProd [x] = Just x -- or Just 1 as said in monad challenges??
tailProd xs = tailMay xs `link` (\tail -> mkMaybe (product tail)) 

tailSum :: Num a => [a] -> Maybe a
tailSum [x] = Just x
tailSum xs = tailMay xs `link` (\tail -> mkMaybe (sum tail))

transMaybe :: Num a => [a] -> ([a] -> Maybe [b]) -> ([b] -> c) -> Maybe c
transMaybe xs f g = f xs `link` (\vals -> mkMaybe (g vals)) 

transMaybe' :: (a -> b) -> Maybe a -> Maybe b
transMaybe' f Nothing = Nothing
transMaybe' f (Just x) = Just (f x)

tailProd2 :: Num a => [a] -> Maybe a
tailProd2 vals = transMaybe vals tailMay product

tailSum2 :: Num a => [a] -> Maybe a
tailSum2 vals = transMaybe vals tailMay sum

tailProd3 :: Num a => [a] -> Maybe a
tailProd3 vals = transMaybe' product $ tailMay vals

tailSum3 :: Num a => [a] -> Maybe a
tailSum3 vals = transMaybe' sum $ tailMay vals 

-- implement a min and max of tail using transMaybe' and
-- previously defined funcs, maximumMay and minimumMay
tailMax :: Ord a => [a] -> Maybe (Maybe a)
tailMax vals = transMaybe' maximumMay $ tailMay vals 

tailMin :: Ord a => [a] -> Maybe (Maybe a)
tailMin vals = transMaybe' minimumMay $ tailMay vals

combine' :: Maybe (Maybe a) -> Maybe a
combine' Nothing = Nothing
combine' (Just (Just a)) = Just a
