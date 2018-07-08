{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax#-}

module Set5 where

import MCPrelude

--------------------------------------------------
-- SET 5. DO NOTATION
--------------------------------------------------
--
-- THEORY
-- --------
-- do notation makes working with monads easier and more terse.
--
-- rule 1 = do
--    foo <- calcFoo
--    bar foo
--
-- GHC automatically desugars this to:
--
-- rule1 = bind calcFoo (\foo -> bar foo)
--
-- it's also important to note what the types are of each line,
-- and the resulting 'rule 1' expression:
-- 
-- rule1 :: m b
-- calcFoo :: m a
-- bar :: a -> m b
--
-- the key is that whatever the 'm' is, it must be the same for all 3 
-- of these types.
--
--
--
-- haskell doesn't use the function name 'bind' that we've been using.
-- instead, it calls it:
--
-- 'bind' is '>>='
--
-- and rule1 becomes:
--
-- rule1 = calcFoo >>= (\foo -> bar foo)
--
--
-- we need to change our Monad typeclass defintion to reflect this.
-- for historical reasons, Monad is required to have a 'fail' function.
-- let's leave it as undefined for now.
--
-- note: when you implement Monad for your own datatypes you should
--       only implement the (>>=) and return functions.
--------------------------------------------------

class Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a

    fail :: String -> m a
    fail = undefined


sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (g:gs) = g >>= (\x -> (sequence gs) >>= (\y -> return (x:y)))

--------------------------------------------------
-- DO NOTATION - SET 1
--------------------------------------------------
newtype Gen a = Gen { getGen :: Seed -> (a,Seed)}

evalGen :: Gen a -> Seed -> a
evalGen gen s = fst $ (getGen gen) s

instance (Show a) => Show (Gen a) where
    show gen = "Gen { getGen=(s -> ("
               ++ show (evalGen gen (mkSeed 1337))
               ++ ", s)}"


instance Monad Gen where
    (>>=) ma f = Gen { getGen=(\s -> let (a,s') = getGen ma s
                                     in getGen (f a) s')} 
    return a = Gen { getGen=(\s -> (a,s))}

makeRandom :: Gen Integer
makeRandom = Gen { getGen=rand}

-- test: 
-- foldl (*) 1 (evalGen fiveRands $ mkSeed 1)
-- == 8681089573064486461641871805074254223660
-- YES.
fiveRands :: Gen [Integer]
fiveRands = do
    sequence $ take 5 $ repeat makeRandom

randLetter :: Gen Char
randLetter = do
    -- makeRandom has type Gen Integer, so using '<-'
    -- on it inside the do-notation, gives us the Integer val
    -- that's pretty cool considering our Gen Integer is a newtype
    -- instance with a getGen val being a function.
    randVal <- makeRandom

    -- using return will wrap up whatever is in the () 
    -- to be whatever type randLetter is defined to be,
    -- here it's Gen Char
    return (toLetter $ randVal `mod` 26)

randString3 :: Seed -> [Char]
randString3 = let genChars = do 
                      c1 <- randLetter
                      c2 <- randLetter
                      c3 <- randLetter
                      return [c1,c2,c3]
              in evalGen genChars


--------------------------------------------------
-- DO NOTATION - SET 2
--------------------------------------------------

data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
    show Nothing = "Nothing"
    show (Just a) = "Just " ++ show a

instance Monad Maybe where
    (>>=) Nothing _ = Nothing
    (>>=) (Just a) f = f a
    return a = Just a




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

queryGreek :: GreekData -> String -> Maybe Double
queryGreek gData key = let xs = lookupMay key gData
                           denom = do
                               lookupResults <- xs
                               tail <- tailMay lookupResults
                               maximum <- maximumMay tail
                               return $ fromIntegral maximum
                           numer = do
                               lookupResults <- xs
                               head <- headMay lookupResults
                               return $ fromIntegral head
                           div = do
                               numerRaw <- numer
                               denomRaw <- denom
                               divMay denomRaw numerRaw 

                       in div


addSalaries :: [(String,Integer)] -> String -> String -> Maybe Integer
addSalaries salaries p1 p2 = do
    p1Salary <- lookupMay p1 salaries
    p2Salary <- lookupMay p2 salaries
    return (p1Salary + p2Salary)


tailProd :: Num a => [a] -> Maybe a
tailProd xs = do
    tail <- tailMay xs
    return $ product tail

tailSum :: Num a => [a] -> Maybe a
tailSum xs = do
    tail <- tailMay xs
    return $ sum tail

tailMax :: Ord a => [a] -> Maybe a
tailMax xs = do
    tail <- tailMay xs
    maximumMay tail


--------------------------------------------------
-- DO NOTATION - SET 3
--------------------------------------------------

data Card = Card Int String

instance Show Card where
    show (Card a b) = show a ++ b

instance Monad [] where
    (>>=) [] _ = []
    (>>=) (a:as) f = f a ++ (>>=) as f
    return a = [a]


-- EXPLANATION FOR HOW [] BEHAVIOURS IN DO-NOTATION
--
-- this gives all permutations of inputs as pairs in a list.
-- below it looks on the face of it that we are just using a
-- single value for 'a' and a single value for 'b'. but
-- it actually peforms all possible permutations because
-- of the way (<<=) is defined in our [] monad instance. namely,
--
--    (>>=) (a:as) f = f a ++ (>>=) as f
--
-- see that this recursively calls (>>=). and if you desugar the do-notation
-- you get a nested call to (>>=) twice. eg. v1 = [1,2] , v2 = [3,4] and
-- [1,2] <<= (\x -> [3,4] <<= (\y -> return $ (x,y)))
--
-- which gets exapanded out to (due to the (<<=) definition for [] ):
-- 
--     1 <<= (\1 -> [3,4] <<= (\y -> return $ (1,y)))
--     ++ 
--     2 <<= (\2 -> [3,4] <<= (\y -> return $ (2,y)))
--
-- and expanded further for the [3,4] list:
--
--     1 <<= (\1 -> 3 <<= (\3 -> return $ (1,3)))
--     ++ 
--     1 <<= (\1 -> 4 <<= (\4 -> return $ (1,4)))
--     ++
--     2 <<= (\2 -> 3 <<= (\3 -> return $ (2,3)))
--     ++
--     2 <<= (\2 -> 4 <<= (\4 -> return $ (2,4)))
--
-- which is just:
-- [(1,3)] ++ [(1,4)] ++ [(2,3)] ++ [(2,4)] = [(1,3),(1,4),(2,3),(2,4)]
--
-- cool.


-- STOKED: THE REDUCTION IN CODE COMPLEXITY WHEN USING THE CORRECT ABSTRACTION (Monad)
-- IS INSANE HERE!!! 
-- In the words of our Hollywood poet, Owen Wilson... "Wow"
allPairs :: [a] -> [b] -> [(a,b)]
allPairs v1 v2 = do
    a <- v1
    b <- v2
    return (a,b)
    
allCards :: [Int] -> [String] -> [Card]
allCards cNums cClasses = do
    n <- cNums
    c <- cClasses
    return $ Card n c

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f v1 v2 v3 = do
    a <- v1
    b <- v2
    c <- v3
    return $ f a b c
