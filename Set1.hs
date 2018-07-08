{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax#-}

module Set1 where

import MCPrelude


--------------------------------------------------
-- SET 1. RANDOM NUMBERS
--------------------------------------------------

-- RANDOM NUMBER GENERATION
----------------------------------------


type Gen a = Seed -> (a, Seed)

-- instance (Show a) => Show (Gen a) where
--     show Gen = "seed -> " + "(" + show a + ", seed)"

fiveRands :: [Integer]
fiveRands = [r1, r2, r3, r4, r5]
    where startSeed = mkSeed 1 
          (r1, s2) = rand startSeed
          (r2, s3) = rand s2
          (r3, s4) = rand s3
          (r4, s5) = rand s4
          (r5, s6) = rand s5


-- RANDOM CHARACTER GENERATION
----------------------------------------
-- randLetter :: Seed -> (Char, Seed)
randLetter :: Gen Char
randLetter seed = (randChar, newSeed)
    where (n1, newSeed) = rand seed
          randChar = toLetter $ n1 `mod` 26 

randString3 :: Seed -> [Char]
randString3 seed = [l1,l2,l3]
    where (l1, s2) = randLetter seed
          (l2, s3) = randLetter s2
          (l3, s4) = randLetter s3
          

-- MORE GENERATORS
----------------------------------------
-- output of rand * 2
randEven :: Gen Integer
randEven = \s -> let (val, newSeed) = rand s
                 in (val*2, newSeed)


-- output of rand * 2 + 1
randOdd :: Gen Integer
randOdd = \s -> let (even, newSeed) = randEven s
                in (even +1, newSeed) 


-- -- output of rand * 10
randTen :: Gen Integer
randTen = \s -> let (val, newSeed) = rand s
                in (val * 10, newSeed)

-- IMPORTANT: generalize this behaviour of the three functions
--            above
generalA :: (a -> b) -> Gen a -> Gen b 
generalA f gen = \s -> let (val, newSeed) = gen s
                   in (f val, newSeed)
       
-- rewrite in terms of generalA
randEven' :: Gen Integer
randEven'= generalA (*2) rand

randOdd' :: Gen Integer
randOdd' = generalA (+1) randEven'

randTen' :: Gen Integer
randTen' = generalA (*10) rand


-- GENERALIZING RANDOM PAIRS
----------------------------------------
--
randPair :: Gen (Char, Integer)
randPair = \s -> let (charVal, newSeed1) = randLetter s
                     (intVal, newSeed2) = rand newSeed1
                 in ((charVal, intVal), newSeed2)

-- define a function which makes generalized pairs
--
-- expanded func sig is really
-- generalPair :: (Seed -> (a,Seed))-> (Seed, (b,Seed)) -> Seed -> ((a,b),Seed)
generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair g1 g2 = \s -> let (a, s1) = g1 s
                              (b, s2) = g2 s1
                          in ((a,b),s2)


-- even more general, allowing you to input an arbitrary func
-- to use for creating our 'combined' value aka the (a,b) tuple above
generalB :: (a -> b -> e) -> Gen a -> Gen b -> Gen e
generalB f g1 g2 = \s -> let (a, s1) = g1 s
                             (b, s2) = g2 s1
                         in (f a b, s2)

-- construct a generalPair-like func that uses generalB
generalPair' :: Gen a -> Gen b -> Gen (a,b)
generalPair' = generalB (\r s -> (r,s))


-- GENERALIZING LISTS OF GENERATORS
----------------------------------------
-- make a function that lets you give it a list of generators [Gen a], 
-- and automatically handle the 'state threading' which just means
-- passing of new seed to the next function/generator in lin
--
-- ((( TRICKIER version using foldl )))
-- repRandom :: [Gen a] -> Gen [a]
-- repRandom gens = (\s -> let (vals, newSeed) = foldl 
--                                                 (\acc x -> (fst (x (snd acc)) : fst acc, snd (x (snd acc)))) 
--                                                 ([],s) 
--                                                 gens
--                         in (reverse vals, newSeed))
--
--
--
-- another version of it using recursion.
-- to see further how it works, expand the type synonym
-- repRandom :: [Gen a] -> Seed -> ([a], Seed)
repRandom :: [Gen a] -> Gen [a]
repRandom [] s = ([],s)
repRandom (x:xs) s = (newVal : fst (repRandom xs newSeed), newSeed)
    where (newVal, newSeed) = x s


-- THREADING THE RANDOM NUMBER STATE
----------------------------------------
-- simpler idea is to have a function that does one step of two generators
-- and the necessary state threading.
genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo g1 f s = (newVal, s3)
    where (val, s2) = g1 s
          (newVal,s3) = (f val) s2


-- type Gen a = Seed -> (a, Seed)

-- reimplementation of generalB to leverage genTwo function.
-- ((( even more general, allowing you to input an arbitrary func
-- to use for creating our 'combined' value aka the (a,b) tuple above )))
generalB2 :: (a -> b -> e) -> Gen a -> Gen b -> Gen e
generalB2 f g1 g2 = g1 `genTwo` (\x ->  g2 `genTwo` (\y -> mkGen (f x y) ))

-- reimplementation of repRandom using only. this hurt my head and a caved into
-- looking up online for help.
-- 1. generalA
-- 2. genTwo
-- 3. mkGen
-- repRandom' :: [Gen a] -> Seed -> ([a], Seed)
repRandom' :: [Gen a] -> Gen [a]
repRandom' [] = mkGen []
repRandom' (g:gs) = genTwo g (\x -> genTwo (repRandom' gs) (\y -> mkGen (x:y)))

-- repRandom' :: [Gen a] -> Gen [a]
-- repRandom' [] s = ([],s)
-- repRandom' (x:xs) s = (newVal : fst (repRandom' xs newSeed), newSeed)
--     where (newVal, newSeed) = x s

-- generalA :: (a -> b) -> Gen a -> Gen b 
-- generalA f gen = \s -> let (val, newSeed) = gen s
--                    in (f val, newSeed)




-- i think this is like the 'return' or 'pure' function associated with
-- the monad typeclass. 
-- "make a Gen a 'out of thin air' "
mkGen :: a -> Gen a
mkGen val = \s -> (val, s) 
