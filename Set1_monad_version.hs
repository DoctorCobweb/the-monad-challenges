{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax#-}

module Set1_monad_version where

import MCPrelude
import Set4


--------------------------------------------------
-- SET 1. RANDOM NUMBERS
--------------------------------------------------

-- RANDOM NUMBER GENERATION
----------------------------------------


-- type Gen a = Seed -> (a, Seed)
newtype Gen a = Gen { getGen ::Seed -> (a, Seed)}

instance (Show a) => Show (Gen a) where
    show gen = "Gen { getGen=(s -> ("  
                ++ show (fst ((getGen gen) (mkSeed 13333))) 
                ++ ", s)}"

evalGen :: Gen a -> Seed -> a
evalGen gen s = fst $ (getGen gen) s

instance Monad Gen where
    return a = Gen { getGen=(\s -> (a,s))}
    bind ma f = Gen { getGen=(\s -> let (a,s') = getGen ma s
                                    in getGen (f a) s')}

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
-- can this be redone using more monadic-esque style??
randLetter :: Gen Char
randLetter = Gen {getGen=(\s-> let (x, s') = rand s
                                   randChar = toLetter $ x `mod` 26
                               in (randChar, s'))}

-- THIS DOESNT WORK. ALWAYS OUTPUTS A VAL OF 'a'.
-- DONT FULLY UNDERSTAND WHY YET.
-- randLetter :: Gen Char
-- randLetter = liftM (toLetter . (mod 26)) Gen {getGen=rand} 


-- "lrf" should be the output for mkSeed 1
randString3 :: Seed -> [Char]
randString3 = evalGen $ sequence [randLetter,randLetter,randLetter]


-- MORE GENERATORS
----------------------------------------
-- output of rand * 2
-- rand has type Seed -> (a, Seed) which is just what we want for getGen func
randEven :: Gen Integer
randEven = Gen {getGen=rand} `bind` (\val -> return (val*2))

-- -- output of rand * 2 + 1
randOdd :: Gen Integer
randOdd = randEven `bind` (\val -> return (val + 1))

-- output of rand * 10
randTen :: Gen Integer
randTen = Gen {getGen=rand} `bind` (\val -> return (val*10))


-- IMPORTANT: generalize this behaviour of the three functions
--            above
generalA :: (a -> b) -> Gen a -> Gen b
generalA = liftM
-- generalA :: (a -> b) -> Gen a -> Gen b 
-- generalA f gen = \s -> let (val, newSeed) = gen s
--                    in (f val, newSeed)
       

-- rewrite in terms of generalA/ liftM
--
-- randEven' :: Gen Integer
-- randEven'= generalA (*2) rand
randEven' :: Gen Integer
randEven' = liftM (*2) Gen{getGen=rand}

-- rewrite in terms of generalA/ liftM
-- 
-- randOdd' :: Gen Integer
-- randOdd' = generalA (+1) randEven'
randOdd' :: Gen Integer
randOdd' = liftM (+1) randEven'

-- rewrite in terms of generalA/ liftM
--
-- randTen' :: Gen Integer
-- randTen' = generalA (*10) rand
randTen' :: Gen Integer
randTen' = liftM (*10) Gen{getGen=rand}

-- GENERALIZING RANDOM PAIRS
----------------------------------------
--
-- randPair :: Gen (Char, Integer)
-- randPair = \s -> let (charVal, newSeed1) = randLetter s
--                      (intVal, newSeed2) = rand newSeed1
--                  in ((charVal, intVal), newSeed2)

-- -- define a function which makes generalized pairs
-- --
-- -- expanded func sig is really
-- -- generalPair :: (Seed -> (a,Seed))-> (Seed, (b,Seed)) -> Seed -> ((a,b),Seed)
-- generalPair :: Gen a -> Gen b -> Gen (a,b)
-- generalPair g1 g2 = \s -> let (a, s1) = g1 s
--                               (b, s2) = g2 s1
--                           in ((a,b),s2)


-- even more general, allowing you to input an arbitrary func
-- to use for creating our 'combined' value aka the (a,b) tuple above
-- generalB :: (a -> b -> e) -> Gen a -> Gen b -> Gen e
-- generalB f g1 g2 = \s -> let (a, s1) = g1 s
--                              (b, s2) = g2 s1
--                          in (f a b, s2)
generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB = liftM2

-- construct a generalPair-like func that uses generalB
-- generalPair' :: Gen a -> Gen b -> Gen (a,b)
-- generalPair' = generalB (\r s -> (r,s))
generalPair' :: Gen a -> Gen b -> Gen (a,b)
generalPair' = liftM2 (,)


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

-- the below type sig is really == repRandom :: [Gen a] -> Seed -> ([a], Seed)
-- repRandom :: [Gen a] -> Gen [a]
-- repRandom [] s = ([],s)
-- repRandom (x:xs) s = (newVal : fst (repRandom xs newSeed), newSeed)
--     where (newVal, newSeed) = x s
repRandom :: [Gen a] -> Gen [a]
repRandom = sequence


-- THREADING THE RANDOM NUMBER STATE
----------------------------------------
-- simpler idea is to have a function that does one step of two generators
-- and the necessary state threading.
-- genTwo :: Gen a -> (a -> Gen b) -> Gen b
-- genTwo g1 f s = (newVal, s3)
--     where (val, s2) = g1 s
--           (newVal,s3) = (f val) s2
genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo = bind


-- reimplementation of generalB to leverage genTwo function.
-- ((( even more general, allowing you to input an arbitrary func
-- to use for creating our 'combined' value aka the (a,b) tuple above )))
-- generalB2 :: (a -> b -> e) -> Gen a -> Gen b -> Gen e
-- generalB2 f g1 g2 = g1 `genTwo` (\x ->  g2 `genTwo` (\y -> mkGen (f x y) ))
generalB2 :: (a -> b -> e) -> Gen a -> Gen b -> Gen e
generalB2 = liftM2

-- reimplementation of repRandom using only. this hurt my head and a caved into
-- looking up online for help.
-- 1. generalA
-- 2. genTwo
-- 3. mkGen
-- repRandom' :: [Gen a] -> Gen [a]
-- repRandom' [] = mkGen []
-- repRandom' (g:gs) = genTwo g (\x -> genTwo (repRandom' gs) (\y -> mkGen (x:y)))
repRandom' :: [Gen a] -> Gen [a]
repRandom' = sequence

-- i think this is like the 'return' or 'pure' function associated with
-- the monad typeclass. 
-- "make a Gen a 'out of thin air' "
-- mkGen :: a -> Gen a
-- mkGen val = \s -> (val, s) 
mkGen :: a -> Gen a
mkGen a = return a
