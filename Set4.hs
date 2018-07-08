{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax#-}

module Set4 where

import MCPrelude

-- look at the following type signatures from Set1.hs and Set2.hs.
-- find commonalities
--
-- -- from Set1.hs
-- genTwo :: Gen a -> (a -> Gen b) -> Gen b
-- generalA :: (a -> b) -> Gen a -> Gen b 
-- generalB :: (a -> b -> e) -> Gen a -> Gen b -> Gen e
-- mkGen :: a -> Gen a

-- -- from Set2.hs
-- link :: Maybe a -> (a -> Maybe b) -> Maybe b
-- transMaybe' :: (a -> b) -> Maybe a -> Maybe b
-- yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
-- mkMaybe :: a -> Maybe a


-- pattern1 :: m a -> (a -> m b) -> m b
-- pattern2 :: (a -> b) -> m a -> m b
-- pattern3 :: (a -> b -> c) -> m a -> m b -> m c
-- pattern4 :: a -> m a

-- FORMALIZING THE PATTERN
--------------------------------------------------
class Monad m where
    bind :: m a -> (a -> m b) -> m b
    return :: a -> m a

--------------------------------------------------
-- now defined in Set2_monadic_version.hs
-- instance Monad Maybe where
--     bind Nothing _ = Nothing
--     bind (Just a) f = f a
--     return a = Just a


--------------------------------------------------
-- now defined in Set3_monadic_version.hs
-- instance Monad [] where
--     bind [] _ = []
--     -- always get this wrong for list. it should be recursive, not like the following
--     -- bind [a] f = f a
--     bind (a:as) f = f a ++ bind as f
--     return a = [a]


--------------------------------------------------
-- now defined in Set1_monadic_version.hs
-- we defined (Gen a) previously using a type alias. this method doesn't work when
-- making types an instance of some type class. instead, we need to use the 
-- `newtype` keyword; it wraps up the datatype as specified in getGen method below,
-- into be the (Gen a) type. 
--
-- ((( you could use the `data` keyword but we're not really
-- creating a new datatype, we're just making a 'convenience' type.)))
--
-- newtype Gen a = Gen { getGen :: Seed -> (a,Seed)}

-- instance (Show a) => Show (Gen a) where
--     show gen = "Gen " ++ show (fst ((getGen gen) (mkSeed 100)))

-- evalGen :: Gen a -> Seed -> a
-- evalGen gen s = fst $ (getGen gen) s

-- instance Monad Gen where
--     return a = Gen { getGen=(\s -> (a,s)) } 
--     bind ma f = Gen { getGen=(\s -> let (a, s') = getGen ma s in getGen (f a) s') }

-- playing around with newtype and showing it.
-- example taken from LYAH 
-- http://learnyouahaskell.com/functors-applicative-functors-and-monoids#the-newtype-keyword
-- newtype ZipList a = ZipList {getZipList :: [a]}
--
-- instance Show a => Show (ZipList a) where
--     show zList = "ZipList " ++ show (getZipList zList)

--------------------------------------------------



-- REVISITING OTHER GENERIC FUNCTIONS
----------------------------------------------------


-- rewrite generalA func
--
-- generalA :: (a -> b) -> Gen a -> Gen b
liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m1 = m1 `bind` (\x -> return (f x))

-- rewrite generalB2/ yLink func
--
-- generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f m1 m2 = m1 `bind` (\x -> m2 `bind` (\y -> return (f x y)))

-- rewrite repRandom' func
--
-- repRandom' :: [Gen a] -> Gen []
-- repRandom' [] = mkGen []
-- repRandom' (g:gs) = genTwo g (\x -> genTwo (repRandom' gs) (\y -> mkGen (x:y)))
sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (g:gs) = bind g (\x -> bind (sequence gs) (\y -> return (x:y)))

-- rewrite chain func
--
-- chain :: (a -> Maybe b) -> Maybe a -> Maybe b
-- chain f Nothing = Nothing
-- chain f (Just x) = f x
(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) f ma = bind ma f


-- rewrite combine func
--
-- combine :: Maybe (Maybe a) -> Maybe a
-- combine Nothing = Nothing
-- combine (Just (Just a)) = Just a
combine :: Monad m => m (m a) -> m a
combine mma = bind mma (\x -> x)

-- rewrite allCombs func
--
-- allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
-- this is the same as liftM2


-- rewrite allCombs3 func
--
-- use this as inspiration for writing liftM3:
-- liftM2 f m1 m2 = m1 `bind` (\x -> m2 `bind` (\y -> return (f x y)))
--
-- allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f ma mb mc = ma `bind` (\x -> mb `bind` (\y -> mc `bind` (\z -> return (f x y z))))

-- rewrite combStep func
-- combStep :: [a -> b] -> [a] -> [b]
ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma = bind mf (\f -> bind ma (\a -> return (f a)))
