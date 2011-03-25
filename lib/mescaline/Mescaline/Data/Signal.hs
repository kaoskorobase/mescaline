-- | This module is an adapted version of 'Euterpea.UI.Signal' from <http://code.haskell.org/Euterpea>.
--
-- This is the original copyright notice:
--
-- Copyright (c) 2008-2009 Paul Hudak <paul.hudak@yale.edu> 
-- 
-- This software is provided 'as-is', without any express or implied
-- warranty. In no event will the authors be held liable for any damages
-- arising from the use of this software.
-- 
-- Permission is granted to anyone to use this software for any purpose,
-- including commercial applications, and to alter it and redistribute it
-- freely, subject to the following restrictions:
-- 
-- 1. The origin of this software must not be misrepresented; you must not
--    claim that you wrote the original software. If you use this software
--    in a product, an acknowledgment in the product documentation would
--    be appreciated but is not required.
-- 
-- 2. Altered source versions must be plainly marked as such, and must not
--    be misrepresented as being the original software.
-- 
-- 3. This notice may not be removed or altered from any source
--    distribution.

module Mescaline.Data.Signal where

import Control.Applicative
import Control.Concurrent.Chan

-- | Signals are represented as streams.
newtype Signal p a = Signal { unS :: [a] }

-- | Event streams are represented by Maybe type.
type EventS p a = Signal p (Maybe a)

-- Signals are of Functor and Applicative class.

instance Functor (Signal p) where
    fmap f (Signal l) = Signal (fmap f l)

instance Applicative (Signal p) where
    pure x = Signal s where s = x : s
    (Signal (f:fs)) <*> (Signal (x:xs)) = initS  (f x) (Signal fs <*> Signal xs)

-- Signal functions

constant :: a -> Signal p a
constant = pure

lift0 :: a -> Signal p a
lift0 = pure

signal :: [a] -> Signal p a
signal = Signal

lift, lift1 :: (a -> b) -> Signal p a -> Signal p b
lift = fmap 
lift1 = lift

lift2 :: (a -> b -> c) -> Signal p a -> Signal p b -> Signal p c
lift2 f x y = (pure f <*> x) <*> y

lift3 :: (a -> b -> c -> d) -> Signal p a -> Signal p b -> Signal p c -> Signal p d
lift3 f x y z = ((pure f <*> x) <*> y) <*> z

lift4 :: (a -> b -> c -> d -> e) -> Signal p a -> Signal p b -> Signal p c -> 
         Signal p d -> Signal p e
lift4 f w x y z = (((pure f <*> w) <*> x) <*> y) <*> z

lift5 :: (a -> b -> c -> d -> e -> f) -> Signal p a -> Signal p b -> Signal p c -> 
         Signal p d -> Signal p e -> Signal p f
lift5 f v w x y z = ((((pure f <*> v) <*> w) <*> x) <*> y) <*> z

zipS :: Signal p a -> Signal p b -> Signal p (a,b)
zipS  = lift2 (,)

join :: Signal p a -> Signal p b -> Signal p (a,b)
join  = zipS

join2 :: Signal p a -> Signal p b -> Signal p (a,b)
join2 = join

join3 :: Signal p a -> Signal p b -> Signal p c -> Signal p (a,b,c)
join3 = lift3 (,,)

join4 :: Signal p a -> Signal p b -> Signal p c -> Signal p d ->
         Signal p (a,b,c,d)
join4 = lift4 (,,,)

join5 :: Signal p a -> Signal p b -> Signal p c -> Signal p d ->
         Signal p e -> Signal p (a,b,c,d,e)
join5 = lift5 (,,,,)

unzipS :: Signal p (a, b) -> (Signal p a, Signal p b)
unzipS (Signal s) = let t = unzip s in (Signal (fst t), Signal (snd t))
    where 
        unzip (x:xs) = (fst x : us, snd x : vs) where (us, vs) = unzip xs

split :: Signal p (a, b) -> (Signal p a, Signal p b)
split = unzipS

fstS :: Signal p (a, b) -> Signal p a
fstS = lift fst

sndS :: Signal p (a, b) -> Signal p b
sndS = lift snd

initS :: a -> Signal p a -> Signal p a
initS x (Signal xs) = Signal (x : xs)

delay :: a -> Signal p a -> Signal p a
delay = initS

initS' :: a -> Signal p a -> Signal p a
initS' x (Signal xs) = Signal (x : tail xs)

instance Show (Signal p a) where
    show _ = "<< signal >>"

instance Eq (Signal p a) where
    (==) = error "Cannot compare signals"

instance Num a => Num (Signal p a) where
    (+) = lift2 (+)
    (*) = lift2 (*)
    negate = lift negate
    abs = lift abs
    signum = lift signum
    fromInteger = pure . fromInteger

instance Fractional a => Fractional (Signal p a) where
    (/) = lift2 (/)
    fromRational = pure . fromRational

instance Floating a => Floating (Signal p a) where
    pi    = pure pi
    sqrt  = lift sqrt
    exp   = lift exp
    log   = lift log
    sin   = lift sin
    cos   = lift cos
    tan   = lift tan
    asin  = lift asin
    acos  = lift acos
    atan  = lift atan
    sinh  = lift sinh
    cosh  = lift cosh
    tanh  = lift tanh
    asinh = lift asinh
    acosh = lift acosh
    atanh = lift atanh

-- Event Functions

edge (Signal l) = Signal (aux False l)
    where
        aux False (True:xs) = Just () : aux True xs
        aux _ (x:xs) = Nothing : aux x xs 

(>*) :: Ord a => Signal p a -> Signal p a -> Signal p Bool
(>*) = lift2 (>)

(<*) :: Ord a => Signal p a -> Signal p a -> Signal p Bool
(<*) = lift2 (<)

(==*) :: Eq a => Signal p a -> Signal p a -> Signal p Bool
(==*) = lift2 (==)

(/=*) :: Eq a => Signal p a -> Signal p a -> Signal p Bool
(/=*) = lift2 (/=)

(>=*) :: (Eq a, Ord a) => Signal p a -> Signal p a -> Signal p Bool
(<=*) = lift2 (<=)

(<=*) :: (Eq a, Ord a) => Signal p a -> Signal p a -> Signal p Bool
(>=*) = lift2 (>=)

(&&*) :: Signal p Bool -> Signal p Bool -> Signal p Bool
(&&*) = lift2 (&&)

(||*) :: Signal p Bool -> Signal p Bool -> Signal p Bool
(||*) = lift2 (||)

notS :: Signal p Bool -> Signal p Bool
notS = lift1 not

switch :: Signal p a -> EventS p (Signal p a) -> Signal p a
Signal x `switch` Signal e = Signal (loop e x)
    where loop ~(e:es) ~(x:xs) = x : maybe (loop es xs) ((loop es) . unS) e

untilS :: Signal p a -> EventS p (Signal p a) -> Signal p a
Signal x `untilS` Signal e = Signal (loop e x)
    where loop ~(e:es) ~(x:xs) = x : maybe (loop es xs) unS e

(=>>) :: EventS p a -> (a -> b) -> EventS p b
(=>>) s f = fmap (fmap f) s

(->>) :: EventS p a -> b -> EventS p b
e ->> v = e =>> \_ -> v

never :: EventS p a
never = pure Nothing

once :: a -> EventS p a
once a = Signal (Just a : repeat Nothing)

now :: EventS p ()
now = once ()

when :: Signal p Bool -> EventS p ()
when x = unique x ->> () 

unique :: Eq a => Signal p a -> EventS p a
unique x = lift2 aux x (initS Nothing (fmap Just x))
    where
        aux x y | Just x == y = Nothing
                | otherwise   = Just x

integral :: Signal p Double -> Signal p Double -> Signal p Double
integral t x = i where
    i = initS 0 (lift3 aux dt x i)
    dt = initS' 0 (lift2 (-) t (initS 0 t))
    aux dt x i = i + dt * x

(.|.) ::  EventS p a ->  EventS p a ->  EventS p a 
(.|.) = lift2 aux where
    aux Nothing y = y
    aux x       _ = x 

mergeWithE :: (a->a->a) -> EventS p a -> EventS p a -> EventS p a
mergeWithE op = lift2 aux where
  aux (Just x) (Just y) = Just (x `op` y)
  aux e1       Nothing  = e1
  aux Nothing  e2       = e2

snapshot :: EventS p a -> Signal p b -> EventS p (a, b)
snapshot = lift2 aux where
    aux Nothing  y = Nothing
    aux (Just x) y = Just (x, y)

snapshot_ :: EventS p a -> Signal p b -> EventS p b
snapshot_ e b = e `snapshot` b =>> snd

hold :: a -> EventS p a -> Signal p a
hold i (Signal e) = Signal (f i e)
    where
        f i (x:xs) = case x of
            Just y  -> y : f y xs
            Nothing -> i : f i xs

-- step :: a -> EventS p a -> Signal p a
-- step = hold

accum :: a -> EventS p (a -> a) -> Signal p a
accum i (Signal e) = Signal (i : f i e)
    where
        f i (x:xs) = let i' = maybe i ($i) x
                      in i' : f i' xs

-- stepAccum :: a -> EventS p (a -> a) -> Signal p a
-- stepAccum = accum

makeStream :: IO (Signal p a, a -> IO ())
makeStream = do
    ch <- newChan
    contents <- getChanContents ch
    return (Signal contents, writeChan ch)
