{-# LANGUAGE ExistentialQuantification #-}

module Mescaline.Pattern.Base where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import qualified Data.Array as A
import qualified Data.HashTable as H
import qualified Data.IntMap as M
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Monoid as M
import qualified Sound.SC3.Lang.Math.Pitch as S
import qualified System.Random as R

data P s a
    = Empty
    | Value a
    | RP (s -> (P s a, s))
    | Append (P s a) (P s a)
    | forall x . Unfoldr (s -> x -> (s, Maybe (a, x))) x
    | forall x . Continue (P s x) (x -> P s x -> P s a)
    | forall x . Apply (P s (x -> a)) (P s x)
    | forall x y . Scan (x -> y -> (x, a)) (Maybe (x -> a)) x (P s y)

data Result s a 
    = Result s a (P s a)
    | Done s

step :: s -> P s a -> Result s a
step g Empty = Done g
step g (Value a) = Result g a M.mempty
step g (RP f) =
    let (p, g') = f g
    in step g' p
step g (Append x y) =
    case step g x of
      Done g' -> step g' y
      Result g' a x' -> Result g' a (Append x' y)
step g (Continue p f) =
    case step g p of
      Done g' -> Done g'
      Result g' x p' -> step g' (f x p')
step g (Unfoldr f x) =
    let (g', y) = f g x
    in case y of
         Nothing -> Done g'
         Just (a, x') -> Result g' a (Unfoldr f x')
step g (Apply p q) =
    case step g p of
      Done g' -> Done g'
      Result g' f p' -> case step g' q of
                          Done g'' -> Done g''
                          Result g'' x q' -> Result g'' (f x) (Apply p' q')
step g (Scan f f' i p) =
    case step g p of
      Done g' -> case f' of
                   Just h -> Result g' (h i) Empty
                   Nothing -> Done g'
      Result g' a p' -> let (j, x) = f i a
                        in Result g' x (Scan f f' j p')

runP :: Monad m =>
        s -> ((a, s) -> m s) -> (b -> a -> b) -> b -> P s a -> m b
runP s u f i p = do
  case step s p of
    Done _ -> return i
    Result s' a p' -> do s'' <- u (a, s')
                         runP s'' u f (f i a) p'

pfoldr' :: s -> (a -> b -> b) -> b -> P s a -> b
pfoldr' g f i p =
    case step g p of
      Done _ -> i
      Result g' a p' -> f a (pfoldr' g' f i p')

evalP :: P () a -> [a]
evalP = pfoldr' () (:) []

evalR :: String -> P R.StdGen a -> [a]
evalR s =
    let g = R.mkStdGen (fromIntegral (H.hashString s))
    in pfoldr' g (:) []

instance (Show a) => Show (P s a) where
    show _ = show "a pattern"

instance (Eq a) => Eq (P s a) where
    _ == _ = False

instance M.Monad (P s) where
    (>>=) p f = Continue p (\x q -> f x `M.mappend` (>>=) q f)
    return = Value

instance M.MonadPlus (P s) where
    mzero = Empty
    mplus = Append

instance M.Monoid (P s a) where
    mempty = Empty
    mappend = Append

-- | Apply `f' pointwise to elements of `p' and `q'.
pzipWith :: (a -> b -> c) -> P s a -> P s b -> P s c
pzipWith f p = (A.<*>) (A.pure f A.<*> p)

instance (Num a) => Num (P s a) where
    (+) = pzipWith (+)
    (-) = pzipWith (-)
    (*) = pzipWith (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = return . fromInteger
    negate = fmap negate

instance (Fractional a) => Fractional (P s a) where
    (/) = pzipWith (/)
    recip = fmap recip
    fromRational = return . fromRational

pcycle :: P s a -> P s a
pcycle x = x `M.mappend` pcycle x

prepeat :: a -> P s a
prepeat = pcycle . return

instance Functor (P a) where
    fmap = (A.<*>) . prepeat

instance A.Applicative (P s) where
    pure = prepeat
    (<*>) = Apply

instance A.Alternative (P s) where
    empty = Empty
    (<|>) = Append

-- * Basic constructors

prp :: (s -> (P s a, s)) -> P s a
prp = RP

pinf :: P s Int
pinf = return 83886028 -- 2 ^^ 23

pcontinue :: P s x -> (x -> P s x -> P s a) -> P s a
pcontinue = Continue

pscan :: (x -> y -> (x, a)) -> Maybe (x -> a) -> x -> P s y -> P s a
pscan = Scan

punfoldr :: (s -> x -> (s, Maybe (a, x))) -> x -> P s a
punfoldr = Unfoldr

-- * Control

pfilter :: (a -> Bool) -> P s a -> P s a
pfilter f p =
    let g x p' = if f x
                 then M.mappend (return x) (pfilter f p')
                 else pfilter f p'
    in pcontinue p g

plist :: [P s a] -> P s a
plist = foldr M.mappend M.mempty

pcons :: a -> P s a -> P s a
pcons = M.mappend . return

preplicate_ :: Int -> P s a -> P s a
preplicate_ n p | n > 0 = M.mappend p (preplicate_ (n - 1) p)
                | otherwise = M.mempty

preplicate :: P s Int -> P s a -> P s a
preplicate n p = n >>= (\x -> preplicate_ x p)

pn :: P s a -> P s Int -> P s a
pn = flip preplicate

pn_ :: P s a -> Int -> P s a
pn_ = flip preplicate_

-- | 'n' initial values at 'p'.
ptake_ :: Int -> P s a -> P s a
ptake_ n p =
    let e = error "ptake_"
    in pzipWith const p (preplicate_ n (return e))

ptake :: P s Int -> P s a -> P s a
ptake n p =
    let e = error "ptake"
    in pzipWith const p (preplicate n (return e))

-- | 'n' initial values at pcycle of 'p'.
prestrict_ :: Int -> P s a -> P s a
prestrict_ n = ptake_ n . pcycle

prestrict :: P s Int -> P s a -> P s a
prestrict n = ptake n . pcycle

pmapMaybe :: (a -> Maybe b) -> P s a -> P s b
pmapMaybe f = fmap M.fromJust . pfilter M.isJust . fmap f

preject :: (a -> Bool) -> P s a -> P s a
preject f = pfilter (not . f)

pzipWith3 :: (a -> b -> c -> d) -> P s a -> P s b -> P s c -> P s d
pzipWith3 f p q = (A.<*>) (A.pure f A.<*> p A.<*> q)

pzipWith4 :: (a -> b -> c -> d -> e) -> 
             P s a -> P s b -> P s c -> P s d -> P s e
pzipWith4 f p q r = (A.<*>) (A.pure f A.<*> p A.<*> q  A.<*> r)

pzip :: P s a -> P s b -> P s (a,b)
pzip = pzipWith (,)

pzip3 :: P s a -> P s b -> P s c -> P s (a,b,c)
pzip3 = pzipWith3 (,,)

pzip4 :: P s a -> P s b -> P s c -> P s d -> P s (a,b,c,d)
pzip4 = pzipWith4 (,,,)

pseries :: (Num a) => a -> a -> Int -> P s a
pseries i s n =
    let f (_, 0) = Nothing
        f (j, m) = Just (return j, (j + s, m - 1))
    in plist (L.unfoldr f (i, n))

pgeom :: (Num a) => a -> a -> Int -> P s a
pgeom i s n =
    let f (_, 0) = Nothing
        f (j, m) = Just (return j, (j * s, m - 1))
    in plist (L.unfoldr f (i, n))

pstutter' :: P s Int -> P s a -> P s a
pstutter' n p =
    let f :: Int -> a -> P s a
        f i e = preplicate (return i) (return e)
    in psequence (pzipWith f n p)

pstutter :: P s Int -> P s a -> P s a
pstutter = pstutter' . pcycle

-- | Count false values preceding each true value.
pcountpre :: P s Bool -> P s Int
pcountpre p =
    let f x e = if e then (0, Just x) else (x + 1, Nothing)
    in pmapMaybe id (pscan f Nothing 0 p)

-- | Count false values following each true value.
pcountpost :: P s Bool -> P s Int
pcountpost p =
    let f x e = if e then (0, Just x) else (x + 1, Nothing)
    in ptail (pmapMaybe id (pscan f (Just Just) 0 p))

pclutch' :: P s a -> P s Bool -> P s a
pclutch' p q =
    let r = fmap (+ 1) (pcountpost q)
    in pstutter' r p

pbool :: (Ord a, Num a) => P s a -> P s Bool
pbool = fmap (> 0)

pclutch :: (Num b, Ord b) => P s a -> P s b -> P s a
pclutch p = pclutch' p . pbool

pcollect :: (a -> b) -> P s a -> P s b
pcollect = fmap

pdegreeToKey :: (RealFrac a) => P s a -> P s [a] -> P s a -> P s a
pdegreeToKey = pzipWith3 S.degree_to_key

pfin :: P s Int -> P s a -> P s a
pfin = ptake

pfin_ :: Int -> P s a -> P s a
pfin_ = ptake_

wrap :: (Ord a, Num a) => a -> a -> a -> a
wrap l r x = if x > r
             then wrap l r (x - (r - l))
             else if x < l
                  then wrap l r (x + (r - l))
                  else x

pwrap :: (Ord a, Num a) => P s a -> P s a -> P s a -> P s a
pwrap x l r =
    let f x' l' r' = wrap l' r' x'
    in pzipWith3 f x (pcycle l) (pcycle r)

-- | Remove successive duplicates.
prsd :: (Eq a) => P s a -> P s a
prsd p =
    let f Nothing a = (Just a, Just a)
        f (Just x) a = (Just a, if a == x then Nothing else Just a)
    in pmapMaybe id (pscan f Nothing Nothing p)

psequence :: P s (P s a) -> P s a
psequence = M.join

pduple :: (a, a) -> P s a
pduple (x, y) = return x `M.mappend` return y

pinterleave :: P s a -> P s a -> P s a
pinterleave p = psequence . fmap pduple . pzip p

ptrigger :: P s Bool -> P s a -> P s (Maybe a)
ptrigger p q =
    let r = pcountpre p
        f i = M.mappend (preplicate_ i (return Nothing)) . return . Just
    in M.join (pzipWith f r q)

pif :: P s Bool -> P s a -> P s a -> P s a
pif b p q =
    let f (x, y) True = ((ptail x, y), phead x)
        f (x, y) False = ((x, ptail y), phead y)
    in psequence (pscan f Nothing (p,q) b)

phead :: P s a -> P s a
phead p = pcontinue p (\x _ -> return x)

ptail :: P s a -> P s a
ptail p = pcontinue p (\_ p' -> p')

pdrop :: P s Int -> P s a -> P s a
pdrop n p = n >>= (\x -> if x > 0
                         then pdrop (return (x-1)) (ptail p)
                         else p)

pscanl :: (a -> y -> a) -> a -> P s y -> P s a
pscanl f i p =
    let g x y = let r = f x y in (r, r)
    in pcons i (pscan g Nothing i p)

-- * Random numbers

prrandf :: (R.RandomGen s, R.Random a) => 
           (a -> a -> a -> a) -> a -> a -> P s a
prrandf f l r = prp (\g -> let (x, g') = R.randomR (l,r) g
                           in (return (f l r x), g'))

prrand :: (R.RandomGen s, R.Random a) => 
          a -> a -> P s a
prrand = prrandf (\_ _ x -> x)

prrandexp :: (R.RandomGen s, Floating a, R.Random a) => 
             a -> a -> P s a
prrandexp = prrandf (\l r x -> l * (log (r / l) * x))

pchoosea :: (R.RandomGen s) => A.Array Int (P s a) -> P s a
pchoosea r = prp (\g -> let (i, g') = R.randomR (A.bounds r) g 
                        in (r A.! i, g'))

pchoose :: R.RandomGen s => [P s a] -> P s a
pchoose l = pchoosea (A.listArray (0, length l - 1) l)

prand :: R.RandomGen s => [P s a] -> P s Int -> P s a
prand p = pseq [pchoose p]

pwhite :: (R.RandomGen s, R.Random a) => 
          P s a -> P s a -> P s Int -> P s a
pwhite l r n = prestrict n (M.join (pzipWith prrand l r))

pexprand :: (R.RandomGen s, Floating a, R.Random a) => 
            P s a -> P s a -> P s Int -> P s a
pexprand l r n = prestrict n (M.join (pzipWith prrandexp l r))

pxrand :: (R.RandomGen s, Eq a) => [P s a] -> P s Int -> P s a
pxrand p n = ptake n (prsd (pseq [pchoose p] pinf))

pwrand :: R.RandomGen s => [P s a] -> [P s a] -> P s Int -> P s a
pwrand = undefined

-- * List

pseq_ :: [P s a] -> Int -> P s a
pseq_ l n = plist (concat (replicate n l))

pseq :: [P s a] -> P s Int -> P s a
pseq l n = n >>= (\x -> plist (concat (replicate x l)))

-- | 'n' values from the infinite cycle of the streams at l.
pser_ :: [P s a] -> Int -> P s a
pser_ l n = prestrict_ n (plist l)

pser :: [P s a] -> P s Int -> P s a
pser l n = prestrict n (plist l)

pswitch :: [P s a] -> P s Int -> P s a
pswitch l i = i >>= (l !!)

pswitch1m :: M.IntMap (P s a) -> P s Int -> P s a
pswitch1m m is =
    let f i js = let h = phead (m M.! i)
                     t = ptail (m M.! i)
                 in h `M.mappend` pswitch1m (M.insert i t m) js
    in pcontinue is f

pswitch1 :: [P s a] -> P s Int -> P s a
pswitch1 = pswitch1m . M.fromList . zip [0..]

ppatlace :: [P s a] -> P s Int -> P s a
ppatlace ps n =
    let is = pseq (map return [0 .. length ps - 1]) pinf
    in ptake n (pswitch1 ps is)

{-

Neither the definition above or the variant below are correct.
Both deadlock once all patterns are empty.  pswitch1 has the 
same problem.  

ppatlacea :: P s (P s a) -> P s a
ppatlacea ps = 
    let f p qs = let h = phead p
                     t = ptail p
                     rs = qs `mappend` return t
                 in h `mappend` (ppatlacea rs)
    in pcontinue ps f
-}

-- * Extend

pzipWith_c :: (a -> b -> c) -> P s a -> P s b -> P s c
pzipWith_c f p = pzipWith f p . pcycle

infixl 7  *., /.
infixl 6  +., -.

(+.) :: Num a => P s a -> P s a -> P s a
(+.) = pzipWith_c (+)

(*.) :: Num a => P s a -> P s a -> P s a
(*.) = pzipWith_c (*)

(/.) :: Fractional a => P s a -> P s a -> P s a
(/.) = pzipWith_c (/)

(-.) :: Num a => P s a -> P s a -> P s a
(-.) = pzipWith_c (-)
