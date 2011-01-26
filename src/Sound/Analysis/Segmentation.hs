module Sound.Analysis.Segmentation (
    Segment(..)
  , Interval(..)
  , Segmentation
  , segments
  , empty
  , fromList
  , fromEvents
  , toList
  , labels
  , totalDuration
  , containing
  , covering
  , segment
) where

import qualified Data.Foldable as F
import qualified Data.List as L
-- import qualified Data.List.Zipper as L
import           Data.IntervalMap.FingerTree (Interval)
import qualified Data.IntervalMap.FingerTree as IM

data Segment t a = Segment {
    onset :: t
  , duration :: t
  , label :: a
  } deriving (Eq, Read, Show)

toInterval :: Num t => Segment t a -> IM.Interval t
toInterval s = IM.Interval (onset s) (onset s + duration s)

instance Functor (Segment t) where
    fmap f (Segment x1 x2 a) = Segment x1 x2 (f a)

data Segmentation t a = Segmentation {
  segments :: IM.IntervalMap t (Segment t a)
  }

instance (Eq t, Eq a) => Eq (Segmentation t a) where
    (==) a b = toList a == toList b

instance (Show t, Show a) => Show (Segmentation t a) where
    show x = "fromList " ++ show (toList x)
    
data Pair a b = Pair !a !b

fromList :: (Num t, Ord t) => [Segment t a] -> Segmentation t a
fromList xs = Segmentation xs'
    where
        xs' = L.foldl' insert IM.empty xs
        insert im s = IM.insert (toInterval s) s im

fromEvents :: (Num t, Ord t) => t -> [(t, a)] -> Segmentation t a
fromEvents _ [] = empty
fromEvents tn xs = fromList (zipWith f xs' (tail xs'))
    where
        xs' = xs ++ [(tn, error "fromEvents: Shouldn't happen")]
        f (t1, a) (t2, _) = Segment t1 (t2-t1) a

empty :: (Num t, Ord t) => Segmentation t a
empty = fromList []

toList :: Segmentation t a -> [Segment t a]
toList = F.toList . segments

labels :: Segmentation t a -> [a]
labels = map label . toList

totalDuration :: Num t => Segmentation t a -> t
totalDuration s =
    case toList s of
        [] -> 0
        xs -> let x = last xs in onset x + duration x

containing :: Ord t => t -> Segmentation t a -> Maybe (Segment t a)
containing t s =
    case IM.search t (segments s) of
        [] -> Nothing
        xs -> Just (snd (last xs))

covering :: (Num t, Ord t) => Interval t -> Segmentation t a -> Segmentation t a
covering i s =
    case  takeWhile (\(i', _) -> IM.low i' < IM.high i)
        $ dropWhile (\(i', _) -> IM.high i' == IM.low i)
        $ IM.intersections i
        $ segments s
    of
        [] -> empty
        xs -> fromList (map snd xs)

segment :: (Num t, Ord t) => Segmentation t a -> Segmentation t b -> Segmentation t (Segmentation t b)
segment s x = Segmentation $ fmap (\s -> const (toInterval s `covering` x) `fmap` s) (segments s)
