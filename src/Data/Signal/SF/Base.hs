{-# LANGUAGE BangPatterns, CPP, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}

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

module Data.Signal.SF.Base (
    SF(..)
  , run
  , unfold
  , nth
  , nth'
) where

#if __GLASGOW_HASKELL__ >= 610
import Control.Category
import Prelude hiding ((.), init, exp)
#else
import Prelude hiding (init, exp)
#endif

import Control.Arrow
--import Control.CCA.CCNF
import Control.CCA.Types
import Control.CCA.ArrowP


newtype SF a b = SF { runSF :: (a -> (b, SF a b)) }

instance ArrowInitP SF p

#if __GLASGOW_HASKELL__ >= 610
instance Category SF where
  id = SF h where h x = (x, SF h)
  g . f = SF (h f g)
    where
      h f g x =
        let (y, f') = runSF f x
            (z, g') = runSF g y
        in f' `seq` g' `seq` (z, SF (h f' g'))

instance Arrow SF where
  arr f = g
    where g = SF (\x -> (f x, g))
  first f = SF (g f)
    where
      g f (x, z) = f' `seq` ((y, z), SF (g f'))
        where (y, f') = runSF f x
  f &&& g = SF (h f g)
    where
      h f g x =
        let (y, f') = runSF f x
            (z, g') = runSF g x 
        in ((y, z), SF (h f' g'))
  f *** g = SF (h f g)
    where
      h f g x =
        let (y, f') = runSF f (fst x)
            (z, g') = runSF g (snd x) 
        in ((y, z), SF (h f' g'))
#else
instance Arrow SF where
  arr f = g
    where g = SF (\x -> (f x, g))
  f >>> g = SF (h f g)
    where
      h f g x =
        let (y, f') = runSF f x
            (z, g') = runSF g y
        in (z, SF (h f' g'))
  first f = SF (g f)
    where
      g f (x, z) = ((y, z), SF (g f'))
        where (y, f') = runSF f x
  f &&& g = SF (h f g)
    where
      h f g x =
        let (y, f') = runSF f x
            (z, g') = runSF g x 
        in ((y, z), SF (h f' g'))
  f *** g = SF (h f g)
    where
      h f g x =
        let (y, f') = runSF f (fst x)
            (z, g') = runSF g (snd x) 
        in ((y, z), SF (h f' g'))
#endif

instance ArrowLoop SF where
  loop sf = SF (g sf)
    where
      g f x = f' `seq` (y, SF (g f'))
        where ((y, z), f') = runSF f (x, z)

instance ArrowChoice SF where
   left sf = SF (g sf)
       where 
         g f x = case x of
                   Left a -> let (y, f') = runSF f a in f' `seq` (Left y, SF (g f'))
                   Right b -> (Right b, SF (g f))

instance ArrowInit SF where
  init i = SF (f i)
    where f i x = (i, SF (f x))
  loopD i g = SF (f i g)
    where
      f i g x = 
        let ((y, i'), g') = runSF g (x, i)
        in (y, SF (f i' g'))
  loopB i g = SF (f i g)
    where
      f i g x = 
        let ((y, (z, i')), g') = runSF g (x, (z, i))
        in (y, SF (f i' g'))

run :: SF a b -> [a] -> [b]
run (SF f) (x:xs) =
  let (y, f') = f x 
  in y `seq` f' `seq` (y : run f' xs)

unfold :: SF () a -> [a]
unfold = flip run inp
  where inp = () : inp


nth :: Int -> SF () a -> a
nth n (SF f) = x `seq` if n == 0 then x else nth (n - 1) f'
  where (x, f') = f ()

nth' :: Int -> (b, ((), b) -> (a, b)) -> a
nth' !n (i, f) = n `seq` i `seq` f `seq` aux n i
  where
    aux !n !i = x `seq` i' `seq` if n == 0 then x else aux (n-1) i'
      where (x, i') = f ((), i)
