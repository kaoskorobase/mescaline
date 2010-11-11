-----------------------------------------------------------------------------
-- |
-- Module      :  Data.STRef
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (uses Control.Monad.ST)
--
-- Mutable references in the (strict) ST monad.
--
-----------------------------------------------------------------------------

module Data.STRef (
	-- * STRefs
	STRef,		-- abstract, instance Eq
	newSTRef,	-- :: a -> ST s (STRef s a)
	readSTRef,	-- :: STRef s a -> ST s a
	writeSTRef,	-- :: STRef s a -> a -> ST s ()
	modifySTRef	-- :: STRef s a -> (a -> a) -> ST s ()
 ) where

import Prelude







import Hugs.ST
import Data.Typeable

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      























































stRefTc = mkTyCon "STRef"; instance Typeable2 STRef where { typeOf2 _ = mkTyConApp stRefTc [] }; instance Typeable a => Typeable1 (STRef a) where {   typeOf1 = typeOf1Default }; instance (Typeable a, Typeable b) => Typeable (STRef a b) where {   typeOf = typeOfDefault }


-- |Mutate the contents of an 'STRef'
modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef ref f = writeSTRef ref . f =<< readSTRef ref
