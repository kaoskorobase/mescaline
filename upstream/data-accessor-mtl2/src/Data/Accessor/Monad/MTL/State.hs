{- | Access helper functions in the State monad class -}
module Data.Accessor.Monad.MTL.State where

import qualified Data.Accessor.Basic as Accessor
import qualified Control.Monad.State as State
import qualified Control.Monad.Trans as Trans
import Control.Monad.State (MonadState, State, runState, StateT(runStateT), )
import Control.Monad.Trans (MonadTrans)

-- * accessors in the form of actions in the state monad

set :: MonadState r m => Accessor.T r a -> a -> m ()
set f x = State.modify (Accessor.set f x)

get :: MonadState r m => Accessor.T r a -> m a
get f = State.gets (Accessor.get f)

modify :: MonadState r m => Accessor.T r a -> (a -> a) -> m ()
modify f g = State.modify (Accessor.modify f g)

{- |
Modify a record element and return its old value.
-}
getAndModify :: MonadState r m => Accessor.T r a -> (a -> a) -> m a
getAndModify f g =
   do x <- get f
      modify f g
      return x

{- |
Modify a record element and return its new value.
-}
modifyAndGet :: MonadState r m => Accessor.T r a -> (a -> a) -> m a
modifyAndGet f g =
   do modify f g
      get f



infix 1 %=, %:

{- |
Infix variant of 'set'.
-}
(%=) :: MonadState r m => Accessor.T r a -> a -> m ()
(%=) = set

{- |
Infix variant of 'modify'.
-}
(%:) :: MonadState r m => Accessor.T r a -> (a -> a) -> m ()
(%:) = modify



-- * lift a state monadic accessor to an accessor of a parent record

lift :: (MonadState r mr) =>
   Accessor.T r s -> State s a -> mr a
lift f m =
   do s0 <- get f
      let (a,s1) = runState m s0
      set f s1
      return a

-- liftT :: (Monad m) =>
--    Accessor.T r s -> StateT s m a -> StateT r m a
liftT :: (Monad m, MonadTrans t, MonadState r (t m)) =>
   Accessor.T r s -> StateT s m a -> t m a
liftT f m =
   do s0 <- get f
      (a,s1) <- Trans.lift $ runStateT m s0
      set f s1
      return a

{- not possible in this generality
lift :: (MonadState r mr, MonadState s ms) =>
   Accessor.T r s -> ms a -> mr a
-}
