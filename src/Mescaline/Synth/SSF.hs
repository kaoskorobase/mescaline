{-# LANGUAGE Arrows, FlexibleInstances, MultiParamTypeClasses, RankNTypes #-}
module Mescaline.Synth.SSF (
    module Data.Signal.SF.Event
  , State
  -- , initially
  , tag
  , never
  , once
  , filter
  , hold
  , accum
  , accumHold
  , scanl
  , edge
  , sample
  , sample_
  -- * Switching
  , switch
  -- , switch'
  , rSwitch
  -- , rswitch'
  , pSwitch
  , rpSwitch
  -- * Server state
  , alloc
  , alloc_
  , recv
  , send
  , send_
  , waitFor
  , wait
  , sync
  , SF
  , lift
  , realTime
  , logicalTime
  , Options(..)
  , execute
  , executeOSC
) where

import           Control.Applicative
import           Control.Arrow
-- import           Control.Arrow.Transformer
import           Control.Arrow.Operations (ArrowState(..))
-- import           Control.Arrow.Transformer (lift)
import           Control.Arrow.Transformer.State (ArrowAddState(..))
import           Control.Category
import           Control.CCA.Types
import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan.Chunked
import           Control.Monad (liftM)
import           Control.Monad.Fix (fix)
import           Data.Accessor
import           Data.Monoid
import qualified Data.List as List
import qualified Data.Signal.SF as SF
import           Data.Signal.SF.Event
import           Mescaline (Time)
import           Sound.OpenSoundControl (OSC(..), Datum(..))
import qualified Sound.OpenSoundControl as OSC
import           Sound.OpenSoundControl.Monoid ()
import           Sound.SC3.Server.Allocator (IdAllocator)
import qualified Sound.SC3.Server.Allocator as Alloc
import           Sound.SC3.Server.Notification
import qualified Sound.SC3.Server.State as Server
import           Sound.SC3.Server.Process.Options (ServerOptions)
import           Prelude hiding ((.), filter, id, init, scanl)

data State = State {
    s_realTime    :: !Time
  , s_logicalTime :: !Time
  , s_state       :: Server.State
  , s_oscInput    :: Event OSC
  , s_oscOutput   :: [OSC]
}

newState :: Time -> Server.State -> State
newState time state = State time time state NoEvent []

newtype SF a b = SF { runState :: SF.SF (a, State) (b, State) }

-- runSF :: SF a b -> (a, State) -> ((b, State), SF a b)
-- runSF sf (a0, s) = second SF (SF.runSF (runState sf) (a0, s))

swapsnd :: ((a, b), c) -> ((a, c), b)
swapsnd ~(~(x, y), z) = ((x, z), y)

instance Category SF where
	id = SF id
	SF f . SF g = SF (f . g)

instance Arrow SF where
	arr f        = SF (arr (\(x, s) -> (f x, s)))
	first (SF f) = SF (arr swapsnd >>> first f >>> arr swapsnd)

-- instance Arrow a => ArrowTransformer SF SF.SF where
--  lift f = SF (first f)

lift :: SF.SF a b -> SF a b
lift = SF . first

instance ArrowState State SF where
	fetch = SF (arr (\(_, s) -> (s, s)))
	store = SF (arr (\(s, _) -> ((), s)))

instance ArrowChoice SF where
    left (SF f) = SF (arr distr >>> left f >>> arr undistr)
        where
            distr   (Left y, s)    = Left (y, s)
            distr   (Right z, s)   = Right (z, s)
            undistr (Left (y, s))  = (Left y, s)
            undistr (Right (z, s)) = (Right z, s)

-- instance ArrowApply a => ArrowApply (StateArrow s a) where
--  app = ST (arr (\((ST f, x), s) -> (f, (x, s))) >>> app)

instance ArrowLoop SF where
	loop (SF f) = SF (loop (arr swapsnd >>> f >>> arr swapsnd))

instance ArrowInit SF where
    init = lift . init
    -- loopD :: (ArrowInit a) => e -> a (b, e) (c, e) -> a b c
    loopD i (SF g) = SF (loopD i (arr swapsnd >>> g >>> arr swapsnd))
    -- loopB :: (ArrowInit a) => e -> a (b, (d, e)) (c, (d, e)) -> a b c
    loopB i (SF g) = SF (loopB i (arr swapsnd >>> g >>> arr swapsnd))

-- instance ArrowPlus SF where
--  SF f <+> SF g = SF (f <+> g)

-- Other instances

instance Functor (SF a) where
	fmap f g = g >>> arr f

instance Applicative (SF a) where
    pure    = arr . const
    f <*> g = f &&& g >>> arr (uncurry ($))

-- instance Alternative (SF a) where
--  empty = zeroArrow
--  f <|> g = f <+> g

-- instance Monoid (SF a b) where
--  mempty = zeroArrow
--  mappend f g = f <+> g

-- ====================================================================
-- Signal functions

-- | Transform initial input value.
(>=-) :: (a -> a) -> SF a b -> SF a b
(>=-) f = SF . (SF.>=-) (first f) . runState

-- | Transform initial output value.
(-=>) :: (b -> b) -> SF a b -> SF a b
(-=>) f = SF . (SF.-=>) (first f) . runState

-- | Override initial input value.
(>--) :: a -> SF a b -> SF a b
(>--) a0 = (>=-) (const a0)

-- | Override initial output value.
-- Initialization operator (cf. Lustre/Lucid Synchrone).
(-->) :: b -> SF a b -> SF a b
(-->) b0 = (-=>) (const b0)

-- Override initial value of input signal.
-- initially :: a -> SF a a
-- initially a = lift (SF.initially a)

-- | Replace event value.
tag :: b -> SF (Event a) (Event b)
tag b = lift (SF.tag b)

-- | Event source that never occurs.
never :: SF a (Event b)
never = lift SF.never

-- | Event source with a single occurrence at time 0. The value of the event
-- is given by the function argument.
once :: b -> SF a (Event b)
once = lift . SF.once

-- | Filter out events that don't satisfy some predicate.
filter :: (a -> Bool) -> SF (Event a) (Event a)
filter = lift . SF.filter

-- | Zero order hold.
hold :: a -> SF (Event a) a
hold = lift . SF.hold

-- | Accumulate from an initial value and an update event.
accum :: a -> SF (Event (a -> a)) (Event a)
accum = lift . SF.accum

accumHold :: a -> SF (Event (a -> a)) a
accumHold = lift . SF.accumHold

scanl :: (b -> a -> b) -> b -> SF a b
scanl f = lift . SF.scanl f

edge :: SF Bool (Event ())
edge = lift SF.edge

sample :: SF (a, Event b) (Event (a, b))
sample = lift SF.sample

sample_ :: SF (a, Event b) (Event a)
sample_ = lift SF.sample_

liftSwitch switch sf f = SF (switch sf' f')
    where
        sf' = runState sf >>> arr swapsnd
        f'  = runState . f

switch :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
switch = liftSwitch SF.switch

-- switch' :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
-- switch' = liftSwitch SF.switch'

-- (>>=) :: forall  a b. m a -> (a -> m b) -> m b
-- (>>) :: forall a b. m a -> m b -> m b
-- return :: a -> m a
-- fail :: String -> m a

-- switchE :: SF (Event a) (Event c) -> (c -> SF (Event a) (Event b)) -> SF (Event a) (Event b)
-- switchE = undefined

-- lift_rswitch g sf f = SF (g sf' f')
--     where
--         sf' = runState sf >>> arr (\((b, c), s) -> ((b, s), c))
--         f'  = runState . f

rSwitch :: SF a b -> SF (a, Event (SF a b)) b
rSwitch sf = switch (first sf) ((second (const NoEvent) >=-) . rSwitch)

-- rswitch' :: SF a b -> SF (a, Event (SF a b)) b
-- rswitch' sf = switch' (first sf) ((second (const NoEvent) >=-) . rswitch')

-- | Parallel switch parameterized on the routing function. This is the most
-- general switch from which all other (non-delayed) switches in principle
-- can be derived. The signal function collection is spatially composed in
-- parallel and run until the event signal function has an occurrence. Once
-- the switching event occurs, all signal function are "frozen" and their
-- continuations are passed to the continuation function, along with the
-- event value.
-- rf .........	Routing function: determines the input to each signal function
--		in the collection. IMPORTANT! The routing function has an
--		obligation to preserve the structure of the signal function
--		collection.
-- sfs0 .......	Signal function collection.
-- sfe0 .......	Signal function generating the switching event.
-- k .......... Continuation to be invoked once event occurs.
-- Returns the resulting signal function.
--
-- !!! Could be optimized on the event source being SFArr, SFArrE, SFArrEE
--
pSwitch :: (Functor col) =>
       (forall sf . (a -> col sf -> col (b, sf)))
    -> (forall s . s -> col (s -> ((c, SF b c), s)) -> (col (c, SF b c), s))
    -> col (SF b c)
    -> SF (a, col c) (Event d)
    -> (col (SF b c) -> d -> SF a (col c))
    -> SF a (col c)
pSwitch rf ff sfs0 sfe0 k = SF (SF.SF tf0)
    where
        tf0 (a0, s0) =
            let bsfs0      = rf a0 sfs0
                (sfcs0, s) = ff s0 $ fmap (\(b0, sf0) -> \s -> swapsnd (second SF (SF.runSF (runState sf0) (b0, s)))) bsfs0
                cs0        = fmap fst sfcs0
                sfs        = fmap snd sfcs0
            in
            case SF.runSF (runState sfe0) ((a0, cs0), s) of
                ((NoEvent, s'), sfe) -> ((cs0, s'), runState (pSwitch rf ff sfs (SF sfe) k))
                ((Event d0, s'), _)  -> SF.runSF (runState (k sfs0 d0)) (a0, s')

-- Recurring parallel switch parameterized on the routing function.
-- rf .........	Routing function: determines the input to each signal function
--		in the collection. IMPORTANT! The routing function has an
--		obligation to preserve the structure of the signal function
--		collection.
-- sfs ........	Initial signal function collection.
-- Returns the resulting signal function.

rpSwitch :: Functor col =>
      (forall sf . (a -> col sf -> col (b, sf)))
   -> (forall s . s -> col (s -> ((c, SF b c), s)) -> (col (c, SF b c), s))
   -> col (SF b c) -> SF (a, Event (col (SF b c) -> col (SF b c))) (col c)
rpSwitch rf ff sfs =
    pSwitch (rf . fst) ff sfs (arr (snd . fst)) $ \sfs' f ->
        second (const NoEvent) >=- rpSwitch rf ff (f sfs')

-- ====================================================================
-- Sampled Signal functions

realTime :: SF a Time
realTime = fetch >>> arr s_realTime

logicalTime :: SF a Time
logicalTime = fetch >>> arr s_logicalTime

eitherToEvent :: Either a b -> Event b
eitherToEvent (Left _)  = NoEvent
eitherToEvent (Right b) = Event b

-- ID allocation
alloc :: IdAllocator i a => (Accessor Server.State a) -> SF (Event b) (Event (b, i))
alloc f =
    proc e -> do
        case e of
            NoEvent -> returnA -< NoEvent
            Event b -> do
                s <- fetch -< ()
                case Alloc.alloc (getVal f (s_state s)) of
                    Left _       -> returnA -< NoEvent -- TODO: signal error condition
                    Right (i, a) -> do
                        store -< s { s_state = setVal f a (s_state s) }
                        returnA -< Event (b, i)

alloc_ :: IdAllocator i a => (Accessor Server.State a) -> SF (Event b) (Event i)
alloc_ = fmap (fmap snd) . alloc

-- alloc f s = modifyMVar s (\s -> fmap (swap . second (flip (setVal f) s)) . errorToIO . Alloc.alloc . getVal f $ s)

-- OSC shtuff
recv :: SF a (Event OSC)
recv = fetch >>> arr s_oscInput

-- | Send an OSC packet.
send :: SF (Event OSC) (Event OSC)
send = fetch &&& id
       >>> arr (\(s, e) -> (event s (\osc -> s { s_oscOutput = osc : s_oscOutput s }) e, e))
       >>> first store
       >>> arr snd

-- | Send an OSC packet.
send_ :: OSC -> SF a (Event OSC)
send_ osc = once osc >>> (id &&& send) >>> arr fst

-- | Wait until a certain logical time.
delayUntil :: SF (Event a) (Event a)
delayUntil = undefined

-- | Wait for an OSC message where the supplied function does not give
--   Nothing, discarding intervening messages.
waitFor :: (OSC -> Maybe a) -> SF (Event OSC) (Event a)
waitFor f = arr (maybeToEvent . (>>= f) . eventToMaybe)

-- | Wait for an OSC message matching a specific address.
wait :: String -> SF (Event OSC) (Event OSC)
wait s = waitFor (\osc -> if has_address s osc then Just osc else Nothing)
    where
        has_address x (OSC.Message y _) = x == y
        has_address x (OSC.Bundle _ xs) = any (has_address x) xs

-- | Synchronization barrier.
sync :: SF (Event a) (Event a)
sync = (never &&& alloc Server.syncId)
       >>> arr (second (fmap (\(a, i) ->
            once (Message "/sync" [Int i]) >>> send
            >>> recv >>> waitFor (synced i) >>> tag a)))
        >>> rSwitch id

-- sync' :: SF a b -> SF (a, Event (SF a b)) b
-- sync' sf0 = second (alloc Server.syncId)
--        >>> arr (second (fmap (\(a, i) ->
--             now (Message "/sync" [Int i]) >>> send
--             >>> recv >>> waitFor (synced i) >>> tag a)))
--         >>> rswitch' sf0

-- ====================================================================
-- Driver

data Options t = Options {
    serverOptions :: ServerOptions
  , transport     :: t
  , tickInterval  :: Double
} deriving (Eq, Show)

execute :: OSC.Transport t => Options t -> SF (Event a) b -> Chan a -> Chan ((Time,Time), b) -> IO ()
execute opts ssf ichan ochan = do
    t <- OSC.utcr
    loop (newState t (Server.new $ serverOptions opts)) (runState ssf)
    where
        dt = tickInterval opts
        loop state0 sf = do
            empty <- isEmptyChan ichan
            as <- if empty then return [NoEvent] else map Event `fmap` readChanAvailable ichan
            rt <- OSC.utcr
            let state1 = state0 { s_realTime = rt }
                ((bs, state2), sf') = List.foldl'
                                        (\((bs, s), sf) a -> let ((b, s'), sf') = SF.runSF sf (a, s)
                                                             in b `seq` s' `seq` ((b:bs, s'), sf'))
                                        (([], state1), sf)
                                        as
            -- let ((b, state'), sf') = SF.runSF sf (a, state { s_realTime = rt })
            -- b `seq` state' `seq` writeChan ochan ((s_realTime state', s_logicalTime state'), b)
            writeList2Chan ochan $ map ((,) (s_realTime state2, s_logicalTime state2)) bs
            let state3 = state2 { s_logicalTime = s_logicalTime state2 + dt }
            OSC.pauseThreadUntil (s_logicalTime state3)
            state3 `seq` loop state3 sf'

withChanContents a0 f chan = do
    empty <- isEmptyChan chan
    if empty
        then return a0
        else fmap f (readChanAvailable chan)

executeOSC :: OSC.Transport t => Options t -> SF (Event a) b -> Chan a -> Chan ((Time,Time), b) -> IO ()
executeOSC opts ssf ichan ochan = do
    t <- OSC.utcr
    oscChan <- newChan
    forkIO $ fix $ \loop -> OSC.recv (transport opts) >>= (\osc -> print osc >> writeChan oscChan osc) >> loop
    loop oscChan (newState t (Server.new $ serverOptions opts)) (runState ssf)
    where
        dt = tickInterval opts
        loop oscChan state0 sf = do
            osc <- withChanContents [] (map Event) oscChan
            as <- withChanContents [] (map Event) ichan
            rt <- OSC.utcr
            let input = if null osc && null as
                        then [(NoEvent, NoEvent)]
                        else let n = max (length osc) (length as)
                                 osc' = osc ++ replicate n NoEvent
                                 as'  = as  ++ replicate n NoEvent
                             in zip osc' as'
                state1 = state0 { s_realTime = rt }
                ((bs, state2), sf') = List.foldl'
                                        (\((bs, s), sf) (osc, a) ->
                                            let ((b, s'), sf') = SF.runSF sf (a, s { s_oscInput = osc })
                                            in b `seq` s' `seq` ((b:bs, s'), sf'))
                                        (([], state1), sf)
                                        input
            -- let ((b, state'), sf') = SF.runSF sf (a, state { s_realTime = rt })
            -- b `seq` state' `seq` writeChan ochan ((s_realTime state', s_logicalTime state'), b)
            writeList2Chan ochan $ map ((,) (s_realTime state2, s_logicalTime state2)) bs
            mapM_ (OSC.send (transport opts)) (s_oscOutput state2)
            let state3 = state2 {
                s_logicalTime = s_logicalTime state2 + dt
              , s_oscInput    = NoEvent
              , s_oscOutput   = []
                }
            OSC.pauseThreadUntil (s_logicalTime state3)
            state3 `seq` loop oscChan state3 sf'
