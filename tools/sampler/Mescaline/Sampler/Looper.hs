module Mescaline.Sampler.Looper (
    Event(..)
  , run
) where

import Data.Map as Map
import Control.Concurrent (ThreadId, forkIO, killThread, throwTo)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent.SampleVar
import Control.Monad (when)
import Sound.OpenSoundControl (pauseThreadUntil, utcr)

-- prepare record
-- record
-- cancel record
-- start playing
-- stop playing
-- overdup
-- overdup multiply
-- resync

data Event e =
    ERecord
  | EStart
  | EStop
  | EInsert e
  deriving (Eq, Show)

type Channel e = Chan (Event e)

data State = Idle | Recording | Playing

data Looper e = Looper {
    state         :: State
  , loopStart     :: Double
  , loopEnd       :: Double
  , events        :: Map Double (Maybe e)
  , input         :: Chan (Double, Event e)
  , output        :: Chan (Double, e)
  , playerProcess :: (ThreadId, SampleVar [(Maybe e, Double)])
}

data Player e = Player {
    p_time     :: Double
  , p_schedule :: [(Maybe e, Double)]
  , p_input    :: SampleVar [(Maybe e, Double)]
  , p_output   :: Chan (Double, e)
}

run :: Show e => (Chan (Double, Event e), Chan (Double, e)) -> IO ()
run (iChan, oChan) = do
    looper $ Looper {
        state = Idle
      , loopStart = undefined
      , loopEnd = undefined
      , events = Map.empty
      , input = iChan
      , output = oChan
      , playerProcess = undefined
      }

minAssocs :: Map k a -> [(k, a)]
minAssocs m =
    if Map.null m
        then []
        else let Just (a, m') = Map.minViewWithKey m
             in a : minAssocs m'

setState :: State -> Looper e -> Looper e
setState s l = l { state = s }

looper :: Show e => Looper e -> IO ()
looper l = do
    evt <- readChan (input l)
    print evt
    case state l of
        Idle -> do
            case evt of
                (t, ERecord) -> do
                    let l' = l { state = Recording
                               , loopStart = t
                               , events = Map.singleton 0 Nothing }
                    looper l'
                (t, EStart) ->
                    if Map.null (events l)
                        then looper l
                        else do
                            let es = toDList (loopEnd l) (minAssocs (events l))
                            v <- newEmptySampleVar
                            pid <- forkIO $ player $ Player t es v (output l)
                            let l' = l { state         = Playing
                                       , playerProcess = (pid, v)
                                       }
                            looper l'
                _ -> looper l
        Recording -> do
            case evt of
                (t, EInsert e) -> do                    
                    let l' = l { events = Map.insert (t - loopStart l) (Just e) (events l) }
                    print (t, e)
                    writeChan (output l) (t, e)
                    looper l'
                (t, EStart) -> do
                    let end = t - loopStart l
                        es = toDList end (minAssocs (events l))
                    v <- newEmptySampleVar
                    pid <- forkIO $ player $ Player t es v (output l)
                    let l' = l { state         = Playing
                               , loopEnd       = end
                               , playerProcess = (pid, v)
                               }
                    looper l'
                (t, EStop) -> do
                    let l' = l { state = Idle
                               }
                    looper l'                    
                _ -> looper l
        Playing -> do
            case evt of
                (t, EInsert e) -> do
                    let l' = l { events = Map.insert (t - loopStart l) (Just e) (events l) }
                    writeSampleVar (snd (playerProcess l)) (toDList (loopEnd l) (minAssocs (events l)))
                    writeChan (output l) (t, e)
                    looper l'
                (t, EStart) -> do
                    -- Signal player thread to jump to the beginning
                    let es = toDList (loopEnd l) (minAssocs (events l))
                    v <- newEmptySampleVar
                    pid <- forkIO $ player $ Player t es v (output l)
                    killThread (fst (playerProcess l))
                    let l' = l { playerProcess = (pid, v)
                               }
                    looper l'
                (t, EStop) -> do
                    killThread (fst (playerProcess l))
                    let l' = l { state = Idle
                               , playerProcess = error "Accessing undefined player process"
                               }
                    looper l'
                _ -> looper l

epsilon :: Double
epsilon = 1e-12

toDList :: Double -> [(Double, Maybe e)] -> [(Maybe e, Double)]
toDList _ [] = []
toDList tn [(t, e)] = [(e, tn - t)]
toDList tn ((t1,e):es@((t2,_):_)) = (e, t2 - t1) :  toDList tn es

player :: Player e -> IO ()
player p = do
    empty <- isEmptySampleVar (p_input p)
    case empty of
        True  -> do
            t' <- playDList (\t e -> writeChan (p_output p) (t, e)) (p_time p) (p_schedule p) 
            let p' = p { p_time = t' }
            player p'
        False -> do
            es <- readSampleVar (p_input p)
            let p' = p { p_schedule = es }
            player p'
        
playDList :: (Double -> e -> IO ()) -> Double -> [(Maybe e, Double)] -> IO Double
playDList output t [] = return t
playDList output t ((i, dt):is) = do
    case i of
        Nothing -> return ()
        Just e  -> output t e
    let t' = t + dt
    pauseThreadUntil t'
    playDList output t' is
