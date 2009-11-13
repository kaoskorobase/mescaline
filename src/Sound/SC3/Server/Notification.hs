-- | Server notification processors.
module Sound.SC3.Server.Notification (
    tr
  , synced
  , NodeNotification(..)
  , n_go , n_end , n_off , n_on , n_move , n_info
) where

import Sound.SC3.Server.State (NodeId)
import Sound.OpenSoundControl (OSC(..), Datum(..))

tr :: NodeId -> Maybe Int -> OSC -> Maybe Double
tr n (Just i) (Message "/tr" [Int n', Int i', Float r]) | fromIntegral n == n' && i == i' = Just r
tr n Nothing  (Message "/tr" [Int n', Int _, Float r])  | fromIntegral n == n' = Just r
tr _ _        _                                         = Nothing

synced :: Int -> OSC -> Maybe Int
synced i (Message "/synced" [Int j]) | j == i = Just i
synced _ _                                    = Nothing

data NodeNotification =
    SynthNotification NodeId NodeId NodeId NodeId
  | GroupNotification NodeId NodeId NodeId NodeId NodeId NodeId

n_notification :: String -> NodeId -> OSC -> Maybe NodeNotification
n_notification s nid (Message s' (Int nid':Int g:Int p:Int n:Int b:r))
    | s == s' && fromIntegral nid == nid' =
        case b of
            0 -> Just $ SynthNotification nid (fromIntegral g) (fromIntegral p) (fromIntegral n)
            _ -> case r of
                    [Int h, Int t] -> Just $ GroupNotification nid (fromIntegral g) (fromIntegral p) (fromIntegral n) (fromIntegral h) (fromIntegral t)
                    _              -> Just $ GroupNotification nid (fromIntegral g) (fromIntegral p) (fromIntegral n) (fromIntegral (-1 :: Int)) (fromIntegral (-1 :: Int))
n_notification _ _ _ = Nothing

n_go :: NodeId -> OSC -> Maybe NodeNotification
n_go = n_notification "/n_go"

n_end :: NodeId -> OSC -> Maybe NodeNotification
n_end = n_notification "/n_end"

n_off :: NodeId -> OSC -> Maybe NodeNotification
n_off = n_notification "/n_off"

n_on :: NodeId -> OSC -> Maybe NodeNotification
n_on = n_notification "/n_on"

n_move :: NodeId -> OSC -> Maybe NodeNotification
n_move = n_notification "/n_move"

n_info :: NodeId -> OSC -> Maybe NodeNotification
n_info = n_notification "/n_info"
