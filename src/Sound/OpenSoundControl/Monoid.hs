module Sound.OpenSoundControl.Monoid () where

import Data.Monoid (Monoid(..))
import Sound.OpenSoundControl

instance Monoid (OSC) where
    mempty = Bundle immediately []
    mappend m1@(Message _ _) m2@(Message _ _)  = Bundle immediately [m1, m2]
    mappend m@(Message _ _)     (Bundle t xs)  = Bundle t           (m:xs)
    mappend   (Bundle t xs)   m@(Message _ _)  = Bundle t           (xs++[m])
    mappend   (Bundle _ [])     (Bundle _ [])  = mempty
    mappend   (Bundle t xs)     (Bundle _ [])  = Bundle t           xs
    mappend   (Bundle _ [])     (Bundle t xs)  = Bundle t           xs
    mappend   (Bundle t xs1)    (Bundle _ xs2) = Bundle t           (xs1++xs2)
