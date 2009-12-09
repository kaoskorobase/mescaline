module Mescaline.Sampler.GUI where
-- module Main where

import Graphics.UI.Gtk

chooseFile :: Window -> IO (Maybe FilePath)
chooseFile parent = do
    d <- fileChooserDialogNew
            (Just "Title")
            (Just parent)
            FileChooserActionOpen
            [ ("Okidoke", ResponseAccept)
            , ("Nooooo!", ResponseCancel) ]
    widgetShow d
    r <- dialogRun d
    widgetHide d
    case r of
        ResponseAccept -> fileChooserGetFilename d
        _              -> return Nothing

main :: (FilePath -> IO ()) -> IO ()
main buttonAction = do
    unsafeInitGUIForThreadedRTS
    window <- windowNew
    button <- buttonNew
    set window [ containerBorderWidth := 10,
                 containerChild := button ]
    set button [ buttonLabel := "Load Pattern" ]
    onClicked button (chooseFile window >>= ba)
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
    where
        ba Nothing  = return ()
        ba (Just p) = buttonAction p