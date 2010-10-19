{-# LANGUAGE CPP #-}

#if darwin_HOST_OS == 1
import Distribution.MacOSX
#endif
import Distribution.Simple

main :: IO ()

#if darwin_HOST_OS == 1
guiApps :: [MacApp]
guiApps = [MacApp "Mescaline"
                  (Just "app/mescaline.icns")
                  Nothing -- Build a default Info.plist for the icon.
                  -- Resources to include
                  [ "resources/mescaline.ui"
                  , "resources/colors.txt" ]
                  -- Binaries to include
                  [ "supercollider/scsynth"
                  , "supercollider/plugins/BinaryOpUGens.scx"
                  , "supercollider/plugins/ChaosUGens.scx"
                  , "supercollider/plugins/DelayUGens.scx"
                  , "supercollider/plugins/DemandUGens.scx"
                  , "supercollider/plugins/DiskIOUGens.scx"
                  , "supercollider/plugins/DynNoiseUGens.scx"
                  , "supercollider/plugins/FFT2_UGens.scx"
                  , "supercollider/plugins/FFT_UGens.scx"
                  , "supercollider/plugins/FilterUGens.scx"
                  , "supercollider/plugins/GendynUGens.scx"
                  , "supercollider/plugins/GrainUGens.scx"
                  , "supercollider/plugins/IOUGens.scx"
                  , "supercollider/plugins/KeyboardUGens.scx"
                  , "supercollider/plugins/LFUGens.scx"
                  , "supercollider/plugins/MachineListening.scx"
                  , "supercollider/plugins/MouseUGens.scx"
                  , "supercollider/plugins/MulAddUGens.scx"
                  , "supercollider/plugins/NoiseUGens.scx"
                  , "supercollider/plugins/OSCUGens.scx"
                  , "supercollider/plugins/PanUGens.scx"
                  , "supercollider/plugins/PhysicalModellingUGens.scx"
                  , "supercollider/plugins/ReverbUGens.scx"
                  , "supercollider/plugins/TestUGens.scx"
                  , "supercollider/plugins/TriggerUGens.scx"
                  , "supercollider/plugins/UnaryOpUGens.scx"
                  , "supercollider/plugins/UnpackFFTUGens.scx"
                  ]
                  DoNotChase
                  -- ChaseWithDefaults
                  -- (ChaseWith (defaultExclusions ++ ["libstdc++"]))
          ]

main = defaultMainWithHooks $ simpleUserHooks {
            -- TODO: Make this dependent on a configure flag (preConfHook)
            postBuild = appBundleBuildHook guiApps -- no-op if not MacOS X
       }
#else
main = defaultMain
#endif
