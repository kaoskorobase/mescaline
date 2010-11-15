{-# LANGUAGE CPP #-}

import Distribution.PackageDescription
import Distribution.Simple
import System.FilePath (combine)

#if darwin_HOST_OS == 1
import Distribution.MacOSX
#endif

main :: IO ()

#if darwin_HOST_OS == 1
mescalineApp :: [FilePath] -> MacApp
mescalineApp resources =
    MacApp "Mescaline"
        (Just "app/mescaline.icns")
        Nothing -- Build a default Info.plist for the icon.
        -- Resources to include
        resources
        -- Binaries to include
        [ "resources/hugs/bin/runhugs"
        , "supercollider/scsynth"
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
        -- NOTE: These cause crashes with Qt
        -- , "supercollider/plugins/MouseUGens.scx"
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

postBuildHook args buildFlags pkgDesc buildInfo = do
    -- Use data files from package description as resources
    let resources = map (combine (dataDir pkgDesc)) (dataFiles pkgDesc)
    appBundleBuildHook [mescalineApp resources] args buildFlags pkgDesc buildInfo
    
main =
    defaultMainWithHooks $ simpleUserHooks {
        -- TODO: Make this dependent on a configure flag (preConfHook)
        postBuild = postBuildHook
    }
#else
main = defaultMain
#endif
