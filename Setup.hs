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
        [ "/usr/local/bin/runhugs"
        , "/usr/local/bin/scsynth"
        , "/usr/local/lib/supercollider/plugins/BinaryOpUGens.scx"
        , "/usr/local/lib/supercollider/plugins/ChaosUGens.scx"
        , "/usr/local/lib/supercollider/plugins/DelayUGens.scx"
        , "/usr/local/lib/supercollider/plugins/DemandUGens.scx"
        , "/usr/local/lib/supercollider/plugins/DiskIOUGens.scx"
        , "/usr/local/lib/supercollider/plugins/DynNoiseUGens.scx"
        , "/usr/local/lib/supercollider/plugins/FFT2_UGens.scx"
        , "/usr/local/lib/supercollider/plugins/FFT_UGens.scx"
        , "/usr/local/lib/supercollider/plugins/FilterUGens.scx"
        , "/usr/local/lib/supercollider/plugins/GendynUGens.scx"
        , "/usr/local/lib/supercollider/plugins/GrainUGens.scx"
        , "/usr/local/lib/supercollider/plugins/IOUGens.scx"
        , "/usr/local/lib/supercollider/plugins/KeyboardUGens.scx"
        , "/usr/local/lib/supercollider/plugins/LFUGens.scx"
        , "/usr/local/lib/supercollider/plugins/MachineListening.scx"
        -- NOTE: These cause crashes with Qt
        -- , "supercollider/plugins/MouseUGens.scx"
        , "/usr/local/lib/supercollider/plugins/MulAddUGens.scx"
        , "/usr/local/lib/supercollider/plugins/NoiseUGens.scx"
        , "/usr/local/lib/supercollider/plugins/OSCUGens.scx"
        , "/usr/local/lib/supercollider/plugins/PanUGens.scx"
        , "/usr/local/lib/supercollider/plugins/PhysicalModellingUGens.scx"
        , "/usr/local/lib/supercollider/plugins/ReverbUGens.scx"
        , "/usr/local/lib/supercollider/plugins/TestUGens.scx"
        , "/usr/local/lib/supercollider/plugins/TriggerUGens.scx"
        , "/usr/local/lib/supercollider/plugins/UnaryOpUGens.scx"
        , "/usr/local/lib/supercollider/plugins/UnpackFFTUGens.scx"
        ]
        -- DoNotChase
        -- ChaseWithDefaults
        (ChaseWith (defaultExclusions ++ ["libstdc++"]))

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
