-- import Distribution.Simple
-- main = defaultMain


import Distribution.MacOSX
import Distribution.Simple

guiApps :: [MacApp]
guiApps = [MacApp "Mescaline"
                  (Just "app/mescaline.icns")
                  Nothing -- Build a default Info.plist for the icon.
                  [ "app/mescaline.ui"
                  , "app/regions.txt" ]
                  [] -- No other binaries.
                  DoNotChase -- Try changing to ChaseWithDefaults
          ]

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
         postBuild = appBundleBuildHook guiApps -- no-op if not MacOS X
       }
