import Data.Maybe (fromJust)
import qualified Data.Vector.Generic as V
import Mescaline.Database
import Mescaline.Database.SqlQuery
import Mescaline.Database.Unit (Segmentation(..))
import qualified Mescaline.Database.Feature as Feature
import qualified Mescaline.Database.Unit as Unit
import Mescaline.Meap.Import as Meap
import Database.HDBC (quickQuery')
import System.Environment (getArgs)
import Prelude hiding (and)

cmd_query dbFile pattern seg features = do
    (units, sfMap) <- withDatabase dbFile $ \c -> do
        unitQuery (quickQuery' c)
              ((url sourceFile `like` pattern) `and` (segmentation unit `eq` seg))
              (map (fromJust . flip lookup Meap.features) features)
    case units of
        Left e -> fail e
        Right us -> let ls = map (\(u, fs) -> show (Unit.id u) : map show (concatMap (V.toList.Feature.value) fs)) us
                    in putStr $ unlines (map unwords ls)

cmd_insert = undefined

cmd_delete = undefined

main = do
    (dbFile:cmd:args) <- getArgs
    case cmd of
        "query" -> do
            let (pattern:seg:features) = args
            cmd_query dbFile pattern (read seg :: Segmentation) features
        otherwise -> putStrLn "Usage: editfeature {query,insert,delete} ... ARGS"
