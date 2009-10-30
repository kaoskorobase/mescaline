module Mescaline.Database.Query where

import 			 Mescaline.Database
import qualified Mescaline.Database.Unit as Unit
import qualified Mescaline.Database.SourceFile as SourceFile

import			 Prelude hiding (not)
import			 Prelude as Prelude

import 			 System.Path.WildMatch
import 			 System.FilePath

liftQ1 :: (Bool -> Bool) -> Query -> Query
liftQ1 f (Query a) = Query (\x -> f (a x))

liftQ2 :: (Bool -> Bool -> Bool) -> Query -> Query -> Query
liftQ2 f (Query a) (Query b) = Query (\x -> a x `f` b x)

(<&>) :: Query -> Query -> Query
(<&>) = liftQ2 (&&)

(<|>) :: Query -> Query -> Query
(<|>) = liftQ2 (||)

not :: Query -> Query
not = liftQ1 Prelude.not

withPath :: FilePath -> Query
withPath p = Query ((==) p . SourceFile.path . Unit.sourceFile)

pathMatch :: String -> Query
pathMatch s = Query (wildCheckCase s . SourceFile.path . Unit.sourceFile)

fileNameMatch :: String -> Query
fileNameMatch s = Query (wildCheckCase s . takeFileName . SourceFile.path . Unit.sourceFile)
