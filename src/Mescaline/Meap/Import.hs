module Mescaline.Meap.Import where

import           Database.HDBC (IConnection, SqlType(..), SqlValue)
import qualified Database.HDBC          as DB
import qualified Database.HDBC.Sqlite3  as DB

import           Mescaline.Data.ListReader (ListReader, runListReader)
import qualified Mescaline.Data.ListReader as ListReader
import           Mescaline.Data.Unique (Unique)
import qualified Mescaline.Data.Unique as Unique
import           Mescaline.Database.Feature (Feature)
import qualified Mescaline.Database.Feature as Feature
import           Mescaline.Database.Model ()
import           Mescaline.Database.Unit (Unit)
import qualified Mescaline.Database.Unit as Unit
import           Mescaline.Database.SourceFile (SourceFile)
import qualified Mescaline.Database.SourceFile as SourceFile
import qualified Mescaline.Database.Table as Table
import           Mescaline.Data.Array.Vector
import           Mescaline.Meap.Chain as Chain
import           Sound.Analysis.Meapsoft as Meap

import           Data.List (intercalate)
import           Control.Monad (unless)
import           Text.Printf (printf)

import           Data.Accessor
import           Data.Accessor.Tuple

insertFeatures :: IConnection c => c -> [Meap.Feature] -> IO ()
insertFeatures c mfs = mapM_ (Table.insert c) fs
    where
        fs   = map f mfs
        f mf = Feature.consDescriptor (Meap.feature_name mf) (Meap.feature_degree mf)

insertFile :: IConnection c => c -> FilePath -> Meap.MEAP -> IO ()
insertFile c path meap = do
    Table.insert c sf
    insertFeatures c (Meap.features meap)
    where sf = SourceFile.cons path SourceFile.noHash 0 0 0
