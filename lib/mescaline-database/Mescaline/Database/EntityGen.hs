#!/usr/bin/env runhaskell

{-# LANGUAGE QuasiQuotes #-}

import qualified Data.List as List
import qualified Data.Set as Set
import           Data.Generics
import           Database.Persist.Base
import           Database.Persist.TH
import           Database.Persist.GenericSql
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

q = share2 mkPersist (mkMigrate "migrateAll") [persist|
SourceFile
    url         FilePath Eq
    hash        Mescaline.Database.Hash.Hash Eq
    numChannels Int
    sampleRate  Double
    frames      Data.Int.Int64
    UniqueSourceFile hash
Unit
    sourceFile  SourceFileId Eq
    onset       Double
    duration    Double
Descriptor
    name        String Eq
    degree      Int
    UniqueDescriptor name
Feature
    unit        UnitId Eq Asc
    descriptor  DescriptorId Eq
    value       Mescaline.Database.Vector.Vector
    UniqueFeature unit descriptor
|]

removeDerivation :: Data a => [String] -> a -> a
removeDerivation ds = everywhere (mkT f)
    where
        s = Set.fromList ds
        remove = filter (not . flip Set.member s . showName)
        f (DataD x1 x2 x3 x4 ns) = DataD x1 x2 x3 x4 (remove ns)
        f (NewtypeD x1 x2 x3 x4 ns) = NewtypeD x1 x2 x3 x4 (remove ns)
        f (DataInstD x1 x2 x3 x4 ns) = DataInstD x1 x2 x3 x4 (remove ns)
        f (NewtypeInstD x1 x2 x3 x4 ns) = NewtypeInstD x1 x2 x3 x4 (remove ns)
        f x = x

extractModules :: Data a => a -> [String]
extractModules = map head . List.group . List.sort . everything (++) ([] `mkQ` f)
     where f (NameQ x) = [modString x]
           f (NameG _ _ x) = [modString x]
           f _ = []

mkModule :: (Data a, Ppr a) => [String] -> String -> String -> a -> String
mkModule exts name prelude e = 
    unlines ([ "{-# LANGUAGE " ++ List.intercalate ", " exts ++ " #-}"
             , "module " ++ name ++ " where" ]
             ++ map ("import qualified "++) (extractModules e))
             ++ "-- Prelude >>>\n" ++ prelude ++ "-- <<< Prelude\n"
             ++ show (ppr e)

main :: IO ()
main = do
    runQ q >>= putStrLn
                . mkModule ["RankNTypes", "TypeFamilies", "GeneralizedNewtypeDeriving"]
                           "Mescaline.Database.Entity"
                           (unlines [ "import Data.Map (Map)"
                                    , "type SourceFileMap = Map (Database.Persist.Base.Key SourceFile) SourceFile" ])
                . removeDerivation ["Web.Routes.Quasi.Classes.SinglePiece"]
