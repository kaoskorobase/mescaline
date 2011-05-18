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
    url         FilePath Eq Ne
    hash        Mescaline.Database.Hash.Hash Eq Ne
    numChannels Int Eq Ne Lt Le Gt Ge In
    sampleRate  Double Eq Ne Lt Le Gt Ge In
    frames      Data.Int.Int64 Eq Ne Lt Le Gt Ge
    UniqueSourceFile hash
Unit
    sourceFile  SourceFileId Eq Ne In
    onset       Double Eq Ne Lt Le Gt Ge Asc Desc
    duration    Double Eq Ne Lt Le Gt Ge Asc Desc
Descriptor
    name        String Eq Ne In
    degree      Int Eq Ne Lt Le Gt Ge In
    UniqueDescriptor name
Feature
    unit        UnitId Eq Ne Lt Le Gt Ge In Asc Desc
    descriptor  DescriptorId Eq Ne In
    UniqueFeature unit descriptor
Value
    feature     FeatureId Eq Ne In
    index       Int Asc
    value       Double Eq Ne Lt Le Gt Ge
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
                           []
                . removeDerivation ["Web.Routes.Quasi.Classes.SinglePiece"]
