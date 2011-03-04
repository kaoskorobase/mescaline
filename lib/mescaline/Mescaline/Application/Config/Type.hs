{-# LANGUAGE CPP, OverloadedStrings #-}

#include "Accessor.h"

module Mescaline.Application.Config.Type where

import Data.Accessor
import Data.Aeson.Config (Configurable(..), option, section)

data BufferCache = BufferCache {
    _logLevelBufferCache :: String
  } deriving (Show)

ACCESSOR(logLevelBufferCache, _logLevelBufferCache, BufferCache, String)

instance Configurable BufferCache where
    config = option "logLevel" logLevelBufferCache

data Database = Database {
    _logLevelDatabase :: String
  } deriving (Show)

instance Configurable Database where
    config = option "logLevel" logLevelDatabase

ACCESSOR(logLevelDatabase, _logLevelDatabase, Database, String)

data FeatureSpace = FeatureSpace {
    _logLevelFeatureSpace :: String
  } deriving (Show)

ACCESSOR(logLevelFeatureSpace, _logLevelFeatureSpace, FeatureSpace, String)

instance Configurable FeatureSpace where
    config = option "logLevel" logLevelFeatureSpace

data Hugs = Hugs {
    _logLevelHugs :: String
  } deriving (Show)

ACCESSOR(logLevelHugs, _logLevelHugs, Hugs, String)

instance Configurable Hugs where
    config = option "logLevel" logLevelHugs

data Sequencer = Sequencer {
    _debugFill :: Bool
  , _logLevelSequencer :: String
  } deriving (Show)

ACCESSOR(debugFill, _debugFill, Sequencer, Bool)
ACCESSOR(logLevelSequencer, _logLevelSequencer, Sequencer, String)

instance Configurable Sequencer where
    config = do
        option "debugFill" debugFill
        option "logLevel" logLevelSequencer

data Synth = Synth {
    _useInternalServer :: Bool
  , _scheduleCompletionBundles :: Bool
  , _logLevelSynth :: String
  } deriving (Show)

ACCESSOR(useInternalServer, _useInternalServer, Synth, Bool)
ACCESSOR(scheduleCompletionBundles, _scheduleCompletionBundles, Synth, Bool)
ACCESSOR(logLevelSynth, _logLevelSynth, Synth, String)

instance Configurable Synth where
    config = do
        option "useInternalServer" useInternalServer
        option "scheduleCompletionBundles" scheduleCompletionBundles
        option "logLevel" logLevelSynth

data Config = Config {
    _bufferCache :: BufferCache
  , _database :: Database
  , _featureSpace :: FeatureSpace
  , _hugs :: Hugs
  , _sequencer :: Sequencer
  , _synth :: Synth
  } deriving (Show)

ACCESSOR(bufferCache, _bufferCache, Config, BufferCache)
ACCESSOR(database, _database, Config, Database)
ACCESSOR(featureSpace, _featureSpace, Config, FeatureSpace)
ACCESSOR(hugs, _hugs, Config, Hugs)
ACCESSOR(sequencer, _sequencer, Config, Sequencer)
ACCESSOR(synth, _synth, Config, Synth)

instance Configurable Config where
    config = do
        section "BufferCache" bufferCache config
        section "Database" database config
        section "FeatureSpace" featureSpace config
        section "Hugs" hugs config
        section "Sequencer" sequencer config
        section "Synth" synth config

defaultLogLevel :: String
defaultLogLevel = "notice"

defaultConfig :: Config
defaultConfig =
    Config
        (BufferCache defaultLogLevel)
        (Database defaultLogLevel)
        (FeatureSpace defaultLogLevel)
        (Hugs defaultLogLevel)
        (Sequencer False defaultLogLevel)
        (Synth True True defaultLogLevel)
