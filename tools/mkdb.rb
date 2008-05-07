#!/usr/bin/env ruby

require 'rubygems'
require 'sqlite3'
require 'digest/sha1'

db = SQLite3::Database.new("mescaline.db")

sql = <<SQL
    drop table if exists source_file;
    create table source_file (
      id integer primary key,
      path varchar(256),
      hash varchar(40)
    );
    drop table if exists unit;
    create table unit (
      id integer primary key,
      sfid int,
      onset_time float(16),
      chunk_length float(16)
    );
    drop table if exists feature;
    create table feature (
      id integer primary key,
      name varchar(32)
    );
    drop table if exists unit_feature;
    create table unit_feature (
      unit_id int,
      feature_id int,
      intval int4,
      realval float8,
      textval text,
      arrayval BLOB 
    );
SQL


  
db.execute_batch(sql)

features = ["AvgChroma",
 "AvgChromaScalar",
 "AvgChunkPower",
 "AvgFreqSimple",
 "AvgMelSpec",
 "AvgMFCC",
 "AvgPitchSimple",
 "AvgSpec",
 "AvgSpecCentroid",
 "AvgSpecFlatness",
 "ChunkLength",
 "ChunkStartTime",
 "Likelihood",
 "SpectralStability"]

features.each {|feature|
  db.execute("insert into feature values(null,?)",feature)
}

puts "feature"

# EOF