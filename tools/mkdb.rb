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
      start_seconds float(16),
      end_seconds float(16)
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
      arrayval float8 array[1024]
    );
SQL


  
db.execute_batch(sql)

db.transaction do
  (1..16).each { |i|
    db.execute(
      "insert into source_file values(null,?,?)",
      "/path/to/testpath_#{i}",
      Digest::SHA1.hexdigest("testhash_#{i}")
    )
    db.execute(
      "insert into unit_feature values(?,?,null,null,null,?)",
      12, 16,
      "array[#{rand(i)}, #{rand(i)}, #{rand(i)}, #{rand(i)}]"
    )
  }
end

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
db.execute("select * from feature") do |row|
  puts row.join("|")
end
  
puts "source_file"
db.execute("select * from source_file") do |row|
  puts row.join("|")
end

puts "unit_feature"
db.execute("select * from unit_feature") do |row|
  puts row.join("|")
end

# EOF