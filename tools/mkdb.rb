#!/usr/bin/env ruby

require 'rubygems'
require 'digest/sha1'
require 'enumerator'
require 'find'
require 'sqlite3'
require 'uri'
require 'progress-meter'

MAKE_SQL_TABLES = <<SQL
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
      onset_time float8,
      chunk_length float8
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

Feature = Struct.new("Feature", :name, :slice, :id)

def make_features(db)
  offset = 0
  features = [
    ["AvgChroma", 12],
    ["AvgChromaScalar", 1],
    ["AvgChunkPower", 1],
    ["AvgFreqSimple", 1],
    ["AvgMelSpec", 40],
    ["AvgMFCC", 13],
    ["AvgPitchSimple", 1],
    ["AvgSpec", 513],
    ["AvgSpecCentroid", 1],
    ["AvgSpecFlatness", 1]
  ].enum_for(:each_with_index).collect { |a,i|
    name, size = a
    feature = Feature.new(name, (offset..offset+size-1), -1)
    offset += size
    feature
  }
  features.each { |feature|
    db.execute("insert into feature values(null,?)", feature.name)
    feature.id = db.last_insert_row_id()
  }
  features
end

def print_features(features)
  $stdout << "Features:\n"
  features.each { |f|
    $stdout \
      <<  ("%2d " % f.id) << f.name << "\n" \
      << "  " << f.slice << "\n"
  }
end

def make_file_hash(file)
  Digest::SHA1.hexdigest(File.read(file))
end

# insert features from featfile into db
# assumptions:
# * only one source file per feature file
def insert_features(db, features, featfile)
  sfid = nil
  
  File.open(featfile, "r") { |file|
    Progress.monitor(File.basename(featfile))
  
    file.each { |line|
      unless /^#/ =~ line
        line = line.split
      
        filename = URI.unescape(line.shift)
        onset_time = line.shift.to_f
        chunk_length = line.shift.to_f
        values = line.collect { |x| x.to_f }

        if sfid.nil?
          # insert source file
          db.execute("insert into source_file values(null,?,?)", filename, make_file_hash(filename))
          sfid = db.last_insert_row_id()
        end
      
        # insert unit
        db.execute("insert into unit values(null,?,?,?)", sfid, onset_time, chunk_length)
        uid = db.last_insert_row_id()
      
        # insert features
        features.each { |f|
          fvalue = values.slice(f.slice)
          if fvalue.size == 1
            # scalar value
            db.execute(
              "insert into unit_feature values(?,?,null,?,null,null)",
              uid, f.id, fvalue[0])
          else
            # vector value
            db.execute(
              "insert into unit_feature values(?,?,null,null,null,?)",
              uid, f.id, SQLite3::Blob.new(fvalue.pack('G'*fvalue.size)))
          end
        }
      end # if
    }
  }  
end

if ARGV.size < 2
  puts "Usage: #{File.basename($0)} DATABASE DIRECTORY..."
  exit(1)
end

db = SQLite3::Database.new(ARGV.shift)
db.execute_batch(MAKE_SQL_TABLES)

FEATURES = make_features(db)
puts "=" * 50
print_features(FEATURES)
puts "=" * 50 + "\n\n"

ARGV.each { |dir|
  Find.find(dir) { |file|
    if File.file?(file) && /\.com\.meapsoft\.feat$/ =~ file
      insert_features(db, FEATURES, file)
    end
  }
}

# EOF