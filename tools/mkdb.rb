#!/usr/bin/env ruby

require 'rubygems'
require 'digest/sha1'
require 'enumerator'
require 'find'
require 'sqlite3'
require 'uri'

MAX_VECTOR_SIZE = 40

MEDIAN = 0

MAKE_SQL_TABLES = <<SQL
    drop table if exists source_file;
    create table source_file (
      id    integer primary key,
      path  varchar(256),
      hash  varchar(40)
    );
    CREATE INDEX idx_source_file ON source_file(id);
    
    drop table if exists unit;
    create table unit (
      id              integer primary key,
      source_file_id  int,
      onset           float8,
      duration        float8
    );
    CREATE INDEX idx_unit ON unit(id,source_file_id);
    
    drop table if exists feature_descriptor;
    create table feature_descriptor (
      id          integer primary key,
      name        varchar(32),
      description text,
      scalar      bool
    );
    CREATE INDEX idx_feature_descriptor ON feature_descriptor(id);
    
    drop table if exists unit_feature_scalar;
    create table unit_feature_scalar (
      unit_id       int,
      feature_id    int,
      int_value     int4,
      real_value    float8,
      text_value    text
    );
    CREATE INDEX idx_unit_feature_scalar ON unit_feature_scalar(unit_id,feature_id);
    
    drop table if exists unit_feature_vector;
    create table unit_feature_vector (
      unit_id       integer,
      feature_id    integer,
      size          int,
      #{(0..MAX_VECTOR_SIZE-1).collect { |i| "'#{i}' float8" }.join(",")}
    );
    CREATE INDEX idx_unit_feature_vector ON unit_feature_vector(unit_id,feature_id);
SQL

Feature = Struct.new("Feature", :name, :slice, :id)

class Feature
  def scalar?
    self.slice.to_a.size > 1
  end
  def vector?
    !self.scalar?
  end
end

def make_feature_descriptors(db)
  offset = 0
  features = [
    ["Chroma",            12],
    ["ChromaCentroid",    1],
    ["Power",             1],
    ["SpectralPeakFreq",  1],
    ["MelSpec",           40],
    ["MFCC",              13],
    ["SpectralPeakPitch", 1],
#    ["Spec", 513],
    ["SpectralCentroid",  1],
    ["SpectralFlatness",  1]
  ].enum_for(:each_with_index).collect { |a,i|
    name, size = a
    feature = Feature.new(name, (offset..offset+size-1), -1)
    offset += size
    feature
  }
  features.each { |feature|
    db.execute("insert into feature_descriptor values(null,?,null,?)", feature.name, feature.scalar?)
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
    file.each { |line|
      unless /^#/ =~ line
        line = line.split
      
        filename = URI.unescape(line.shift)
        onset    = line.shift.to_f
        duration = line.shift.to_f
        values   = line.collect { |x| x.to_f }

        if sfid.nil?
          # insert source file
          db.execute("insert into source_file values(null,?,?)", filename, make_file_hash(filename))
          sfid = db.last_insert_row_id()
        end
      
        # insert unit
        db.execute("insert into unit values(null,?,?,?)", sfid, onset, duration)
        uid = db.last_insert_row_id()
      
        # insert features
        features.each { |f|
          fvalue = values.slice(f.slice)
          if fvalue.size == 1
            # scalar value
            db.execute(
              "insert into unit_feature_scalar values(?,?,null,?,null)",
              uid, f.id, fvalue[0])
          else
            # vector value
            # db.execute(
            #   "insert into unit_feature values(?,?,null,null,null,?)",
            #   uid, f.id, SQLite3::Blob.new([fvalue.size].pack('N') + fvalue.pack('G'*fvalue.size)))
            if fvalue.size > MAX_VECTOR_SIZE
              raise "#{filename} (#{f.name}): MAX_VECTOR_SIZE exceeded (#{fvalue.size})"
            end
            cols = (["?"] * fvalue.size) + (["null"] * (MAX_VECTOR_SIZE - fvalue.size))
            db.execute(
              "insert into unit_feature_vector values(?,?,?,#{cols.join(",")})",
              uid, f.id, fvalue.size, *fvalue)
          end
        }
      end # if
    }
  }  
end

def get_feat_files(*args)
  res = []
  args.each { |dir|
    Find.find(dir) { |file|
      if File.file?(file) && /\.com\.meapsoft\.feat$/ =~ file
        res << file
      end
    }
  }
  res
end

if ARGV.size < 2
  puts "Usage: #{File.basename($0)} DATABASE DIRECTORY..."
  exit(1)
end

db = SQLite3::Database.new(ARGV.shift)
db.execute_batch(MAKE_SQL_TABLES)

FEATURES = make_feature_descriptors(db)
puts "=" * 50
print_features(FEATURES)
puts "=" * 50 + "\n\n"

feat_files = get_feat_files(*ARGV)
puts "Processing #{feat_files.size} feature files"

feat_files.each_with_index { |file,i|
  $stdout.printf("\b\b\b\b%3.d%%", (i/feat_files.size.to_f*100).to_i)
  $stdout.flush
  insert_features(db, FEATURES, file)
}

puts ""

# EOF
