#!/usr/bin/env ruby
require 'rubygems'
require 'sqlite3'
require 'digest/sha1'

db = SQLite3::Database.new("mescaline.db")

segmentfile = ARGV[0] 
featfile = ARGV[1]
cachefile = ARGV[2]

count = 0
File.open(segmentfile).each { |line|
    count+=1
    if count > 3
      splitted = line.split(" ") 
      row = db.execute("select id from source_file where path = ?",splitted[0]) 
      if row.length == 0
        db.execute("insert into source_file values(null,?,?)",splitted[0],Digest::SHA1.hexdigest(splitted[0]))
        row = db.execute("select id from source_file where path = ?",splitted[0]) 
        id = row[0][0]
      else
        id = row[0][0]
      end
      query = "select * from unit where sfid = " + id + " and onset_time = " + splitted[1] + " and chunk_length = " + splitted[2]
      #p query
      row = db.execute(query)
      if row.length == 0
        db.execute("insert into unit values(null,?,?,?)",id, splitted[1],splitted[2])
      end
    end
}
puts "everything done for #{ARGV[0]}" 
 
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

count = 0
File.open(featfile).each { |line|
  count+=1
  if count > 3
    
    splitted = line.split
    filename = splitted.shift
    row = db.execute("select id from source_file where path = ?", filename)
    if row.length > 0
      sfid = row[0][0]  
      onset_time = splitted.shift
      chunk_length = splitted.shift
      row2 = db.execute("select id from unit where sfid = ? and onset_time = ? and chunk_length = ?",sfid, onset_time, chunk_length)
      unit_id = row2[0][0] 
      feature_id = db.execute("select id from feature where name = ?","AvgChroma")
      avgChroma = splitted.slice!( 0, 12 )
      test = db.execute("select * from unit_feature where feature_id = ? and unit_id = ?", feature_id, unit_id)
      if test.length > 0
        db.execute("update unit_feature set arrayval = ? where feature_id = ? and unit_id = ?",SQLite3::Blob.new(avgChroma.pack('G'* avgChroma.length)), feature_id, unit_id )
      else
        db.execute("insert into unit_feature values(?,?,null,null,null,?)", unit_id, feature_id,  SQLite3::Blob.new(avgChroma.pack('G'* avgChroma.length)) )
      end
      feature_id = db.execute("select id from feature where name = ?","AvgChromaScalar")
      avgChromaScalar = splitted.shift
      test = db.execute("select * from unit_feature where feature_id = ? and unit_id = ?", feature_id, unit_id)
      if test.length > 0
        db.execute("update unit_feature set realval = ? where feature_id = ? and unit_id = ?",avgChromaScalar , feature_id, unit_id)
      else
        db.execute("insert into unit_feature values(?,?,null,null,null,?)", unit_id, feature_id,  avgChromaScalar)
      end
      feature_id = db.execute("select id from feature where name = ?","AvgChunkPower")
      avgChunkPower = splitted.shift
      test = db.execute("select * from unit_feature where feature_id = ? and unit_id = ?", feature_id, unit_id)
      if test.length > 0
        db.execute("update unit_feature set realval = ? where feature_id = ? and unit_id = ?",avgChunkPower, feature_id, unit_id )
      else
        db.execute("insert into unit_feature values(?,?,null,null,null,?)", unit_id, feature_id,  avgChunkPower)
      end
      feature_id = db.execute("select id from feature where name = ?","AvgFreqSimple")
      avgFreqSimple = splitted.shift
      test = db.execute("select * from unit_feature where feature_id = ? and unit_id = ?", feature_id, unit_id)
      if test.length > 0
        db.execute("update unit_feature set realval = ? where feature_id = ? and unit_id = ?",avgFreqSimple, feature_id, unit_id )
      else
        db.execute("insert into unit_feature values(?,?,null,null,null,?)", unit_id, feature_id,  avgFreqSimple)
      end
      feature_id = db.execute("select id from feature where name = ?","AvgMelSpec")
      avgMelSpec = splitted.slice!( 0, 40 )
      test = db.execute("select * from unit_feature where feature_id = ? and unit_id = ?", feature_id, unit_id)
      if test.length > 0
        db.execute("update unit_feature set arrayval = ? where feature_id = ? and unit_id = ?",SQLite3::Blob.new(avgMelSpec.pack('G'* avgMelSpec.length)), feature_id, unit_id )
      else
        db.execute("insert into unit_feature values(?,?,null,null,null,?)", unit_id, feature_id,  SQLite3::Blob.new(avgMelSpec.pack('G'* avgMelSpec.length)) )
      end
      feature_id = db.execute("select id from feature where name = ?","AvgMFCC")
      avgMFCC = splitted.slice!( 0, 13 )
      test = db.execute("select * from unit_feature where feature_id = ? and unit_id = ?", feature_id, unit_id)
      if test.length > 0
        db.execute("update unit_feature set arrayval = ? where feature_id = ? and unit_id = ?",SQLite3::Blob.new(avgMFCC.pack('G'* avgMFCC.length)), feature_id, unit_id )
      else
        db.execute("insert into unit_feature values(?,?,null,null,null,?)", unit_id, feature_id,  SQLite3::Blob.new(avgMFCC.pack('G'* avgMFCC.length)) )
      end
      feature_id = db.execute("select id from feature where name = ?","AvgPitchSimple")
      avgPitchSimple = splitted.shift
      test = db.execute("select * from unit_feature where feature_id = ? and unit_id = ?", feature_id, unit_id)
      if test.length > 0
        db.execute("update unit_feature set realval = ? where feature_id = ? and unit_id = ?",avgPitchSimple, feature_id, unit_id )
      else
        db.execute("insert into unit_feature values(?,?,null,null,null,?)", unit_id, feature_id,  avgPitchSimple)
      end
      feature_id = db.execute("select id from feature where name = ?","AvgSpec")
      avgSpec = splitted.slice!( 0, 513 )
      test = db.execute("select * from unit_feature where feature_id = ? and unit_id = ?", feature_id, unit_id)
      if test.length > 0
        db.execute("update unit_feature set arrayval = ? where feature_id = ? and unit_id = ?",SQLite3::Blob.new(avgSpec.pack('G'* avgSpec.length)), feature_id, unit_id )
      else
        db.execute("insert into unit_feature values(?,?,null,null,null,?)", unit_id, feature_id,  SQLite3::Blob.new(avgSpec.pack('G'* avgSpec.length)) )
      end
      feature_id = db.execute("select id from feature where name = ?","AvgSpecCentroid")
      avgSpecCentroid = splitted.shift
      test = db.execute("select * from unit_feature where feature_id = ? and unit_id = ?", feature_id, unit_id)
      if test.length > 0
        db.execute("update unit_feature set realval = ? where feature_id = ? and unit_id = ?",avgSpecCentroid, feature_id, unit_id )
      else
        db.execute("insert into unit_feature values(?,?,null,null,null,?)", unit_id, feature_id,  avgSpecCentroid)
      end
      feature_id = db.execute("select id from feature where name = ?","AvgSpecFlatness")
      avgSpecFlatness = splitted.shift
      test = db.execute("select * from unit_feature where feature_id = ? and unit_id = ?", feature_id, unit_id)
      if test.length > 0
        db.execute("update unit_feature set realval = ? where feature_id = ? and unit_id = ?",avgSpecFlatness, feature_id, unit_id )
      else
        db.execute("insert into unit_feature values(?,?,null,null,null,?)", unit_id, feature_id,  avgSpecFlatness)
      end
      
      
    else
      puts "soundfile #{filename} not found in table"
    end
  end

}
puts "everything done for #{ARGV[1]}"  


