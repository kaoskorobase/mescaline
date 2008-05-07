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
      query = "select * from unit where sfid = " + id + " and start_seconds = " + splitted[1] + " and end_seconds = " + splitted[2]
      #p query
      row = db.execute(query)
      if row.length == 0
        db.execute("insert into unit values(null,?,?,?)",id, splitted[1],splitted[2])
      end
    end
}

count = 0
File.open(featfile).each { |line|
  count+=1
  if count > 3
    splitted = line.split(" ")
    
  end
}




db.execute( "select * from source_file" ) do |row|
  #p row
end


