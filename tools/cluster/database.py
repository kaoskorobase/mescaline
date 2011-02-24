import binascii
import fnmatch
import itertools
import numpy as np
import sqlite3
import struct
import sys

class SourceFile:
    def __init__(self, id, url, hash, numChannels, sampleRate, frames):
        self.__id = id
        self.__url = url
        self.__hash = binascii.b2a_hex(hash)
        self.__numChannels = numChannels
        self.__sampleRate = sampleRate
        self.__frames = frames

    def id(self): return self.__id
    def url(self): return self.__url
    def hash(self): return self.__hash
    def numChannels(self): return self.__numChannels
    def sampleRate(self): return self.__sampleRate
    def frames(self): return self.__frames

    def __repr__(self):
        return str(self.__class__) + str ((self.id(), self.url(), self.hash(), self.numChannels(), self.sampleRate(), self.frames()))

class Unit:
    def __init__(self, id, sourceFile, onset, duration):
        self.__id = id
        self.__sourceFile = sourceFile
        self.__onset = onset
        self.__duration = duration
    
    def id(self): return self.__id
    def sourceFile(self): return self.__sourceFile
    def onset(self): return self.__onset
    def duration(self): return self.__duration

    def __repr__(self):
        return str(self.__class__) + str((self.id(), self.sourceFile(), self.onset(), self.duration()))

class Descriptor:
    def __init__(self, id, name, degree):
        self.__id = id
        self.__name = name
        self.__degree = degree
    
    def id(self): return self.__id
    def name(self): return self.__name
    def degree(self): return self.__degree

    def __repr__(self):
        return str(self.__class__) + str((self.id(), self.name(), self.degree()))

class Feature:
    def __init__(self, id, unit, descriptor, ndarray):
        self.__id = id
        self.__unit = unit
        self.__descriptor = descriptor
        # Convert from buffer to ndarray: 4-bytes of length followed by 8-byte floats (big-endian)
        self.__value = ndarray
    
    @staticmethod
    def from_buffer(id, unit, descriptor, buffer):
        return Feature(id, unit, descriptor, np.ndarray(shape=((len(buffer)-4)/8,), dtype='>f8', offset=4, buffer=buffer))
        
    def id(self): return self.__id
    def unit(self): return self.__unit
    def descriptor(self): return self.__descriptor
    def value(self): return self.__value

    def to_sql_row(self):
        b = struct.pack(">i" + ("d" * self.value().size), self.value().size, *self.value().tolist())
        return (self.unit().id(), self.descriptor().id(), buffer(b))

    def __repr__(self):
        return str(self.__class__) + str((self.id(), self.unit(), self.descriptor(), self.value()))

def get_descriptor_map(c):
    dm = {}
    for row in c.execute('select * from Descriptor'):
        d = Descriptor(*row)
        dm[str(d.name())] = d
    return dm

def get_descriptor(c, name, degree):
    ds = get_descriptor_map(c)
    if ds.has_key(name):
        d = ds[name]
        if d.degree() != degree:
            raise RuntimeError("Descriptor '" + name + "' degree mismatch: expected " + str(d.degree()) + ", got " + str(degree))
        else:
            return d
    cursor = c.cursor()
    cursor.execute('insert into Descriptor values (null, ?, ?)', (name, degree))
    id = cursor.lastrowid
    cursor.close()
    c.commit()
    return Descriptor(id, name, degree)

def get_source_file_map(c, pattern='*'):
    return dict(itertools.ifilter( lambda t: fnmatch.fnmatch(t[1].url(), pattern)
                                 , itertools.imap( lambda row: (row[0], SourceFile(*row))
                                                 , c.execute('select * from SourceFile'))))

def get_units(c, source_files):
    for sf in source_files:
        for row in c.execute('select * from Unit where sourceFile = ?', (sf.id(),)):
            yield Unit(row[0], sf, row[2], row[3])

def get_features(c, descriptors, units):
    def mk_feature(u, d, i):
        row = list(i)[0]
        return Feature.from_buffer(row[0], u, d, row[3])    
    return dict((u.id(), list(mk_feature(u, d, c.execute( 'select * from Feature where unit = ? and descriptor = ?'
                                                        , (u.id(), d.id())))
                         for d in descriptors))
                for u in units)

def delete_feature(c, descriptor):
    c.execute('delete from Feature where descriptor = ?', (descriptor.id(),))

def insert_features(c, features):
    c.executemany('insert into Feature(unit,descriptor,value) values (?, ?, ?)', itertools.imap(Feature.to_sql_row, features))
    c.commit()

if __name__ == "__main__":
    conn = sqlite3.connect(sys.argv[1])
    sm = get_source_file_map(conn)
    print sm
    # print map(lambda x: x[1].numChannels, sm)
    # print sm.values()
    dm = get_descriptor_map(conn)
    # print type(dm['es.globero.mescaline.spectral'].degree())
    us = get_units(conn, sm.values())
    # fs = get_features(conn, dm.values(), us)
    # print map(lambda x: Feature.value(x[1][1]), fs)
    # print fs
    # print get_descriptor("es.globero.spectral_cluster", 2)

    d = get_descriptor(conn, "es.globero.mescaline.cluster", 2)
    print d
    u = list(us)[0]
    delete_feature(conn, d)
    v = np.array([1, 2])
    insert_features(conn, [Feature(-1, u, d, v)])
    print get_features(conn, [d], [u])
    conn.close()
