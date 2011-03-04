#!/usr/bin/env python

import database as db
import matplotlib
# matplotlib.use('MacOSX')
import numpy as np
import pylab
import scipy.spatial.distance
import scipy.cluster.hierarchy
import sqlite3
import sys

MFCC_DESCRIPTOR = 'http://vamp-plugins.org/rdf/plugins/qm-vamp-plugins#qm-mfcc'

conn = sqlite3.connect(sys.argv[1])
sm = db.get_source_file_map(conn)
dm = db.get_descriptor_map(conn)
us = db.get_units(conn, sm.values())
fm = db.get_features(conn, [dm[MFCC_DESCRIPTOR]], us)

fs = list(fm[k][0] for k in sorted(fm.keys()))
print len(fs)

data = np.array(map(db.Feature.value, fs))
print data.shape

dist = scipy.spatial.distance.pdist(data)
print dist.shape

link = scipy.cluster.hierarchy.linkage(dist)
print link.shape

# class myplot(object):
#     def __init__(self, filename):
#         self._filename = filename
# 
#     def resetFileName(self, fileName):
#         self._filename = fileName
# 
#     def __call__(self):
#         matplotlib.pylab.savefig(self._filename)
# 
# plotfunction = myplot("foo.pdf")
# matplotlib.pylab.draw_if_interactive = plotfunction

dendro = scipy.cluster.hierarchy.dendrogram(link, p = 10, truncate_mode = 'lastp')
# print dendro

# print scipy.cluster.hierarchy.inconsistent(link)
# tree = scipy.cluster.hierarchy.to_tree(link)
# print tree

def mk_feature(u, d, nc, c):
    return db.Feature(None, u, d, np.array([c]))

for nc in [1, 2, 3, 4, 5, 6, 7, 8]:
    clust = scipy.cluster.hierarchy.fcluster(link, nc, criterion='maxclust').tolist()
    # print clust
    units = map(db.Feature.unit, fs)
    desc = db.get_descriptor(conn, "es.globero.mescaline.cluster_%d" % (nc,), 1, True)
    db.delete_feature(conn, desc)
    clust_fs = map(lambda xs: mk_feature(xs[0], desc, nc, xs[1]), zip(units, clust))
    db.insert_features(conn, clust_fs)
    conn.commit()
    clust_fs_2 = map(lambda l: l[0], db.get_features(conn, [desc], units).values())
    if not (map(lambda f: f.value().tolist(), clust_fs) == map(lambda f: f.value().tolist(), clust_fs_2)):
        raise RuntimeError("DB value doesn't match computed value")

conn.close()

# pylab.show()
