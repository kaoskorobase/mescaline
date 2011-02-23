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
conn.close()

fs = list(fm[k][0] for k in sorted(fm.keys()))

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

print scipy.cluster.hierarchy.inconsistent(link)
# tree = scipy.cluster.hierarchy.to_tree(link)
# print tree

pylab.show()
