# meap-extract

Automate the MEAPsoft segmenter and feature extractor....

## Usage

> $ /path/to/meap-extract --help

## Issues

* The feature vectors are normalized for each file, which prevents combining
  feature vectors from independent runs of the program.  
  *TODO*: Remove the normalization from `FeatExtractor.java`.
* The feature file is a plain text file with a flat format.  
  *TODO*: Write the segmentation and feature vectors into a SQL database.
* Caching is very ad-hoc, writing the cache information in the same directory
  as the source sound file.  
  *TODO*: Clean up caching and write the information to the same or a
  different database
