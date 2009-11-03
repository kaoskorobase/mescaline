# Mescaline

Audio mosaicing on drugs.

## Prototype

See [Prototype](doc/Prototype)

## Hierarchical structure of units

* patterns, sentences
* beats, words, syllables
* onsets, phonemes
* regular subdivision
* zerocrossings

## Unit database 

See [Database](doc/Database)

## TOOLS

### Segmentation Browser

* display hierarchical segmentation of one phrase in the database
* play segments on mouse-over/-click
* apply some simple transformations (reordering) from a transform library

### Feature space browser

* add/remove units
* PCA
* scatter plot
* relative weighting of features
* save/load configurations
* play segments on mouse-over/-click
* some continuous mosaicing in 2D (joystick)

### Pattern resynthesizer

* pattern emitting events of distinct types (sounds)
* map (clusters of) units to event types based on features
  * duration
  * pitch
  * harmony
  * GM drum sound
  * etc.
* export mapping
* morph between mappings

## TRANSFORMATIONS

* reorder units on a given hierarchy level (recurse)
* resynthesize patterns
  * max deviation from prototype (kNN)
  * reassignment of sounds

## Tools

* [meap-extract](tools/meap-extract/README)
