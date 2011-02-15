SC_DIR = $(HOME)/dev/supercollider/supercollider

GHC_BIN  = /usr/local/ghc/7.0.1/bin
GHC_PATH = $(GHC_BIN):$(PATH)
CABAL    = PATH=$(GHC_PATH) cabal

all:
	@echo Please specify a target.

upstream-cabal-macosx:
	cd upstream/cabal-macosx && $(CABAL) install
upstream-data-accessor-mtl2:
	cd upstream/data-accessor-mtl2 && $(CABAL) install
upstream-persistent:
	cd upstream/persistent && $(CABAL) install
	cd upstream/persistent/backends/sqlite && $(CABAL) install
	cd upstream/persistent/packages/template && $(CABAL) install
upstream: upstream-cabal-macosx upstream-data-accessor-mtl2 upstream-persistent
.PHONY: upstream upstream-cabal-macosx upstream-data-accessor-mtl2 upstream-persistent

MESCALINE_CONFIGURE_ARGS = \
	--extra-include-dir=$(SC_DIR)/include/{common,plugin_interface}

mescaline:
	$(CABAL) install $(MESCALINE_CONFIGURE_ARGS)

mescaline-configure:
	$(CABAL) configure $(MESCALINE_CONFIGURE_ARGS)

mescaline-build:
	$(CABAL) build

mescaline-clean:
	$(CABAL) clean

mescaline-tools:
	cd tools && $(CABAL) install

mescaline-tools-clean:
	cd tools && $(CABAL) clean

mescaline-app:
	cd app && $(CABAL) install

mescaline-app-clean:
	cd app && $(CABAL) clean