SC_DIR = $(HOME)/dev/supercollider
CABAL = cabal

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

.PHONY: lib/mescaline-database
lib/mescaline-database:
	cd $@ && $(CABAL) install

MESCALINE_CONFIGURE_ARGS = \
	--extra-include-dir=$(SC_DIR)/include/common \
	--extra-include-dir=$(SC_DIR)/include/plugin_interface \

.PHONY: mescaline mescaline-configure mescaline-build mescaline-clean

mescaline: lib/hsc3-server lib/mescaline-database
	$(CABAL) install $(MESCALINE_CONFIGURE_ARGS)

mescaline-configure:
	$(CABAL) configure $(MESCALINE_CONFIGURE_ARGS)

mescaline-build:
	$(CABAL) build

mescaline-clean:
	$(CABAL) clean

.PHONY: mescaline-tools mescaline-tools-configure mescaline-tools-build mescaline-tools-clean

mescaline-tools: lib/mescaline-database
	cd tools && $(CABAL) install

mescaline-tools-configure:
	cd tools && $(CABAL) configure

mescaline-tools-build:
	cd tools && $(CABAL) build

mescaline-tools-clean:
	cd tools && $(CABAL) clean

.PHONY: mescaline-app mescaline-app-clean

mescaline-app:
	cd app && $(CABAL) install

mescaline-app-clean:
	cd app && $(CABAL) clean
