SC_DIR = $(HOME)/dev/supercollider
CABAL = cabal

all:
	@echo Please specify a target.

.PHONY: upstream upstream-cabal-macosx upstream-data-accessor-mtl2 upstream-persistent
upstream: upstream-cabal-macosx upstream-data-accessor-mtl2 upstream-persistent
upstream-cabal-macosx:
	cd upstream/cabal-macosx && $(CABAL) install
upstream-data-accessor-mtl2:
	cd upstream/data-accessor-mtl2 && $(CABAL) install
upstream-persistent:
	cd upstream/persistent && $(CABAL) install
	cd upstream/persistent/backends/sqlite && $(CABAL) install
	cd upstream/persistent/packages/template && $(CABAL) install

.PHONY: mescaline-database mescaline-database-clean mescaline-database-configure mescaline-database-install
mescaline-database:
	cd lib/mescaline-database && $(CABAL) build
mescaline-database-clean:
	cd lib/mescaline-database && $(CABAL) clean
mescaline-database-configure:
	cd lib/mescaline-database && $(CABAL) configure
mescaline-database-install:
	cd lib/mescaline-database && $(CABAL) install

MESCALINE_CONFIGURE_ARGS = \
	--extra-include-dir=$(SC_DIR)/include/common \
	--extra-include-dir=$(SC_DIR)/include/plugin_interface \

.PHONY: mescaline mescaline-clean mescaline-configure mescaline-install
mescaline:
	cd lib/mescaline && $(CABAL) build
mescaline-clean:
	cd lib/mescaline && $(CABAL) clean
mescaline-configure:
	cd lib/mescaline && $(CABAL) configure $(MESCALINE_CONFIGURE_ARGS)
mescaline-install: mescaline-database-install
	cd lib/mescaline && $(CABAL) install $(MESCALINE_CONFIGURE_ARGS)

.PHONY: mescaline-tools mescaline-tools-clean mescaline-tools-configure mescaline-tools-install
mescaline-tools:
	cd tools && $(CABAL) build
mescaline-tools-clean:
	cd tools && $(CABAL) clean
mescaline-tools-configure:
	cd tools && $(CABAL) configure
mescaline-tools-install: mescaline-database-install
	cd tools && $(CABAL) install

.PHONY: mescaline-app mescaline-app-clean mescaline-app-configure mescaline-app-install
mescaline-app:
	cd app && $(CABAL) build
mescaline-app-clean:
	cd app && $(CABAL) clean
mescaline-app-configure:
	cd app && $(CABAL) configure
mescaline-app-install: mescaline-install
	cd app && $(CABAL) install

.PHONY: clean
clean: mescaline-database-clean mescaline-clean mescaline-tools-clean mescaline-app-clean
