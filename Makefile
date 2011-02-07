SC_DIR = $(HOME)/dev/supercollider/supercollider

ADDITIONAL_SOURCES = \
	./upstream/data-accessor/mtl \
	./upstream/persistent/ \
	./upstream/persistent/backends/sqlite/ \
	./upstream/hmatrix-0.11.0.0/ \
	$(HOME)/dev/haskell/hsc3-process/

GHC_BIN = /usr/local/ghc/7.0.1/bin
IPHONE_SIMULATOR = /opt/iphone/bin/iphone-simulator

all: mescaline-install

# mescaline-install:
# 	PATH=$(GHC_BIN):$(PATH) cabal-dev -s cabal-dev add-source $(ADDITIONAL_SOURCES)
# 	PATH=$(GHC_BIN):$(PATH) cabal-dev -v -s cabal-dev install \
# 		--flags='-template-haskell -ios analysis' \
# 		--extra-include-dir=$(SC_DIR)/include/{common,plugin_interface}
# 		# --extra-lib-dir=$(HOME)/dev/supercollider/supercollider/platform/iphone/build_iphone/Debug-iphonesimulator/

mescaline-install:
	# PATH=$(GHC_BIN):$(PATH) cabal $(ADDITIONAL_SOURCES)
	PATH=$(GHC_BIN):$(PATH) cabal configure \
		--flags='-template-haskell -ios analysis' \
		--extra-include-dir=$(SC_DIR)/include/{common,plugin_interface}
		# --extra-lib-dir=$(HOME)/dev/supercollider/supercollider/platform/mac/build 
		# --ld-options="-framework CoreAudio"
	PATH=$(GHC_BIN):$(PATH) cabal build

# mescaline:
# 	PATH=$(GHC_BIN):$(PATH) cabal-dev -s cabal-dev install

iphone-simulator-configure:
	PATH=$(IPHONE_SIMULATOR):$(PATH) cabal-dev -s cabal-dev-iphone-simulator add-source $(ADDITIONAL_SOURCES)
	PATH=$(IPHONE_SIMULATOR):$(PATH) cabal-dev -s cabal-dev-iphone-simulator configure \
		--flags='-template-haskell ios' \
		--extra-include-dir=$(SC_DIR)/include/{common,plugin_interface} \
		--extra-lib-dir=$(HOME)/dev/supercollider/supercollider/platform/iphone/build_iphone/Debug-iphonesimulator/

iphone-simulator:
	PATH=$(IPHONE_SIMULATOR):$(PATH) cabal-dev -s cabal-dev-iphone-simulator build
