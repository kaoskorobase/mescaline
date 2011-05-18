SC_DIR = $(HOME)/dev/supercollider
CABAL_ARGS :=
CABAL = cabal $(CABAL_ARGS)

all:
	@echo Please specify a target.

.PHONY: upstream upstream-cabal-macosx upstream-persistent
upstream: upstream-cabal-macosx upstream-persistent
upstream-cabal-macosx:
	cd upstream/cabal-macosx && $(CABAL) install
upstream-persistent:
	cd upstream/persistent && $(CABAL) install
	cd upstream/persistent/backends/sqlite && $(CABAL) install
	cd upstream/persistent/packages/template && $(CABAL) install

.PHONY: mescaline-database mescaline-database-clean mescaline-database-configure mescaline-database-install
mescaline-database:
MESCALINE_DATABASE_ENTITY = lib/mescaline-database/Mescaline/Database/Entity.hs
MESCALINE_DATABASE_DEPENDS = $(MESCALINE_DATABASE_ENTITY)
$(MESCALINE_DATABASE_ENTITY): $(MESCALINE_DATABASE_ENTITY:.hs=Gen.hs)
	runhaskell $? > $@
mescaline-database: $(MESCALINE_DATABASE_DEPENDS)
	cd lib/mescaline-database && $(CABAL) build
mescaline-database-clean:
	cd lib/mescaline-database && $(CABAL) clean
mescaline-database-configure: $(MESCALINE_DATABASE_DEPENDS)
	cd lib/mescaline-database && $(CABAL) configure
mescaline-database-install: $(MESCALINE_DATABASE_DEPENDS)
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

.PHONY: mescaline-app mescaline-app-clean mescaline-app-configure mescaline-app-install mescaline-app-dmg
mescaline-app:
	cd app && $(CABAL) build
mescaline-app-clean:
	cd app && $(CABAL) clean
mescaline-app-configure:
	cd app && $(CABAL) configure
mescaline-app-install: mescaline-install
	cd app && $(CABAL) install
# mescaline-app-dmg: mescaline-app-install
# 	version = `grep '^version:' mescaline.cabal`.split[1]
# 	osx_version = `sw_vers`.split("\n").collect { |x| x.split(":\t") }.find { |x| x[0] == "ProductVersion" }[1].sub(/\.[0-9]+$/, "")
# 
# 	src = "./dist/build/Mescaline.app"
# 	dst = "./dist/Mescaline-#{version}-#{osx_version}.dmg"
# 	icon = "app/Mescaline.icns"
# 
# 	volname = "Mescaline-#{version}"
# 	system("./tools/pkg-dmg --verbosity 0 --source \"#{src}\" --target \"#{dst}\" --sourcefile --volname Mescaline --icon \"#{icon}\" --symlink /Applications:/Applications")

MESCALINE_STS = tools/sts
.PHONY: mescaline-sts mescaline-sts-clean mescaline-sts-configure mescaline-sts-install
mescaline-sts:
	cd $(MESCALINE_STS) && $(CABAL) build
mescaline-sts-clean:
	cd $(MESCALINE_STS) && $(CABAL) clean
mescaline-sts-configure:
	cd $(MESCALINE_STS) && $(CABAL) configure
mescaline-sts-install: mescaline-install
	cd $(MESCALINE_STS) && $(CABAL) install

.PHONY: install
install: mescaline-tools-install mescaline-app-install mescaline-sts-install

.PHONY: clean
clean: mescaline-database-clean mescaline-clean mescaline-tools-clean mescaline-app-clean

HADDOCK_BASE_DOC = "http://www.haskell.org/ghc/docs/6.12.2/html/libraries/base-4.2.0.1/"

.PHONY: haddock
haddock:
	cd lib/mescaline && $(CABAL) haddock --hyperlink-source --html-location=#{base_doc}
	rsync -av lib/mescaline/dist/doc n222@null2.net:html/sk/sites/mescaline.globero.es/

.PHONY: logo
logo:
	inkscape doc/logo/mescaline_layers.svg --export-png=doc/logo/mescaline_layers.png
	makeicns -in doc/logo/mescaline_layers.png -out app/mescaline.icns
