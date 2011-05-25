SC_DIR = $(HOME)/dev/supercollider
ACTION := build
ARGS :=
CABAL = ./tools/cabal-build.rb $(ACTION)

all:
	@echo Please specify a target.

configure:
	$(eval ACTION := configure)
clean:
	$(eval ACTION := clean)
install:
	$(eval ACTION := install)
.PHONY: configure clean install

upstream: upstream-cabal-macosx upstream-persistent
upstream-cabal-macosx:
	cd upstream/cabal-macosx && $(CABAL) install
upstream-persistent: install
	$(CABAL) upstream/persistent $(ARGS)
	$(CABAL) upstream/persistent/backends/sqlite $(ARGS)
	$(CABAL) upstream/persistent/packages/template $(ARGS)
.PHONY: upstream upstream-cabal-macosx upstream-persistent

# mescaline-database
MESCALINE_DATABASE_ENTITY = lib/mescaline-database/Mescaline/Database/Entity.hs
MESCALINE_DATABASE_DEPENDS = $(MESCALINE_DATABASE_ENTITY)
$(MESCALINE_DATABASE_ENTITY): $(MESCALINE_DATABASE_ENTITY:.hs=Gen.hs)
	runhaskell $? > $@
mescaline-database: $(MESCALINE_DATABASE_DEPENDS)
	$(CABAL) lib/mescaline-database $(ARGS)
.PHONY: mescaline-database

# mescaline
MESCALINE_CONFIGURE_ARGS = \
	--extra-include-dir=$(SC_DIR)/include/common \
	--extra-include-dir=$(SC_DIR)/include/plugin_interface \

mescaline:
	$(CABAL) lib/mescaline $(MESCALINE_CONFIGURE_ARGS) $(ARGS)
.PHONY: mescaline

mescaline-tools:
	$(CABAL) tools $(ARGS)
.PHONY: mescaline-tools

mescaline-app:
	$(CABAL) app
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
.PHONY: mescaline-app mescaline-app-dmg

MESCALINE_STS = tools/sts
mescaline-sts:
	$(CABAL) $(MESCALINE_STS) $(ARGS)
.PHONY: mescaline-sts

install-all: install mescaline-database mescaline mescaline-tools mescaline-app mescaline-sts
.PHONY: install-all

clean-all: clean mescaline-database mescaline mescaline-tools mescaline-app mescaline-sts
.PHONY: clean-all

HADDOCK_BASE_DOC = "http://hackage.haskell.org/packages/archive/latest/doc/html"
HADDOCK_ARGS = --hyperlink-source --html-location=$(HADDOCK_BASE_DOC)

haddock:
	$(eval ACTION := haddock)
	$(CABAL) lib/mescaline-database $(HADDOCK_ARGS)
	$(CABAL) lib/mescaline $(HADDOCK_ARGS)
	# rsync -av lib/mescaline/dist/doc n222@null2.net:html/sk/sites/mescaline.globero.es/
.PHONY: haddock

logo:
	inkscape doc/logo/mescaline_layers.svg --export-png=doc/logo/mescaline_layers.png
	makeicns -in doc/logo/mescaline_layers.png -out app/mescaline.icns
.PHONY: logo
