SC_DIR = $(HOME)/dev/supercollider
ACTION := build
ARGS :=
CABAL_CMD := cabal
CABAL_CMD_ARGS :=
# CABAL_CMD := cabal-dev
# CABAL_CMD_ARGS := -s $(PWD)/cabal-dev $(ARGS)
CABAL = $(PWD)/tools/cabal-build.rb $(CABAL_CMD) $(ACTION) $(CABAL_CMD_ARGS) $(ARGS)

all:
	@echo Please specify a target.

add-source:
	cabal-dev add-source ~/dev/haskell/{hosc,hsc3,hsc3-process,hsc3-server}
	cabal-dev add-source upstream/persistent{,/backends/sqlite,/packages/template}

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
	cd upstream/persistent 			 $(CABAL)
	cd upstream/persistent/backends/sqlite   $(CABAL)
	cd upstream/persistent/packages/template $(CABAL)
.PHONY: upstream upstream-cabal-macosx upstream-persistent

# mescaline-database
MESCALINE_DATABASE_ENTITY = lib/mescaline-database/Mescaline/Database/Entity.hs
MESCALINE_DATABASE_DEPENDS = $(MESCALINE_DATABASE_ENTITY)
$(MESCALINE_DATABASE_ENTITY): $(MESCALINE_DATABASE_ENTITY:.hs=Gen.hs)
	runhaskell $? > $@
mescaline-database: $(MESCALINE_DATABASE_DEPENDS)
	cd lib/mescaline-database && $(CABAL)
.PHONY: mescaline-database

# mescaline
MESCALINE_CONFIGURE_ARGS = \
	--extra-include-dir=$(SC_DIR)/include/common \
	--extra-include-dir=$(SC_DIR)/include/plugin_interface \

mescaline:
	cd lib/mescaline && $(CABAL) $(MESCALINE_CONFIGURE_ARGS)
.PHONY: mescaline

mescaline-tools:
	cd tools && $(CABAL)
.PHONY: mescaline-tools

mescaline-app:
	cd app && $(CABAL)
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
	cd $(MESCALINE_STS) && $(CABAL)
.PHONY: mescaline-sts

install-all: install mescaline-database mescaline mescaline-tools mescaline-app mescaline-sts
.PHONY: install-all

clean-all: clean mescaline-database mescaline mescaline-tools mescaline-app mescaline-sts
.PHONY: clean-all

HADDOCK_BASE_DOC = "http://hackage.haskell.org/packages/archive/latest/doc/html"
HADDOCK_ARGS = --hyperlink-source --html-location=$(HADDOCK_BASE_DOC)

haddock:
	$(eval ACTION := haddock)
	cd lib/mescaline-database && $(CABAL) $(HADDOCK_ARGS)
	cd lib/mescaline 	  && $(CABAL) $(HADDOCK_ARGS)
	# rsync -av lib/mescaline/dist/doc n222@null2.net:html/sk/sites/mescaline.globero.es/
.PHONY: haddock

test:
	runhaskell -ilib/mescaline -ilib/mescaline/tests lib/mescaline/runtests.hs
	runhaskell -DUSE_ANALYSIS=1 -ilib/mescaline-database lib/mescaline-database/runtests.hs

logo:
	inkscape doc/logo/mescaline_layers.svg --export-png=doc/logo/mescaline_layers.png
	makeicns -in doc/logo/mescaline_layers.png -out app/mescaline.icns
.PHONY: logo
