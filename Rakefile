task :default => [:doc]

require 'rubygems'
require 'bluecloth'
require 'find'
require 'fileutils'

DocumentWrapper = %{
<html>
  <head><title>%s</title></head>
  <body>
%s
  </body>
</html>
}

def md_to_html(input, output, title)
  contents = File::readlines(input, nil)
	bc = BlueCloth::new(contents.join)
	puts "#{input} -> #{output} (#{title})"
	File.open(output, "w") { |io| io.puts DocumentWrapper % [title, bc.to_html] }
end

def with_doc_files(dir)
  Find.find(".") { |file|
    if /(.*)\.md$/ =~ file then
      outfile = file == "./README.md" ? $1 + ".html" : $1
      yield(file, outfile)
    end
  }
end

task :doc do
  Find.find(".") { |file|
    if /(.*)\.md$/ =~ file then
      output = file == "./README.md" ? $1 + ".html" : $1
      title = File.basename($1)
      md_to_html(file, output, title)
    end
  }
end

task [:doc, :clean] do
  with_doc_files(".") { |infile,outfile|
    FileUtils.rm(outfile)
  }
end

task [:haddock] do
	base_doc = "http://www.haskell.org/ghc/docs/6.12.2/html/libraries/base-4.2.0.1/"
	system("cabal haddock --hyperlink-source --html-location=#{base_doc}")
	system("rsync -av dist/doc null2:html/sk/sites/mescaline.globero.es/")
end

task [:logo] do
	system("inkscape doc/logo/mescaline_layers.svg --export-png=doc/logo/mescaline_layers.png")
	system("makeicns -in doc/logo/mescaline_layers.png -out app/mescaline.icns")
end

task [:dmg] do
	system("cabal build")

	version = `grep '^version:' mescaline.cabal`.split[1]
	osx_version = `sw_vers`.split("\n").collect { |x| x.split(":\t") }.find { |x| x[0] == "ProductVersion" }[1].sub(/\.[0-9]+$/, "")

	src = "./dist/build/Mescaline.app"
	dst = "Mescaline-#{version}-#{osx_version}.dmg"
	icon = "app/Mescaline.icns"

	volname = "Mescaline-#{version}"
	system("./tools/pkg-dmg --verbosity 0 --source \"#{src}\" --target \"#{dst}\" --sourcefile --volname Mescaline --icon \"#{icon}\" --symlink /Applications:/Applications")
end
