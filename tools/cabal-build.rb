#!/usr/bin/env ruby

def configure(*args)
  unless system("cabal", "configure", *args)
    raise "`cabal configure` failed"
  end
end

def build1(configuring, args)
  needs_configure = false
  IO.popen("cabal build #{args.join(' ')} 2>&1") do |io|
    io.each do |line|
      puts line
      if !line.grep(/<command line>: cannot satisfy/).empty? \
				|| !line.grep(/Run the 'configure' command first/).empty?
        needs_configure = true
      end
    end
  end
  (configuring && !needs_configure) || ($? == 0)
end

def build(*args)
  unless build1(true, args)
    configure(*args)
    unless build1(false, args)
      raise "`cabal build` failed"
    end
  end
end

def install(*args)
  unless system("cabal", "install", *args)
    raise "`cabal install` failed"
  end
end

def clean(*args)
  unless system("cabal", "clean", *args)
    raise "`cabal clean` failed"
  end
end

begin
  cmd = ARGV.shift
  unless cmd
    raise "missing command"
  end
  dir = ARGV.shift
  unless dir
    raise "missing directory"
  end
  Dir.chdir(dir)
  case cmd
  when "build" then build
  when "clean" then clean
  when "configure" then configure(*ARGV)
  when "install" then install(*ARGV)
  else raise "invalid command: #{cmd}"
  end
rescue
  puts "cabal-build: #{$!}"
end
