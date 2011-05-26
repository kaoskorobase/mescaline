#!/usr/bin/env ruby

class Cabal
  def initialize(tool)
    @cabal = tool
  end

  def cabal(cmd, args)
    unless system(@cabal, cmd, *args)
      raise "`#{@cabal} #{cmd}` failed"
    end
  end

  def configure(*args)
    cabal("configure", args)
  end

  def build1(configuring, args)
    needs_configure = false
    IO.popen("#{@cabal} build #{args.join(' ')} 2>&1") do |io|
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
    cabal("install", args)
  end

  def clean(*args)
    cabal("clean", args)
  end

  def haddock(*args)
    cabal("haddock", args)
  end
end

begin
  tool = ARGV.shift
  unless tool
    raise "missing tool path"
  end
  cabal = Cabal.new(tool)
  cmd = ARGV.shift
  unless cmd
    raise "missing command"
  end
  case cmd
    when "build" then cabal.build
    when "clean" then cabal.clean
    when "configure" then cabal.configure(*ARGV)
    when "haddock" then cabal.haddock(*ARGV)
    when "install" then cabal.install(*ARGV)
    else raise "invalid command: #{cmd}"
  end
rescue
  puts "cabal-build: #{$!}"
end
