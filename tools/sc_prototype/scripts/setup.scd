(
// ====================================================
// set 'd' to the workshop folder containing this file
// execute the contents of this file
d = "~/scwork/Workshops/ESMUC 2008".standardizePath;
// ====================================================


// server setup
//s.options.device = "Built-in Audio";
s.boot;

// buffers
b = Dictionary.new;

// tempo clock setup
~clk = TempoClock.default;
//             3 is a magic number
~clk.tempo = 1.3159888066038;

// load synthdefs
(d +/+ "synthdefs.scd").loadPath;

// override the braindead default
Event.parentEvents[\default][\db] = 0;
)

// EOF
