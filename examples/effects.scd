d = Platform.case(
	\linux,	"~/.Mescaline/",
	\osx,	"~/Library/Application Support/Mescaline/synthdefs/",
	\windows,	"APPDATA".getenv,
	"").standardizePath;

SynthDef(\echo, { arg out=0, sendBus=0, maxdtime=0.2, dtime=0.2, decay=2, gate=1;
	var env, in;
	env = Linen.kr(gate, 0.05, 1, 0.1, 2);
	in = In.ar(sendBus, 2);
	Out.ar(out, CombL.ar(in * env, maxdtime, dtime, decay, 1));
}, [\ir, \ir, 0.1, 0.1, 0]).writeDefFile(d);

// Courtesy James McCartney
SynthDef("tank", { | out=0, sendBus=0, gate=1, wet=1 |
	var local, env, in;
	
	env = Linen.kr(gate, 0.05, 1, 0.4, 2);
	in = In.ar(sendBus, 2);
		
	4.do { in = AllpassN.ar(in, 0.03, {Rand(0.005,0.02)}.dup, 1); };
	
	local = LocalIn.ar(2) * 0.98;
	local = OnePole.ar(local, 0.5);
	
	local = Rotate2.ar(local[0], local[1], 0.23);
	local = AllpassN.ar(local, 0.05, {Rand(0.01,0.05)}.dup, 2);
	 			
	local = DelayN.ar(local, 0.3, [0.19,0.26]);
	local = AllpassN.ar(local, 0.05, {Rand(0.03,0.15)}.dup, 2);
	
	local = LeakDC.ar(local);	
	local = local + in;
		
	LocalOut.ar(local);
	
	Out.ar(out, wet*env*local);
}).writeDefFile(d);
