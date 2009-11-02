(
var delim = Char.tab; // MEAP 2.0
var path = "/Users/sk/projects/mescaline/repos/meap_test/00 Unknown Track-1.wav";
//var path = "/Users/sk/projects/mescaline/repos/meap_test/1-04 Black & Tan Fantasy.wav";
//var path = "/Users/sk/projects/mescaline/repos/meap_test/00 Unknown Track.wav";

var segs = TabFileReader.read(path++".seg", delimiter: delim, skipBlanks: true).drop(1).collect(_.drop(1));

var synthdef = SynthDef(\grain, { | bufnum=0, start=0, grainDur=0, gate=1, attack=0, release=0 |
	var env = EnvGen.ar(Env.asr(attack, 1, release), gate, doneAction: 2);
	//var env = EnvGen.ar(Env.linen(attack, grainDur, release), doneAction: 2);
	OffsetOut.ar(0, PlayBuf.ar(2, bufnum, startPos: start*BufSampleRate.kr(bufnum))*env);
});

var attack = 0.0025;
var release = 0.003;
var latency = 0.05;

fork {
	var buffer;
	
	s.bootSync;
	synthdef.send(s);
	buffer = Buffer.read(s, path);
	s.sendMsg("/g_new", 0, 1);
	s.sync;
		
	if (\AudioDevice.asClass.notNil) {
		var device = \AudioDevice.asClass.new("Built-in Audio");
		var pos = device.sampleTime - s.sampleStartTime;
		//pos = s.blockCount * s.options.blockSize;
		var sr = 44100.0;
		
		segs.do { |seg|
			var onset = seg[0].asFloat;
			var dur = seg[1].asFloat;
			
			s.sendPosBundle(pos + (latency * sr),
				[ "/s_new", "grain", -1, 1, 1,
				  \bufnum, buffer.bufnum, \start, onset.asFloat, \grainDur, dur,
				  \attack, attack, \release, release
				]);
			pos = pos + (dur * sr);
			dur.wait;
			s.sendPosBundle(pos + (latency * sr), [ "/n_set", -1, "gate", 0 ]);
		};
	}{		
		segs.do { |seg|
			var onset = seg[0].asFloat;
			var dur = seg[1].asFloat;
			
			s.sendBundle(latency,
				[ "/s_new", "grain", -1, 1, 1,
				  \bufnum, buffer.bufnum, \start, onset.asFloat, \grainDur, dur,
				  \attack, attack, \release, release
				]);
			dur.wait;
			s.sendBundle(latency, [ "/n_set", -1, "gate", 0 ]);
		};
	};
};
)
