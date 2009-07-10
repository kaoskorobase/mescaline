// microphone check
{ PinkNoise.ar(mul: 0.2).dup(16) }.play;

// ** bubbles [jmc] **
(
SynthDef(\bubbles, { | lfoFreq=0.3, gate=1 |
	var env = EnvGen.kr(Env.cutoff(2), gate);
	var o = LFSaw.kr([8, 7.23, 6.3, 2.5], 0, 3, 80);
	var f = LFSaw.kr(lfoFreq, 0, 24, o);
	var s = SinOsc.ar(f.midicps, 0, 0.04);
	Out.ar(0, env*CombN.ar(s, 0.2, 0.2, 1).clump(2).sum);
}).play;
)

// ** moto-rev [jmc] **
(
SynthDef(\moto_rev, {
 	var f = SinOsc.kr(0.2, 0, 10, 21);
	var s = LFPulse.ar(f, [0, 0.1], 0.1);
	Out.ar(0, RLPF.ar(s, 100, 0.1).clip2(0.4));
}).play;
)

// ** gendy **
(
{
	Pan2.ar(
		Normalizer.ar(
			RLPF.ar(
				RLPF.ar(
					Gendy3.ar(freq:[230, 419, 546, 789]).clump(2).sum,
					MouseX.kr(10,10000,'exponential'),
					0.05),
				MouseY.kr(10,10000,'exponential'),
				0.05),
			0.9),
		Lag.kr(LFNoise0.kr(1), 0.5))
}.play
)

Pdef(\gendy, Pbind(\instrument, \gendy, \dur, Pseq([1, 1, 0.5]), \note, Pseq([0, 1, 7])));
Pdef(\gendy).play;

// transpose
(Pdef(\gendy) <> (gtranspose: 12)).play;

// sequence
Pseq([Pdef(\gendy), Pdef(\gendy) <> (gtranspose: 12)]).play;

// repeat
Pseq([Pdef(\gendy).repeat(2), (Pdef(\gendy) <> (gtranspose: 12)).repeat(2)]).play;

// parallel composition
Pdef(\gendy2,
	Ppar([
		Pmul(\note, -1, Pdef(\gendy)).repeat(4),
		Pseq([Pdef(\gendy).repeat(2), (Pdef(\gendy) <> (gtranspose: 12)).repeat(2)])
	])
).play;

// stream modification
Pdef(\gendy2).drop(2).play;
Pdef(\gendy2).stutter(2).play;

// wobol
Pdef(\wobol,
	Pmono(\wobol, \dur, 0.125, \freq, Pbrown(10, 5000, 100))
).play;
Pdef(\wobol).clear;

// Pseg
Pmono(\wobol, \dur, 0.025, \freq, 3000*Env.cutoff(8).asPseg+10).play

Pdef(\seq, Pseq([Pdef(\wobol), Pdef(\wobol)])).play;

// dial
Pbind(\instrument, \dial, \dur, 12).play;

// noise
Pdef(\noise,
	Pseq(
		[Pbind(\instrument, \bitredux, \q, Pseq([0.01, 0.05, 0.09]), \dur, Pseq([2])),
		 Pbind(\instrument, \zing, \dur, Pseq([0.5, 1.5]))]
	)
);

Pdef(\noise).repeat(6).play;
Pdef(\swirl, Pbind(\instrument, \swirl, \dur, Pseq([1, 2]), \freq, Pseq([100, 500]))).play;

// parallel composition
Pdef(\noiseSwirl, Ppar([Pdef(\noise), Pdef(\swirl)])).play;

Ptpar([
	0.0, Pdef(\wobol).finDur(4),
	0.75, Pdef(\noiseSwirl)
]).play;

// herbie
(
var scales = #[[0,4,7,9,12],[0,5,7,10],[0,3,6,10]];
var dts = [0.125,0.25,1.25,2.5];
Pdef(\herbie,
	Ppar(dts.collect { |dt,i|
		Pbind(
			\instrument, "wha'?",
			\midinote, Pseq([Pshuf(#[40,44,49,50],12).stutter(2),Pshuf([Pshuf(#[60,65,60],3),Pshuf(#[70,71],2)]).mirror2], inf),
			\ctranspose, Pshuf([Pshuf(scales[0]).mirror,Pseq(scales[1]),Pseq(scales[2])],inf),
			\amp, 3.dbamp * (1-dts.normalizeSum[i]),
			\dur, dt,
			\legato, Pfunc({ [0.75, 1.5].wchoose([0.7, 0.3]) }),
			\rq, Pstep(Pseq({ exprand(0.1,1) }.dup(4)), Pseq(dts)).repeat(inf),
			\pan, Pfunc { rrand(-0.1,0.1) * i }
		)
	})
);
)

Pdef(\herbie).play;
Pdef(\herbie).clear;

// swing
Pdelay.swing(Pdef(\herbie), 0.25, 1/8, 1e-9).play(quant: 1);

// Pfx
Pbus(Pfx(Pdef(\herbie), \distort, \pregain, 100)).play;
Pbus(Pfx(Pdef(\herbie), \tank, \wet, 0.25)).play;

// MEAPSoft
//
// execute this before playing with the MEAPSoft stuff
(
// load a MEAP segmentation, analysis and edit list
//~makeMeap = { | name |
//	var meapData = d +/+ "meap", sounds = d +/+ "sounds";
//	var meap = MEAPSoft.new;
//	meap.loadSEG(meapData +/+ name ++ ".seg");
//	meap.loadFEAT(meapData +/+ name ++ ".feat");
//	meap.loadEDL(meapData +/+ name ++ ".edl");
//	meap.fileNames.do { |x|
//		b.add(x -> Buffer.read(s, sounds +/+ PathName(x.asString).fileName))
//	};
//	meap;
//};
// create a pattern from a MEAP chunk list
~makeMeapPattern = { | chunks, instr, tempo=1 |
	Pbind(
		\instrument, instr,
		\dur, chunks.collect { |x| thisThread.clock.tempo / tempo * x.duration },
		\bufnum, chunks.collect { |x| b[x.fileName] },
		\startTime, chunks.collect { |x| x.startTime },
		\rate0, Pfunc { thisThread.clock.tempo / tempo },
		\length, chunks.collect { |x| tempo / thisThread.clock.tempo * x.duration }
	)
};
// extract MEAP feature values
~meapFeat = { | meap, feat |
	meap.featDict[feat].collect { |x| x.featDict.at(feat) }
};
// select chunks from chunk list according to func applied to feat
~selectMeapFeat = { | meap, feat, func |
	meap.featDict[feat].select { | chunk |
		func.(chunk.featDict[feat])
	};
};
// sort chunk list according to feature
~sortMeapFeat = { | meap, feat, func |
	meap.featDict[feat].sort { |a,b| func.value(a.featDict[feat], b.featDict[feat]) }
};
)

// thelonious monk "black & tan fantasy"
~mMonk = MEAP.readFile(d +/+ "meap" +/+ "1-04 Black & Tan Fantasy.wav.feat", d +/+ "sounds");
TempoClock.default.tempo = 1;
~mMonk.files;
b.putAll(~mMonk.allocBuffers);
(~mMonk.unitsSortedBy(\startTime).collect(_.startTime).dup + [0, ~mMonk.unitsSortedBy(\startTime).collect(_.duration)]).lace.differentiate
Pdef(\monk, ~makeMeapPattern.(Pseq(~mMonk.unitsSortedBy(\startTime)), \unit_2)).play;
Pdef(\monk, ~makeMeapPattern.(Pseq(~mMonk.units.reverse), \unit_2)).play;

(
var tree = ~mMonk.asKDTree;
var pat = ~makeMeapPattern.(Pseq(Array.fill(100, {
	var unit = tree.label;
	tree = tree.nearestToNode[0];
	unit.duration.postln;
	unit
}).postln));
Pdef(\monk, pat, \unit_2).play;
)

(
// select some chord changes and quantize to TempoClock 
Pdef(\monk, ~makeMeapPattern.(Pindex(~mMonk.units,
	Pclutch(Pshuf((21..24), inf), Pseq([0, 1], inf))), \unit_2)
	<> (dur:0.5)
).play(quant: 1);
)

// fx
(
Pbindf(
	Pdef(\monk),
	\wet, Pseq([0, 0, 0.3, 0.5], inf),
	\delayTime, 0.015,
	\decayTime, 0.5,
	\amp, -3.dbamp
).play;
)

Pfx(Pdef(\monk), \bpf, \freq, 500, \bw, 100).play;

// emulate the MEAP palindrome composer
Pmul(\length, 1, Pdef(\monk) <> Pbind(\rate, Pseq([1, -1], inf))).play;

// king tubby "rub a dub"
~mKing = MEAP.readFile(d +/+ "meap" +/+ "19 Rub A Dub.wav.feat", d +/+ "sounds");
b.putAll(~mKing.allocBuffers);
// approximate tempo in bps and bpm (beat estimation found mostly 1/4 notes):
~mKing.segments.collect(_.duration).mean.reciprocal.dup * [1, 60];
~mKing.featDict.keys;

// max deviation
(~mKing.segments.collect(_.duration) absdif: ~mKing.segments.collect(_.duration).mean).maxItem;

// set default quantization to 1/4
Pdef(\king).quant_(1);

(
// play segments (beat-rotated bars)
var meap = ~mKing;
var chunks = meap.segments.clump(4).collect(_.rotate(-2)).flatten;
var rate = Place([1, 1, [1, 4/6, -1], 1], inf);
Pdef(\king,
	Pmul(\length, rate.collect { |x| if (x < 0, 0.5, 1) },
		Pbindf(
			~makeMeapPattern.(Pclutch(Pseq(chunks), Pfunc({ 0.99.coin })), \unit_2, 1.3159888066038),
			\rate, rate,
			\db, Pseq([0, -2, -3, 0], inf),
			\dur, 1,
			\wet, rate.collect { |x| if (x < 0, 1, 0) }))).play(quant: 1);
)

Pdef(\sub,
	Pbind(
		\instrument, \sub,
		\midinote, Pseq([26,30,26,\,\],inf),
		\dur, Pseq([0.5, 2, 0.25, 0.25, 1],inf),
		\gain, Pseq([10, 10, 50, 50,0], inf),
		\suslevel, -12.dbamp)).play(quant: 1);

Pdef(\king).quant_(4)

~clk.tempo = 1.3;

(
// some breakbeat action
var sections = [0,0,40,20,20,150,80,80,0,0,40,150,150];
var meap = ~mKing;
var segs = meap.segments;
//var segs = meap.unitsSortedBy(\AvgSpecCentroid);
//var segs = meap.unitsSortedBy(\AvgPitchSimple);
//TempoClock.default.tempo_(meap.segList[50..].collect(_.segLength).mean.reciprocal.postln);
Pdef(\king,
	Pseq(sections.collect { |i|
		var idx1 = Pseq(i+(8..12)).stutter(2);
		var idx2 = Pseq(i+(20..23)).mirror2;
		var seq1 = ~makeMeapPattern.(Pindex(segs, idx1), \unit_2, 1.3159888066038);
		var seq2 = ~makeMeapPattern.(Pindex(segs, idx2), \unit_2, 1.3159888066038);
		Pbindf(
			(seq1.repeat(2) ++ seq2.repeat(2)),
			\dur, Place([1, [0.5, Pseq(0.25.dup)], 0.5, 0.75, 0.125, 0.125], inf),
			//\length, Pkey(\dur).collect({ |x| thisThread.clock.beatDur * x }),
			\rate, Pwrand([Pseq([1, 2]), Pseq([-1, -2])], [15, 1].normalizeSum, inf),
			\db, -12,
			\fade, 0.005,
			\wet, Pif(Ptime.beatInBar.collect([0.5, 3].includes(_)), 0.4, 0),
			//\wet, Pseq([0.4, 0, 0, -0.4, 0], inf),
			\delayTime, Pseq([0.015, 0.018, Pwrand(#[0, 0.2], #[2, 1].normalizeSum)], inf),
			\pan, Pseq([-0.2, 0, -0.4, 0, 0.2, 0, 0.4, 0], inf))
	}).repeat(inf)).play(quant: 4);
)

// makigami koichi "kuchinoba"
~mMaki = MEAP.readFile(d +/+ "meap" +/+ "00 Unknown Track.wav.feat");
b.putAll(~mMaki.allocBuffers);
~mMaki.featDict.keys;
~mMakiChunks = [
	~selectMeapFeat.(~mMaki, \AvgChunkPower, { |x| x.ampdb.inclusivelyBetween(-60, inf) }),
	~selectMeapFeat.(~mMaki, \AvgChunkPower, { |x| x.ampdb.inclusivelyBetween(-inf, -60) }),
	~sortMeapFeat.(~mMaki, \AvgSpecCentroid, { |a,b| a <= b }),
	~sortMeapFeat.(~mMaki, \AvgSpecCentroid, { |a,b| a >= b })
];

Pdef(\maki).quant = 1;

Pdef(\maki, ~makeMeapPattern.(Pseq(~mMakiChunks[0], inf), \unit_2)).play;
Pdef(\maki, ~makeMeapPattern.(Pseq(~mMakiChunks[1], inf), \unit_2)).play;
Pdef(\maki, ~makeMeapPattern.(Pseq(~mMakiChunks[2], inf), \unit_2)).play;
Pdef(\maki, ~makeMeapPattern.(Pseq(~mMakiChunks[3], inf), \unit_2)).play;

(
Pdef(\maki,
	Ppar({ |i| Pbindf(
		~makeMeapPattern.(Pseq(~mMaki.unitsSortedBy(\duration)[20..40].mirror2.rotate(i*7), inf), \unit_2),
		//\dur, Pwrand([0.25, 0.125, 0.15]*2, [1, 4, 2].normalizeSum, inf),
		\rate, Pseq([1, 1.25], inf)) }.dup(2))
).play(quant: 1);
)

(
Pdef(\maki,
	~makeMeapPattern.(Pshuf(~mMaki.unitsSortedBy(\duration)[20..40]).mirror2.repeat, \unit_2)
	<> Pbind(\dur, Pwrand([0.25, 0.5, 1], [1, 4, 2].normalizeSum, inf),
			\rate, Pseq([1, 1.5], inf))
).play(quant: 1);
)

Pfx(Pdef(\maki), \distort, \pregain, 80).play;
Pfx(Pfx(Pdef(\maki), \distort, \pregain, 40), \echo).play;

// EOF
