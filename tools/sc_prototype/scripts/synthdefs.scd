// various synthdefs
// attributions:
//   jmc - james mccartney
//   jrb - julian rohrhuber
//   rd  - rohan drape
//   sk  - stefan kersten
// ** wobol [rd] **
SynthDef(\wobol, { | out=0, freq=10, gate=1 |
var env = EnvGen.kr(Env.cutoff, gate, doneAction: 2);
Out.ar(out, env * Pan2.ar(
	Normalizer.ar(
	RLPF.ar(
	RLPF.ar(Gendy2.ar(a:SinOsc.kr(0.4,0,0.05,0.05),c:SinOsc.kr(0.3,0,0.1,0.5)),
	freq,0.05),
	Fold.kr(freq+1000, 10, 10000),0.05)
	, 0.9)
,Lag.kr(LFNoise0.kr(1),0.5)))
}).store;
// ** bitredux [jrb] **
SynthDef(\bitredux, { | out=0, q=0, gate=1 |
var env = EnvGen.kr(Env.asr(0.01, 1, 0.2), gate, doneAction: 2);
var f = { LFNoise2.kr(8) }.dup;
var nh = { LFNoise2.kr(3) }.dup;
var src = Blip.ar(f * 200 + 300, nh * 10 + 20);
var sr = Lag.kr(LinExp.kr(q, 0, 1, 1000, SampleRate.ir * 0.1), 0.25);
Out.ar(out, Latch.ar(src, Impulse.ar(sr, 0)));
}).store;
// ** gendy [rd] **
SynthDef(\gendy, { | out=0, freq=100, gate=1 |
	var env = EnvGen.ar(Env.adsr(0.01, 0.05, -12.dbamp, 0.25), gate, levelScale: 3.dbamp, doneAction: 2);
	Out.ar(out, Pan2.ar(
		Normalizer.ar(
			RLPF.ar(
				RLPF.ar(
					Gendy3.ar(freq:[230, 419, 546, 789]).clump(2).sum,
					XLine.kr(freq*1.5, freq, 0.1),
					0.05),
				XLine.kr(freq*0.5, freq, 0.1),
				0.05),
			0.9),
		Lag.kr(LFNoise0.kr(1), 0.5),
		env))
}).store;
// ** bell [rd] **
SynthDef(\bell, { | freq=100 |
var p = 15;
var f1 = freq;
var f2 = Rand(f1, f1*0.7.midiratio);
var y = { var f = Array.fill(p, { f1 + f2.rand } )
          ; var rt = Array.fill(p, { 1.0 + 4.0.rand })
          ; `[f, nil, rt] };
var z = Array.fill(2, y);
var t = Impulse.ar(0);
var n = WhiteNoise.ar(0.03);
z = Klank.ar(z, Decay.ar(t, 0.004, n));
DetectSilence.ar(z, doneAction: 2);
Out.ar(0, z);
}, [nil, \tr]).store;
// ** dial [jrb] **
SynthDef(\dial, #{ | out=0, gate=1 |
	var mfv = [[697, 770, 852, 941], [1209, 1336, 1477, 1633]];
	var env = EnvGen.kr(Env.cutoff, gate, doneAction: 2);
	var numbers = [[3, 1]] ++ {: [a, b], a <- (0..2), b <- (0..2) }.all;
	var n = Dwhite(7, 12, inf);
	var w = Dwhite(2, 7, 1);
	var b = Dbrown(0.1, 0.2, 0.01, n);
	var rate = Dseq([w, b], inf);
	var q = Dseq((1..10), inf);
	var trig = Trig.kr(TDuty.kr(rate, 0, q), 0.09);
	var pat = Latch.kr(trig, trig);
	var x = MouseX.kr(0, 1, 'linear', 0.2);
	var h = Hasher.kr(pat * x);
	var which = h.range(0, numbers.size).trunc.(1);
	var both = Select.kr(which, numbers);
	var dial = Select.kr(both, mfv.flop);
	var sig = SinOsc.ar(dial, 0) * 0.05 * trig;
	var d = LFDNoise3.kr(0.5);
	var dsig = DelayC.ar(sig, 0.2, d.range(0, 0.01));
	var g1 = { GrayNoise.ar }.dup;
	var g2 = { GrayNoise.ar }.dup;
	var hiss = (g1 * 0.01) + HPF.ar(g2 * 0.02, 3000);
	Out.ar(out, dsig+hiss);
}).store;
// ** halflife [jrb] **
SynthDef(\halflife, { | out=0, half = 10 |
  var n_atoms = 1e+5
; var n = max(0, n_atoms - PulseCount.ar(LocalIn.ar(2), 0))
; var activity = Dust.ar(n * 2.log / half)
; LocalOut.ar(activity)
; DetectSilence.ar(activity, 0.0001, 0.01)
; Out.ar(out, activity)
}).store;
// ** crawk [rd] **
SynthDef(\crawk, { | out=0 |
  var amp = 1
; var pan = 0
; var variation = 0.9
; var n = Rand(7, 46)
; var dt1 = 25.0 + Rand(-1.7, 1.7)
; var dt2 = (dt1 + LFNoise2.kr(2).dup) * variation * 0.001
; var freq = 901 + Rand(0, 65)
; var t = Impulse.ar(dt2.reciprocal, 0, 100)
; var count = PulseCount.ar(t, 0)
; var mul = count < n
; var u1 = BPF.ar(mul * t, freq, 1) * 0.1
; var freq2 = freq * ((count % LFNoise1.kr(1).range(2, 20)) + 1)
; var u2 = BPF.ar(u1, freq2, 1) * 0.2
; DetectSilence.ar(u2, 0.0001, 0.2, 2)
; Out.ar(out, u2 * amp * 10) }).store;
// ** wha'? [sk] **
SynthDef("wha'?", { | out=0, amp=1, freq=440, dur=1, rq=1, pan=0 |
	var aenv, fenv, att, rel;
	att = 0.01;
	rel = (dur - att).max(0.0001);
	aenv = EnvGen.ar(Env.perc(att, rel, amp/3), doneAction: 2);
	fenv = EnvGen.ar(Env.perc(att, rel), levelScale: freq * Rand(1, 20));
	Out.ar(out, Pan2.ar(
		Mix(
			BPF.ar(
				Saw.ar(freq * [1, 1], mul: aenv),
				//freq * Rand(3, 10),
				fenv,
				rq
			)
		),
		pan)
	);
}).store;
// ** swirl [rd] **
SynthDef(\swirl, { | rate=1, freq=1000, gate=1|
  var env = EnvGen.kr(Env.cutoff(0.5), gate, doneAction: 2)
; var r = LinLin.kr(rate, 0, 1, 1, 20)
; var t = r.reciprocal
; var c = Impulse.kr(r, 0.4)
; var cf = freq
; var f = Latch.kr(WhiteNoise.kr(cf * 0.5, cf), c)
; var p = Latch.kr(WhiteNoise.kr, c)
; var i = Pan2.ar(SinOsc.ar(f, 0, Decay2.kr(c, 0.1 * t, 0.9 * t)), p, 1)
; Out.ar(0, env*CombN.ar(i, 0.3, 0.3, 2))
}).store;
// ** zing [rd] **
SynthDef(\zing, { | gate=1 |
  var env = EnvGen.kr(Env.asr(0.2, 1, 0.2), gate, doneAction: 2)
; var n = 6
; var base = ExpRand(60, 4000)
; var range = Rand(500.0, 8000.0)
; var n0 = BrownNoise.ar([0.007, 0.007])
; var r0 = exprand(0.125, 0.5)
; var n1 = 1
; var exc = n0 * max(0, n1)
; var f = Array.fill(n, { linrand(range) + base })
; var dt = Array.fill(n, { rrand(0.1, 2.0) })
; var s = Klank.ar(`[f, nil, dt], exc)
; Out.ar(0, (Normalizer.ar(s, 0.1) * env).softclip)
}).store;
// ** unit [sk] **
(1..2).do { |nc|
	SynthDef("unit_%".format(nc), { arg out=0, bufnum=0, rate0=1, rate=1, amp=1, fade=0, pan=0, wet=0, delayTime=0.01, decayTime=0.2, startTime, length;
		//var env = EnvGen.kr(Env([0,1,1,0],[0.01,length-0.02,0.01]),doneAction:2);
		var env = EnvGen.kr(Env.linen(fade, length-fade-fade, fade), doneAction: 2);
		var pbuf = PlayBuf.ar(nc, bufnum, BufRateScale.kr(bufnum)*rate0*rate, 1, startTime*BufSampleRate.kr(bufnum)) * amp;
		//var z = env * (((1-wet)*pbuf) + (wet*CombC.ar(pbuf, 0.2, delayTime, decayTime)));
		var z = XFade2.ar(pbuf, CombC.ar(pbuf, 0.2, delayTime, decayTime), wet*2-1);
		var y = [{ Pan2.ar(z, pan) }, { Balance2.ar(z[0], z[1], pan) }].at(nc-1).value * env;
//		var freq = 1000 + (Amplitude.ar(z, 0.01, 0.1) * 8000);
//		y = ((1-wet) * y) + (wet*EWLP6.ar(y, freq, res));
		Out.ar(out, y);
	}, [nil, nil, nil, nil, nil, 0.25]).store;
};
SynthDef(\sub, { | out=0, freq=20, gate=1, amp=1, gain=0.5, suslevel=0.5 |
	var env = EnvGen.kr(Env.adsr(0.01, 0.1, suslevel, 0.1), gate, doneAction: 2);
	var y = tanh(SinOsc.ar(freq, mul: env*gain)) * amp;
	Out.ar(out, y.dup);
}).store;

// ===================================================================================
// FX

// ** echo **
SynthDef(\echo, { arg out=0, maxdtime=0.2, dtime=0.2, decay=2, gate=1;
	var env, in;
	env = Linen.kr(gate, 0.05, 1, 0.1, 2);
	in = In.ar(out, 2);
	XOut.ar(out, env, CombL.ar(in * env, maxdtime, dtime, decay, 1, in));
}, [\ir, \ir, 0.1, 0.1, 0]).store;
// ** distort **
SynthDef(\distort, { arg out=0, pregain=40, amp=0.2, gate=1;
	var env;
	env = Linen.kr(gate, 0.05, 1, 0.1, 2);
	XOut.ar(out, env, (In.ar(out, 2) * pregain).tanh * amp);
}, [\ir, 0.1, 0.1, 0]).store;
// ** wah **
SynthDef(\wah, { arg out=0, gate=1, res=0.3;
	var env, in;
	env = Linen.kr(gate, 0.05, 1, 0.4, 2);
	in = In.ar(out, 2);
	XOut.ar(out, env, EWLP6.ar(in, LinExp.kr(LFNoise1.kr(0.3), -1, 1, 200, 8000), res).softclip * 0.8);
}, [\ir, 0]).store;
// ** bpf **
SynthDef(\bpf, { arg out=0, gate=1, freq=0, bw=100;
	var env, in;
	env = Linen.kr(gate, 0.05, 1, 0.4, 2);
	in = In.ar(out, 2);
	XOut.ar(out, env, BPF.ar(in, freq, bw/freq));
}, [\ir, 0]).store;
// ** scale **
SynthDef(\scale, { arg out=0, gate=1, scale=1;
	var env, in;
	env = Linen.kr(gate, 0.05, 1, 0.4, 2);
	in = In.ar(out, 2);
	XOut.ar(out, env, in * scale);
}, [\ir, 0]).store;
// ** tank [jmc] **
SynthDef("tank", { | out=0, gate=1, wet=0 |
	var local, env, in;
	
	env = Linen.kr(gate, 0.05, 1, 0.4, 2);
	in = In.ar(out, 2);
		
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
	
	XOut.ar(out, wet*env, local);
}).store;
