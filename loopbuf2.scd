(
SynthDef(\loopbuf, {
	arg buf=0, rate=1, gate=1, startpos=0, startloop=0, endloop=1, interpolation=1;
	Out.ar(0, LoopBuf.ar(2, buf, rate, gate, startpos, startloop, BufFrames.kr(buf) * endloop), interpolation);
}).store;
)