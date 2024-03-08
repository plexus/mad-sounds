(
SynthDef(\sinosc3, {
        arg freq = 440, mul = 0.1;
	Out.ar(0, SinOsc.ar(freq, 0.0, mul));
}).store;
)