SynthDef(\tick, {
	var sig, env;
	
	env = \env.kr(Env.perc().asArray); 		// Make a synth parameter out of an Env array....
	env = EnvGen.kr(env, gate: \gate.kr(1));	// Make sure to EnvGen it.
	sig = WhiteNoise.ar;
	
	Out.ar(
		\out.kr(0),
		sig * env * [1, 1]
	);
})