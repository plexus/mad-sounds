SynthDef("fm-gong", {
 arg freq, dr = 1.0, amp = 1.0;
 var mod1sig, mod1f = freq * 1.16;
 var mod2sig, mod2f = freq * 3.14;
 var mod3sig, mod3f = freq * 1.005;
 var idx1a = 0.01 * mod1f;
 var idx1b = 0.30 * mod1f;
 var idx1scaler   = idx1b - idx1a;

 var idx2a = 0.01 * mod2f;
 var idx2b = 0.38 * mod2f;
 var idx2scaler   = idx2b - idx2a;

 var idx3a = 0.01 * mod3f;
 var idx3b = 0.50 * mod3f;
 var idx3scaler   = idx3b - idx3a;

 var mod1env, mod2env, mod3env, ampenv;
 var carsig;

 mod1env = EnvGen.kr(
  Env(levels: [0,1,1,0], times: [0.75*dr, 0.24*dr, 0.01*dr]),
  doneAction: 2);

 mod2env = EnvGen.kr(
  Env(levels: [0,1,0], times: [0.02*dr, 0.98*dr]),
  doneAction: 2);

 mod3env = EnvGen.kr(
  Env(levels: [ 0, 0.3, 1, 0.5, 0], times: [0.15 * dr, 0.15 *dr, 0.45*dr, 0.25*dr]),
  doneAction: 2);

 // Exp curve...
 ampenv = EnvGen.kr(
  Env(levels: [ 0, 1, 0.001], times: [0.002, dr - 0.002], curve: \exp),
  doneAction: 2);

 mod1sig = SinOsc.ar(freq: mod1f);
 // envelope the signal, afterwards
 mod1sig = mod1sig * ((idx1a + idx1scaler) * mod1env);

 mod2sig = SinOsc.ar(freq: mod2f);
 // envelope the signal, afterwards
 mod2sig = mod2sig * ((idx2a + idx2scaler) * mod2env);

 mod3sig = SinOsc.ar(freq: mod3f);

 // envelope the signal, afterwards
 mod3sig = mod3sig * ((idx3a + idx3scaler) * mod3env);

 carsig = SinOsc.ar(freq + mod1sig + mod2sig + mod3sig);

 Out.ar(0, carsig * ampenv * amp);

}).store