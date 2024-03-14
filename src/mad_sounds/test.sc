SynthDef("test", {
 Out.ar(0,
 SinOsc.ar(NamedControl.kr(\freq, [300, 330, 370], [1, 0.3, 0.02])).sum * 0.1
)
})