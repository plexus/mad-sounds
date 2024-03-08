"Building ".post;
thisProcess.argv.postln;

if(thisProcess.argv.size == 0, { "No file provided.".postln; 1.exit; },
{
    try {
        r = this.compileFile(thisProcess.argv[0]);

        if(r.value == nil, 
            { 1.exit; }, 
            { r.value.writeDefFile("target"); 0.exit; });

    } {
        1.exit;
    }
});
