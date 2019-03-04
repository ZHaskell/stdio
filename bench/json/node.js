// use this script like this: node node.js  1000 ./json-data/buffer-builder.json

fs = require('fs')

var count = process.argv[2]
var filenames = process.argv.splice(3)

var start = new Date().getTime()

filenames.map(function(filename){
    for (var n=0; n < parseInt(count); n++){
        var inp = fs.readFileSync(filename)
        r = JSON.parse(inp)
    }
});

end = new Date().getTime()
console.log(count, ' good, ', (end - start)/1000, 's')
