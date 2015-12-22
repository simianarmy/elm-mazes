//text output -- it no html or graphical output
var myports = Elm.fullscreen(Elm.Main, {
    startTime: Date.now(),
    openFromTextFile: "",
    openFromPNGFile: {
        width: 0,
        height: 0,
        blackFlags: []
    }
});

function imageDataToMask (png) {
    var index, red, green, blue, alpha;
    var width = png.width,
        height = png.height,
        data = png.decode(),
        mask = [];

    for (var i = 0, n = data.length; i < n; i += 4) {
        red = data[i];
        green = data[i+1];
        blue = data[i+2];
        mask.push(red > 0 || green > 0 || blue > 0);
    }
    return mask;
}

var maskUpload = document.getElementById('maskfileinput');

maskUpload.onchange = function (e) {
    if (maskUpload.files.length == 0) {
        return;
    }
	var reader = new FileReader();
    var filename = maskUpload.files[0].name;

    if (filename.indexOf('.txt') !== -1) {
        reader.onload = function (event) {
            data = event.target.result;	
            //file's text data is sent to 'openfromfile' port
            myports.ports.openFromTextFile.send(data);
        };
        reader.readAsText(maskUpload.files[0]);
    }
    else if (filename.indexOf('.png') !== -1) {
        reader.onload = function (event) {
            var data = new Uint8Array(event.target.result);
            var png = new PNG(data);
            // Convert img data to mask data
            var mask = imageDataToMask(png);
            console.log('mask', mask);

            myports.ports.openFromPNGFile.send({
                width: png.width,
                height: png.height,
                blackFlags: mask
            });
        };
        reader.readAsArrayBuffer(maskUpload.files[0]);
    }
};
	
function logger(x) { console.log(x); }

//data from 'output' port is sent to logger function
//myports.ports.openFromPNGFile.subscribe(logger);

