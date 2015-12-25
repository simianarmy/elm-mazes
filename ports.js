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

/**
 * Converts image PNG data to array flags for an image mask
 * where every black or transparent pixel is a mask
 * @param {PNG} png
 * @return {Array[Bool]}
 */
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
    var file = maskUpload.files[0];
    var filename = file.name;

    if (filename.indexOf('.txt') !== -1) {
        reader.onload = function (event) {
            var data = event.target.result;
            myports.ports.openFromTextFile.send(data);
        };
        reader.readAsText(file);
    }
    else if (filename.indexOf('.png') !== -1) {
        reader.onload = function (event) {
            var data = new Uint8Array(event.target.result);
            var png = new PNG(data);
            // Convert img data to mask data
            var mask = imageDataToMask(png);

            myports.ports.openFromPNGFile.send({
                width: png.width,
                height: png.height,
                blackFlags: mask
            });
        };
        reader.readAsArrayBuffer(file);
    }
};
	
function logger(x) { console.log(x); }

//data from 'output' port is sent to logger function
//myports.ports.openFromPNGFile.subscribe(logger);

