//text output -- it no html or graphical output
var myports = Elm.fullscreen(Elm.Main, {
    startTime: Date.now(),
    openFromFile: ""
});

var maskUpload = document.getElementById('maskfileinput');

maskUpload.onchange = function (e) {
	reader = new FileReader();

	reader.onload = function (event) {
		data = event.target.result;	
		//file's text data is sent to 'openfromfile' port
		myports.ports.openFromFile.send(data);
    };
    reader.readAsText(maskUpload.files[0]);
};
	
function logger(x) { console.log(x); }

//data from 'output' port is sent to logger function
//myports.ports.output.subscribe(logger);

