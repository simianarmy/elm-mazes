import { Elm } from "./Main";

Elm.Main.embed(document.querySelector("main"));

var app = Elm.Main.fullscreen();

// send current time to initialize rng
app.ports.startTime.send(Date.now());
