'use strict';

require("./styles.scss");
const { Elm } = require('./Main');
const bytes = localStorage.getItem("bytes")

console.log(bytes);
var app = Elm.Main.init({ flags: bytes ? bytes.split(',').map(s=>parseInt(s)) : null});

app.ports.genRandomBytes.subscribe(size => {
    const buffer = new Uint8Array(size);
    crypto.getRandomValues(buffer);
    const bytes = Array.from(buffer);
    console.log(bytes);
    localStorage.setItem("bytes", bytes);
    app.ports.randomBytes.send(bytes);
})

