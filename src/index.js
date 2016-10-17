'use strict';

require("./style.css");
require("./index.html");

var Elm = require("./Main.elm");
var mountNode = document.getElementById("app");
var app = Elm.Main.embed(mountNode);
