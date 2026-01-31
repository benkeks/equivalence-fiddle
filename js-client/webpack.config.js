const path = require("path");
const webpack = require("webpack");

const config = require("./scalajs.webpack.config.js");

const entryKey = Object.keys(config.entry)[0];
const preludePath = path.resolve(__dirname, "webpack-prelude.js");

config.entry[entryKey] = [preludePath];

config.plugins = (config.plugins || []).concat([
  new webpack.ProvidePlugin({
    $: "jquery",
    jQuery: "jquery",
    "window.jQuery": "jquery",
    CodeMirror: "codemirror",
    "window.CodeMirror": "codemirror",
    d3: "d3"
  })
]);

module.exports = config;
