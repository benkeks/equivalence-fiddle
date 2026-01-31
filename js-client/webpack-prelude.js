const d3 = require("d3");
const $ = require("jquery");
const CodeMirror = require("codemirror");

window.d3 = d3;
window.jQuery = $;
window.$ = $;
window.CodeMirror = CodeMirror;

require("bootstrap");
require("./cm_transitionsystem_mode.js");

try {
  const app = require("./eqfiddle-client-fastopt.js");
  if (app && app.TransitionSystemFiddle) {
    window.TransitionSystemFiddle = app.TransitionSystemFiddle;
  }
} catch (err) {
  // noop: entry will still run via webpack if needed
}
