(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("../../lib/codemirror"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
"use strict";

CodeMirror.defineMode("dces", function () {
  return {
    startState: function () {
      return {
        inString: false,
        inParams: false,
        lhs: false,
        stringType: ""
      };
    },
    token: function (stream, state) {
      //check for state changes
      if (!state.inString && (stream.peek() == '"')) {
        state.stringType = stream.peek();
        stream.next(); // Skip quote
        state.inString = true; // Update state
      }
      
      //return state
      if (state.inString) {
        while (state.inString && !stream.eol()) {
          if (stream.peek() === state.stringType) {
            stream.next(); // Skip quote
            state.inString = false; // Clear flag
          } else {
            stream.match(/^.[^\\\"\']*/);
          }
        }
        return "string"; // Token style
      } else if (state.inParams && stream.peek() === ')') {
        stream.next();
        state.lhs = false;
        state.inParams--;
        return 'bracket';
      /*} else if (stream.peek() === '(' && stream.skipTo(')')) {
        stream.next();//skip closing )
        // array of objects has an extra open & close []
        if (stream.peek() === ')') stream.next();
        return "bracket"; */
      } else if (stream.eatSpace()) {
        return null;
      } else if (state.inParams && state.lhs && stream.eatWhile(function (c) { return c != '=' && c != ' ' && c != ')'; })) {
        return "property";
      } else if (state.inParams && state.lhs && stream.peek() === "=") {
        stream.next();
        state.lhs = false;
        return null;
      } else if (stream.match(/@\w+/)) {
    	return 'tag';
      } else if (!state.lhs && (stream.match('-->') || stream.match('->') || stream.match('|-'))) {
        return 'atom-stepto';
      } else if (!state.lhs && stream.peek() === '(') {
      	state.lhs = true;
        state.inParams++;
        stream.next();
        return 'bracket';
      } else if (stream.match(/[\{\}\[\]]/)) {
    	return 'bracket';
      } else if (!state.lhs && stream.match(/([A-z]|_)(\w|_)*/)) {
        return 'variable';
      } else if (!state.lhs && stream.match(/^\-?\d+(?:\.\d+)?/)) {
        return 'number';
      } else if (!stream.eatSpace()) {
        stream.next();
      }
      return null;
    }
  };
});

CodeMirror.defineMIME('text/x-toml', 'toml');

});