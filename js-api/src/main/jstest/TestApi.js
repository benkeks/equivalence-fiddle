const assert = require("node:assert/strict");
const eqSpectro = require("../../../target/scala-2.13/eqfiddle-api-fastopt/main.js");

const ltsSpec = {
  "lts": {
    "initialState": "P0",
    "states": {
      "P5": {
        "transitions": [],
        "ccs": "0"
      },
      "P0": {
        "transitions": [
          {
            "label": "a",
            "target": "P1"
          },
          {
            "label": "a",
            "target": "P2"
          }
        ],
        "ccs": "a.(b.(0) + c.(d.(0))) + a.(f.(0) + c.(g.(0)))"
      },
      "P1": {
        "transitions": [
          {
            "label": "b",
            "target": "P5"
          },
          {
            "label": "c",
            "target": "P3"
          }
        ],
        "ccs": "b.(0) + c.(d.(0))"
      },
      "P2": {
        "transitions": [
          {
            "label": "f",
            "target": "P5"
          },
          {
            "label": "c",
            "target": "P4"
          }
        ],
        "ccs": "f.(0) + c.(g.(0))"
      },
      "P3": {
        "transitions": [
          {
            "label": "d",
            "target": "P5"
          }
        ],
        "ccs": "d.(0)"
      },
      "P4": {
        "transitions": [
          {
            "label": "g",
            "target": "P5"
          }
        ],
        "ccs": "g.(0)"
      }
    },
    "_id": "6256a7d5b5d5c91834cb17e2"
  }
};

const lts = eqSpectro.loadLTS(ltsSpec);
const spectroResult = eqSpectro.performSpectroscopy(lts, "P0", "P2");
assert.ok(Array.isArray(spectroResult));
assert.ok(spectroResult.some(pair => pair.left === "P0" && pair.right === "P2"));

const { equiv } = eqSpectro;
const request = { source: "P = a.b + a.c\nQ = a.(b + c)", left: "P", right: "Q" };
const result = equiv.spectroscopy(request);
assert.equal(result.status, "complete");
assert.equal(result.request.semantics, "strong");
assert.equal(equiv.lastResult, result);
assert.equal(result.pairs.length, 2);
const relation = name => result.relations.find(item => item.name === name);
assert.equal(relation("trace").equivalent, true);
assert.equal(relation("failure").equivalent, false);
assert.equal(relation("bisimulation").equivalent, false);
assert.equal(relation("simulation").leftToRight, true);
assert.equal(relation("simulation").rightToLeft, false);
assert.ok(result.pairs.find(pair => pair.left === "P").distinctions.some(item => item.inequivalences.includes("failure")));
assert.deepEqual(JSON.parse(equiv.spectroscopyJSON(JSON.stringify(request))), result);
assert.deepEqual(JSON.parse(JSON.stringify(result)), result);
assert.deepEqual(structuredClone(result), result);
assert.deepEqual(equiv.runFragment(equiv.requestFragment(request)), result);
assert.deepEqual(equiv.runFragment("#/request/" + encodeURIComponent(JSON.stringify(request))), result);
assert.equal(equiv.runFragment("#code=legacy"), undefined);
assert.equal(equiv.runFragment("#request=%").error.code, "INVALID_ENCODING");
assert.equal(equiv.runFragment("#request=not-json").error.code, "INVALID_JSON");

const reversed = equiv.spectroscopy({ ...request, left: "Q", right: "P" });
assert.equal(reversed.relations.find(item => item.name === "simulation").rightToLeft, true);
const identical = equiv.spectroscopy({ ...request, right: "P" });
assert.equal(identical.pairs.length, 1);
assert.equal(identical.relations.find(item => item.name === "bisimulation").equivalent, true);

const silent = { source: "P = tau.a\nQ = a", left: "P", right: "Q" };
assert.equal(equiv.spectroscopy(silent).relations.find(item => item.name === "trace").equivalent, false);
assert.ok(equiv.spectroscopy({ ...silent, semantics: "weak" }).strongestEquivalences.includes("sr-branching-bisimulation"));
const unicode = { source: "P = τ.a\nQ = a", left: "P", right: "Q", semantics: "weak" };
assert.deepEqual(equiv.runFragment(equiv.requestFragment(unicode)), equiv.spectroscopy(unicode));
const communication = equiv.spectroscopy({ source: "P = (a.0 | a!0) \\ {a}\nQ = tau.0", left: "P", right: "Q" });
assert.equal(communication.relations.find(item => item.name === "bisimulation").equivalent, true);

for (const invalid of [null, undefined, [], "text", {}, { ...request, semantics: "other" },
  { ...request, maxStates: 0 }, { ...request, maxStates: 1.5 }, { ...request, maxStates: NaN },
  { ...request, maxStates: "5" }, { ...request, left: "" }]) {
  assert.equal(equiv.spectroscopy(invalid).error.code, "INVALID_REQUEST");
}
assert.equal(JSON.parse(equiv.spectroscopyJSON("{")).error.code, "INVALID_JSON");
assert.equal(equiv.spectroscopy({ ...request, left: "Missing" }).error.code, "UNKNOWN_PROCESS");
assert.equal(equiv.spectroscopy({ ...request, source: "P = (" }).error.code, "PARSE_ERROR");
assert.equal(equiv.spectroscopy({ ...request, source: "P = P\nQ = 0" }).error.code, "INTERPRETATION_ERROR");
assert.equal(equiv.spectroscopy({ ...request, maxStates: 1 }).status, "error");
assert.equal(equiv.capabilities().execution, "synchronous");
const checkRequest = { ...request, notion: "simulation" };
const check = equiv.check(checkRequest);
assert.equal(check.status, "complete");
assert.equal(check.leftToRight, true);
assert.equal(check.rightToLeft, false);
assert.equal(check.equivalent, false);
assert.equal(check.notion, "simulation");
assert.deepEqual(structuredClone(check), check);
assert.deepEqual(equiv.runFragment(equiv.requestFragment({ ...checkRequest, operation: "check" })), check);
assert.deepEqual(JSON.parse(equiv.spectroscopyJSON(JSON.stringify({ ...checkRequest, operation: "check" }))), check);
assert.equal(equiv.check({ ...request, notion: "trace" }).equivalent, true);
assert.equal(equiv.check({ ...request, notion: "bisimulation" }).equivalent, false);
assert.equal(equiv.check({ ...checkRequest, right: "P" }).equivalent, true);
assert.equal(equiv.check({ ...silent, notion: "sr-branching-bisimulation" }).equivalent, true);
assert.equal(equiv.check(request).error.code, "INVALID_REQUEST");
assert.equal(equiv.check({ ...request, notion: "unknown" }).error.code, "UNKNOWN_NOTION");
assert.equal(equiv.check({ ...checkRequest, left: "Missing" }).error.code, "UNKNOWN_PROCESS");
assert.equal(JSON.parse(equiv.spectroscopyJSON(JSON.stringify({ ...request, operation: "unknown" }))).error.code, "INVALID_REQUEST");
console.log("Legacy API and browser spectroscopy assertions passed.");