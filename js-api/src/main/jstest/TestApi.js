const eqSpectro = require("../../../target/eqfiddle-api");

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
console.log(spectroResult);