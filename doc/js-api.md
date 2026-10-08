# JavaScript Objects

See [browser API usage](browser-api.md) for entry points. Inputs and outputs use plain JSON-compatible objects.

## Requests

```js
{
  source: "P = a.b + a.c\nQ = a.(b + c)",
  left: "P",
  right: "Q",
  semantics: "strong",
  maxStates: 5000
}
```

- `source`, `left`, `right`: required strings; names must not be empty.
- `semantics`: `"strong"` (default) or `"weak"`, for spectroscopy only.
- `maxStates`: positive integer; defaults to 5000 and limits CCS interpretation, not spectroscopy time or game size.
- `operation`: `"spectroscopy"` (default) or `"check"`, for serialized requests.
- `notion`: required for checks, such as `"trace"`, `"simulation"`, or `"sr-branching-bisimulation"`. Its name determines the spectrum.

CCS syntax and preprocessing match the editor. Coordinates are optional; `@compare` and `@check` declarations in the source do not trigger extra operations.

## Responses

Successful responses contain `apiVersion: 1`, `status: "complete"`, the normalized `request`, and `states` (interpreted state count). For spectroscopy:

- `relations`: every spectrum notion, with `name`, `leftToRight`, `rightToLeft`, and `equivalent` booleans.
- `strongestEquivalences`: names of the strongest classes equating the pair.
- `pairs`: requested ordered pairs with `left`, `right`, strongest `preorderings`, and `distinctions`. Each distinction contains `formula`, observation-coordinate `price`, and classifying `inequivalences`. The formula holds at the pair's left state but not its right.

Checks instead return `notion`, `leftToRight`, `rightToLeft`, `equivalent`, and `relation` (the algorithm's supporting relation as `[source, label, target]` tuples, not necessarily the complete preorder).

Directions denote inclusion of observations: `leftToRight` means every left observation is also a right observation. For simulation, the right simulates the left. Equivalence requires both directions.

## Errors

```js
{ apiVersion: 1, status: "error",
  error: { code: "UNKNOWN_PROCESS", message: "Unknown process: R", line: 0, column: 0 } }
```

Codes are `INVALID_REQUEST`, `INVALID_JSON`, `INVALID_ENCODING`, `PARSE_ERROR`, `INTERPRETATION_ERROR`, `UNKNOWN_PROCESS`, `UNKNOWN_NOTION`, and `COMPUTATION_ERROR`. Source locations are one-based when available, otherwise zero.