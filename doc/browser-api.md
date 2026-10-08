# Browser API

The browser bundle exposes `window.equiv`. Calls run synchronously in Scala.js, without a server. [Request and result objects](js-api.md) are JSON-compatible.

```js
const request = {
  source: "P = a.b + a.c\nQ = a.(b + c)",
  left: "P",
  right: "Q"
};
const result = equiv.spectroscopy(request);
const checked = equiv.check({ ...request, notion: "simulation" });
```

`spectroscopy` compares the whole strong spectrum by default; set `semantics: "weak"` for the weak spectrum. `check` is the equivalent of `@check "notion", P, Q`: it checks both preorder directions for one notion, which selects the strong or weak spectrum automatically.

Other entry points:

- `equiv.capabilities()`: supported operations and check notion names.
- `equiv.lastResult`: the latest response.
- `equiv.spectroscopyJSON(json)`: JSON-string request and response; optional `operation: "check"` selects a notion check instead of spectroscopy.
- `equiv.runFragment(fragment)`: execute an encoded request without changing the page; unrelated fragments return `undefined`.

## URL Requests

```js
const url = new URL(location.href);
url.hash = equiv.requestFragment({
  ...request, operation: "check", notion: "simulation"
});
console.log(url.href);
```

The format is `#request=<URI-encoded JSON>`; `#/request/<URI-encoded JSON>` is also accepted. Opening a request URL or changing its fragment runs the operation, logs the output JSON to the console, and replaces the page body with that JSON text. Read `document.body.textContent` or `equiv.lastResult` to obtain the result.

For example, [this minimal spectroscopy request](https://equiv.io/main/#request=%7B%22source%22%3A%22P%3Da%5CnQ%3Db%22%2C%22left%22%3A%22P%22%2C%22right%22%3A%22Q%22%7D) compares `P=a` with `Q=b` using default strong semantics. It encodes only `{"source":"P=a\nQ=b","left":"P","right":"Q"}`.

## Tests

```sh
sbt "jsApi/fastLinkJS"
node js-api/src/main/jstest/TestApi.js
```