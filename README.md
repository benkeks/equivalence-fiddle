# Linear-time–Branching-time Spectroscope

This is a tool for finding the best ways of distinguishing finite process models.

It runs online on https://concurrency-theory.org/ltbt-spectroscope/ .

## How to build

The project can be built using `sbt` by.

```
sbt webStage
```

This will download all dependencies and output a website under `web/target/web/stage/index.html` that can be run locally in any modern browser.

## Developed by

The LTBT Spectroscope is developed at [MTV TU Berlin](https://www.mtv.tu-berlin.de) by Benjamin Bisping (benjamin.bisping@tu-berlin.de).

The frontend is derived from our [HDES Tool](http://hdes.mtv.tu-berlin.de/) for higher-order dynamic-causality event structures.