# Linear-time–Branching-time Spectroscope

This is a tool for finding the best ways of equating / preordering / distinguishing finite process models.

It runs online on https://concurrency-theory.org/ltbt-spectroscope/ .

![](doc/usage-illustration.gif)

Just input CSS-style processes, give the two processes you want to compare with `@compare "P1,P2"` and click on the gutter next to the compare-statement!

## How to build

The project can be built using `sbt` by.

```
sbt webStage
```

This will download all dependencies and output a website under `web/target/web/stage/index.html` that can be run locally in any modern browser.

In order to test that the algorithm determines the expected (in-)equivaences for the example processes from https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.36.8596, run:

```
sbt shared/test
```

## Developed by

The LTBT Spectroscope is developed at [MTV TU Berlin](https://www.mtv.tu-berlin.de) by Benjamin Bisping (benjamin.bisping@tu-berlin.de).

The frontend is derived from our [HDES Tool](http://hdes.mtv.tu-berlin.de/) for higher-order dynamic-causality event structures.