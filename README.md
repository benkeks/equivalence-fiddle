# Linear-Time–Branching-Time Spectroscope

The “Linear-time–Branching-time Spectroscope” is a web app to find all preorders, equivalences and inequivalences from the (strong) linear-time–branching-time spectrum for small processes as described in [Bisping, Jansen, Nestmann, arXiv 2022](https://doi.org/10.48550/arXiv.2109.15295).

It runs online on https://concurrency-theory.org/ltbt-spectroscope/ .

![](doc/usage-illustration.gif)

Just input CCS-style processes, give the two processes you want to compare with `@compare "P1,P2"` and click on the gutter next to the compare-statement!

## How to build

The project can be built using `sbt` by.

```
sbt webStage
```

This will download all dependencies and output a website under `web/target/web/stage/index.html` that can be run locally in any modern browser.

In order to test that the algorithm determines the expected (in-)equivaences for the example processes from https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.36.8596, run:

```
sbt "shared/test"
```

To generate distinguishing formulas displayed in Table 2 of the [TACAS 2021 paper](https://doi.org/10.1007/978-3-030-72016-2_1):

```
sbt "shared/run"
```

## Theoretical background

The algorithm uses a generalization of the bisimulation game to find all relevant distinguishing Hennessy–Milner logic formulas for two compared finite-state processes. Using these, we can give a precise characterization of how much distinguishing power is needed to tell two processes apart—and thus also determine the best fit of equivalences to equate them.

## Developed by

The LTBT Spectroscope is developed at [MTV TU Berlin](https://www.mtv.tu-berlin.de) by Benjamin Bisping (benjamin.bisping@tu-berlin.de).

The code is subject to the MIT License to be found in `LICENSE`. The full source can be obtained from <https://concurrency-theory.org/ltbt-spectroscope/code/>.