# Linear-Time–Branching-Time Spectroscope

The “Linear-time–Branching-time Spectroscope” is a web app to find all preorders, equivalences and inequivalences from the (strong) linear-time–branching-time spectrum for small processes.

![](doc/usage-illustration.gif)

Just input CCS-style processes, give the two processes you want to compare with `@compare "P1,P2"` and click on the gutter next to the compare-statement!

To explore possible minimizations of the system, write `@minimize` and click on the gutter!

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

To perform benchmarks, run:

```
sbt "shared/run"
```

## Theoretical background

The algorithm uses a generalization of the bisimulation game to find all relevant distinguishing Hennessy–Milner logic formulas for two compared finite-state processes. Using these, we can give a precise characterization of how much distinguishing power is needed to tell two processes apart—and thus also determine the best fit of equivalences to equate them.