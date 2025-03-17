# Using the Artifact of the Linear-Time–Branching-Time Spectroscope

This is a preliminary artifact for Benjamin Bisping's CAV'23 submission “Process Equivalence Problems as Energy Games”.

## Starting up

The only prerequisites for this walk-through are Docker and Bash.
(Tested on Debian GNU/Linux 11 (bullseye), Docker version 20.10.5+dfsg1, using Intel® Core™ i7-8550U @ 1.80GHz × 8 CPU and 16 GB RAM.)

Load the image and start the container:

```
docker load < equivalence-fiddle-docker.tar.gz
docker run -p 127.0.0.1:8080:8080 --name equivalence-fiddle -d equivalence-fiddle
```

This should present the webtool at: http://127.0.0.1:8080/ (You can also find a current version on https://equiv.io.)

## Using the artifact

You can now run tests like this:

```
docker exec -it equivalence-fiddle sbt shared/test
```

To generate a set of example distinguishing formulas as in https://doi.org/10.46298/lmcs-18(3:19)2022.

```
docker exec -it equivalence-fiddle sbt -warn "shared/run formulas"
```

(We recommend using `-warn` to reduce sbt output noise. If you know your way around sbt, you may want to just enter once using `docker exec -it equivalence-fiddle sbt`.)

Details of the usage are printed by:

```
docker exec -it equivalence-fiddle sbt -warn "shared/run help"
```

## Reproducing results

After starting up the container, the results of Table 1 in the paper can be reproduced by running:

```
bash ./reproduce-results.sh
```

This will only go through the not-so-hard examples, which don't time-out often. It should be enough to see the advantages of the clever energy spectroscopy, in particular, that it works way better on systems with high non-determinism as in the Peterson's mutex example.

An example of what one executions would look like, can be found in `example_reproduce-results.sh.log`.

If you have more time (and RAM), feel free to also run the harder examples (example log in `example_reproduce-results-hard.sh.log`):

```
bash ./reproduce-results-hard.sh
```

(Many will time out and the bigger ones might well run out of heap space, depending on your setup.)

The distinguishing formula of Example 1 can be reproduced by entering the following definition into the tool (at http://127.0.0.1:8080/) and clicking onto the gutter next to `@compare`.

```
Div = tau.Div
S1 = tau.S1 + enA.Div
S = tau.S + tau.Div + enA.Div

@compare S, S1
```

This should lead to the output:

> @compare S, S1
>- 0: Preordered by:
>     simulation
>- 1: Distinguished by:
>     ⟨tau⟩⋀{¬⟨enA⟩⊤} (failure)
>- 2: Equated by:
>     simulation

Also, in the console (Ctrl+Shift+I in Chrome), you can find a Graphviz representation of the underlying game. (Some line beginning like `digraph rel{1017767982 [shape=square, color=red, style=bold, label="S, {S1}\n------\n(2,2,0,0,1,1)\n------\n⟨tau⟩⋀{¬⟨enA⟩}"];....`.)

## Cleaning up

To stop the container and remove the image from your system, enter:

```
docker stop equivalence-fiddle
docker rm equivalence-fiddle
docker image rm equivalence-fiddle
```