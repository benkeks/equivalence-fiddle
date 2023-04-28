# Using the Artifact of the Linear-Time–Branching-Time Spectroscope

This is a preliminary artifact for Benjamin Bisping's CAV'23 submission “Process Equivalence Problems as Energy Games”.

## Starting up

The only prerequisites for this walk-through are Docker (tested on Docker version 20.10.5+dfsg1, build 55c4c88) and Bash.

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

This will only go through the not-so-hard examples, which don't time-out often. It should be enough to see the advantages of the clever energy spectroscopy, in particular, that it works way better on systems with high non-determinism as in the Peterson's mutex example. If you have more time (and RAM), feel free to also run the harder examples:

```
bash ./reproduce-results-hard.sh
```



## Cleaning up

To stop the container and remove the image from your system, enter:

```
docker stop equivalence-fiddle
docker rm equivalence-fiddle
docker image rm equivalence-fiddle
```