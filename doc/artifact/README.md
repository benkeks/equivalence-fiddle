# Using the Artifact of the Linear-Time–Branching-Time Spectroscope

## Starting up

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

## Cleaning up

To stop the container and remove the image from your system, enter:

```
docker stop equivalence-fiddle
docker rm equivalence-fiddle
docker image rm equivalence-fiddle
```