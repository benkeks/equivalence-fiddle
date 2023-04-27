FROM sbtscala/scala-sbt:eclipse-temurin-jammy-11.0.17_8_1.8.2_2.12.17

RUN apt-get update
RUN apt-get install busybox

WORKDIR /equivalence-fiddle
COPY . .

RUN sbt update
RUN sbt compile
RUN sbt webStage

EXPOSE 8080/tcp
CMD busybox httpd -f -p 8080 -h web/target/web/stage/

ENTRYPOINT []