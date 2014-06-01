#!/bin/sh
java -jar -Dconfig.file=etc/application.conf -Dorg.clapper.avsl.config=etc/avsl-production.conf bin/atuinssite_2.10-0.1-SNAPSHOT.jar
