#!/bin/sh
ARTIFACT_TARGET_PATH=$1

if [ $CLOUDWRAP_ENVIRONMENT = "local" ]
then
  # wait for postgres
  sleep 2

  java -jar $ARTIFACT_TARGET_PATH
else
  /usr/bin/java -jar $ARTIFACT_TARGET_PATH
fi
