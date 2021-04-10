#!/bin/sh

BASE=$(dirname "$0")
PG_CONTAINER=postgres:9.5.10
PG_PORT=5432

CONTAINER_ID=$(docker ps | grep "$PG_CONTAINER" | awk '{ print $1 }')
if [ -n "$CONTAINER_ID" ]; then
    echo "Found running container :: $PG_CONTAINER -> $CONTAINER_ID"
else
    echo "Starting container :: $PG_CONTAINER"
    docker run -p 5432:$PG_PORT -i "$PG_CONTAINER" &
fi

$BASE/script_console.sh
