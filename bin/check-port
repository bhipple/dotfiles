#!/usr/bin/env bash
PORT=$1
HOST=localhost
if [ $# -eq 2 ]; then
    HOST=$2
fi

echo "Checking $HOST:$PORT"
nc -w 2 -v $HOST $PORT </dev/null; echo $?

# Bash portable one when nc isn't available
#timeout 1 bash -c "</dev/tcp/${HOST}/${PORT}; echo $?"
