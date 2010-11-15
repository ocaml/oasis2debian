#!/bin/sh 

set -e 

EXEC=$1

if [ -z  "$EXEC" ]; then 
  echo exec not set >&2
  exit 1
fi

for i 
$EXEC
