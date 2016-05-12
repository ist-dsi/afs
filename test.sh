#!/bin/bash
START=$(date +%s)

cd docker-afs

source start.sh
EXIT_CODE=$?

END=$(date +%s)
echo $(($END-$START)) "seconds"

exit $EXIT_CODE
