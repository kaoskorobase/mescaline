#!/bin/sh

rd_packages="hosc hsc3 hsc3-lang"

for x in $rd_packages; do
    if [ ! -d "packages/$x" ]; then
        (
            cd packages && darcs get --lazy http://rd.slavepianos.org/sw/$x
        )
    else
        (
            cd packages/$x && darcs pull
        )
    fi
done

