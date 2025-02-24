#!/bin/ksh

INDIR=../indata/airnow/2013

cd $INDIR

for file in 2013*zip
do
    unzip $file
done

/bin/mv *.dat ..
