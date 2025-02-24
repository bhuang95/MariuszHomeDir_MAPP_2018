#!/bin/ksh

INDIR=../indata/airnow/airnow_2012

cd $INDIR

for file in 2012*gz
do
    tar xvf $file -C ../
done