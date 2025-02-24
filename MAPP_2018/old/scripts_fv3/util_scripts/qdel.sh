#!/bin/ksh

jobstart=29046925
jobend=29047142

job=$jobstart

while [[ job -le $jobend ]]
do
    qdel $job
    ((job=job+1))
done
