#!/bin/ksh

cd /scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/FV3_RUNS/DA_ENKF/2015081006/OUTPUT_FV3/

#mkdir ensmean

itile=1
while [[ $itile -le 6 ]]
do
    echo $itile
    ncea -O  mem*/20150810060000.fv3_history2d.tile${itile}.nc 20150810060000.fv3_history2d.tile${itile}.nc
    /bin/mv 20150810060000.fv3_history2d.tile${itile}.nc ensmean
    ((itile=itile+1))
done
