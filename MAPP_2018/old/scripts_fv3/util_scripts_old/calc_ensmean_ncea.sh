#!/bin/ksh

cd /scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/FV3_RUNS/DA_ENKF/2015081006/OUTPUT_FV3/

SRCDIR=/home/Mariusz.Pagowski/codes/src_da_utils

if [[ ! -r ensmean ]]
then
    mkdir -p ensmean
fi


itile=1
while [[ $itile -le 6 ]]
do
    echo $itile
    itime=0
    while [[ $itime -le 8 ]]
    do 
	echo $itime
	ncea -O  -d time,$itime mem*/20150810060000.fv3_history.tile${itile}.nc 20150810060000.fv3_history.tile${itile}_${itime}.nc
	/bin/mv 20150810060000.fv3_history.tile${itile}_${itime}.nc ensmean
	((itime=itime+1))
    done
    ((itile=itile+1))
done
