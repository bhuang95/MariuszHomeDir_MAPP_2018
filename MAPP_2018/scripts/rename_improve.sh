#!/bin/ksh

INDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/DATA/OBS/improve

cd $INDIR

for dir in 2016*
do
    cd $dir
    for file in improve.*.nc4
    do
	newfile=`echo $file | sed -e "s/00.nc4/12.nc4/g"`
	echo $newfile
	/bin/mv $file $newfile
    done
    cd ..
done

