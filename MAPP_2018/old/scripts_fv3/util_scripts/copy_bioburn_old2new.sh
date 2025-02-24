#!/bin/ksh

indir=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS_old/fv3GFS/FV3_RUNS/no_DA_cold
outdir=/scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/FV3_DATA/C192/emiss_bioburn_data

start_date=2015080200
end_date=2015090100

ndate=~/bin/ndate

currdate=$start_date

while [[ $currdate -le $end_date ]]
do
    echo $currdate
    indirdate=${indir}/${currdate}
    outdirdate=${outdir}/${currdate}

    if [[ ! -r $outdirdate ]]
    then
	mkdir -p $outdirdate
    fi

    /bin/cp  ${indirdate}/emiss_bioburn_data/* ${outdirdate}
    currdate=`$ndate 24 $currdate`
done
