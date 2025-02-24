#!/bin/ksh

BASEDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/FV3_DATA/C192

INDIR=${BASEDIR}/emiss_anthro_data
OUTDIR=${BASEDIR}/emiss_bioburn_data

if [[ ! -r $OUTDIR ]]
then
    mkdir -p $OUTDIR
fi


/bin/cp zero_bioburn.nco $OUTDIR

cd $OUTDIR
/bin/rm -rf ?? *.nc *.tmp


month=1

while [[ $month -le 12 ]]
do

    cmonth=`printf %02i $month`
    echo "month $cmonth"

    itile=1

    mkdir -p ${cmonth}

    while [[ $itile -le 6 ]]
    do
	echo "tile $itile"
	ncap2 -O -v -S zero_bioburn.nco ${INDIR}/${cmonth}/emiss_anthro_${cmonth}.tile${itile}.nc emiss_bioburn_${cmonth}.tile${itile}.nc
	((itile=itile+1))
    done

    /bin/mv emiss_bioburn_${cmonth}.tile?.nc ${cmonth}

    ((month=month+1))

done

/bin/rm -rf *.nc zero_bioburn.nco
