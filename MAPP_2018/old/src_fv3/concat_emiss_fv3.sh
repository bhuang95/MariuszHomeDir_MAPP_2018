#!/bin/ksh

BASEDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/FV3_DATA/C192

EDGARDIR=${BASEDIR}/edgar_anthro_data
SULFDIR=${BASEDIR}/sulf_anthro_data
DMSDIR=${BASEDIR}/dms_data
OUTDIR=${BASEDIR}/emiss_anthro_data

if [[ ! -r $OUTDIR ]]
then
    mkdir -p $OUTDIR
fi

cd $OUTDIR
/bin/rm -rf ?? *.nc

month=1

while [[ $month -le 12 ]]
do

    cmonth=`printf %02i $month`
    echo "month $cmonth"
    /bin/cp ${EDGARDIR}/${cmonth}/emiss_edgar_anthro_${cmonth}.tile?.nc ${DMSDIR}/${cmonth}/emiss_dms_${cmonth}.tile?.nc ${SULFDIR}/emiss_sulf_anthro_01.tile?.nc .
    itile=1

    mkdir -p ${cmonth}

    while [[ $itile -le 6 ]]
    do
	echo "tile $itile"
	ncks -A emiss_sulf_anthro_01.tile${itile}.nc emiss_dms_${cmonth}.tile${itile}.nc
	ncks -A emiss_dms_${cmonth}.tile${itile}.nc emiss_edgar_anthro_${cmonth}.tile${itile}.nc
	/bin/mv emiss_edgar_anthro_${cmonth}.tile${itile}.nc ${cmonth}/emiss_anthro_${cmonth}.tile${itile}.nc
	((itile=itile+1))
    done

    ((month=month+1))

done

/bin/rm -rf *.nc
