#!/bin/ksh

NCLDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/ncl
SCRIPTDIR=~/mapp_2018/ncl


MAINDIR=$NCLDIR

DATADIR=${MAINDIR}/indata_fv3_co

start_ident=2019081000
end_ident=2019081000


cycle_frequency=6

ndate=~/bin/ndate
ident=$start_ident

while [[ ${ident} -le ${end_ident} ]]
do
    year=`echo $ident | cut -c1-4`
    month=`echo $ident | cut -c5-6`
    day=`echo $ident | cut -c7-8`
    hour=`echo $ident | cut -c9-10`

    echo $ident

    indir=${DATADIR}
    datestring=${year}${month}${day}.${hour}0000
    ntiles=6
    
    cd ${indir}

    itile=1
    while [[ $itile -le $ntiles ]]
    do 
	file_in=${datestring}.fv_aod.res.tile${itile}.nc
	ln -sf ${file_in} fv_aod.tile${itile}.nc
	((itile=itile+1))
    done

    cd ${SCRIPTDIR}

    species="aod"
    export TITLE="AOD   ${ident}"
    export LIMITLOW=0.025
    export LIMITHIGH=0.2
    export SPACING=0.025

    ncl plotit_globe_aod_firex.ncl

    /bin/mv aod.png ${NCLDIR}/pics_co/${species}_${ident}.png

    exit
    ident=`ndate +${cycle_frequency} ${ident}`

done
