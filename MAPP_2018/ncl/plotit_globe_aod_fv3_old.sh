#!/bin/ksh

SCRIPTDIR=~/mapp_2018/ncl

NCLDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/ncl

MAINDIR=$NCLDIR

DATADIR=${MAINDIR}/indata_fv3_aod

start_ident=2016061506
end_ident=2016062318

start_ident=2019080700
end_ident=2019080918

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
#	file_in=${datestring}.fv_aod_v.modis_aqua.res.tile${itile}.nc_anl
	ln -sf ${file_in} fv_aod.tile${itile}.nc
	((itile=itile+1))
    done

    export SPECIES="aod"
    export TITLE="AOD   ${ident}"
    export LIMITLOW=0.1
    export LIMITHIGH=1.0
    export NCONS=9

    cd $SCRIPTDIR

    ncl plotit_globe_aod.ncl


    /bin/mv aod.png ${NCLDIR}/pics_aod/fv3_aod_${ident}.png
#    rm -f incr.00000?.png


    ident=`ndate +${cycle_frequency} ${ident}`

done
