#!/bin/ksh

SCRIPTDIR=~/mapp_2018/ncl

NCLDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/ncl

MAINDIR=$NCLDIR

DATADIR=${MAINDIR}/indata_fv3_nrt

start_ident=2021080100
end_ident=2021081218

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
	file_in=${datestring}.fv_aod_LUTs_v.viirs-m_npp.res.tile${itile}.nc
	ln -sf ${file_in} fv_aod.tile${itile}.nc
	((itile=itile+1))
    done

    export SPECIES="aod"
    export TITLE="AOD   ${ident}"
    export LIMITLOW=0.
    export LIMITHIGH=1.0
    export NCONS=10

    cd $SCRIPTDIR

    ncl plotit_globe_aod_4shobha.ncl

    /bin/mv aod.png ${NCLDIR}/pics_model/${SPECIES}_${ident}.png
#    rm -f incr.00000?.png

    ident=`ndate +${cycle_frequency} ${ident}`

done
