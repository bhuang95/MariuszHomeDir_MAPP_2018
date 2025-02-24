#!/bin/ksh

NCLDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/ncl
SCRIPTDIR=~/mapp_2018/ncl

MAINDIR=$NCLDIR

DATADIR=${MAINDIR}/indata_obs

start_ident=2020071000
end_ident=2020071100

start_ident=2019080700
end_ident=2019081018

start_ident=2018041500
end_ident=2018041500


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

    cd ${DATADIR}

    aodfile=viirs_aod_snpp.${ident}.nc

    ln -sf ${aodfile} aodfile.nc

    cd $SCRIPTDIR

    export TITLE="AOD   ${ident}"
    #the numbers below match contour AOD plots from
    #plotit_globe_aod.sh:
    #    export LIMITLOW=0.1
    #    export LIMITHIGH=1.0
    #    export NCONS=9
    # who knows why
    export LIMITLOW=0.1
    export LIMITHIGH=1.1
    export NCONS=10
    
    ncl plotit_globe_viirs.ncl

    /bin/mv aod.png ${NCLDIR}/pics_obs/viirs_aod_snpp_${ident}.png

    ident=`ndate +${cycle_frequency} ${ident}`

done
