#!/bin/ksh

NCLDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/ncl
SCRIPTDIR=~/mapp_2018/ncl

MAINDIR=$NCLDIR

DATADIR=${MAINDIR}/indata_obs

start_ident=2016010100
end_ident=2016021812

start_ident=2016021818
end_ident=2016022718

start_ident=2016022800
end_ident=2016022918


cycle_frequency=6

. ~/.nc

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

    aodfile=nnr_aqua.${ident}.nc
    if [[ -r $aodfile ]]
    then
	ln -sf ${aodfile} aodfile_aqua.nc
    fi

    aodfile=nnr_terra.${ident}.nc
    if [[ -r $aodfile ]]
    then
	ln -sf ${aodfile} aodfile_terra.nc
    fi

    cd $SCRIPTDIR

    export TITLE="AOD   ${ident}"
    #the numbers below match contour AOD plots from
    #plotit_globe_aod.sh:
    #    export LIMITLOW=0.1
    #    export LIMITHIGH=1.0
    #    export NCONS=9
    # who knows why
    export LIMITLOW=0.0
    export LIMITHIGH=1.1
    export NCONS=11
    
    ncl plotit_globe_nnr.ncl

    /bin/mv aod.png ${NCLDIR}/pics_obs/nnr_aod_aqua_terra_${ident}.png

    ident=`ndate +${cycle_frequency} ${ident}`

done
