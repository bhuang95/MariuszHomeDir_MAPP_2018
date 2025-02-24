#!/bin/ksh

. ~/.nc

NCLDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/ncl
SCRIPTDIR=~/mapp_2018/ncl

MAINDIR=$NCLDIR

DATADIR=${MAINDIR}/indata_obs

start_ident=2021080100
end_ident=2021081212

cycle_frequency=6

ntiles=5

ndate=~/bin/ndate
ident=$start_ident

while [[ ${ident} -le ${end_ident} ]]
do
    year=`echo $ident | cut -c1-4`
    month=`echo $ident | cut -c5-6`
    day=`echo $ident | cut -c7-8`
    hour=`echo $ident | cut -c9-10`

    datestring=${year}${month}${day}${hour}

    echo $ident

    cd ${DATADIR}

    itile=0
    while [[ $itile -le $ntiles ]]
    do
        file_in=aod_viirs_npp_hofx_3dvar_LUTs_${datestring}_000${itile}.nc4
	ncks -O -v MetaData/latitude,MetaData/longitude,ObsValue/aerosol_optical_depth $file_in fv_aod.tile${itile}.nc
        ((itile=itile+1))
    done

    ncrcat -O fv_aod.tile?.nc viirs_aod_snpp.${ident}.nc > /dev/null 2>&1

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
    export LIMITLOW=0.
    export LIMITHIGH=1.1
    export NCONS=11
    
    ncl plotit_globe_viirs_4shobha.ncl

    /bin/mv aod.png ${NCLDIR}/pics_obs/viirs_aod_snpp_${ident}.png

    ident=`ndate +${cycle_frequency} ${ident}`

done
