#!/bin/ksh

SCRIPTDIR=~/mapp_2018/ncl

MAINDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/ncl

DATADIR=${MAINDIR}/indata_fv3_aod

start_ident=2016010100
end_ident=2016022018

anal=1

if [[ $anal = 0 ]]
then
    suffix=".ges"
    OUTDIR=${MAINDIR}/fv3/pics_fv3_aod_bckg
    bckg=1
else
    suffix=""
    OUTDIR=${MAINDIR}/fv3/pics_fv3_aod_anal
    bckg=0
fi

if [[ ! -r  ${OUTDIR} ]]
then
    mkdir -p ${OUTDIR}
fi

cycle_frequency=6

ndate=~/bin/ndate
ident=$start_ident

. ~/.nc

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
#	file_in=${datestring}.fv_aod_LUTs_v.viirs-m_npp.res.tile${itile}.nc
	file_in=${datestring}.fv_aod_LUTs_v.modis_aqua.res.tile${itile}.nc${suffix}
	ln -sf ${file_in} fv_aod.tile${itile}.nc
	((itile=itile+1))
    done

    export SPECIES="aod"
    export TITLE="AOD   ${ident}"
    export LIMITLOW=0.0
    export LIMITHIGH=1.0
    export NCONS=10

    cd $SCRIPTDIR

    ncl plotit_globe_aod_fv3.ncl

    /bin/mv aod.png ${OUTDIR}/fv3_${SPECIES}_${ident}.png

    ident=`ndate +${cycle_frequency} ${ident}`

done
