#!/bin/ksh

SCRIPTDIR=~/mapp_2018/ncl

NCLDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/ncl

MAINDIR=$NCLDIR

DATADIR=${MAINDIR}/indata_fv3_co

grid=C96

start_ident=2019072806
start_ident=2019080100
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

    cd $indir

    itile=1
    while [[ $itile -le $ntiles ]]
    do
        file_in=${datestring}.fv_tracer.anal.tile${itile}.nc
        ln -sf ${datestring}.fv_tracer.res.tile${itile}.nc fv_tracer.res.tile${itile}.nc
        ln -sf ${datestring}.fv_core.res.tile${itile}.nc fv_core.res.tile${itile}.nc

        ((itile=itile+1))
    done

    
    cd ${SCRIPTDIR}

    export TITLE="CO Column Integral  ${ident}"
#    export LIMITLOW=4500
#    export LIMITHIGH=5500
    export LIMITLOW=0
    export LIMITHIGH=2000
    export NCONS=10
    export SPACING=100
    export SPECIES="co"
    export GRID="${grid}"

    species=${SPECIES}

    ncl plotit_globe_${species}_column.ncl

    /bin/mv ${species}_column.png ${NCLDIR}/pics_${species}/${species}_column_${ident}.png

    ident=`ndate +${cycle_frequency} ${ident}`

done
