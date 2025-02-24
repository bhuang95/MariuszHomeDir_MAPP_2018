#!/bin/ksh

SCRIPTDIR=~/mapp_2018/ncl

NCLDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/ncl

MAINDIR=$NCLDIR

DATADIR=${MAINDIR}/indata_fv3_co

grid=C96

start_ident=2019072900
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
        file_in=${datestring}.fv_co_column.res.tile${itile}.nc
	ln -sf ${file_in} fv_co_column.res.tile${itile}.nc
        ((itile=itile+1))
    done
    
    cd ${SCRIPTDIR}

    export TITLE="CO Column Integral  ${ident}"
#    export LIMITLOW=4500
#    export LIMITHIGH=5500
    export LIMITLOW=0
    export LIMITHIGH=4500
    export NCONS=15
    export SPACING=100
    export SPECIES="co_column"
    export GRID="${grid}"

    species=${SPECIES}

    ncl plotit_globe_${species}_pre_spacing.ncl

    if [[ ! -r  ${NCLDIR}/pics_${species} ]]
    then
	mkdir -p ${NCLDIR}/pics_${species}
    fi

    /bin/mv ${species}.png ${NCLDIR}/pics_${species}/${species}_pre_${ident}.png

    ident=`ndate +${cycle_frequency} ${ident}`

done
