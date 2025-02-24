#!/bin/ksh

NCLDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/ncl
SCRIPTDIR=~/mapp_2018/ncl


MAINDIR=$NCLDIR

DATADIR=${MAINDIR}/indata_singleob

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

    indir=${DATADIR}
    datestring=${year}${month}${day}.${hour}0000
    ntiles=6
    
    cd ${indir}

    itile=1
    while [[ $itile -le $ntiles ]]
    do 
	file_fcst=${datestring}.fv_tracer.res.tile${itile}.nc
#enkf
	file_anal=${datestring}.anal.fv_tracer.res.tile${itile}.nc
#hybrid
#	file_anal=${datestring}.hyb-3dvar-gfs_aero.fv_tracer.res.tile${itile}.nc
	ncdiff -O $file_anal $file_fcst ${datestring}.fv_tracer.incr.tile${itile}.nc
	ncap2 -O -S ${SCRIPTDIR}/add_species_singleob.nco ${datestring}.fv_tracer.incr.tile${itile}.nc ${datestring}.fv_tracer.incr.total.tile${itile}.nc
	ln -sf ${datestring}.fv_tracer.incr.total.tile${itile}.nc fv_tracer.incr.total.tile${itile}.nc
	
	((itile=itile+1))

    done

    cd ${SCRIPTDIR}

    export TITLE="Sea-salt increment   ${ident}"
    export LIMITLOW=-50
    export LIMITHIGH=100
    export SPACING=10
    export SPECIES="seas_total"

    export TITLE="Dust increment"
    export LIMITLOW=0
    export LIMITHIGH=100
    export SPACING=10
    export SPECIES="dust_total"

    export TITLE="Carbonaceus increment   ${ident}"
    export LIMITLOW=-10
    export LIMITHIGH=10
    export SPACING=2
    export SPECIES="carbon_total"

    species=${SPECIES}

    ncl plotit_globe_increment_total_singleob.ncl

    /bin/mv incr.png ${NCLDIR}/pics_incr/${species}_incr_singleob_${ident}.png
#    rm -f incr.00000?.png

    exit

    ident=`ndate +${cycle_frequency} ${ident}`

done
