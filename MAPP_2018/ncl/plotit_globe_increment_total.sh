#!/bin/ksh

NCLDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/ncl
SCRIPTDIR=~/mapp_2018/ncl

MAINDIR=$NCLDIR

DATADIR=${MAINDIR}/indata_fv3

start_ident=2016061506
end_ident=2016062300
end_ident=2016061506

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
	file_bkg=${datestring}.fv_tracer.res.tile${itile}.nc.ges
	file_anl=${datestring}.fv_tracer.res.tile${itile}.nc
	file_inc=${datestring}.fv_tracer.incr.tile${itile}.nc
	ncdiff -O $file_anl $file_bkg $file_inc
	ncap2 -O -S ${SCRIPTDIR}/add_species.nco ${datestring}.fv_tracer.incr.tile${itile}.nc ${datestring}.fv_tracer.incr.total.tile${itile}.nc
	ln -sf ${datestring}.fv_tracer.incr.total.tile${itile}.nc fv_tracer.incr.total.tile${itile}.nc
	
	((itile=itile+1))
    done

    cd ${SCRIPTDIR}


    export TITLE="Sea-salt increment   ${ident}"
    export LIMITLOW=-100
    export LIMITHIGH=100
    export NCONS=20
#    export CMAPSTR="BlueWhiteOrangeRed"
#    export SPACING=10
    export SPECIES="seas_total"

    export TITLE="Dust increment   ${ident}"
    export LIMITLOW=-200
    export LIMITHIGH=200
#    export SPACING=20
    export NCONS=20
#    export CMAPSTR="BlueWhiteOrangeRed"
    export SPECIES="dust_total"

#    export TITLE="Carbonaceus increment   ${ident}"
    export LIMITLOW=-10
    export LIMITHIGH=10
#    export SPACING=2
    export NCONS=10
    export SPECIES="carbon_total"

    species=${SPECIES}

    ncl plotit_globe_increment_total.ncl

    /bin/mv incr.png ${NCLDIR}/pics_incr/${species}_incr_${ident}.png
#    rm -f incr.00000?.png

    ident=`ndate +${cycle_frequency} ${ident}`

done
