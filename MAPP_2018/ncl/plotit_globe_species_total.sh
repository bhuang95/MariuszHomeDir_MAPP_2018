#!/bin/ksh

NCLDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/ncl
SCRIPTDIR=~/mapp_2018/ncl


MAINDIR=$NCLDIR

DATADIR=${MAINDIR}/indata_fv3

start_ident=2018041406
end_ident=2018041718

start_ident=2016061506
end_ident=2016061506


ftype="fcst"
#needs to have suffix _ges - not implemented
ftype="anal"

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
	file_in=${datestring}.fv_tracer.res.tile${itile}.nc
	ncap2 -O -S ${SCRIPTDIR}/add_species.nco ${datestring}.fv_tracer.res.tile${itile}.nc ${datestring}.fv_tracer.res.total.tile${itile}.nc
	ln -sf ${datestring}.fv_tracer.res.total.tile${itile}.nc fv_tracer.total.tile${itile}.nc
	
	((itile=itile+1))
    done

    cd ${SCRIPTDIR}

    export TITLE="Sea-salt analysis   ${ident}"
    export LIMITLOW=20
    export LIMITHIGH=300
##    export SPACING=25
    export NCONS=14
    export SPECIES="seas_total"

#    export TITLE="Dust analysis   ${ident}"
#    export LIMITLOW=20
#    export LIMITHIGH=400
##    export SPACING=25
#    export NCONS=19
#    export SPECIES="dust_total"

#    export TITLE="Carbonaceus analysis   ${ident}"
#    export LIMITLOW=0
#    export LIMITHIGH=24
#    export SPACING=2
#    export SPECIES="carbon_total"
    
    species=${SPECIES}

    ncl plotit_globe_species_total.ncl

    /bin/mv tracer.png ${NCLDIR}/pics_${ftype}/${species}_${ftype}_${ident}.png
#    rm -f anal.00000?.png

    ident=`ndate +${cycle_frequency} ${ident}`

done
