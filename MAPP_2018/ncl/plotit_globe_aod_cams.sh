#!/bin/ksh

. ~/.nc

SCRIPTDIR=~/mapp_2018/ncl

NCLDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/ncl

MAINDIR=$NCLDIR

DATADIR=${MAINDIR}/indata_aod_ll


start_ident=2016020100
end_ident=2016022918

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
    
    cd ${indir}

    case $hour in

	'00' )
	    export TIME_INDEX=0 ;;
	'06' )
	    export TIME_INDEX=1 ;;
	'12' ) 
	    export TIME_INDEX=2 ;;
	'18' )
	    export TIME_INDEX=3 ;;

    esac

    file_in=cams_aods_${year}${month}${day}_ll.nc
    ln -sf ${file_in} aod_ll.nc

    export SPECIES="aod550"
    export TITLE="AOD   ${ident}"
    export LIMITLOW=0.0
    export LIMITHIGH=1.0
    export NCONS=10
    export LON="longitude"
    export LAT="latitude"


    cd $SCRIPTDIR

    ncl plotit_globe_aod_ll.ncl

    /bin/mv aod.png ${NCLDIR}/pics_aod/cams_aod_${ident}.png

    ident=`ndate +${cycle_frequency} ${ident}`

done
