#!/bin/ksh

. /etc/profile

module load ncl

TMPDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/tmpdir/workdir_emi

cd $TMPDIR

/bin/cp ~/mapp_2018/scripts/mknc_bckg.ncl .

export GRID="192"
export LEV="64"

DATADIR="/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/DATA/EMISSIONS/bin/C${GRID}"

OUTDIR="/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/DATA/EMISSIONS/ncdf/C${GRID}/BCKG"

imonth=1

while [[ $imonth -le 12 ]]
do

    export MONTH=`printf %02i $imonth`

    outdir=${OUTDIR}/${MONTH}

    if [[ ! -r ${outdir} ]] 
    then
	mkdir -p ${outdir}
    fi

    itile=1

    while [[ $itile -le 6 ]]
    do

	export TILE="$itile"
	
	export PATHLON="/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/DATA/C${GRID}_fix/lon/lon_tile${TILE}.dat"
	export PATHLAT="/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/DATA/C${GRID}_fix/lat/lat_tile${TILE}.dat"
	export PATHLEV="/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/DATA/C${GRID}_fix/lev/z.dat"
	
	export PATHH2O2="${DATADIR}/${MONTH}/tile${TILE}/h2o2.dat"
	export PATHNO3="${DATADIR}/${MONTH}/tile${TILE}/no3.dat"
	export PATHOH="${DATADIR}/${MONTH}/tile${TILE}/oh.dat"
	
	ncl mknc_bckg.ncl
	echo "month=$MONTH tile=$TILE"

	/bin/mv emi2_data.tile.nc ${outdir}/emi2_data.tile${itile}.nc

	((itile=itile+1))

    done

    ((imonth=imonth+1))

done
