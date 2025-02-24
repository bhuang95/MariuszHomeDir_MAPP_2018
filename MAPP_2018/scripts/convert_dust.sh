#!/bin/ksh

. /etc/profile

module load ncl

TMPDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/tmpdir/workdir_dust

cd $TMPDIR

/bin/cp ~/mapp_2018/scripts/mknc_dust.ncl .

export GRID="192"

DATADIR="/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/DATA/EMISSIONS/GEFS-Aerosol_emissions/fengsha/bsmfv3/C${GRID}"

OUTDIR="/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/DATA/EMISSIONS/ncdf/C${GRID}/DUST"

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
	
	export PATHSAND="${DATADIR}/${MONTH}/tile${TILE}/sand.dat"
	export PATHCLAY="${DATADIR}/${MONTH}/tile${TILE}/clay.dat"
	export PATHRDRAG="${DATADIR}/${MONTH}/tile${TILE}/rdrag.dat"
	export PATHSSM="${DATADIR}/${MONTH}/tile${TILE}/ssm.dat"
	export PATHUTHR="${DATADIR}/${MONTH}/tile${TILE}/uthr.dat"
	
	ncl mknc_dust.ncl
	echo "month=$MONTH tile=$TILE"

	/bin/mv dust_data.tile.nc ${outdir}/dust_data.tile${itile}.nc

	((itile=itile+1))

    done

    ((imonth=imonth+1))

done
