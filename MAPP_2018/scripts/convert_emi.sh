#!/bin/ksh

. /etc/profile

module load ncl

TMPDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/tmpdir/workdir_emi

cd $TMPDIR

/bin/cp ~/mapp_2018/scripts/mknc_emi.ncl .

export GRID="192"

DATADIR="/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/DATA/EMISSIONS/bin/C${GRID}"

OUTDIR="/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/DATA/EMISSIONS/ncdf/C${GRID}/EMI"

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
	
	export PATHEBC="${DATADIR}/${MONTH}/tile${TILE}/e_bc.dat"
	export PATHEOC="${DATADIR}/${MONTH}/tile${TILE}/e_oc.dat"
	export PATHECO="${DATADIR}/${MONTH}/tile${TILE}/e_co.dat"
	export PATHESULF="${DATADIR}/${MONTH}/tile${TILE}/e_sulf.dat"
	export PATHEPM10="${DATADIR}/${MONTH}/tile${TILE}/e_pm_10.dat"
	export PATHEPM25="${DATADIR}/${MONTH}/tile${TILE}/e_pm_25.dat"
	export PATHESO2="${DATADIR}/${MONTH}/tile${TILE}/e_so2.dat"
	export PATHDM0="${DATADIR}/${MONTH}/tile${TILE}/dm0.dat"
	export PATHERO1="${DATADIR}/${MONTH}/tile${TILE}/dm0.dat"
	export PATHERO2="${DATADIR}/${MONTH}/tile${TILE}/dm0.dat"
	export PATHERO3="${DATADIR}/${MONTH}/tile${TILE}/dm0.dat"
#	export PATHERO1="${DATADIR}/${MONTH}/tile${TILE}/erod1.dat"
#	export PATHERO2="${DATADIR}/${MONTH}/tile${TILE}/erod2.dat"
#	export PATHERO3="${DATADIR}/${MONTH}/tile${TILE}/erod3.dat"

	
	ncl mknc_emi.ncl
	echo "month=$MONTH tile=$TILE"

	/bin/mv emi_data.tile.nc ${outdir}/emi_data.tile${itile}.nc

	((itile=itile+1))

    done

    ((imonth=imonth+1))

done
