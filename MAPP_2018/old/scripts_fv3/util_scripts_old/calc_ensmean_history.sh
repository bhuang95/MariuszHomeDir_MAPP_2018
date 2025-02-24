#!/bin/ksh

ident=2015081006

nanals=20

INDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/FV3_RUNS/DA_ENKF/${ident}/OUTPUT_FV3
SRCDIR=/home/Mariusz.Pagowski/codes/src_da_utils

EXEC=${SRCDIR}/calc_ensmean_fv3_history.x

cd $INDIR

if [[ ! -r ensmean ]]
then
    mkdir -p ensmean
fi

/bin/cp ${SRCDIR}/ensmean.nml .

itile=1
while [[ $itile -le 6 ]]
do
    echo "tile = $itile"
    /bin/cp ./mem001/${ident}0000.fv3_history.tile${itile}.nc fv3.history.nc.ensmean
    nanal=1
    while [[ $nanal -le $nanals ]]
    do
	charnanal=mem`printf %03i $nanal`
	((nanal=nanal+1))
	ln -sf ./${charnanal}/${ident}0000.fv3_history.tile${itile}.nc fv3.history.nc.${charnanal}
    done
    mpirun -n $nanals ${EXEC} $nanals fv3.history.nc 'T'
    /bin/mv fv3.history.nc.ensmean ./ensmean/${ident}0000.fv3_history.tile${itile}.nc
    ((itile=itile+1))
done


