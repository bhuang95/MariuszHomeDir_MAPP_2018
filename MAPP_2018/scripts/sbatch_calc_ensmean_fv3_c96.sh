#!/bin/ksh 
#SBATCH -n 8
#SBATCH -t 00:10:00
#SBATCH -q debug
#SBATCH -A chem-var
#SBATCH -J calc_ensmean_fv3
#SBATCH -D /home/Mariusz.Pagowski/mapp_2018/scripts
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j

#sbatch --export=ALL sbatch_calc_ensmean_fv3_c96.sh

. /etc/profile

. ~/MAPP_2018/.environ.ksh

set -x

nanals=8

if [[ $nanals -gt $SLURM_NTASKS ]]
then
    echo "SLURM_NTASKS=$SLURM_NTASKS < nanals=$nanals - Stopping"
    exit 1
fi

ident=2018041500

analdate=$ident
year=`echo $analdate | cut -c1-4`
month=`echo $analdate | cut -c5-6`
day=`echo $analdate | cut -c7-8`
hour=`echo $analdate | cut -c9-10`

adate=${year}${month}${day}

ndate=~/bin/ndate

MAINDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski
EXECDIR=${MAINDIR}/exec
EXEC=${EXECDIR}/calc_ensmean_fv3.x

INDIR=/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/envar_2018/C96/RESTART/gfs_aero_c96

workdir=${MAINDIR}/tmpdir/workdir_calc_ensmean

/bin/rm -rf $workdir
mkdir -p $workdir

cd $workdir

cat > ensmean.nml <<EOF
&ensmean_nml
varnames =  'sphum','bc1','bc2','oc1','oc2','sulf','dust1','dust2','dust3','dust4','dust5','seas1','seas2','seas3','seas4','seas5'
/
EOF


itile=1
while [[ $itile -le 6 ]]
do
    echo $itile
    /bin/cp ${INDIR}/mem001/${adate}.${hour}0000.fv_tracer.res.tile${itile}.nc restart.ensmean
    chmod u+w restart.ensmean

    nanal=1
    while [[ $nanal -le $nanals ]]
    do
	charnanal=mem`printf %03i $nanal`
	indir=${INDIR}/${charnanal}
	ln -sf ${indir}/${adate}.${hour}0000.fv_tracer.res.tile${itile}.nc restart.${charnanal}
	if [[ ! -s restart.${charnanal} ]]
	then
	    echo "Missing fv3 restart output for member ${charnanal} tile ${itile} - Stopping"
	    exit 1
	fi
	((nanal=nanal+1))
    done

    mpirun -np $nanals ${EXEC} $nanals restart
    if [[ $? != 0 ]]
    then
	echo "calc_ensmean failed - Stopping"
	exit 1
    fi

    if [[ ! -r ${INDIR}/ensmean ]]
    then
	mkdir -p ${INDIR}/ensmean
    fi

    /bin/mv restart.ensmean ${INDIR}/ensmean/${adate}.${hour}0000.fv_tracer.res.tile${itile}.nc
    if [[ $itile == 1 ]] then
	/bin/cp ${INDIR}/mem001/${adate}.${hour}0000.coupler.res ${INDIR}/ensmean
    fi

    ((itile=itile+1))
done


cat > ensmean.nml <<EOF
&ensmean_nml
varnames =  'T','delp'
/
EOF


itile=1
while [[ $itile -le 6 ]]
do
    echo $itile
    /bin/cp ${INDIR}/mem001/${adate}.${hour}0000.fv_core.res.tile${itile}.nc restart.ensmean
    chmod u+w restart.ensmean

    nanal=1
    while [[ $nanal -le $nanals ]]
    do
	charnanal=mem`printf %03i $nanal`
	indir=${INDIR}/${charnanal}
	ln -sf ${indir}/${adate}.${hour}0000.fv_core.res.tile${itile}.nc restart.${charnanal}
	if [[ ! -s restart.${charnanal} ]]
	then
	    echo "Missing fv3 restart output for member ${charnanal} tile ${itile} - Stopping"
	    exit 1
	fi
	((nanal=nanal+1))
    done

    mpirun -np $nanals ${EXEC} $nanals restart
    if [[ $? != 0 ]]
    then
	echo "calc_ensmean failed - Stopping"
	exit 1
    fi

    if [[ ! -r ${INDIR}/ensmean ]]
    then
	mkdir -p ${INDIR}/ensmean
    fi

    /bin/mv restart.ensmean ${INDIR}/ensmean/${adate}.${hour}0000.fv_core.res.tile${itile}.nc

    ((itile=itile+1))
done

exit 0
