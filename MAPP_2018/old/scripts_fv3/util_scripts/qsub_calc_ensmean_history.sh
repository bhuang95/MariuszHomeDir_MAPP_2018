#!/bin/ksh --login
#PBS -N calc_fv3history_ensmean
##PBS -A wrf-chem
##PBS -A chem-var
##PBS -A gsd-fv3-dev
#PBS -A wrf-chem
#PBS -l procs=24
##PBS -q debug
##PBS -q urgent
#PBS -q batch
##PBS -q bigmem 
#PBS -l walltime=00:10:00
#PBS -d /home/Mariusz.Pagowski/codes/scripts_fv3/util_scripts
#PBS -o /scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/qslogs

#qsub -v ident=2015080106 qsub_calc_ensmean_history.sh


. /etc/profile

. /home/Mariusz.Pagowski/enkf_runs/scripts/modules/gsi_modules.sh

set -x

. ./environ.sh

nanals=$NANALS

if [[ $nanals -gt $PBS_NP ]]
then
    echo "PBS_NP=$PBS_NP < nanals=$nanals - Stopping"
    exit 1
fi


sim=DA_ENKF

ndate=~/bin/ndate

EXECDIR=${BASEDIR}/execs

analdate=$ident

EXEC=${MY_EXECDIR}/calc_ensmean_fv3.x

workdir=${MAINDIR}/tmp_dirs/workdir_calc_ensmean

/bin/rm -rf $workdir
mkdir -p $workdir

cd $workdir

cat > ensmean.nml <<EOF
&ensmean_nml
varnames2d = ,
varnames3d =  'bc1','bc2','oc1','oc2','p25','sulf','dust1','dust2','dust3','dust4','dust5','seas1','seas2','seas3','seas4'
/
EOF

adate=`echo ${analdate} | cut -c 1-8`

INDIR=${FV3_RUNS}/${sim}/${analdate}

itile=1
while [[ $itile -le 6 ]]
do
    echo $itile
    /bin/cp ${INDIR}/mem001/OUTPUT_FV3/${adate}.fv3_history.tile${itile}.nc fv3.history.nc.ensmean
    nanal=1
    while [[ $nanal -le $nanals ]]
    do
	charnanal=mem`printf %03i $nanal`
	indir=${FV3_RUNS}/${sim}/${analdate}/${charnanal}/OUTPUT_FV3
	((nanal=nanal+1))
	ln -sf ${indir}/${adate}.fv3_history.tile${itile}.nc fv3.history.nc.${charnanal}
	if [[ ! -s fv3.history.nc.${charnanal} ]]
	then
	    echo "Missing fv3 history output for member ${charnanal} tile ${itile}- Stopping"
	    exit 1
	fi
    done
    mpirun -n $nanals ${EXEC} $nanals fv3.history.nc 'T'
    if [[ $? -ne 0 ]]
    then
	echo "calc_ensmean failed -Stopping"
	exit 1
    fi

    if [[ ! -r ${INDIR}/ensmean/OUTPUT_FV3 ]]
    then
	mkdir -p ${INDIR}/ensmean/OUTPUT_FV3
    fi

    /bin/mv fv3.history.nc.ensmean ${INDIR}/ensmean/OUTPUT_FV3/${adate}.fv3_history.tile${itile}.nc
    /bin/cp ${INDIR}/mem001/OUTPUT_FV3/${adate}.fv3_history2d.tile${itile}.nc ${INDIR}/ensmean/OUTPUT_FV3/${adate}.fv3_history2d.tile${itile}.nc

    ((itile=itile+1))
done

cd $SCRIPTDIR_UTIL

echo "qsub -v ident=$ident,member=ensmean qsub_run_regrid-nemsio_4da_ensemble.sh"
qsub -v ident=$ident,member=ensmean qsub_run_regrid-nemsio_4da_ensemble.sh

echo "qsub -v ident=$ident qsub_calc_ensmean_increment.sh"
qsub -v ident=$ident qsub_calc_ensmean_increment.sh

echo "qsub -v ident=$ident qsub_calc_ensmean_restart.sh"
qsub -v ident=$ident qsub_calc_ensmean_restart.sh

echo "qsub -v ident=$ident,member=0 qsub_calc_aod_ensemble.sh"
qsub -v ident=$ident,member=0 qsub_calc_aod_ensemble.sh


exit 0
