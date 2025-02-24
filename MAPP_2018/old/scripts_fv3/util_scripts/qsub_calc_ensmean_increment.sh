#!/bin/ksh --login
#PBS -N calc_incr_ensmean
##PBS -A chem-var
#PBS -A wrf-chem
##PBS -A gsd-fv3-dev
#PBS -l procs=24
##PBS -q debug
##PBS -q urgent
#PBS -q batch
##PBS -q bigmem 
#PBS -l walltime=00:10:00
#PBS -d /home/Mariusz.Pagowski/codes/scripts_fv3/util_scripts
#PBS -o /scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/qslogs

#qsub -v ident=2015080106 qsub_calc_ensmean_increment.sh

#ident=2015081006

. /etc/profile

. /home/Mariusz.Pagowski/enkf_runs/scripts/modules/gsi_modules.sh

set -x

. ./environ.sh

nanals=$NANALS
PBS_NP=$nanals

if [[ $nanals -gt $PBS_NP ]]
then
    echo "PBS_NP=$PBS_NP < nanals=$nanals - Stopping"
    exit 1
fi

sim=DA_ENKF

ndate=~/bin/ndate

EXECDIR=${BASEDIR}/execs

analdate=$ident

EXEC=${MY_EXECDIR}/calc_mean_increment_fv3.x

workdir=${MAINDIR}/tmp_dirs/workdir_calc_mean_increment

/bin/rm -rf $workdir
mkdir -p $workdir

cd $workdir

cat > increment_mean.nml <<EOF
&increment_mean_nml
varnames2d = ,
varnames3d =  'bc1_inc','bc2_inc','oc1_inc','oc2_inc','p25_inc','sulf_inc','dust1_inc','dust2_inc','dust3_inc','dust4_inc','dust5_inc','seas1_inc','seas2_inc','seas3_inc','seas4_inc'
/
EOF

INDIR=${FV3_RUNS}/${sim}/${analdate}

/bin/cp ${FV3_FIXDIR}/C${RES}/fv3_increment_zero.nc increment.ensmean

nanal=1
while [[ $nanal -le $nanals ]]
do
    charnanal=mem`printf %03i $nanal`
    indir=${INDIR}/${charnanal}/OUTPUT_ENKF
    ln -sf ${indir}/increment_${analdate}_${charnanal}.nc increment.${charnanal}
    if [[ ! -s increment.${charnanal} ]]
    then
	echo "Missing increment for member ${charnanal} - Stopping"
	exit 1
    fi
    ((nanal=nanal+1))
done

mpirun -n $nanals ${EXEC} $nanals increment
if [[ $? -ne 0 ]]
then
    echo "calc_mean_increment failed -Stopping"
    exit 1
fi

if [[ ! -r ${INDIR}/ensmean/OUTPUT_ENKF ]]
then
    mkdir -p ${INDIR}/ensmean/OUTPUT_ENKF
fi

/bin/mv increment.ensmean ${INDIR}/ensmean/OUTPUT_ENKF/increment_${analdate}_ensmean.nc
if [[ $? -ne 0 ]]
then
    echo "Increment failed for ensmean"
    exit 1
fi

((ANALINC2=2*ANALINC))

analdatem2=`$ndate -$ANALINC2 $analdate`

INDIRm2=${FV3_RUNS}/${sim}/${analdatem2}

if [[ -r ${INDIRm2}/ensmean/OUTPUT_ENKF/increment_${analdatem2}_ensmean.nc && -r ${INDIRm2}/ensmean/RESTART ]]
then
    echo "rm -rf ${INDIRm2}/mem???"
#    rm -rf ${INDIRm2}/mem???
fi

exit 0

cd ${INDIR}

nanal=1
while [[ $nanal -le $nanals ]]
do
    charnanal=mem`printf %03i $nanal`
    /bin/rm -rf ${charnanal}
    ((nanal=nanal+1))
done



exit 0
