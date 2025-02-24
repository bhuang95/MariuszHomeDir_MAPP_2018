#!/bin/ksh --login
#PBS -N calc_ensmean_restart
#PBS -A chem-var
##PBS -A fim
#PBS -l procs=24
##PBS -q debug
#PBS -q urgent
##PBS -q bigmem 
#PBS -l walltime=00:10:00
#PBS -d /home/Mariusz.Pagowski/codes/fv3/util_scripts
#PBS -o /scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/qslogs

#qsub -v ident=2015081012 qsub_calc_ensmean_resatrt.sh

echo "executable needs to be written yet"
exit 1

ident=2015081012

. /etc/profile

. /home/Mariusz.Pagowski/enkf_runs/scripts/modules/gsi_modules.sh

set -x

. ./environ.sh

ndate=~/bin/ndate

EXECDIR=${BASEDIR}/execs

analdate=$ident
analdatem=`$ndate -${ANALINC} $analdate`

nanals=20

datestring=$analdate

INDIR=${FV3_RUNS}/DA_ENKF/${analdate}/OUTPUT_ENKF_nemsio_univartracer_true

outdir=${INDIR}/ensmean

if [[ ! -r $outdir ]]
then
    mkdir -p $outdir
fi

EXEC=${EXECDIR}/calc_ensmean_fv3.x

workdir=${BASEDIR}/workdir_calc_ensmean

/bin/rm -rf $workdir
mkdir -p $workdir

cd $workdir

cat > ensmean.nml <<EOF
&ensmean_nml
varnames2d = ,
varnames3d =  'bc1','bc2','oc1','oc2','p25','sulf','dust1','dust2','dust3','dust4','dust5','seas1','seas2','seas3','seas4'
/
EOF

prefix=increment_${datestring}

/bin/cp ${INDIR}/mem001/${prefix}_ ${prefix}.ensmean
nanal=1
while [[ $nanal -le $nanals ]]
do
    charnanal=mem`printf %03i $nanal`
    ((nanal=nanal+1))
    ln -sf ${INDIR}/${charnanal}/${prefix} ${prefix}.${charnanal}
done
mpirun -n $nanals ${EXEC} $nanals ${prefix} 'F'
/bin/mv ${prefix}.ensmean ${INDIR}/ensmean/${prefix}
((itile=itile+1))

exit

qsub -v ident=$ident qsub_fv3_gfs_c192_warm_da.sh
