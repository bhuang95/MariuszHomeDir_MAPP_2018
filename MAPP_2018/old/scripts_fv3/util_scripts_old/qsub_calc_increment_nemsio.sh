#!/bin/ksh --login
#PBS -N calc_incr
#PBS -A chem-var
##PBS -A fim
#PBS -l procs=24
#PBS -q debug
##PBS -q urgent
#PBS -l walltime=00:10:00
#PBS -d /home/Mariusz.Pagowski/codes/fv3/util_scripts
#PBS -o /scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/qslogs

#qsub -v ident=2015080612 qsub_calc_increment_nemsio.sh

ident=2015081012

. /etc/profile
. /apps/lmod/lmod/init/sh

set -x

. ./environ.sh

ndate=~/bin/ndate

EXEC=/home/Mariusz.Pagowski/codes/src_da_utils/calc_increment_nemsio.x

workdir=${BASEDIR}/workdir_calc_increment

/bin/rm -rf $workdir
mkdir -p $workdir

analdate=$ident
analdatem=`$ndate -${ANALINC} $analdate`

file_fcst=${FV3_RUNS}/DA_GSI/${analdatem}/OUTPUT_FV3_nemsio/sfg_${analdate}_fhr06_control
file_anal=${FV3_RUNS}/DA_GSI/${analdate}/OUTPUT_GSI_nemsio/siganl_${analdate}_control
file_nc_in=${FV3_FIXDIR}/C${CRES}/fv3_increment_zero.nc

OUTDIR=${FV3_RUNS}/DA_GSI/${analdate}/OUTPUT_GSI_increment

file_increment=fv3_increment.nc

if [[ ! -r $OUTDIR ]]
then
    mkdir -p $OUTDIR
fi

cd $workdir

/bin/cp ${file_nc_in} ./${file_increment}

ln -sf $file_fcst file_fcst
ln -sf $file_anal file_anal

${EXEC} file_fcst file_anal $file_increment
ncpdq -O -a '-lev' $file_increment ${file_increment}_lev_reversed

/bin/mv $file_increment ${file_increment}_lev_reversed $OUTDIR

cd $SCRIPTDIR

echo "qsub -v ident=$ident qsub_fv3_gfs_c192_warm_da.sh"

exit

qsub -v ident=$ident qsub_fv3_gfs_c192_warm_da.sh
