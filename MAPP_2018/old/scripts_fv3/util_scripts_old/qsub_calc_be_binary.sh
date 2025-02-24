#!/bin/ksh --login
#PBS -N calc_be
#PBS -A chem-var
##PBS -A fim
##PBS -l procs=120
#PBS -l procs=72
#PBS -q debug
##for trunc >=574 use bigmem
##PBS -q bigmem 
#PBS -l walltime=00:30:00
#PBS -d /home/Mariusz.Pagowski/codes/fv3/util_scripts
#PBS -o /scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/qslogs

set -x

datestring=2015080500


SRCDIR=~/codes/src_da_utils
INDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/FV3_RUNS/BCKG/bin_regrid_bckg

#nens=107
nens=56

. /etc/profile
. /apps/lmod/lmod/init/sh

module purge
module load intel
#module load impi
module load mvapich2
module load netcdf
module load udunits
module load nco
echo "MODULE LIST"

module list

cd $INDIR

/bin/cp $SRCDIR/calc_be_binary.x .

mpirun -np $nens calc_be_binary.x $nens $datestring
