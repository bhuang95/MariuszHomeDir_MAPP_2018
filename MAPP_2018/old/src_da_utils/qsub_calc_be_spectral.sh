#!/bin/ksh --login
#PBS -N calc_be
#PBS -A chem-var
##PBS -A fim
#PBS -l procs=96
#PBS -q debug
##for trunc >=574 use bigmem
##PBS -q bigmem 
#PBS -l walltime=00:10:00
#PBS -d /home/Mariusz.Pagowski/codes/src_da_utils
#PBS -o /home/Mariusz.Pagowski/codes/src_da_utils/qslogs
#PBS -e /home/Mariusz.Pagowski/codes/src_da_utils/qslogs

set -x

datestring=2012081400
#datestring=2016082206


SRCDIR=~/codes/src_da_utils
INDIR=/scratch3/BMC/chem-var/pagowski/fv3/indata/${datestring}

nens=80

. /etc/profile
. /apps/lmod/lmod/init/sh

module purge
module load intel
module load mvapich2/2.1a
module load netcdf
module load udunits
module load nco
echo "MODULE LIST"

module list

cd $INDIR

/bin/cp $SRCDIR/calc_be_spectral.x .

mpirun -np $nens calc_be_spectral.x $nens $datestring
