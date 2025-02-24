#!/bin/ksh --login
#PBS -N calc_be
#PBS -A chem-var
##PBS -A fim
#PBS -l procs=24
#PBS -q debug
##for trunc >=574 use bigmem
##PBS -q bigmem 
#PBS -l walltime=00:10:00
#PBS -d /home/Mariusz.Pagowski/codes/src_da_utils
#PBS -o /home/Mariusz.Pagowski/codes/src_da_utils/qslogs
#PBS -e /home/Mariusz.Pagowski/codes/src_da_utils/qslogs

set -x

datestring=2015080100
#datestring=2016082206


SRCDIR=~/codes/src_da_utils
INDIR=/scratch3/BMC/chem-var/pagowski/tmp/indata_gfs_spectral

nens=2

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

/bin/cp $SRCDIR/read_spectral.x .

./read_spectral.x 1 $datestring

