#!/bin/ksh --login
#SBATCH -J getnnr
#SBATCH -A chem-var
#SBATCH -n 1
#SBATCH -t 24:00:00
#SBATCH -p service
#SBATCH --mem=5g
#SBATCH -D /home/Mariusz.Pagowski/mapp_2018/scripts
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j

#sbatch --export=ALL sbatch_get_nnr.sh

set -x

. /etc/profile
. /apps/lmod/lmod/init/sh

module load hpss

OUTDIR=/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/OBS/tmp
NNRDIR=/BMC/fim/5year/MAPP_2018/OBS/NNR_003_6Targets/MOD04
#NNRDIR=/BMC/fim/5year/MAPP_2018/OBS/NNR_003_6Targets/MYD04

cd $OUTDIR

hsi get -R $NNRDIR 


