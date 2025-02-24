#!/bin/ksh 
#SBATCH -n 1
#SBATCH -t 08:00:00
##SBATCH -t 00:30:00
#SBATCH -q batch
##SBATCH -q debug
##SBATCH -A wrf-chem
#SBATCH -A chem-var
#SBATCH -J nucaps_co2ioda
#SBATCH -D /home/Mariusz.Pagowski/mapp_2018/scripts
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j

#sbatch --export=ALL sbatch_run_viirs2ioda.sh

. /etc/profile

. ~/MAPP_2018/.environ.ksh

module use -a /scratch2/NCEPDEV/marineda/Jong.Kim/save/modulefiles/
module load anaconda/3.15.1

#module load contrib
#module load anaconda/latest

set -x

cd /home/Mariusz.Pagowski/mapp_2018/src_omp

python run_nucaps_co2ioda.py


