#!/bin/bash
#SBATCH -J sig2nc_run 
#SBATCH -q batch
#SBATCH -p bigmem
#SBATCH -A gsd-fv3-dev
##SBATCH -A chem-var
#SBATCH -N 8
#SBATCH -D /home/Mariusz.Pagowski/mapp_2018/scripts
#SBATCH -n 20
#SBATCH -t 7:59:00
##SBATCH --open-mode=truncate
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j

#sbatch --export=ALL,start=2015121300,end=2015121300 sbatch_nems2nc.sh

set -x

ndate=~/bin/ndate

export yyyymmddhh_start=$start
export yyyymmddhh_end=$end

source /home/Mariusz.Pagowski/mapp_2018/scripts/machine-setup.sh

python /home/Mariusz.Pagowski/mapp_2018/scripts/nems2nc_jediaero_met.py

