#!/bin/bash
#SBATCH -J nems2nc_c
#SBATCH -q batch
#SBATCH -p bigmem
#SBATCH -A gsd-fv3-dev
##SBATCH -A chem-var
#SBATCH -n 20
#SBATCH -D /home/Mariusz.Pagowski/mapp_2018/scripts
#SBATCH -n 20
#SBATCH -t 1:59:00
##SBATCH --open-mode=truncate
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j

##sbatch --export=ALL,start=2019072000,end=2019073018 sbatch_nems2nc_control.sh

set -x

export yyyymmddhh_start=$start
export yyyymmddhh_end=$end

source /home/Mariusz.Pagowski/mapp_2018/scripts/machine-setup.sh

python /home/Mariusz.Pagowski/mapp_2018/scripts/nems2nc_control.py

