#!/bin/ksh
#PBS -N get_fire
#PBS -A chem-var
#PBS -l procs=1
#PBS -l walltime=08:00:00
#PBS -d /home/Mariusz.Pagowski/codes/src_fv3
#PBS -o /scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/qslogs
#PBS -q service

set -x

module purge
module load hpss

. ~/bin/funcs.sh


hpssdir=/BMC/fdr/Permanent
storedirmodis=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/FIRE_DATA/MODIS
storedirwfabba=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/FIRE_DATA/WFABBA-WEST

startdate=2015080100
enddate=2015090100

increment=24

current_date=${startdate}

while [[ $current_date -le $enddate ]]
  do

  year=`echo $current_date | cut -c1-4`
  month=`echo $current_date | cut -c5-6`
  day=`echo $current_date | cut -c7-8`
  hr=`echo $current_date | cut -c9-10`

  fname=${current_date}00.zip

  cd $storedirmodis

  hsi get ${hpssdir}/${year}/${month}/${day}/data/sat/firms/global/$fname

  unzip -o $fname

  cd $storedirwfabba

  hsi get ${hpssdir}/${year}/${month}/${day}/data/sat/ssec/goes-west/wf_abba/$fname

  unzip -o $fname

  increment_date $increment

  current_date=${end_year}${end_month}${end_day}${end_hr}

  echo $current_date

done

