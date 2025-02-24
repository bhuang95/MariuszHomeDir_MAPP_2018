#!/bin/ksh 
#SBATCH -J getfires
#SBATCH -A chem-var
#SBATCH -n 1
#SBATCH -t 08:00:00
#SBATCH -p service
#SBATCH -D /home/Mariusz.Pagowski/mapp_2018/scripts
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j

#sbatch --export=ALL sbatch_get_firedata.sh

set -x

. /etc/profile
. /apps/lmod/lmod/init/sh

module load hpss

. ~/bin/funcs.sh

hpssdir=/BMC/fdr/Permanent
storedirmodis=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/frp_issue/MODIS_data
#storedirwfabba=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/frp_issue/WFABBA_data

startdate=2019050500
enddate=2019060600

startdate=2016050100
enddate=2016070100

startdate=2016070100
enddate=2016083100


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

#  cd $storedirwfabba

#  hsi get ${hpssdir}/${year}/${month}/${day}/data/sat/ssec/goes-west/wf_abba/$fname
#  hsi get ${hpssdir}/${year}/${month}/${day}/data/sat/nesdis/wf_abba/$fname

#  unzip -o $fname

  increment_date $increment

  current_date=${end_year}${end_month}${end_day}${end_hr}

  echo $current_date

done

