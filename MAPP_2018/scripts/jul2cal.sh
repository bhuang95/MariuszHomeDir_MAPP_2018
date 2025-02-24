#!/bin/ksh

#convert julian date to calendar date

cd /scratch1/BMC/chem-var/pagowski/DATA/OBS/VIIRS_lunar

for infile in npp*.nc
do
    echo $infile
    year=`echo $infile | cut -c6-9`
    jday=`echo $infile | cut -c10-12`
    hhmm=`echo $infile | cut -c14-17`
    newdate=`date -d "${year}-01-01 +$(( ${jday} - 1 ))days" +%Y%m%d`$hhmm
    ln -sf $infile VIIRS_lunar_npp_s${newdate}000_e${newdate}001.nc
done
