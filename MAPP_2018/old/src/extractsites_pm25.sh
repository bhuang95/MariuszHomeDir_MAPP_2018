#!/bin/sh

INDIR=/scratch1/portfolios/BMC/chem-var/pagowski/codes/indata
OUTDIR=${INDIR}

sitefile=${OUTDIR}/monitoring_site_locations_2012.dat
pm25_sitefile=${OUTDIR}/PM2_5_airnow_sites.dat

echo pm2.5 > ${pm25_sitefile}

sed -e /PM2.5/\!d -e /PM2.5-15/d $sitefile | cut -f1,9-11 -d'|' --output-delimiter=','  >> ${pm25_sitefile}

