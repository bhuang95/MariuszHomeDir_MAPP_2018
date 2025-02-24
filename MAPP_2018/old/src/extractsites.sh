#!/bin/sh

INDIR=/scratch1/portfolios/BMC/chem-var/pagowski/codes/indata
OUTDIR=${INDIR}

sitefile=${OUTDIR}/monitoring_site_locations_2012.dat

o3_sitefile=${OUTDIR}/o3_airnow_sites.dat
pm2_5_sitefile=${OUTDIR}/PM2_5_DRY_airnow_sites.dat
all_sitefile=${OUTDIR}/monitoring_sites_2012.dat

/bin/rm -f ${all_sitefile}

sed -e /O3/\!d $sitefile | cut -f1,9-11 -d'|' --output-delimiter=','      > $o3_sitefile
  
sed -e /PM2.5/\!d -e /PM2.5-15/d $sitefile | cut -f1,9-11 -d'|' --output-delimiter=','  > ${pm2_5_sitefile}

cat $o3_sitefile ${pm2_5_sitefile} > ${all_sitefile}
