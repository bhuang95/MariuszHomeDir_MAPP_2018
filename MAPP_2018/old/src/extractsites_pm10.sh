#!/bin/sh

INDIR=/scratch1/portfolios/BMC/chem-var/pagowski/codes/indata
OUTDIR=${INDIR}

sitefile=${OUTDIR}/monitoring_site_locations_2012.dat
pm10_sitefile=${OUTDIR}/PM10_airnow_sites.dat

/bin/rm -f ${all_sitefile}

echo pm10 > ${pm10_sitefile}

sed -e /PM10/\!d -e /PM10-15/d $sitefile | cut -f1,9-11 -d'|' --output-delimiter=','  >> ${pm10_sitefile}

