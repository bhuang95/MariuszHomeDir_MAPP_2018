#!/bin/sh

INDIR=/scratch3/BMC/chem-var/pagowski/gmac_2018/aeronet
OUTDIR=${INDIR}

infile=${INDIR}/AERONET_NA_2015.lev20
outfile=${OUTDIR}/aod550_4met.txt

start_ident=2015080100
end_ident=2012090100

ident=$start_ident

echo $file

tmpfile=${OUTDIR}/tmp.txt

echo "date,time,870,675,440,380,site_name,lat,lon,elev" > $tmpfile
  
sed '1,8d' $infile | cut -f 2-3,8,11,23,26,74-77 -d',' | sed '$d' >> $tmpfile

# sed '$d' - remove the last line because of od characters

aeronet4met.x $tmpfile $outfile

