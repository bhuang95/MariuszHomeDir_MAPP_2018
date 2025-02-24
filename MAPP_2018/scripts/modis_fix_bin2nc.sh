#!/bin/ksh

year=`echo "${ident}" | cut -c1-4`
month=`echo "${ident}" | cut -c5-6`
day=`echo "${ident}" | cut -c7-8`
hour=`echo "${ident}" | cut -c9-10`


time="${year}-${month}-${day} ${hour}:00:00.0"
date=${year}${month}${day}

modistime=${year}-${month}-${day}-000000

outdir=${OUTDIR}/${date}

if [[ ! -r $outdir ]]
then
    mkdir -p $outdir
fi

tile=1

while [[ $tile -le 6 ]]
do 

    pathlon=${GBBEPxDIR}/C${nxy}_fix/lon/lon_tile${tile}.dat
    pathlat=${GBBEPxDIR}/C${nxy}_fix/lat/lat_tile${tile}.dat
    pathebc=${MODISDIR}/tile${tile}/C${nxy}-T-${modistime}-BC-bb.bin
    patheoc=${MODISDIR}/tile${tile}/C${nxy}-T-${modistime}-OC-bb.bin
    pathepm25=${MODISDIR}/tile${tile}/C${nxy}-T-${modistime}-BBURN2-bb.bin
    patheso2=${MODISDIR}/tile${tile}/C${nxy}-T-${modistime}-SO2-bb.bin
    patheco=${MODISDIR}/tile${tile}/C${nxy}-T-${modistime}-CO-bb.bin
    patheplume=${MODISDIR}/tile${tile}/C${nxy}-T-${modistime}-plume.bin

    outfile=${outdir}/FIRE_GBBEPx_data.tile${tile}.nc

cat > convert_gbbepx.nl <<EOF
&record_input
   title = "GBBEPx emissions"
   tile = $tile
   time = "$time"
   nlon = $nxy
   nlat = $nxy
   outfile     = "$outfile"
   pathlon     = "$pathlon"
   pathlat     = "$pathlat"
   pathebc     = "$pathebc"
   patheoc     = "$patheoc"
   pathepm25   = "$pathepm25"
   patheso2    = "$patheso2"
   patheco     = "$patheco"
   patheplume  = "$patheplume"
/
EOF

    ./convert_fix_modis.x

    ((tile=tile+1))

done
