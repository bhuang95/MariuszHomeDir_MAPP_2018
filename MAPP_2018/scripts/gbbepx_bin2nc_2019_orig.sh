#!/bin/ksh

#this script converts bin to ncdf only but some FRP values are noise 
year=`echo "${ident}" | cut -c1-4`
month=`echo "${ident}" | cut -c5-6`
day=`echo "${ident}" | cut -c7-8`
hour=`echo "${ident}" | cut -c9-10`


time="${year}-${month}-${day} ${hour}:00:00.0"
date=${year}${month}${day}

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
    pathebc=${GBBEPxDIR}/C${nxy}_frp/${date}/GBBEPx.bc.${date}.FV3.C${nxy}Grid.tile${tile}.bin
    patheoc=${GBBEPxDIR}/C${nxy}_frp/${date}/GBBEPx.oc.${date}.FV3.C${nxy}Grid.tile${tile}.bin
    pathepm25=${GBBEPxDIR}/C${nxy}_frp/${date}/GBBEPx.pm25.${date}.FV3.C${nxy}Grid.tile${tile}.bin
    patheso2=${GBBEPxDIR}/C${nxy}_frp/${date}/GBBEPx.so2.${date}.FV3.C${nxy}Grid.tile${tile}.bin
    patheco=${GBBEPxDIR}/C${nxy}_frp/${date}/GBBEPx.co.${date}.FV3.C${nxy}Grid.tile${tile}.bin
    patheplume=${GBBEPxDIR}/C${nxy}_frp/${date}/meanFRP.${date}.FV3.C${nxy}Grid.tile${tile}.bin
    

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

    ./convert_gbbepx.x

    ((tile=tile+1))

done
