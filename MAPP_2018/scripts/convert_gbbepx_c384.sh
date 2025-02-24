#!/bin/ksh

. /etc/profile

. ../.environ.ksh

EXECDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/exec
TMPDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/tmpdir/workdir_gbbepx

cd $TMPDIR

/bin/cp ${EXECDIR}/convert_gbbepx.x .

nxy=384

cat > convert_gbbepx.nl <<EOF
&record_input
   title = "gbbepx emissions"
   tile = 1
   date = '2020-02-01'
   nlon = $nxy
   nlat = $nxy
   outfile     = "FIRE_GBBEPx_c${nxy}.tile1.nc"
   pathlon     = "/scratch1/BMC/gsd-fv3-dev/Haiqin.Li/Develop/emi_C${nxy}/lon/lon_tile1.dat"
   pathlat     = "/scratch1/BMC/gsd-fv3-dev/Haiqin.Li/Develop/emi_C${nxy}/lat/lat_tile1.dat"
   pathebc     = "/scratch1/BMC/gsd-fv3-dev/lzhang/GBBEPx/20200201/GBBEPx.bc.20200201.FV3.C${nxy}Grid.tile1.bin"
   patheoc     = "/scratch1/BMC/gsd-fv3-dev/lzhang/GBBEPx/20200201/GBBEPx.oc.20200201.FV3.C${nxy}Grid.tile1.bin"
   pathepm25   = "/scratch1/BMC/gsd-fv3-dev/lzhang/GBBEPx/20200201/GBBEPx.pm25.20200201.FV3.C${nxy}Grid.tile1.bin"
   patheso2    = "/scratch1/BMC/gsd-fv3-dev/lzhang/GBBEPx/20200201/GBBEPx.so2.20200201.FV3.C${nxy}Grid.tile1.bin"
   patheco    = "/scratch1/BMC/gsd-fv3-dev/lzhang/GBBEPx/20200201/GBBEPx.co.20200201.FV3.C${nxy}Grid.tile1.bin"
   patheplume  = "/scratch1/BMC/gsd-fv3-dev/lzhang/GBBEPx/20200201/meanFRP.20200201.FV3.C${nxy}Grid.tile1.bin"
/
EOF

./convert_gbbepx.x
