#!/bin/ksh

. /etc/profile

. ../.environ.ksh

EXECDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/exec
TMPDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/tmpdir/workdir_gbbepx

cd $TMPDIR

/bin/cp ${EXECDIR}/convert_gbbepx_noco.x .

nxy=96

cat > convert_gbbepx_noco.nl <<EOF
&record_input
   title = "gbbepx emissions"
   tile = 1
   time = '2015-12-05 00:00:00.0'
   nlon = $nxy
   nlat = $nxy
   outfile     = "FIRE_GBBEPx_c${nxy}.tile1.nc"
   pathlon     = "/scratch1/BMC/gsd-fv3-dev/Haiqin.Li/Develop/emi_C${nxy}/lon/lon_tile1.dat"
   pathlat     = "/scratch1/BMC/gsd-fv3-dev/Haiqin.Li/Develop/emi_C${nxy}/lat/lat_tile1.dat"
   pathebc     = "/scratch1/BMC/gsd-fv3-dev/MAPP_2018/swei/emissionfiles/C96/2015120512/gfs/prep/tile1/ebu_bc.dat"
   patheoc     = "/scratch1/BMC/gsd-fv3-dev/MAPP_2018/swei/emissionfiles/C96/2015120512/gfs/prep/tile1/ebu_oc.dat"
   pathepm25   = "/scratch1/BMC/gsd-fv3-dev/MAPP_2018/swei/emissionfiles/C96/2015120512/gfs/prep/tile1/ebu_pm_25.dat"
   patheso2    = "/scratch1/BMC/gsd-fv3-dev/MAPP_2018/swei/emissionfiles/C96/2015120512/gfs/prep/tile1/ebu_so2.dat"
   patheplume  = "/scratch1/BMC/gsd-fv3-dev/MAPP_2018/swei/emissionfiles/C96/2015120512/gfs/prep/tile1/plumestuff.dat"
/
EOF

./convert_gbbepx_noco.x
