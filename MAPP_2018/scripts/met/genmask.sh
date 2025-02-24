#!/bin/ksh
set -x 

. /etc/profile
. ~/mapp_2018/.environ_met.ksh

TMPDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/tmpdir/workdir_genmask

MASKDIR=/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/MODEL/masks

GRIDFILE=${MASKDIR}/input_grid/gdas.t00z.pgrb2.0p25.f000
#GRIDFILE=${MASKDIR}/input_grid/gdas.pgrb2.0p10

OUTDIR=${MASKDIR}/nc_mask

MSKNAME='RUSC2S'
MSKNAME='TROP'
MSKNAME='NAFRME'
MSKNAME='SOCEAN'
MSKNAME='EASIA'
MSKNAME='SAFRTROP'
MSKNAME='CONUS'

case $MSKNAME in
'TROP')
   channel='Y'
   minlat=-20; maxlat=20; minlon=-180; maxlon=180 ;;
'NAFRME')
   minlat=5; maxlat=40; minlon=-20; maxlon=70 ;;
'SAFRTROP')
   minlat=-20; maxlat=5; minlon=-10; maxlon=35 ;;
'SOCEAN')
   channel='Y'
   minlat=-65; maxlat=-40; minlon=-180; maxlon=180 ;;
'RUSC2S')
   minlat=45; maxlat=70; minlon=50; maxlon=120 ;;
'EASIA')
   minlat=20; maxlat=50; minlon=105; maxlon=145 ;;
'CONUS')
   polyfile=$MET_PATH/share/met/poly/CONUS.poly ;;
esac

if [[ ! -r $TMPDIR ]]
then
    mkdir -p $TMPDIR
fi

cd $TMPDIR

if [ -s tmppoly.txt ]; then
   rm tmppoly.txt
fi

cat > tmppoly.txt << EOF
$MSKNAME
$minlat $minlon
$maxlat $minlon
$maxlat $maxlon
$minlat $maxlon
EOF

polyfile=${polyfile:-./tmppoly.txt}
channel=${channel:-'N'}

if [ $channel == 'Y' ]; then
   thresh_string=">=$minlat&&<=$maxlat"
   gen_vx_mask $GRIDFILE $GRIDFILE ${MSKNAME}_MSK.nc -type lat -thresh $thresh_string -name $MSKNAME
else
   gen_vx_mask $GRIDFILE $polyfile ${MSKNAME}_MSK.nc -type poly -name $MSKNAME
fi

/bin/mv ${MSKNAME}_MSK.nc $OUTDIR


