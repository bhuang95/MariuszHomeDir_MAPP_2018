#!/bin/ksh

. /etc/profile

. ../.environ.ksh

#modis
start_date=2015120100
end_date=2016020300

#nofrp
start_date=2016020400
end_date=2016123100

#frp
#start_date=2016060100
#end_date=2016083100

#2019
start_date=2019072000
end_date=2019081000

start_date=2021062900
end_date=2021062900

nxy=96

cycle_frequency=24

GBBEPxDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/DATA/GBBEPx
GBBEPxDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/WORKDATA/NRTdata/rawdata/gbbepx

MODISDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/frp_issue/prep

OUTDIR=${GBBEPxDIR}/ncdf/C${nxy}
OUTDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/WORKDATA/NRTdata/GBBEPx/C96

EXECDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/exec
SCRIPTDIR=~/mapp_2018/scripts
TMPDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/tmpdir/workdir_gbbepx

ndate=~/bin/ndate

cd $TMPDIR

/bin/cp ${EXECDIR}/convert_gbbepx.x ${EXECDIR}/convert_gbbepx_fixfrp.x ${EXECDIR}/convert_fix_modis.x ${EXECDIR}/convert_gbbepx_nofrp2frp.x .

ident=${start_date}

while [[ ${ident} -le ${end_date} ]]
do

#    echo $ident

    . ${SCRIPTDIR}/gbbepx_bin2nc.sh 
#    . ${SCRIPTDIR}/gbbepx_bin2nc_2019.sh 
# the following is to calculate FRP from CO
#    . ${SCRIPTDIR}/gbbepx_fixfrp_bin2nc.sh  
# the following convert original modis to ncdf
#    . ${SCRIPTDIR}/modis_bin2nc.sh
# the following convert fixes original modis using regression to gbbepx
#    . ${SCRIPTDIR}/modis_fix_bin2nc.sh     
#    . ${SCRIPTDIR}/gbbepx_nofrp_bin2nc.sh
#    . ${SCRIPTDIR}/gbbepx_nofrp2frp_bin2nc.sh
    ident=`$ndate $cycle_frequency $ident`

done
