#!/bin/ksh

. /etc/profile

. ../.environ.ksh

start_date=2021052700
end_date=2021052700

start_date=2019080100
end_date=2019093000

nxy=96

cycle_frequency=24

#NRTDIR=/scratch2/BMC/public/data/grids/nesdis/GBBEPx/C96
NRTDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/WORKDATA/NRTdata/rawdata/gbbepx/data

GBBEPxDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/GBBEPx

#OUTDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/nrt/GBBEPx
OUTDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/WORKDATA/NRTdata/GBBEPx/C96

EXECDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/exec
SCRIPTDIR=~/mapp_2018/scripts
TMPDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/tmpdir/workdir_gbbepx

ndate=~/bin/ndate

cd $TMPDIR

/bin/cp ${EXECDIR}/convert_gbbepx.x .

ident=${start_date}

while [[ ${ident} -le ${end_date} ]]
do

    . ${SCRIPTDIR}/gbbepx_nrt_bin2nc.sh
    ident=`$ndate $cycle_frequency $ident`

done
