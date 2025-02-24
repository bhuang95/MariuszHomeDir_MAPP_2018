#!/bin/ksh

SRCDIR=~/mapp_2018/src_gsidiag2nc
INDIR=/scratch3/BMC/wrf-chem/pagowski/tmp/FV3GFS/FV3_RUNS/DA_ENKF

. /etc/profile
. /apps/lmod/lmod/init/sh

cd $INDIR

ndate=~/bin/ndate
start_date=2015080200
end_date=2015081700

start_date=2015080900
end_date=2015081618

cycle_frequency=6
ident=$start_date

while [[ ${ident} -le ${end_date} ]]
do

    echo $ident

    indir=${INDIR}/${ident}/ensmean/OUTPUT_GSI_nemsio

    cd $indir

    echo $indir

    charnanal=ensmean
    fname=diag_nnr_ges.${ident}_${charnanal}.4iugg
    ${SRCDIR}/gsidiag_aod_bin2nc4.x -append_nc4 ${fname}

    ident=`$ndate $cycle_frequency $ident`

done
