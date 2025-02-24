#!/bin/ksh

NCLDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/ncl
cd $NCLDIR

start_ident=2018041406
end_ident=2018041718

cycle_frequency=6

ndate=~/bin/ndate
ident=$start_ident

MAINDIR=$NCLDIR

ANALDIR=${MAINDIR}/pics_anal
CNTRDIR=${ANALDIR}/control
ENSMDIR=${ANALDIR}/ensmean
INCRDIR=${MAINDIR}/pics_incr
FONT=CARLITO

ident=${start_ident}

while [[ ${ident} -le ${end_ident} ]]
do

    echo $ident

    convert ${INCRDIR}/seas_incr_${ident}.png -font $FONT -pointsize 60 -gravity South -annotate 0 "Increment" -trim ${MAINDIR}/process/a.png
    convert ${CNTRDIR}/seas_anal_${ident}.png -font $FONT -pointsize 60 -gravity South -annotate 0  "Hyb-Var Analysis" -trim ${MAINDIR}/process/b.png
    convert ${ENSMDIR}/seas_anal_${ident}.png -font $FONT -pointsize 60 -gravity South -annotate 0 "EnKF Analysis" -trim ${MAINDIR}/process/c.png
    
    convert ${INCRDIR}/dust_incr_${ident}.png -font $FONT -trim ${MAINDIR}/process/d.png
    convert ${CNTRDIR}/dust_anal_${ident}.png -font $FONT -trim ${MAINDIR}/process/e.png
    convert ${ENSMDIR}/dust_anal_${ident}.png -font $FONT -trim ${MAINDIR}/process/f.png
    
    montage ${MAINDIR}/process/a.png ${MAINDIR}/process/b.png ${MAINDIR}/process/c.png ${MAINDIR}/process/d.png ${MAINDIR}/process/e.png ${MAINDIR}/process/f.png -tile 3x2  -geometry 400x seas_dust_${ident}.png

    /bin/mv seas_dust_${ident}.png ${MAINDIR}/pics_4movie

    ident=`ndate +${cycle_frequency} ${ident}`

done
