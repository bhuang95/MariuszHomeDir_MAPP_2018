#!/bin/ksh

ndate=~/bin/ndate

first_year=2019
first_month=05
first_day=11
first_hr=00

last_year=2019
last_month=06
last_day=10
last_hr=18

cycle_frequency=6

INDIR=/scratch3/BMC/fim/Mariusz.Pagowski/FV3_CHEM/INOUTDATA
OUTDIR=$INDIR

first_date=${first_year}${first_month}${first_day}${first_hr}
last_date=${last_year}${last_month}${last_day}${last_hr}

current_date=$first_date

last_day=${last_year}${last_month}${last_day}

year=$first_year
month=$first_month
day=$first_day
hr=$first_hr

charnanal=control

current_day=${year}${month}${day}

while [[ ${current_day} -le ${last_day} ]]
do

    YYYY=${year}
    YYYYMM=${year}${month}
    YYYYMMDD=${year}${month}${day}
    HH=${hr}

    outdir=${OUTDIR}/${charnanal}/FV3ICS/${YYYYMMDD}${HH}/gfs
    cd $outdir

    jday=`date -d ${YYYYMMDD} +%j`
    
    /bin/rm -f sfcanl.gfs.* 

    ln -sf ${YYYY}${jday}${HH}00.gdas.t${HH}z.atmanl.nemsio gdas.t${HH}z.atmanl.nemsio
    ln -sf ${YYYY}${jday}${HH}00.gdas.t${HH}z.sfcanl.nemsio gdas.t${HH}z.sfcanl.nemsio
    ln -sf ${YYYY}${jday}${HH}00.gdas.t${HH}z.nstanl.nemsio gdas.t${HH}z.nstanl.nemsio


    ident=${year}${month}${day}${hr}

    newident=`$ndate +${cycle_frequency} ${ident}`

    year=`echo "${newident}" | cut -c1-4`
    month=`echo "${newident}" | cut -c5-6`
    day=`echo "${newident}" | cut -c7-8`
    hr=`echo "${newident}" | cut -c9-10`

    current_day=${year}${month}${day}

    echo $current_day

done

