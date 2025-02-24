#!/bin/ksh 

nanals=10

first_year=2018
first_month=04
first_day=06
first_hr=00

last_year=2018
last_month=04
last_day=16
last_hr=18

cycle_frequency=6

INDIR=/scratch3/BMC/fim/Mariusz.Pagowski/FV3_CHEM/INDATA/FV3ICs

OUTDIR=$INDIR

first_date=${first_year}${first_month}${first_day}${first_hr}
last_date=${last_year}${last_month}${last_day}${last_hr}

. ~/bin/funcs.sh

current_date=$first_date

last_day=${last_year}${last_month}${last_day}

year=$first_year
month=$first_month
day=$first_day
hr=$first_hr

current_day=${year}${month}${day}

while [[ ${current_day} -le ${last_day} ]]
    do

    YYYY=${year}
    YYYYMM=${year}${month}
    YYYYMMDD=${year}${month}${day}
    HH=${hr}

    nanal=1

    while [[ $nanal -le $nanals ]]
    do
	charnanal=mem`printf %03i $nanal`
	indir=${OUTDIR}/${YYYYMMDD}${HH}/${charnanal}/gfs
	outdir=${OUTDIR}/${charnanal}/${YYYYMMDD}${HH}/gfs
	mkdir -p ${outdir}
	cd $outdir
	/bin/mv ${indir}/*nemsio .
	jday=`date -d ${YYYYMMDD} +%j`

	echo ${YYYYMMDD}${HH} $nanal
	/bin/mv gdas.t${HH}z.ratmanl.${charnanal}.nemsio ${YYYY}${jday}${HH}00.gdas.t${HH}z.ratmanl.nemsio
	ln -sf ${YYYY}${jday}${HH}00.gdas.t${HH}z.ratmanl.nemsio gdas.t${HH}z.ratmanl.nemsio
	ln -sf ${YYYY}${jday}${HH}00.gdas.t${HH}z.ratmanl.nemsio siganl.gfs.${YYYYMMDD}${HH}

	/bin/mv gdas.t${HH}z.sfcanl.${charnanal}.nemsio ${YYYY}${jday}${HH}00.gdas.t${HH}z.sfcanl.nemsio

	ln -sf ${YYYY}${jday}${HH}00.gdas.t${HH}z.sfcanl.nemsio gdas.t${HH}z.sfcanl.nemsio
	ln -sf ${YYYY}${jday}${HH}00.gdas.t${HH}z.sfcanl.nemsio sfcanl.gfs.${YYYYMMDD}${HH}

	/bin/mv gdas.t${HH}z.nstanl.${charnanal}.nemsio ${YYYY}${jday}${HH}00.gdas.t${HH}z.nstanl.nemsio

	ln -sf ${YYYY}${jday}${HH}00.gdas.t${HH}z.nstanl.nemsio gdas.t${HH}z.nstanl.nemsio
	ln -sf ${YYYY}${jday}${HH}00.gdas.t${HH}z.nstanl.nemsio nstanl.gfs.${YYYYMMDD}${HH}

	((nanal=nanal+1))
    done

    increment_date ${cycle_frequency}

    year=${end_year}
    month=${end_month}
    day=${end_day}
    hr=${end_hr}

    current_day=${year}${month}${day}

done

