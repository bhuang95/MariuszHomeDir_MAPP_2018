#!/bin/ksh

. /etc/profile

. ../.environ.ksh

start_date=2016020400
end_date=2016123100

cycle_frequency=24

grid=C192

maindir=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/GBBEPx
indir=${maindir}/${grid}_nofrp

/bin/rm -f ${maindir}/gbbepx_miss_${grid}.log

ndate=~/bin/ndate

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hour=`echo "${ident}" | cut -c9-10`

    yymmdd=${year}${month}${day}

    echo $yymmdd

    ls -1 ${indir}/${yymmdd}/GBBEPx.bc.${yymmdd}.FV3.${grid}Grid.tile1.bin > /dev/null 2>&1 
    rc=$?

    if [[ $rc > 0 ]]
    then
	echo ${yymmdd} >> ${maindir}/gbbepx_miss_${grid}.log
    fi

    ident=`$ndate +${cycle_frequency} $ident`

done

