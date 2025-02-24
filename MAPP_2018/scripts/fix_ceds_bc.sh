#!/bin/ksh

cd /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/DATA/EMISSIONS/UFS-Aerosols/CEDS_orig

start_date=2017101200
end_date=2017101200

cycle_frequency=24

ndate=~/bin/ndate

ident=$start_date

while [[ $ident -le $end_date ]]
do
    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`

    file=CEDS.2019.emis.${year}${month}${day}.nc

    hh=0
    while [[ $hh -le 23 ]]
    do
	hour=`printf %02i $hh`	
	newfile=../CEDS/CEDS.2019.emis.${year}${month}${day}t${hour}:00:00z.nc
	ncap2 -O -s "time=time+$hh;BC=float($hh*BC);BC_elev=float($hh*BC_elev)" $file $newfile
	((hh=hh+1))
	echo $hour
    done

    echo $file

    ident=`$ndate $cycle_frequency $ident`

    echo $ident

done
