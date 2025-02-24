#!/bin/ksh

cd /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/DATA/EMISSIONS/UFS-Aerosols/GBBEPx_orig

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

    file=GBBEPx_all01GRID.emissions_v004_${year}${month}${day}.nc

    hh=00
    while [[ $hh -le 23 ]]
    do
	hour=`printf %02i $hh`
	newfile=../GBBEPx/GBBEPx_all01GRID.emissions_v004.${year}${month}${day}t${hour}:00:00z.nc
	ncap2 -O -s "time=time+$hh-12;SO2=$hh*SO2;OC=$hh*OC;BC=$hh*BC;NH3=$hh*NH3" $file $newfile
	((hh=hh+1))
	echo $hour
    done

    echo $file

    echo $ident

    ident=`$ndate $cycle_frequency $ident`

done
