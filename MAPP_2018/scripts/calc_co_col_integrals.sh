#!/bin/ksh

. /etc/profile

. ../.environ.ksh

start_date=2019072818
end_date=2019080918

maindir=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/FV3_CHEM_2019/comrot/pagowski_test_gsdchem_c384_6hr/C96

cycle_frequency=6

ndate=~/bin/ndate

ident=$start_date

mwair=0.0289645
mwco=0.0280101
grav=9.80665

((factor=mwco/mwair/grav*1.e-6))

while [[ $ident -le $end_date ]]
do

    echo $ident

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hr=`echo "${ident}" | cut -c9-10`

    indir=${maindir}/gfs.${year}${month}${day}/${hr}/RESTART

    cd $indir

    ident=`$ndate +${cycle_frequency} $ident`

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hr=`echo "${ident}" | cut -c9-10`

    itile=1

    while [[ $itile -le 6 ]]
    do
	infilecore=${year}${month}${day}.${hr}0000.fv_core.res.tile${itile}.nc
	infiletracer=${year}${month}${day}.${hr}0000.fv_tracer.res.tile${itile}.nc

	ncks -O -v DELP $infilecore dz.nc
	ncks -O -v co $infiletracer co.nc

	ncks -A dz.nc co.nc

	outfile=${year}${month}${day}.${hr}0000.fv_co_column.res.tile${itile}.nc
	ncap2 -O -v -s 'co_column=float(-co*DZ*$factor).total($zaxis_1)' co.nc $outfile

	exit "check if $foctor in ncap2 works corerctly"

	((itile=itile+1))

    done

    rm -f co.nc dz.nc *tmp

done
