#!/bin/ksh

codedir=/scratch1/portfolios/BMC/chem-var/pagowski/codes/src

aoddir=/scratch1/portfolios/BMC/chem-var/pagowski/aod/modis_aod_hdf2bufr

cd ${aoddir}/../scripts

echo ${aoddir}/../scripts

indir_aqua=${aoddir}/outdata_aqua/2012
indir_terra=${aoddir}/outdata_terra/2012

outdir=${aoddir}/outdata_aqua_terra/2012

if [[ ! -r $outdir ]]
then
    mkdir -p $outdir
fi

. ~/bin/funcs.sh

prefix_aqua='Aqua_AOD_BUFR'
prefix_terra='Terra_AOD_BUFR'
prefix='Aqua_Terra_AOD_BUFR'

start_ident=2012053000
end_ident=2012090100

cycle_frequency=24

ident=$start_ident

while [[ $ident -lt $end_ident ]]
do

    echo $ident

    year=`echo $ident | cut -c1-4`
    month=`echo $ident | cut -c5-6`
    day=`echo $ident | cut -c7-8`
    hr=`echo $ident | cut -c9-10`

    aqua=${indir_aqua}/${prefix_aqua}:${year}-${month}-${day}_${hr}:00:00
    terra=${indir_terra}/${prefix_terra}:${year}-${month}-${day}_${hr}:00:00

    echo $aqua > fort.55
    echo $terra >> fort.55

    ${codedir}/combfr.x 

    /bin/mv fort.50 ${outdir}/${prefix}:${year}-${month}-${day}_${hr}:00:00

    increment_date $cycle_frequency
    ident=${end_year}${end_month}${end_day}${end_hr}

done

