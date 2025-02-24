#!/bin/ksh

#to add  total seas and dust to CAMS species

. /etc/profile

. ../.environ.ksh

start_date=2016060100
end_date=2016063000

cycle_frequency=6

grid=C384

maindir=/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/OBS/VIIRS/AOT

indir=${maindir}/thinned_${grid}
outdir=${maindir}/thinned_${grid}_corrected


ndate=~/bin/ndate

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hr=`echo "${ident}" | cut -c9-10`

    infile=${indir}/${ident}/viirs_snpp.${ident}.nc

    outfile=${outdir}/${ident}/viirs_aod_snpp.${ident}.nc

    if [[ ! -r ${outdir}/${ident} ]]
    then
	mkdir -p ${outdir}/${ident}
    fi

    echo $outfile

    /bin/cp $infile $outfile

    ident=`$ndate +${cycle_frequency} $ident`

done
