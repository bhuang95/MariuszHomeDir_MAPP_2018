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

    infile=${indir}/${ident}/viirs_aod_npp_${ident}.nc

    outfile=${outdir}/${ident}/viirs_snpp.${ident}.nc

    if [[ ! -r ${outdir}/${ident} ]]
    then
	mkdir -p ${outdir}/${ident}
    fi

    echo $outfile

    ncrename -O -v surface_type@MetaData,surface_type_at_MetaData $infile ${outdir}/${ident}/tmp0

    ncap2 -O -s "where(surface_type_at_MetaData == 1) surface_type_at_MetaData=3; where(surface_type_at_MetaData == 2) surface_type_at_MetaData=1; where(surface_type_at_MetaData == 3) surface_type_at_MetaData=2" ${outdir}/${ident}/tmp0 ${outdir}/${ident}/tmp1

    ncrename -O -v surface_type_at_MetaData,surface_type@MetaData ${outdir}/${ident}/tmp1 ${outfile}

    rm -f ${outdir}/${ident}/tmp[0-1]

    ncatted -O -a history,global,d,, $outfile

    ident=`$ndate +${cycle_frequency} $ident`

done
