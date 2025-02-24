#!/bin/ksh

#to add  total seas and dust to CAMS species

. /etc/profile

. ../.environ.ksh

start_date=2016050100
end_date=2016053118

cycle_frequency=6

grid=C192

maindir=/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/OBS/NNR_003_6Targets

indir=${maindir}/MOD04/Y2016/M05_thinned_${grid}
outdir=${maindir}/MOD04/Y2016/M05_thinned_${grid}_corrected

satellite=nnr_terra
#satellite=nnr_aqua

ndate=~/bin/ndate

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hr=`echo "${ident}" | cut -c9-10`

    infile=${indir}/${satellite}.${ident}.nc

    outfile=${outdir}/${satellite}.${ident}.nc

    if [[ ! -r ${outdir} ]]
    then
	mkdir -p ${outdir}
    fi

    echo $outfile

    ncrename -O -v surface_type@MetaData,surface_type_at_MetaData $infile ${outdir}/tmp0

    ncap2 -O -s "where(surface_type_at_MetaData == 2) surface_type_at_MetaData=1; where(surface_type_at_MetaData == 4) surface_type_at_MetaData=2" ${outdir}/tmp0 ${outdir}/tmp1

    ncrename -O -v surface_type_at_MetaData,surface_type@MetaData ${outdir}/tmp1 ${outfile}

    rm -f ${outdir}/tmp[0-1]

    ncatted -O -a history,global,d,, $outfile

    ident=`$ndate +${cycle_frequency} $ident`

done
