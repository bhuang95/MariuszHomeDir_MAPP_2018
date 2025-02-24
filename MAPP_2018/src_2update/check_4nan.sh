#!/bin/ksh

test=DA_ENKF

satellite='MYD04' #aqua
satellite='MOD04' # terra

obstype=deep
#obstype=land
#obstype=ocean

cycle_frequency=6

start_date=2015080500
end_date=2015081712

maindir=/scratch3/BMC/fim/MAPP_2018/OBS/NNR_003_6Targets
indir=${maindir}/4hl_${test}
prefix=nnr_fv3.${satellite}.${obstype}

ndate=~/bin/ndate

ident=$start_date

i=0

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hr=`echo "${ident}" | cut -c9-10`

    infile=${indir}/${prefix}.${year}${month}${day}${hr}.nc

    ncdump -v AOD_model ${infile} | grep '_'
    ncdump -v AOD_model ${infile} | grep '0,'
    ncdump -v AOD_model ${infile} | grep -i 'inf,'

    ((i=i+1))
    echo $i

    ident=`$ndate $cycle_frequency $ident`

done
