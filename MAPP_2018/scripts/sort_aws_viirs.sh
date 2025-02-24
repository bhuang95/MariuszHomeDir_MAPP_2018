#!/bin/ksh 

start_date=2018123100
end_date=2019040100

cycle_frequency=24

ndate=~/bin/ndate

#INDIR=/scratch1/BMC/chem-var/MAPP_2018/bhuang/Data/VIIRS-AWS/data

INDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/DATA/OBS/VIIRS/AWS
OUTDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/DATA/OBS/VIIRS/AOT

ident=$start_date

while [[ ${ident} -le ${end_date} ]]
    do

    echo $ident

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hr=`echo "${ident}" | cut -c9-10`

    outdir=${OUTDIR}/${year}${month}${day}

    if [[ ! -r $outdir ]]
    then
	mkdir -p $outdir
    fi

    cd $outdir
#    ln -sf ${INDIR}/JRR-AOD_v3r0_npp_s${year}${month}${day}*nc .
    /bin/mv ${INDIR}/JRR-AOD_v3r0_npp_s${year}${month}${day}*nc .

    ident=`$ndate $cycle_frequency $ident`

done

