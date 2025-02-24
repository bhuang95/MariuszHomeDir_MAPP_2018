#!/bin/ksh

#to add  total seas and dust to CAMS species

. /etc/profile

. ../.environ.ksh

start_date=2016060100
end_date=2016063000

grid=C96

maindir=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski

model=fv3
model=m2

modeldir=/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/MODEL/${model}
indir=${modeldir}/pll
outdir=${modeldir}/pll

if [[ ${model} == 'fv3' ]]
then
    cycle_frequency=6
else
    cycle_frequency=24
fi


ndate=~/bin/ndate

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hr=`echo "${ident}" | cut -c9-10`

    if [[ ${model} == 'fv3' ]]
    then
        datestring=${year}${month}${day}${hr}
    else
        datestring=${year}${month}${day}
    fi

    infile=${indir}/${model}"_aeros_${datestring}_pll.nc"

    /bin/mv $infile ${infile}.tmp

    outfile=${outdir}/${model}"_aeros_${datestring}_pll.nc"

    echo $outfile

    ncap2 -O -s "CPHOBIC=float(BCPHOBIC+OCPHOBIC);CPHILIC=float(BCPHILIC+OCPHILIC);CTOTAL=float(BCPHOBIC+OCPHOBIC+BCPHILIC+OCPHILIC)" ${infile}.tmp $outfile
    ncatted -O -a long_name,CPHOBIC,o,c,"Hydrophobic Carbon Aerosol Total Mixing Ratio" $outfile
    ncatted -O -a long_name,CPHILIC,o,c,"Hydrophilic Carbon Aerosol Total Mixing Ratio" $outfile
    ncatted -O -a long_name,CTOTAL,o,c,"Carbon Aerosol Total Mixing Ratio" $outfile

    ncatted -O -a history,global,d,, $outfile

    rm -f ${infile}.tmp

    ident=`$ndate +${cycle_frequency} $ident`

done
