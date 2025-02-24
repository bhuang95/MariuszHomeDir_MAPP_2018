#!/bin/ksh

#to add  total seas and dust to CAMS species

. /etc/profile

. ~/.nc

start_date=2016060100
end_date=2016063018

cycle_frequency=24

rhfactor=4.3

maindir=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski

camsdir=/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/MODEL/cams
indir=${camsdir}/pll_orig
outdir=${camsdir}/pll

if [[ ! -r $outdir ]]
then
    mkdir -p $outdir
fi


ndate=~/bin/ndate

cd $outdir

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hr=`echo "${ident}" | cut -c9-10`

    infile=${indir}/"cams_aods_${year}${month}${day}.nc"

    outfile="cams_aods_${year}${month}${day}_ll.nc"

    ln -sf $infile $outfile

    ident=`$ndate +${cycle_frequency} $ident`

done    

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hr=`echo "${ident}" | cut -c9-10`

    infile=${indir}/"cams_aeros_${year}${month}${day}.nc"

    outfile=${outdir}/"cams_aeros_${year}${month}${day}_pll.nc"

    echo $outfile

    ncap2 -O -s "SEASTOTAL=float((aermr01+aermr02+aermr03)/$rhfactor);aermr01=float(aermr01/$rhfactor);aermr02=float(aermr02/$rhfactor);aermr03=float(aermr03/$rhfactor);DUSTTOTAL=float(aermr04+aermr05+aermr06);CPHOBIC=float(aermr08+aermr10);CPHILIC=float(aermr07+aermr09);CTOTAL=float(aermr07+aermr08+aermr09+aermr10)" $infile $outfile

    ncatted -O -a long_name,SEASTOTAL,o,c,"Sea Salt Aerosol Total Mixing Ratio" $outfile
    ncatted -O -a long_name,DUSTTOTAL,o,c,"Dust Aerosol Total Mixing Ratio" $outfile
    ncatted -O -a long_name,CPHOBIC,o,c,"Hydrophobic Carbon Aerosol Total Mixing Ratio" $outfile
    ncatted -O -a long_name,CPHILIC,o,c,"Hydrophilic Carbon Aerosol Total Mixing Ratio" $outfile
    ncatted -O -a long_name,CTOTAL,o,c,"Carbon Aerosol Total Mixing Ratio" $outfile

    ncatted -O -a history,global,d,, $outfile

    ident=`$ndate +${cycle_frequency} $ident`

done
