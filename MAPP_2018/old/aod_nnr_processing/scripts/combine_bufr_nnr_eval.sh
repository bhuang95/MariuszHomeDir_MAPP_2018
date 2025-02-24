#!/bin/ksh

codedir=/scratch1/portfolios/BMC/chem-var/pagowski/codes/src

aoddir=/scratch1/portfolios/BMC/chem-var/pagowski/aod/aod_nnr

cd ${aoddir}/scripts

aqua=MYD04
terra=MOD04

indir_aqua=${aoddir}/outdata/${aqua}
indir_terra=${aoddir}/outdata/${terra}

outdir=${aoddir}/outdata/aqua_terra_eval

if [[ ! -r $outdir ]]
then
    mkdir -p $outdir
fi

. ~/bin/funcs.sh

prefix='Aqua_Terra_AOD_nnr_BUFR'

prefix_aqua_land=nnr_002.${aqua}_L2a.land
prefix_aqua_ocean=nnr_002.${aqua}_L2a.ocean

prefix_terra_land=nnr_002.${terra}_L2a.land
prefix_terra_ocean=nnr_002.${terra}_L2a.ocean

start_ident=2012060100
end_ident=2012090100

cycle_frequency=6
file_frequency=3

ident=$start_ident

while [[ $ident -le $end_ident ]]
do

    echo $ident

    year=`echo $ident | cut -c1-4`
    month=`echo $ident | cut -c5-6`
    day=`echo $ident | cut -c7-8`
    hr=`echo $ident | cut -c9-10`

    aqua_l=${indir_aqua}/${prefix_aqua_land}.${year}${month}${day}_${hr}00z.bufr
    aqua_o=${indir_aqua}/${prefix_aqua_ocean}.${year}${month}${day}_${hr}00z.bufr

    terra_l=${indir_terra}/${prefix_terra_land}.${year}${month}${day}_${hr}00z.bufr
    terra_o=${indir_terra}/${prefix_terra_ocean}.${year}${month}${day}_${hr}00z.bufr

    echo $aqua_l > fort.55
    echo $aqua_o >> fort.55
    echo $terra_l >> fort.55
    echo $terra_o >> fort.55

    increment_date -$file_frequency

    aqua_l=${indir_aqua}/${prefix_aqua_land}.${end_year}${end_month}${end_day}_${end_hr}00z.bufr
    aqua_o=${indir_aqua}/${prefix_aqua_ocean}.${end_year}${end_month}${end_day}_${end_hr}00z.bufr

    terra_l=${indir_terra}/${prefix_terra_land}.${end_year}${end_month}${end_day}_${end_hr}00z.bufr
    terra_o=${indir_terra}/${prefix_terra_ocean}.${end_year}${end_month}${end_day}_${end_hr}00z.bufr

    echo $aqua_l >> fort.55
    echo $aqua_o >> fort.55
    echo $terra_l >> fort.55
    echo $terra_o >> fort.55

    increment_date $file_frequency

    aqua_l=${indir_aqua}/${prefix_aqua_land}.${end_year}${end_month}${end_day}_${end_hr}00z.bufr
    aqua_o=${indir_aqua}/${prefix_aqua_ocean}.${end_year}${end_month}${end_day}_${end_hr}00z.bufr

    terra_l=${indir_terra}/${prefix_terra_land}.${end_year}${end_month}${end_day}_${end_hr}00z.bufr
    terra_o=${indir_terra}/${prefix_terra_ocean}.${end_year}${end_month}${end_day}_${end_hr}00z.bufr

    echo $aqua_l >> fort.55
    echo $aqua_o >> fort.55
    echo $terra_l >> fort.55
    echo $terra_o >> fort.55

    ((frequency=2*file_frequency))

    increment_date $frequency

    aqua_l=${indir_aqua}/${prefix_aqua_land}.${end_year}${end_month}${end_day}_${end_hr}00z.bufr
    aqua_o=${indir_aqua}/${prefix_aqua_ocean}.${end_year}${end_month}${end_day}_${end_hr}00z.bufr

    terra_l=${indir_terra}/${prefix_terra_land}.${end_year}${end_month}${end_day}_${end_hr}00z.bufr
    terra_o=${indir_terra}/${prefix_terra_ocean}.${end_year}${end_month}${end_day}_${end_hr}00z.bufr

    echo $aqua_l >> fort.55
    echo $aqua_o >> fort.55
    echo $terra_l >> fort.55
    echo $terra_o >> fort.55

    ((frequency=3*file_frequency))

    increment_date $frequency

    aqua_l=${indir_aqua}/${prefix_aqua_land}.${end_year}${end_month}${end_day}_${end_hr}00z.bufr
    aqua_o=${indir_aqua}/${prefix_aqua_ocean}.${end_year}${end_month}${end_day}_${end_hr}00z.bufr

    terra_l=${indir_terra}/${prefix_terra_land}.${end_year}${end_month}${end_day}_${end_hr}00z.bufr
    terra_o=${indir_terra}/${prefix_terra_ocean}.${end_year}${end_month}${end_day}_${end_hr}00z.bufr

    echo $aqua_l >> fort.55
    echo $aqua_o >> fort.55
    echo $terra_l >> fort.55
    echo $terra_o >> fort.55

    ((frequency=4*file_frequency))

    increment_date $frequency

    aqua_l=${indir_aqua}/${prefix_aqua_land}.${end_year}${end_month}${end_day}_${end_hr}00z.bufr
    aqua_o=${indir_aqua}/${prefix_aqua_ocean}.${end_year}${end_month}${end_day}_${end_hr}00z.bufr

    terra_l=${indir_terra}/${prefix_terra_land}.${end_year}${end_month}${end_day}_${end_hr}00z.bufr
    terra_o=${indir_terra}/${prefix_terra_ocean}.${end_year}${end_month}${end_day}_${end_hr}00z.bufr

    echo $aqua_l >> fort.55
    echo $aqua_o >> fort.55
    echo $terra_l >> fort.55
    echo $terra_o >> fort.55

    ${codedir}/combfr.x 

    /bin/mv fort.50 ${outdir}/${prefix}:${year}-${month}-${day}_${hr}:00:00

    increment_date $cycle_frequency

    ident=${end_year}${end_month}${end_day}${end_hr}

done

