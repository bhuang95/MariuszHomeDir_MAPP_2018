#!/bin/ksh

codedir=/home/Mariusz.Pagowski/codes/src

aoddir=/scratch3/BMC/chem-var/pagowski/aod_nnr_processing

ndate=~/bin/ndate

cd ${aoddir}/scripts

aqua=MYD04
terra=MOD04

indir_aqua=${aoddir}/outdata/${aqua}
indir_terra=${aoddir}/outdata/${terra}

outdir=${aoddir}/outdata/aqua_terra

if [[ ! -r $outdir ]]
then
    mkdir -p $outdir
fi

prefix='Aqua_Terra_AOD_nnr_BUFR'

prefix_aqua_land=nnr_003.${aqua}_L2a.land
prefix_aqua_ocean=nnr_003.${aqua}_L2a.ocean
prefix_aqua_deep=nnr_003.${aqua}_L2a.deep

prefix_terra_land=nnr_003.${terra}_L2a.land
prefix_terra_ocean=nnr_003.${terra}_L2a.ocean
prefix_terra_deep=nnr_003.${terra}_L2a.deep

start_ident=2015080100
end_ident=2015090100

cycle_frequency=6
file_frequency=3

ident=$start_ident

while [[ $ident -le $end_ident ]]
do

    echo $ident

    year=`echo $ident | cut -c1-4`
    month=`echo $ident | cut -c5-6`
    day=`echo $ident | cut -c7-8`
    hour=`echo $ident | cut -c9-10`

#-6
    newident=`$ndate -$cycle_frequency $ident`
    echo $newident

    dident=`echo $newident | cut -c1-8`
    hr=`echo $newident | cut -c9-10`

    aqua_l=${indir_aqua}/${prefix_aqua_land}.${dident}_${hr}00z.bufr
    aqua_o=${indir_aqua}/${prefix_aqua_ocean}.${dident}_${hr}00z.bufr
    aqua_d=${indir_aqua}/${prefix_aqua_deep}.${dident}_${hr}00z.bufr

    terra_l=${indir_terra}/${prefix_terra_land}.${dident}_${hr}00z.bufr
    terra_o=${indir_terra}/${prefix_terra_ocean}.${dident}_${hr}00z.bufr
    terra_d=${indir_terra}/${prefix_terra_deep}.${dident}_${hr}00z.bufr

    echo $aqua_l > fort.55
    echo $aqua_o >> fort.55
    echo $aqua_d >> fort.55
    echo $terra_l >> fort.55
    echo $terra_o >> fort.55
    echo $terra_d >> fort.55


#-3
    newident=`$ndate -$file_frequency $ident`
    echo $newident

    dident=`echo $newident | cut -c1-8`
    hr=`echo $newident | cut -c9-10`

    aqua_l=${indir_aqua}/${prefix_aqua_land}.${dident}_${hr}00z.bufr
    aqua_o=${indir_aqua}/${prefix_aqua_ocean}.${dident}_${hr}00z.bufr
    aqua_d=${indir_aqua}/${prefix_aqua_deep}.${dident}_${hr}00z.bufr

    terra_l=${indir_terra}/${prefix_terra_land}.${dident}_${hr}00z.bufr
    terra_o=${indir_terra}/${prefix_terra_ocean}.${dident}_${hr}00z.bufr
    terra_d=${indir_terra}/${prefix_terra_deep}.${dident}_${hr}00z.bufr

    echo $aqua_l >> fort.55
    echo $aqua_o >> fort.55
    echo $aqua_d >> fort.55
    echo $terra_l >> fort.55
    echo $terra_o >> fort.55
    echo $terra_d >> fort.55

#0
    newident=$ident
    echo $newident

    dident=`echo $newident | cut -c1-8`
    hr=`echo $newident | cut -c9-10`

    aqua_l=${indir_aqua}/${prefix_aqua_land}.${dident}_${hr}00z.bufr
    aqua_o=${indir_aqua}/${prefix_aqua_ocean}.${dident}_${hr}00z.bufr
    aqua_d=${indir_aqua}/${prefix_aqua_deep}.${dident}_${hr}00z.bufr

    terra_l=${indir_terra}/${prefix_terra_land}.${dident}_${hr}00z.bufr
    terra_o=${indir_terra}/${prefix_terra_ocean}.${dident}_${hr}00z.bufr
    terra_d=${indir_terra}/${prefix_terra_deep}.${dident}_${hr}00z.bufr

    echo $aqua_l >> fort.55
    echo $aqua_o >> fort.55
    echo $aqua_d >> fort.55
    echo $terra_l >> fort.55
    echo $terra_o >> fort.55
    echo $terra_d >> fort.55

#+3
    newident=`$ndate +$file_frequency $ident`
    echo $newident

    dident=`echo $newident | cut -c1-8`
    hr=`echo $newident | cut -c9-10`

    aqua_l=${indir_aqua}/${prefix_aqua_land}.${dident}_${hr}00z.bufr
    aqua_o=${indir_aqua}/${prefix_aqua_ocean}.${dident}_${hr}00z.bufr
    aqua_d=${indir_aqua}/${prefix_aqua_deep}.${dident}_${hr}00z.bufr

    terra_l=${indir_terra}/${prefix_terra_land}.${dident}_${hr}00z.bufr
    terra_o=${indir_terra}/${prefix_terra_ocean}.${dident}_${hr}00z.bufr
    terra_d=${indir_terra}/${prefix_terra_deep}.${dident}_${hr}00z.bufr

    echo $aqua_l >> fort.55
    echo $aqua_o >> fort.55
    echo $aqua_d >> fort.55
    echo $terra_l >> fort.55
    echo $terra_o >> fort.55
    echo $terra_d >> fort.55

    ${codedir}/combfr.x 

    /bin/mv fort.50 ${outdir}/${prefix}:${year}${month}${day}${hour}

    ident=`$ndate +$cycle_frequency $ident`

done

