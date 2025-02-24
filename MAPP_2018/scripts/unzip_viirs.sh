#!/bin/ksh

indir=/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/DATA/OBS/VIIRS/AOT
indir=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/DATA/OBS/VIIRS/AOT

ndate=~/bin/ndate


start_date=2018080200
end_date=2018100100

cycle_frequency=24

ident=$start_date

while [[ $ident -le $end_date ]]
do

    id=`echo "${ident}" | cut -c1-8`

    cd ${indir}/${id}
    unzip ${ident}00.zip '*_npp_s*.nc' > /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/unzip.log 2>&1 

#    /bin/rm ${ident}00.zip

    ident=`$ndate $cycle_frequency $ident`

done
