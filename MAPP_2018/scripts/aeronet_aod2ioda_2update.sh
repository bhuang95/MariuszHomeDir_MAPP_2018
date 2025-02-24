#!/bin/ksh

. /etc/profile

start_date=2022100100
end_date=2022123123

#start_date=2017100100
#end_date=2017110100

start_date=2023060100
end_date=2023063023
end_date=2023060101

#will only work for AOD15
aod="AOD15"

JEDIDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/build/ioda-bundle/bin
JEDICODE=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/code/ioda-bundle

module use -a /contrib/anaconda/modulefiles
#module load anaconda/latest
module load anaconda/anaconda3-5.3.1

export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${JEDIDIR}/lib/"

#pyscript=aeronet_aod2ioda_IODAv2_Interp550nm.py
pyscript=aeronet_aod2ioda_IODAv1_Interp550nm.py
py_aeronet_lunar_aod2ioda_IODAv1_Inp550nm.py
cycle_frequency=1

maindir=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski

workdir=${maindir}/tmpdir/workdir_aeronet

outdir=${maindir}/DATA/OBS/AERONET_${aod}

if [[ ! -r $outdir ]]
then
    mkdir -p $outdir
fi

ndate=~/bin/ndate

if [[ ! -r $workdir ]]
then
    mkdir -p $workdir
fi

cd $workdir

/bin/cp ~/mapp_2018/scripts/${pyscript} .

. ~/.ioda

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hour=`echo "${ident}" | cut -c9-10`

    echo $ident

    outfile_v2=${outdir}/aeronet_aod.${ident}_v2.nc
    outfile=${outdir}/aeronet_aod.${ident}.nc

    python ${pyscript} -t $ident -w ${cycle_frequency} -o $outfile_v2

    exit

    ${JEDIDIR}/ioda-upgrade-v2-to-v3.x ${outfile_v2} ${outfile} ${JEDICODE}/ioda/share/ioda/yaml/validation/ObsSpace.yaml

    ident=`$ndate +${cycle_frequency} $ident`

    exit

done

