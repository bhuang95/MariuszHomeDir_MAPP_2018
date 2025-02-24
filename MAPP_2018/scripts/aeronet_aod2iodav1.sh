#!/bin/ksh

. /etc/profile

start_date=2023060100
end_date=2023070100

aodtype="AOD15"

lunar=0

JEDIDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/build/ioda-bundle/bin
JEDICODE=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/code/ioda-bundle

#pyscript=py_aeronet_lunar_aod2ioda_IODAv1.py
pyscript=py_aeronet_lunar_aod2ioda_IODAv1_Inp550nm.py

if [[ $lunar == 0 ]]
then
    type="solar"
else
    type="lunar"
fi

cycle_frequency=1

maindir=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski

workdir=${maindir}/tmpdir/workdir_aeronet

outdir=${maindir}/DATA/OBS/AERONET_${type}_${aodtype}

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

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hour=`echo "${ident}" | cut -c9-10`

    echo $ident

    outfile_v1=aeronet_aod.${ident}_v1.nc
    outfile_v2=aeronet_aod.${ident}_v2.nc
    outfile_v3=${outdir}/aeronet_aod.${ident}.nc

    module purge
    module load intel/2022.1.2
    module use -a /contrib/anaconda/modulefiles
    module load anaconda/latest

    python ${pyscript} -t $ident -w ${cycle_frequency} -l ${lunar} -q ${aodtype} -o ${outfile_v1}

    err=$?
#    if [[ ${err} != '0' ]]
#    then
#	echo "No lunar AERONET at $ident"
#	ident=`$ndate +${cycle_frequency} $ident`
#	continue
#    fi

    . ~/.nc

    ncrename -O -v wavelength@VarMetaData,wavelength@MetaData -v frequency@VarMetaData,frequency@MetaData -v sensor_channel@VarMetaData,sensor_channel@MetaData ${outfile_v1}

    . ~/.ioda

    ${JEDIDIR}/ioda-upgrade-v1-to-v2.x ${outfile_v1} ${outfile_v2}
    ${JEDIDIR}/ioda-upgrade-v2-to-v3.x ${outfile_v2} ${outfile_v3} ${JEDICODE}/ioda/share/ioda/yaml/validation/ObsSpace.yaml

    /bin/rm ${outfile_v1} ${outfile_v2}

    ident=`$ndate +${cycle_frequency} $ident`

done

