#!/bin/ksh --login
#SBATCH -J get_aeronet
#SBATCH -A chem-var
#SBATCH -n 1
#SBATCH -t 08:00:00
#SBATCH -p service
#SBATCH --mem=5g
#SBATCH -D /home/Mariusz.Pagowski/mapp_2018/scripts
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j


. /etc/profile

start_date=2017100100
end_date=2017110100

start_date=2020053118
end_date=2020070100

start_date=2021123118
end_date=2023010106

aodtype="AOD20"

lunar=0

if [[ $lunar == 0 ]]
then
    type="solar"
else
    type="lunar"
fi

cycle_frequency=6

maindir=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski

workdir=${maindir}/tmpdir/workdir_aeronet

jedimod=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/bhuang/JEDI-2020/JEDI-FV3/expCodes/ioda-bundle/jedi_module_base.hera.sh

pycmd_jedi=/scratch1/NCEPDEV/global/spack-stack/apps/miniconda/py39_4.12.0\/bin/python3.9

. ${jedimod}

IODABULT=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/bhuang/JEDI-2020/JEDI-FV3/expCodes/ioda-bundle/ioda-bundle-20230809/build
export PYTHONPATH=${PYTHONPATH}:${IODABULT}/lib/
export PYTHONPATH=${PYTHONPATH}:${IODABULT}/lib/python3.9/
export PYTHONPATH=${PYTHONPATH}:${IODABULT}/iodaconv/src
export PYTHONPATH=${PYTHONPATH}:/home/Bo.Huang/JEDI-2020/miscScripts-home/JEDI-Support/aeronetScript/readAeronet/lib-python/

pyscript=py_aeronet_lunar_aod2ioda_IODAv3_Intp550nm.py

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

    outfile_v3=${outdir}/aeronet_aod.${ident}.nc

    ${pycmd_jedi} ${pyscript} -t $ident -w ${cycle_frequency} -l ${lunar} -q ${aodtype} -o ${outfile_v3} -p 1

    ident=`$ndate +${cycle_frequency} $ident`

done

