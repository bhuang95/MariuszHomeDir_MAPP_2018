#!/bin/ksh

ndate=~/bin/ndate

start_date=2017113018
end_date=2018011006

start_date=2020053118
end_date=2020071006

cycle_frequency=6

outdir=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/DATA/OBS/AERONET/AERONET_ALM20

if [[ ! -r $outdir ]]
then
    mkdir -p $outdir
fi

jedimod=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/bhuang/JEDI-2020/JEDI-FV3/expCodes/ioda-bundle/jedi_module_base.hera.sh
pycmd_jedi=/scratch1/NCEPDEV/global/spack-stack/apps/miniconda/py39_4.12.0/bin/python3.9

. ${jedimod}

IODABULT=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/bhuang/JEDI-2020/JEDI-FV3/expCodes/ioda-bundle/ioda-bundle-20230809/build
export PYTHONPATH=${PYTHONPATH}:${IODABULT}/lib/
export PYTHONPATH=${PYTHONPATH}:${IODABULT}/lib/python3.9/
export PYTHONPATH=${PYTHONPATH}:${IODABULT}/iodaconv/src
export PYTHONPATH=${PYTHONPATH}:/home/Bo.Huang/JEDI-2020/miscScripts-home/JEDI-Support/aeronetScript/readAeronet/lib-python/

# -t: center time of AERONET AOD
# -w: Time wihdonws in odd or enven hour around center time [-0.5*window, 0.5*window]
# -q: AERONET QOD QC level (ALM15 or ALM20)
# -o: output file name

PYEXE=aeronet_alm2ioda_IODAv3.py
WIN=$cycle_frequency
ALMQC="ALM20"

ident=$start_date
while [[ $ident -le $end_date ]]
do

    CDATE=$ident

    ${pycmd_jedi} ${PYEXE}  -t ${CDATE} -w ${WIN} -o ${outdir}/aeronet_alm.${CDATE}_v3.nc -l ${ALMQC}

    ident=`$ndate $cycle_frequency $ident`

done
