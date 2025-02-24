#!/bin/ksh

. /etc/profile

sim=RET_EP4_FreeRun_NoSPE_YesSfcanl_v14_0dz0dp_1M_C96_201712
sim=RET_EP4_AeroDA_NoSPE_YesSfcanl_v14_0dz0dp_41M_C96_201712
sim=RET_EP4_AeroDA_YesSPEEnKF_YesSfcanl_v14_0dz0dp_41M_C96_201712
start_date=2017120800
end_date=2017123118

sim=RET_EP4_FreeRun_NoSPE_YesSfcanl_v15_0dz0dp_1M_C96_202006
sim=RET_EP4_AeroDA_NoSPE_YesSfcanl_v15_0dz0dp_41M_C96_202006
sim=RET_EP4_AeroDA_YesSPEEnKF_YesSfcanl_v15_0dz0dp_41M_C96_202006
start_date=2020060800
end_date=2020063018

err=`echo $sim | grep FreeRun`
AeroDA=$?

aeronet_data="ALM20"

cycle_frequency=6
ndate=~/bin/ndate

MAINDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski
EXPDIR=/scratch2/BMC/gsd-fv3-dev/MAPP_2018/pagowski/expRuns/UFS-Aerosols_RETcyc

jedidir=${MAINDIR}/jedi
jediddir=${jedidir}/code/fv3-bundle/fv3-jedi/test/Data
jedibdir=${jedidir}/build/fv3-bundle/bin

scriptdir=~/mapp_2018/scripts

workdir=${MAINDIR}/tmpdir/workdir_aeronet_hofx_alm

OBSDIR=${MAINDIR}/DATA/OBS/AERONET/AERONET_${aeronet_data}
MODELDIR=${EXPDIR}/${sim}
OUTDIR=${EXPDIR}/${sim}/${aeronet_data}_hofx

((cycle_frequency_half=cycle_frequency/2))

if [[ ! -r $workdir ]]
then
    mkdir -p $workdir
fi

cd $workdir

echo ${jediddir}

obsdir=${OBSDIR}
modeldir=${MODELDIR}/gdas_${start_date}-${end_date}
outdir=${OUTDIR}

if [[ ! -r $outdir ]]
then
    mkdir -p $outdir
fi

/bin/cp ${scriptdir}/hofx_gfs_aeronet_alm_template.yaml .

/bin/cp -rP ${jediddir} ${jedidir}/lutsfiles/*.rc . 

/bin/cp ${jedidir}/lutsfiles/*.nc ./Data

. ~/.jedi

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hour=`echo "${ident}" | cut -c9-10`
    
    identm=`$ndate -$cycle_frequency_half $ident`
    yearm=`echo "${identm}" | cut -c1-4`
    monthm=`echo "${identm}" | cut -c5-6`
    daym=`echo "${identm}" | cut -c7-8`
    hourm=`echo "${identm}" | cut -c9-10`

    datestd=$ident
    datecf=${year}-${month}-${day}T${hour}:00:00Z
    datefv3=${year}${month}${day}.${hour}0000

    datestdm=$identm
    datemcf=${yearm}-${monthm}-${daym}T${hourm}:00:00Z
    datemfv3=${yearm}${monthm}${daym}.${hourm}0000
    
    sed -e "s/datestd/${datestd}/g" -e "s/datecf/${datecf}/g" -e "s/datefv3/${datefv3}/g" -e "s/datemstd/${datemstd}/g" -e "s/datemcf/${datemcf}/g"  -e "s/datemfv3/${datemfv3}/g" hofx_gfs_aeronet_alm_template.yaml > hofx_gfs_aeronet_alm.yaml

    obsfile=${obsdir}/aeronet_alm.${ident}_v3.nc

    mkdir -p Data/obs  Data/inputs Data/hofx 

    /bin/rm -f Data/obs/*.nc  Data/inputs/*.nc Data/inputs/*.res  Data/hofx/*nc

    ln -sf $obsfile  Data/obs

    ln -sf ${modeldir}/${datefv3}.coupler.res Data/inputs/${datefv3}.coupler.res
    
    itile=1
    while [[ $itile -le 6 ]]
    do
	ln -sf ${modeldir}/${datefv3}.fv_core.res.tile${itile}.nc Data/inputs/${datefv3}.fv_core.res.tile${itile}.nc

	if [[ ${AeroDA} == 1 ]] 
	then
	    ln -sf ${modeldir}/${datefv3}.fv_tracer_aeroanl.res.tile${itile}.nc Data/inputs/${datefv3}.fv_tracer.res.tile${itile}.nc
	else
	    ln -sf ${modeldir}/${datefv3}.fv_tracer.res.tile${itile}.nc Data/inputs/${datefv3}.fv_tracer.res.tile${itile}.nc
	fi
	((itile=itile+1))
    done

    srun -n 6 ${jedibdir}/fv3jedi_hofx_nomodel.x hofx_gfs_aeronet_alm.yaml hofx_gfs_aero.run
    err=$?

    if [ ${err} -ne 0 ]
    then
	echo "hofx error on $ident"
	exit 1
    fi

    /bin/mv Data/hofx/aeronet_alm_hofx.${ident}.nc Data/hofx/aeronet_aod_hofx.${ident}.nc ${outdir}

    echo "finished $ident"

    ident=`$ndate +${cycle_frequency} $ident`

done
