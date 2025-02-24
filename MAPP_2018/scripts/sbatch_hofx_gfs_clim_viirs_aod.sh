#!/bin/ksh

#SBATCH -n 6
#SBATCH -t 00:30:00
#SBATCH -q debug
##SBATCH -q batch
#SBATCH -A wrf-chem
#SBATCH -J hofx_clim
#SBATCH -D /home/Mariusz.Pagowski/mapp_2018/scripts
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j


. /etc/profile

nproc=$SLURM_NTASKS

start_date=2017120100
end_date=2017123100
#sim=RET_EP4_FreeRun_NoSPE_YesSfcanl_v14_0dz0dp_1M_C96_201712
#sim=RET_EP4_AeroDA_NoSPE_YesSfcanl_v14_0dz0dp_41M_C96_201712
sim=RET_EP4_AeroDA_YesSPEEnKF_YesSfcanl_v14_0dz0dp_41M_C96_201712

start_date=2020061800
end_date=2020063000

#sim=RET_EP4_FreeRun_NoSPE_YesSfcanl_v15_0dz0dp_1M_C96_202006
#sim=RET_EP4_AeroDA_NoSPE_YesSfcanl_v15_0dz0dp_41M_C96_202006
#sim=RET_EP4_AeroDA_YesSPEEnKF_YesSfcanl_v15_0dz0dp_41M_C96_202006

cycle_frequency=24
half_window=3 # half-window for hofx initial time

ndate=~/bin/ndate
MAINDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski
EXPDIR=/scratch2/BMC/gsd-fv3-dev/MAPP_2018/pagowski/expRuns/UFS-Aerosols_RETcyc

jedidir=${MAINDIR}/jedi
jedicdir=${jedidir}/code/fv3-bundle/fv3-jedi/test/Data
jedibdir=${jedidir}/build/fv3-bundle/bin

scriptdir=~/mapp_2018/scripts

#workdir=${MAINDIR}/tmpdir/workdir_aod_hofx
workdir=/scratch2/BMC/gsd-fv3-dev/MAPP_2018/pagowski/tmpdir/workdir_aod_hofx

OBSDIR=${MAINDIR}/DATA/OBS/VIIRS/thinned_debiased_C192
MODELDIR=${EXPDIR}/${sim}/clim_fcst
OUTDIR=${EXPDIR}/${sim}/clim_monthly_aod_hofx

ndate=~/bin/ndate

if [[ ! -r $workdir ]]
then
    mkdir -p ${workdir}
fi

/bin/cp hofx_gfs_viirs_aod_template.yaml $workdir

cd $workdir

/bin/rm -f hofx_gfs_aod.run*

if [[ ! -r Data ]]
then
    mkdir -p Data
fi

ln -sf ${jedicdir}/fv3files ./Data
ln -sf ${jedicdir}/fieldmetadata ./Data

. ~/.jedi

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hour=`echo "${ident}" | cut -c9-10`
    yyyymmdd=${year}${month}${day}

    for fcsthour in `seq 0 6 120`
    do

	cfcsthour=`printf %03i $fcsthour`

	modeldir=${MODELDIR}
	aoddir=${modeldir}/fhr${cfcsthour}
	
	fcstdate=`$ndate $fcsthour ${ident}`
	fyear=`echo "${fcstdate}" | cut -c1-4`
	fmonth=`echo "${fcstdate}" | cut -c5-6`
	fday=`echo "${fcstdate}" | cut -c7-8`
	fhour=`echo "${fcstdate}" | cut -c9-10`
	
	fyyyymmdd=${fyear}${fmonth}${fday}
	fyyyymmddhh=${fyear}${fmonth}${fday}${fhour}
	datefv3=${fyyyymmdd}.${fhour}0000
	
	aodfiles=fv_aod_fhr${cfcsthour}_mean.res
	
	obsdir=${OBSDIR}/${fyyyymmddhh}
	obsfile=${obsdir}/VIIRS_AOD_npp.${fyyyymmddhh}.nc
	
	mkdir -p Data/obs Data/inputs Data/hofx
	
	/bin/rm -f Data/obs/*.nc  Data/inputs/*.nc Data/inputs/*coupler.res Data/hofx/*nc
	
	identm=`$ndate -$half_window $fcstdate`
	yearm=`echo "${identm}" | cut -c1-4`
	monthm=`echo "${identm}" | cut -c5-6`
	daym=`echo "${identm}" | cut -c7-8`
	hourm=`echo "${identm}" | cut -c9-10`
	
	datestd=$fcstdate
	datecf=${fyear}-${fmonth}-${fday}T${fhour}:00:00Z
	datefv3=${fyear}${fmonth}${fday}.${fhour}0000
	
	datestdm=$identm
	datemcf=${yearm}-${monthm}-${daym}T${hourm}:00:00Z
	datemfv3=${yearm}${monthm}${daym}.${hourm}0000
	
	sed -e "s/datestd/${datestd}/g" -e "s/datecf/${datecf}/g" -e "s/datefv3/${datefv3}/g" -e "s/datemstd/${datemstd}/g" -e "s/datemcf/${datemcf}/g" -e "s/datemfv3/${datemfv3}/g" hofx_gfs_viirs_aod_template.yaml > hofx_gfs_aod.yaml
	
	outdir=${OUTDIR}/${ident}/fhr${cfcsthour}
	
	if [[ ! -r $outdir ]]
	then
	    mkdir -p $outdir
	fi
	
	ln -sf $obsfile  Data/obs

	echo "     2        (Calendar: no_calendar=0, thirty_day_months=1, julian=2, gregorian=3, noleap=4)" > Data/inputs/${datefv3}.coupler.res
	echo "  ${fyear}    ${fmonth}    ${fday}    ${fhour}    00    00   00        Model start time:   year, month, day, hour, minute, second" >> Data/inputs/${datefv3}.coupler.res
	echo "  ${fyear}    ${fmonth}    ${fday}    ${fhour}    00    00   00        Current model time:   year, month, day, hour, minute, second" >> Data/inputs/${datefv3}.coupler.res

	itile=1
	while [[ $itile -le 6 ]]
	do
            ln -sf ${aoddir}/${aodfiles}.tile${itile}.nc Data/inputs/${datefv3}.fv_aod.res.tile${itile}.nc
            ((itile=itile+1))
	done

	srun -n $nproc ${jedibdir}/fv3jedi_hofx_nomodel.x hofx_gfs_aod.yaml hofx_gfs_aod.run
	err=$?

	if [[ $err -ne 0 ]] 
	then
	    echo "hofx failed at $ident fhr${cfcsthour}"
	    exit 1
	fi
	
	/bin/rm hofx_gfs_aod.run*

	/bin/mv Data/hofx/VIIRS_AOD_npp_hofx.${fyyyymmddhh}.nc ${outdir}
	
	echo "finished $ident fhr${cfcsthour}"

    done

    echo "finished $ident for all fcsthours"	

    ident=`$ndate +${cycle_frequency} $ident`

done
