#!/bin/ksh
#SBATCH -n 1
#SBATCH -t 00:30:00
#SBATCH -q debug
##SBATCH -q batch
#SBATCH -A wrf-chem
#SBATCH -J obs_clim
#SBATCH -D /home/Mariusz.Pagowski/mapp_2018/scripts
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j


. /etc/profile

start_date_fcst=2020060800
end_date_fcst=2020063000
climdate=june_2012-2019

start_date_fcst=2017120800
end_date_fcst=2017123100
climdate=dec_2012-2019
#climdate=dec_2012-2020_excl_2017

cycle_frequency=24

ndate=~/bin/ndate
MAINDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski
EXPDIR=/scratch2/BMC/gsd-fv3-dev/MAPP_2018/pagowski/expRuns/UFS-Aerosols_RETcyc
EXECDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/exec

scriptdir=~/mapp_2018/scripts

workdir=${MAINDIR}/tmpdir/workdir_aod_clim

CLIMDIR=${MAINDIR}/DATA/OBS/VIIRS/AWS_gridded_monthly
OBSDIR=${MAINDIR}/DATA/OBS/VIIRS/thinned_debiased_C192
OUTDIR=${EXPDIR}/clim_obs_${climdate}

ndate=~/bin/ndate

if [[ ! -r $workdir ]]
then
    mkdir -p ${workdir}
fi

cd $workdir

. ~/MAPP_2018/.environ.ksh

ident=$start_date_fcst

while [[ $ident -le $end_date_fcst ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hour=`echo "${ident}" | cut -c9-10`
    yyyymmdd=${year}${month}${day}

    for fcsthour in `seq 0 6 120`
    do

	cfcsthour=`printf %03i $fcsthour`

	fcstdate=`$ndate $fcsthour ${ident}`
	fyear=`echo "${fcstdate}" | cut -c1-4`
	fmonth=`echo "${fcstdate}" | cut -c5-6`
	fday=`echo "${fcstdate}" | cut -c7-8`
	fhour=`echo "${fcstdate}" | cut -c9-10`
	
	fyyyymmdd=${fyear}${fmonth}${fday}
	fyyyymmddhh=${fyear}${fmonth}${fday}${fhour}
	datefv3=${fyyyymmdd}.${fhour}0000
	
	obsdir=${OBSDIR}/${fyyyymmddhh}
	obsfile=${obsdir}/VIIRS_AOD_npp.${fyyyymmddhh}.nc

	outdir=${OUTDIR}/${ident}/fhr${cfcsthour}

	climfile=${CLIMDIR}/viirs_aod_monthly_snpp_0.250_deg_${climdate}.nc
	outfile=${outdir}/VIIRS_AOD_npp_clim_monthly.${fyyyymmddhh}.nc

	if [[ ! -r $outdir ]]
	then
	    mkdir -p $outdir
	fi

	${EXECDIR}/viirsaod_clim_interp2ioda.x ${climfile} ${obsfile} ${outfile}

	echo "finished $ident fhr${cfcsthour}"

    done

    echo "finished $ident for all fcsthours"	

    ident=`$ndate +${cycle_frequency} $ident`

done
