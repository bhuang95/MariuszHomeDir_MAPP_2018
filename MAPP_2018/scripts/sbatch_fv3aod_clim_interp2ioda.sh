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

start_date=2020060800
end_date=2020063000

sim=RET_EP4_FreeRun_NoSPE_YesSfcanl_v14_0dz0dp_1M_C96_201712
sim=RET_EP4_AeroDA_NoSPE_YesSfcanl_v14_0dz0dp_41M_C96_201712
sim=RET_EP4_AeroDA_YesSPEEnKF_YesSfcanl_v14_0dz0dp_41M_C96_201712

cycle_frequency=24

grid="C96"

MAINDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski
EXPDIR=/scratch2/BMC/gsd-fv3-dev/MAPP_2018/pagowski/expRuns/UFS-Aerosols_RETcyc
EXECDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/exec
MODELDIR=${EXPDIR}/${sim}/clim_fcst

scriptdir=~/mapp_2018/scripts
ndate=~/bin/ndate

workdir=${MAINDIR}/tmpdir/workdir_aod_fv3interp

OBSDIR=${MAINDIR}/DATA/OBS/VIIRS/thinned_debiased_C192
OUTDIR=${EXPDIR}/${sim}/viirs_aod_climfcst

ndate=~/bin/ndate

if [[ ! -r $workdir ]]
then
    mkdir -p ${workdir}
fi

cd $workdir

. ~/MAPP_2018/.environ.ksh

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

	fv3indir=${MODELDIR}/fhr${cfcsthour}

	outdir=${OUTDIR}/${ident}/fhr${cfcsthour}
	outfile=${outdir}/VIIRS_AOD_npp_fcst_mean.${fyyyymmddhh}.nc

	if [[ ! -r $outdir ]]
	then
	    mkdir -p $outdir
	fi

	cat > fv3aod_interp2ioda.nl <<EOF
&record_fv3_input
 date="${fyear}${fmonth}${fday}${fhour}"
 input_grid_dir="${MAINDIR}/fix_fv3/${grid}"
 fname_grid="grid_spec.tile?.nc"
 input_fv3_dir="${fv3indir}"
 fname_fv3="fv_aod_fhr${cfcsthour}_mean.res.tile?.nc"
/
&record_viirs_inout
 fname_viirs_aod_in="${obsfile}"
 fname_viirs_aod_out="${outfile}"
/
EOF

	${EXECDIR}/fv3aod_interp2ioda.x

	echo "finished $ident fhr${cfcsthour}"

    done

    echo "finished $ident for all fcsthours"	

    ident=`$ndate +${cycle_frequency} $ident`

done
