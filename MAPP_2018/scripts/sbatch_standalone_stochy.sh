#!/bin/ksh
#SBATCH --ntasks=10
#SBATCH --cpus-per-task=1
#SBATCH --exclusive
#SBATCH -t 00:30:00
#SBATCH -q debug
#SBATCH -A chem-var
#SBATCH -J stochy
#SBATCH -D /home/Mariusz.Pagowski/mapp_2018/scripts
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j

set -x

nmem=$SLURM_NTASKS
np=$SLURM_CPUS_PER_TASK

STOCHDIR=/home/Mariusz.Pagowski/mapp_2018/src_sppt
EXECDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/exec

TESTDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/tmpdir/stochastic_tests

cd ${STOCHDIR}

. ./module-setup.sh
module purge
module use $( pwd -P )
module load modules.hera.intel

export LD_LIBRARY_PATH="/home/Mariusz.Pagowski/MAPP_2018/libs/fortran-datetime/lib:${LD_LIBRARY_PATH}"

cd ${TESTDIR}

/bin/cp ${EXECDIR}/standalone_stochy_chem.x ${EXECDIR}/standalone_stochy_chem_modified.x .

if [ ! -r OUTPUT ]
then
    mkdir -p OUTPUT
fi

INDIR=${TESTDIR}/INPUT
OUTDIR=${TESTDIR}/OUTPUT

ndate=~/bin/ndate

current_date=2017100106
yyyy=`echo $current_date | cut -c1-4`
mm=`echo $current_date | cut -c5-6`
dd=`echo $current_date | cut -c7-8`
hh=`echo $current_date | cut -c9-10`
yyyymmdd=`echo $current_date | cut -c1-8`


#use full hours (for now)
ocnsppt_tau=6
tstep=1
fcst_length=6

end_date=`$ndate $fcst_length $current_date`
yyyyp=`echo $end_date | cut -c1-4`
mmp=`echo $end_date | cut -c5-6`
ddp=`echo $end_date | cut -c7-8`
hhp=`echo $end_date | cut -c9-10`
yyyymmddp=`echo $end_date | cut -c1-8`

delay_time=6 # time to decorrelate initial emissions between members
output_interval=1
nx_fixed=180
ny_fixed=90
sppt_interpolate=".T."

#if stochini=".T." no need for spin-up so delay_time can be set to 0
stochini=".F."
write_stoch_pattern=".T."

((ocnsppt_tau=ocnsppt_tau*3600.))
((tstep=tstep*3600.))
((fcst_length=fcst_length*3600.))
((delay_time=delay_time*3600.))
((output_interval=output_interval*3600.))

bfactor=1. # bias multiplier
cv=1.  # coefficient of variation - should be in the range [0-1]
#fnamein_pattern="null"
#fnameout_pattern="null"

mem=1
while [[ $mem -le $nmem ]]
do

    ((seed=mem+current_date))

    cmem=mem`printf %03i $mem`

    outdir=${OUTDIR}/${cmem}

    if [[ ! -r $outdir ]]
    then
	mkdir -p $outdir
    fi

    cd ${outdir}
    
    fnamein_pattern="${outdir}/PATTERN.${yyyymmdd}t${hh}:00:00z.nc"
    fnameout_pattern="${outdir}/PATTERN.${yyyymmddp}t${hhp}:00:00z.nc"

    cat > input.nml <<EOF
&chem_io
  cdate='${current_date}'
  fnamein_prefix='${INDIR}/CEDS.2019.emis.${yyyymmdd}'
  fnameout_prefix='${outdir}/CEDS.2019.emis.'
  varlist='BC','OC','SO2','BC_elev','OC_elev','SO2_elev','BC_ship','OC_ship','SO2_ship','SO4_ship','NH3_oc','NH3_tr','NH3_re','NH3_in','NH3_ag'
  tstep=${tstep}
  fcst_length=${fcst_length}
  delay_time=${delay_time}
  output_interval=${output_interval}
  nx_fixed=${nx_fixed}
  ny_fixed=${ny_fixed}
  sppt_interpolate=${sppt_interpolate}
  cv=${cv}
  bfactor=${bfactor}
  write_stoch_pattern=${write_stoch_pattern}
  fnameout_pattern='${fnameout_pattern}'
/
&nam_stochy
  stochini=${stochini}
  ocnsppt=1.0, !0.4,0.1
  ocnsppt_lscale=500000, !1000000,2000000
  ocnsppt_tau=${ocnsppt_tau}, !86400,216000
  iseed_ocnsppt=${seed}
/
&nam_sfcperts
/
&nam_sppperts
/
&chem_stoch
  do_sppt=.true.
/

EOF

#INPUT/ocn_stoch.res.nc is hardcoded in inherited code

    if [[ $stochini == ".T." ]]
    then
#needs to be changed for $stochini == ".T."
	if [[ -r ${fnamein_pattern} ]]
	then
            ln -sf ${fnamein_pattern} ./INPUT/ocn_stoch.res.nc
	else
            echo "Mandatory input ./INPUT/ocn_stoch.res.nc required"
            exit 1
	fi
    fi

    srun -n $np ${EXECDIR}/standalone_stochy_chem_modified.x &

    ((mem=mem+1))

done

wait

write_stoch_pattern=.F.

mem=1
while [[ $mem -le $nmem ]]
do

    ((seed=mem+current_date))

    cmem=mem`printf %03i $mem`

    outdir=${OUTDIR}/${cmem}

    if [[ ! -r $outdir ]]
    then
	mkdir -p $outdir
    fi

    cd ${outdir}

cat > input.nml <<EOF
&chem_io
  cdate='${current_date}'
  fnamein_prefix='${INDIR}/GBBEPx_all01GRID.emissions_v004_${yyyymmdd}'
  fnameout_prefix='${outdir}/GBBEPx_all01GRID.emissions_v004_'
  varlist='BC','OC','CO','SO2','NH3','PM2.5','MeanFRP'
  tstep=${tstep}
  fcst_length=${fcst_length}
  delay_time=${delay_time}
  output_interval=${output_interval}
  nx_fixed=${nx_fixed}
  ny_fixed=${ny_fixed}
  sppt_interpolate=${sppt_interpolate}
  fillvalue_correct=.T.
  cv=${cv}
  bfactor=${bfactor}
  write_stoch_pattern=${write_stoch_pattern}
/
&nam_stochy
  stochini=${stochini}
  ocnsppt=1.0, !0.4,0.1
  ocnsppt_lscale=500000, !1000000,2000000
  ocnsppt_tau=${ocnsppt_tau}, !86400,216000
  iseed_ocnsppt=${seed}
/
&nam_sfcperts
/
&nam_sppperts
/
&chem_stoch
  do_sppt=.true.
/

EOF

#INPUT/ocn_stoch.res.nc is hardcoded in inherited code

    if [[ $stochini == ".T." ]]
    then
#needs to be changed for $stochini == ".T."
	if [[ -r ${fnamein_pattern} ]]
	then
            ln -sf ${fnamein_pattern} ./INPUT/ocn_stoch.res.nc
	else
            echo "Mandatory input ./INPUT/ocn_stoch.res.nc required"
            exit 1
	fi
    fi
    
    srun -n ${np} ${EXECDIR}/standalone_stochy_chem_modified.x &

    ((mem=mem+1))

done

wait

cv_ad=0.25
cv_clay=0.25
cv_sand=0.25
cv_ut=0.25
cv_sep=0.25

mem=1
while [[ $mem -le $nmem ]]
do

    ((seed=mem+current_date))

    cmem=mem`printf %03i $mem`

    outdir=${OUTDIR}/${cmem}

    if [[ ! -r $outdir ]]
    then
	mkdir -p $outdir
    fi

    cd ${outdir}

cat > input.nml <<EOF
&chem_io
  cdate='${current_date}'
  fnamein_prefix='${INDIR}/FENGSHA_2022_NESDIS_inputs_10km_v3.2.2018${mm}01'
  fnameout_prefix='${outdir}/FENGSHA_2022_NESDIS_inputs_10km_v3.2.'
  varlist='albedo_drag','clayfrac','sandfrac','uthres','sep'
  tstep=${tstep}
  fcst_length=${fcst_length}
  delay_time=${delay_time}
  output_interval=${output_interval}
  nx_fixed=${nx_fixed}
  ny_fixed=${ny_fixed}
  sppt_interpolate=${sppt_interpolate}
  fillvalue_correct=.T.
  cv_ad=${cv_ad}
  cv_clay=${cv_clay}
  cv_sand=${cv_sand}
  cv_ut=${cv_ut}
  cv_sep=${cv_sep}
  write_stoch_pattern=${write_stoch_pattern}
  fnameout_pattern='${fnameout_pattern}'
/
&nam_stochy
  stochini=${stochini}
  ocnsppt=1.0, !0.4,0.1
  ocnsppt_lscale=500000, !1000000,2000000
  ocnsppt_tau=${ocnsppt_tau}, !86400,216000
  iseed_ocnsppt=${seed}
/
&nam_sfcperts
/
&nam_sppperts
/
&chem_stoch
  do_sppt=.true.
/
EOF

#INPUT/ocn_stoch.res.nc is hardcoded in inherited code

    if [[ $stochini == ".T." ]]
    then
#needs to be changed for $stochini == ".T."
	if [[ -r ${fnamein_pattern} ]]
	then
            ln -sf ${fnamein_pattern} ./INPUT/ocn_stoch.res.nc
	else
            echo "Mandatory input ./INPUT/ocn_stoch.res.nc required"
            exit 1
	fi
    fi
    
    srun -n ${np} ${EXECDIR}/standalone_stochy_dust.x &

    ((mem=mem+1))

done

wait


