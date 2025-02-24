#!/bin/ksh

set -x

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

fnamein_pattern="${OUTDIR}/PATTERN_CEDS.${yyyymmdd}t${hh}:00:00z.nc"
fnameout_pattern="${OUTDIR}/PATTERN_CEDS.${yyyymmddp}t${hhp}:00:00z.nc"

cat > input.nml <<EOF
&chem_io
  cdate='${current_date}'
  fnamein_prefix='${INDIR}/CEDS.2019.emis.${yyyymmdd}'
  fnameout_prefix='${OUTDIR}/CEDS.2019.emis.'
  varlist='BC','OC','SO2','BC_elev','OC_elev','SO2_elev','BC_ship','OC_ship','SO2_ship','SO4_ship','NH3_oc','NH3_tr','NH3_re','NH3_in','NH3_ag'
  tstep=${tstep}
  fcst_length=${fcst_length}
  delay_time=${delay_time}
  output_interval=${output_interval}
  nx_fixed=${nx_fixed}
  ny_fixed=${ny_fixed}
  sppt_interpolate=${sppt_interpolate}
  write_stoch_pattern=${write_stoch_pattern}
  fnameout_pattern='${fnameout_pattern}'
/
&nam_stochy
  stochini=${stochini}
  ocnsppt=1.0, !0.4,0.1
  ocnsppt_lscale=500000, !1000000,2000000
  ocnsppt_tau=${ocnsppt_tau}, !86400,216000
  iseed_ocnsppt=3
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
    if [[ -r ${fnamein_pattern} ]]
    then
        ln -sf ${fnamein_pattern} ./INPUT/ocn_stoch.res.nc
    else
        echo "Mandatory input ./INPUT/ocn_stoch.res.nc required"
        exit 1
    fi
fi

./standalone_stochy_chem.x

fnamein_pattern="${OUTDIR}/PATTERN_GBBEPx.${yyyymmdd}t${hh}:00:00z.nc"
fnameout_pattern="${OUTDIR}/PATTERN_GBBEPx.${yyyymmddp}t${hhp}:00:00z.nc"

cat > input.nml <<EOF
&chem_io
  cdate='${current_date}'
  fnamein_prefix='${INDIR}/GBBEPx_all01GRID.emissions_v004_${yyyymmdd}'
  fnameout_prefix='${OUTDIR}/GBBEPx_all01GRID.emissions_v004_'
  varlist='BC','OC','CO','SO2','NH3','PM2.5','MeanFRP'
  tstep=${tstep}
  fcst_length=${fcst_length}
  delay_time=${delay_time}
  output_interval=${output_interval}
  nx_fixed=${nx_fixed}
  ny_fixed=${ny_fixed}
  sppt_interpolate=${sppt_interpolate}
  fillvalue_correct=.T.
  write_stoch_pattern=${write_stoch_pattern}
  fnameout_pattern='${fnameout_pattern}'
/
&nam_stochy
  stochini=${stochini}
  ocnsppt=1.0, !0.4,0.1
  ocnsppt_lscale=500000, !1000000,2000000
  ocnsppt_tau=${ocnsppt_tau}, !86400,216000
  iseed_ocnsppt=3
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
    if [[ -r ${fnamein_pattern} ]]
    then
        ln -sf ${fnamein_pattern} ./INPUT/ocn_stoch.res.nc
    else
        echo "Mandatory input ./INPUT/ocn_stoch.res.nc required"
        exit 1
    fi
fi

./standalone_stochy_chem.x

bfactor=2. # bias multiplier
cv=1.  # coefficient of variation - should be in the range [0-1]
stochini=.T.
#fnamein_pattern="null"
write_stoch_pattern=.T.
#fnameout_pattern="null"

cat > input.nml <<EOF
&chem_io
  cdate='${current_date}'
  fnamein_prefix='${INDIR}/CEDS.2019.emis.${yyyymmdd}'
  fnameout_prefix='${OUTDIR}/CEDS.2019.emis_modified.'
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
  iseed_ocnsppt=3
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
    if [[ -r ${fnamein_pattern} ]]
    then
        ln -sf ${fnamein_pattern} ./INPUT/ocn_stoch.res.nc
    else
        echo "Mandatory input ./INPUT/ocn_stoch.res.nc required"
        exit 1
    fi
fi

./standalone_stochy_chem_modified.x

cat > input.nml <<EOF
&chem_io
  cdate='${current_date}'
  fnamein_prefix='${INDIR}/GBBEPx_all01GRID.emissions_v004_${yyyymmdd}'
  fnameout_prefix='${OUTDIR}/GBBEPx_all01GRID.emissions_v004_modified_'
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
  iseed_ocnsppt=3
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
    if [[ -r ${fnamein_pattern} ]]
    then
        ln -sf ${fnamein_pattern} ./INPUT/ocn_stoch.res.nc
    else
        echo "Mandatory input ./INPUT/ocn_stoch.res.nc required"
        exit 1
    fi
fi

./standalone_stochy_chem_modified.x


