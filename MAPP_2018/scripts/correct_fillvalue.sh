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

/bin/cp ${EXECDIR}/correct_fillvalue.x ${EXECDIR}/correct_fillvalue_dust.x .

if [ ! -r OUTPUT ]
then
    mkdir -p OUTPUT
fi

INDIR=${TESTDIR}/INPUT
OUTDIR=${TESTDIR}/OUTPUT

ndate=~/bin/ndate

current_date=2017010106
yyyy=`echo $current_date | cut -c1-4`
mm=`echo $current_date | cut -c5-6`
dd=`echo $current_date | cut -c7-8`
hh=`echo $current_date | cut -c9-10`
yyyymmdd=`echo $current_date | cut -c1-8`


#use full hours (for now)
tstep=1
fcst_length=6

output_interval=1

((tstep=tstep*3600.))
((fcst_length=fcst_length*3600.))
((output_interval=output_interval*3600.))

cat > input.nml <<EOF
&chem_io
  cdate='${current_date}'
  fnamein_prefix='${INDIR}/CEDS.2019.emis.${yyyymmdd}'
  fnameout_prefix='${OUTDIR}/CEDS.2019.emis_control.'
  varlist='BC','OC','SO2','BC_elev','OC_elev','SO2_elev','BC_ship','OC_ship','SO2_ship','SO4_ship','NH3_oc','NH3_tr','NH3_re','NH3_in','NH3_ag'
  tstep=${tstep}
  fcst_length=${fcst_length}
  output_interval=${output_interval}
  fillvalue_correct=.T.
/

EOF

#./correct_fillvalue.x

cat > input.nml <<EOF
&chem_io
  cdate='${current_date}'
  fnamein_prefix='${INDIR}/GBBEPx_all01GRID.emissions_v004_${yyyymmdd}'
  fnameout_prefix='${OUTDIR}/GBBEPx_all01GRID.emissions_v004_control_'
  varlist='BC','OC','CO','SO2','NH3','PM2.5','MeanFRP'
  tstep=${tstep}
  fcst_length=${fcst_length}
  output_interval=${output_interval}
  fillvalue_correct=.T.
/

EOF

#./correct_fillvalue.x

cat > input.nml <<EOF
&chem_io
  cdate='${current_date}'
  fnamein_prefix='${INDIR}/FENGSHA_2022_NESDIS_inputs_10km_v3.2.2018${mm}01'
  fnameout_prefix='${OUTDIR}/FENGSHA_2022_NESDIS_inputs_10km_v3.2_control.'
  varlist='albedo_drag','clayfrac','sandfrac','uthres','sep'
  tstep=${tstep}
  fcst_length=${fcst_length}
  output_interval=${output_interval}
  fillvalue_correct=.T.
/

EOF

./correct_fillvalue_dust.x
