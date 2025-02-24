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

/bin/cp ${EXECDIR}/standalone_stochy_weights.x .

if [ ! -r OUTPUT ]
then
    mkdir -p OUTPUT
fi

INDIR=${TESTDIR}/INPUT
OUTDIR=${TESTDIR}/OUTPUT

ndate=~/bin/ndate

current_date=2020060106
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

fnamein_pattern="${OUTDIR}/PATTERN_weights.${yyyymmdd}t${hh}:00:00z.nc"
fnameout_pattern="${OUTDIR}/PATTERN_weights.${yyyymmddp}t${hhp}:00:00z.nc"

cat > input.nml <<EOF
&chem_io
  cdate='${current_date}'
  fnamein_prefix='${INDIR}/weights'
  fnameout_prefix='${OUTDIR}/weights.'
  varlist='weights'
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

./standalone_stochy_weights.x

