#!/bin/ksh

# evaluation step 2
# interpolate forecasts to obs locations
# interpolate model geopt to obs locations
# output parallel obs and forecasts and heights
# need to rerun for dots for all tests lower than and including
# 435


sim=test_441
ftype='fcst' #'fcst' #'anal'

daystoskip='05' #for dot output

mapfile=./mapfile_ravan_60km.input

MAINDIR=/scratch1/portfolios/BMC/chem-var/pagowski/enkf_runs

EXECS=${MAINDIR}/execs
EXECS=.

var=PM2_5_DRY

indirfcst=${MAINDIR}/eval/${sim}

inobs=${MAINDIR}/obs_data_gfs/gsi_pm2_5_2012_all.txt

infcst=${indirfcst}/${var}

outfile=${indirfcst}/obs_${ftype}_${var}.txt

${EXECS}/interp_6hrfcst2obs_enkf.x $mapfile $var $inobs $infcst ${infcst}/geopt_bin $outfile $daystoskip

. ./sort.sh ${outfile}.00z ${outfile}.00z.sort
. ./sort.sh ${outfile}.06z ${outfile}.06z.sort
. ./sort.sh ${outfile}.12z ${outfile}.12z.sort
. ./sort.sh ${outfile}.18z ${outfile}.18z.sort
