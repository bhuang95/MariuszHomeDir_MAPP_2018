#!/bin/ksh

sim=noda_rt
ident=2015071100

year=`echo "${ident}" | cut -c1-4`
month=`echo "${ident}" | cut -c5-6`
day=`echo "${ident}" | cut -c7-8`
hr=`echo "${ident}" | cut -c9-10`

ident_dir=${year}_${month}_${day}_${hr}

daystoskip='00' #for dot output

mapfile=./mapfile_rt.input

MAINDIR=/scratch1/portfolios/BMC/chem-var/pagowski/codes/src_rt

EXECS=.

var=PM2_5_DRY

inobs=${MAINDIR}/eval_rt/outdata/obs_rt/${ident_dir}/${var}_obs.txt
infcstdir=${MAINDIR}/eval_rt/outdata/${sim}/${ident_dir}
fixdir=${MAINDIR}/model_output_rt

outfile=${MAINDIR}/eval_rt/outdata/${sim}/stats/obs_fcst_${var}.txt

${EXECS}/interp_fcst2obs_rt.x ${fixdir}/$mapfile $var $inobs $infcstdir ${fixdir}/geopt.bin $outfile $daystoskip

. ./sort.sh ${outfile} ${outfile}.sort
