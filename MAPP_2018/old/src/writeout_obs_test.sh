#!/bin/ksh

startdate=2012062000
enddate=2012062100

mapfile=./mapfile_ravan_20km.input

MAINDIR=/scratch1/portfolios/BMC/chem-var/pagowski/enkf_runs

EXECS=.

var=PM2_5_DRY
inobs=${MAINDIR}/obs_data_gfs/gsi_pm2_5_2012_all.txt
outfile=${MAINDIR}/eval/${var}_obs_${startdate}_${enddate}.txt

${EXECS}/writeout_obs.x $mapfile $var $inobs $startdate $enddate $outfile
