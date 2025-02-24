#!/bin/ksh

# evaluation step 3
# run programs based on parallel obs <-> fcst data
# checks if height of obs and model consistent if below logheight=TRUE
# in evaluation programs if height of obs site unknown assumes
# that obs and model heights consistent
# 1) timeseries of forecasts vs. obs

sim=test_441
logheight=FALSE
ftype='fcst' # 'anal' #'fcst' # 'anal' #
#logheight=TRUE
#for 27km runs use logheight=FALSE since all data assimilated

MAINDIR=/scratch1/portfolios/BMC/chem-var/pagowski/enkf_runs
EXECS=.

indirfcst=${MAINDIR}/eval/${sim}

var=PM2_5_DRY

infile=${indirfcst}/obs_fcst_${var}.txt

set -A hours 00z 06z 12z 18z

for hour in ${hours[*]}
do
    echo $hour
    infiledot=${infile}.${hour}.sort
    outfiledot=${indirfcst}/stats_${string}${var}_dots.${hour}.txt
    ${EXECS}/eval_stats_6hrtseries_dots.x $infiledot $logheight $outfiledot
done

