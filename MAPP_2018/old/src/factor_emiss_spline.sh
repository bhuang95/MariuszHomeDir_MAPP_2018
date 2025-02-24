#!/bin/ksh

. ~/bin/funcs.sh

prefix=/scratch1/portfolios/BMC/chem-var/pagowski/enkf_runs/wrf_run_gfs_0th/Output_002

emissdir=/scratch1/portfolios/BMC/chem-var/pagowski/enkf_runs/emiss_data/data_24km

scriptdir=/scratch1/portfolios/BMC/chem-var/pagowski/codes/src
outdir=/scratch1/portfolios/BMC/chem-var/pagowski/codes/outdata/tmp

cd $outdir

ident=2012060106

year=`echo "${ident}" | cut -c1-4`
month=`echo "${ident}" | cut -c5-6`
day=`echo "${ident}" | cut -c7-8`
hr=`echo "${ident}" | cut -c9-10`

cycle_frequency=6

allexist=1

/bin/rm -rf wrfchemi_??

i=1
while [[ i -le 4 ]]
do
    ci="`printf %02i $i`"
    ((back_hours = -i *cycle_frequency))
    increment_date $back_hours
    echo $end_year $end_month $end_day $end_hr
    wrfdir=${prefix}/${end_year}_${end_month}_${end_day}_${end_hr}
    if [[ ! -r ${wrfdir}/wrfchemi_d01_output_enkf ]]
    then
	allexist=0
	break
    fi
    ln -sf ${wrfdir}/wrfchemi_d01_output_enkf ./wrfchemi_${ci}
    ((i=i+1))
done

/bin/cp ${emissdir}/wrfchemi_??z_d01 .

time ${scriptdir}/factor_emiss_spline_tspack.x $hr wrfchemi $allexist 

