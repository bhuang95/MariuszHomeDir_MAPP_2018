#!/bin/ksh

#set -x 

test=test_301

INDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/wrf_run_all

RUNDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/gribrun

/bin/cp wrf_aod_ncf2grib.x $RUNDIR

cd $RUNDIR

start_date=2015080118
end_date=2015083018

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hr=`echo "${ident}" | cut -c9-10`
    
    ident_dir=${year}_${month}_${day}_${hr}

    indir=${INDIR}/${test}/Output/${ident_dir}
    pattern="aod_*.nc"

    /bin/rm -f fort.51

cat >'wrf_aod2grib.ini' <<EOF
&control
varlist='AOD',
start_date="${ident}",
fcst_length=24,
indir="${indir}",
pattern="${pattern}",
id_gribdomain=255
/
EOF

    ./wrf_aod_ncf2grib.x

    /bin/mv fort.51 ${indir}/aod_${ident}.grib 

    ident=`ndate +24 $ident`

done
