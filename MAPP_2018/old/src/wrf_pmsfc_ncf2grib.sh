#!/bin/ksh

set -x 

test=test_212

INDIR=/scratch3/BMC/chem-var/pagowski/enkf_runs/wrf_run_gfs_all/wrf_pmsfc_${test}

start_date=2013061012
#start_date=2013062212
end_date=2013071912
#end_date=2013061812

cycle=t12z

infile="pmsfc.${cycle}.nc"
outfile="pmsfc.${cycle}.25pm"

cat >'wrf_pmsfc2grib.ini' <<EOF
&control
varlist='PM2.5'
infile=$infile
outfile=$outfile
id_gribdomain=255
ave1hr=.false.
/
EOF

ident=$start_date

while [[ $ident -le $end_date ]]
do
    echo $ident

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hr=`echo "${ident}" | cut -c9-10`
    
    ident_dir=${year}_${month}_${day}_${hr}
    datestamp=${year}-${month}-${day}_${hr}:00:00

    ln -sf ${INDIR}/${ident_dir}/pmsfc_${datestamp} $infile
    ./wrf_pmsfc_ncf2grib.x
    /bin/mv ${outfile}?? ${INDIR}/${ident_dir}
    ident=`ndate +24 $ident`
    exit
done
