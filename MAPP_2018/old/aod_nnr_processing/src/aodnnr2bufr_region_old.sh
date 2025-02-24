#!/bin/ksh

aqua='MYD04'
terra='MOD04'
surface=land
surface=ocean

satellite=$aqua


frequency=3

start_date=2012060100
end_date=2012090100

maindir=/scratch1/portfolios/BMC/chem-var/pagowski/aod/aod_nnr

prefix=nnr_002.${satellite}_L2a.${surface}
suffix=ods

outdir=${maindir}/outdata/${satellite}

if [[ ! -r $outdir ]]
then
    mkdir -p $outdir
fi

. ~/bin/funcs.sh

ident=$start_date

while [[ $ident -le $end_date ]]
do
    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hr=`echo "${ident}" | cut -c9-10`

    indir=${maindir}/indata/${satellite}/Y${year}/M${month}

    infile=${prefix}.${year}${month}${day}_${hr}00z.${suffix}
    outfile=${prefix}.${year}${month}${day}_${hr}00z.bufr

cat > namelist.modis_nnr_converter << __EOF
&record1
 input_dir = "${indir}"
 input_file = "${infile}"
 output_dir = "${outdir}"
 output_file = "${outfile}"
 select_domain = .true.
 lat_ll = 10.0
 lat_ur = 60.0
 lon_ll = -150.0
 lon_ur = -50.0
/
__EOF

    aodnnr2bufr.x 

    increment_date $frequency

    ident=${end_year}${end_month}${end_day}${end_hr}

    echo $ident

done
