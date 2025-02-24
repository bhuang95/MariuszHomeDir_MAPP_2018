#!/bin/ksh

aqua='MYD04'
terra='MOD04'
surface=deep
#surface=land
#surface=ocean

satellite=$aqua

frequency=3

start_date=2015080100
end_date=2015090100

maindir=/scratch3/BMC/chem-var/pagowski/aod_nnr_processing

prefix=nnr_003.${satellite}_L2a.${surface}
suffix=ods

outdir=${maindir}/outdata/${satellite}

if [[ ! -r $outdir ]]
then
    mkdir -p $outdir
fi

ndate=~/bin/ndate

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
 lat_ur = 55.0
 lon_ll = -130.0
 lon_ur = -60.0

/
__EOF

    aodnnr2bufr.x 

    ident=`$ndate $frequency $ident`

    echo $ident

done
