#!/bin/ksh

. ~/bin/funcs.sh

INDIR=../outdata/2012
OUTDIR=$INDIR/bufr_aeronet

infile_aeronet=${INDIR}/aeronet_data_bundle

start_ident=2012060100 
end_ident=2012090100 

cycle_frequency=24

if [[ ! -r $OUTDIR ]]
then
    mkdir -p $OUTDIR
fi

ident=$start_ident

while [[ $ident -lt $end_ident ]] 
do

    echo $ident

    year=`echo $ident | cut -c1-4`
    month=`echo $ident | cut -c5-6`
    day=`echo $ident | cut -c7-8`
    hr=`echo $ident | cut -c9-10`

    aeronet_txt2bufr.x $infile_aeronet $OUTDIR $ident

    increment_date $cycle_frequency
    ident=${end_year}${end_month}${end_day}${end_hr}

done
