#!/bin/ksh

set -x

BASEDIR=/scratch3/BMC/chem-var/pagowski/tmp
INDIR=${BASEDIR}/viirs_bufr_concat
OUTDIR=${BASEDIR}/viirs_bufr_concat_sampled

bufr_table=AOT_BUFR_Table.txt

ident_start=2015080100
ident_end=2015090100

sampling_frequency=50

cycle_frequency=6

ndate=~/bin/ndate

if [[ ! -r $OUTDIR ]]
then
    mkdir -p $OUTDIR
fi

ident=$ident_start

while [[ $ident -le $ident_end ]]
do
    infile=${INDIR}/VIIRS_AOD_BUFR:${ident}
    outfile=${OUTDIR}/VIIRS_AOD_BUFR:${ident}

    ../src/sample_viirsaod_bufr.x $infile $bufr_table $sampling_frequency $outfile

    ident=`$ndate +$cycle_frequency $ident`
done


