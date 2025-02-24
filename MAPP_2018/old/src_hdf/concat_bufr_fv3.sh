#!/bin/ksh

set -x

BASEDIR=/scratch3/BMC/chem-var/pagowski/tmp
INDIR=${BASEDIR}/viirs_bufr
OUTDIR=${BASEDIR}/viirs_bufr_concat
SCRIPTDIR=/home/Mariusz.Pagowski/codes/src_hdf
EXECDIR=$SCRIPTDIR

COMBFR=${EXECDIR}/combfr.x

ident_start=2015080100
ident_end=2015090100

cycle_frequency=6
((window=cycle_frequency / 2))

ndate=~/bin/ndate

if [[ ! -r $OUTDIR ]]
then
    mkdir -p $OUTDIR
fi

workdir=$OUTDIR/tmp

ident=$ident_start

while [[ $ident -le $ident_end ]]
do

    /bin/rm -rf $workdir
    mkdir -p $workdir
    cd $workdir

    identm=`$ndate -$window $ident`
    identp=`$ndate +$window $ident`

    identw=$identm

    while [[ $identw -lt $identp ]]
    do
        indir=${INDIR}/${identw}
        if [[ -r ${indir}/fort.55 ]]
        then
            cat ${indir}/fort.55 >> fort.55
            ln -sf ${indir}/JPSS_AOD_BUFR* .
        fi

        identw=`$ndate +1 $identw`
    done

    ${COMBFR}

    cd $OUTDIR

    /bin/mv ${workdir}/fort.50 ./VIIRS_AOD_BUFR:${ident}

    ident=`$ndate +$cycle_frequency $ident`

done

/bin/rm -rf $workdir

