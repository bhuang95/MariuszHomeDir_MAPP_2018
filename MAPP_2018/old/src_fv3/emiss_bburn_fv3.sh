#!/bin/ksh

INDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/PREPOUT_DATA
SRCDIR=/home/Mariusz.Pagowski/codes/src_fv3

RUNDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/workdir_emiss

cd $RUNDIR

/bin/cp ${SRCDIR}/emiss_bburn_fv3.x .

ln -sf ${INDIR}/*-bb.bin .

./emiss_bburn_fv3.x 'LL4FV3' '2016071600' 'emiss_bburn_fv3'
