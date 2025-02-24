#!/bin/ksh

INDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/EMISS_DATA/SULF
SRCDIR=/home/Mariusz.Pagowski/codes/src_fv3

RUNDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/workdir_emiss

cd $RUNDIR

/bin/cp ${SRCDIR}/emiss_sulf_anthro_fv3.x ${SRCDIR}/add_time.nco .

ln -sf ${INDIR}/anthro.bin

./emiss_sulf_anthro_fv3.x

