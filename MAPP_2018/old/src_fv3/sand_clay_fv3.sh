#!/bin/ksh

INDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/EMISS_DATA/SAND_CLAY
SRCDIR=/home/Mariusz.Pagowski/codes/src_fv3

RUNDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/workdir_emiss

cd $RUNDIR

/bin/cp ${SRCDIR}/sand_clay_fv3.x .

ln -sf ${INDIR}/*60_1KM.1gd4r .

./sand_clay_fv3.x

