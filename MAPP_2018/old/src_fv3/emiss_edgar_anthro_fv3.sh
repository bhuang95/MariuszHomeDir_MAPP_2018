#!/bin/ksh

INDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/EMISS_DATA/EDGAR_HTAP
SRCDIR=/home/Mariusz.Pagowski/codes/src_fv3

RUNDIR=/scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/workdir_emiss

cd $RUNDIR

/bin/cp ${SRCDIR}/emiss_edgar_anthro_fv3.x .

ln -sf ${INDIR}/EDGAR-HTAP_*.h5 .

./emiss_edgar_anthro_fv3.x
