#!/bin/ksh

INDIR=/scratch3/BMC/chem-var/pagowski/tmp/bin_regrid_bckg
SRCDIR=/home/Mariusz.Pagowski/codes/src_da_utils

file_bckg_in='/scratch3/BMC/chem-var/pagowski/enkf_runs/trunk_r79009/fix/Big_Endian/global_berror.l64y386.f77'

file_bckg_out=global_berror.l64y386_aero_gocart.f77

prefix_aero_bckg_in='be_tracer'

cd $INDIR

/bin/cp ${SRCDIR}/add_aero_bckg.x .

./add_aero_bckg.x $file_bckg_in $prefix_aero_bckg_in $file_bckg_out 


