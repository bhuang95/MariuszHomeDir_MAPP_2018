#!/bin/sh

hour=00 #00 06 12 18
method=nmc #ens
pbl=ysu
resolution=24km

prefix=wrf-arw-gsi_be
suffix=gcv_little_endian

AERODIR=/scratch1/portfolios/BMC/chem-var/pagowski/gen_be_aero
INDIR_BE=${AERODIR}/outdata_${pbl}_${method}_${resolution}_${hour}z
INDIR_R=${AERODIR}/aero_R_scales_${pbl}_${method}_${resolution}_out

infile_be=${INDIR_BE}/${prefix}.${suffix}
prefix_r=${INDIR_R}/stats_R_4gsi_
suffix_r=_${hour}z.txt

outfile_be=${INDIR_BE}/${prefix}_${pbl}_${method}_${hour}z.${suffix}

convert_aero_be.x $infile_be $prefix_r $suffix_r $outfile_be


