#!/bin/bash

for n in {1..6}; do
#n=1
export  srcfile=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/WORKDATA/NRTdata/rawdata/gbbepx_0.1deg/GBBEPx_addVIIRS.emisX.001.20190911.nc
export  dstfile=/scratch1/NCEPDEV/global/glopara/fix/fix_fv3/C96/C96_grid_spec.tile$n.nc
export  wgtfile2=./weights_file_s2d_c96_tile$n.nc
export  wgtfile=./weights_file_cons_c96_tile$n.nc
export  infile=$srcfile 
export  outfile=./GBBEPx_20190911_tile$n.nc
rm -f ${outfile}

# ncl reg2esg_s2d.ncl
ncl reg2esg_conserv.ncl
done
