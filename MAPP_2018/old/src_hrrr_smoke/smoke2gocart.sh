#!/bin/ksh

INDIR=/scratch3/BMC/wrf-chem/pagowski/hrrr_smoke/wrfout
infile=wrfinput_gsi_d01_2016-09-02_18_00_00
infile=wrfout_d01_2017-06-12_18_00_00_slim
weightsfile=/scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/smokeoutdir/smoke_lsq_weights.txt

#INDIR=/scratch3/BMC/wrf-chem/pagowski/hrrr_smoke/wrfout/idaho_fire
#infile=wrf_4crtm_2016082818.nc



smoke2gocart.x ${INDIR}/$infile $weightsfile


