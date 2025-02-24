#!/bin/ksh

INDIR1=/scratch1/portfolios/BMC/chem-var/pagowski/enkf_runs/emiss_data/data_12km/Weekday

INDIR2=/scratch1/portfolios/BMC/chem-var/pagowski/enkf_runs/wrf_run_gfs_0th/Output_001/2012_05_30_18

factor_emiss.x 00 ${INDIR1}/wrfchemi_d01_tfactors_ave ${INDIR2}/wrfchemi_output_enkf_d01_2012-05-31_00:00:00  ${INDIR2}/a00z.nc ${INDIR2}/a12z.nc 
