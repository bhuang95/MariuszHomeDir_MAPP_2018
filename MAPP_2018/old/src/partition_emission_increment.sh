#!/bin/ksh

INDIR=/scratch1/portfolios/BMC/chem-var/pagowski/enkf_runs/wrf_run_gfs_all/test_104/Output_000/2012_06_03_00

partition_emission_increment.x ${INDIR}/wrfchemi_00z_d01_input_enkf_test ${INDIR}/wrfchemi_d01_output_enkf_test
