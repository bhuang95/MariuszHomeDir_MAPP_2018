#!/bin/ksh

INDIR=/scratch1/portfolios/BMC/chem-var/pagowski/enkf_runs/wrf_run_gfs_0th/Output_000/2012_06_04_18

cat > ensmean.nl << __EOF
&ensmean_nl
logemiss=.false.,
varnames2d = 'MU',
varnames3d =  'U','V','T','QVAPOR','W','PH','BC1','BC2','OC1','OC2','SEAS_1','SEAS_2','DUST_1','DUST_2','sulf','P25'
/

__EOF

/bin/cp ${INDIR}/wrfout_d01_2012-06-04_18:00:00.mem001 ${INDIR}/wrfout_d01_2012-06-04_18:00:00.ensmean


cat > ensmean.nl << __EOF
&ensmean_nl
logemiss=.true.,
varnames2d = ,
varnames3d =  'E_ECI','E_ECJ','E_ORGI','E_ORGJ','E_PM25I','E_PM25J'
/

__EOF

INDIR=/scratch1/portfolios/BMC/chem-var/pagowski/enkf_runs/enkf_run_gfs_0th

/bin/cp ${INDIR}/firstguess_emiss.mem001 ${INDIR}/firstguess_emiss.ensmean

mpiexec_mpt -n 12 getensmean.x 12 ${INDIR}/firstguess_emiss

#mpiexec_mpt -n 12 getensmean.x 12 ${INDIR}/wrfout_d01_2012-06-04_18:00:00
