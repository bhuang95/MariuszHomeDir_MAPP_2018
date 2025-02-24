#!/bin/ksh
#process wrf  emissions files with perturbations

echo "the code is potentially incorrect for so2"
exit

infile=wrfchemi_00z_d01
#infile=wrfchemi_12z_d01
outfile=${infile}

INDIR=/scratch1/portfolios/BMC/chem-var/pagowski/enkf_runs/emiss_data_20km
OUTDIR=${INDIR}/perturb/test_17

nens=50

if [[ ! -r ${OUTDIR} ]]
    then
    mkdir -p ${OUTDIR}
fi

i=1

while [ $i -le $nens ]
do
  echo "member $i"
  seed="`printf %03i $i`"
  ./perturb_emissions_20km.x ${INDIR}/$infile ${OUTDIR}/$outfile $seed
  /bin/mv ${OUTDIR}/$outfile ${OUTDIR}/$outfile.${seed}
  i=`expr $i + 1`
done
