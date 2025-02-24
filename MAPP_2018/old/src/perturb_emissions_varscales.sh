#!/bin/sh
#process wrf  emissions files with perturbations

utc=00 
utc=12

resolution="24km"

infile=wrfchemi_${utc}z_d01_lscales
outfile=wrfchemi_${utc}z_d01

INDIR=/scratch1/portfolios/BMC/chem-var/pagowski/enkf_runs/emiss_data/data_${resolution}

OUTDIR=${INDIR}/test_52

nens=50

if [ ! -r ${OUTDIR} ]
    then
    mkdir ${OUTDIR}
fi

i=1

while [ $i -le $nens ]
do
  echo "member $i"
  seed="`printf %03i $i`"
  ./perturb_emissions_varscales.x ${INDIR}/$infile ${OUTDIR}/$outfile $seed

  /bin/mv ${OUTDIR}/$outfile ${OUTDIR}/$outfile.${seed}
  i=`expr $i + 1`

done

/bin/cp ${INDIR}/${outfile} ${OUTDIR}/${outfile}.000

#scaling no longer necessary for newer emissions
#./scale_emissions.x ${INDIR}/$infile ${OUTDIR}/${outfile}.000