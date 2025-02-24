#!/bin/sh
#process wrf  emissions files with perturbations

intime=12z

area="PM_10"

infile=wrfchemi_${intime}_d01_lscales_sector
outfile=wrfchemi_${intime}_d01

INDIR=/scratch1/portfolios/BMC/chem-var/pagowski/convert_emiss/${area}
OUTDIR=${INDIR}/test_15

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
  ./perturb_emissions_varscales_pm10.x ${INDIR}/$infile ${OUTDIR}/$outfile $seed

  /bin/mv ${OUTDIR}/$outfile ${OUTDIR}/$outfile.${seed}
  i=`expr $i + 1`

done
