#!/bin/sh
#process wrf  emissions files with perturbations
#test 15 was renamed as test_12 (sigma_log=0.3 -> sigma_log=0.25

infile=wrfchemi_00z_d01
infile=wrfchemi_12z_d01
outfile=${infile}

INDIR=../emissdata
OUTDIR=$INDIR/perturb/test_15

nens=80

if [ ! -r ${OUTDIR} ]
    then
    mkdir ${OUTDIR}
fi

i=1

while [ $i -le $nens ]
do
  echo "member $i"
  seed="`printf %03i $i`"
  ./perturb_emissions.x ${INDIR}/$infile ${OUTDIR}/$outfile $seed
  /bin/mv ${OUTDIR}/$outfile ${OUTDIR}/$outfile.${seed}
  i=`expr $i + 1`
done
