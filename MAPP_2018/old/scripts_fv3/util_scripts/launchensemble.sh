#!/bin/ksh

. ./environ.sh

cd $SCRIPTDIR_DRIVER

nanals=$NANALS

#ident=2015080606
#ident=2015081006
ident=2015081718

nanal=17
while [[ $nanal -le $nanals ]]
do
    echo "qsub -v ident=$ident,member=$nanal qsub_fv3_gfs_c192_warm_da_ensemble.sh"
    qsub -v ident=$ident,member=$nanal qsub_fv3_gfs_c192_warm_da_ensemble.sh
    ((nanal=nanal+1))
done

exit 0
