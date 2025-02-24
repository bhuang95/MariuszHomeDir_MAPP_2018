#!/bin/ksh

NCLDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/ncl
SCRIPTDIR=~/mapp_2018/ncl

MAINDIR=$NCLDIR

ident=20180415.000000
ntiles=6

DATADIR=${MAINDIR}/indata_incr_c48

cd $DATADIR

itile=1
while [[ $itile -le $ntiles ]]
do 
    file_incr=${ident}.fv_tracer.C48.res.tile${itile}.nc
    ln -sf ${file_incr} incr.tile${itile}.nc
    ((itile=itile+1))
done

cd ${SCRIPTDIR}

export TITLE="Medium Sea-salt Increment"
export SPECIES="seas2"
export LIMITLOW=-1
export LIMITHIGH=1

#export TITLE="Fine Dust Increment"
#export SPECIES="dust1"
#export LIMITLOW=-1
#export LIMITHIGH=1

#export TITLE="Hydrophobic Black Carbon Increment"
#export SPECIES="bc1"
#export LIMITLOW=-1
#export LIMITHIGH=1

#export TITLE="Hydrophobic Organic Carbon Increment"
#export SPECIES="oc1"
#export LIMITLOW=0
#export LIMITHIGH=25

#export TITLE="Hydrophilic Organic Carbon Increment"
#export SPECIES="oc2"
#export LIMITLOW=0
#export LIMITHIGH=5


export NCONS=10

species=${SPECIES}

ncl plotit_globe_increment_c48.ncl

/bin/mv incr.png ${NCLDIR}/pics_incr_c48/${species}_incr.png
