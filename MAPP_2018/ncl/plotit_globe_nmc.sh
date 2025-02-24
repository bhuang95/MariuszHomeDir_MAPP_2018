#!/bin/ksh

NCLDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/ncl
SCRIPTDIR=~/mapp_2018/ncl

MAINDIR=$NCLDIR

start_date=2016052000
end_date=2016063012

ntiles=6

DATADIR=${MAINDIR}/indata_nmc

cd $DATADIR

itile=1
while [[ $itile -le $ntiles ]]
do 
    file_nmc=nmc_${start_date}_${end_date}.tile${itile}.nc
    ln -sf ${file_nmc} nmc.tile${itile}.nc
    ((itile=itile+1))
done

cd ${SCRIPTDIR}

export TITLE="Sulfate Error Stdev"
export SPECIES="sulf"
export LIMITLOW=0
export LIMITHIGH=5

export TITLE="Medium Sea-salt Error Stdev"
export SPECIES="seas3"
export LIMITLOW=0
export LIMITHIGH=2.5

export TITLE="Fine Dust Error Stdev"
export SPECIES="dust1"
export LIMITLOW=0
export LIMITHIGH=10

export TITLE="Hydrophobic Black Carbon Error Stdev"
export SPECIES="bc1"
export LIMITLOW=0
export LIMITHIGH=2.5

export TITLE="Hydrophobic Organic Carbon Error Stdev"
export SPECIES="oc1"
export LIMITLOW=0
export LIMITHIGH=20

export TITLE="Hydrophilic Organic Carbon Error Stdev"
export SPECIES="oc2"
export LIMITLOW=0
export LIMITHIGH=2.5


export NCONS=10


species=${SPECIES}

ncl plotit_globe_nmc.ncl

/bin/mv nmc.png ${NCLDIR}/pics_nmc/${species}_nmc.png
