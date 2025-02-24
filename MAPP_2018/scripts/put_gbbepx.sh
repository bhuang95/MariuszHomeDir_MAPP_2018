#!/bin/ksh 

set -x

. /etc/profile
. /apps/lmod/lmod/init/sh

module load hpss

INDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/GBBEPx
HPSSDIR=/BMC/fim/5year/MAPP_2018/GBBEPx

cd $OUTDIR

hsi put ${INDIR}/C96_gbbepx_2016.tar : /BMC/fim/5year/MAPP_2018/GBBEPx/C96_gbbepx_2016.tar


