#!/bin/ksh

#sample
INDIR=/scratch4/BMC/public/data/sat/nesdis/viirs

INDIRAOT=${INDIR}/aot/conus
INDIRADP=${INDIR}/adp/conus

suffix=.h5

aotprefix=IVAOT_npp_Enterprise
maskprefix=VIIRS_ADP_MASK_Enterprise

outfile=junk

for file in ${INDIRAOT}/${aotprefix}*${suffix}
do
    basetime=`basename ${file} | sed -e "s/${aotprefix}//g"`
    aotfile=${INDIRAOT}/${aotprefix}$basetime
    maskfile=${INDIRADP}/${maskprefix}$basetime
    echo $basetime
    ./hdf2bufr_rt.x $aotfile $maskfile $outfile
done






