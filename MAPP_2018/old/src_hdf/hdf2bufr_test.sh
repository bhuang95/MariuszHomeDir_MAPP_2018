#!/bin/ksh

fireseason=0
arizona=1
africa=0

INDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/indata/arizona
OUTDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/outdata/test

date_start=2014050500
date_end=2014051500

if [[ ! -r ${OUTDIR} ]]
then
    mkdir -p ${OUTDIR}
fi

cd ${OUTDIR}

/bin/cp /scratch3/BMC/chem-var/pagowski/codes/src/combfr.x .
/bin/cp /home/Mariusz.Pagowski/codes/src_hdf/hdf2bufr.x .
/bin/cp /home/Mariusz.Pagowski/codes/src_hdf/AOT_BUFR_Table.txt .


INDIRAOT=${INDIR}/AOT
INDIRADP=${INDIR}/ADP
INDIRGMTCO=${INDIR}/GMTCO

suffix=.h5

aotprefix=IVAOT_npp_Enterprise
maskprefix=VIIRS_ADP_MASK_Enterprise
gmtcoprefix=GMTCO_npp
bufr_aot_prefix=VIIRS_AOD_BUFR


ndate=~/bin/ndate

date_current=$date_start

while [[ $date_current -le $date_end ]]
do
    datedir=`echo $date_current | cut -c1-8`
    /bin/rm -f fort.55

    outfile_dust_mask=${OUTDIR}/dust_mask_${datedir}.txt
    outfile_smoke_mask=${OUTDIR}/smoke_mask_${datedir}.txt

    /bin/rm -f  $outfile_dust_mask $outfile_smoke_mask

    for file in ${INDIRAOT}/${datedir}/${aotprefix}*${suffix}
    do
	basetime=`basename ${file} | sed -e "s/${aotprefix}//g" -e "s/${suffix}//g"`
	aotfile=${INDIRAOT}/${datedir}/${aotprefix}${basetime}${suffix}
	maskfile=${INDIRADP}/${datedir}/${maskprefix}${basetime}${suffix}
	set -A gmtcofiles `ls -1 ${INDIRGMTCO}/${datedir}/${gmtcoprefix}${basetime}*${suffix}`
	outfile_aot=${OUTDIR}/${bufr_aot_prefix}${basetime}

	gmtcofile=${gmtcofiles[0]}

	if [[ ! -r $gmtcofile || $gmtcofile == '' ]] 
	then
	    echo "file missing $gmtcofile"
	    exit
	fi

	if [[ ! -r $maskfile ]]
	then
	    ./hdf2bufr.x $aotfile $gmtcofile $outfile_aot 
	else
#	    ./hdf2bufr.x $aotfile $gmtcofile $outfile_aot 
	    ./hdf2bufr.x $aotfile $gmtcofile $maskfile $outfile_aot $outfile_dust_mask $outfile_smoke_mask
	fi

	echo $outfile_aot >> fort.55
	echo ${basetime}

    done

    exit

    date_current=`$ndate +24 ${date_current}` 
    echo $date_current
done

 
