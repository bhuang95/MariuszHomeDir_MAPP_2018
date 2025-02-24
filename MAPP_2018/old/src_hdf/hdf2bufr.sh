#!/bin/ksh

#sample
#INDIR=/scratch4/BMC/public/data/sat/nesdis/viirs

#INDIRAOT=${INDIR}/aot/conus
#INDIRADP=${INDIR}/adp/conus

#arizona

arizona=1
africa=0
fireseason=0
INDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/indata/arizona
OUTDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/outdata/arizona

date_start=2014050500
date_end=2014051500

arizona=0
fireseason=0
africa=1

INDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/indata/africa
OUTDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/outdata/test

date_start=2014040100
date_end=2014040400

date_start=2014033000
date_end=2014033100

#new bufr format
#arizona

arizona=1
africa=0
fireseason=0
INDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/indata/arizona
OUTDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/outdata/arizona_new

date_start=2014051100
date_end=2014051100

#crtm test
#arizona=1
#africa=0
#INDIR=/scratch3/BMC/chem-var/pagowski/crtm_work/viirs
#OUTDIR=/scratch3/BMC/chem-var/pagowski/crtm_work/viirs/outdata

#date_start=2016041100
#date_end=2016041300

fireseason=1
arizona=0
africa=0

INDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/indata/fireseason
OUTDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/outdata/fireseason

date_start=2017090200
date_start=2017082800
date_end=2017090700


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

    echo $date_current

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
	    echo "maskfile not available for $basetime"
	else
#	    ./hdf2bufr.x $aotfile $gmtcofile $outfile_aot 
	    ./hdf2bufr.x $aotfile $gmtcofile $maskfile $outfile_aot $outfile_dust_mask $outfile_smoke_mask
	fi

	echo $outfile_aot >> fort.55
	echo ${basetime}

    done

    yyyy=`echo $date_current | cut -c1-4`
    mm=`echo $date_current | cut -c5-6`
    dd=`echo $date_current | cut -c7-8`

    if [[ $fireseason == 1 || $arizona == 1 && $africa == 0 ]]
    then
	hh=18
    fi

    if [[ $fireseason == 0 || $arizona == 0 && $africa == 1 ]]
    then
	hh=12
    fi


    bufrname=${bufr_aot_prefix}:${yyyy}-${mm}-${dd}_${hh}:00:00

    ./combfr.x 

    /bin/mv fort.50 ${bufrname}
    /bin/rm VIIRS_AOD_BUFR_*

    date_current=`$ndate +24 ${date_current}` 

done

 
