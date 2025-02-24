#!/bin/ksh

INDIR=/scratch3/BMC/chem-var/pagowski/tmp/viirs_hdf
INDIR=/scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/OBS/viirs_hdf
OUTDIR=/scratch3/BMC/chem-var/pagowski/tmp/viirs_bufr
OUTDIR=/scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/OBS/bufr_tmp

INDIRAOT=${INDIR}/AOT
INDIRGMTCO=${INDIR}/GMTCO

aotprefix=JPSS_AOD
gmtcoprefix=GMTCO_npp
bufr_aot_prefix=JPSS_AOD_BUFR
suffix=.h5

date_start=2015080400
date_end=2015081000
date_start=2015081100
date_end=2015082200
date_start=2015082300
date_end=2015083100
date_start=2015080100
date_end=2015083100

date_start=2015081000
date_end=2015081000

if [[ ! -r ${OUTDIR} ]]
then
    mkdir -p ${OUTDIR}
fi

cd ${OUTDIR}

/bin/cp /home/Mariusz.Pagowski/codes/src_hdf/hdf2bufr_fv3.x .
/bin/cp /home/Mariusz.Pagowski/codes/src_hdf/AOT_BUFR_Table.txt .

ndate=~/bin/ndate

date_current=$date_start

while [[ $date_current -le $date_end ]]
do

    echo $date_current

    datedir=`echo $date_current | cut -c1-8`


    /bin/rm -f ${datedir}*/fort.55

    for file in ${INDIRAOT}/${datedir}/${aotprefix}*${suffix}
    do
	basetime=`basename ${file} | sed -e "s/${aotprefix}//g" -e "s/${suffix}//g" | sed -e "s/_V1//g" | cut -c13-`

	aotfile=$file
#	aotfile=${INDIRAOT}/${datedir}/${aotprefix}${basetime}${suffix}
	set -A gmtcofiles `ls -1 ${INDIRGMTCO}/${datedir}/${gmtcoprefix}${basetime}*${suffix}`
	outfile_aot=${OUTDIR}/${bufr_aot_prefix}${basetime}

	gmtcofile=${gmtcofiles[0]}

	if [[ ! -r $gmtcofile || $gmtcofile == '' ]] 
	then
	    echo "file missing $gmtcofile"
	    exit
	fi

	echo $aotfile
	echo $gmtcofile

	./hdf2bufr_fv3.x $aotfile $gmtcofile $outfile_aot 

	bdate=`echo $date_current | cut -c1-8`
        btime=`echo ${basetime} | cut -c13-14`

	echo ${bdate}${btime}

	if [[ ! -r ${bdate}${btime} ]]
	then
	    mkdir ${bdate}${btime}
	fi

	/bin/mv $outfile_aot ./${bdate}${btime}

	basef=`basename $outfile_aot`
	echo $basef >> ./${bdate}${btime}/fort.55

    done

    exit

    yyyy=`echo $date_current | cut -c1-4`
    mm=`echo $date_current | cut -c5-6`
    dd=`echo $date_current | cut -c7-8`

    date_current=`$ndate +24 ${date_current}` 

done

 
