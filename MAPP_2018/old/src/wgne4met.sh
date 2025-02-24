#!/bin/ksh

#set -x

INDIR_SFC=/scratch1/portfolios/BMC/chem-var/pagowski/wgne/brasil/surface-inmet
INDIR_RADIOS=/scratch1/portfolios/BMC/chem-var/pagowski/wgne/brasil/radiosonde
OUTDIR=/scratch1/portfolios/BMC/chem-var/pagowski/wgne/brasil/sfc_radios4met
METEXEC=/scratch1/portfolios/BMC/chem-var/pagowski/met/bin
SRCDIR=/scratch1/portfolios/BMC/chem-var/pagowski/codes/src

. ~/bin/funcs.sh

start_ident=2012090100
end_ident=2012093023

if [[ -r $OUTDIR ]]
then
    /bin/rm -r  $OUTDIR
fi

mkdir -p $OUTDIR

cd $OUTDIR

outfile_sfc=wgn_sfc.txt
outfile_radios=wgn_radios.txt

ident=$start_ident

while [[ $ident -le $end_ident ]]
do

    echo $ident

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hr=`echo "${ident}" | cut -c9-10`
    
    sfcexist=0

    infile_sfc=is${year}-${month}-${day}-${hr}00
    if [[ -r ${INDIR_SFC}/${infile_sfc} ]]
    then
	${SRCDIR}/wgne_sfc4met.x ${INDIR_SFC}/${infile_sfc} ${outfile_sfc}
	sfcexist=1
#	/bin/cp ${outfile_sfc} ${outfile_sfc}_${ident}
    fi

    radiosexist=0

    infile_radios=TEMP-${year}${month}${day}${hr}00_${year}${month}${day}${hr}00.dat
    if [[ -r ${INDIR_RADIOS}/${infile_radios} ]]
    then
	${SRCDIR}/wgne_radios4met.x ${INDIR_RADIOS}/${infile_radios} ${outfile_radios}
	radiosexist=1
#	/bin/cp ${outfile_radios} ${outfile_radios}_${ident}
    fi

    if [[ $sfcexist == 1 ]]
    then
	if [[ $radiosexist == 1 ]]
	then
	    cat ${outfile_sfc} ${outfile_radios} > outfile.txt
	else
	    /bin/mv ${outfile_sfc} outfile.txt
	fi
	${METEXEC}/ascii2nc outfile.txt brasil_${ident}.nc
    else
	if [[ $radiosexist == 1 ]]
	then
	    /bin/mv ${outfile_radios} outfile.txt
	    ${METEXEC}/ascii2nc outfile.txt brasil_${ident}.nc
	fi
    fi
    
    increment_date 1
    
    ident=${end_year}${end_month}${end_day}${end_hr}

done


