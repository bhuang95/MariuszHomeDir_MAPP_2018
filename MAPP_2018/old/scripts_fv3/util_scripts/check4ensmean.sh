#!/bin/ksh -l

cd /home/Mariusz.Pagowski/codes/scripts_fv3/util_scripts

. ./environ.sh

. ${MAINDIR}/env_cron.kshrc

ndate=~/bin/ndate

sim=DA_ENKF

if [[ -r ${FV3_RUNS}/${sim}/ensmean_date.txt ]]
then
    analdate=`cat ${FV3_RUNS}/${sim}/ensmean_date.txt`
else
    exit 0
fi

analdatep=`$ndate $ANALINC $analdate`

member=1

while [[ $member -le $NANALS ]]
do
    charnanal=mem`printf %03i $member`
    
    fh=$FHMIN

    while [[ $fh -le $FHMAX ]]
    do

	indir=${FV3_RUNS}/${sim}/${analdate}/${charnanal}/OUTPUT_FV3_nemsio
        cfh=`printf %02i $fh`
	if [[ ! -s ${indir}/bfg_${analdatep}_fhr${cfh} && ! -s ${indir}/sfg_${analdatep}_fhr${cfh} ]]
	then
	    exit 1
	fi


	((fh=fh+1))

    done

    ((member=member+1))

done

charnanal=ensmean

indir=${FV3_RUNS}/${sim}/${analdate}/${charnanal}/OUTPUT_FV3

adate=`echo ${analdate} | cut -c 1-8`

if [[ ! -s ${indir}/${adate}.fv3_history.tile1.nc ]]
then
    /bin/rm ${FV3_RUNS}/${sim}/ensmean_date.txt
    cd $SCRIPTDIR_UTIL
    echo "qsub -v ident=${analdate} qsub_calc_ensmean_history.sh"
    qsub -v ident=${analdate} qsub_calc_ensmean_history.sh
fi

