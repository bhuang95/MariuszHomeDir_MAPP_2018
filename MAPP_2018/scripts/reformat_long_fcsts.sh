#!/bin/ksh 

start_date=2020060100
end_date=2020060100

. ~/.nc

cycle_frequency=24

ndate=~/bin/ndate

sim=RET_FreeRun_NoEmisStoch_C96_202006

INDIR=/scratch2/BMC/gsd-fv3-dev/MAPP_2018/bhuang/JEDI-2020/JEDI-FV3/expRuns/UFS-Aerosols_RETcyc

OUTDIR=/scratch2/BMC/gsd-fv3-dev/MAPP_2018/pagowski/long_fcsts

ident=$start_date

while [[ ${ident} -le ${end_date} ]]
    do

    echo $ident

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hour=`echo "${ident}" | cut -c9-10`
    yyyymmdd=${year}${month}${day}

    for fcst in `seq -f '%03.f' 0 6 120`
    do
	suffix=${sim}/dr-data-longfcst-backup/gdas.${yyyymmdd}/${hour}/diag/aod_grid

	indir=${INDIR}/${suffix}/AOD_NATIVEGRID_fv_tracer_${ident}_fhr${fcst}

	outdir=${OUTDIR}/${suffix}/AOD_NATIVEGRID_fv_tracer_${ident}_fhr${fcst}

	if [[ ! -r ${outdir} ]] 
	then
	    mkdir -p ${outdir}
	fi

	cd $outdir
	
	for file in  ${indir}/*fv_aod_LUTs.fv_tracer.res.tile?.nc
	do
	    echo $file
	    basefile=`basename $file`
	    ncks -O -v aod -d nchannels,2,2 $file tmp.nc
	    ncap2 -O -s 'time[$time]={1.f}' tmp.nc $basefile
	done

	/bin/rm tmp.nc

    done

    ident=`$ndate $cycle_frequency $ident`

done

