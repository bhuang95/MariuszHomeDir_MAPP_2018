#!/bin/ksh

#rename fcts from 6-hr cycle to 1-hr cycle for FGAt in JEDI

INDIR_samefcst=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/data/fv3-jedi/inputs/gfs_aero_c12_4fgat_samefcst
INDIR=/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/envar_2018/C12/RESTART/gfs_aero_c12
OUTDIR=/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/envar_2018/C12/RESTART4fgat_fake/gfs_aero_c12

member=bkg #mem001 #"bkg"

cycle_frequency=6

pseudo_cycle_frequency=1

start_date=2018041500

once=0
ident=$start_date

year=`echo $ident | cut -c1-4`
month=`echo $ident | cut -c5-6`
day=`echo $ident | cut -c7-8`
hour=`echo $ident | cut -c9-10`0000

end_date_m=2018041406
end_date_p=2018041518

ident_p=$start_date
ident_m=$start_date

ident_pp=$start_date
ident_mm=$start_date

if [[ ! -r $OUTDIR/${member} ]]
then
    mkdir -p $OUTDIR/${member}
fi

ndate=~/bin/ndate

/bin/cp ${INDIR}/akbk.nc $OUTDIR

cd $INDIR/${member}

echo $INDIR/${member}

/bin/cp ${year}${month}${day}.${hour}.* $OUTDIR/${member}

/bin/cp ${INDIR_samefcst}/${member}/*.coupler.res $OUTDIR/${member}

while [[ $ident_p -lt $end_date_p ]]
do

    ident_m=`$ndate -${cycle_frequency} $ident_m`
    ident_p=`$ndate +${cycle_frequency} $ident_p`

    year_m=`echo $ident_m | cut -c1-4`
    month_m=`echo $ident_m | cut -c5-6`
    day_m=`echo $ident_m | cut -c7-8`
    hour_m=`echo $ident_m | cut -c9-10`0000

    year_p=`echo $ident_p | cut -c1-4`
    month_p=`echo $ident_p | cut -c5-6`
    day_p=`echo $ident_p | cut -c7-8`
    hour_p=`echo $ident_p | cut -c9-10`0000

    echo 

    echo $ident_m $ident_p

    ident_mm=`$ndate -${pseudo_cycle_frequency} $ident_mm`
    ident_pp=`$ndate +${pseudo_cycle_frequency} $ident_pp`

    year_mm=`echo $ident_mm | cut -c1-4`
    month_mm=`echo $ident_mm | cut -c5-6`
    day_mm=`echo $ident_mm | cut -c7-8`
    hour_mm=`echo $ident_mm | cut -c9-10`0000

    year_pp=`echo $ident_pp | cut -c1-4`
    month_pp=`echo $ident_pp | cut -c5-6`
    day_pp=`echo $ident_pp | cut -c7-8`
    hour_pp=`echo $ident_pp | cut -c9-10`0000

    echo $ident_mm $ident_pp


    itile=1
    while [[ $itile -le 6 ]]
    do

	if [[ $once == 0 ]]
	then

	    ncatted -O -a filename,global,o,c,"RESTART/${year}${month}${day}.${hour}.fv_tracer.res.tile${itile}.nc" $OUTDIR/${member}/${year}${month}${day}.${hour}.fv_tracer.res.tile${itile}.nc
	    ncatted -O -a history,global,d,, $OUTDIR/${member}/${year}${month}${day}.${hour}.fv_tracer.res.tile${itile}.nc

	    ncatted -O -a filename,global,o,c,"RESTART/${year}${month}${day}.${hour}.fv_core.res.tile${itile}.nc" $OUTDIR/${member}/${year}${month}${day}.${hour}.fv_core.res.tile${itile}.nc
#	    ncatted -O -a history,global,d,, $OUTDIR/${member}/${year}${month}${day}.${hour}.fv_core.res.tile${itile}.nc


	fi

	/bin/cp ${year_p}${month_p}${day_p}.${hour_p}.fv_tracer.res.tile${itile}.nc  $OUTDIR/${member}/${year_pp}${month_pp}${day_pp}.${hour_pp}.fv_tracer.res.tile${itile}.nc
	ncatted -O -a filename,global,o,c,"RESTART/${year_pp}${month_pp}${day_pp}.${hour_pp}.fv_tracer.res.tile${itile}.nc" $OUTDIR/${member}/${year_pp}${month_pp}${day_pp}.${hour_pp}.fv_tracer.res.tile${itile}.nc
	ncatted -O -a history,global,d,, $OUTDIR/${member}/${year_pp}${month_pp}${day_pp}.${hour_pp}.fv_tracer.res.tile${itile}.nc

	/bin/cp ${year_p}${month_p}${day_p}.${hour_p}.fv_core.res.tile${itile}.nc  $OUTDIR/${member}/${year_pp}${month_pp}${day_pp}.${hour_pp}.fv_core.res.tile${itile}.nc
	ncatted -O -a filename,global,o,c,"RESTART/${year_pp}${month_pp}${day_pp}.${hour_pp}.fv_core.res.tile${itile}.nc" $OUTDIR/${member}/${year_pp}${month_pp}${day_pp}.${hour_pp}.fv_core.res.tile${itile}.nc
	ncatted -O -a history,global,d,, $OUTDIR/${member}/${year_pp}${month_pp}${day_pp}.${hour_pp}.fv_core.res.tile${itile}.nc


	/bin/cp ${year}${month}${day}.${hour}.fv_tracer.res.tile${itile}.nc  $OUTDIR/${member}/${year_mm}${month_mm}${day_mm}.${hour_mm}.fv_tracer.res.tile${itile}.nc
	ncatted -O -a filename,global,o,c,"RESTART/${year_mm}${month_mm}${day_mm}.${hour_mm}.fv_tracer.res.tile${itile}.nc" $OUTDIR/${member}/${year_mm}${month_mm}${day_mm}.${hour_mm}.fv_tracer.res.tile${itile}.nc
	ncatted -O -a history,global,d,, $OUTDIR/${member}/${year_mm}${month_mm}${day_mm}.${hour_mm}.fv_tracer.res.tile${itile}.nc

	/bin/cp ${year}${month}${day}.${hour}.fv_core.res.tile${itile}.nc  $OUTDIR/${member}/${year_mm}${month_mm}${day_mm}.${hour_mm}.fv_core.res.tile${itile}.nc
	ncatted -O -a filename,global,o,c,"RESTART/${year_mm}${month_mm}${day_mm}.${hour_mm}.fv_core.res.tile${itile}.nc" $OUTDIR/${member}/${year_mm}${month_mm}${day_mm}.${hour_mm}.fv_core.res.tile${itile}.nc
	ncatted -O -a history,global,d,, $OUTDIR/${member}/${year_mm}${month_mm}${day_mm}.${hour_mm}.fv_core.res.tile${itile}.nc


	((itile=itile+1))

    done

    once=1

done
