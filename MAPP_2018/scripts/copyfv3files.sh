#!/bin/ksh

INDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/data/fv3-jedi/inputs/gfs_aero_c12_all_4fgat/mem008


ndate=~/bin/ndate

cycle_frequency=1

start_date=2018041500
end_date_m=2018041422
end_date_p=2018041502


ident=$start_date
year=`echo $ident | cut -c1-4`
month=`echo $ident | cut -c5-6`
day=`echo $ident | cut -c7-8`
hour=`echo $ident | cut -c9-10`0000


ident_p=$start_date
ident_m=$start_date

cd $INDIR



while [[ $ident_p -le $end_date_p ]]
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

    echo $ident_m $ident_p

#    /bin/cp ${year}${month}${day}.${hour}.coupler.res ${year_p}${month_p}${day_p}.${hour_p}.coupler.res

#    /bin/cp ${year}${month}${day}.${hour}.coupler.res ${year_m}${month_m}${day_m}.${hour_m}.coupler.res

    itile=1
    while [[ $itile -le 6 ]]
    do

	if [[ $ident -eq $start_date ]]
	then

	    ncatted -O -a filename,global,o,c,"RESTART/${year}${month}${day}.${hour}.fv_tracer.res.tile1.nc" ${year}${month}${day}.${hour}.fv_tracer.res.tile${itile}.nc
	    ncatted -O -a history,global,d,, ${year}${month}${day}.${hour}.fv_tracer.res.tile${itile}.nc

	    ncatted -O -a filename,global,o,c,"RESTART/${year}${month}${day}.${hour}.fv_core.res.tile1.nc" ${year}${month}${day}.${hour}.fv_core.res.tile${itile}.nc
	    ncatted -O -a history,global,d,, ${year}${month}${day}.${hour}.fv_core.res.tile${itile}.nc


	fi

	/bin/cp ${year}${month}${day}.${hour}.fv_tracer.res.tile${itile}.nc  ${year_p}${month_p}${day_p}.${hour_p}.fv_tracer.res.tile${itile}.nc
	ncatted -O -a filename,global,o,c,"RESTART/${year_p}${month_p}${day_p}.${hour_p}.fv_tracer.res.tile1.nc" ${year_p}${month_p}${day_p}.${hour_p}.fv_tracer.res.tile${itile}.nc
	ncatted -O -a history,global,d,, ${year_p}${month_p}${day_p}.${hour_p}.fv_tracer.res.tile${itile}.nc


	/bin/cp ${year}${month}${day}.${hour}.fv_core.res.tile${itile}.nc  ${year_p}${month_p}${day_p}.${hour_p}.fv_core.res.tile${itile}.nc
	ncatted -O -a filename,global,o,c,"RESTART/${year_p}${month_p}${day_p}.${hour_p}.fv_core.res.tile1.nc" ${year_p}${month_p}${day_p}.${hour_p}.fv_core.res.tile${itile}.nc
	ncatted -O -a history,global,d,, ${year_p}${month_p}${day_p}.${hour_p}.fv_core.res.tile${itile}.nc


	/bin/cp ${year}${month}${day}.${hour}.fv_tracer.res.tile${itile}.nc  ${year_m}${month_m}${day_m}.${hour_m}.fv_tracer.res.tile${itile}.nc
	ncatted -O -a filename,global,o,c,"RESTART/${year_m}${month_m}${day_m}.${hour_m}.fv_tracer.res.tile1.nc" ${year_m}${month_m}${day_m}.${hour_m}.fv_tracer.res.tile${itile}.nc
	ncatted -O -a history,global,d,, ${year_m}${month_m}${day_m}.${hour_m}.fv_tracer.res.tile${itile}.nc


	/bin/cp ${year}${month}${day}.${hour}.fv_core.res.tile${itile}.nc  ${year_m}${month_m}${day_m}.${hour_m}.fv_core.res.tile${itile}.nc
	ncatted -O -a filename,global,o,c,"RESTART/${year_m}${month_m}${day_m}.${hour_m}.fv_core.res.tile1.nc" ${year_m}${month_m}${day_m}.${hour_m}.fv_core.res.tile${itile}.nc
	ncatted -O -a history,global,d,, ${year_m}${month_m}${day_m}.${hour_m}.fv_tracer.res.tile${itile}.nc


	((itile=itile+1))

    done

done
