#!/bin/ksh

test=DA_ENKF

satellite='MYD04' #aqua
satellite='MOD04' # terra

obstype=deep
#obstype=land
#obstype=ocean
thin_spatial=".F."
fcst_hour=6
cycle_frequency=6

start_date=2015081618
end_date=2015081712

maindir_obs=/scratch3/BMC/fim/MAPP_2018/OBS/NNR_003_6Targets
prefix=nnr_003.${satellite}_L2a.${obstype}
suffix=ods

maindir_model=/scratch3/BMC/chem-var/pagowski/tmp/FV3GFS

nanals=20

workdir=${maindir_obs}/tmp4hl

if [[ ! -r $workdir ]]
then
    mkdir -p $workdir
fi

/bin/cp aodnnr_multichannel_correlation_4hl.x $workdir

cd $workdir

ndate=~/bin/ndate

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hr=`echo "${ident}" | cut -c9-10`

    member=0

    while [[ $member -le $nanals ]]
    do

	if [[ $member == 0 ]]
	then
	    charnanal='ensmean'
	else
	    charnanal=mem`printf %03i $member`
	fi
	
	outdir=${maindir_obs}/4hl_${test}_$charnanal
	
	if [[ ! -r $outdir ]]
	then
	    mkdir -p $outdir
	fi
	
	indir_model=${maindir_model}/FV3_RUNS/${test}/${ident}/${charnanal}/OUTPUT_FV3
	indir_grid=${maindir_model}/FV3_FIX/C192
	
	indir_grid_thin=${maindir_model}/FV3_FIX/C384
	indir_grid_thin=${maindir_model}/FV3_FIX/C192
	indir_grid_thin=${maindir_model}/FV3_FIX/C96
	indir_grid_thin=${maindir_model}/FV3_FIX/C768
	
	obsident=`$ndate +$fcst_hour $ident`
	
	obsyear=`echo "${obsident}" | cut -c1-4`
	obsmonth=`echo "${obsident}" | cut -c5-6`
	obsday=`echo "${obsident}" | cut -c7-8`
	obshr=`echo "${obsident}" | cut -c9-10`
	
	indir_obs=${maindir_obs}/${satellite}/Y${obsyear}/M${obsmonth}
	infile_obs=${prefix}.${obsyear}${obsmonth}${obsday}_${obshr}00z.${suffix}
	
	outfile=nnr_fv3.${satellite}.${obstype}.${ident}.nc
	
cat > namelist.modis_nnr_correlation_4hl <<EOF
&record_obs
 input_dir_obs = "$indir_obs"
 input_file_obs = "$infile_obs"
 select_domain = .false.
 lat_ll = -90.0
 lat_ur = 90.0
 lon_ll = -180.0
 lon_ur = 180.0
/

&record_model
 input_dir_fv3 = "$indir_model"
 input_dir_grid = "$indir_grid"
 yyyymmddhh = "$ident"
 fcst_hour = $fcst_hour
 var_list = "AOD"
/

&record_obs_thinning
 input_dir_grid_thinning = "$indir_grid_thin"
 thinning_grid_ratio_min = .75
 thinning_grid_ratio_max = 1.5
 time_half_window = 90 !minutes
 thin_spatial=$thin_spatial
/

&record_model_interp
 method = "bilinear" ! "none" to exit
/

&record_output
 output_dir = "$outdir"
 output_file = "$outfile"
/

EOF

    echo $outfile

    ./aodnnr_multichannel_correlation_4hl.x

    ((member=member+1))

    done

    ident=`$ndate $cycle_frequency $ident`

done
