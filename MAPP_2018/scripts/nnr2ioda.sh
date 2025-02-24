#!/bin/ksh

. /etc/profile

. ../.environ.ksh

satellite='MYD04' #aqua
satellite='MOD04' # terra

obstype=deep
#obstype=land
#obstype=ocean
thin_spatial=".F."
cycle_frequency=3
grid=C192

start_date=2015123121
end_date=2017010100

if [[ $satellite == 'MYD04' ]]
then
    satid='aqua'
else
    satid='terra'
fi

maindir=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski
execdir=${maindir}/exec

maindir_obs=${maindir}/DATA/OBS/NNR_003_6Targets
prefix=nnr_003.${satellite}_L2a.${obstype}
suffix=ods

workdir=${maindir}/tmpdir/workdir_nnr2ioda_${satellite}


if [[ ! -r $workdir ]]
then
    mkdir -p $workdir
fi

/bin/cp ${execdir}/nnr2ioda.x $workdir

cd $workdir

ndate=~/bin/ndate

repeat=0
ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hr=`echo "${ident}" | cut -c9-10`

    indir_grid_thin=${maindir}/fix_fv3/${grid}

    obsident=$ident

    obsyear=`echo "${obsident}" | cut -c1-4`
    obsmonth=`echo "${obsident}" | cut -c5-6`
    obsday=`echo "${obsident}" | cut -c7-8`
    obshr=`echo "${obsident}" | cut -c9-10`

    indir_obs=${maindir_obs}/${satellite}/Y${obsyear}/M${obsmonth}
    infile_obs=${prefix}.${obsyear}${obsmonth}${obsday}_${obshr}00z.${suffix}


    outdir=.

#    if [[ $thin_spatial == ".T." ]]
#    then
#	outdir=${maindir_obs}/${satellite}/Y${obsyear}/M${obsmonth}_thinned_${grid}
#    else
#	outdir=${maindir_obs}/${satellite}/Y${obsyear}/M${obsmonth}_unthinned
#    fi

#    if [[ ! -r $outdir ]]
#    then
#	mkdir -p $outdir
#    fi

    ((offset=hr%6))

    if [[ ${offset} == 0 ]]
    then
	off_center=0
	repeat=0
	interval=$cycle_frequency
	outfile=nnr_${satid}_${obstype}.${ident}.nc
    else
	((repeat=repeat+1))
	if [[ $repeat == 1 ]]
	then
	    newdate=`$ndate -$cycle_frequency $ident`
	    off_center=-1
	    interval=0
	    outfile=nnr_${satid}_${obstype}.${newdate}+.nc
	else 
	    newdate=`$ndate +$cycle_frequency $ident`
	    repeat=0
	    off_center=1
	    interval=$cycle_frequency
	    outfile=nnr_${satid}_${obstype}.${newdate}-.nc
	fi
    fi

cat > nnr2ioda.nl <<EOF
&record_obs
 input_dir_obs = "$indir_obs"
 input_file_obs = "$infile_obs"
 select_domain = .false.
 lat_ll = -90.0
 lat_ur = 90.0
 lon_ll = -180.0
 lon_ur = 180.0
/

&record_obs_thinning
 thin_spatial = $thin_spatial
 center_date_time = "$ident" 
 off_center = $off_center
 input_dir_grid_thinning = "$indir_grid_thin"
 thinning_grid_ratio_min = .75
 thinning_grid_ratio_max = 1.5
 time_half_window = 90 ! minutes from center_date_time all
/

&record_output
 output_multichannel = .F.
 correct_bias = .F.
 output_dir = "$outdir"
 output_file = "$outfile"
/

EOF

    echo $outfile

    ./nnr2ioda.x

    ident=`$ndate $interval $ident`

done
