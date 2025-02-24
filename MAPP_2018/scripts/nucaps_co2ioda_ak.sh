#!/bin/ksh

. /etc/profile

. ../.environ.ksh

thin_spatial=".F."
cycle_frequency=6
grid=C192

start_date=2019080106
end_date=2019081018


time_half_window=3
((time_half_window_mins=time_half_window*60))

echo $time_half_window_mins

maindir=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski
execdir=${maindir}/exec

maindir_obs=${maindir}/OBS/NUCAPS_AK
prefix=NUCAPS-sciEDR_n20_?m

workdir=${maindir}/tmpdir/workdir_co2ioda
workdirtmp=${workdir}/tmp

rm -rf $workdir
mkdir -p $workdir 
mkdir -p ${workdirtmp}

cd $workdir

/bin/cp ${execdir}/nucaps_co2ioda.x .

ndate=~/bin/ndate

indir_grid_thin=${maindir}/fix_fv3/${grid}

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hr=`echo "${ident}" | cut -c9-10`

    indir_obs=${maindir_obs}/${year}${month}${day}
    outdir_obs=${maindir_obs}/thinned_${grid}/${year}${month}/${day}

    if [[ ! -r $outdir_obs ]]
    then
	mkdir -p $outdir_obs
    fi

    datem=`$ndate -$time_half_window $ident`
    datep=`$ndate +$time_half_window $ident`

    fdate=$datem

    while [[ $fdate -le $datep ]]
    do

	for infile_obs in ${indir_obs}/retfiles/${prefix}_s${fdate}*nc 
	do
	    echo $infile_obs
	    if [[ -s $infile_obs ]]
	    then
		ln -sf $infile_obs .
		infile=`basename $infile_obs`
		echo $infile
		outfile=$infile

cat > nucaps_co2ioda.nl <<EOF
&record_obs
 input_dir_obs = "./"
 input_file_obs = "$infile"
/

&record_obs_thinning
 thin_spatial = $thin_spatial
 center_date_time = "$ident" 
 input_dir_grid_thinning = "$indir_grid_thin"
 thinning_grid_ratio_min = .75
 thinning_grid_ratio_max = 1.5
 time_half_window = $time_half_window_mins ! minutes from center_date_time all
/


&record_output
 output_dir = "$workdirtmp"
 output_file = "$outfile"
/

EOF

                ./nucaps_co2ioda.x

	    fi

	done

	fdate=`$ndate +1 $fdate`

    done

    ncrcat -O ${workdirtmp}/${prefix}*nc ${outdir_obs}/nucaps_co.${ident}.nc
    rm -f ${workdirtmp}/${prefix}*nc

    ident=`$ndate $cycle_frequency $ident`

done
