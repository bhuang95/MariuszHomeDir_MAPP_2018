#!/bin/ksh --login
#PBS -N aodnnr_hl
#PBS -A chem-var
#PBS -l procs=24
#PBS -l walltime=00:30:00
#PBS -q debug
#PBS -d /home/Mariusz.Pagowski/mapp_2018/src
#PBS -o /scratch3/BMC/fim/MAPP_2018/qslogs
#PBS -e /scratch3/BMC/fim/MAPP_2018/qslogs

. /etc/profile
. /apps/lmod/lmod/init/sh
. ../.modules

test=DA_ENKF

satellite='MYD04' #aqua
satellite='MOD04' # terra

obstype=deep
obstype=land
obstype=ocean

start_date=2015080500
end_date=2015081712
 
cycle_frequency=6

logaod=".F."
epsilon=0.0
b_boar=0.5
calc_scales=".F."

maindir_obs=/scratch3/BMC/fim/MAPP_2018/OBS/NNR_003_6Targets
prefix_in=nnr_fv3.${satellite}.${obstype}
prefix_covar=nnr_fv3.${satellite}.${obstype}.covar_hl

workdir=${maindir_obs}/tmplh

indir=${maindir_obs}/4hl_${test}
outdir=${maindir_obs}/hl_${test}

if [[ ! -r $outdir ]]
then
    mkdir -p $outdir
fi

if [[ ! -r $workdir ]]
then
    mkdir -p $workdir
fi

/bin/cp aodnnr_hl.x $workdir

cd $workdir

ndate=~/bin/ndate

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hr=`echo "${ident}" | cut -c9-10`

    infile=${prefix_in}.${ident}.nc

    outfile_covar=${prefix_covar}.${ident}.nc

cat > namelist.modis_nnr_hl <<EOF
&record_inout
 input_dir = "$indir"
 input_file = "$infile"
 output_dir = "$outdir"
 output_file_covar = "$outfile_covar"
 logaod = $logaod
 epsilon = $epsilon 
 b_boar=$b_boar
 calc_scales=$calc_scales
/

EOF

    echo $ident

    ./aodnnr_hl.x

    ident=`$ndate $cycle_frequency $ident`

done

