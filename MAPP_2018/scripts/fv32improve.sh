#!/bin/ksh

. /etc/profile

. ../.environ.ksh

start_date=2016010112
end_date=2016123112

sim="anal"
sim="noda"

((cycle_frequency=24*3))

grid=C96

maindir=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski
execdir=${maindir}/exec

workdir=${maindir}/tmpdir/workdir_fv32improve
improvedir=${maindir}/DATA/OBS/improve
fv3dir=${maindir}/DATA/MODEL/fv3
indir=${fv3dir}/aerospecies_${sim}
outdir=${maindir}/fv32improve/${sim}

ndate=~/bin/ndate

if [[ ! -r $outdir ]]
then
    mkdir -p $outdir
fi

if [[ ! -r $workdir ]]
then
    mkdir -p $workdir
fi

/bin/cp ${execdir}/fv32improve.x $workdir

cd $workdir

ndate=~/bin/ndate

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hour=`echo "${ident}" | cut -c9-10`

    outfile="improve_hofx.${year}${month}${day}${hour}.txt"

cat > fv32improve.nl <<EOF
&record_input
 date="${year}${month}${day}${hour}"
 varlist_aeros="concentration_of_elemental_carbon_in_air","concentration_of_organic_carbon_in_air","concentration_of_seasalt_in_air","concentration_of_sulfate_in_air"
 input_improve_dir="${improvedir}/${year}${month}"
 fname_improve="improve.${year}${month}${day}${hour}.nc4"
 input_grid_dir="${maindir}/fix_fv3/${grid}"
 fname_grid="grid_spec.tile?.nc"
 input_fv3_dir="${indir}"
 fname_fv3_aeros="${year}${month}${day}.${hour}0000.fv_aerospecies.res.tile?.nc"
/
&record_output
 output_dir="${outdir}"
 fname_fv32improve="${outfile}"
/
EOF

    if [[ ! -r ${input_improve_dir}/${fname_improve} ]]
    then
	echo "IMPROVE file ${fname_improve} missing"
        ident=`$ndate +${cycle_frequency} $ident`
	continue
    fi

    echo $outfile

    ./fv32improve.x

    ident=`$ndate +${cycle_frequency} $ident`

done
