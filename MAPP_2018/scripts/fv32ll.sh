#!/bin/ksh

. /etc/profile

. ../.environ.ksh

start_date=2016092700
end_date=2016093018

yyyymm=`echo "${start_date}" | cut -c1-6`

cycle_frequency=6

grid=C96

maindir=/work/noaa/gsd-fv3-dev/pagowski
execdir=${maindir}/exec

workdir=${maindir}/tmpdir/workdir_fv32ll
fv3dir=${maindir}/DATA/MODEL/fv3
indir=${fv3dir}/akbk_anal
outdir=${fv3dir}/ll_reanalysis/${yyyymm}

ndate=~/bin/ndate

if [[ ! -r $outdir ]]
then
    mkdir -p $outdir
fi


if [[ ! -r $workdir ]]
then
    mkdir -p $workdir
fi

/bin/cp ${execdir}/fv32ll.x ${execdir}/fv3aod2ll.x ${execdir}/fv3aod2ll_reanalysis.x $workdir

cd $workdir

ndate=~/bin/ndate

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hour=`echo "${ident}" | cut -c9-10`

    outfile="NARA-1.0_aero_${year}${month}${day}${hour}.nc4"

#fv3 files are analyses

cat > fv32ll.nl <<EOF
&record_input
 date="${year}${month}${day}${hour}"
 input_grid_dir="${maindir}/fix_fv3/${grid}"
 fname_grid="grid_spec.tile?.nc"
 input_fv3_dir="${indir}"
 fname_fv3_tracer="${year}${month}${day}.${hour}0000.fv_tracer.res.tile?.nc"
 fname_fv3_core="${year}${month}${day}.${hour}0000.fv_core.res.tile?.nc.ges"
 fname_akbk="${year}${month}${day}.${hour}0000.fv_core.res.nc.ges"
/
&record_interp
 varlist_core= "T"
 varlist_tracer= "sphum","bc1","bc2","dust1","dust2","dust3","dust4","dust5","oc1","oc2","seas1","seas2","seas3","seas4","seas5","sulf"
 dlon=0.5
 dlat=0.5
/
&record_output
 output_dir="${outdir}"
 fname_ll="${outfile}"
/
EOF

    echo $outfile

    ./fv32ll.x

    outfile="NARA-1.0_AOD_${year}${month}${day}${hour}.nc4"

cat > fv3aod2ll.nl <<EOF
&record_input
 date="${year}${month}${day}${hour}"
 input_grid_dir="${maindir}/fix_fv3/${grid}"
 fname_grid="grid_spec.tile?.nc"
 input_fv3_dir="${indir}"
 fname_fv3="${year}${month}${day}.${hour}0000.fv_aod_LUTs_v.modis_aqua.res.tile?.nc"
/
&record_interp
 dlon=0.5
 dlat=0.5
/
&record_output
 output_dir="${outdir}"
 fname_aod_ll="${outfile}"
/
EOF

    echo $outfile

    ./fv3aod2ll_reanalysis.x
#    ./fv3aod2ll.x

    ident=`$ndate +${cycle_frequency} $ident`

done
