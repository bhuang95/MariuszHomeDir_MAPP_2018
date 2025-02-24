#!/bin/ksh

. /etc/profile

. ../.environ.ksh

start_date=2016060100
end_date=2016063018

cycle_frequency=6

grid=C96

maindir=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski
execdir=${maindir}/exec

workdir=${maindir}/tmpdir/workdir_fv32ll
fv3dir=/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/MODEL/fv3
indir=${fv3dir}/akbk
outdir=${fv3dir}/pll

ndate=~/bin/ndate

if [[ ! -r $workdir ]]
then
    mkdir -p $workdir
fi

/bin/cp ${execdir}/fv32pll.x ${execdir}/fv3aod2ll.x $workdir

cd $workdir

ndate=~/bin/ndate

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hour=`echo "${ident}" | cut -c9-10`

    outfile="fv3_aeros_${year}${month}${day}${hour}_pll.nc"

#fv3 files are analyses

cat > fv32pll.nl <<EOF
&record_input
 date="${year}${month}${day}${hour}"
 input_grid_dir="${maindir}/fix_fv3/${grid}"
 fname_grid="grid_spec.tile?.nc"
 input_fv3_dir="${indir}"
 fname_fv3_tracer="${year}${month}${day}.${hour}0000.fv_tracer.res.tile?.nc"
 fname_fv3_core="${year}${month}${day}.${hour}0000.fv_core.res.tile?.nc"
 fname_akbk="${year}${month}${day}.${hour}0000.fv_core.res.nc"
/
&record_interp
!varlist_in is only for illustration since translation is hard-coded
!and will not aggregate correctly if all species not present
 varlist_in= "bc1","bc2","dust1","dust2","dust3","dust4","dust5","oc1","oc\
2","seas1","seas2","seas3","seas4","seas5","sulf"
!varlist_out is only for illustration since translation is hard-coded
!and will not aggregate correctly if all species not present
 varlist_out= "BCPHOBIC","BCPHILIC","DUSTFINE","DUSTMEDIUM","DUSTCOARSE","DUSTTOTAL","OCPHOBIC","OCPHILIC","SEASFINE","SEASMEDIUM","SEASCOARSE","SEASTOTAL","SO4"
 plist = 100.,250.,400.,500.,600.,700.,850.,925.,1000.
 dlon=0.5
 dlat=0.5
/
&record_output
 output_dir="${outdir}"
 fname_pll="${outfile}"
/
EOF

    echo $outfile

    ./fv32pll.x

    outfile="fv3_aods_${year}${month}${day}${hour}_ll.nc"

cat > fv3aod2ll.nl <<EOF
&record_input
 date="${year}${month}${day}${hour}"
 input_grid_dir="${maindir}/fix_fv3/${grid}"
 fname_grid="grid_spec.tile?.nc"
 input_fv3_dir="${indir}"
! fname_fv3="${year}${month}${day}.${hour}0000.fv_aod_v.viirs-m_npp.res.tile?.nc"
 fname_fv3="${year}${month}${day}.${hour}0000.fv_aod_v.viirs-m_npp.res.tile?.nc_anl"
! fname_fv3="${year}${month}${day}.${hour}0000.fv_aod_v.modis_aqua.res.tile?.nc_anl"
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

    ./fv3aod2ll.x

    ident=`$ndate +${cycle_frequency} $ident`

done
