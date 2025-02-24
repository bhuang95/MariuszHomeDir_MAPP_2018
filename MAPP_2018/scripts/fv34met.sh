#!/bin/ksh

. /etc/profile

start_date=2016060918
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

    
#fv3 files are analyses

    echo "fv32pll processing"

    outfile_tmp="fv3_tmp_pll.nc"

cat > fv32pll.nl <<EOF
&record_input
 date="${year}${month}${day}${hour}"
 input_grid_dir="${maindir}/fix_fv3/${grid}"
 fname_grid="grid_spec.tile?.nc"
 input_fv3_dir="${indir}"
 fname_fv3_tracer="${year}${month}${day}.${hour}0000.fv_tracer.res.tile?.nc.ges"
 fname_fv3_core="${year}${month}${day}.${hour}0000.fv_core.res.tile?.nc.ges"
 fname_akbk="${year}${month}${day}.${hour}0000.fv_core.res.nc.ges"
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
 output_dir="."
 fname_pll="${outfile_tmp}"
/
EOF

    . ~/mapp_2018/.environ.ksh

    ./fv32pll.x

    . ~/.nc

    echo "nco processing"

    outfile=${outdir}/"fv3_aeros_${year}${month}${day}${hour}_pll.nc"


    ncap2 -O -s "CPHOBIC=float(BCPHOBIC+OCPHOBIC);CPHILIC=float(BCPHILIC+OCPHILIC);CTOTAL=float(BCPHOBIC+OCPHOBIC+BCPHILIC+OCPHILIC)" ${outfile_tmp}  ${outfile}

    ncatted -O -a long_name,CPHOBIC,o,c,"Hydrophobic Carbon Aerosol Total Mixing Ratio" $outfile
    ncatted -O -a long_name,CPHILIC,o,c,"Hydrophilic Carbon Aerosol Total Mixing Ratio" $outfile
    ncatted -O -a long_name,CTOTAL,o,c,"Carbon Aerosol Total Mixing Ratio" $outfile

    ncatted -O -a history,global,d,, $outfile

    rm -f $outfile_tmp

    echo "AOD processing"

    outfile="fv3_aods_${year}${month}${day}${hour}_ll.nc"

cat > fv3aod2ll.nl <<EOF
&record_input
 date="${year}${month}${day}${hour}"
 input_grid_dir="${maindir}/fix_fv3/${grid}"
 fname_grid="grid_spec.tile?.nc"
 input_fv3_dir="${indir}"
 fname_fv3="${year}${month}${day}.${hour}0000.fv_aod_LUTs_v.viirs-m_npp.res.tile?.nc"
! fname_fv3="${year}${month}${day}.${hour}0000.fv_aod_LUTs_v.modis_aqua.res.tile?.nc.ges"
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

    . ~/mapp_2018/.environ.ksh

    ./fv3aod2ll.x

    ident=`$ndate +${cycle_frequency} $ident`

done
