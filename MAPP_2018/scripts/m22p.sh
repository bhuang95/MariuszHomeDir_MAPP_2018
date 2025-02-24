#!/bin/ksh

. /etc/profile

. ../.environ.ksh

start_date=2016060100
end_date=2016063018

cycle_frequency=24

maindir=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski
execdir=${maindir}/exec

workdir=${maindir}/tmpdir/workdir_m22p
m2dir=/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/MODEL/m2
indir=${m2dir}/akbkll
outdir=${m2dir}/pll

ndate=~/bin/ndate

if [[ ! -r $workdir ]]
then
    mkdir -p $workdir
fi

/bin/cp ${execdir}/m22p.x $workdir

cd $workdir

ndate=~/bin/ndate

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hr=`echo "${ident}" | cut -c9-10`

    outfile="m2_aeros_${year}${month}${day}_pll.nc"

    ln -sf ${indir}/MERRA2_400.inst3_2d_gas_Nx.${year}${month}${day}.nc4 ${outdir}/m2_aods_${year}${month}${day}_ll.nc

cat > m22p.nl <<EOF
&record_input
 date="${year}${month}${day}"
 input_m2_dir="/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/MODEL/m2/akbkll"
 fname_m2="MERRA2_400.inst3_3d_aer_Nv.${year}${month}${day}.nc4"
 fname_akbk="/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/fix_m2/akbk72.nc4"
/
&record_interp
!varlist_in is only for illustration since translation is hard-coded
!and will not aggregate correctly if all species not present
 varlist_in= "BCPHOBIC","BCPHILIC","DU001","DU002","DU003","DU004","DU005","OCPHOBIC","OCPHILIC","SS001","SS002","SS003","SS004","SS005","SO4"

!varlist_out is only for illustration since translation is hard-coded
!and will not aggregate correctly if all species not present
 varlist_out= "BCPHOBIC","BCPHILIC","DUSTFINE","DUSTMEDIUM","DUSTCOARSE","DUSTTOTAL","OCPHOBIC","OCPHILIC","SEASFINE","SEASMEDIUM","SEASCOARSE","SEASTOTAL","SO4"
 plist = 100.,250.,400.,500.,600.,700.,850.,925.,1000.
/
&record_output
 output_dir="${outdir}"
 fname_pll="${outfile}"
/
EOF

    echo $outfile

    ./m22p.x

    ident=`$ndate +${cycle_frequency} $ident`

done
