#!/bin/ksh

. /etc/profile

. ../.environ.ksh

start_date=2021080100
end_date=2021080118

cycle_frequency=6

maindir=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski
execdir=${maindir}/exec

workdir=${maindir}/tmpdir/workdir_geos2p
geosdir=/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/MODEL/geos
indir=${geosdir}/akbkll
outdir=${geosdir}/pll

ndate=~/bin/ndate

if [[ ! -r $workdir ]]
then
    mkdir -p $workdir
fi

/bin/cp ${execdir}/geos2p.x $workdir

cd $workdir

ndate=~/bin/ndate

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hour=`echo "${ident}" | cut -c9-10`

    outfile="geos_aeros_${year}${month}${day}${hour}_pll.nc"

#forget about aod for now
#    ln -sf ${indir}/GEOS.fp.asm.inst3_3d_aer_Nv.${year}${month}${day}_${hour}00.V01.nc4 ${outdir}/geos_aods_${year}${month}${day}_ll.nc

cat > geos2p.nl <<EOF
&record_input
 date="${year}${month}${day}${hour}"
 input_geos_dir="${indir}"
 fname_geos="GEOS.fp.asm.inst3_3d_aer_Nv.${year}${month}${day}_${hour}00.V01.nc4"
 fname_akbk="/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/fix_geos/akbk72.nc4"
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

    ./geos2p.x

    ident=`$ndate +${cycle_frequency} $ident`

done
