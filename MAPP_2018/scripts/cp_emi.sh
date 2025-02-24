#!/bin/ksh

indir=/scratch3/BMC/fim/Mariusz.Pagowski/FV3_CHEM/C48_CEDS
outdir=/scratch3/BMC/fim/Mariusz.Pagowski/FV3_CHEM/INDATA/emi_C48/04

cd $indir

date=2018-04-01-000000
for it in 1 2 3 4 5 6 
do
dir=tile${it}
newdir=${outdir}/${dir}

if [[ ! -r $newdir ]]
then
    mkdir -p $newdir
fi

/bin/cp $dir/C48-CEDS-T-$date-ALD-ab.bin $newdir/e_ald.dat
/bin/cp $dir/C48-CEDS-T-$date-ASH-ab.bin $newdir/e_ash.dat
/bin/cp $dir/C48-CEDS-T-$date-BC-ab.bin $newdir/e_bc.dat
/bin/cp $dir/C48-CEDS-T-$date-CO-ab.bin $newdir/e_co.dat
/bin/cp $dir/C48-CEDS-T-$date-CSL-ab.bin $newdir/e_csl.dat
/bin/cp $dir/C48-CEDS-T-$date-DMS-ab.bin $newdir/e_dms.dat
/bin/cp $dir/C48-CEDS-T-$date-ETH-ab.bin $newdir/e_eth.dat
/bin/cp $dir/C48-CEDS-T-$date-HC3-ab.bin $newdir/e_hc3.dat
/bin/cp $dir/C48-CEDS-T-$date-HC5-ab.bin $newdir/e_hc5.dat
/bin/cp $dir/C48-CEDS-T-$date-HC8-ab.bin $newdir/e_hc8.dat
/bin/cp $dir/C48-CEDS-T-$date-HCHO-ab.bin $newdir/e_hcho.dat
/bin/cp $dir/C48-CEDS-T-$date-ISO-ab.bin $newdir/e_iso.dat
/bin/cp $dir/C48-CEDS-T-$date-KET-ab.bin $newdir/e_ket.dat
/bin/cp $dir/C48-CEDS-T-$date-NH3-ab.bin $newdir/e_nh3.dat
/bin/cp $dir/C48-CEDS-T-$date-NO2-ab.bin $newdir/e_no2.dat
/bin/cp $dir/C48-CEDS-T-$date-NO-ab.bin $newdir/e_no.dat
/bin/cp $dir/C48-CEDS-T-$date-OC-ab.bin $newdir/e_oc.dat
/bin/cp $dir/C48-CEDS-T-$date-OLI-ab.bin $newdir/e_oli.dat
/bin/cp $dir/C48-CEDS-T-$date-OLT-ab.bin $newdir/e_olt.dat
/bin/cp $dir/C48-CEDS-T-$date-ORA2-ab.bin $newdir/e_ora2.dat
/bin/cp $dir/C48-CEDS-T-$date-SO2-ab.bin $newdir/e_so2.dat
/bin/cp $dir/C48-CEDS-T-$date-SO4-ab.bin $newdir/e_sulf.dat
/bin/cp $dir/C48-CEDS-T-$date-TOL-ab.bin $newdir/e_tol.dat
/bin/cp $dir/C48-CEDS-T-$date-URBAN2-ab.bin $newdir/e_pm_25.dat
/bin/cp $dir/C48-CEDS-T-$date-URBAN3-ab.bin $newdir/e_pm_10.dat
/bin/cp $dir/C48-CEDS-T-$date-XYL-ab.bin $newdir/e_xyl.dat
/bin/cp $dir/C48-CEDS-T-$date-gocartBG-DMS.bin $newdir/dm0.dat
/bin/cp $dir/C48-CEDS-T-$date-gocartBG-H2O2.bin $newdir/h2o2.dat
/bin/cp $dir/C48-CEDS-T-$date-gocartBG-OH.bin $newdir/oh.dat
/bin/cp $dir/C48-CEDS-T-$date-gocartBG-NO3.bin $newdir/no3.dat
/bin/cp $dir/C48-CEDS-T-$date-gocartBG-erod1.bin $newdir/erod1.dat
/bin/cp $dir/C48-CEDS-T-$date-gocartBG-erod2.bin $newdir/erod2.dat
/bin/cp $dir/C48-CEDS-T-$date-gocartBG-erod3.bin $newdir/erod3.dat
/bin/cp $dir/C48-CEDS-T-$date-SAND.bin $newdir/sand.dat
/bin/cp $dir/C48-CEDS-T-$date-CLAY.bin $newdir/clay.dat

done
