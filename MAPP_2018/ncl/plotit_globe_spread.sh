#!/bin/ksh

SCRIPTDIR=~/mapp_2018/ncl

NCLDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/ncl

MAINDIR=$NCLDIR

DATADIR=${MAINDIR}/indata_fv3_spread

unit=" [kg m-2 s-1]"

species=duem_std
species_title="Spread: Dust Emissions "
limitlow="1.e-10"
limithigh="1.e-9"
factor=1.e9
ncons=9

species=ssem_std
species_title="Spread: Sea-salt Emissions "
limitlow="1.e-10"
limithigh="1.e-9"
factor=1.e9
ncons=9

species=bcembb_std
species_title="Spread: Black Carbon Biogenic Emissions "
limitlow="1.e-12"
limithigh="1.e-11"
factor=1.e11
ncons=9

species=bceman_std
species_title="Spread: Black Carbon Anthropogenic Emissions "
limitlow="1.e-10"
limithigh="1.e-9"
factor=1.e9
ncons=9

species=ocembb_std
species_title="Spread: Organic Carbon Biogenic Emissions "
limitlow="1.e-13"
limithigh="1.e-12"
factor=1.e12
ncons=9

species=oceman_std
species_title="Spread: Organic Carbon Anthropogenic Emissions "
limitlow="1.e-13"
limithigh="1.e-12"
factor=1.e12
ncons=9

species=so2embb_std
species_title="Spread: SO2 Biogenic Emissions "
limitlow="1.e-11"
limithigh="1.e-10"
factor=1.e10
ncons=9

species=so2eman_std
species_title="Spread: SO2 Anthropogenic Emissions "
limitlow="1.e-12"
limithigh="1.e-11"
factor=1.e11
ncons=9

unit="kg m^-2"

species=aecmass_std
species_title="Spread: Total Aerosol Column Integral "
limitlow="1.e-6"
limithigh="1.e-5"
factor=1.e5
ncons=9

species=ducmass_std
species_title="Spread: Dust Column Integral "
limitlow="1.e-7"
limithigh="1.e-6"
factor=1.e6
ncons=9

species=sscmass_std
species_title="Spread: Sea-salt Column Integral "
limitlow="1.e-7"
limithigh="1.e-6"
factor=1.e6
ncons=9

species=bccmass_std
species_title="Spread: Black Carbon Column Integral "
limitlow="1.e-8"
limithigh="1.e-7"
factor=1.e7
ncons=9

species=occmass_std
species_title="Spread: Organic Carbon Column Integral "
limitlow="1.e-7"
limithigh="1.e-6"
factor=1.e6
ncons=9

species=sucmass_std
species_title="Spread: Sulfate Column Integral "
limitlow="1.e-8"
limithigh="1.e-7"
factor=1.e7
ncons=9

start_ident=2016100300
end_ident=2016100400

cycle_frequency=1

ndate=~/bin/ndate
ident=$start_ident
itime=0

while [[ ${ident} -le ${end_ident} ]]
do
    year=`echo $ident | cut -c1-4`
    month=`echo $ident | cut -c5-6`
    day=`echo $ident | cut -c7-8`
    hour=`echo $ident | cut -c9-10`

    echo $ident

    indir=${DATADIR}
    datestring=${ident}
    ntiles=6
    
    cd ${indir}

    itile=1
    while [[ $itile -le $ntiles ]]
    do 
	file_in=fv3_history2d_spread.tile${itile}.nc
	ln -sf ${file_in} fv_spread.tile${itile}.nc
	((itile=itile+1))
    done

    export SPECIES=$species
    export TITLE="$species_title ${ident}"
    export LIMITLOW=$limitlow
    export LIMITHIGH=$limithigh
    export NCONS=$ncons
    export TIME=$itime
    export FACTOR=$factor

    cd $SCRIPTDIR

    ncl plotit_globe_spread.ncl

    /bin/mv spread.png ${NCLDIR}/pics_spread/${species}_${ident}.png

    ident=`ndate +${cycle_frequency} ${ident}`
    ((itime=itime+1))

done
