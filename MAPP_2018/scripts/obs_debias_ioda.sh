#!/bin/ksh

#to add  total seas and dust to CAMS species

. /etc/profile

. ../.environ.ksh

start_date=2016060100
end_date=2016063018

#start_date=2016050100
#end_date=2016053118

cycle_frequency=6

grid=C192

maindir=/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/OBS

oldobsdir=${maindir}/VIIRS/thinned_${grid}
newobsdir=${maindir}/VIIRS/thinned_debiased_${grid}
obs=viirs_aod_snpp

#oldobsdir=${maindir}/NNR_003_6Targets/MOD04_thinned_${grid}
#newobsdir=${maindir}/NNR_003_6Targets/MOD04_thinned_debiased_${grid}
#obs=nnr_terra

#oldobsdir=${maindir}/NNR_003_6Targets/MYD04_thinned_${grid}
#newobsdir=${maindir}/NNR_003_6Targets/MYD04_thinned_debiased_${grid}
#obs=nnr_aqua

ndate=~/bin/ndate

ident=$start_date

while [[ $ident -le $end_date ]]
do

    echo $ident

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hr=`echo "${ident}" | cut -c9-10`

    indir=${oldobsdir}/${year}${month}
    outdir=${newobsdir}/${year}${month}

    if [[ ! -r $outdir ]]
    then
	mkdir -p $outdir
    fi

    cd $outdir

    ln -sf ${indir}/${obs}.${ident}.nc ${obs}.${ident}_old.nc

    ncrename -O -v aerosol_optical_depth_4@ObsValue,aerosol_optical_depth_4_ObsValue -v aerosol_optical_depth_4@KnownObsBias,aerosol_optical_depth_4_KnownObsBias -v aerosol_optical_depth_4@ObsBias,aerosol_optical_depth_4_ObsBias ${obs}.${ident}_old.nc tmp1.nc

    ncap2 -O -s "aerosol_optical_depth_4_ObsValue=(float(aerosol_optical_depth_4_ObsValue-aerosol_optical_depth_4_KnownObsBias));where(aerosol_optical_depth_4_ObsValue < 0.) aerosol_optical_depth_4_ObsValue=float(0);aerosol_optical_depth_4_ObsBias=float(aerosol_optical_depth_4_ObsBias-aerosol_optical_depth_4_ObsBias);aerosol_optical_depth_4_KnownObsBias=float(aerosol_optical_depth_4_KnownObsBias-aerosol_optical_depth_4_KnownObsBias)" tmp1.nc tmp2.nc

    ncrename -O -v aerosol_optical_depth_4_ObsValue,aerosol_optical_depth_4@ObsValue -v aerosol_optical_depth_4_KnownObsBias,aerosol_optical_depth_4@KnownObsBias -v aerosol_optical_depth_4_ObsBias,aerosol_optical_depth_4@UnKnownObsBias tmp2.nc ${obs}.${ident}.nc

    ncatted -O -a history,global,d,, ${obs}.${ident}.nc

    ident=`$ndate +${cycle_frequency} $ident`

done

rm -f *tmp*
