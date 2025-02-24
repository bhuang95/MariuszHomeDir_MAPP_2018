#!/bin/ksh

. /etc/profile

. ~/.nc

satellite='MYD04' #aqua
satellite='MOD04' # terra

start_date=2016010100
end_date=2017010100


ident=$start_date

year=`echo "${ident}" | cut -c1-4`
month=`echo "${ident}" | cut -c5-6`
day=`echo "${ident}" | cut -c7-8`
hr=`echo "${ident}" | cut -c9-10`

if [[ $satellite == 'MYD04' ]]
then
    satid='aqua'
else
    satid='terra'
fi

grid=C192
cycle_frequency=6

typeset -A obstypes 
obstypes[ocean]="ocean"
obstypes[land]="land"
obstypes[deep]="deep"


maindir=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski

maindir_obs=${maindir}/DATA/OBS/NNR_003_6Targets/all
outdir=${maindir_obs}/${satellite}

if [[ ! -r $outdir ]]
then
    mkdir -p $outdir
fi

#from nnr2ioda.sh 
workdir=${maindir}/tmpdir/workdir_nnr2ioda_${satellite}


if [[ ! -r $workdir ]]
then
    echo "$workdir does not exist: run nnr2ioda.sh first"
fi


cd $workdir

ndate=~/bin/ndate

((halfcycle=cycle_frequency / 2))

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hr=`echo "${ident}" | cut -c9-10`

    set -A listback \
        ./nnr_${satid}_${obstypes[ocean]}.${ident}-.nc \
        ./nnr_${satid}_${obstypes[land]}.${ident}-.nc \
        ./nnr_${satid}_${obstypes[deep]}.${ident}-.nc \

    set -A listcenter \
	./nnr_${satid}_${obstypes[ocean]}.${ident}.nc \
        ./nnr_${satid}_${obstypes[land]}.${ident}.nc \
        ./nnr_${satid}_${obstypes[deep]}.${ident}.nc 

    set -A listfor \
        ./nnr_${satid}_${obstypes[ocean]}.${ident}+.nc \
        ./nnr_${satid}_${obstypes[land]}.${ident}+.nc \
        ./nnr_${satid}_${obstypes[deep]}.${ident}+.nc \

    set -A filelist ${listback[*]} ${listcenter[*]} ${listfor[*]}

#    if [[ $ident == $start_date ]]
#    then
#	set -A filelist ${listcenter[*]} ${listfor[*]}
#    elif [[ $ident == $end_date ]]
#    then
#	set -A filelist ${listback[*]} ${listcenter[*]}
#    else
#	set -A filelist ${listback[*]} ${listcenter[*]} ${listfor[*]}
#    fi
    

    ncrcat -O ${filelist[*]} ${outdir}/nnr_${satid}.${ident}.nc
    if [[ $? -ne 0 ]]
    then
	echo "ncrcat failed - files missing"
	echo "ncrcat smaller subset for $ident"
	ncrcat -O nnr_${satid}_*.${ident}*.nc ${outdir}/nnr_${satid}.${ident}.nc
    fi

    
    echo $ident

    ident=`$ndate $cycle_frequency $ident`

done

/bin/rm -f ${outdir}/nnr_${satid}.*tmp
