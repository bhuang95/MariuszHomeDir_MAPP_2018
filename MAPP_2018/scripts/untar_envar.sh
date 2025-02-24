#!/bin/ksh

. /etc/profile

. ../.environ.ksh

start_date=2018041400
end_date=2018041600

cycle_frequency=6

grid=C96

nanals=8

maindir=/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/envar_2018
indir=${maindir}/${grid}
outdir=${maindir}/${grid}/RESTART
tmpdir=${maindir}/tmp

if [[ ! -r $outdir ]]
then
    mkdir -p $outdir
fi

if [[ ! -r ${outdir}/bkg ]]
then
    mkdir -p ${outdir}/bkg
fi

nanal=1

while [[ $nanal -le $nanals ]]
do
    charnanal=mem`printf %03i $nanal`
    if [[ ! -r ${outdir}/${charnanal} ]]
    then
	mkdir -p ${outdir}/${charnanal}
    fi
    ((nanal=nanal+1))
done


ndate=~/bin/ndate

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hr=`echo "${ident}" | cut -c9-10`

    if [[ ! -r $tmpdir ]]
    then
	mkdir -p $tmpdir
    fi

    cd $tmpdir

    tar -xf ${indir}/gdas.${ident}.tar --strip-components=13 "/scratch1/BMC/gsd-fv3-dev/MAPP_2018/bhuang/JEDI-2020/JEDI-FV3/expRuns/aero_c96_jedi3densvar/dr-data/gdas.${year}${month}${day}/${hr}/RESTART/*ges"

    /bin/mv *ges ${outdir}/bkg

    tar -xf ${indir}/enkfgdas.${ident}.tar --strip-components=12 "/scratch1/BMC/gsd-fv3-dev/MAPP_2018/bhuang/JEDI-2020/JEDI-FV3/expRuns/aero_c96_jedi3densvar/dr-data/enkfgdas.${year}${month}${day}/${hr}/mem00[1-8]/RESTART/*ges"

    nanal=1

    while [[ $nanal -le $nanals ]]
    do
	charnanal=mem`printf %03i $nanal`
	/bin/mv ${tmpdir}/${charnanal}/RESTART/*ges ${outdir}/${charnanal}
	((nanal=nanal+1))
    done

    echo $ident

    ident=`$ndate +${cycle_frequency} $ident`

done

rm -rf ${tmpdir}

cd $outdir

echo renaming files in $outdir

for dir in bkg mem*
do
    cd ${outdir}/${dir}
    echo $dir
    for file in *ges
    do
	newfile=`echo $file | sed  "s/\.ges//g"`
	/bin/mv $file $newfile
    done
done


