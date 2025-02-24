#!/bin/ksh

. /etc/profile

. ../.environ.ksh

start_date=2016060100
end_date=2016063018

maindir=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski
execdir=${maindir}/exec
modeldir=/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/MODEL

workdir=${maindir}/tmpdir/workdir_integral

model=m2
model=fv3
model=cams

if [[ ${model} == 'fv3' ]]
then
    cycle_frequency=6
else
    cycle_frequency=24
fi

echo $cycle_frequency

indir=${modeldir}/${model}/pll
outdir=$indir

ndate=~/bin/ndate

if [[ ! -r $workdir ]]
then
    mkdir -p $workdir
fi

/bin/cp ${execdir}/calc_col_integrals.x ${execdir}/calc_col_integrals_cams.x $workdir

cd $workdir

ndate=~/bin/ndate

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hr=`echo "${ident}" | cut -c9-10`


    if [[ ${model} == 'fv3' ]] 
    then
	datestring=${year}${month}${day}${hr}
    else
	datestring=${year}${month}${day}
    fi

    infile="${model}_aeros_${datestring}_pll.nc"
    outfile="${model}_aeros_int_${datestring}_pll.nc"

    echo $outfile

    if [[ ${model} == 'cams' ]]
    then

cat > calc_col_integrals_cams.nl <<EOF
&record_input
 date="${datestring}"
 input_dir="${indir}"
 fname_in="${infile}"
 varlist= "aermr01","aermr02","aermr03","aermr04","aermr05","aermr06","aermr07","aermr08","aermr09","aermr10","aermr11","DUSTTOTAL","SEASTOTAL"
/
&record_output
 output_dir="${outdir}"
 fname_out="tmp.nc"
/
EOF

    ./calc_col_integrals_cams.x

    ncap2 -O -s "CPHOBIC_INTEGRAL=float(aermr08_INTEGRAL+aermr10_INTEGRAL);CPHILIC_INTEGRAL=float(aermr07_INTEGRAL+aermr09_INTEGRAL);CTOTAL_INTEGRAL=float(aermr07_INTEGRAL+aermr08_INTEGRAL+aermr09_INTEGRAL+aermr10_INTEGRAL)" ${outdir}/tmp.nc ${outdir}/${outfile}

    rm -rf ${outdir}/tmp.nc

    else

cat > calc_col_integrals.nl <<EOF
&record_input
 input_dir="${indir}"
 fname_in="${infile}"
 varlist= "BCPHOBIC","BCPHILIC","DUSTFINE","DUSTMEDIUM","DUSTCOARSE","DUSTTOTAL","OCPHOBIC","OCPHILIC","SEASFINE","SEASMEDIUM","SEASCOARSE","SEASTOTAL","SO4"
/
&record_output
 output_dir="${outdir}"
 fname_out="tmp.nc"
/
EOF

    ./calc_col_integrals.x

    ncap2 -O -s "CPHOBIC_INTEGRAL=float(BCPHOBIC_INTEGRAL+OCPHOBIC_INTEGRAL);CPHILIC_INTEGRAL=float(BCPHILIC_INTEGRAL+OCPHILIC_INTEGRAL);CTOTAL_INTEGRAL=float(BCPHOBIC_INTEGRAL+OCPHOBIC_INTEGRAL+BCPHILIC_INTEGRAL+OCPHILIC_INTEGRAL)" ${outdir}/tmp.nc ${outdir}/${outfile}

    rm -rf ${outdir}/tmp.nc

    fi

    ident=`$ndate +${cycle_frequency} $ident`

done
