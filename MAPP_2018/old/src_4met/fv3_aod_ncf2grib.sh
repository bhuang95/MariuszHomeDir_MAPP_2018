#!/bin/ksh

#set -x 

test=DA_ENKF

FV3DIR=/scratch3/BMC/chem-var/pagowski/tmp/FV3GFS
INDIR=${FV3DIR}/FV3_RUNS/${test}
RUNDIR=${FV3DIR}/tmp_dirs/workdir_aod_grib
FV3POPDIR=/home/Mariusz.Pagowski/codes/fv3pop/pop
RES=192

/bin/rm -rf $RUNDIR

mkdir $RUNDIR
cd $RUNDIR

/bin/cp ${FV3POPDIR}/fv3pop $RUNDIR/fv3pop.x
/bin/cp ${FV3POPDIR}/fv3_gribtable $RUNDIR

start_date=2015080318
end_date=2015081400

cycle_frequency=6
fcst_length=9

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hour=`echo "${ident}" | cut -c9-10`
    
    indir=${INDIR}/${year}${month}${day}${hour}/ensmean/OUTPUT_FV3


cat >'FV3pop.nl' <<EOF
&fv3post
  yyyymmddhhmmss = "${year}${month}${day}${hour}0000"
  cres = ${RES}
  nz = 1
  inputdir = "$indir"
  gridfiledir= "${FV3DIR}/FV3_FIX/C${RES}"
  outputdir = "."
  input_filetype = "aod"
  output_fmt = "grib"
  gribtable = "fv3_gribtable"
  grid_id = 4
  is = 1
  npls = 1
  numvars = 1
  var_list = "AOD"
  tbeg = 1
  tend = $fcst_length
  delta_t = 1
/
EOF

    ./fv3pop.x $indir

    /bin/mv  *.grib $indir

    ident=`ndate +${cycle_frequency} $ident`

done
