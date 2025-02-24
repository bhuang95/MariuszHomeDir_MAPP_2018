#!/bin/ksh 
#SBATCH -n 1
#SBATCH -t 00:10:00
#SBATCH -q debug
#SBATCH -A chem-var
#SBATCH -J calc_cntrlincr
#SBATCH -D /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/scripts
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/qslogs/%x.e%j

#sbatch --export=ALL,ident=2018041500 sbatch_calc_control_increment.sh

. /etc/profile

. ~/MAPP_2018/.environ_ksh

set -x

MAINDIR=/scratch1/NCEPDEV/da/Cory.R.Martin/aero_c96_jedi3densvar
TMPDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/tmpdir
OUTDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/from_cory/control

cycle_frequency=6

ndate=~/bin/ndate

analdate=$ident

yeara=`echo $analdate | cut -c1-4`
montha=`echo $analdate | cut -c5-6`
daya=`echo $analdate | cut -c7-8`
houra=`echo $analdate | cut -c9-10`
adatea=${yeara}${montha}${daya}

fcstdate=`$ndate -$cycle_frequency $ident`
yearf=`echo $fcstdate | cut -c1-4`
monthf=`echo $fcstdate | cut -c5-6`
dayf=`echo $fcstdate | cut -c7-8`
hourf=`echo $fcstdate | cut -c9-10`
adatef=${yearf}${monthf}${dayf}

INDIR=${MAINDIR}/gdas.${adatef}/${hourf}

itile=1
while [[ $itile -le 6 ]]
do

    echo $itile

    ncdiff -O  ${INDIR}/RESTART/${adatea}.${houra}0000.fv_tracer.res.tile${itile}.nc ${INDIR}/RESTART/${adatea}.${houra}0000.fv_tracer.res.tile${itile}.nc.ges ${OUTDIR}/${adatea}.${houra}0000.fv_tracer.incr.tile${itile}.nc

    ncpdq -O -a -zaxis_1  ${OUTDIR}/${adatea}.${houra}0000.fv_tracer.incr.tile${itile}.nc ${OUTDIR}/${adatea}.${houra}0000.fv_tracer.incr.tile${itile}_rev.nc

    /bin/cp ${INDIR}/RESTART/${adatea}.${houra}0000.fv_tracer.res.tile${itile}.nc $OUTDIR/${adatea}.${houra}0000.fv_tracer.anal.tile${itile}.nc

    ncpdq -O -a -zaxis_1 $OUTDIR/${adatea}.${houra}0000.fv_tracer.anal.tile${itile}.nc $OUTDIR/${adatea}.${houra}0000.fv_tracer.anal.tile${itile}_rev.nc

    ((itile=itile+1))
done

exit 0
