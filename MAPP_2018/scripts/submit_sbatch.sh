#!/bin/ksh

ndate=~/bin/ndate

cycle_frequency=24

start_date=2019072900
end_date=2019072918

ident=$start_date

while [[ $ident -le $end_date ]]
do
    echo $ident
    start=$ident
    end=`$ndate +18 $start`
#    sbatch --export=ALL,start=$start,end=$end sbatch_nems2nc.sh
#    sbatch --export=ALL,start=$start,end=$end sbatch_chgres_nrt.sh
    sbatch --export=ALL,start=$start,end=$end sbatch_nemssfc2nc.sh
    ident=`$ndate +${cycle_frequency} $ident`
done

