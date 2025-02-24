#!/bin/ksh

year=2015
month=08
day=10
hr=00

end_year=2015
end_month=08
end_day=10
end_hr=00

start_date=${year}${month}${day}${hr}
end_date=${end_year}${end_month}${end_day}${end_hr}

INDIR=/scratch3/BMC/chem-var/pagowski/tmp/viirs_hdf/SVMs/001
OUTDIR=/scratch3/BMC/chem-var/pagowski/tmp/viirs_hdf/SVM

. ~/bin/funcs.sh

date=$start_date

while [[ ${date} -le ${end_date} ]]
    do

    echo $date

    mkdir -p $OUTDIR/SVM03/${year}${month}${day} $OUTDIR/SVM04/${year}${month}${day} $OUTDIR/SVM05/${year}${month}${day}
    
    /bin/mv ${INDIR}/SVM03_npp_d${year}${month}${day}* $OUTDIR/SVM03/${year}${month}${day}
    /bin/mv ${INDIR}/SVM04_npp_d${year}${month}${day}* $OUTDIR/SVM04/${year}${month}${day}
    /bin/mv ${INDIR}/SVM05_npp_d${year}${month}${day}* $OUTDIR/SVM05/${year}${month}${day}

    increment_date 24

    year=${end_year}
    month=${end_month}
    day=${end_day}
    hr=${end_hr}

    date=${year}${month}${day}${hr}

done
