#!/bin/sh

INDIR=/scratch1/portfolios/BMC/chem-var/pagowski/aeronet/outdata
OUTDIR=../outdata

echo "WRITE AERONET at the top of the file"
#sleep 5

aeronet_outfile=${OUTDIR}/aeronet_data_bundle  

#today=`date +%Y%m%d`
#yesterday=`date -d '-1 day' +%Y%m%d`
dates='2012*'

files=`ls ${INDIR}/${dates}.txt`

/bin/rm -f ${OUTDIR}/*data_bundle

for file in ${files[*]}
do

  echo $file

  intime=`basename $file | sed -e "s/.txt//g"`

  indate=`echo $intime | cut -c1-10`

  indata=${INDIR}/${intime}.txt
  
  outdata=${OUTDIR}/${intime}_aeronet_out.txt
  
  ./aeronet2gsi_bundle.x  $indata $outdata

  cat $outdata >> ${aeronet_outfile}

done

/bin/rm ${OUTDIR}/*.txt

