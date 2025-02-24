#!/bin/sh

hour=00 #06

BASEDIR=.
INDIR=../indata
OUTDIR=../outdata
infile=${INDIR}/wrfchem_params_${hour}z.txt
outfile=${OUTDIR}/wrfchem_pm2_5_reg_berror_${hour}z.bin

cd $BASEDIR

./makeberror_wrfchem_pm2_5.x $infile $outfile
