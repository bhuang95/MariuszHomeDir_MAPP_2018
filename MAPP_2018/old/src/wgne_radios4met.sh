#!/bin/ksh

indir=/scratch1/portfolios/BMC/chem-var/pagowski/wgne/brasil/radiosonde
infile=TEMP-201209010000_201209010000.dat

outdir=.
outfile=wgn_radios.txt

wgne_radios4met.x ${indir}/${infile} ${outdir}/${outfile}


