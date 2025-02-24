#!/bin/ksh

indir=/scratch1/portfolios/BMC/chem-var/pagowski/wgne/brasil/surface-inmet
infile=is2012-09-01-0000

outdir=.
outfile=wgn_sfc.txt

wgne_sfc4met.x ${indir}/${infile} ${outdir}/${outfile}


