#!/bin/ksh

low_bound=0.000
high_bound=0.059

#low_bound=0.059
#high_bound=0.077

#low_bound=0.077
#high_bound=0.109

#low_bound=0.109
#high_bound=5.000


test=DA_ENKF_${low_bound}_${high_bound}


#test=BCKG_012
#test=BCKG_024

obstype=deep
obstype=land
obstype=ocean

ident=201508
b_boar=0.15

maindir_obs=/scratch3/BMC/fim/MAPP_2018/OBS/NNR_003_6Targets
prefix_in=nnr_fv3.${obstype}.covar_hl

indir=${maindir_obs}/hl_${test}
outdir=${indir}

year=`echo "${ident}" | cut -c1-4`
month=`echo "${ident}" | cut -c5-6`

infile=${prefix_in}.${ident}.nc
outfile=${prefix_in}_final.${ident}.nc

cat > namelist.modis_nnr_hl_final <<EOF
&record_inout
 input_dir = "$indir"
 input_file = "$infile"
 output_dir = "$outdir"
 output_file = "$outfile"
 b_boar=$b_boar
/
EOF

./aodnnr_hl_final.x
