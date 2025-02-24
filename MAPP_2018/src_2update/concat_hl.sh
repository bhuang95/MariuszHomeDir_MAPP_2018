#!/bin/ksh

low_bound=0.000
high_bound=0.059

low_bound=0.059
high_bound=0.077

low_bound=0.077
high_bound=0.109

low_bound=0.109
high_bound=5.000


test=DA_ENKF_${low_bound}_${high_bound}

#test=DA_ENKF
#test=BCKG_012


#not surprisinly: for both satellites covars are similar - concat both sateelites together

#obstype=deep
#obstype=land
obstype=ocean

cd /scratch3/BMC/fim/MAPP_2018/OBS/NNR_003_6Targets/hl_${test}

satellite=MYD04

ncecat -O nnr_fv3.${satellite}.${obstype}.covar_hl.201508????.nc nnr_fv3.${satellite}.${obstype}.covar_hl.201508.nc

satellite=MOD04

ncecat -O nnr_fv3.${satellite}.${obstype}.covar_hl.201508????.nc nnr_fv3.${satellite}.${obstype}.covar_hl.201508.nc

ncecat -O nnr_fv3.*.${obstype}.covar_hl.201508????.nc nnr_fv3.${obstype}.covar_hl.201508.nc

