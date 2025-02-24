ncrename -O -v seas5,seas6 -v seas4,seas5 -v seas3,seas4 -v seas2,seas3 -v seas1,seas2 20180411.000000.fv_tracer.res.tile1_fcst2da.nc 20180411.000000.fv_tracer.res.tile1_tmp.nc
ncrename -O -v seas6,seas1 20180411.000000.fv_tracer.res.tile1_tmp.nc 20180411.000000.fv_tracer.res.tile1_da2fcst.nc
#ncpdq -O -a -zaxis_1 20180411.000000.fv_tracer.res.tile1_da2fcst.nc 20180411.000000.fv_tracer.res.tile1_da2fcst_rev.nc
