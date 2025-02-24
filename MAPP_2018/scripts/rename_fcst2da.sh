ncrename -O -v seas1,seas6 -v seas2,seas1 -v seas3,seas2 -v seas4,seas3 -v seas5,seas4 20180411.000000.fv_tracer.res.tile1.nc 20180411.000000.fv_tracer.res.tile1_tmp.nc
ncrename -O -v seas6,seas5 20180411.000000.fv_tracer.res.tile1_tmp.nc 20180411.000000.fv_tracer.res.tile1_fcst2da.nc
#ncpdq -O -a -zaxis_1 20180411.000000.fv_tracer.res.tile1_fcst2da.nc 20180411.000000.fv_tracer.res.tile1_fcst2da_rev.nc
