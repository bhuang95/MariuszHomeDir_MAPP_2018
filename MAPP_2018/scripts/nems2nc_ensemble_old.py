#!/usr/bin/env python
import subprocess
import os
import datetime
import multiprocessing

yyyymmddhh_start = os.getenv('yyyymmddhh_start')
yyyymmddhh_end = os.getenv('yyyymmddhh_end')

startdate = datetime.datetime(int(yyyymmddhh_start[:4]),int(yyyymmddhh_start[4:6]),int(yyyymmddhh_start[6:8]),int(yyyymmddhh_start[8:10]))

enddate = datetime.datetime(int(yyyymmddhh_end[:4]),int(yyyymmddhh_end[4:6]),int(yyyymmddhh_end[6:8]),int(yyyymmddhh_end[8:10]))

execcnvt = '/work/noaa/gsd-fv3-dev/pagowski/exec/nemsioatm2nc.x'
nemsDir = '/work/noaa/gsd-fv3-dev/pagowski/DATA/MET_ANALYSES/nemsio/'
ncDir = '/work/noaa/gsd-fv3-dev/pagowski/DATA/MET_ANALYSES/ncdf/'
RootMP=nemsDir
RootMe=ncDir

nproc = multiprocessing.cpu_count()
nmems = 40

infiles = []
outfiles = []
nowdate = startdate

def run_convert(idx):
  #command = execcnvt+' '+infiles[idx]+' '+outfiles[idx] # uncompressed 
  command = execcnvt+' '+infiles[idx]+' '+outfiles[idx]+' 14 1' # compression like GFSv16
  print(command)
  err = subprocess.check_call(command, shell=True)
  print(command,err)

while nowdate <= enddate:
  
  inc96file = RootMP+nowdate.strftime('%Y%m%d%H')+'/gdas.'+nowdate.strftime('%Y%m%d%H')+'.atmanl.C96.nc'
  inc192file = RootMP+nowdate.strftime('%Y%m%d%H')+'/gdas.'+nowdate.strftime('%Y%m%d%H')+'.atmanl.C192.nc'

  try:
    os.makedirs(RootMe+nowdate.strftime('%Y%m%d%H')+'/control_C96/')
  except:
    pass

  try:
    os.makedirs(RootMe+nowdate.strftime('%Y%m%d%H')+'/control_C192/')
  except:
    pass

  outc96file = RootMe+nowdate.strftime('%Y%m%d%H')+'/control_C96/gdas.t'+nowdate.strftime('%H')+'z.atmanl.nc'
  outc192file = RootMe+nowdate.strftime('%Y%m%d%H')+'/control_C192/gdas.t'+nowdate.strftime('%H')+'z.atmanl.nc'

  cpstring = 'cp '+inc96file+' '+outc96file
  os.system(cpstring)

  cpstring = 'cp '+inc192file+' '+outc192file
  os.system(cpstring)

  for mem in range(1,nmems+1):
    memstr = "mem{0:03d}".format(mem)
  
    infiles.append(RootMP+nowdate.strftime('%Y%m%d%H')+
                   #'/gdas.t'+nowdate.strftime('%H')+'z.ratmanl.'+memstr+'.nemsio')
                    '/siganl_'+nowdate.strftime('%Y%m%d%H') + '_' + memstr) 
    outfiles.append(RootMe+nowdate.strftime('%Y%m%d%H')+'/'+memstr+'/gdas.t'+nowdate.strftime('%H')+'z.orig.atmanl.nc') 
    try:
      os.makedirs(RootMe+nowdate.strftime('%Y%m%d%H')+'/'+memstr+'/')
    except:
      pass

  nowdate = nowdate + datetime.timedelta(hours=6)

p = multiprocessing.Pool(nproc)
p.map(run_convert,range(0,len(infiles)))
p.close()
p.join()
