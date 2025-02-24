#!/usr/bin/env python
import subprocess
import os
import datetime
import multiprocessing

yyyymmddhh_start = os.getenv('yyyymmddhh_start')
yyyymmddhh_end = os.getenv('yyyymmddhh_end')

startdate = datetime.datetime(int(yyyymmddhh_start[:4]),int(yyyymmddhh_start[4:6]),int(yyyymmddhh_start[6:8]),int(yyyymmddhh_start[8:10]))

enddate = datetime.datetime(int(yyyymmddhh_end[:4]),int(yyyymmddhh_end[4:6]),int(yyyymmddhh_end[6:8]),int(yyyymmddhh_end[8:10]))

execcnvt = '/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/exec/nemsioatm2nc.x'
nemsDir = '/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/DATA/MET_ANALYSES/nemsio/control/'
ncDir = '/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/DATA/MET_ANALYSES/ncdf/control/'

RootMP=nemsDir
RootMe=ncDir

nproc = multiprocessing.cpu_count()

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
  
  try:
    os.makedirs(RootMe+nowdate.strftime('%Y%m%d%H'))
  except:
    pass

  outcfile = RootMe+nowdate.strftime('%Y%m%d%H')+'/gdas.t'+nowdate.strftime('%H')+'z.atmanl.nc'

  infiles.append(RootMP+nowdate.strftime('%Y%m%d%H')+
                 '/gdas.t'+nowdate.strftime('%H') + 'z.atmanl.ensres.nemsio') 
  outfiles.append(RootMe+nowdate.strftime('%Y%m%d%H')+'/gdas.t'+nowdate.strftime('%H')+'z.atmanl.nc') 

  nowdate = nowdate + datetime.timedelta(hours=6)

p = multiprocessing.Pool(nproc)
p.map(run_convert,range(0,len(infiles)))
p.close()
p.join()
