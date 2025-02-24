load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin
   nlon = 384
   nlat = 384

   pathlon     = "/scratch1/BMC/gsd-fv3-dev/Haiqin.Li/Develop/emi_C384/lon/lon_tile1.dat"
   pathlat     = "/scratch1/BMC/gsd-fv3-dev/Haiqin.Li/Develop/emi_C384/lat/lat_tile1.dat"
   pathsand    = "/scratch1/BMC/gsd-fv3-dev/lzhang/emi_C384/fengsha/10/tile1/sand.dat"
   pathclay    = "/scratch1/BMC/gsd-fv3-dev/lzhang/emi_C384/fengsha/10/tile1/clay.dat"
   pathrdrag   = "/scratch1/BMC/gsd-fv3-dev/lzhang/emi_C384/fengsha/10/tile1/rdrag.dat"
   pathssm     = "/scratch1/BMC/gsd-fv3-dev/lzhang/emi_C384/fengsha/10/tile1/ssm.dat"
   pathuthr    = "/scratch1/BMC/gsd-fv3-dev/lzhang/emi_C384/fengsha/10/tile1/uthr.dat"

   nrec = 0
   dims = (/384,384/)

   setfileoption("bin","ReadByteOrder","LittleEndian")

   lon    = fbinrecread(pathlon,    nrec, nlon, "float")
   lat    = fbinrecread(pathlat,    nrec, nlat, "float")
   sand   = fbinrecread(pathsand,   nrec, dims, "float")
   clay   = fbinrecread(pathclay,   nrec, dims, "float")
   rdrag  = fbinrecread(pathrdrag,  nrec, dims, "float")
   ssm    = fbinrecread(pathssm,    nrec, dims, "float")
   uthr   = fbinrecread(pathuthr,   nrec, dims, "float")

   lat!0   = "lat"
   lat&lat = lat

   lon!0   = "lon"
   lon&lon = lon


   sand!0="lat"
   sand!1="lon"
   sand&lat  = lat
   sand&lon  = lon

   clay!0="lat"
   clay!1="lon"
   clay&lat  = lat
   clay&lon  = lon

   rdrag!0="lat"
   rdrag!1="lon"
   rdrag&lat  = lat
   rdrag&lon  = lon

   ssm!0="lat"
   ssm!1="lon"
   ssm&lat  = lat
   ssm&lon  = lon

   uthr!0="lat"
   uthr!1="lon"
   uthr&lat  = lat
   uthr&lon  = lon
                                                                  
  ;============================================================
  ; create the output file
  ;============================================================
  diro = "./"                              ; Output directory
  filo = "dust_data.tile1.nc"            ; Output file
  system("/bin/rm -f " + diro + filo)      ; remove if exists
  fout = addfile (diro + filo, "c")        ; open output file
  ;============================================================
  fout@title       ="Fengsha Dust scheme input (C384, 10, tile1)"
  fout->lat      = lat
  fout->lon      = lon
  fout->clay     = clay
  fout->rdrag    = rdrag
  fout->sand     = sand
  fout->ssm      = ssm
  fout->uthr     = uthr

end
