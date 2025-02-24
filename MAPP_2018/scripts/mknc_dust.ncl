load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin

   pathlon=getenv("PATHLON")
   pathlat=getenv("PATHLAT")
   pathsand=getenv("PATHSAND")
   pathclay=getenv("PATHCLAY")
   pathrdrag=getenv("PATHRDRAG")
   pathssm=getenv("PATHSSM")
   pathuthr=getenv("PATHUTHR")

   nlon=toint(getenv("GRID"))
   nlat=toint(getenv("GRID"))	

   tile=toint(getenv("TILE"))
   month=getenv("MONTH")

   nrec = 0
   dims = (/nlon,nlat/)

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
  filo = "dust_data.tile.nc"            ; Output file
  system("/bin/rm -f " + diro + filo)      ; remove if exists
  fout = addfile (diro + filo, "c")        ; open output file
  ;============================================================
  fout@title       ="Fengsha Dust scheme input (C$GRID$, $month$, tile$TILE$)"
  fout->lat      = lat
  fout->lon      = lon
  fout->clay     = clay
  fout->rdrag    = rdrag
  fout->sand     = sand
  fout->ssm      = ssm
  fout->uthr     = uthr

end
