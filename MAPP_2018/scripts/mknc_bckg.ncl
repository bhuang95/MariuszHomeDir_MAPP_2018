load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin

   pathlon=getenv("PATHLON")
   pathlat=getenv("PATHLAT")
   pathlev=getenv("PATHLEV")
   pathh2o2=getenv("PATHH2O2")
   pathno3=getenv("PATHNO3")
   pathoh=getenv("PATHOH")

   nlon=toint(getenv("GRID"))
   nlat=toint(getenv("GRID"))	
   nlev=toint(getenv("LEV"))

   tile=toint(getenv("TILE"))
   month=getenv("MONTH")

   nrec = 0

   dims = (/nlev,nlon,nlat/)

   setfileoption("bin","ReadByteOrder","LittleEndian")

   lon     = fbinrecread(pathlon,    nrec, nlon, "float")
   lat     = fbinrecread(pathlat,    nrec, nlat, "float")
   z       = fbinrecread(pathlev,    nrec, nlev, "float")
   h2o2    = fbinrecread(pathh2o2,   nrec, dims, "float")
   no3     = fbinrecread(pathno3,    nrec, dims, "float")
   oh      = fbinrecread(pathoh,     nrec, dims, "float")

   lat!0   = "lat"
   lat&lat = lat

   lon!0   = "lon"
   lon&lon = lon

   z!0     = "z"
   z&z     =  z

   h2o2!0="z"
   h2o2!1="lat"
   h2o2!2="lon"
   h2o2&z    = z
   h2o2&lat  = lat
   h2o2&lon  = lon

   no3!0="z"
   no3!1="lat"
   no3!2="lon"
   no3&z    = z
   no3&lat  = lat
   no3&lon  = lon

   oh!0="z"
   oh!1="lat"
   oh!2="lon"
   oh&z    = z
   oh&lat  = lat
   oh&lon  = lon

  ;============================================================
  ; create the output file
  ;============================================================
  diro = "./"                              ; Output directory
  filo = "emi2_data.tile.nc"            ; Output file
  system("/bin/rm -f " + diro + filo)      ; remove if exists
  fout = addfile (diro + filo, "c")        ; open output file
  ;============================================================
  fout@title       ="3D background input (C$GRID$, $month$, tile$TILE$)"

  fout->z        = z
  fout->lat      = lat
  fout->lon      = lon
  fout->h2o2     = h2o2
  fout->no3      = no3
  fout->oh       = oh

end









































