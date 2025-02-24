load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin

   pathlon=getenv("PATHLON")
   pathlat=getenv("PATHLAT")
   pathebc=getenv("PATHEBC")
   patheoc=getenv("PATHEOC")
   patheco=getenv("PATHECO")
   pathesulf=getenv("PATHESULF")
   pathepm10=getenv("PATHEPM10")
   pathepm25=getenv("PATHEPM25")
   patheso2=getenv("PATHESO2")
   pathdm0=getenv("PATHDM0")
   pathero1=getenv("PATHERO1")
   pathero2=getenv("PATHERO2")
   pathero3=getenv("PATHERO3")



   nlon=toint(getenv("GRID"))
   nlat=toint(getenv("GRID"))	

   tile=toint(getenv("TILE"))
   month=getenv("MONTH")

   nrec = 0
   dims = (/nlon,nlat/)

   setfileoption("bin","ReadByteOrder","LittleEndian")

   lon     = fbinrecread(pathlon,    nrec, nlon, "float")
   lat     = fbinrecread(pathlat,    nrec, nlat, "float")
   e_bc    = fbinrecread(pathebc,    nrec, dims, "float")
   e_oc    = fbinrecread(patheoc,    nrec, dims, "float")
   e_co    = fbinrecread(patheco,    nrec, dims, "float")
   e_sulf  = fbinrecread(pathesulf,  nrec, dims, "float")
   e_pm_10 = fbinrecread(pathepm10,  nrec, dims, "float")
   e_pm_25 = fbinrecread(pathepm25,  nrec, dims, "float")
   e_so2   = fbinrecread(patheso2,   nrec, dims, "float")
   dm0     = fbinrecread(pathdm0,    nrec, dims, "float")
   ero1    = fbinrecread(pathero1,   nrec, dims, "float")
   ero2    = fbinrecread(pathero2,   nrec, dims, "float")
   ero3    = fbinrecread(pathero3,   nrec, dims, "float")


   lat!0   = "lat"
   lat&lat = lat

   lon!0   = "lon"
   lon&lon = lon


   e_bc!0="lat"
   e_bc!1="lon"
   e_bc&lat  = lat
   e_bc&lon  = lon

   e_oc!0="lat"
   e_oc!1="lon"
   e_oc&lat  = lat
   e_oc&lon  = lon

   e_co!0="lat"
   e_co!1="lon"
   e_co&lat  = lat
   e_co&lon  = lon

   e_sulf!0="lat"
   e_sulf!1="lon"
   e_sulf&lat  = lat
   e_sulf&lon  = lon

   e_pm_10!0="lat"
   e_pm_10!1="lon"
   e_pm_10&lat  = lat
   e_pm_10&lon  = lon

   e_pm_25!0="lat"
   e_pm_25!1="lon"
   e_pm_25&lat  = lat
   e_pm_25&lon  = lon

   e_so2!0="lat"
   e_so2!1="lon"
   e_so2&lat  = lat
   e_so2&lon  = lon

   dm0!0="lat"
   dm0!1="lon"
   dm0&lat  = lat
   dm0&lon  = lon

   ero1!0="lat"
   ero1!1="lon"
   ero1&lat  = lat
   ero1&lon  = lon

   ero2!0="lat"
   ero2!1="lon"
   ero2&lat  = lat
   ero2&lon  = lon

   ero3!0="lat"
   ero3!1="lon"
   ero3&lat  = lat
   ero3&lon  = lon

  ;============================================================
  ; create the output file
  ;============================================================
  diro = "./"                              ; Output directory
  filo = "emi_data.tile.nc"            ; Output file
  system("/bin/rm -f " + diro + filo)      ; remove if exists
  fout = addfile (diro + filo, "c")        ; open output file
  ;============================================================
  fout@title       ="Anthropogenic emission input (C$GRID$, $month$, tile$TILE$)"
  fout->lat      = lat
  fout->lon      = lon
  fout->e_bc     = e_bc
  fout->e_oc     = e_oc
  fout->e_co     = e_co
  fout->e_sulf   = e_sulf
  fout->e_pm_10  = e_pm_10
  fout->e_pm_25  = e_pm_25
  fout->e_so2    = e_so2
  fout->dm0      = dm0
  fout->ero1     = ero1
  fout->ero2     = ero2
  fout->ero3     = ero3

end









































