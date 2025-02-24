load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

  title=getenv("TITLE")
  limitlow=tofloat(getenv("LIMITLOW"))
  limithigh=tofloat(getenv("LIMITHIGH"))
  ncons=toint(getenv("NCONS"))

  obsfile   = addfile("./indata_obs/aodfile.nc","r")        ; lons, lats, AOD

  lats  =   ndtooned(obsfile->$"MetaData/latitude"$)    ; Pull off lat
  lons  =   ndtooned(obsfile->$"MetaData/longitude"$)    ; Pull off lon variables
  aod   =   ndtooned(obsfile->$"ObsValue/aerosol_optical_depth"$) ; Pull off AOD variables

  aodmax = limithigh
  aodmaxm = aodmax-0.01*aodmax

  aod = where(aod.gt.aodmax,aodmaxm,aod)

  nlevels = ncons+1
  nlevelsm1 = nlevels-1

  levels = fspan(limitlow,aodmax,nlevels)

  wks = gsn_open_wks("png","aod")

  res                     = True
  res@gsnMaximize         = False         ; Maximize size of plot in frame

  res@tiMainFontHeightF = 0.02

  res@tiMainString        = title

  res@mpProjection          = "Orthographic"
  res@mpCenterLatF          =  40.         ; Rotate map.
  res@mpCenterLonF          =  -100. ;180.

  res@mpDataBaseVersion     = "MediumRes"      ; Improve the resolution
                                               ; of the map outlines 
                                               ; Default is "LowRes".

  res@mpOutlineOn = True ; (True - turn off continental outlines)
  res@mpFillOn = False ; (True - turn on map fill)


  res@mpPerimOn             = False       ; Turn off map perimeter

  res@pmTickMarkDisplayMode = "Always"    ; Turn on map tickmarks

  res@gsnDraw  = False
  res@gsnFrame = False

  map = gsn_csm_map(wks,res)

  palette="wh-bl-gr-ye-re"

  gsn_define_colormap(wks,palette)	

  icols = span_color_indexes(palette,nlevels)
  rcols = span_color_rgba(palette,nlevels)

  dum = new(nlevelsm1,graphic)

  mkres = True

  do i=0,nlevels-2

     ii = ind(levels(i).le.aod.and.aod.lt.levels(i+1))   

     if (ismissing(ii(0))) then
        delete(ii)
     	continue
     end if	

     mkres@gsMarkerIndex    = 16          ; Filled dots
     mkres@gsMarkerSizeF    = 0.0025
     mkres@gsMarkerColor    = rcols(i,:)

     dum(i) = gsn_add_polymarker(wks,map,lons(ii),lats(ii),mkres)

     delete(ii)

  end do

  font_height = 0.02

  labels = "" + levels ;(2::2)

  lbres                    = True      ; Set up a resource list for the labelbar.
  lbres@vpWidthF           = 0.5
  lbres@vpHeightF          = 0.08

  lbres@lbBoxLinesOn       = False
  lbres@lbFillColors       = rcols ;(2::2,:)
  lbres@lbMonoFillPattern  = True

  lbres@lbOrientation      = "Horizontal"
  lbres@lbPerimOn          = False

  lbres@lbLabelFontHeightF = 0.015
  lbres@lbLabelAutoStride  = True
  lbres@lbLabelAlignment   = "InteriorEdges"
 

;  lbid = gsn_create_labelbar(wks,dimsizes(levels(2::2)),labels,lbres)
  lbid = gsn_create_labelbar(wks,dimsizes(levels),labels,lbres)

  amres                  = True
  amres@amZone           = 2
  amres@amParallelPosF   =  0.5
  amres@amOrthogonalPosF =  0.15
  annoid = gsn_add_annotation(map,lbid,amres)

  draw(map)

  frame(wks)

end
