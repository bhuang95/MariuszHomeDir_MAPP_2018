load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

	
  title=getenv("TITLE")
  limitlow=tofloat(getenv("LIMITLOW"))*1.e-6
  limithigh=tofloat(getenv("LIMITHIGH"))*1.e-6
  ncons=toint(getenv("NCONS"))


  obsfile   = addfile("./indata_obs/nucaps_cofile.nc","r")        ; lons, lats, NUCAPS_CO

  lats  =   ndtooned(obsfile->$"latitude@MetaData"$)    ; Pull off lat
  lons  =   ndtooned(obsfile->$"longitude@MetaData"$)    ; Pull off lon variables
  nucaps_co   =   ndtooned(obsfile->$"co_column@ObsValue"$) ; Pull off NUCAPS_CO variables

  nucaps_comax = limithigh
  nucaps_comaxm = nucaps_comax -0.01*nucaps_comax

  nucaps_co = where(nucaps_co.gt.nucaps_comax,nucaps_comaxm,nucaps_co)

  nlevels = ncons+1
  nlevelsm1 = nlevels-1

  levels = fspan(limitlow,nucaps_comax,nlevels)

  wks = gsn_open_wks("png","nucaps_co")

  res                     = True
  res@gsnMaximize         = False         ; Maximize size of plot in frame

  res@tiMainFontHeightF = 0.02

  res@tiMainString        = title

  res@mpProjection = "CylindricalEquidistant"
  res@mpDataBaseVersion     = "MediumRes"      ; Improve the resolution
                                               ; of the map outlines 
                                               ; Default is "LowRes".

  res@mpOutlineOn = True ; (True - turn off continental outlines)
  res@mpFillOn = False ; (True - turn on map fill)


  res@mpPerimOn             = False       ; Turn off map perimeter
  res@mpCenterLatF          =  0.         ; Rotate map.
  res@mpCenterLonF          =  0.  ;180.

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

     ii = ind(levels(i).le.nucaps_co.and.nucaps_co.lt.levels(i+1))   

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
  lbres@lbTitleString       = "kg"
  lbres@lbTitlePosition  = "bottom"

 

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
