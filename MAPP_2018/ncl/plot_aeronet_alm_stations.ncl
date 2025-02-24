load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

  title = "AERONET stations"

  freq_min = 0
  freq_max = 60
  ncons = freq_max/10

  fd  = addfile("./indata_aeronet/aeronet_alm_lonlats.nc","r")

  lons  :=  fd->longitude(:)
  lats  :=  fd->latitude(:)
  freqs :=  fd->frequency(:)

  freqs_max = max(freqs)
  print(freqs_max)

  dfreq = (freq_max-freq_min)/ncons
  freq_minm = freq_min + dfreq
  freq_maxm = freq_max - dfreq

  freqs := where(freqs.gt.freq_maxm,freq_maxm,freqs)
  freqs := where(freqs.lt.freq_minm,freq_minm,freqs)

  nlevels = ncons+1
  nlevelsm1 = nlevels-1

  levels = fspan(0,freq_max,nlevels)

  wks = gsn_open_wks("png","./pics_aeronet/aeronet_alm_stations")

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

     ii = ind(levels(i).le.freqs.and.freqs.lt.levels(i+1))   

     if (ismissing(ii(0))) then
        delete(ii)
     	continue
     end if	

     mkres@gsMarkerIndex    = 16          ; Filled dots
     mkres@gsMarkerSizeF    = 0.005
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

  lbres@lbLabelFontHeightF = 0.0135
  lbres@lbLabelAutoStride  = True
  lbres@lbLabelAlignment   = "InteriorEdges"
  lbres@lbTitleString        = "          Number of retrievals"
  lbres@lbTitlePosition  = "bottom"
  lbres@lbTitleFontHeightF= .0135
  lbres@lbTitleJust = "CenterCenter"


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
