      MODULE varinfo
        INTEGER nvls, npls
        TYPE var_attr
          CHARACTER(len=128) var_description 
          CHARACTER(len=128) units 
          CHARACTER(len=128) var_postname 
          INTEGER nlevels
        END TYPE var_attr

     CONTAINS
! subroutine to obtain the variable attributes information through variable symbol
          SUBROUTINE var_info(var_symbol, var_attr_info)
          IMPLICIT NONE

          CHARACTER(len=*), INTENT(IN)  :: var_symbol
          TYPE(var_attr), INTENT(OUT) ::var_attr_info

          SELECT CASE (var_symbol)
      
! 3D variables for dynamics 
          CASE ("temp")
          var_attr_info%var_description = "Temperature"
          var_attr_info%units = "Kelvin"
          var_attr_info%var_postname ="temp"
          var_attr_info%nlevels = nvls

          CASE ("ucomp")
          var_attr_info%var_description = "U wind"
          var_attr_info%units = "meter/second"
          var_attr_info%var_postname ="ucom"
          var_attr_info%nlevels = nvls 

          CASE ("vcomp")
          var_attr_info%var_description = "V wind"
          var_attr_info%units = "meter/second"
          var_attr_info%var_postname ="vcom"
          var_attr_info%nlevels = nvls

          CASE ("w")
          var_attr_info%var_description = "Vertical velocity"
          var_attr_info%units = "Meter/second"
          var_attr_info%var_postname ="wcom"
          var_attr_info%nlevels = nvls

          CASE ("delz")
          var_attr_info%var_description = "Height thickness"
          var_attr_info%units = "meter"
          var_attr_info%var_postname ="delz"
          var_attr_info%nlevels = nvls

          CASE ("delp")
          var_attr_info%var_description = "3-D Delta Pressure"
          var_attr_info%units = "pascals"
          var_attr_info%var_postname ="delp"
          var_attr_info%nlevels = nvls

          CASE ("nhpres")
          var_attr_info%var_description = "3-D Pressure"
          var_attr_info%units = "pascals"
          var_attr_info%var_postname ="nhpr"
          var_attr_info%nlevels = nvls 

          CASE ("clwmr")
          var_attr_info%var_description = "cloud/water mixing ratio"
          var_attr_info%units = "kg/kg"
          var_attr_info%var_postname ="cwmr"
          var_attr_info%nlevels = nvls

          CASE ("sphum")
          var_attr_info%var_description = "Specific humidity"
          var_attr_info%units = "kg/kg"
          var_attr_info%var_postname ="sphm"
          var_attr_info%nlevels = nvls

          CASE ("o3mr")
          var_attr_info%var_description = "ozone mixing ratio"
          var_attr_info%units = "kg/kg"
          var_attr_info%var_postname ="o3mr"
          var_attr_info%nlevels = nvls

!2D variables for physics

          CASE ("AOD")
          var_attr_info%var_description = "AOD at 550 nm"
          var_attr_info%units = ""
          var_attr_info%var_postname ="AOD"
          var_attr_info%nlevels = 1

          CASE ("sn2D")
          var_attr_info%var_description = "snow water equivalent"
          var_attr_info%units = "meter"
          var_attr_info%var_postname ="sn2D"
          var_attr_info%nlevels = 1

          CASE ("PRATEsfc")
          var_attr_info%var_description = "sfc precip. rate"
          var_attr_info%units = "millimeter"
          var_attr_info%var_postname ="sprt"
          var_attr_info%nlevels = 1

          CASE ("CPRATsfc")
          var_attr_info%var_description = "sfc convect. precip. rate"
          var_attr_info%units = "kg/m^2/s"
          var_attr_info%var_postname ="scpr"
          var_attr_info%nlevels = 1

          CASE ("PWATclm")
          var_attr_info%var_description = "atmos column precip. water"
          var_attr_info%units = "millimeter"
          var_attr_info%var_postname ="acpw"
          var_attr_info%nlevels = 1

          CASE ("ts2D")
          var_attr_info%var_description = "skin temperature"
          var_attr_info%units = "deg. Kelvin"
          var_attr_info%nlevels = 1

          CASE ("hf2D")
          var_attr_info%var_description = "sensible heat flux"
          var_attr_info%units = "watt/meter^2"
          var_attr_info%nlevels = 1

          CASE ("qf2D")
          var_attr_info%var_description = "water vapor flux"
          var_attr_info%units = "kg/meter^2"
          var_attr_info%nlevels = 1

          CASE ("lw2D")
          var_attr_info%var_description = "lw"
          var_attr_info%units = "kg/meter^2"
          var_attr_info%nlevels = 1

          CASE ("PRESsfc")
          var_attr_info%var_description = "surface pressure"
          var_attr_info%units = "Pa"
          var_attr_info%var_postname ="sfcp"
          var_attr_info%nlevels = 1

          CASE ("ct2D")
          var_attr_info%var_description = "cloud top height"
          var_attr_info%units = "m" 
          var_attr_info%nlevels = 1

          CASE ("cb2D")
          var_attr_info%var_description = "cloud base height"
          var_attr_info%units = "m"
          var_attr_info%nlevels = 1

          CASE ("id2D")
          var_attr_info%var_description = "integrated fine dust"
          var_attr_info%units = "ug/kg"
          var_attr_info%nlevels = 1

          CASE ("is2D")
          var_attr_info%var_description = "integrated sulfate"
          var_attr_info%units = "ppm"
          var_attr_info%nlevels = 1
 
          CASE ("h850")
          var_attr_info%var_description = "height at 850mb"
          var_attr_info%units = "meter"
          var_attr_info%var_postname ="h850"
          var_attr_info%nlevels = 1
 
          CASE ("h500")
          var_attr_info%var_description = "height at 500mb"
          var_attr_info%units = "meter"
          var_attr_info%var_postname ="h500"
          var_attr_info%nlevels = 1
 
          CASE ("h200")
          var_attr_info%var_description = "height at 200mb"
          var_attr_info%units = "meter"
          var_attr_info%var_postname ="h200"
          var_attr_info%nlevels = 1
 
          CASE ("u850")
          var_attr_info%var_description = "u-wind at 850mb"
          var_attr_info%units = "meter/s"
          var_attr_info%var_postname ="u850"
          var_attr_info%nlevels = 1
 
          CASE ("u500")
          var_attr_info%var_description = "u-wind at 500mb"
          var_attr_info%units = "meter/s"
          var_attr_info%var_postname ="u500"
          var_attr_info%nlevels = 1
 
          CASE ("u200")
          var_attr_info%var_description = "u-wind at 200mb"
          var_attr_info%units = "meter/s"
          var_attr_info%var_postname ="u200"
          var_attr_info%nlevels = 1
 
          CASE ("v850")
          var_attr_info%var_description = "v-wind at 850mb"
          var_attr_info%units = "meter/s"
          var_attr_info%var_postname ="v850"
          var_attr_info%nlevels = 1
 
          CASE ("v500")
          var_attr_info%var_description = "v-wind at 500mb"
          var_attr_info%units = "meter/s"
          var_attr_info%var_postname ="v500"
          var_attr_info%nlevels = 1
 
          CASE ("v200")
          var_attr_info%var_description = "v-wind at 200mb"
          var_attr_info%units = "meter/s"
          var_attr_info%var_postname ="v200"
          var_attr_info%nlevels = 1


! output variables at standard pressure levels
          CASE ("hgtP")
          var_attr_info%var_description = "height at pressure levels"
          var_attr_info%units = "meter2/second2"
          var_attr_info%nlevels = 40

          CASE ("tmpP")
          var_attr_info%var_description = &
            "temperature at pressure levels"
          var_attr_info%units = "deg"
          var_attr_info%nlevels = 40

          CASE ("rh3P")
          var_attr_info%var_description = "Relative humidity"
          var_attr_info%units = "percentage"
          var_attr_info%nlevels = 40

          CASE ("uw3P")
          var_attr_info%var_description = "U wind"
          var_attr_info%units = "meter.second"
          var_attr_info%nlevels = 40 

          CASE ("vw3P")
          var_attr_info%var_description = "V wind"
          var_attr_info%units = "meter.second"
          var_attr_info%nlevels = 40


          CASE default
          write(6,*)'var_info: unknown input variable:', trim(var_symbol)
          STOP

          END SELECT  

        END SUBROUTINE var_info

! subroutine to set number of levels for the model
        SUBROUTINE set_model_nlevels(nlevels)
          INTEGER nlevels
          nvls = nlevels
        END SUBROUTINE set_model_nlevels

        SUBROUTINE set_model_nplevels(nplevels)
          INTEGER nplevels
          npls = nplevels
        END SUBROUTINE set_model_nplevels

      
      END MODULE varinfo
