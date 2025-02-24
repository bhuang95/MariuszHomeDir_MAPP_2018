 subroutine get_eta_level(npz, p_s, pf, ph, ak, bk, pscale)
  integer, intent(in) :: npz    
  real, intent(in)  :: p_s            !< unit: pascal
  real, intent(in)  :: ak(npz+1)
  real, intent(in)  :: bk(npz+1)
  real, intent(in), optional :: pscale
  real, intent(out) :: pf(npz)
  real, intent(out) :: ph(npz+1)
  integer k

  ph(1) = ak(1)               
  do k=2,npz+1
     ph(k) = ak(k) + bk(k)*p_s
  enddo                           
   
  if ( present(pscale) ) then
      do k=1,npz+1
         ph(k) = pscale*ph(k)
      enddo
  endif 

  if( ak(1) > 1.E-8 ) then   
     pf(1) = (ph(2) - ph(1)) / log(ph(2)/ph(1))
  else
     pf(1) = (ph(2) - ph(1)) * kappa/(kappa+1.)
  endif

  do k=2,npz
     pf(k) = (ph(k+1) - ph(k)) / log(ph(k+1)/ph(k))
  enddo

 end subroutine get_eta_level
