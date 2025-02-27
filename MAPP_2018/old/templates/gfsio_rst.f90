!-------------------------------------------------------------------------------
module gfsio_rst
!$$$ module document block
! Module : gfsio_rst  temporary histroy file header and data 
! Abstract
! Program history log
!   2006-10-18 Fanglin Yang
! Public Defined Types
!   gfsio_head         gfsio file header information
!   gfsio_headv        second gfsio file header information
!   gfsio_data         gfsio file data information
!
!$$$ end module document block
!-------------------------------------------------------------------------------
!
  implicit none
  private
  integer,parameter:: intkind=4,realkind=4,dblekind=8
  integer,parameter:: charkind=8
  real(intkind),parameter:: intfill=-9999_intkind
  real(realkind),parameter:: realfill=-9999._realkind
  real(dblekind),parameter:: dblefill=-9999._dblekind
!
  type,public:: gfsio_head
    integer(intkind):: version=intfill
    real(realkind)  :: fhour=realfill
    integer(intkind):: idate(4)=intfill
    integer(intkind):: nrec=intfill
    integer(intkind):: latb=intfill
    integer(intkind):: lonb=intfill
    integer(intkind):: levs=intfill
    integer(intkind):: jcap=intfill
    integer(intkind):: itrun=intfill
    integer(intkind):: iorder=intfill
    integer(intkind):: irealf=intfill
    integer(intkind):: igen=intfill
    integer(intkind):: latf=intfill
    integer(intkind):: lonf=intfill
    integer(intkind):: latr=intfill
    integer(intkind):: lonr=intfill
    integer(intkind):: ntrac=intfill
    integer(intkind):: icen2=intfill
    integer(intkind):: iens(2)=intfill
    integer(intkind):: idpp=intfill
    integer(intkind):: idsl=intfill
    integer(intkind):: idvc=intfill
    integer(intkind):: idvm=intfill
    integer(intkind):: idvt=intfill
    integer(intkind):: idrun=intfill
    integer(intkind):: idusr=intfill
    real(realkind):: pdryini=realfill
    integer(intkind):: ncldt=intfill
    integer(intkind):: ixgr=intfill
    integer(intkind):: nvcoord=intfill
    integer(intkind):: idrt=intfill
  end type gfsio_head

  type,public:: gfsio_headv
    real(realkind),allocatable      :: vcoord(:,:)
    character(charkind),allocatable :: recname(:)
    character(charkind*2),allocatable :: reclevtyp(:)
    integer(intkind),allocatable    :: reclev(:)
  end type gfsio_headv

  type,public:: gfsio_data
    real(realkind),allocatable:: zs(:,:)      !surface height, m
    real(realkind),allocatable:: ps(:,:)      !surface pressure, pa
    real(realkind),allocatable:: p(:,:,:)     !layer pressure, pa  
    real(realkind),allocatable:: dp(:,:,:)    !layer pressure thickness, pa
    real(realkind),allocatable:: t(:,:,:)     !layer temperature, k
    real(realkind),allocatable:: u(:,:,:)     !layer zonal wind, m/s
    real(realkind),allocatable:: v(:,:,:)     !layer meridional wind, m/s
    real(realkind),allocatable:: q(:,:,:,:)   !tracers, 1-spfh; 2-O3; 3-CLW , kg/kg
  end type gfsio_data

  end module gfsio_rst

