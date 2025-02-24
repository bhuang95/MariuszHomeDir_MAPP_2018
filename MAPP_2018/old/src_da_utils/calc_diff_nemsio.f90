PROGRAM calc_diff_nemsio
!calculate difference between two fcsts for NMC method
!script in /home/Mariusz.Pagowski/codes/fv3/util_scripts
  
  USE nemsio_module
  
  IMPLICIT NONE

!  INTEGER, PARAMETER :: ntracers_gocart=18
  INTEGER, PARAMETER :: ntracers_gocart=15
  INTEGER, PARAMETER :: diff_time=12
!char length as in the nemsio declaration below 
  CHARACTER(len=16), DIMENSION(ntracers_gocart), PARAMETER :: tracer_names_gocart=(/&
!       &'dms','so2','msa',&
       &'sulf','bc1','bc2','oc1','oc2','p25',&
       &'dust1','dust2','dust3','dust4','dust5',&
       &'seas1','seas2','seas3','seas4'&
       &/)

  INTEGER, PARAMETER:: DOUBLE=SELECTED_REAL_KIND(p=13,r=200)
  TYPE(nemsio_gfile) :: gfile_1,gfile_2
!
  CHARACTER(500) :: filename_in_1,filename_in_2,filename_out
  INTEGER :: unit_out=101
  INTEGER :: iargc
  REAL, ALLOCATABLE :: vcoord(:,:,:)
  INTEGER :: k
  REAL, DIMENSION(:), ALLOCATABLE :: pk,bk
  LOGICAL :: skip
  
  REAL,ALLOCATABLE  :: tmp_1(:),tmp_2(:)

  REAL (kind=8) timef
  CHARACTER(8) gdatatype,modelname
  CHARACTER(2) level

!---------------------------------------------------------------------------
!--- nemsio meta data

  INTEGER nrec,im,jm,lm,l,idate(7),version, im2,jm2, nframe, &
       ntrac,irealf,nrec1,version1,nmeta1,nfhour,nfminute,nfsecond, &
       nfsecondn,nfsecondd,nmeta,tlmeta

  INTEGER :: nvcoord
  INTEGER :: nfhour_1,nfhour_2


  INTEGER nsoil,jcap,ncld,idsl,idvc,idvm,idrt,rlon_min,rlon_max, &
       rlat_min,rlat_max
  INTEGER nmetavari,nmetavarr,nmetavarl,nmetavarc,nmetavarr8,    &
       nmetaaryi,nmetaaryr,nmetaaryl,nmetaaryc,nmetaaryr8
  INTEGER ihrst,idat(3),mp_physics,sf_surface_physics,icycle,fieldsize
  LOGICAL global, run,extrameta
  CHARACTER(16),ALLOCATABLE :: recname(:),reclevtyp(:)

!---------------------------------------------------------------------------
!--- local vars
  CHARACTER(16) vname,vname_1,vname_2
  CHARACTER(32) gtype
  CHARACTER(16) vlevtyp
  INTEGER i,ii,j,jj,jrec,krec,vlev,iret,lev,ista,iend,jsta,jend
!---------------------------------------------------------------------------
!
  CHARACTER(16),ALLOCATABLE :: variname(:),varrname(:),varlname(:),varcname(:),varr8name(:), &
       aryiname(:),aryrname(:),arylname(:),arycname(:),aryr8name(:)
  INTEGER,ALLOCATABLE :: varival(:),aryilen(:),aryrlen(:),aryllen(:),aryclen(:),aryr8len(:)
  INTEGER,ALLOCATABLE :: aryival(:,:)
  REAL,ALLOCATABLE :: varrval(:),aryrval(:,:)
  REAL(8),ALLOCATABLE :: varr8val(:),aryr8val(:,:)
  LOGICAL,ALLOCATABLE :: varlval(:),arylval(:,:)
  CHARACTER(16),ALLOCATABLE :: varcval(:),arycval(:,:)

!
!---------------------------------------------------------------------------
!
!-------------set up nemsio write--------------------------
!  call nemsio_init(iret=iret)
!  print *,'nemsio_init, iret=',iret
!
!+++++++++++++++++ read nemsil file with 2 meta data
!+++++++++++++++++++++++++++
!

  IF (iargc() < 3) THEN
     PRINT *,'Needs two input files plus output file - Stopping'
     STOP
  ENDIF

  CALL getarg(1,filename_in_1)
  CALL getarg(2,filename_in_2)
  CALL getarg(3,filename_out)

  PRINT *,'start reading nemsio file '
!--- open gfile_1, gfile_2 for reading
  CALL nemsio_open(gfile_1,TRIM(filename_in_1),'read',iret=iret)
  IF(iret/=0) PRINT *,'after open read, ',TRIM(filename_in_1), ' iret=',iret
  CALL nemsio_open(gfile_2,TRIM(filename_in_2),'read',iret=iret)
  IF(iret/=0) PRINT *,'after open read, ',TRIM(filename_in_2), ' iret=',iret

!
!--- get dimension
  im=0;jm=0;lm=0;nframe=0;nrec=0
  CALL nemsio_getfilehead(gfile_1,dimx=im,dimy=jm,dimz=lm,nframe=nframe,nrec=nrec,&
       gdatatype=gdatatype,modelname=modelname,nmeta=nmeta,ntrac=ntrac,tlmeta=tlmeta,iret=iret)
  IF ( iret .NE.0) PRINT *,'iret=',iret
  PRINT *,'gfile_1,im=',im,'jm=',jm,'lm=',lm,'nrec=',nrec, &
       'gdatatype=',gdatatype,' modelname=',modelname,' ntrac=',ntrac, &
       'iret=',iret
!--- meta data info
  CALL nemsio_getfilehead(gfile_1,nfhour=nfhour,nfminute=nfminute,nsoil=nsoil,ncldt=ncld,&
       idsl=idsl,idvc=idvc,idvm=idvm,idrt=idrt,iret=iret)
  IF ( iret .NE.0) PRINT *,'iret=',iret
  nfhour_1=nfhour
  PRINT *,'gfile_1,nfhour=',nfhour
  CALL nemsio_getfilehead(gfile_2,nfhour=nfhour,nfminute=nfminute,nsoil=nsoil,ncldt=ncld,&
       idsl=idsl,idvc=idvc,idvm=idvm,idrt=idrt,iret=iret)
  IF ( iret .NE.0) PRINT *,'iret=',iret
  nfhour_2=nfhour
  PRINT *,'gfile_2,nfhour=',nfhour

  IF (ABS(nfhour_1-nfhour_2) /= diff_time) THEN
     PRINT *,'Fcst difference should equal ',diff_time
     PRINT *,'Stopping'
     STOP
  ENDIF

!
! call nemsio_getheadvar(gfile_1,'nfhour',nfhour,iret=iret)
! print *,'nfhour=',nfhour
! call nemsio_getheadvar(gfile_1,'latf', latf,iret=iret)
! print *,'latf=',latf
!
  CALL nemsio_getfilehead(gfile_1,nmetavari=nmetavari,nmetavarr=nmetavarr,nmetavarl=nmetavarl, &
       nmetaaryi=nmetaaryi,nmetaaryr=nmetaaryr)
  IF(nmetavari>0) THEN
     ALLOCATE(variname(nmetavari),varival(nmetavari))
     CALL nemsio_getfilehead(gfile_1,variname=variname,varival=varival)
  ENDIF
  IF(nmetavarr>0) THEN
     ALLOCATE(varrname(nmetavarr),varrval(nmetavarr))
     CALL nemsio_getfilehead(gfile_1,varrname=varrname,varrval=varrval)
  ENDIF
  IF(nmetavarr8>0) THEN
     ALLOCATE(varr8name(nmetavarr8),varr8val(nmetavarr8))
     CALL nemsio_getfilehead(gfile_1,varr8name=varr8name,varr8val=varr8val)
  ENDIF
  IF(nmetavarl>0) THEN
     ALLOCATE(varlname(nmetavarl),varlval(nmetavarl))
     CALL nemsio_getfilehead(gfile_1,varlname=varlname,varlval=varlval)
  ENDIF
  IF(nmetavarc>0) THEN
     ALLOCATE(varcname(nmetavarc),varcval(nmetavarc))
     CALL nemsio_getfilehead(gfile_1,varcname=varcname,varcval=varcval)
  ENDIF

  IF(nmetaaryi>0) THEN
     ALLOCATE(aryiname(nmetaaryi),aryilen(nmetaaryi))
     CALL nemsio_getfilehead(gfile_1,aryiname=aryiname,aryilen=aryilen)
     ALLOCATE(aryival(MAXVAL(aryilen),nmetaaryi))
     CALL nemsio_getfilehead(gfile_1,aryival=aryival)
  ENDIF
  IF(nmetaaryr>0) THEN
     ALLOCATE(aryrname(nmetaaryr),aryrlen(nmetaaryr))
     CALL nemsio_getfilehead(gfile_1,aryrname=aryrname,aryrlen=aryrlen)
     ALLOCATE(aryrval(MAXVAL(aryrlen),nmetaaryr))
     CALL nemsio_getfilehead(gfile_1,aryrval=aryrval)
  ENDIF
  IF(nmetaaryr8>0) THEN
     ALLOCATE(aryr8name(nmetaaryr8),aryr8len(nmetaaryr8))
     CALL nemsio_getfilehead(gfile_1,aryr8name=aryr8name,aryr8len=aryr8len)
     ALLOCATE(aryr8val(MAXVAL(aryr8len),nmetaaryr8))
     CALL nemsio_getfilehead(gfile_1,aryr8val=aryr8val)
  ENDIF
  IF(nmetaaryl>0) THEN
     ALLOCATE(arylname(nmetaaryl),aryllen(nmetaaryl))
     CALL nemsio_getfilehead(gfile_1,arylname=arylname,aryllen=aryllen)
     ALLOCATE(arylval(MAXVAL(aryllen),nmetaaryl))
     CALL nemsio_getfilehead(gfile_1,arylval=arylval)
  ENDIF
  IF(nmetaaryc>0) THEN
     ALLOCATE(arycname(nmetaaryc),aryclen(nmetaaryc))
     CALL nemsio_getfilehead(gfile_1,arycname=arycname,aryclen=aryclen)
     ALLOCATE(arycval(MAXVAL(aryclen),nmetaaryc))
     CALL nemsio_getfilehead(gfile_1,arycval=arycval)
  ENDIF

!
!---read fields
!

  fieldsize=(im+2*nframe)*(jm+2*nframe)
  CALL nemsio_getheadvar(gfile_1,'NVCOORD', nvcoord,iret=iret)
  ALLOCATE(vcoord(lm+1,3,2),pk(lm+1),bk(lm+1))
  CALL nemsio_getfilehead(gfile_1,VCOORD=vcoord,iret=iret)

!from jsw comments in regrid-nemsio  

  IF(nvcoord == -9999) THEN
     nvcoord = 3
     IF(MAXVAL(vcoord(:,3,1))==0..AND.                &
          &        MINVAL(vcoord(:,3,1))==0. ) THEN
        nvcoord = 2
        IF(idsl == -9999) idsl = 1
        IF(MAXVAL(vcoord(:,2,1))==0. .AND.              &
             &         MINVAL(vcoord(:,2,1))==0.) THEN
           nvcoord = 1
        ENDIF
     ENDIF
  ENDIF

  IF (idvc == 2 .OR. idvc == 3) THEN ! hybrid coordinate
     pk = vcoord(1:lm+1,1,1)  !in Pa
     bk = vcoord(1:lm+1,2,1) 
  ELSE
     PRINT *,'Stopping - not a FV3 vertical coordinate type idvc =',idvc
     STOP
  END IF

  OPEN(unit=unit_out,file=TRIM(filename_out),form='unformatted')
  WRITE(unit_out)im,jm,lm,ntracers_gocart

  vname_1='pk'
  WRITE(unit_out)vname_1
  WRITE(unit_out)pk

  vname_1='bk'
  WRITE(unit_out)vname_1
  WRITE(unit_out)bk

  ALLOCATE(tmp_1(fieldsize),tmp_2(fieldsize))

  CALL nemsio_getfilehead(gfile_1,LAT=tmp_1,iret=iret)
  IF ( iret .NE.0) PRINT *,'iret=',iret
  vname_1='lat'
  WRITE(unit_out)vname_1
  WRITE(unit_out)(tmp_1(i),i=1,fieldsize,im)

  CALL nemsio_getfilehead(gfile_1,LON=tmp_1,iret=iret)
  IF ( iret .NE.0) PRINT *,'iret=',iret
  vname_1='lon'
  WRITE(unit_out)vname_1
  WRITE(unit_out)tmp_1(1:im)

  DO jrec=1,nrec

     CALL nemsio_getrechead(gfile_1,jrec,vname,vlevtyp,vlev,iret)
     IF ( iret .NE.0) PRINT *,'iret=',iret
     IF (TRIM(vname_1) /= TRIM(vname)) THEN
        skip=.TRUE.
        DO k=1,ntracers_gocart
           IF (TRIM(vname) == TRIM(tracer_names_gocart(k))) THEN
              WRITE(unit_out) vname
              PRINT *,vname
              skip=.FALSE.
              EXIT
           ENDIF
        ENDDO
        IF (TRIM(vname) == 'pres') THEN 
           WRITE(unit_out) vname
           PRINT *,vname
           skip=.FALSE.
        ENDIF
     ENDIF

     vname_1=vname
     CALL nemsio_readrec(gfile_1,jrec,tmp_1,iret=iret)

     CALL nemsio_getrechead(gfile_2,jrec,vname,vlevtyp,vlev,iret)
     vname_2=vname
     CALL nemsio_readrec(gfile_2,jrec,tmp_2,iret=iret)

     IF (vname_1 /= vname_2) THEN
        PRINT *,'Variables not matching: vname_1 = ',TRIM(vname_1), &
             &' vname_2 = ',TRIM(vname_2)
     ENDIF

!     PRINT *,'read,jrec=',jrec,'iret=',iret,' vname=',TRIM(vname), &
!          ' vlevtyp=',TRIM(vlevtyp),' vlev=',vlev,'data=',MAXVAL(tmp_1),MINVAL(tmp_1)

!     IF (.NOT. skip) PRINT *,'Outputting ', vname
     IF (.NOT. skip) THEN 
        IF (TRIM(vname) == 'pres') THEN
           WRITE(unit_out)0.5*(tmp_1+tmp_2)
        ELSE
           WRITE(unit_out)tmp_2-tmp_1
        ENDIF
     ENDIF

  ENDDO

  DEALLOCATE(tmp_1,tmp_2)

!
!--- close nemsio file
  CALL nemsio_close(gfile_1,iret=iret)
  IF ( iret .NE.0) PRINT *,'iret=',iret

  CALL nemsio_close(gfile_2,iret=iret)
  IF ( iret .NE.0) PRINT *,'iret=',iret

  CALL nemsio_finalize()

  CLOSE(unit_out)

!!---------------------------------------------------------------------------
!
!---------------------------------------------------------------------------
!
! - - - - -- - -- - -- - -- - - -- - --  -- - -- - -- - - -- - - - -- -
! --

END PROGRAM calc_diff_nemsio

