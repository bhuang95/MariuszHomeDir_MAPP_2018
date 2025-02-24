program fim_enkf_vintsig

! interpolate ensemble fim output to GFS vertical coordinate

implicit none

integer, parameter :: ntracm=3
integer, parameter :: levso=64
integer, parameter :: npp=6
real, parameter   :: p1000    = 100000.    ! p at 1000mb (pascals)
real, parameter   :: cp = 1004.6855        ! specific heat at const pres
real, parameter   :: rd = 287.0586         ! spec gas constant for dry air

INTEGER i,j,k,nanal,nfhour,nip,iunit,nptsin,levsoin,ntracin,nt,nanals,ihrinc,stat,ierr,iret
INTEGER numproc,nproc,niter,niter2,nglvl
CHARACTER(len=200) datadir, filename
CHARACTER(len=10) datestring
character(len=6) charnanal
character(len=2) charhr,glvl
character(len=16) header1,header2

integer, allocatable :: nprox(:), prox(:, :)

real, allocatable, dimension(:) :: vardata, lons, lats, zs,psf
real, allocatable, dimension(:,:) :: u,v,temp,pressi,pressi2,pressl,u2,v2,temp2,ex3d
real, allocatable, dimension(:,:,:) :: tr,tr2
real ptop,exn,ptopin
  real, parameter :: ak(1:65) =                                         &
  (/  0.000000,     0.000000,     0.575000,     5.741000,    21.516001, &
     55.712002,   116.899002,   214.014999,   356.222992,   552.719971, &
    812.489014,  1143.988037,  1554.788940,  2051.149902,  2637.552979, &
   3316.217041,  4086.614014,  4945.028809,  5884.206055,  6893.117188, &
   7956.908203,  9057.050781, 10171.711914, 11276.347656, 12344.490234, &
  13348.670898, 14261.434570, 15056.341797, 15708.892578, 16197.315430, &
  16503.144531, 16611.603516, 16511.736328, 16197.966797, 15683.489258, &
  14993.074219, 14154.316406, 13197.065430, 12152.936523, 11054.852539, &
   9936.614258,  8832.537109,  7777.149902,  6804.874023,  5937.049805, &
   5167.145996,  4485.493164,  3883.052002,  3351.459961,  2883.038086, &
   2470.788086,  2108.365967,  1790.051025,  1510.711060,  1265.751953, &
   1051.079956,   863.057983,   698.456970,   554.424011,   428.433990, &
    318.265991,   221.957993,   137.789993,    64.247002,     0.010000 /)

  real, parameter :: bk(1:65) =                                         &
(/ 1.000000000,  0.994671166,  0.988626599,  0.981742263,  0.973867595, &
   0.964827597,  0.954434097,  0.942491055,  0.928797305,  0.913151026, &
   0.895354986,  0.875223577,  0.852590680,  0.827318847,  0.799309731, &
   0.768514693,  0.734945238,  0.698682904,  0.659887016,  0.618799627, &
   0.575746655,  0.531134844,  0.485443324,  0.439210802,  0.393018246, &
   0.347468495,  0.303164124,  0.260685444,  0.220570192,  0.183296233, &
   0.149268776,  0.118812189,  0.092166908,  0.069474578,  0.050646842, &
   0.035441618,  0.023555880,  0.014637120,  0.008294020,  0.004106710, &
   0.001635910,  0.000431060,  0.000036970,  0.000000000,  0.000000000, &
   0.000000000,  0.000000000,  0.000000000,  0.000000000,  0.000000000, &
   0.000000000,  0.000000000,  0.000000000,  0.000000000,  0.000000000, &
   0.000000000,  0.000000000,  0.000000000,  0.000000000,  0.000000000, &
   0.000000000,  0.000000000,  0.000000000,  0.000000000,  0.000000000 /)

! mpi definitions.
include 'mpif.h'
integer MPI_Status(MPI_STATUS_SIZE)

call MPI_Init(ierr)
! nproc is process number, numproc is total number of processes.
call MPI_Comm_rank(MPI_COMM_WORLD,nproc,ierr)
call MPI_Comm_size(MPI_COMM_WORLD,numproc,ierr)

nanals=numproc

call getarg(1,glvl)
read(glvl,'(i2)') nglvl
call getarg(2,datestring)
call getarg(3,datadir)
call getarg(4,charnanal)
read(charnanal,'(f7.3)') ptop
call getarg(5,charnanal)
read(charnanal,'(i1)') niter
call getarg(6,charnanal)
read(charnanal,'(i1)') niter2
iunit = 82
ihrinc = 1
nip=10*(2**nglvl)**2+2

allocate(vardata(nip),zs(nip),lons(nip),lats(nip))
allocate(pressi(nip,levso+1),pressl(nip,levso),u(nip,levso),v(nip,levso))
allocate(psf(nip),pressi2(nip,levso+1))
allocate(temp(nip,levso),tr(nip,levso,ntracm),ex3d(nip,levso))
allocate(u2(nip,levso),v2(nip,levso))
allocate(temp2(nip,levso),tr2(nip,levso,ntracm))

! read in glvl.dat on root processor and distribute to all PEs
ALLOCATE(nprox(nip))
ALLOCATE(prox(npp, nip))


IF (nproc.EQ.0) THEN
   open(unit=28,file="glvl.G"//trim(GLVL)//".dat", form="unformatted")
   read (28) header1
   read (28) header2
   read(28)  nprox
   read(28)  nprox
   read(28)  nprox
   do i=1,npp
      read(28) prox(i,:)
   enddo
   do i=1,npp
     read(28) prox(i,:)
   enddo
ENDIF

call MPI_Bcast(nprox,nip,MPI_REAL,0,MPI_COMM_WORLD,ierr)
call MPI_Bcast(prox,nip*npp,MPI_REAL,0,MPI_COMM_WORLD,ierr)


DO nfhour=3,9,ihrinc
   write(charhr,'(i2.2)') nfhour
   nanal = nproc + 1
   write(charnanal,'(a3,i3.3)') 'mem',nanal
   filename = trim(datadir)//"/"//trim(datestring)//"/"//"gfg_"//trim(datestring)//"_fhr"//charhr//"_"//trim(charnanal)
   print *,nfhour,nproc,trim(filename)
   open(iunit,file=filename,form="unformatted",status="old")
   read(iunit) nptsin, levsoin, ntracin, ptopin
   IF (nptsin .ne. nip .or. levsoin .ne. levso .or. ntracin .ne. ntracm) THEN
      print *,'mismatch npts,levso,ntracin should be',nip,levso,ntracm
      print *,'got',nptsin,levsoin,ntracin
      STOP
   ENDIF   
   read(iunit) lons
   read(iunit) lats
   read(iunit) zs
   DO k=1,levso
      read(iunit) vardata ! pressl
   ENDDO
   DO k=1,levso
      read(iunit) pressi(:,k)
   ENDDO
   pressi(:,levso+1)=ptop
   psf=pressi(:,1)
   if (niter.GT.0) THEN
      call smoothflield(psf,nip,1,niter,nanals,nproc,nprox,prox)
   ENDIF
   DO k=1,levso+1
      pressi2(:,k)=ak(k)+bk(k)*psf*100
      ex3d(:,k)=cp*(pressi2(:,k)/p1000)**(rd/cp)
   ENDDO
   DO i=1,nip
     DO k=1,levso
       ! layer pressure from average of interface exner function
       if (pressi2(i,k).gt.pressi2(i,k+1)+0.1) then
          exn = (ex3d(i,k  )*pressi2(i,k  )                     &
                -ex3d(i,k+1)*pressi2(i,k+1))/                   &
                ((cp+rd)*(pressi2(i,k)-pressi2(i,k+1)))
       else
          exn = .5*(ex3d(i,k)+ex3d(i,k+1))/cp
       end if
       pressl(i,k)=p1000*(exn)**(cp/rd) ! layer pressure
     ENDDO
  ENDDO
! convert pressi and pressl back to mb
   pressi2=0.01*pressi2
   pressl=0.01*pressl
  do k=1,levso
     read(iunit) temp(:,k)
  enddo
  do k=1,levso
     read(iunit) u(:,k)
  enddo
  do k=1,levso
     read(iunit) v(:,k)
  enddo
  ! read "tracers"
  do nt=1,ntracm
    do k=1,levso
      read(iunit) tr(:,k,nt)
    enddo
  enddo
  close(iunit) 

  call vlint2coor(nip, levso, levso+1, pressi, temp, temp2, pressl, levso)
  call vlint2coor(nip, levso, levso+1, pressi, u,    u2,    pressl, levso)
  call vlint2coor(nip, levso, levso+1, pressi, v,    v2,    pressl, levso)
  do nt=1,ntracm
      call vlint2coor(nip, levso, levso+1, pressi, tr(:,:,nt), tr2(:,:,nt), pressl ,levso)
  enddo
  
  IF (niter2.GT.0) THEN
     call smoothflield(temp,nip,levso,niter2,nanals,nproc,nprox,prox)
     call smoothflield(u,nip,levso,niter2,nanals,nproc,nprox,prox)
     call smoothflield(v,nip,levso,niter2,nanals,nproc,nprox,prox)
  ENDIF
   filename = trim(datadir)//"/"//trim(datestring)//"/"//"gfg2_"//trim(datestring)//"_fhr"//charhr//"_"//trim(charnanal)
   !print *,trim(filename)
   open(iunit,file=filename,form="unformatted",status="replace")
   write(iunit) nip, levso, ntracm, ptop
   write(iunit) lons
   write(iunit) lats
   write(iunit) zs
   do k=1,levso
      write(iunit) pressl(:,k)
   enddo
   do k=1,levso
      write(iunit) pressi2(:,k)
   enddo
   do k=1,levso
      write(iunit) temp2(:,k)
   enddo
   do k=1,levso
      write(iunit) u2(:,k)
   enddo
   do k=1,levso
      write(iunit) v2(:,k)
   enddo
   do nt=1,ntracm
   do k=1,levso
      write(iunit) tr2(:,k,nt)
   enddo
   enddo
   close(iunit) 
   
enddo ! end loop over forecast hour.
call MPI_Barrier(MPI_COMM_WORLD,ierr)
call MPI_Finalize(ierr)

end program fim_enkf_vintsig

!=============================================================================
! Level and layer interpolation, target coordinate: passed in (v_coor) 
!
!
! Figure 1.                          | Figure 2.
! Level variables at interface:      | Layer variables: 
!                                    |
! -------------- int. level nvl + 1  | ---------------- int. level nvl + 1 
!       :                            |         :
!       :                            |         :
! -------------- int. level k + 1    | ---------------- int. level k + 1
!                                    | //////////////// variable at layer k
! -------------- int. level k        | ---------------- int. level k
!       :                            |         :
!       :                            |         :
! -------------- int. level 1        | ---------------- int. level 1
!                                    |
! 
!
! N. Wang, Feb. 2008
!=============================================================================
!                vlint2coor(nip, levso, levso+1,  pressi,       v,    v2,    pressl, levso)
      SUBROUTINE vlint2coor(nip,   nvl,   nvlp1, data_pr, data_var, data,    v_coor,   nvc)
        IMPLICIT NONE

        INTEGER, intent(in) ::  nip, nvl, nvlp1, nvc
        REAL, intent(in) ::  data_pr(nip, nvlp1),  data_var(nip, nvl), v_coor(nip, nvl)
        REAL, intent(out) :: data(nip, nvc)

        REAL pi_dn, pi_up, pi_co, dn_val, up_val
        INTEGER i, k, l

        DO i = 1, nip   
            k = 1
            DO l = 1, nvc 
              IF (v_coor(i,l) >= data_pr(i, 1)) THEN
                data(i, l) = data_var(i, 1)
                CYCLE
              END IF
              IF (v_coor(i,l) <= data_pr(i, nvlp1)) THEN
                data(i, l) = data_var(i, nvl)
                CYCLE
              END IF
              DO WHILE (v_coor(i,l) < data_pr(i, k + 1)) 
                IF (k == nvlp1 - 1) THEN
                  EXIT 
                ELSE
                  k = k + 1
                ENDIF
              END DO ! k and k+1 are the current indexes for interpolation  
              IF (nvl == nvlp1) THEN ! level variables, see fig. 1.
                pi_dn = (data_pr(i, k) / 1000.00)**0.286
                pi_up = (data_pr(i, k + 1) / 1000.00)**0.286 
                pi_co = (v_coor(i,l) / 1000.00)**0.286
                dn_val = data_var(i, k)
                up_val = data_var(i, k + 1)
              ELSE  ! layer variables, see fig. 2.
                pi_dn = (data_pr(i, k) / 1000.00)**0.286
                pi_up = (data_pr(i, k + 1) / 1000.00)**0.286 
                pi_co = (v_coor(i,l) / 1000.00)**0.286
                IF (pi_co > (pi_dn + pi_up) / 2.0) THEN ! lower half of the layer
                  IF (k == 1) THEN
                    pi_dn = (data_pr(i, k) / 1000.00)**0.286
                    pi_up = ((data_pr(i, k) / 1000.00)**0.286 + (data_pr(i, k + 1) / 1000.00)**0.286)/ 2.0
                    pi_co = (v_coor(i,l) / 1000.00)**0.286
                    dn_val = data_var(i, k)
                    up_val = dn_val 
                  ELSE
                    pi_dn = ((data_pr(i, k) / 1000.00)**0.286 + (data_pr(i, k - 1) / 1000.00)**0.286)/ 2.0 
                    pi_up = ((data_pr(i, k) / 1000.00)**0.286 + (data_pr(i, k + 1) / 1000.00)**0.286)/ 2.0 
                    pi_co = (v_coor(i,l) / 1000.00)**0.286
                    dn_val = data_var(i, k - 1)
                    up_val = data_var(i, k)
                  ENDIF
                ELSE ! upper half of the layer
                  IF (k == nvl) THEN
                    pi_dn = ((data_pr(i, k) / 1000.00)**0.286 + (data_pr(i, k + 1) / 1000.00)**0.286)/ 2.0 
                    pi_up = (data_pr(i, k + 1) / 1000.00)**0.286
                    pi_co = (v_coor(i,l) / 1000.00)**0.286
                    dn_val = data_var(i, k)
                    up_val = dn_val 
                  ELSE
                    pi_dn = ((data_pr(i, k) / 1000.00)**0.286 + (data_pr(i, k + 1) / 1000.00)**0.286)/ 2.0
                    pi_up = ((data_pr(i, k + 1) / 1000.00)**0.286 + (data_pr(i, k + 2) / 1000.00)**0.286)/ 2.0 
                    pi_co = (v_coor(i,l) / 1000.00)**0.286
                    dn_val = data_var(i, k)
                    up_val = data_var(i, k + 1)
                  ENDIF

                ENDIF
              ENDIF
              data(i, l) = up_val +  (pi_co - pi_up) * &
                (dn_val - up_val) / (pi_dn - pi_up) 
            END DO
        END DO
      END SUBROUTINE vlint2coor
subroutine smoothflield(fldi,nip,km,niter,nanals,nproc,nprox,prox)

! this subroutine smooths on icoshedrial grid, the perturbations around the ensmeble mean
! fldi    input field
! nip      nuber of horizontal grid points
! km      number of vertical levles
! niter  number of smoothing passes
! nanals  number of ensemble members (and processors)
 
implicit none
integer, intent(in)    :: nip,km,niter,nanals,nproc,nprox(nip),prox(6,nip)
real   , intent(inout) :: fldi(nip,km)
!locas
real   , allocatable   :: fld_mn(:,:),fld(:,:),smthvar(:,:)
integer                :: i,j,k,l,ierr
real                   :: neighbor
! mpi definitions.
include 'mpif.h'
integer MPI_Status(MPI_STATUS_SIZE)
allocate(fld_mn(nip,km))
allocate(fld(nip,km))
allocate(smthvar(nip,km))
   IF (nproc .EQ. 0) THEN
      fld_mn=fldi
      DO i=1,nanals-1
         CALL MPI_RECV(fld,nip*km,MPI_REAL,i,i,MPI_COMM_WORLD,MPI_Status,ierr)
         fld_mn=fld + fld_mn
      ENDDO
      fld_mn = fld_mn / float(nanals)
   ENDIF
   IF (nproc .NE. 0) THEN
      CALL MPI_SEND(fldi,nip*km,MPI_REAL,0,nproc,MPI_COMM_WORLD,ierr)
   ENDIF
!  send ensemble mean surface pressure to each processor
   call MPI_Bcast(fld_mn,nip*km,MPI_REAL,0,MPI_COMM_WORLD,ierr)
!  call MPI_Bcast(prox,nip*npp, MPI_REAL,0,MPI_COMM_WORLD,ierr)
! remove ensemble mean surface pressure then smoth perturbation
   fld=fldi-fld_mn
   do l=1,niter
      do k=1,km
         do i=1,nip
            smthvar(i,k)=fld(i,k)*nprox(i)
            neighbor=0.0
            do j=1,nprox(i)
               neighbor=fld(prox(j,i),k)+neighbor
            enddo
            smthvar(i,k)=(fld(i,k)*nprox(i)+neighbor)/(2*nprox(i))
         enddo
      enddo
      fld=smthvar
   enddo
!  now add back ensemble mean
   fldi=fld+fld_mn
end subroutine smoothflield
