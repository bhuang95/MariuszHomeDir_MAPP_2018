!----------convert CMAQ output to grib format 
!
!  by      Youhua Tang, Jun 2009
!  modified for cmaq471 by Jianping Huang, July 2011

      include 'PARMS3.EXT'      ! i/o API
      include 'FDESC3.EXT'      ! i/o API
      include 'IODECL3.EXT'     ! i/o API

      real, allocatable :: work(:,:), pm25(:,:), sigmas(:), workdot(:,:)
      real, allocatable :: workdiam(:,:),workd1(:,:),workd2(:,:), &
                           workd3(:,:)
      
      logical, allocatable :: lb(:,:)   ! logical mask
      
      integer, parameter :: naerospec=40, ncmaq=138, nmet=11,ndiam=3
      character*16 aerospec(naerospec),varlist(ncmaq),metlist(nmet), &
          diamlist(ndiam),cmaqspec(ncmaq),metspec(nmet),diamspec(ndiam)
      real conv_ratio(ncmaq)
      	  
      character*200 outfile
      character chtmp*2
      integer kpds(200),kgds(200),gribid(ncmaq),gribtb(ncmaq),  &
          mgribid(nmet),mgribtb(nmet),           &
          dgribid(ndiam),dgribtb(ndiam)
      logical i3dvar(ncmaq),mi3dvar(nmet),iflag,ave1hr
      logical di3dvar(ndiam)      
      
      data aerospec/'ASO4I','ANO3I','ANH4I','AORGPAI','AECI','ACLI', &
       'ASO4J', 'ANO3J','ANH4J','AORGPAJ','AECJ','ANAJ','ACLJ','A25J', &
       'AXYL1J','AXYL2J','AXYL3J','ATOL1J','ATOL2J','ATOL3J', &
       'ABNZ1J','ABNZ2J','ABNZ3J','AALKJ','AOLGAJ','AISO1J', &
       'AISO2J','AISO3J','ATRP1J','ATRP2J','ASQTJ','AOLGBJ','AORGCJ', &
       'ASO4K','ANO3K','ANH4K','ANAK','ACLK','ACORS','ASOIL'/ 


      data cmaqspec(1),gribid(1),gribtb(1),i3dvar(1)/'NO2',142,141,.TRUE./
      data cmaqspec(2),gribid(2),gribtb(2),i3dvar(2)/'NO',141,141,.TRUE./
      data cmaqspec(3),gribid(3),gribtb(3),i3dvar(3)/'O',133,141,.TRUE./
      data cmaqspec(4),gribid(4),gribtb(4),i3dvar(4)/'O3',180,129,.TRUE./
      data cmaqspec(5),gribid(5),gribtb(5),i3dvar(5)/'NO3',137,128,.TRUE./
      data cmaqspec(6),gribid(6),gribtb(6),i3dvar(6)/'O1D',134,141,.TRUE./
      data cmaqspec(7),gribid(7),gribtb(7),i3dvar(7)/'OH',187,141,.TRUE./
      data cmaqspec(8),gribid(8),gribtb(8),i3dvar(8)/'HO2',188,141,.TRUE./
      data cmaqspec(9),gribid(9),gribtb(9),i3dvar(9)/'N2O5',143,141,.TRUE./
      data cmaqspec(10),gribid(10),gribtb(10),i3dvar(10)/'HNO3',144,141,.TRUE./
      data cmaqspec(11),gribid(11),gribtb(11),i3dvar(11)/'HONO',147,141,.TRUE./
      data cmaqspec(12),gribid(12),gribtb(12),i3dvar(12)/'PNA',146,141,.TRUE./
      data cmaqspec(13),gribid(13),gribtb(13),i3dvar(13)/'H2O2',186,141,.TRUE./
      data cmaqspec(14),gribid(14),gribtb(14),i3dvar(14)/'XO2',180,141,.TRUE./
      data cmaqspec(15),gribid(15),gribtb(15),i3dvar(15)/'XO2N',181,141,.TRUE./
      data cmaqspec(16),gribid(16),gribtb(16),i3dvar(16)/'NTR',173,141,.TRUE./
      data cmaqspec(17),gribid(17),gribtb(17),i3dvar(17)/'ROOH',177,141,.TRUE./
      data cmaqspec(18),gribid(18),gribtb(18),i3dvar(18)/'FORM',166,141,.TRUE./
      data cmaqspec(19),gribid(19),gribtb(19),i3dvar(19)/'ALD2',167,141,.TRUE./
      data cmaqspec(20),gribid(20),gribtb(20),i3dvar(20)/'ALDX',170,141,.TRUE./
      data cmaqspec(21),gribid(21),gribtb(21),i3dvar(21)/'PAR',159,141,.TRUE./
      data cmaqspec(22),gribid(22),gribtb(22),i3dvar(22)/'CO',148,141,.TRUE./
      data cmaqspec(23),gribid(23),gribtb(23),i3dvar(23)/'MEO2',182,141,.TRUE./
      data cmaqspec(24),gribid(24),gribtb(24),i3dvar(24)/'MEPX',183,141,.TRUE./
      data cmaqspec(25),gribid(25),gribtb(25),i3dvar(25)/'MEOH',184,141,.TRUE./
      data cmaqspec(26),gribid(26),gribtb(26),i3dvar(26)/'HCO3',140,128,.TRUE./
      data cmaqspec(27),gribid(27),gribtb(27),i3dvar(27)/'FACD',144,128,.TRUE./
      data cmaqspec(28),gribid(28),gribtb(28),i3dvar(28)/'C2O3',145,128,.TRUE./
      data cmaqspec(29),gribid(29),gribtb(29),i3dvar(29)/'PAN',172,141,.TRUE./
      data cmaqspec(30),gribid(30),gribtb(30),i3dvar(30)/'PACD',175,141,.TRUE./
      data cmaqspec(31),gribid(31),gribtb(31),i3dvar(31)/'AACD',176,141,.TRUE./
      data cmaqspec(32),gribid(32),gribtb(32),i3dvar(32)/'CXO3',152,141,.TRUE./
      data cmaqspec(33),gribid(33),gribtb(33),i3dvar(33)/'PANX',174,141,.TRUE./
      data cmaqspec(34),gribid(34),gribtb(34),i3dvar(34)/'ROR',153,141,.TRUE./
      data cmaqspec(35),gribid(35),gribtb(35),i3dvar(35)/'OLE',161,141,.TRUE./
      data cmaqspec(36),gribid(36),gribtb(36),i3dvar(36)/'ETH',165,141,.TRUE./
      data cmaqspec(37),gribid(37),gribtb(37),i3dvar(37)/'IOLE',185,141,.TRUE./
      data cmaqspec(38),gribid(38),gribtb(38),i3dvar(38)/'TOL',162,141,.TRUE./
      data cmaqspec(39),gribid(39),gribtb(39),i3dvar(39)/'CRES',169,141,.TRUE./
      data cmaqspec(40),gribid(40),gribtb(40),i3dvar(40)/'TO2',198,128,.TRUE./
      data cmaqspec(41),gribid(41),gribtb(41),i3dvar(41)/'TOLRO2',199,128,.TRUE./
      data cmaqspec(42),gribid(42),gribtb(42),i3dvar(42)/'OPEN',200,128,.TRUE./
      data cmaqspec(43),gribid(43),gribtb(43),i3dvar(43)/'CRO',201,128,.TRUE./
      data cmaqspec(44),gribid(44),gribtb(44),i3dvar(44)/'MGLY',168,141,.TRUE./
      data cmaqspec(45),gribid(45),gribtb(45),i3dvar(45)/'XYL',163,141,.TRUE./
      data cmaqspec(46),gribid(46),gribtb(46),i3dvar(46)/'XYLRO2',202,128,.TRUE./
      data cmaqspec(47),gribid(47),gribtb(47),i3dvar(47)/'ISOP',164,141,.TRUE./
      data cmaqspec(48),gribid(48),gribtb(48),i3dvar(48)/'ISPD',203,128,.TRUE./
      data cmaqspec(49),gribid(49),gribtb(49),i3dvar(49)/'ISOPRXN',204,128,.TRUE./
      data cmaqspec(50),gribid(50),gribtb(50),i3dvar(50)/'TERP',205,128,.TRUE./
      data cmaqspec(51),gribid(51),gribtb(51),i3dvar(51)/'TRPRXN',206,128,.TRUE./
      data cmaqspec(52),gribid(52),gribtb(52),i3dvar(52)/'SO2',232,141,.TRUE./
      data cmaqspec(53),gribid(53),gribtb(53),i3dvar(53)/'SULF',207,128,.TRUE./
      data cmaqspec(54),gribid(54),gribtb(54),i3dvar(54)/'SULRXN',208,128,.TRUE./
      data cmaqspec(55),gribid(54),gribtb(55),i3dvar(55)/'ETOH',209,128,.TRUE./
      data cmaqspec(56),gribid(56),gribtb(56),i3dvar(56)/'ETHA',210,128,.TRUE./
      data cmaqspec(57),gribid(57),gribtb(57),i3dvar(57)/'CL2',211,128,.TRUE./
      data cmaqspec(58),gribid(58),gribtb(58),i3dvar(58)/'CL',212,128,.TRUE./
      data cmaqspec(59),gribid(59),gribtb(59),i3dvar(59)/'HOCL',213,128,.TRUE./
      data cmaqspec(60),gribid(60),gribtb(60),i3dvar(60)/'CLO',214,128,.TRUE./
      data cmaqspec(61),gribid(61),gribtb(61),i3dvar(61)/'FMCL',215,128,.TRUE./
      data cmaqspec(62),gribid(62),gribtb(62),i3dvar(62)/'HCL',150,141,.TRUE./
      data cmaqspec(63),gribid(63),gribtb(63),i3dvar(63)/'TOLNRXN',216,128,.TRUE./
      data cmaqspec(64),gribid(64),gribtb(64),i3dvar(64)/'TOLHRXN',217,128,.TRUE./
      data cmaqspec(65),gribid(65),gribtb(65),i3dvar(65)/'XYLNRXN',218,128,.TRUE./
      data cmaqspec(66),gribid(66),gribtb(66),i3dvar(66)/'XYLHRXN',219,128,.TRUE./
      data cmaqspec(67),gribid(67),gribtb(67),i3dvar(67)/'BENZENE',220,128,.TRUE./
      data cmaqspec(68),gribid(68),gribtb(68),i3dvar(68)/'BENZRO2',221,128,.TRUE./
      data cmaqspec(69),gribid(69),gribtb(69),i3dvar(69)/'BNZNRXN',222,128,.TRUE./
      data cmaqspec(70),gribid(70),gribtb(70),i3dvar(70)/'BNZHRXN',223,128,.TRUE./
      data cmaqspec(71),gribid(71),gribtb(71),i3dvar(71)/'SESQ',224,128,.TRUE./
      data cmaqspec(72),gribid(72),gribtb(72),i3dvar(72)/'SESQRXN',225,128,.TRUE./
      data cmaqspec(73),gribid(73),gribtb(73),i3dvar(73)/'ASO4J',200,141,.TRUE./
      data cmaqspec(74),gribid(74),gribtb(74),i3dvar(74)/'ASO4I',189,141,.TRUE./
      data cmaqspec(75),gribid(75),gribtb(75),i3dvar(75)/'ANH4J',201,141,.TRUE./
      data cmaqspec(76),gribid(76),gribtb(76),i3dvar(76)/'ANH4I',190,141,.TRUE./
      data cmaqspec(77),gribid(77),gribtb(77),i3dvar(77)/'ANO3J',202,141,.TRUE./
      data cmaqspec(78),gribid(78),gribtb(78),i3dvar(78)/'ANO3I',191,141,.TRUE./
      data cmaqspec(79),gribid(79),gribtb(79),i3dvar(79)/'AALKJ',226,128,.TRUE./
      data cmaqspec(80),gribid(80),gribtb(80),i3dvar(80)/'AXYL1J',227,128,.TRUE./
      data cmaqspec(81),gribid(81),gribtb(81),i3dvar(81)/'AXYL2J',228,128,.TRUE./
      data cmaqspec(82),gribid(82),gribtb(82),i3dvar(82)/'AXYL3J',229,128,.TRUE./
      data cmaqspec(83),gribid(83),gribtb(83),i3dvar(83)/'ATOL1J',230,128,.TRUE./
      data cmaqspec(84),gribid(84),gribtb(84),i3dvar(84)/'ATOL2J',231,128,.TRUE./
      data cmaqspec(85),gribid(85),gribtb(85),i3dvar(85)/'ATOL3J',232,128,.TRUE./
      data cmaqspec(86),gribid(86),gribtb(86),i3dvar(86)/'ABNZ1J',233,128,.TRUE./
      data cmaqspec(87),gribid(87),gribtb(87),i3dvar(87)/'ABNZ2J',234,128,.TRUE./
      data cmaqspec(88),gribid(88),gribtb(88),i3dvar(88)/'ABNZ3J',235,128,.TRUE./
      data cmaqspec(89),gribid(89),gribtb(89),i3dvar(89)/'ATRP1J',236,128,.TRUE./
      data cmaqspec(90),gribid(90),gribtb(90),i3dvar(90)/'ATRP2J',237,128,.TRUE./
      data cmaqspec(91),gribid(91),gribtb(91),i3dvar(91)/'AISO1J',238,128,.TRUE./
      data cmaqspec(92),gribid(92),gribtb(92),i3dvar(92)/'AISO2J',239,128,.TRUE./
      data cmaqspec(93),gribid(93),gribtb(93),i3dvar(93)/'ASQTJ',240,128,.TRUE./
      data cmaqspec(94),gribid(94),gribtb(94),i3dvar(94)/'AORGCJ',241,128,.TRUE./
      data cmaqspec(95),gribid(95),gribtb(95),i3dvar(95)/'AORGPAJ',204,141,.TRUE./
      data cmaqspec(96),gribid(96),gribtb(96),i3dvar(96)/'AORGPAI',191,141,.TRUE./
      data cmaqspec(97),gribid(97),gribtb(97),i3dvar(97)/'AECJ',206,141,.TRUE./
      data cmaqspec(98),gribid(98),gribtb(98),i3dvar(98)/'AECI',192,141,.TRUE./
      data cmaqspec(99),gribid(99),gribtb(99),i3dvar(99)/'A25J',207,141,.TRUE./
      data cmaqspec(100),gribid(100),gribtb(100),i3dvar(100)/'A25I',193,141,.TRUE./
      data cmaqspec(101),gribid(101),gribtb(101),i3dvar(101)/'ACORS',194,141,.TRUE./
      data cmaqspec(102),gribid(102),gribtb(102),i3dvar(102)/'ASOIL',195,142,.TRUE./
      data cmaqspec(103),gribid(103),gribtb(103),i3dvar(103)/'NUMATKN',222,141,.TRUE./
      data cmaqspec(104),gribid(104),gribtb(104),i3dvar(104)/'NUMACC',223,141,.TRUE./
      data cmaqspec(105),gribid(105),gribtb(105),i3dvar(105)/'NUMCOR',224,141,.TRUE./
      data cmaqspec(106),gribid(106),gribtb(106),i3dvar(106)/'SRFATKN',228,141,.TRUE./
      data cmaqspec(107),gribid(107),gribtb(107),i3dvar(107)/'SRFACC',229,141,.TRUE./
      data cmaqspec(108),gribid(108),gribtb(108),i3dvar(108)/'SRFCOR',225,141,.TRUE./
      data cmaqspec(109),gribid(109),gribtb(109),i3dvar(109)/'AH2OJ',208,141,.TRUE./
      data cmaqspec(110),gribid(110),gribtb(110),i3dvar(110)/'AH2OI',211,141,.TRUE./
      data cmaqspec(111),gribid(111),gribtb(111),i3dvar(111)/'ANAJ',209,141,.TRUE./
      data cmaqspec(112),gribid(112),gribtb(112),i3dvar(112)/'ANAI',212,141,.TRUE./
      data cmaqspec(113),gribid(113),gribtb(113),i3dvar(113)/'ACLJ',210,141,.TRUE./
      data cmaqspec(114),gribid(114),gribtb(114),i3dvar(114)/'ACLI',213,141,.TRUE./
      data cmaqspec(115),gribid(115),gribtb(115),i3dvar(115)/'ANAK',214,141,.TRUE./
      data cmaqspec(116),gribid(116),gribtb(116),i3dvar(116)/'ACLK',215,141,.TRUE./
      data cmaqspec(117),gribid(117),gribtb(117),i3dvar(117)/'ASO4K',216,141,.TRUE./
      data cmaqspec(118),gribid(118),gribtb(118),i3dvar(118)/'ANH4K',217,141,.TRUE./
      data cmaqspec(119),gribid(119),gribtb(119),i3dvar(119)/'ANO3K',202,141,.TRUE./
      data cmaqspec(120),gribid(120),gribtb(120),i3dvar(120)/'AH2OK',241,128,.TRUE./
      data cmaqspec(121),gribid(121),gribtb(121),i3dvar(121)/'AISO3J',242,128,.TRUE./
      data cmaqspec(122),gribid(122),gribtb(122),i3dvar(122)/'AOLGAJ',243,128,.TRUE./
      data cmaqspec(123),gribid(123),gribtb(123),i3dvar(123)/'AOLGBJ',244,128,.TRUE./
      data cmaqspec(124),gribid(124),gribtb(124),i3dvar(124)/'NH3',149,141,.TRUE./
      data cmaqspec(125),gribid(125),gribtb(125),i3dvar(125)/'SV_ALK',245,128,.TRUE./
      data cmaqspec(126),gribid(126),gribtb(126),i3dvar(126)/'SV_XYL1',246,128,.TRUE./
      data cmaqspec(127),gribid(127),gribtb(127),i3dvar(127)/'SV_XYL2',247,128,.TRUE./
      data cmaqspec(128),gribid(128),gribtb(128),i3dvar(128)/'SV_TOL1',248,128,.TRUE./
      data cmaqspec(129),gribid(129),gribtb(129),i3dvar(129)/'SV_TOL2',249,128,.TRUE./
      data cmaqspec(130),gribid(130),gribtb(130),i3dvar(130)/'SV_BNZ1',250,128,.TRUE./
      data cmaqspec(131),gribid(131),gribtb(131),i3dvar(131)/'SV_BNZ2',251,128,.TRUE./
      data cmaqspec(132),gribid(132),gribtb(132),i3dvar(132)/'SV_TRP1',252,128,.TRUE./
      data cmaqspec(133),gribid(133),gribtb(133),i3dvar(133)/'SV_TRP2',253,128,.TRUE./
      data cmaqspec(134),gribid(134),gribtb(134),i3dvar(134)/'SV_ISO1',254,128,.TRUE./
      data cmaqspec(135),gribid(135),gribtb(135),i3dvar(135)/'SV_ISO2',253,141,.TRUE./
      data cmaqspec(136),gribid(136),gribtb(136),i3dvar(136)/'SV_SQT',254,141,.TRUE./
      data cmaqspec(137),gribid(137),gribtb(137),i3dvar(137)/'PM2.5',157,129,.TRUE./
      data cmaqspec(138),gribid(138),gribtb(138),i3dvar(138)/'EXT_Mie',128,141,.TRUE./


      data metspec(1),mgribid(1),mgribtb(1),mi3dvar(1)/'ZF',8,2,.TRUE./
      data metspec(2),mgribid(2),mgribtb(2),mi3dvar(2)/'PRES',1,2,.TRUE./
      data metspec(3),mgribid(3),mgribtb(3),mi3dvar(3)/'TA',11,2,.TRUE./
      data metspec(4),mgribid(4),mgribtb(4),mi3dvar(4)/'QV',51,2,.TRUE./
      data metspec(5),mgribid(5),mgribtb(5),mi3dvar(5)/'PBL',221,2,.FALSE./
      data metspec(6),mgribid(6),mgribtb(6),mi3dvar(6)/'PBL2',221,130,.FALSE./    ! ACM2 Richardson
      data metspec(7),mgribid(7),mgribtb(7),mi3dvar(7)/'PBLR',221,131,.FALSE./    ! NCEP Richardson based
      data metspec(8),mgribid(8),mgribtb(8),mi3dvar(8)/'MIXHT',67,2,.FALSE./
      data metspec(9),mgribid(9),mgribtb(9),mi3dvar(9)/'UWIND',33,2,.TRUE./
      data metspec(10),mgribid(10),mgribtb(10),mi3dvar(10)/'VWIND',34,2,.TRUE./
      data metspec(11),mgribid(11),mgribtb(11),mi3dvar(11)/'CWATER',153,2,.TRUE./

      data diamspec(1),dgribid(1),dgribtb(1),di3dvar(1)/'PM25AT',240,139,.TRUE./
      data diamspec(2),dgribid(2),dgribtb(2),di3dvar(2)/'PM25AC',241,139,.TRUE./
      data diamspec(3),dgribid(3),dgribtb(3),di3dvar(3)/'PM25CO',242,139,.TRUE./
                   
      data varlist/ncmaq*'   '/ 
      data diamlist/ndiam*'   '/
      data metlist/nmet*'   '/
      data conv_ratio/ncmaq*1./

      integer indexcmaq(ncmaq),indexmet(nmet),istime(5),ietime(5)    ! file start and ending in YYYYDDDHH
      integer indexdiam(ndiam)    ! file start and ending in YYYYDDDHH

      namelist /control/varlist,metlist,diamlist,outfile,nlayers,id_gribdomain,&   !   (table A)
                         ave1hr
      open(7,file='cmaq2grib.ini')      
      read(7,control)
      close(7)

!--- cmaq species
      do L=1,ncmaq
       if(varlist(L).ne.'    ') then
         
	 do L2=1,ncmaq
	  if(varlist(L).eq.cmaqspec(L2)) exit	  
	 enddo
	 if(L2.gt.ncmaq) then
	  print*,'wrong varlist ', varlist(L)
	  stop
	 endif	 
	 indexcmaq(L)=L2
	 
       else
        exit
       endif	 	 
      enddo
      
      nspcmaq=L-1 

      if(nspcmaq.lt.1) then
       print*,'no CMAQ species provided'
!       stop
      endif

!--- diam species

      do L=1,ndiam
       if(diamlist(L).ne.'    ') then

         do L2=1,ndiam
          if(diamlist(L).eq.diamspec(L2)) exit
         enddo
         if(L2.gt.ndiam) then
          print*,'wrong diamlist ', diamlist(L)
          stop
         endif
         indexdiam(L)=L2

       else
        exit
       endif
      enddo

      nspcdiam=L-1

      if(nspcdiam.lt.1) then
       print*,'no DIAM species provided'
!       stop
      endif


!-----met species

      do L=1,nmet
       if(metlist(L).ne.'    ') then         
	 do L2=1,nmet
	  if(metlist(L).eq.metspec(L2)) exit	  
	 enddo
	 if(L2.gt.nmet) then
	  print*,'wrong metlist ', metlist(L)
	  stop
	 endif	 
	 indexmet(L)=L2
	 
       else
        exit
       endif	 	 
      enddo
      
      nspcmet=L-1
      if(nspecmet.lt.1) print*,'no met species provided'

      
! open files

      if(.not.OPEN3('CHEM3D',FSREAD3,'pathway')) then
       print*,'open input file error for CHEM3D'
       stop
      endif

      if (.not. DESC3('CHEM3D') ) then   ! get grid information from CMAQ output
       print*, 'Error getting info from CHEM3D' 
       stop
      endif
      
      do L=1,ncmaq
       do L2=1,nvars3d
        if(vname3d(L2).eq.cmaqspec(L).and.units3d(L2)(1:3).eq.'ppm') conv_ratio(L)=1000. ! convert to ppbv
       enddo
      enddo 	
      

      if(id_gribdomain.eq.148.and.     &
       (ncols3d.ne.442.or.nrows3d.ne.265)) then
        print*,'5x domain dimension does not match'
	stop
      endif
      
      if(tstep3d.ne.10000) then
       print*,'need hourly input in CMAQFILEs'
       stop
      endif 
    
      imax=ncols3d
      jmax=nrows3d
      kmax=nlays3d
      
      if(nlayers.gt.kmax) then
       print*,'nlayers too high ', nlayers, kmax
       stop
      endif 
      
      allocate(sigmas(kmax))
      do k=1,kmax
       sigmas(k)=0.5*(vglvs3d(k)+vglvs3d(k+1))
      enddo
      
      nowsdate=sdate3d
      nowstime=stime3d
      maxrec1=mxrec3d
      
      istime(1)=sdate3d*100+stime3d/10000     ! file start time in YYYYDDDHH
            
      ntmpdate=sdate3d
      ntmptime=stime3d      
      do n=1,mxrec3d-1
       call nextime(ntmpdate,ntmptime,tstep3d)
      enddo
      ietime(1)=ntmpdate*100+ntmptime/10000   ! file end time in YYYYDDDHH      
      
      allocate(lb(imax,jmax))
      lb(:,:)=.true.
      allocate(work(imax,jmax))
      allocate(pm25(imax,jmax))

      
!---met files
      if(nspcmet.ge.1) then 
       if(.not.OPEN3('METCRO3D',FSREAD3,'cmaq2grib')) stop
       if(.not.DESC3('METCRO3D')) stop

       if(ncols3d.ne.imax.or.nrows3d.ne.jmax.or.nlays3d.lt.nlayers) then
         print*,'METCRO3D dimenison mismatch ',ncols3d,nrows3d,nlays3d
	 stop
       endif

       istime(2)=sdate3d*100+stime3d/10000     ! file start time in YYYYDDDHH            
       ntmpdate=sdate3d
       ntmptime=stime3d      
       do n=1,mxrec3d-1
        call nextime(ntmpdate,ntmptime,tstep3d)
       enddo
       ietime(2)=ntmpdate*100+ntmptime/10000   ! file end time in YYYYDDDHH
       
       if(istime(2).gt.istime(1).or.ietime(2).lt.ietime(1)) then
        print*,'METCRO3D time mismatch ',istime(2),ietime(2),istime(1),ietime(1)
	stop
       endif

       if(.not.OPEN3('METCRO2D',FSREAD3,'cmaq2grib')) stop
       if(.not.DESC3('METCRO2D')) stop

       if(ncols3d.ne.imax.or.nrows3d.ne.jmax) then
         print*,'METCRO2D dimenison mismatch ',ncols3d,nrows3d
	 stop
       endif

       istime(3)=sdate3d*100+stime3d/10000     ! file start time in YYYYDDDHH            
       ntmpdate=sdate3d
       ntmptime=stime3d      
       do n=1,mxrec3d-1
        call nextime(ntmpdate,ntmptime,tstep3d)
       enddo
       ietime(3)=ntmpdate*100+ntmptime/10000   ! file end time in YYYYDDDHH
       if(istime(3).gt.istime(1).or.ietime(3).lt.ietime(1)) then
        print*,'METCRO2D time mismatch ',istime(3),ietime(3),istime(1),ietime(1)
	stop
       endif

       do L=1,nspcmet
        if(metlist(L).eq.'UWIND'.or.metlist(L).eq.'VWIND') exit
       enddo
       if(L.le.nspcmet) then
        if(.not.OPEN3('METDOT3D',FSREAD3,'cmaq2grib')) stop
        if(.not.DESC3('METDOT3D')) stop

        if(ncols3d.ne.imax+1.or.nrows3d.ne.jmax+1.or.nlays3d.lt.nlayers) then
         print*,'METDOT3D dimenison mismatch ',ncols3d,nrows3d,nlays3d
	 stop
        endif
        istime(4)=sdate3d*100+stime3d/10000     ! file start time in YYYYDDDHH            
        ntmpdate=sdate3d
        ntmptime=stime3d      
        do n=1,mxrec3d-1
         call nextime(ntmpdate,ntmptime,tstep3d)
        enddo
        ietime(4)=ntmpdate*100+ntmptime/10000   ! file end time in YYYYDDDHH
       
        if(istime(4).gt.istime(1).or.ietime(4).lt.ietime(1)) then
         print*,'METDOT3D time mismatch ',istime(4),ietime(4),istime(1),ietime(1)
	 stop
        endif
   
        allocate(workdot(imax+1,jmax+1))
       endif
       	 
      endif

! --- diam species
      if(nspcdiam.ge.1) then
       if(.not.OPEN3('DIAM',FSREAD3,'cmaq2grib')) stop
       if(.not.DESC3('DIAM')) stop

       if(ncols3d.ne.imax.or.nrows3d.ne.jmax.or.nlays3d.lt.nlayers) then
         print*,'DIAM dimenison mismatch ',ncols3d,nrows3d,nlays3d
         stop
       endif

       istime(5)=sdate3d*100+stime3d/10000     ! file start time in YYYYDDDHH
       ntmpdate=sdate3d
       ntmptime=stime3d
       do n=1,mxrec3d-1
        call nextime(ntmpdate,ntmptime,tstep3d)
       enddo
       ietime(5)=ntmpdate*100+ntmptime/10000   ! file end time in YYYYDDDHH

       if(istime(5).gt.istime(1).or.ietime(5).lt.ietime(1)) then
        print*,'DIAM time mismatch ',istime(5),ietime(5),istime(1),ietime(1)
!jp        stop
       endif

        allocate(workdiam(imax,jmax))
        allocate(workd1(imax,jmax))
        allocate(workd2(imax,jmax))
        allocate(workd3(imax,jmax))
       endif

!---



!-----Grib file header information      
            
      kpds(1)=07          ! ID OF CENTER, NCEP
      kpds(2)=211         ! Generating process ID number, table A
      kpds(3)=id_gribdomain      ! Grid Identification (255 = User defined grid, defined in kgds)
      kpds(4)=128        !  Flag indicating presence of grid description section (almost always
                        !  included in NCEP grib files) or bit map section (BMS) (not usually 
                        !  included, but does happen to be included for your sst files).  It's 
                        !  a binary value; 128 = GDS(yes), BMS(no); 192 = GDS(yes), BMS(yes).
      if(nlayers.eq.1) then
       kpds(6)=1
      else 			
       kpds(6)=107     ! Type of level.  1 = surface (of the Earth, including sea surface).
                       ! Refer to Tables 3 & 3a in GG.  Other types of levels include 100,
                       ! which means standard pressure level. 109 Hybrid, 107 sigma
      endif
      
      print*,'nlayers, kpds(6), kpds(7) =', nlayers, kpds(6), kpds(7)		       
!      kpds(7)=10000   ! Actual value of the height or pressure level.  0 = surface.
      
      call daymon(istime(1)/100,imonth,idate)   ! istime is in YYYYDDDHH

      kpds(8)=mod(istime(1)/100000,100)	! Initial yy of analysis or forecast 
      kpds(9)=imonth  	                ! Initial mm of analysis or forecast 
      kpds(10)=idate  	                ! Initial dd of analysis or forecast
      kpds(11)=mod(istime(1),100)       ! Initial hh of analysis or forecast
      kpds(12)=0		        ! Initial min of analysis or forecast


      kpds(13)=1      ! forecast time unit of kpds(14), table 4, 1= hour

      kpds(15)=0      ! However, if the data in this GRIB record contain, for 
                      ! example, an average of a value from one time to another, kpds(14) will 
                      ! hold the value of the beginning time of the average, and kpds(15) will 
                      ! hold the ending time.
      if(ave1hr) then
       kpds(16)=3
       kpds(17)=1
      else
       kpds(16)=0     ! time range indicator, table 5
      endif 
      kpds(18)=1     ! grib version
!      kpds(19)=129   ! Version number of Parameter Table (table 2)
      kpds(20)=0     ! number missing from average; meaningless for this data 
      kpds(21)=istime(1)/10000000+1                    ! Century of initial time of your data 
!      kpds(22)=6     ! Units decimal scale factor

!----kdgs start
      if(id_gribdomain.eq.148) then	   ! CMAQ 5x domain

! http://www.nco.ncep.noaa.gov/pmb/docs/libs/w3lib/putgb.html
	  
      kgds(1)=3 		! Data representation type (map projection).  0 = Lat/Lon grid. See table 6
      kgds(2)=imax		  ! Number of grid points in x-direction
      kgds(3)=jmax		  ! Number of grid points in y-direction
      kgds(4)=21821		 ! LA1 LAT OF ORIGIN (LOWER LEFT)
      kgds(5)=-120628		 ! LO1 LON OF ORIGIN (LOWER LEFT
      kgds(6)=136		  ! (1001000) Resolution Flag (see Table 7 in GG).  
      kgds(7)=-97000		 ! LOV - ORIENTATION OF GRID
      kgds(8)=12000		 ! DX - X-DIR INCREMENT
      kgds(9)=12000		 ! DY - Y-DIR INCREMENT      
      kgds(10)=0		 !  PROJECTION CENTER FLAG
				  !	 Bit 1 set to 0 if the North pole is on the projection plane.
				  !	 Bit 1 set to 1 if the South pole is on the projection plane.
      kgds(11)=64	    ! SCANNING MODE FLAG (RIGHT ADJ COPY OF OCTET 28)
      kgds(12)=33000	      ! LATIN 1 - FIRST LAT FROM POLE OF SECANT CONE INTER
      kgds(13)=45000	      ! LATIN 2 - SECOND LAT FROM POLE OF SECANT CONE INTER
      endif
     	           
!-----read CMAQ files

      nowdate=nowsdate     
      nowtime=nowstime      
      
      do nt=1,maxrec1
       
       print*,'nowdate,nowtime=',nowdate,nowtime

       kpds(14)=nt-1  ! time range. For the forecast, valid time is kpds(8-12)+kpds(14)

       if(ave1hr) then
        kpds(15)=nt
        write(chtmp,'(i2.2)')nt
       else
        write(chtmp,'(i2.2)')nt-1
       endif
       
       call baopenw(51,trim(outfile)//chtmp,ierr)
       if(ierr.ne.0) then
        print*,'can not open ',trim(outfile)//chtmp
        stop 2001
       endif

       do K=1, nlayers
        
	kpds(7)=nint(sigmas(K)*10000)
	if(nlayers.eq.1) kpds(7)=0
	
! read diam 

        do L=1,nspcdiam
	 if((.not.di3dvar(indexdiam(L))).and.K.gt.1) cycle
! to adjust time for reading diam file
           nowdate1 = nowdate
           nowtime1 = nowtime
           call nextime (nowdate1,nowtime1,10000)
             if(.not.read3('DIAM',diamlist(L),K,nowdate1,nowtime1,workdiam)) stop
           if ( diamlist(L) .eq. 'PM25AT' ) then
            workd1(1:imax,1:jmax)=workdiam(1:imax,1:jmax)
           elseif ( diamlist(L) .eq. 'PM25AC' ) then
            workd2(1:imax,1:jmax)=workdiam(1:imax,1:jmax)
           else
            workd3(1:imax,1:jmax)=workdiam(1:imax,1:jmax)
           endif
 
!         kpds(5)=dgribid(indexdiam(L))        ! parameter ID
!         kpds(19)=dgribtb(indexdiam(L))       ! table version
!         kpds(22)=8-alog10(maxval(work))     ! Units decimal scale factor

!        call gribitb(lb,workdiam,imax,jmax,51,kpds)
        
        enddo
!
! read cmaq

	do L=1,nspcmaq
         
	 if((.not.i3dvar(indexcmaq(L))).and.K.gt.1) cycle

	 if(varlist(L).ne.'PM2.5') then
	      
          if(.not.read3('CHEM3D',varlist(L),K,nowdate,nowtime,work)) stop
      
          work(1:imax,1:jmax)=work(1:imax,1:jmax)*conv_ratio(indexcmaq(L))  ! for gaseous species, ppm to ppb
	 
         else             ! PM2.5
          work(1:imax,1:jmax)=0.
 
          do L2=1,naerospec
           if(.not.read3('CHEM3D',aerospec(L2),K,nowdate,nowtime,pm25)) stop
            if ( L2 .le. 6 ) then
	      work(1:imax,1:jmax)=pm25(1:imax,1:jmax)*workd1(1:imax,1:jmax)  &
                           +work(1:imax,1:jmax)
            elseif (L2 .ge. 7 .and. L2 .le. 33 ) then
              work(1:imax,1:jmax)=pm25(1:imax,1:jmax)*workd2(1:imax,1:jmax)  &
                           +work(1:imax,1:jmax)
            else
              work(1:imax,1:jmax)=pm25(1:imax,1:jmax)*workd3(1:imax,1:jmax)  &
                           +work(1:imax,1:jmax)
           endif
 	  enddo          
         endif
        	
         kpds(5)=gribid(indexcmaq(L))        ! parameter ID
	 kpds(19)=gribtb(indexcmaq(L))       ! table version
	 kpds(22)=8-alog10(maxval(work))     ! Units decimal scale factor	
	
        call gribitb(lb,work,imax,jmax,51,kpds)
	  
        enddo     ! CMAQ species loop
!

        do L=1,nspcmet
	 if(mi3dvar(indexmet(L))) then
	  if(metlist(L).eq.'UWIND'.or.metlist(L).eq.'VWIND') then

	   if(.not.read3('METDOT3D',metlist(L),K,nowdate,nowtime,workdot)) stop
	   if(metlist(L).eq.'UWIND') then
	    do i=1,imax
	     do j=1,jmax
	      work(i,j)=0.5*(workdot(i,j)+workdot(i+1,j))   ! for CMAQ's C grid
	     enddo
	    enddo
	   else
	    do i=1,imax
	     do j=1,jmax
	      work(i,j)=0.5*(workdot(i,j)+workdot(i,j+1))
	     enddo
	    enddo
	   endif   

	  else if(metlist(L).eq.'CWATER') then     ! total condensed water
 	   
	   if(.not.read3('METCRO3D','QC',K,nowdate,nowtime,work)) stop   ! cloud water
	   if(.not.read3('METCRO3D','QR',K,nowdate,nowtime,pm25)) stop   ! rain water
	   work(1:imax,1:jmax)=work(1:imax,1:jmax)+pm25(1:imax,1:jmax)
	   if(.not.read3('METCRO3D','QI',K,nowdate,nowtime,pm25)) stop   ! ice water
	   work(1:imax,1:jmax)=work(1:imax,1:jmax)+pm25(1:imax,1:jmax)
	   if(.not.read3('METCRO3D','QI',K,nowdate,nowtime,pm25)) stop   ! snow water
	   work(1:imax,1:jmax)=work(1:imax,1:jmax)+pm25(1:imax,1:jmax)
	   
	  else	  
	   if(.not.read3('METCRO3D',metlist(L),K,nowdate,nowtime,work)) stop
	  endif 
	 else if((.not.mi3dvar(indexmet(L))).and.K.eq.1) then
	  if(.not.read3('METCRO2D',metlist(L),K,nowdate,nowtime,work)) stop
	 else
	  cycle
	 endif
       
         kpds(5)=mgribid(indexmet(L))        ! parameter ID
	 kpds(19)=mgribtb(indexmet(L))       ! table version
	 kpds(22)=8-alog10(maxval(work))    ! Units decimal scale factor	
	
         call gribitb(lb,work,imax,jmax,51,kpds)
	enddo  ! met species loop
	   	  
       enddo      ! K loop 

       call nextime(nowdate,nowtime,10000)
	
       call baclose(51,ierr)
       
       enddo     ! time loop

       iflag=shut3()
      end
       

      subroutine gribitb(ln,ozout,im,jm,iunit,KPDSOUT)

      parameter (mxbit=16,lenpds=28,lengds=32)
      character*1  kbuf(30+lenpds+lengds+im*jm*(mxbit+2)/8)

      character*1  iflag*1, pds*28
      integer ibdsfl(9), igrd(im,jm),igds(18), ibmap(im,jm)
      integer KPDSOUT(25),id(25),kgds(20)
      real ozout(im,jm)
!      save kbuf
!****************************************************************
!     PREPARE GRIB PDS
!     SET ARRAY ID VALUES TO GENERATE GRIB1 PDS.  
        
      id(1) =28 !NUMBER OF BYTES IN PRODUCT DEFINITION SECTION(PDS)
      id(2) =KPDSOUT(19)!PARAMETER TABLE VERSION NO (2 or 129 or 130)
      id(3) =KPDSOUT(1)  !IDENTIFICATION OF ORIGINATING CENTER
      id(4) =KPDSOUT(2)!MODEL IDENTIFICATION (BY ORIGINATING CENTER)
      id(5) =KPDSOUT(3)!GRID IDENTIFICATION
      id(6) =1  !IF NO GDS SECTION, 1 IF GDS SECTION IS INCLUDED
      id(7) =0  !IF NO BMS SECTION, 1 IF BMS SECTION IS INCLUDED
      id(8) =KPDSOUT(5)
      id(9) =  KPDSOUT(6)                !Had temporarily for 5x made into 105
      id(11) = KPDSOUT(7)
      id(10) = KPDSOUT(7)/256
      id(12)=KPDSOUT(8)          !  YEAR OF CENTURY
      id(13)=KPDSOUT(9)          !  MONTH OF YEAR
      id(14)=KPDSOUT(10)         !  DAY OF MONTH
      id(15)=KPDSOUT(11)         !  HOUR OF DAY
      id(16)=KPDSOUT(12)         !  MINUTE OF HOUR
      id(17)=KPDSOUT(13)         !  FCST TIME UNIT: 1 for h
      id(18)=KPDSOUT(14)         !  P1 PERIOD OF TIME
      id(19)=KPDSOUT(15)         !  P2 PERIOD OF TIME
      id(20)=KPDSOUT(16)         !  TIME RANGE INDICATOR
      id(21)=KPDSOUT(17)         !  NUMBER INCLUDED IN AVERAGE
      id(22)=0                   !  NUMBER MISSING FROM AVERAGES
      id(23)=KPDSOUT(21)         !  CENTURY 
      id(24)=0                   !  RESERVED - SET TO 0
      sgdg = 5.0        !  MAXIMUM SIGNIFICANT DIGITS TO KEEP
                        !  (E.G. SGDS=3.0 KEEPS 3 SIGNIFICANT DIGITS)
                        !  OR BINARY PRECISION IF <0
                        !  (E.G. SGDS=-2.0 KEEPS FIELD TO NEAREST 1/4
                        !             -3.0 "                    " 1/8
                        !  2**SGDS PRECISION)
      ibm=0
      ibitm=0
      ibitm = im*jm
      ibmap=1
      ibm =1    !as dictated by id(7)=0

      nout = im*jm
      call get_bits(ibm,sgdg,nout,ibmap,ozout,   &
                   ideci,ozout,gmin,gmax,nbit)

      id(25) =ideci     !   SCALING POWER OF 10

      itype=0
      ibitl = min(nbit,mxbit)
      ipflag=0
      igflag=0
      igrid=id(5)

      do 20 k = 1,18
         igds(k) = 0
 20   continue

      icomp=1
      ibflag=0
      iblen=nout
      do 30 k = 1,9
         ibdsfl(k) = 0
 30   continue

      call w3fi72(itype,ozout,igrd,ibitl,      &
     &            ipflag,id,pds,               &
     &            igflag,igrid,igds,icomp,     &
     &            ibflag,ibmap,iblen,          &
     &            ibdsfl,                      &
     &            npts,kbuf,itot,ier)


      call wryte(iunit,itot,kbuf)

      return
      end

      SUBROUTINE GET_BITS(IBM,SGDS,LEN,MG,G,ISCALE,GROUND,    &
     &                    GMIN,GMAX,NBIT)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    GET_BITS      COMPUTE NUMBER OF BITS AND ROUND FIELD.
!   PRGMMR: IREDELL          ORG: W/NP23     DATE: 92-10-31
!
! ABSTRACT: THE NUMBER OF BITS REQUIRED TO PACK A GIVEN FIELD
!   AT A PARTICULAR DECIMAL SCALING IS COMPUTED USING THE FIELD RANGE.
!   THE FIELD IS ROUNDED OFF TO THE DECIMAL SCALING FOR PACKING.
!   THE MINIMUM AND MAXIMUM ROUNDED FIELD VALUES ARE ALSO RETURNED.
!   GRIB BITMAP MASKING FOR VALID DATA IS OPTIONALLY USED.
!
! PROGRAM HISTORY LOG:
!   92-10-31  IREDELL
!   95-04-14  BALDWIN - MODIFY FOLLOWING KEITH BRILL'S CODE
!			TO USE SIG DIGITS TO COMPUTE DEC SCALE
!
! USAGE:   CALL GET_BITS(IBM,ISGDS,LEN,MG,G,ISCALE,GROUND,GMIN,GMAX,NBIT)
!   INPUT ARGUMENT LIST:
!     IBM      - INTEGER BITMAP FLAG (=0 FOR NO BITMAP)
!     SGDS     - MAXIMUM SIGNIFICANT DIGITS TO KEEP
!		 (E.G. SGDS=3.0 KEEPS 3 SIGNIFICANT DIGITS)
!		 OR BINARY PRECISION IF <0
!		 (E.G. SGDS=-2.0 KEEPS FIELD TO NEAREST 1/4
!			    -3.0 "		      " 1/8
!			   2**SGDS PRECISION)
!      LEN	- INTEGER LENGTH OF THE FIELD AND BITMAP
!      MG	- INTEGER (LEN) BITMAP IF IBM=1 (0 TO SKIP, 1 TO KEEP)
!      G	- REAL (LEN) FIELD
!
!    OUTPUT ARGUMENT LIST:
!      ISCALE	- INTEGER DECIMAL SCALING
!      GROUND	- REAL (LEN) FIELD ROUNDED TO DECIMAL SCALING
!      GMIN	- REAL MINIMUM VALID ROUNDED FIELD VALUE
!      GMAX	- REAL MAXIMUM VALID ROUNDED FIELD VALUE
!      NBIT	- INTEGER NUMBER OF BITS TO PACK
!
!  SUBPROGRAMS CALLED:
!    ISRCHNE  - FIND FIRST VALUE IN AN ARRAY NOT EQUAL TO TARGET VALUE
!
!  ATTRIBUTES:
!    LANGUAGE: FORTRAN


      DIMENSION MG(LEN),G(LEN),GROUND(LEN)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  DETERMINE EXTREMES WHERE BITMAP IS ON
!
      IF(IBM.EQ.0) THEN
        GMAX=G(1)
        GMIN=G(1)
        DO I=2,LEN
          GMAX=MAX(GMAX,G(I))
          GMIN=MIN(GMIN,G(I))
        ENDDO
      ELSE
        I1=0
        DO I=1,LEN
          IF(MG(I).NE.0.AND.I1.EQ.0) I1=I
        ENDDO
        IF(I1.GT.0.AND.I1.LE.LEN) THEN
          GMAX=G(I1)
          GMIN=G(I1)
          DO I=I1+1,LEN
            IF(MG(I).NE.0) THEN
              GMAX=MAX(GMAX,G(I))
              GMIN=MIN(GMIN,G(I))
            ENDIF
          ENDDO
        ELSE
          GMAX=0.
          GMIN=0.
        ENDIF
      ENDIF
!
!
      CALL FNDBIT  ( GMIN, GMAX, SGDS, NBIT, ISCALE, RMIN, IRETT)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	RETURN
	END


	SUBROUTINE FNDBIT  ( rmin, rmax, rdb, nmbts, iscale, rmn, iret )
!************************************************************************
!* FNDBIT								*
!*									*
!* This subroutine computes the number of packing bits given the	*
!* maximum number (< 50) of significant digits to preserve or the	*
!* binary precision to store the data.  The binary precision is given	*
!* as zero, a negative integer, or as a postitive integer greater than  *
!* or equal to 50.  If the binary precision is given, ISCALE will	*
!* always be zero in this case. 					*
!*									*
!* The binary precision translates as follows:  			*
!*     53  =>  store data to nearest 8  				*
!*     52  =>  store data to nearest 4  				*
!*     51  =>  store data to nearest 2  				*
!*     50  =>  store data to nearest 1  				*
!*	0  =>  store data to nearest 1  				*
!*     -1  =>  store data to nearest 1/2				*
!*     -2  =>  store data to nearest 1/4				*
!*     -3  =>  store data to nearest 1/8				*
!*									*
!* Note that RDB - 50 give the nearest whole power of two for binary	*
!* precision.								*
!*									*
!* Note that a fractional number of significant digits is allowed.	*
!*									*
!* FNDBIT ( RMIN, RMAX, RDB, NBITS, ISCALE, RMN, IRET ) 		*
!*									*
!* Input parameters:							*
!*	RMIN		REAL		Minimum value			*
!*	RMAX		REAL		Maximum value			*
!*	RDB		REAL		Maximum # of significant digits *
!*					  OR binary precision if < 0	*
!*									*
! * Output parameters:							*
! *	NBITS		INTEGER 	Number of bits for packing	*
! *	ISCALE  	INTEGER 	Power of 10 scaling to use	*
! *	RMN		REAL		Rounded miniumum		*
! *	IRET		INTEGER 	Return code			*
! *					  0 = normal return		*
! **									*
! * Log: 								*
! * K. Brill/NMC 	06/92						*
! * K. Brill/EMC 	12/95	Added binary precision; added RMN	*
! * K. Brill/EMC 	 1/97	Add .5 in rr= & rng2= for better rnd off*
! * K. Brill/EMC 	 1/97	Use 10**iscale in rounding the min	*
! ************************************************************************

	DATA		rln2/0.69314718/
!-----------------------------------------------------------------------
	iret = 0
	icnt = 0
	iscale = 0
	rmn = rmin
	range = rmax - rmin
	IF ( range .le. 0.00 ) THEN
	    nmbts = 8
	    RETURN
	END IF

	IF ( rdb .gt. 0.0 .and. rdb .lt. 50. ) THEN
	    po = FLOAT ( INT ( ALOG10 ( range ) ) )
	    IF ( range .lt. 1.00 ) po = po - 1.
	    po = po - rdb + 1.
	    iscale = - INT ( po )
	    rr = range * 10. ** ( -po ) + .5
	    nmbts = INT ( ALOG ( rr ) / rln2 ) + 1
	ELSE
	    ibin = NINT ( -rdb )
	    IF ( ibin .le. -50. ) ibin = ibin + 50
	    rng2 = range * 2. ** ibin + .5
	    nmbts = INT ( ALOG ( rng2 ) / rln2 ) + 1
	END IF
        IF(NMBTS.GT.0) then

!*	Compute RMN, the first packable value less than or equal to
!*	RMIN.

	tp = 10. ** iscale
	x = ( ALOG ( range * tp ) - ALOG ( 2 ** nmbts - 1. ) ) / rln2
	ixp = INT ( x )
	IF ( FLOAT ( ixp ) .ne. x .and. x .gt. 0. ) ixp = ixp + 1
	irmn = NINT ( ( rmin * tp ) / ( 2. ** ixp ) )
	rmn = FLOAT ( irmn ) * ( 2. ** ixp )
	IF ( rmn .gt. rmin * tp ) rmn = rmn - ( 2. ** ixp )
	rmn = rmn / tp

           rmn = rmn / 10. ** iscale
        ELSE
          nmbts=0
          rmn = rmin
          IF(ABS(rmin).GE.1.) THEN
           ISCALE=INT(ALOG10(ABS(rmin)))
          ELSE IF (ABS(rmin).LT.1..AND.ABS(rmin).GT.0.) then
           ISCALE=INT(ALOG10(ABS(rmin)))+1
          ELSE
           ISCALE=0
          ENDIF
        ENDIF

	RETURN
	END

      SUBROUTINE W3FI71 (IGRID, IGDS, IERR)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    W3FI71      MAKE ARRAY USED BY GRIB PACKER FOR GDS
!   PRGMMR: R.E.JONES        ORG: W/NMC42    DATE: 93-03-26
!
! ABSTRACT: W3FI71 MAKES A 18, 37, 55, 64, OR 91 WORD INTEGER ARRAY
!     USED BY W3FI72 GRIB PACKER TO MAKE THE GRID DESCRIPTION SECTION
!     (GDS) - SECTION 2.
!
! PROGRAM HISTORY LOG:
!   92-02-21  R.E.JONES
!   92-07-01  M. FARLEY    ADDED REMARKS FOR 'IGDS' ARRAY ELEMENTS.
!			   ADDED LAMBERT CONFORMAL GRIDS AND ENLARGED
!			   IDGS ARRAY FROM 14 TO 18 WORDS.
!   92-10-03  R.E.JONES    ADDED CORRECTIONS TO AWIPS GRIB TABLES
!   92-10-16  R.E.JONES    ADD GAUSSIAN GRID 126 TO TABLES
!   92-10-18  R.E.JONES    CORRECTIONS TO LAMBERT CONFORMAL TABLES
!			   AND OTHER TABLES
!   92-10-19  R.E.JONES    ADD GAUSSIAN GRID  98 TO TABLES
!   93-01-25  R.E.JONES    ADD ON84 GRIDS 87, 106, 107 TO TABLES
!   93-03-10  R.E.JONES    ADD ON84 GRIDS 1, 55, 56 TO TABLES
!   93-03-26  R.E.JONES    ADD GRIB GRIDS 2, 3 TO TABLES
!   93-03-29  R.E.JONES    ADD SAVE STATEMENT
!   93-06-15  R.E.JONES    ADD GRIB GRIDS 37 TO 44 TO TABLES
!   93-09-29  R.E.JONES    GAUSSIAN GRID DOCUMENT NOT CORRECT,
!			   W3FI74 WILL BE CHANGED TO AGREE WITH
!			   IT. GAUSSIAN GRID 98 TABLE HAS WRONG
!			   VALUE.
!   93-10-12  R.E.JONES    CHANGES FOR ON388 REV. OCT 8,1993 FOR
!			   GRID 204, 208.
!   93-10-13  R.E.JONES    CORRECTION FOR GRIDS 37-44, BYTES 7-8,
!			   24-25 SET TO ALL BITS 1 FOR MISSING.
!   93-11-23  R.E.JONES    ADD GRIDS 90-93 FOR ETA MODEL
!			   ADD GRID 4 FOR 720*361 .5 DEG. GRID
!   94-04-12  R.E.JONES    CORRECTION FOR GRID 28
!   94-06-01  R.E.JONES    ADD GRID 45, 288*145 1.25 DEG. GRID
!   94-06-22  R.E.JONES    ADD GRIDS 94, 95 FOR ETA MODEL
!   95-04-11  R.E.JONES    ADD GRIDS 96, 97 FOR ETA MODEL
!   95-05-19  R.E.JONES    ADD FROM 20 KM ETA MODEL AWIPS GRID 215
!   95-10-19  R.E.JONES    ADD FROM 20 KM ETA MODEL ALASKA GRID 216
!   95-10-31  IREDELL	   REMOVED SAVES AND PRINTS
!   96-05-08  IREDELL	   CORRECT FIRST LATITUDE FOR GRIDS 27 AND 28
!   96-07-02  R.E.JONES    ADD FROM 10 KM ETA MODEL OLYMPIC GRID 218
!   96-07-02  R.E.JONES    ADD 196 FOR ETA MODEL
!   96-08-15  R.E.JONES    ADD O.N. 84 GRID 8 AND 53 AS GRIB GRID 8
! 			   AND 53
!   96-11-29  R.E.JONES    CORRECTION TO TABLES FOR GRID 21-26, 61-64
!   97-01-31  IREDELL	   CORRECT FIRST LATITUDE FOR GRID 30
!   97-10-20  IREDELL	   CORRECT LAST LONGITUDE FOR GRID 98
!   98-07-07  Gilbert	   Add grids 217 and 219 through 235
!   98-09-21  BALDWIN	   ADD GRIDS 190, 192 FOR ETA MODEL
!   99-01-20  BALDWIN	   ADD GRIDS 236, 237
!   99-08-18  IREDELL	   ADD GRID 170
!   01-03-08  ROGERS	   CHANGED ETA GRIDS 90-97, ADDED ETA GRIDS
!			   194, 198. ADDED AWIPS GRIDS 241,242,243,
!			   245, 246, 247, 248, AND 250
!   01-03-19  VUONG	   ADDED AWIPS GRIDS 238,239,240, AND 244
!   01-04-02  VUONG	   CORRECT LAST LONGITUDE FOR GRID 225
!   01-05-03  ROGERS	   ADDED GRID 249
!   01-10-10  ROGERS	   REDEFINED 218 FOR 12-KM ETA
!			   REDEFINED GRID 192 FOR NEW 32-KM ETA GRID
!   02-03-27  VUONG	   ADDED RSAS GRID 88 AND AWIPS GRIDS 251 AND 252
!    02-08-06  ROGERS	    REDEFINED GRIDS 90-93,97,194,245-250 FOR THE
!			    8KM HI-RES-WINDOW MODEL AND ADD AWIPS GRID 253
!  2003-06-30  GILBERT      ADDED GRIDS 145 and 146 for CMAQ
!			    and GRID 175 for AWIPS over GUAM.
!  2003-07-08  VUONG	    CORRECTED LATITUDE FOR GRID 253 AND 170, ADD GRID
!  2005-02-08  PLee	    GRIDS 138, 147, 148 for CMAQ, 255 CONUS
!			    110, 127, 171 AND 172
!  2009-07-08  PLee	    GRIDS 139, 140 for CMAQ, HI and AK
!
!  USAGE:    CALL W3FI71 (IGRID, IGDS, IERR)
!    INPUT ARGUMENT LIST:
!      IGRID	   - GRIB GRID NUMBER, OR OFFICE NOTE 84 GRID NUMBER
!
!    OUTPUT ARGUMENT LIST:
!      IGDS	 - 18, 37, 55, 64, OR 91 WORD INTEGER ARRAY WITH
!		   INFORMATION TO MAKE A GRIB GRID DESCRIPTION SECTION.
!      IERR	  - 0  CORRECT EXIT
!		    1  GRID TYPE IN IGRID IS NOT IN TABLE
!
!  REMARKS:
!     1) OFFICE NOTE GRID TYPE 26 IS 6 IN GRIB, 26 IS AN
!	 INTERNATIONAL EXCHANGE GRID.
!
!     2) VALUES RETURNED IN 18, 37, 55, 64, OR 91 WORD INTEGER ARRAY
!	  IGDS VARY DEPENDING ON GRID REPRESENTATION TYPE.
!
!	 LAT/LON GRID:
!	     IGDS( 1) = NUMBER OF VERTICAL COORDINATES
!	     IGDS( 2) = PV, PL OR 255
!	     IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6)
!	     IGDS( 4) = NO. OF POINTS ALONG A LATITUDE
!	     IGDS( 5) = NO. OF POINTS ALONG A LONGITUDE MERIDIAN
!	     IGDS( 6) = LATITUDE OF ORIGIN (SOUTH - IVE)
!	     IGDS( 7) = LONGITUDE OF ORIGIN (WEST -IVE)
!	     IGDS( 8) = RESOLUTION FLAG (CODE TABLE 7)
!	     IGDS( 9) = LATITUDE OF EXTREME POINT (SOUTH - IVE)
!	     IGDS(10) = LONGITUDE OF EXTREME POINT (WEST - IVE)
!	     IGDS(11) = LATITUDE INCREMENT
!	     IGDS(12) = LONGITUDE INCREMENT
!	     IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
!	     IGDS(14) = ... THROUGH ...
!	     IGDS(18) =   ... NOT USED FOR THIS GRID
!	     IGDS(19) - IGDS(91) FOR GRIDS 37-44, NUMBER OF POINTS
!			IN EACH OF 73 ROWS.
!
!	 GAUSSIAN GRID:
!	     IGDS( 1) = ... THROUGH ...
!	     IGDS(10) =   ... SAME AS LAT/LON GRID
!	     IGDS(11) = NUMBER OF LATITUDE LINES BETWEEN A POLE
!			AND THE EQUATOR
!	     IGDS(12) = LONGITUDE INCREMENT
!	     IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
!	     IGDS(14) = ... THROUGH ...
!	     IGDS(18) =   ... NOT USED FOR THIS GRID
!
!	 SPHERICAL HARMONICS:
!	     IGDS( 1) = NUMBER OF VERTICAL COORDINATES
!	     IGDS( 2) = PV, PL OR 255
!	     IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6)
!	     IGDS( 4) = J - PENTAGONAL RESOLUTION PARAMETER
!	     IGDS( 5) = K - PENTAGONAL RESOLUTION PARAMETER
!	     IGDS( 6) = M - PENTAGONAL RESOLUTION PARAMETER
!	    IGDS( 7) = REPRESENTATION TYPE (CODE TABLE 9)
!	    IGDS( 8) = REPRESENTATION MODE (CODE TABLE 10)
!	    IGDS( 9) = ... THROUGH ...
!	    IGDS(18) =   ... NOT USED FOR THIS GRID
!
!	POLAR STEREOGRAPHIC:
!	    IGDS( 1) = NUMBER OF VERTICAL COORDINATES
!	    IGDS( 2) = PV, PL OR 255
!	    IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6)
!	    IGDS( 4) = NO. OF POINTS ALONG X-AXIS
!	    IGDS( 5) = NO. OF POINTS ALONG Y-AXIS
!	    IGDS( 6) = LATITUDE OF ORIGIN (SOUTH -IVE)
!	    IGDS( 7) = LONGITUTE OF ORIGIN (WEST -IVE)
!	    IGDS( 8) = RESOLUTION FLAG (CODE TABLE 7)
!	    IGDS( 9) = LONGITUDE OF MERIDIAN PARALLEL TO Y-AXIS
!	    IGDS(10) = X-DIRECTION GRID LENGTH (INCREMENT)
!	    IGDS(11) = Y-DIRECTION GRID LENGTH (INCREMENT)
!	    IGDS(12) = PROJECTION CENTER FLAG (0=NORTH POLE ON PLANE,
!					       1=SOUTH POLE ON PLANE,
!	    IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
!	    IGDS(14) = ... THROUGH ...
!	    IGDS(18) =   .. NOT USED FOR THIS GRID
!
!	MERCATOR:
!	    IGDS( 1) = ... THROUGH ...
!	    IGDS(12) =   ... SAME AS LAT/LON GRID
!	    IGDS(13) = LATITUDE AT WHICH PROJECTION CYLINDER
!			 INTERSECTS EARTH
!	    IGDS(14) = SCANNING MODE FLAGS
!	    IGDS(15) = ... THROUGH ...
!	    IGDS(18) =   .. NOT USED FOR THIS GRID
!
!	LAMBERT CONFORMAL:
!	    IGDS( 1) = NUMBER OF VERTICAL COORDINATES
!	    IGDS( 2) = PV, PL OR 255
!	    IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6)
!	    IGDS( 4) = NO. OF POINTS ALONG X-AXIS
!	    IGDS( 5) = NO. OF POINTS ALONG Y-AXIS
!	    IGDS( 6) = LATITUDE OF ORIGIN (SOUTH -IVE)
!	    IGDS( 7) = LONGITUTE OF ORIGIN (WEST -IVE)
!	    IGDS( 8) = RESOLUTION FLAG (CODE TABLE 7)
!	    IGDS( 9) = LONGITUDE OF MERIDIAN PARALLEL TO Y-AXIS
!	    IGDS(10) = X-DIRECTION GRID LENGTH (INCREMENT)
!	    IGDS(11) = Y-DIRECTION GRID LENGTH (INCREMENT)
!	    IGDS(12) = PROJECTION CENTER FLAG (0=NORTH POLE ON PLANE,
!					       1=SOUTH POLE ON PLANE,
!	    IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
!	    IGDS(14) = NOT USED
!	    IGDS(15) = FIRST LATITUDE FROM THE POLE AT WHICH THE
!		       SECANT CONE CUTS THE SPERICAL EARTH
!	    IGDS(16) = SECOND LATITUDE ...
!	    IGDS(17) = LATITUDE OF SOUTH POLE (MILLIDEGREES)
!	    IGDS(18) = LONGITUDE OF SOUTH POLE (MILLIDEGREES)
!
!	ARAKAWA SEMI-STAGGERED E-GRID ON ROTATED LAT/LON GRID
!	    IGDS( 1) = NUMBER OF VERTICAL COORDINATES
!	    IGDS( 2) = PV, PL OR 255
!	    IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6) [201]
!	    IGDS( 4) = NI  - TOTAL NUMBER OF ACTUAL DATA POINTS
!			     INCLUDED ON GRID
!	    IGDS( 5) = NJ  - DUMMY SECOND DIMENSION; SET=1
!	    IGDS( 6) = LA1 - LATITUDE  OF FIRST GRID POINT
!	    IGDS( 7) = LO1 - LONGITUDE OF FIRST GRID POINT
!	    IGDS( 8) = RESOLUTION AND COMPONENT FLAG (CODE TABLE 7)
!	    IGDS( 9) = LA2 - NUMBER OF MASS POINTS ALONG
!			     SOUTHERNMOST ROW OF GRID
!	    IGDS(10) = LO2 - NUMBER OF ROWS IN EACH COLUMN
!	    IGDS(11) = DI  - LONGITUDINAL DIRECTION INCREMENT
!	    IGDS(12) = DJ  - LATITUDINAL  DIRECTION INCREMENT
!	    IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
!	    IGDS(14) = ... THROUGH ...
!	    IGDS(18) = ... NOT USED FOR THIS GRID (SET TO ZERO)
!
!	ARAKAWA FILLED E-GRID ON ROTATED LAT/LON GRID
!	    IGDS( 1) = NUMBER OF VERTICAL COORDINATES
!	    IGDS( 2) = PV, PL OR 255
!	    IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6) [202]
!	    IGDS( 4) = NI  - TOTAL NUMBER OF ACTUAL DATA POINTS
!			     INCLUDED ON GRID
!	    IGDS( 5) = NJ  - DUMMY SECOND DIMENTION; SET=1
!	    IGDS( 6) = LA1 - LATITUDE LATITUDE OF FIRST GRID POINT
!	    IGDS( 7) = LO1 - LONGITUDE OF FIRST GRID POINT
!	    IGDS( 8) = RESOLUTION AND COMPONENT FLAG (CODE TABLE 7)
!	    IGDS( 9) = LA2 - NUMBER OF (ZONAL) POINTS IN EACH ROW
!	    IGDS(10) = LO2 - NUMBER OF (MERIDIONAL) POINTS IN EACH
!			     COLUMN
!	    IGDS(11) = DI  - LONGITUDINAL DIRECTION INCREMENT
!	    IGDS(12) = DJ  - LATITUDINAL  DIRECTION INCREMENT
!	    IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
!	    IGDS(14) = ... THROUGH ...
!	    IGDS(18) = ... NOT USED FOR THIS GRID
!
!	ARAKAWA STAGGERED E-GRID ON ROTATED LAT/LON GRID
!	    IGDS( 1) = NUMBER OF VERTICAL COORDINATES
!	    IGDS( 2) = PV, PL OR 255
!	    IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6) [203]
!	    IGDS( 4) = NI  - NUMBER OF DATA POINTS IN EACH ROW
!	    IGDS( 5) = NJ  - NUMBER OF ROWS
!	    IGDS( 6) = LA1 - LATITUDE OF FIRST GRID POINT
!	    IGDS( 7) = LO1 - LONGITUDE OF FIRST GRID POINT
!	    IGDS( 8) = RESOLUTION AND COMPONENT FLAG (CODE TABLE 7)
!	    IGDS( 9) = LA2 - CENTRAL LATITUDE
!	    IGDS(10) = LO2 - CENTRAL LONGTITUDE
!	    IGDS(11) = DI  - LONGITUDINAL DIRECTION INCREMENT
!	    IGDS(12) = DJ  - LATITUDINAL  DIRECTION INCREMENT
!	    IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
!	    IGDS(14) = ... THROUGH ...
!	    IGDS(18) = ... NOT USED FOR THIS GRID
!
!   SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM SP
!


      INTEGER       IGRID
      INTEGER       IGDS  (*)
      INTEGER       GRD1  (18)
      INTEGER       GRD2  (18)
      INTEGER       GRD3  (18)
      INTEGER       GRD4  (18)
      INTEGER       GRD5  (18)
      INTEGER       GRD6  (18)
      INTEGER       GRD8  (18)
      INTEGER       GRD21 (55)
      INTEGER       GRD22 (55)
      INTEGER       GRD23 (55)
      INTEGER       GRD24 (55)
      INTEGER       GRD25 (37)
      INTEGER       GRD26 (37)
      INTEGER       GRD27 (18)
      INTEGER       GRD28 (18)
      INTEGER       GRD29 (18)
      INTEGER       GRD30 (18)
      INTEGER       GRD33 (18)
      INTEGER       GRD34 (18)
      INTEGER       GRD37 (91)
      INTEGER       GRD38 (91)
      INTEGER       GRD39 (91)
      INTEGER       GRD40 (91)
      INTEGER       GRD41 (91)
      INTEGER       GRD42 (91)
      INTEGER       GRD43 (91)
      INTEGER       GRD44 (91)
      INTEGER       GRD45 (18)
      INTEGER       GRD53 (18)
      INTEGER       GRD55 (18)
      INTEGER       GRD56 (18)
      INTEGER       GRD61 (64)
      INTEGER       GRD62 (64)
      INTEGER       GRD63 (64)
      INTEGER       GRD64 (64)
      INTEGER       GRD85 (18)
      INTEGER       GRD86 (18)
      INTEGER       GRD87 (18)
      INTEGER       GRD88 (18)
      INTEGER       GRD90 (18)
      INTEGER       GRD91 (18)
      INTEGER       GRD92 (18)
      INTEGER       GRD93 (18)
      INTEGER       GRD94 (18)
      INTEGER       GRD95 (18)
      INTEGER       GRD96 (18)
      INTEGER       GRD97 (18)
      INTEGER       GRD98 (18)
      INTEGER       GRD100(18)
      INTEGER       GRD101(18)
      INTEGER       GRD103(18)
      INTEGER       GRD104(18)
      INTEGER       GRD105(18)
      INTEGER       GRD106(18)
      INTEGER       GRD107(18)
      INTEGER       GRD110(18)
      INTEGER       GRD126(18)
      INTEGER       GRD127(18)
!***TLO 11 Mar 04 and PLEE July 8 2009 ***start
      INTEGER       GRD138(18)
      INTEGER       GRD139(18)
      INTEGER       GRD140(18)
      INTEGER       GRD255(18)
!PL*PLee 08 Feb 2005 ***Comment out starts
!PL   INTEGER       GRD139(18)
!***TLO 11 Mar 04 ***end
!***TLO 08 Mar 04 ***start
!PL   INTEGER       GRD140(18)
!PL   INTEGER       GRD141(18)
!PL   INTEGER       GRD142(18)
!PL*PLee 08 Feb 2005 ***renaming Grid 142 to 147 starts
      INTEGER       GRD147(18)
!PL*PLee 08 Feb 2005 ***renaming Grid 142 to 147 ends
!PL   INTEGER       GRD144(18)
!***TLO 08 Mar 04 ***end
      INTEGER       GRD145(18)
      INTEGER       GRD146(18)
!***TLO 08 Mar 04 ***start
!PL   INTEGER       GRD148(18)
!PL   INTEGER       GRD149(18)
!PL   INTEGER       GRD150(18)
!PL*PLee 08 Feb 2005 ***renaming Grid 150 to 148 starts
      INTEGER       GRD148(18)
!PL*PLee 08 Feb 2005 ***renaming Grid 150 to 148 ends
!***TLO 08 Mar 04 ***end
      INTEGER       GRD170(18)
      INTEGER       GRD171(18)
      INTEGER       GRD172(18)
      INTEGER       GRD175(18)
      INTEGER       GRD190(18)
      INTEGER       GRD192(18)
      INTEGER       GRD194(18)
      INTEGER       GRD196(18)
      INTEGER       GRD198(18)
      INTEGER       GRD201(18)
      INTEGER       GRD202(18)
      INTEGER       GRD203(18)
      INTEGER       GRD204(18)
      INTEGER       GRD205(18)
      INTEGER       GRD206(18)
      INTEGER       GRD207(18)
      INTEGER       GRD208(18)
      INTEGER       GRD209(18)
      INTEGER       GRD210(18)
      INTEGER       GRD211(18)
      INTEGER       GRD212(18)
      INTEGER       GRD213(18)
      INTEGER       GRD214(18)
      INTEGER       GRD215(18)
      INTEGER       GRD216(18)
      INTEGER       GRD217(18)
      INTEGER       GRD218(18)
      INTEGER       GRD219(18)
      INTEGER       GRD220(18)
      INTEGER       GRD221(18)
      INTEGER       GRD222(18)
      INTEGER       GRD223(18)
      INTEGER       GRD224(18)
      INTEGER       GRD225(18)
      INTEGER       GRD226(18)
      INTEGER       GRD227(18)
      INTEGER       GRD228(18)
      INTEGER       GRD229(18)
      INTEGER       GRD230(18)
      INTEGER       GRD231(18)
      INTEGER       GRD232(18)
      INTEGER       GRD233(18)
      INTEGER       GRD234(18)
      INTEGER       GRD235(18)
      INTEGER       GRD236(18)
      INTEGER       GRD237(18)
      INTEGER       GRD238(18)
      INTEGER       GRD239(18)
      INTEGER       GRD240(18)
      INTEGER       GRD241(18)
      INTEGER       GRD242(18)
      INTEGER       GRD243(18)
      INTEGER       GRD244(18)
      INTEGER       GRD245(18)
      INTEGER       GRD246(18)
      INTEGER       GRD247(18)
      INTEGER       GRD248(18)
      INTEGER       GRD249(18)
      INTEGER       GRD250(18)
      INTEGER       GRD251(18)
      INTEGER       GRD252(18)
      INTEGER       GRD253(18)

      DATA  GRD1  / 0, 255, 1,  73, 23, -48090,       0, 128,   48090,  &
     &       0, 513669,513669, 22500, 64, 0, 0, 0, 0/
      DATA  GRD2  / 0, 255, 0, 144, 73,  90000,       0, 128,  -90000,  &
     &   -2500,   2500, 2500,  0, 0, 0, 0, 0, 0/
      DATA  GRD3  / 0, 255, 0, 360,181,  90000,       0, 128,  -90000,  &
     &   -1000,   1000, 1000,  0, 0, 0, 0, 0, 0/
      DATA  GRD4  / 0, 255, 0, 720,361,  90000,       0, 128,  -90000,  &
     &    -500,    500,  500,  0, 0, 0, 0, 0, 0/
      DATA  GRD5  / 0, 255, 5,  53, 57,   7647, -133443,   8, -105000,  &
     &  190500, 190500, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD6  / 0, 255, 5,  53, 45,   7647, -133443,   8, -105000,  &
     &  190500, 190500, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD8  / 0, 255, 1, 116, 44, -48670,    3104, 128,   61050,  &
     &       0, 318830, 318830, 22500, 64, 0, 0, 0, 0/
      DATA  GRD21 / 0,  33, 0,65535,37,      0,       0, 128,   90000,  &
     &  180000,   2500, 5000, 64, 0, 0, 0, 0, 0,                        &
     & 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,      &
     & 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,      &
     & 37, 37, 37, 37, 37, 37,  1/
      DATA  GRD22 / 0,  33, 0,65535,37,      0, -180000, 128,   90000,  &
     &       0,   2500, 5000, 64, 0, 0, 0, 0, 0,                        &
     & 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,      &
     & 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,      &
     & 37, 37, 37, 37, 37, 37,  1/
      DATA  GRD23 / 0,  33, 0,65535, 37, -90000,       0, 128,       0, &
     &  180000,   2500, 5000, 64, 0, 0, 0, 0, 0,			&
     &  1, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,	&
     & 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,	&
     & 37, 37, 37, 37, 37, 37, 37/
      DATA  GRD24 / 0,  33, 0,65535, 37, -90000, -180000, 128,       0, &
     &       0,   2500, 5000, 64, 0, 0, 0, 0, 0,			&
     &  1, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,	&
     & 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,	&
     & 37, 37, 37, 37, 37, 37, 37/
      DATA  GRD25 / 0,  33, 0,65535, 19,      0,       0, 128,   90000, &
     &  355000,   5000, 5000, 64, 0, 0, 0, 0, 0,			&
     & 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72,	&
     & 72, 72, 72,  1/							
      DATA  GRD26 / 0,  33, 0,65535, 19, -90000,       0, 128,       0, &
     &  355000,   5000, 5000, 64, 0, 0, 0, 0, 0,			&
     &  1, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72,	&
     & 72, 72, 72, 72/							
      DATA  GRD27 / 0, 255, 5,  65, 65, -20826, -125000,   8,  -80000,  &
     &  381000, 381000, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD28 / 0, 255, 5,  65, 65,  20826,  145000,   8,  100000,  &
     &  381000, 381000,128, 64, 0, 0, 0, 0, 0/
      DATA  GRD29 / 0, 255, 0, 145, 37,      0,       0, 128,   90000,  &
     &  360000,   2500, 2500, 64, 0, 0, 0, 0, 0/
      DATA  GRD30 / 0, 255, 0, 145, 37,  -90000,      0, 128,       0,  &
     &  360000,   2500, 2500, 64, 0, 0, 0, 0, 0/
      DATA  GRD33 / 0, 255, 0, 181, 46,      0,       0, 128,   90000,  &
     &  360000,   2000, 2000, 64, 0, 0, 0, 0, 0/
      DATA  GRD34 / 0, 255, 0, 181, 46, -90000,       0, 128,       0,  &
     &  360000,   2000, 2000, 64, 0, 0, 0, 0, 0/
      DATA  GRD37 / 0,  33, 0,65535,73,      0,  -30000, 128,   90000,  &
     &   60000,  1250,65535, 64, 0, 0, 0, 0, 0,                         &
     & 73, 73, 73, 73, 73, 73, 73, 73, 72, 72, 72, 71, 71, 71, 70,      &  
     & 70, 69, 69, 68, 67, 67, 66, 65, 65, 64, 63, 62, 61, 60, 60,	&
     & 59, 58, 57, 56, 55, 54, 52, 51, 50, 49, 48, 47, 45, 44, 43,	&
     & 42, 40, 39, 38, 36, 35, 33, 32, 30, 29, 28, 26, 25, 23, 22,	&
     & 20, 19, 17, 16, 14, 12, 11,  9,  8,  6,  5,  3,  2/
      DATA  GRD38 / 0,  33, 0,65535,73,      0,   60000, 128,   90000,  &
     &  150000,  1250,65535, 64, 0, 0, 0, 0, 0, 			&
     & 73, 73, 73, 73, 73, 73, 73, 73, 72, 72, 72, 71, 71, 71, 70,	&
     & 70, 69, 69, 68, 67, 67, 66, 65, 65, 64, 63, 62, 61, 60, 60,	&
     & 59, 58, 57, 56, 55, 54, 52, 51, 50, 49, 48, 47, 45, 44, 43,      &
     & 42, 40, 39, 38, 36, 35, 33, 32, 30, 29, 28, 26, 25, 23, 22,      &
     & 20, 19, 17, 16, 14, 12, 11,  9,  8,  6,  5,  3,  2/
      DATA  GRD39 / 0,  33, 0,65535,73,      0,  150000, 128,   90000,  &
     & -120000,  1250,65535, 64, 0, 0, 0, 0, 0,                         &
     & 73, 73, 73, 73, 73, 73, 73, 73, 72, 72, 72, 71, 71, 71, 70, 	&
     & 70, 69, 69, 68, 67, 67, 66, 65, 65, 64, 63, 62, 61, 60, 60,	&
     & 59, 58, 57, 56, 55, 54, 52, 51, 50, 49, 48, 47, 45, 44, 43,	&
     & 42, 40, 39, 38, 36, 35, 33, 32, 30, 29, 28, 26, 25, 23, 22,      &
     & 20, 19, 17, 16, 14, 12, 11,  9,  8,  6,  5,  3,  2/
      DATA  GRD40 / 0,  33, 0,65535,73,       0, -120000, 128,   90000, &
     &  -30000,  1250,65535, 64, 0, 0, 0, 0, 0,                         &
     & 73, 73, 73, 73, 73, 73, 73, 73, 72, 72, 72, 71, 71, 71, 70,	&
     & 70, 69, 69, 68, 67, 67, 66, 65, 65, 64, 63, 62, 61, 60, 60,	&
     & 59, 58, 57, 56, 55, 54, 52, 51, 50, 49, 48, 47, 45, 44, 43,	&
     & 42, 40, 39, 38, 36, 35, 33, 32, 30, 29, 28, 26, 25, 23, 22,      &
     & 20, 19, 17, 16, 14, 12, 11,  9,  8,  6,  5,  3,  2/
      DATA  GRD41 / 0,  33, 0,65535,73, -90000,  -30000, 128,       0,  &
     &   60000,  1250,65535, 64, 0, 0, 0, 0, 0, 			&
     &  2,  3,  5,  6,  8,  9, 11, 12, 14, 16, 17, 19, 20, 22, 23,	&
     & 25, 26, 28, 29, 30, 32, 33, 35, 36, 38, 39, 40, 42, 43, 44,	&
     & 45, 47, 48, 49, 50, 51, 52, 54, 55, 56, 57, 58, 59, 60, 60,      &
     & 61, 62, 63, 64, 65, 65, 66, 67, 67, 68, 69, 69, 70, 70, 71,      &
     & 71, 71, 72, 72, 72, 73, 73, 73, 73, 73, 73, 73, 73/
      DATA  GRD42 / 0,  33, 0,65535,73, -90000,   60000, 128,       0,  &
     &  150000,  1250,65535, 64, 0, 0, 0, 0, 0, 			&
     &  2,  3,  5,  6,  8,  9, 11, 12, 14, 16, 17, 19, 20, 22, 23,	&
     & 25, 26, 28, 29, 30, 32, 33, 35, 36, 38, 39, 40, 42, 43, 44,	&
     & 45, 47, 48, 49, 50, 51, 52, 54, 55, 56, 57, 58, 59, 60, 60,      &
     & 61, 62, 63, 64, 65, 65, 66, 67, 67, 68, 69, 69, 70, 70, 71,      &
     & 71, 71, 72, 72, 72, 73, 73, 73, 73, 73, 73, 73, 73/
      DATA  GRD43 / 0,  33, 0,65535,73, -90000,  150000, 128,       0,  &
     & -120000,  1250,65535, 64, 0, 0, 0, 0, 0, 			&
     &  2,  3,  5,  6,  8,  9, 11, 12, 14, 16, 17, 19, 20, 22, 23,	&
     & 25, 26, 28, 29, 30, 32, 33, 35, 36, 38, 39, 40, 42, 43, 44,	&
     & 45, 47, 48, 49, 50, 51, 52, 54, 55, 56, 57, 58, 59, 60, 60,      &
     & 61, 62, 63, 64, 65, 65, 66, 67, 67, 68, 69, 69, 70, 70, 71,      & 
     & 71, 71, 72, 72, 72, 73, 73, 73, 73, 73, 73, 73, 73/
      DATA  GRD44 / 0,  33, 0,65535,73, -90000, -120000, 128,       0,  &
     &  -30000,  1250,65535, 64, 0, 0, 0, 0, 0, 			&
     &  2,  3,  5,  6,  8,  9, 11, 12, 14, 16, 17, 19, 20, 22, 23,	&
     & 25, 26, 28, 29, 30, 32, 33, 35, 36, 38, 39, 40, 42, 43, 44,	&
     & 45, 47, 48, 49, 50, 51, 52, 54, 55, 56, 57, 58, 59, 60, 60,      &
     & 61, 62, 63, 64, 65, 65, 66, 67, 67, 68, 69, 69, 70, 70, 71,      &
     & 71, 71, 72, 72, 72, 73, 73, 73, 73, 73, 73, 73, 73/
      DATA  GRD45 / 0, 255, 0, 288,145,  90000,       0, 128,  -90000,  &
     &   -1250,   1250, 1250,  0, 0, 0, 0, 0, 0/
      DATA  GRD53 / 0, 255, 1, 117, 51, -61050,       0, 128,   61050,  &
     &       0,  318830, 318830, 22500, 64, 0, 0, 0, 0/
      DATA  GRD55 / 0, 255, 5,  87, 71, -10947, -154289,   8, -105000,  &
     &  254000, 254000, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD56 / 0, 255, 5,  87, 71,   7647, -133443,   8, -105000,  &
     &  127000, 127000, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD61 / 0,  33, 0,65535, 46,      0,       0, 128,   90000, &
     &  180000,   2000, 2000, 64, 0, 0, 0, 0, 0,                        & 
     & 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,	&
     & 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,	&
     & 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,	&
     &  1/
      DATA  GRD62 / 0,  33, 0,65535, 46,      0, -180000, 128,   90000, &
     &       0,   2000, 2000, 64, 0, 0, 0, 0, 0,                        &
     & 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,	&
     & 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,	&
     & 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,	&
     &  1/
      DATA  GRD63 / 0,  33, 0,65535, 46,      0,  -90000, 128,       0, &
     &  180000,   2000, 2000, 64, 0, 0, 0, 0, 0,			&
     &  1, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,	&
     & 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,	&
     & 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,      &
     & 91/
      DATA  GRD64 / 0,  33, 0,65535, 46, -90000, -180000, 128,       0, &
     &       0,   2000, 2000, 64, 0, 0, 0, 0, 0,			&
     &  1, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,	&
     & 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,	&
     & 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,      &
     & 91/
      DATA  GRD85 / 0, 255, 0, 360, 90,    500,     500, 128,   89500,  &
     &  359500,   1000, 1000, 64, 0, 0, 0, 0, 0/
      DATA  GRD86 / 0, 255, 0, 360, 90, -89500,     500, 128,    -500,  &
     &  359500,   1000, 1000, 64, 0, 0, 0, 0, 0/
      DATA  GRD87 / 0, 255, 5,  81, 62,  22876, -120491,   8, -105000,  &
     &   68153,  68153, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD88 / 0, 255, 5, 580,548,  10000, -128000,   8, -105000,  &
     &   15000,  15000, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD90 / 0, 255,203,223,501,  23060,  -92570, 136,   37000,  &
     & -80000,     53,53,64, 0, 0, 0, 0, 0/
      DATA  GRD91 / 0, 255,203,223,501,  23060, -110570, 136,   37000,  &
     & -98000,     53,53,64, 0, 0, 0, 0, 0/
      DATA  GRD92 / 0, 255,203,223,501,  25986, -127871, 136,   40000,  &
     & -115000,    53,53,64, 0, 0, 0, 0, 0/
      DATA  GRD93 / 0, 255,203,223,501,  44232, -169996, 136,   63000,  &
     & -150000,    67,66,64, 0, 0, 0, 0, 0/
      DATA  GRD94 / 0, 255,203,345,569,  -3441, -148799, 136,   50000,  &
     & -111000,    154,141,64, 0, 0, 0, 0, 0/
      DATA  GRD95 / 0, 255,203,146,247,  35222, -131741, 136,   44000,  &
     & -240000,     67, 66,64, 0, 0, 0, 0, 0/
      DATA  GRD96 / 0, 255,203,606,1067, -3441, -148799, 136,   50000,  &
     & -111000,     88,75,64, 0, 0, 0, 0, 0/
      DATA  GRD97 / 0, 255,203, 89,143,  14451,  -71347, 136,   18000,  &
     &  -66500,     53, 53,64, 0, 0, 0, 0, 0/
      DATA  GRD98 / 0, 255, 4, 192, 94,  88542,       0, 128,  -88542,  &
     &    -1875, 47,1875, 0, 0, 0, 0, 0, 0/
      DATA  GRD100/ 0, 255, 5,  83, 83,  17108, -129296,   8, -105000,  &
     &   91452,  91452, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD101/ 0, 255, 5, 113, 91,  10528, -137146,   8, -105000,	&
     &   91452,  91452, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD103/ 0, 255, 5,  65, 56,  22405, -121352,   8, -105000,	&
     &   91452,  91452, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD104/ 0, 255, 5, 147,110,   -268, -139475,   8, -105000,	&
     &   90755,  90755, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD105/ 0, 255, 5,  83, 83,  17529, -129296,   8, -105000,	&
     &   90755,  90755, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD106/ 0, 255, 5, 165,117,  17533, -129296,   8, -105000,	&
     &   45373,  45373, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD107/ 0, 255, 5, 120, 92,  23438, -120168,   8, -105000,	&
     &   45373,  45373, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD110/ 0, 255, 0, 464,224,  25063, -124938, 128,   52938,  &
     & -67063,    125, 125, 64, 0, 0, 0, 0, 0/
      DATA  GRD126/ 0, 255, 4, 384,190,  89277,       0, 128,  -89277,  &
     &    -938,    95, 938, 0, 0, 0, 0, 0, 0/
      DATA  GRD127/ 0, 255, 4, 768,384,  89642,       0, 128,  -89642,  &
     &    -469,   192, 469, 0, 0, 0, 0, 0, 0/
!***TLO 11 Mar 04 ***start
      DATA  GRD138/ 0, 255, 3, 468,288,  21017,-123282,   8,   -97000,  &
     &  12000, 12000, 0, 64, 0, 33000, 45000, 0, 0/
!!    DATA  GRD139/ 0, 255, 3, 64,44,  18144, -161076,  8,   -157500,   &
!!   &  12000, 12000, 0, 64, 0, 19000, 21000, 0, 0/
      DATA  GRD139/ 0, 255, 3, 80,52,  17721, -161973,  8,   -157500,   &
     &  12000, 12000, 0, 64, 0, 19000, 21000, 0, 0/
!!   DATA  GRD140/ 0, 255, 3, 198,162,  53033, -166459,  8,   -148600, &
!!   &  12000, 12000, 0, 64, 0, 57000, 63000, 0, 0/
      DATA  GRD140/ 0, 255, 3, 199,163, 53020, -166477,  8,  -148600,  &
     &  12000, 12000, 0, 64, 0, 57000, 63000, 0, 0/
!CER   DATA  GRD255/ 0, 255,203,420, 769, 13194, -143539, 136,   50000,	&
!CER  & -111000,     88,75,64, 0, 0, 0, 0, 0/
      DATA  GRD255/ 0, 255,203,419, 768, 13194, -143540, 136,   50000,	&
     & -111000,     88,75,64, 0, 0, 0, 0, 0/
!PL*PLee 08 Feb 2005 ***Comment out starts				
!PL   DATA  GRD139/ 0, 255, 3, 469,289,  20953,-123319,   8,   -97000,  &
!PL  &  12000, 12000, 0, 64, 0, 33000, 45000, 0, 0/			
!***TLO 11 Mar 04 ***end
!***TLO 08 Mar 04 ***start						
!PL   DATA  GRD140/ 0, 255, 3, 270,261,  24486,-101108,   8,   -97000,  &
!PL  &  12000, 12000, 0, 64, 0, 33000, 45000, 0, 0/			
!PL   DATA  GRD141/ 0, 255, 3, 271,262,  24431,-101163,   8,   -97000,  &
!PL  &  12000, 12000, 0, 64, 0, 33000, 45000, 0, 0/
!PL   DATA  GRD142/ 0, 255, 3, 268,259,  24595,-100998,   8,   -97000,	&
!PL  &  12000, 12000, 0, 64, 0, 33000, 45000, 0, 0/
!***PLee 08 Feb 2005 ***renaming Grid 142 to 147 starts 		
      DATA  GRD147/ 0, 255, 3, 268,259,  24595,-100998,   8,   -97000,  &
     &  12000, 12000, 0, 64, 0, 33000, 45000, 0, 0/			
!***PLee 08 Feb 2005 ***renaming Grid 142 to 147 ends
!PL   DATA  GRD144/ 0, 255, 3, 168,144,  32233, -90104,   8,   -79500,	&
!PL  &  12000, 12000, 0, 64, 0, 36000, 46000, 0, 0/
!***PLee 08 Feb 2005 ***Comment out ends				
!***TLO 08 Mar 04 ***end
!***TLO 12 Dec 02 ***start						
      DATA  GRD145/ 0, 255, 3, 169,145,  32174, -90159,   8,   -79500,  &
     &  12000, 12000, 0, 64, 0, 36000, 46000, 0, 0/
!***TLO 12 Dec 02 ***end
!***TLO 08 Mar 04 ***start
      DATA  GRD146/ 0, 255, 3, 166,142,  32353, -89994,   8,   -79500,  &
     &  12000, 12000, 0, 64, 0, 36000, 46000, 0, 0/
!***PLee 08 Feb 2005 ***Comment out starts
!PL   DATA  GRD148/ 0, 255, 3, 444,267,  21694,-120707,   8,   -97000, 
!PL  &  12000, 12000, 0, 64, 0, 33000, 45000, 0, 0/
!PL   DATA  GRD149/ 0, 255, 3, 445,268,  21630,-120747,   8,   -97000,
!PL  &  12000, 12000, 0, 64, 0, 33000, 45000, 0, 0/
!PL   DATA  GRD150/ 0, 255, 3, 442,265,  21821,-120628,   8,   -97000,
!PL  &  12000, 12000, 0, 64, 0, 33000, 45000, 0, 0/
!***PLee 08 Feb 2005 ***renaming Grid 150 to 148 starts
      DATA  GRD148/ 0, 255, 3, 442,265,  21821,-120628,   8,   -97000,  &
     &  12000, 12000, 0, 64, 0, 33000, 45000, 0, 0/
!***PLee 08 Feb 2005 ***renaming Grid 150 to 148 ends
!***PLee 08 Feb 2005 ***Comment out ends
!***TLO 08 Mar 04 ***end
      DATA  GRD170/ 0, 255, 4,2880,1440, 89938,      62,  72,  -89938,  &
     &     -62,   125, 125,64, 0, 0, 0, 0, 0/
      DATA  GRD171/ 0, 255, 5, 770,930,  25009, -119560,  72,  -80000,	&
     &   12700,  12700, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD172/ 0, 255, 5, 690,710,  36900, -220194,  72, -260000,	&
     &   12700,  12700, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD175/ 0, 255, 0, 556,334,      0,  130000, 128,   30060,	&
     &  180040,    90,  90, 64, 0, 0, 0, 0, 0/
      DATA  GRD190 / 0, 255,203, 92,141,   182, -149887, 136,   52000,	&
     & -111000,    577,538,64, 0, 0, 0, 0, 0/
      DATA  GRD192 / 0, 255,203,237,387, -3441, -148799, 136,   50000,	&
     & -111000,    225,207,64, 0, 0, 0, 0, 0/
      DATA  GRD194 / 0, 255,203, 89,143, 16444, -162244, 136,   20250,	&
     & -157350,     53, 53,64, 0, 0, 0, 0, 0/
      DATA  GRD196/ 0, 255,201,45903,1,  23476,  -96745, 136,     151,  &
     &     305,     67, 66, 64, 0, 0, 0, 0, 0/
      DATA  GRD198/ 0, 255,203,160,261,  -3441, -148799, 136,   50000,	&
     & -111000,    333,308,64, 0, 0, 0, 0, 0/
      DATA  GRD201/ 0, 255, 5,  65, 65, -20826, -150000,   8, -105000,	&
     &  381000, 381000, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD202/ 0, 255, 5,  65, 43,   7838, -141028,   8, -105000,	&
     &  190500, 190500, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD203/ 0, 255, 5,  45, 39,  19132, -185837,   8, -150000,	&
     &  190500, 190500, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD204/ 0, 255, 1,  93, 68, -25000,  110000, 128,   60644,	&
     & -109129, 160000, 160000, 20000, 64, 0, 0, 0, 0/
      DATA  GRD205/ 0, 255, 5,  45, 39,    616,  -84904,   8,  -60000,	&
     &  190500, 190500, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD206/ 0, 255, 3,  51, 41,  22289, -117991,   8, - 95000,  &
     &   81271,  81271, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD207/ 0, 255, 5,  49, 35,  42085, -175641,   8, -150000,	&
     &   95250,  95250, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD208/ 0, 255, 1,  29, 27,   9343, -167315, 128,   28092,	&
     & -145878, 80000, 80000, 20000, 64, 0, 0, 0, 0/
      DATA  GRD209/ 0, 255, 3, 275,223,  -4850, -151100,   8, -111000,	&
     &   44000,  44000, 0, 64, 0, 45000, 45000, 0, 0/
      DATA  GRD210/ 0, 255, 1,  25, 25,   9000,  -77000, 128,   26422,	&
     &  -58625, 80000, 80000, 20000, 64, 0, 0, 0, 0/
      DATA  GRD211/ 0, 255, 3,  93, 65,  12190, -133459,   8,  -95000,	&
     &   81271,  81271, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD212/ 0, 255, 3, 185,129,  12190, -133459,   8,  -95000,	&
     &   40635,  40635, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD213/ 0, 255, 5, 129, 85,   7838, -141028,   8, -105000,  &
     &   95250,  95250, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD214/ 0, 255, 5,  97, 69,  42085, -175641,   8, -150000,	&
     &   47625,  47625, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD215/ 0, 255, 3, 369,257,  12190, -133459,   8,  -95000,	&
     &   20318,  20318, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD216/ 0, 255, 5, 139,107,  30000, -173000,   8, -135000,	&
     &   45000,  45000, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD217/ 0, 255, 5, 277,213,  30000, -173000,   8, -135000,	&
     &   22500,  22500, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD218/ 0, 255, 3, 614,428,  12190, -133459,   8,  -95000,	&
     &   12191,  12191, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD219/ 0, 255, 5, 385,465,  25008, -119559,  72,  -80000,	&
     &   25400,  25400, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD220/ 0, 255, 5, 345,355, -36889, -220194,  72, -260000,  &
     &   25400,  25400, 1, 64, 0, 0, 0, 0, 0/
      DATA  GRD221/ 0, 255, 3, 349,277,   1000, -145500,   8, -107000,	&
     &   32463,  32463, 0, 64, 0, 50000, 50000, 0, 0/
      DATA  GRD222/ 0, 255, 3, 138,112,  -4850, -151100,   8, -111000,	&
     &   88000,  88000, 0, 64, 0, 45000, 45000, 0, 0/
      DATA  GRD223/ 0, 255, 5, 129,129, -20826, -150000,   8, -105000,	&
     &  190500, 190500, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD224/ 0, 255, 5,  65, 65,  20826,  120000,   8, -105000,	&
     &  381000, 381000, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD225/ 0, 255, 1, 185,135, -25000, -250000, 128,   60640,	&
     & -109129, 80000, 80000, 20000, 64, 0, 0, 0, 0/
      DATA  GRD226/ 0, 255, 3, 737,513,  12190, -133459,   8,  -95000,	&
     &   10159,  10159, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD227/ 0, 255, 3,1473,1025,  12190, -133459,   8, -95000,  &
     &    5079,   5079, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD228/ 0, 255, 0, 144, 73,  90000,       0, 128,  -90000,	&
     &   -2500,   2500, 2500, 64, 0, 0, 0, 0, 0/
      DATA  GRD229/ 0, 255, 0, 360,181,  90000,       0, 128,  -90000,	&
     &   -1000,   1000, 1000, 64, 0, 0, 0, 0, 0/
      DATA  GRD230/ 0, 255, 0, 720,361,  90000,       0, 128,  -90000,	&
     &    -500,    500,  500, 64, 0, 0, 0, 0, 0/
      DATA  GRD231/ 0, 255, 0, 720,181,      0,       0, 128,   90000,	&
     &    -500,    500,  500, 64, 0, 0, 0, 0, 0/
      DATA  GRD232/ 0, 255, 0, 360, 91,      0,       0, 128,   90000,	&
     &   -1000,   1000, 1000, 64, 0, 0, 0, 0, 0/
      DATA  GRD233/ 0, 255, 0, 288,157,  78000,       0, 128,  -78000,	&
     &   -1250,   1250, 1000, 64, 0, 0, 0, 0, 0/
      DATA  GRD234/ 0, 255, 0, 133,121,  15000,  -98000, 128,  -45000,  &
     &  -65000,    250,  250, 64, 0, 0, 0, 0, 0/
      DATA  GRD235/ 0, 255, 0, 720,360,  89750,     250,  72,  -89750,	&
     &    -250,    250, 1000, 64, 0, 0, 0, 0, 0/
      DATA  GRD236/ 0, 255, 3, 151,113,  16281,  233862,   8,  -95000,	&
     &   40635,  40635, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD237/ 0, 255, 3,  54, 47,  16201,  285720,   8, -107000,	&
     &   32463,  32463, 0, 64, 0, 50000, 50000, 0, 0/
      DATA  GRD238/ 0, 255, 0, 275, 203,  50750, 261750,  72,    -205,  & 
     &   -29750, 0,  0, 64, 0, 0, 0, 0, 0/
      DATA  GRD239/ 0, 255, 0, 155, 123, 75750,  159500,  72,   44750,  &
     &  -123500,  0, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD240/ 0, 255, 5, 1121, 881, 23098, -119036,  8, -105000,	&
     &   47625,  47625, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD241/ 0, 255, 3, 549,445,  -4850, -151100,   8, -111000,  &
     &   22000,  22000, 0, 64, 0, 45000, 45000, 0, 0/
      DATA  GRD242/ 0, 255, 5, 553,425,  30000, -173000,   8, -135000,	&
     &   11250,  11250, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD243/ 0, 255, 0, 126,101,  10000, -170000, 128,   50000,	&
     &  -120000, 400, 400, 64, 0, 0, 0, 0, 0/
      DATA  GRD244/ 0, 255, 0, 275, 203,  50750, 261750,  72,    -205,  & 
     &   -29750, 0,  0, 64, 0, 0, 0, 0, 0/
      DATA  GRD245/ 0, 255, 3, 336,372,  22980, -92840,   8,   -80000,	&
     &   8000,  8000, 0, 64, 0, 35000, 35000, 0, 0/
      DATA  GRD246/ 0, 255, 3, 332,371,  25970, -127973,  8,  -115000,	&
     &   8000,  8000, 0, 64, 0, 40000, 40000, 0, 0/
      DATA  GRD247/ 0, 255, 3, 336,372,  22980, -110840,   8,  -98000,	&
     &   8000,  8000, 0, 64, 0, 35000, 35000, 0, 0/
      DATA  GRD248/ 0, 255, 0, 135,101,  14500,  -71500, 128,   22000,  &
     &  -61450,    75,  75, 64, 0, 0, 0, 0, 0/
      DATA  GRD249/ 0, 255, 5, 367,343,  45400, -171600,   8, -150000,	&
     &   9868,  9868, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD250/ 0, 255, 0, 135,101,  16500, -162000, 128,   24000,	&
     & -151950,    75,  75, 64, 0, 0, 0, 0, 0/
      DATA  GRD251/ 0, 255, 0, 332,210,  26350,  -83050, 128,   47250,	&
     &  -49950,    100,  100, 64, 0, 0, 0, 0, 0/
      DATA  GRD252/ 0, 255, 3, 301,225,  16281, -126138,  8,   -95000,	&
     &   20317,  20317, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD253/ 0, 255, 0, 373,224,   6050, -170250,  72,    4750,	&
     &  -77250,    75,  75, 64, 0, 0, 0, 0, 0/
									
      IERR = 0

        DO 1 I = 1,18
          IGDS(I) = 0
 1      CONTINUE

      IF (IGRID.GE.37.AND.IGRID.LE.44) THEN
        DO 2 I = 19,91
          IGDS(I) = 0
 2      CONTINUE
      END IF

      IF (IGRID.GE.21.AND.IGRID.LE.24) THEN
        DO I = 19,55
          IGDS(I) = 0
        END DO
      END IF

      IF (IGRID.GE.25.AND.IGRID.LE.26) THEN
        DO I = 19,37
          IGDS(I) = 0
        END DO
      END IF

      IF (IGRID.GE.61.AND.IGRID.LE.64) THEN
        DO I = 19,64
          IGDS(I) = 0
        END DO
      END IF

      IF (IGRID.EQ.1) THEN
        DO 3 I = 1,18
          IGDS(I) = GRD1(I)
  3     CONTINUE

      ELSE IF (IGRID.EQ.2) THEN
        DO 4 I = 1,18
          IGDS(I) = GRD2(I)
  4     CONTINUE

      ELSE IF (IGRID.EQ.3) THEN
        DO 5 I = 1,18
          IGDS(I) = GRD3(I)
  5     CONTINUE

      ELSE IF (IGRID.EQ.4) THEN
        DO 6 I = 1,18
          IGDS(I) = GRD4(I)
  6     CONTINUE

      ELSE IF (IGRID.EQ.5) THEN
        DO 10 I = 1,18
          IGDS(I) = GRD5(I)
 10     CONTINUE

      ELSE IF (IGRID.EQ.6) THEN
        DO 20 I = 1,18
          IGDS(I) = GRD6(I)
 20     CONTINUE

      ELSE IF (IGRID.EQ.8) THEN
        DO I = 1,18
          IGDS(I) = GRD8(I)
        END DO

      ELSE IF (IGRID.EQ.21) THEN
        DO 30 I = 1,55
          IGDS(I) = GRD21(I)
 30     CONTINUE

      ELSE IF (IGRID.EQ.22) THEN
        DO 40 I = 1,55
          IGDS(I) = GRD22(I)
 40     CONTINUE

      ELSE IF (IGRID.EQ.23) THEN
        DO 50 I = 1,55
          IGDS(I) = GRD23(I)
 50     CONTINUE

      ELSE IF (IGRID.EQ.24) THEN
        DO 60 I = 1,55
          IGDS(I) = GRD24(I)
 60     CONTINUE

      ELSE IF (IGRID.EQ.25) THEN
        DO 70 I = 1,37
          IGDS(I) = GRD25(I)
 70     CONTINUE

      ELSE IF (IGRID.EQ.26) THEN
        DO 80 I = 1,37
          IGDS(I) = GRD26(I)
 80     CONTINUE

      ELSE IF (IGRID.EQ.27) THEN
        DO 90 I = 1,18
          IGDS(I) = GRD27(I)
 90     CONTINUE

      ELSE IF (IGRID.EQ.28) THEN
        DO 100 I = 1,18
          IGDS(I) = GRD28(I)
 100    CONTINUE

      ELSE IF (IGRID.EQ.29) THEN
        DO 110 I = 1,18
          IGDS(I) = GRD29(I)
 110    CONTINUE

      ELSE IF (IGRID.EQ.30) THEN
        DO 120 I = 1,18
          IGDS(I) = GRD30(I)
 120    CONTINUE

      ELSE IF (IGRID.EQ.33) THEN
        DO 130 I = 1,18
          IGDS(I) = GRD33(I)
 130     CONTINUE

      ELSE IF (IGRID.EQ.34) THEN
        DO 140 I = 1,18
          IGDS(I) = GRD34(I)
 140    CONTINUE

      ELSE IF (IGRID.EQ.37) THEN
        DO 141 I = 1,91
          IGDS(I) = GRD37(I)
 141    CONTINUE

      ELSE IF (IGRID.EQ.38) THEN
        DO 142 I = 1,91
          IGDS(I) = GRD38(I)
 142    CONTINUE

      ELSE IF (IGRID.EQ.39) THEN
        DO 143 I = 1,91
          IGDS(I) = GRD39(I)
 143    CONTINUE

      ELSE IF (IGRID.EQ.40) THEN
        DO 144 I = 1,91
          IGDS(I) = GRD40(I)
 144    CONTINUE

      ELSE IF (IGRID.EQ.41) THEN
        DO 145 I = 1,91
          IGDS(I) = GRD41(I)
 145    CONTINUE

      ELSE IF (IGRID.EQ.42) THEN
        DO 146 I = 1,91
          IGDS(I) = GRD42(I)
 146    CONTINUE

      ELSE IF (IGRID.EQ.43) THEN
        DO 147 I = 1,91
          IGDS(I) = GRD43(I)
 147    CONTINUE

      ELSE IF (IGRID.EQ.44) THEN
        DO 148 I = 1,91
          IGDS(I) = GRD44(I)
 148    CONTINUE

      ELSE IF (IGRID.EQ.45) THEN
        DO 149 I = 1,18
          IGDS(I) = GRD45(I)
 149    CONTINUE

      ELSE IF (IGRID.EQ.53) THEN
        DO I = 1,18
          IGDS(I) = GRD53(I)
        END DO

      ELSE IF (IGRID.EQ.55) THEN
        DO 152 I = 1,18
          IGDS(I) = GRD55(I)
 152    CONTINUE

      ELSE IF (IGRID.EQ.56) THEN
        DO 154 I = 1,18
          IGDS(I) = GRD56(I)
 154    CONTINUE

      ELSE IF (IGRID.EQ.61) THEN
        DO 160 I = 1,64
          IGDS(I) = GRD61(I)
 160    CONTINUE

      ELSE IF (IGRID.EQ.62) THEN
        DO 170 I = 1,64
          IGDS(I) = GRD62(I)
 170    CONTINUE

      ELSE IF (IGRID.EQ.63) THEN
        DO 180 I = 1,64
          IGDS(I) = GRD63(I)
 180    CONTINUE

      ELSE IF (IGRID.EQ.64) THEN
        DO 190 I = 1,64
          IGDS(I) = GRD64(I)
 190    CONTINUE

      ELSE IF (IGRID.EQ.85) THEN
        DO 192 I = 1,18
          IGDS(I) = GRD85(I)
 192    CONTINUE

      ELSE IF (IGRID.EQ.86) THEN
        DO 194 I = 1,18
          IGDS(I) = GRD86(I)
 194    CONTINUE

      ELSE IF (IGRID.EQ.87) THEN
        DO 195 I = 1,18
          IGDS(I) = GRD87(I)
 195    CONTINUE

      ELSE IF (IGRID.EQ.88) THEN
        DO 2195 I = 1,18
          IGDS(I) = GRD88(I)
2195    CONTINUE

      ELSE IF (IGRID.EQ.90) THEN
        DO 196 I = 1,18
          IGDS(I) = GRD90(I)
 196    CONTINUE

      ELSE IF (IGRID.EQ.91) THEN
        DO 197 I = 1,18
          IGDS(I) = GRD91(I)
 197    CONTINUE

      ELSE IF (IGRID.EQ.92) THEN
        DO 198 I = 1,18
          IGDS(I) = GRD92(I)
 198    CONTINUE

      ELSE IF (IGRID.EQ.93) THEN
        DO 199 I = 1,18
          IGDS(I) = GRD93(I)
 199    CONTINUE

      ELSE IF (IGRID.EQ.94) THEN
        DO 200 I = 1,18
          IGDS(I) = GRD94(I)
 200    CONTINUE

      ELSE IF (IGRID.EQ.95) THEN
        DO 201 I = 1,18
          IGDS(I) = GRD95(I)
 201    CONTINUE

      ELSE IF (IGRID.EQ.96) THEN
        DO 202 I = 1,18
          IGDS(I) = GRD96(I)
 202    CONTINUE

      ELSE IF (IGRID.EQ.97) THEN
        DO 203 I = 1,18
          IGDS(I) = GRD97(I)
 203    CONTINUE

      ELSE IF (IGRID.EQ.98) THEN
        DO 204 I = 1,18
          IGDS(I) = GRD98(I)
 204    CONTINUE

      ELSE IF (IGRID.EQ.100) THEN
        DO 205 I = 1,18
          IGDS(I) = GRD100(I)
 205    CONTINUE

      ELSE IF (IGRID.EQ.101) THEN
        DO 210 I = 1,18
          IGDS(I) = GRD101(I)
 210    CONTINUE

      ELSE IF (IGRID.EQ.103) THEN
        DO 220 I = 1,18
          IGDS(I) = GRD103(I)
 220   CONTINUE

      ELSE IF (IGRID.EQ.104) THEN
        DO 230 I = 1,18
          IGDS(I) = GRD104(I)
 230    CONTINUE

      ELSE IF (IGRID.EQ.105) THEN
        DO 240 I = 1,18
          IGDS(I) = GRD105(I)
 240    CONTINUE

      ELSE IF (IGRID.EQ.106) THEN
        DO 242 I = 1,18
          IGDS(I) = GRD106(I)
 242    CONTINUE

      ELSE IF (IGRID.EQ.107) THEN
        DO 244 I = 1,18
          IGDS(I) = GRD107(I)
 244    CONTINUE

      ELSE IF (IGRID.EQ.110) THEN
        DO I = 1,18
          IGDS(I) = GRD110(I)
        ENDDO

      ELSE IF (IGRID.EQ.126) THEN
        DO 245 I = 1,18
          IGDS(I) = GRD126(I)
 245    CONTINUE

      ELSE IF (IGRID.EQ.127) THEN
        DO I = 1,18
          IGDS(I) = GRD127(I)
        ENDDO
!***TLO 11 Mar 04 and PLEE July 8, 2009 ***start
      ELSE IF (IGRID.EQ.138) THEN
        DO 1008 I = 1,18
          IGDS(I) = GRD138(I)
1008    CONTINUE
      ELSE IF (IGRID.EQ.139) THEN
        DO 1039 I = 1,18
          IGDS(I) = GRD139(I)
1039    CONTINUE
      ELSE IF (IGRID.EQ.140) THEN
        DO 1040 I = 1,18
          IGDS(I) = GRD140(I)
1040    CONTINUE
      ELSE IF (IGRID.EQ.255) THEN
        DO 1009 I = 1,18
          IGDS(I) = GRD255(I)
1009    CONTINUE
!PL*PLee 08 Feb 2005 ***Comment out starts
!***TLO 11 Mar 04 ***end
!***TLO 08 Mar 04 ***start
!PL   ELSE IF (IGRID.EQ.140) THEN
!PL     DO 1108 I = 1,18
!PL       IGDS(I) = GRD140(I)
!PL 1108    CONTINUE
!PL   ELSE IF (IGRID.EQ.141) THEN
!PL     DO 1109 I = 1,18
!PL       IGDS(I) = GRD141(I)
!PL 1109    CONTINUE
!PL   ELSE IF (IGRID.EQ.142) THEN
!PL     DO 1110 I = 1,18
!PL       IGDS(I) = GRD142(I)
!PL 1110    CONTINUE
!***PLee 08 Feb 2005 ***renaming Grid 142 to 147 starts
      ELSE IF (IGRID.EQ.147) THEN
        DO 1110 I = 1,18
          IGDS(I) = GRD147(I)
1110    CONTINUE
!***PLee 08 Feb 2005 ***renaming Grid 142 to 147 ends
!PL   ELSE IF (IGRID.EQ.144) THEN
!PL     DO 1208 I = 1,18
!PL       IGDS(I) = GRD144(I)
!PL 1208    CONTINUE
!***TLO 08 Mar 04 ***end
!***TLO 16 Dec 02 ***start
      ELSE IF (IGRID.EQ.145) THEN
        DO 1209 I = 1,18
          IGDS(I) = GRD145(I)
1209    CONTINUE
!***TLO 16 Dec 02 ***end
!***TLO 08 Mar 04 ***start
      ELSE IF (IGRID.EQ.146) THEN
        DO 1210 I = 1,18
          IGDS(I) = GRD146(I)
1210    CONTINUE
!PL   ELSE IF (IGRID.EQ.148) THEN
!PL     DO 1308 I = 1,18
!PL       IGDS(I) = GRD148(I)
!PL 1308    CONTINUE
!PL   ELSE IF (IGRID.EQ.149) THEN
!PL     DO 1309 I = 1,18
!PL       IGDS(I) = GRD149(I)
!PL 1309    CONTINUE
!PL   ELSE IF (IGRID.EQ.150) THEN
!PL     DO 1310 I = 1,18
!PL       IGDS(I) = GRD150(I)
!PL 1310    CONTINUE
!PL*PLee 08 Feb 2005 ***renaming Grid 150 to 148 starts
      ELSE IF (IGRID.EQ.148) THEN
        DO 1310 I = 1,18
          IGDS(I) = GRD148(I)
1310    CONTINUE
!PL*PLee 08 Feb 2005 ***renaming Grid 150 to 148 ends
!PL*PLee 08 Feb 2005 ***Comment out ends
!***TLO 08 Mar 04 ***end

      ELSE IF (IGRID.EQ.170) THEN
        DO I = 1,18
          IGDS(I) = GRD170(I)
        ENDDO

      ELSE IF (IGRID.EQ.171) THEN
        DO I = 1,18
          IGDS(I) = GRD171(I)
        ENDDO

      ELSE IF (IGRID.EQ.172) THEN
        DO I = 1,18
          IGDS(I) = GRD172(I)
        ENDDO

      ELSE IF (IGRID.EQ.175) THEN
        DO I = 1,18
          IGDS(I) = GRD175(I)
        ENDDO

      ELSE IF (IGRID.EQ.190) THEN
        DO 2190 I = 1,18
          IGDS(I) = GRD190(I)
 2190   CONTINUE

      ELSE IF (IGRID.EQ.192) THEN
        DO 2191 I = 1,18
          IGDS(I) = GRD192(I)
 2191   CONTINUE

      ELSE IF (IGRID.EQ.194) THEN
        DO 2192 I = 1,18
          IGDS(I) = GRD194(I)
 2192   CONTINUE

      ELSE IF (IGRID.EQ.196) THEN
        DO 249 I = 1,18
          IGDS(I) = GRD196(I)
 249    CONTINUE

      ELSE IF (IGRID.EQ.198) THEN
        DO 2490 I = 1,18
          IGDS(I) = GRD198(I)
 2490   CONTINUE

      ELSE IF (IGRID.EQ.201) THEN
        DO 250 I = 1,18
          IGDS(I) = GRD201(I)
 250    CONTINUE

      ELSE IF (IGRID.EQ.202) THEN
        DO 260 I = 1,18
          IGDS(I) = GRD202(I)
 260    CONTINUE

      ELSE IF (IGRID.EQ.203) THEN
        DO 270 I = 1,18
          IGDS(I) = GRD203(I)
 270    CONTINUE

      ELSE IF (IGRID.EQ.204) THEN
        DO 280 I = 1,18
          IGDS(I) = GRD204(I)
 280    CONTINUE

      ELSE IF (IGRID.EQ.205) THEN
        DO 290 I = 1,18
          IGDS(I) = GRD205(I)
 290    CONTINUE

      ELSE IF (IGRID.EQ.206) THEN
        DO 300 I = 1,18
          IGDS(I) = GRD206(I)
 300    CONTINUE

      ELSE IF (IGRID.EQ.207) THEN
        DO 310 I = 1,18
          IGDS(I) = GRD207(I)
 310    CONTINUE

      ELSE IF (IGRID.EQ.208) THEN
        DO 320 I = 1,18
          IGDS(I) = GRD208(I)
 320    CONTINUE

      ELSE IF (IGRID.EQ.209) THEN
        DO 330 I = 1,18
          IGDS(I) = GRD209(I)
 330    CONTINUE

      ELSE IF (IGRID.EQ.210) THEN
        DO 340 I = 1,18
          IGDS(I) = GRD210(I)
 340    CONTINUE

      ELSE IF (IGRID.EQ.211) THEN
        DO 350 I = 1,18
          IGDS(I) = GRD211(I)
 350    CONTINUE

      ELSE IF (IGRID.EQ.212) THEN
        DO 360 I = 1,18
          IGDS(I) = GRD212(I)
 360    CONTINUE

      ELSE IF (IGRID.EQ.213) THEN
        DO 370 I = 1,18
          IGDS(I) = GRD213(I)
 370    CONTINUE

      ELSE IF (IGRID.EQ.214) THEN
        DO 380 I = 1,18
          IGDS(I) = GRD214(I)
 380    CONTINUE

      ELSE IF (IGRID.EQ.215) THEN
        DO 390 I = 1,18
          IGDS(I) = GRD215(I)
 390    CONTINUE

      ELSE IF (IGRID.EQ.216) THEN
        DO 400 I = 1,18
          IGDS(I) = GRD216(I)
 400    CONTINUE

      ELSE IF (IGRID.EQ.217) THEN
        DO 401 I = 1,18
          IGDS(I) = GRD217(I)
 401    CONTINUE

      ELSE IF (IGRID.EQ.218) THEN
        DO 410 I = 1,18
          IGDS(I) = GRD218(I)
 410    CONTINUE

      ELSE IF (IGRID.EQ.219) THEN
        DO 411 I = 1,18
          IGDS(I) = GRD219(I)
 411    CONTINUE

      ELSE IF (IGRID.EQ.220) THEN
        DO 412 I = 1,18
          IGDS(I) = GRD220(I)
 412    CONTINUE

      ELSE IF (IGRID.EQ.221) THEN
        DO 413 I = 1,18
          IGDS(I) = GRD221(I)
 413    CONTINUE

      ELSE IF (IGRID.EQ.222) THEN
        DO 414 I = 1,18
          IGDS(I) = GRD222(I)
 414    CONTINUE

      ELSE IF (IGRID.EQ.223) THEN
        DO 415 I = 1,18
          IGDS(I) = GRD223(I)
 415    CONTINUE

      ELSE IF (IGRID.EQ.224) THEN
        DO 416 I = 1,18
          IGDS(I) = GRD224(I)
 416    CONTINUE

      ELSE IF (IGRID.EQ.225) THEN
        DO 417 I = 1,18
          IGDS(I) = GRD225(I)
 417    CONTINUE

      ELSE IF (IGRID.EQ.226) THEN
        DO 418 I = 1,18
          IGDS(I) = GRD226(I)
 418    CONTINUE

      ELSE IF (IGRID.EQ.227) THEN
        DO 419 I = 1,18
          IGDS(I) = GRD227(I)
 419    CONTINUE

      ELSE IF (IGRID.EQ.228) THEN
        DO 420 I = 1,18
          IGDS(I) = GRD228(I)
 420    CONTINUE

      ELSE IF (IGRID.EQ.229) THEN
        DO 421 I = 1,18
          IGDS(I) = GRD229(I)
 421    CONTINUE

      ELSE IF (IGRID.EQ.230) THEN
        DO 422 I = 1,18
          IGDS(I) = GRD230(I)
 422    CONTINUE

      ELSE IF (IGRID.EQ.231) THEN
        DO 423 I = 1,18
          IGDS(I) = GRD231(I)
 423    CONTINUE

      ELSE IF (IGRID.EQ.232) THEN
        DO 424 I = 1,18
          IGDS(I) = GRD232(I)
 424    CONTINUE

      ELSE IF (IGRID.EQ.233) THEN
        DO 425 I = 1,18
          IGDS(I) = GRD233(I)
 425    CONTINUE

      ELSE IF (IGRID.EQ.234) THEN
        DO 426 I = 1,18
          IGDS(I) = GRD234(I)
 426    CONTINUE

      ELSE IF (IGRID.EQ.235) THEN
        DO 427 I = 1,18
          IGDS(I) = GRD235(I)
 427    CONTINUE

      ELSE IF (IGRID.EQ.236) THEN
        DO 428 I = 1,18
          IGDS(I) = GRD236(I)
 428    CONTINUE

      ELSE IF (IGRID.EQ.237) THEN
        DO 429 I = 1,18
          IGDS(I) = GRD237(I)
 429    CONTINUE

      ELSE IF (IGRID.EQ.238) THEN
        DO I = 1,18
          IGDS(I) = GRD238(I)
        END DO

      ELSE IF (IGRID.EQ.239) THEN
        DO I = 1,18
          IGDS(I) = GRD239(I)
        END DO

      ELSE IF (IGRID.EQ.240) THEN
        DO I = 1,18
          IGDS(I) = GRD240(I)
        END DO

      ELSE IF (IGRID.EQ.241) THEN
        DO 430 I = 1,18
          IGDS(I) = GRD241(I)
 430    CONTINUE

      ELSE IF (IGRID.EQ.242) THEN
        DO 431 I = 1,18
          IGDS(I) = GRD242(I)
 431    CONTINUE

      ELSE IF (IGRID.EQ.243) THEN
        DO 432 I = 1,18
          IGDS(I) = GRD243(I)
 432    CONTINUE

      ELSE IF (IGRID.EQ.244) THEN
        DO I = 1,18
          IGDS(I) = GRD244(I)
        END DO

      ELSE IF (IGRID.EQ.245) THEN
        DO 433 I = 1,18
          IGDS(I) = GRD245(I)
 433    CONTINUE

      ELSE IF (IGRID.EQ.246) THEN
        DO 434 I = 1,18
          IGDS(I) = GRD246(I)
 434    CONTINUE

      ELSE IF (IGRID.EQ.247) THEN
        DO 435 I = 1,18
          IGDS(I) = GRD247(I)
 435    CONTINUE

      ELSE IF (IGRID.EQ.248) THEN
        DO 436 I = 1,18
          IGDS(I) = GRD248(I)
 436    CONTINUE

      ELSE IF (IGRID.EQ.249) THEN
        DO 437 I = 1,18
          IGDS(I) = GRD249(I)
 437    CONTINUE

      ELSE IF (IGRID.EQ.250) THEN
        DO 438 I = 1,18
          IGDS(I) = GRD250(I)
 438    CONTINUE

      ELSE IF (IGRID.EQ.251) THEN
        DO 439 I = 1,18
          IGDS(I) = GRD251(I)
 439    CONTINUE

      ELSE IF (IGRID.EQ.252) THEN
        DO 440 I = 1,18
          IGDS(I) = GRD252(I)
 440    CONTINUE
      ELSE IF (IGRID.EQ.253) THEN
        DO 441 I = 1,18
          IGDS(I) = GRD253(I)
 441    CONTINUE

      ELSE
        IERR = 1
      ENDIF

      RETURN
      END
