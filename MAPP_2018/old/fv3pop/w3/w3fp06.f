       SUBROUTINE W3FP06(ID,KTITLE,N)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C $
C SUBPROGRAM: W3FP06         NMC TITLE SUBROUTINE
C   AUTHOR: JONES,R.E.       ORG: W342       DATE: 86-12-03
C $
C ABSTRACT: PROVIDES A TITLE FOR DATA FIELDS FORMULATED ACCORDING TO
C   NMC O.N. 84. THE EXTRACTED INFORMATION IS CONVERTED INTO UP TO
C   81 WORDS AND STORED AT A USER PROVIDED LOCATION.
C $
C PROGRAM HISTORY LOG:
C   88-11-28  R.E.JONES
C   90-02-12  R.E.JONES   CONVERT TO CRAY CFT77 FORTRAN
C   91-04-26  R.E.JONES   ADD Q TYPE 23, 136, 137, 71, 159, 75, 118,
C                         119, 24 TO TABLES, CHANGES FOR BIG RECORDS.
C   93-02-23  R.E.JONES   ADD Q TYPE 157 & 158 (CORE & TKE) TO TABLES
C $
C USAGE: CALL W3FP06 (ARG1, ARG2, N)
C $
C   INPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     ARG1   ARG LIST  12 WORD FIELD LABEL DESCRIBING THE DATA (6
C                      INTEGER WORDS) OFFICE NOTE 84
C     N      ARG LIST  INTEGER NUMBER OF LINES OF OUTPUT DESIRED
C                      = 1  FIRST 88 CHAR. THE ABBREVIATED TITLE
C                           (LINE 1 STARTS AT ARG2(1))
C                      = 2  FIRST 216 CHAR. DECIMAL VALUES OF THE
C                           PARAMETERS
C                      = 3  ALL 324 CHAR., HEXIDECIMAL DUMP OF THE 12
C                           WORD FIELD LABEL (LINE 3 CHAR. 221
C $
C   OUTPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     ARG2   ARG LIST  CHARACTER*324 SIZE ARRAY TO CONTAIN
C                      THE TITLE IN ACSII
C $
C   SUBPROGRAMS CALLED:
C     NAMES                                                   LIBRARY
C     ------------------------------------------------------- --------
C     LINE01 LINE02 LINE03 VALUE1                             UNIQUE
C     INTERNAL (WRITE) AND SHIFT                              SYSTEM
C $
C   REMARKS: SEE NMC O.N. 84 FOR DATA FIELD ABBREVIATIONS
C $
C ATTRIBUTES:
C   LANGUAGE: IBM XL FORTRAN
C   MACHINE:  IBM SP
C $
C$$$
C $
      INTEGER(8)         ID(6)
      INTEGER(4)      MASK(8)
C $
      CHARACTER * 324  KTITLE
C $
      DATA  MASK(1)/X'0000000F'/
      DATA  MASK(2)/X'000000FF'/
      DATA  MASK(3)/X'00000FFF'/
      DATA  MASK(4)/X'0000FFFF'/
      DATA  MASK(5)/X'000FFFFF'/
      DATA  MASK(6)/X'00FFFFFF'/
      DATA  MASK(7)/X'0FFFFFFF'/
      DATA  MASK(8)/X'FFFFFFFF'/
C $
      CALL LINE01(ID,MASK,KTITLE)
        IF (N.GT.1) GO TO 10
      RETURN
C $
   10 CONTINUE
        CALL LINE02(ID,MASK,KTITLE)
        IF (N.GT.2) GO TO 20
        RETURN
C $
   20 CONTINUE
        CALL LINE03(ID,KTITLE)
        RETURN
      END
       SUBROUTINE LINE01(ID,MASK,KTITLE)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C $
C SUBPROGRAM: LINE01         CREATES THE FIRST LINE OF TITLE
C   AUTHOR: JONES,R.E.       ORG: W342       DATE: 86-12-03
C $
C ABSTRACT: CREATES THE FIST LINE OF THE TITLE FROM THE ID WORDS.
C   CALL BY W3FP06 TO MAKE 1ST LINE OF TITLE. WORDS 1 TO 22.
C $
C PROGRAM HISTORY LOG:
C   88-09-02  R.E.JONES
C   93-02-23  R.E.JONES   ADD Q TYPE 157 & 158 (CORE & TKE) TO TABLES
C $
C USAGE:  CALL LINE01(ID,MASK,KTITLE)
C $
C   INPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     ID     ARG LIST  ID WORDS (6 INTEGER WORDS) OFFICE NOTE 84
C     MASK   ARG LIST  MASK FOR UNPACKING ID WORDS (8 INTEGER WORDS)
C $
C   OUTPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     KTITLE ARG LIST  CHARACTER *324 ARRAY
C            TAPE6     ERROR MESSAGES
C $
C   SUBPROGRAMS CALLED:
C     NAMES                                                   LIBRARY
C     ------------------------------------------------------- --------
C     AND SHIFT                                               SYSLIB
C     VALUE1 CLIMO SETCL                                      UNIQUE
C $
C ATTRIBUTES:
C   LANGUAGE: IBM XL FORTRAN
C   MACHINE:  IBM SP
C $
C$$$
C $
C     CREATES THE FIRST 22 WORDS OF TITLER
C $
      INTEGER(8)          ID(6)
      INTEGER(4)       MASK(8)
      INTEGER(4)       SHFMSK(17)
C $
      CHARACTER * 4    UNIT
      CHARACTER * 4    UNIT1
      CHARACTER * 4    DAYS
      CHARACTER * 5    FOR
      CHARACTER * 5    FOR1
      CHARACTER * 1    DASH
      CHARACTER * 8    KNAME(9)
      CHARACTER * 8    KNAME1(3)
      CHARACTER * 324  KTITLE
      CHARACTER * 8    KWRITE(3)
      CHARACTER * 8    INUM1
      CHARACTER * 8    INUM2
      CHARACTER * 6    QNAME1
      CHARACTER * 6    QNAME2
      CHARACTER * 6    QNAME3
      CHARACTER * 2    DN
      CHARACTER * 6    QNAME(166)
      CHARACTER * 6    QWRITE
      CHARACTER * 4    SNAME(18)
      CHARACTER * 20   VUNIT(2)
      CHARACTER * 7    AFTER
      CHARACTER * 7    AFTBEF
C $
      INTEGER          KK(3)
      INTEGER          LL(166)
      INTEGER          JKEEP(17)
      INTEGER          JLIST(17)
      INTEGER          C1,C2,E1,E2,S1,S2,Q,M,G
      INTEGER          YY,MM,DD,HH,F1,F2,JT,JN
C $
C     IDWORDS:  MASK CONTROL (INTEGER)
C $
      DATA SHFMSK( 1)/X'20020100'/
      DATA SHFMSK( 2)/X'28020400'/
      DATA SHFMSK( 3)/X'30020400'/
      DATA SHFMSK( 4)/X'38020400'/
      DATA SHFMSK( 5)/X'08050100'/
      DATA SHFMSK( 6)/X'00020100'/
      DATA SHFMSK( 7)/X'08050200'/
      DATA SHFMSK( 8)/X'00020200'/
      DATA SHFMSK( 9)/X'3C010200'/
      DATA SHFMSK(10)/X'28030100'/
      DATA SHFMSK(11)/X'28030200'/
      DATA SHFMSK(12)/X'34030100'/
      DATA SHFMSK(13)/X'20020400'/
      DATA SHFMSK(14)/X'30020400'/
      DATA SHFMSK(15)/X'1C010100'/
      DATA SHFMSK(16)/X'1C010200'/
      DATA SHFMSK(17)/X'20020200'/
C $
C     REFERENCE TABLE FOR SNAME.
C $
      DATA JLIST(1)/1/
      DATA JLIST(2)/2/
      DATA JLIST(3)/6/
      DATA JLIST(4)/7/
      DATA JLIST(5)/8/
      DATA JLIST(6)/16/
      DATA JLIST(7)/19/
      DATA JLIST(8)/128/
      DATA JLIST(9)/129/
      DATA JLIST(10)/130/
      DATA JLIST(11)/144/
      DATA JLIST(12)/145/
      DATA JLIST(13)/146/
      DATA JLIST(14)/147/
      DATA JLIST(15)/148/
      DATA JLIST(16)/131/
      DATA JLIST(17)/132/
C $
C     SNAME TABLE.
C $
      DATA SNAME( 1)/' GPM'/
      DATA SNAME( 2)/' PA '/
      DATA SNAME( 3)/' M  '/
      DATA SNAME( 4)/' M  '/
      DATA SNAME( 5)/' MB '/
      DATA SNAME( 6)/' DEG'/
      DATA SNAME( 7)/' POT'/
      DATA SNAME( 8)/' MSL'/
      DATA SNAME( 9)/' SFC'/
      DATA SNAME(10)/' TRO'/
      DATA SNAME(11)/' BDY'/
      DATA SNAME(12)/' TRS'/
      DATA SNAME(13)/' STS'/
      DATA SNAME(14)/' QCP'/
      DATA SNAME(15)/' SIG'/
      DATA SNAME(16)/'MWSL'/
      DATA SNAME(17)/'PLYR'/
      DATA SNAME(18)/'    '/
C $
C     REFERENCE TABLE FOR QNAME.
C $
      DATA LL( 1)/  1/
      DATA LL( 2)/  2/
      DATA LL( 3)/  6/
      DATA LL( 4)/  8/
      DATA LL( 5)/ 16/
      DATA LL( 6)/ 17/
      DATA LL( 7)/ 18/
      DATA LL( 8)/ 19/
      DATA LL( 9)/ 20/
      DATA LL(10)/ 21/
      DATA LL(11)/ 40/
      DATA LL(12)/ 41/
      DATA LL(13)/ 42/
      DATA LL(14)/ 43/
      DATA LL(15)/ 44/
      DATA LL(16)/ 48/
      DATA LL(17)/ 49/
      DATA LL(18)/ 50/
      DATA LL(19)/ 51/
      DATA LL(20)/ 52/
      DATA LL(21)/ 53/
      DATA LL(22)/ 54/
      DATA LL(23)/ 55/
      DATA LL(24)/ 56/
      DATA LL(25)/ 57/
      DATA LL(26)/ 58/
      DATA LL(27)/ 59/
      DATA LL(28)/ 60/
      DATA LL(29)/ 72/
      DATA LL(30)/ 73/
      DATA LL(31)/ 74/
      DATA LL(32)/ 80/
      DATA LL(33)/ 81/
      DATA LL(34)/ 88/
      DATA LL(35)/ 89/
      DATA LL(36)/ 90/
      DATA LL(37)/ 91/
      DATA LL(38)/ 92/
      DATA LL(39)/ 93/
      DATA LL(40)/ 94/
      DATA LL(41)/ 95/
      DATA LL(42)/ 96/
      DATA LL(43)/112/
      DATA LL(44)/113/
      DATA LL(45)/114/
      DATA LL(46)/115/
      DATA LL(47)/120/
      DATA LL(48)/121/
      DATA LL(49)/160/
      DATA LL(50)/161/
      DATA LL(51)/162/
      DATA LL(52)/163/
      DATA LL(53)/164/
      DATA LL(54)/165/
      DATA LL(55)/166/
      DATA LL(56)/167/
      DATA LL(57)/168/
      DATA LL(58)/169/
      DATA LL(59)/170/
      DATA LL(60)/171/
      DATA LL(61)/176/
      DATA LL(62)/177/
      DATA LL(63)/178/
      DATA LL(64)/184/
      DATA LL(65)/185/
      DATA LL(66)/186/
      DATA LL(67)/187/
      DATA LL(68)/188/
      DATA LL(69)/384/
      DATA LL(70)/385/
      DATA LL(71)/386/
      DATA LL(72)/387/
      DATA LL(73)/388/
      DATA LL(74)/389/
      DATA LL(75)/390/
      DATA LL(76)/391/
      DATA LL(77)/ 97/
      DATA LL(78)/ 98/
      DATA LL(79)/ 99/
      DATA LL(80)/100/
      DATA LL(81)/101/
      DATA LL(82)/102/
      DATA LL(83)/103/
      DATA LL(84)/172/
      DATA LL(85)/200/
      DATA LL(86)/201/
      DATA LL(87)/202/
      DATA LL(88)/203/
      DATA LL(89)/392/
      DATA LL(90)/  7/
      DATA LL(91)/ 61/
      DATA LL(92)/104/
      DATA LL(93)/173/
      DATA LL(94)/174/
      DATA LL(95)/175/
      DATA LL(96)/304/
      DATA LL(97)/305/
      DATA LL(98)/400/
      DATA LL(99)/401/
      DATA LL(100)/402/
      DATA LL(101)/403/
      DATA LL(102)/404/
      DATA LL(103)/405/
      DATA LL(104)/  9/
      DATA LL(105)/105/
      DATA LL(106)/116/
      DATA LL(107)/106/
      DATA LL(108)/107/
      DATA LL(109)/108/
      DATA LL(110)/179/
      DATA LL(111)/180/
      DATA LL(112)/181/
      DATA LL(113)/182/
      DATA LL(114)/183/
      DATA LL(115)/189/
      DATA LL(116)/190/
      DATA LL(117)/191/
      DATA LL(118)/192/
      DATA LL(119)/193/
      DATA LL(120)/194/
      DATA LL(121)/195/
      DATA LL(122)/196/
      DATA LL(123)/197/
      DATA LL(124)/198/
      DATA LL(125)/199/
      DATA LL(126)/204/
      DATA LL(127)/210/
      DATA LL(128)/211/
      DATA LL(129)/212/
      DATA LL(130)/213/
      DATA LL(131)/214/
      DATA LL(132)/215/
      DATA LL(133)/216/
      DATA LL(134)/117/
      DATA LL(135)/209/
      DATA LL(136)/ 22/
      DATA LL(137)/ 62/
      DATA LL(138)/ 63/
      DATA LL(139)/ 82/
      DATA LL(140)/ 83/
      DATA LL(141)/ 84/
      DATA LL(142)/ 85/
      DATA LL(143)/205/
      DATA LL(144)/206/
      DATA LL(145)/207/
      DATA LL(146)/208/
      DATA LL(147)/217/
      DATA LL(148)/109/
      DATA LL(149)/110/
      DATA LL(150)/111/
      DATA LL(151)/86/
      DATA LL(152)/87/
      DATA LL(153)/218/
      DATA LL(154)/133/
      DATA LL(155)/134/
      DATA LL(156)/135/
      DATA LL(157)/23/
      DATA LL(158)/136/
      DATA LL(159)/137/
      DATA LL(160)/71/
      DATA LL(161)/159/
      DATA LL(162)/75/
      DATA LL(163)/157/
      DATA LL(164)/119/
      DATA LL(165)/24/
      DATA LL(166)/158/
C $
C     QNAME TABLE:  CHARACTER*6
C $
      DATA QNAME( 1)/' HGT  '/
      DATA QNAME( 2)/' P ALT'/
      DATA QNAME( 3)/' DIST '/
      DATA QNAME( 4)/' PRES '/
      DATA QNAME( 5)/' TMP  '/
      DATA QNAME( 6)/' DPT  '/
      DATA QNAME( 7)/' DEPR '/
      DATA QNAME( 8)/' POT  '/
      DATA QNAME( 9)/' T MAX'/
      DATA QNAME(10)/' T MIN'/
      DATA QNAME(11)/' V VEL'/
      DATA QNAME(12)/' NETVD'/
      DATA QNAME(13)/' DZDT '/
      DATA QNAME(14)/' OROW '/
      DATA QNAME(15)/' FRCVV'/
      DATA QNAME(16)/' U GRD'/
      DATA QNAME(17)/' V GRD'/
      DATA QNAME(18)/' WIND '/
      DATA QNAME(19)/' T WND'/
      DATA QNAME(20)/' VW SH'/
      DATA QNAME(21)/' U DIV'/
      DATA QNAME(22)/' V DIV'/
      DATA QNAME(23)/' WDIR '/
      DATA QNAME(24)/' WWND '/
      DATA QNAME(25)/' SWND '/
      DATA QNAME(26)/' RATS '/
      DATA QNAME(27)/' VECW '/
      DATA QNAME(28)/' SFAC '/
      DATA QNAME(29)/' ABS V'/
      DATA QNAME(30)/' REL V'/
      DATA QNAME(31)/' DIV  '/
      DATA QNAME(32)/' STRM '/
      DATA QNAME(33)/' V POT'/
      DATA QNAME(34)/' R H  '/
      DATA QNAME(35)/' P WAT'/
      DATA QNAME(36)/' A PCP'/
      DATA QNAME(37)/' P O P'/
      DATA QNAME(38)/' P O Z'/
      DATA QNAME(39)/' SNO D'/
      DATA QNAME(40)/' ACPCP'/
      DATA QNAME(41)/' SPF H'/
      DATA QNAME(42)/' L H2O'/
      DATA QNAME(43)/' LFT X'/
      DATA QNAME(44)/' TOTOS'/
      DATA QNAME(45)/' K X  '/
      DATA QNAME(46)/' C INS'/
      DATA QNAME(47)/' L WAV'/
      DATA QNAME(48)/' S WAV'/
      DATA QNAME(49)/' DRAG '/
      DATA QNAME(50)/' LAND '/
      DATA QNAME(51)/' KFACT'/
      DATA QNAME(52)/' 10TSL'/
      DATA QNAME(53)/' 7TSL '/
      DATA QNAME(54)/' RCPOP'/
      DATA QNAME(55)/' RCMT '/
      DATA QNAME(56)/' RCMP '/
      DATA QNAME(57)/' ORTHP'/
      DATA QNAME(58)/' ALBDO'/
      DATA QNAME(59)/' ENFLX'/
      DATA QNAME(60)/' TTHTG'/
      DATA QNAME(61)/' LAT  '/
      DATA QNAME(62)/' LON  '/
      DATA QNAME(63)/' RADIC'/
      DATA QNAME(64)/' PROB '/
      DATA QNAME(65)/' CPROB'/
      DATA QNAME(66)/' USTAR'/
      DATA QNAME(67)/' TSTAR'/
      DATA QNAME(68)/' MIXHT'/
      DATA QNAME(69)/' WTMP '/
      DATA QNAME(70)/' WVHGT'/
      DATA QNAME(71)/' SWELL'/
      DATA QNAME(72)/' WVSWL'/
      DATA QNAME(73)/' WVPER'/
      DATA QNAME(74)/' WVDIR'/
      DATA QNAME(75)/' SWPER'/
      DATA QNAME(76)/' SWDIR'/
      DATA QNAME(77)/' RRATE'/
      DATA QNAME(78)/' TSTM '/
      DATA QNAME(79)/' CSVR '/
      DATA QNAME(80)/' CTDR '/
      DATA QNAME(81)/' MIXR '/
      DATA QNAME(82)/' PSVR '/
      DATA QNAME(83)/' MCONV'/
      DATA QNAME(84)/' ENRGY'/
      DATA QNAME(85)/' RDNCE'/
      DATA QNAME(86)/' BRTMP'/
      DATA QNAME(87)/' TCOZ '/
      DATA QNAME(88)/' OZMR '/
      DATA QNAME(89)/' ICWAT'/
      DATA QNAME(90)/' DEPTH'/
      DATA QNAME(91)/' GUST '/
      DATA QNAME(92)/' VAPP '/
      DATA QNAME(93)/' TOTHF'/
      DATA QNAME(94)/' SPEHF'/
      DATA QNAME(95)/' SORAD'/
      DATA QNAME(96)/' UOGRD'/
      DATA QNAME(97)/' VOGRD'/
      DATA QNAME(98)/' HTSGW'/
      DATA QNAME(99)/' PERPW'/
      DATA QNAME(100)/' DIRPW'/
      DATA QNAME(101)/' PERSW'/
      DATA QNAME(102)/' DIRSW'/
      DATA QNAME(103)/' WCAPS'/
      DATA QNAME(104)/' PTEND'/
      DATA QNAME(105)/' NCPCP'/
      DATA QNAME(106)/' 4LFTX'/
      DATA QNAME(107)/' ICEAC'/
      DATA QNAME(108)/' NPRAT'/
      DATA QNAME(109)/' CPRAT'/
      DATA QNAME(110)/'CEILHT'/
      DATA QNAME(111)/' VISIB'/
      DATA QNAME(112)/'LIQPCP'/
      DATA QNAME(113)/'FREPCP'/
      DATA QNAME(114)/'FROPCP'/
      DATA QNAME(115)/' MIXLY'/
      DATA QNAME(116)/' DLRFL'/
      DATA QNAME(117)/' ULRFL'/
      DATA QNAME(118)/' DSRFL'/
      DATA QNAME(119)/' USRFL'/
      DATA QNAME(120)/' UTHFL'/
      DATA QNAME(121)/' UTWFL'/
      DATA QNAME(122)/' TTLWR'/
      DATA QNAME(123)/' TTSWR'/
      DATA QNAME(124)/' TTRAD'/
      DATA QNAME(125)/' MSTAV'/
      DATA QNAME(126)/' SWABS'/
      DATA QNAME(127)/' CDLYR'/
      DATA QNAME(128)/' CDCON'/
      DATA QNAME(129)/' PBCLY'/
      DATA QNAME(130)/' PTCLY'/
      DATA QNAME(131)/' PBCON'/
      DATA QNAME(132)/' PTCON'/
      DATA QNAME(133)/' SFEXC'/
      DATA QNAME(134)/' A EVP'/
      DATA QNAME(135)/' STCOF'/
      DATA QNAME(136)/' TSOIL'/
      DATA QNAME(137)/'D DUDT'/
      DATA QNAME(138)/'D DVDT'/
      DATA QNAME(139)/' U STR'/
      DATA QNAME(140)/' V STR'/
      DATA QNAME(141)/' TUVRD'/
      DATA QNAME(142)/' TVVRD'/
      DATA QNAME(143)/' TTLRG'/
      DATA QNAME(144)/' TTSHL'/
      DATA QNAME(145)/' TTDEP'/
      DATA QNAME(146)/' TTVDF'/
      DATA QNAME(147)/' ZSTAR'/
      DATA QNAME(148)/' TQDEP'/
      DATA QNAME(149)/' TQSHL'/
      DATA QNAME(150)/' TQVDF'/
      DATA QNAME(151)/'XGWSTR'/
      DATA QNAME(152)/'YGWSTR'/
      DATA QNAME(153)/' STDZG'/
      DATA QNAME(154)/' A LEV'/
      DATA QNAME(155)/' T AIL'/
      DATA QNAME(156)/' B AIL'/
      DATA QNAME(157)/' EPOT '/
      DATA QNAME(158)/' MSLSA'/
      DATA QNAME(159)/' MSLMA'/
      DATA QNAME(160)/'MGSTRM'/
      DATA QNAME(161)/' CONDP'/
      DATA QNAME(162)/' POT V'/
      DATA QNAME(163)/' CAPE '/
      DATA QNAME(164)/' CIN  '/
      DATA QNAME(165)/' VTMP '/
      DATA QNAME(166)/' TKE  '/
C $
C  REFERENCE TABLE FOR G (GENERATING PROGRAM NAME)
C $
      DATA KK(1)/57/
      DATA KK(2)/58/
      DATA KK(3)/59/
C $
C  G TABLE (GENERATING PROGRM NAME):
C $
      DATA KNAME/'   ECMWF', ' READING', ',UK.    ',
     &           '    FNOC', ' MONTERE', 'Y, CA.  ',
     &           '  AFGWC ', 'OFFUTT A', 'FB, NB. '/
      DATA KNAME1/'   WMC N','MC WASHI', 'NGTON   '/
C $
      DATA AFTER /' AFTER '/
      DATA DN    /'DN'/
      DATA QNAME1/' THCK '/
      DATA QNAME2/' THKDN'/
      DATA QNAME3/' PRSDN'/
C $
      DATA VUNIT(1)/' 0-HR FCST VALID AT '/
      DATA VUNIT(2)/' ANALYSIS  VALID AT '/
      DATA UNIT1   /' HRS'/
      DATA DAYS    /' DYS'/
      DATA FOR1    /' FOR '/
      DATA DASH    /'-'/
C $
 200   FORMAT ( ' ',A7,A4,' ',A7)
 210   FORMAT ( A4,1X,A6,A5,F4.1,A4,A7,
     &          I2.2,A1,I2.2,A1,I2.2,1X,I2.2,'Z',3A8)
 220   FORMAT ( 13X,A7)
 230   FORMAT ( '  Q IS AN ILLEGAL OFFICE NOTE 84 DATA TYPE, Q = ',
     &       I5,35X)
 240   FORMAT ( A4,1X,A6,A20,
     &          I2.2,A1,I2.2,A1,I2.2,1X,I2.2,'Z',3A8)
C $
C $        1.   UNPACK ID WORDS.
C $
      DO 10 N = 1,17
        ITEMP    = 0
        KTEMP    = 0
        ITEMP    = SHFMSK(N)
        NSHIFT   = IAND(ISHFT(ITEMP,-24),255)
        NMASK    = IAND(ISHFT(ITEMP,-16),255)
        NID      = IAND(ISHFT(ITEMP,-8),255)
        ITEMP    = MASK(NMASK)
        KTEMP    = ID(NID)
        JKEEP(N) = IAND(ITEMP,ISHFT(KTEMP,-NSHIFT))
   10 CONTINUE
C $
      F1 = JKEEP(1)
      DD = JKEEP(2)
      MM = JKEEP(3)
      YY = JKEEP(4)
      C1 = JKEEP(5)
      E1 = JKEEP(6)
      C2 = JKEEP(7)
      E2 = JKEEP(8)
      M  = JKEEP(9)
      S1 = JKEEP(10)
      S2 = JKEEP(11)
      Q  = JKEEP(12)
      HH = JKEEP(13)
      G  = JKEEP(14)
      JT = JKEEP(15)
      JN = JKEEP(16)
      F2 = JKEEP(17)
C $
      KS = IAND(ISHFT(ID(3),-40_8),255_8)
C $
C         2.  FIND WHICH PARAMETER (Q) IS INDICATED BE THE ID WORDS.
C $
      DO 20 N = 1,166
        NN = N
        IF (Q.EQ.LL(N)) GO TO 30
 20   CONTINUE
C $
C     CAN NOT FIND A LEGAL Q
      GO TO 170
C $
 30   CONTINUE
      UNIT(1:4)    = UNIT1(1:4)
      FOR(1:5)     = FOR1(1:5)
      AFTBEF(1:7)  = AFTER(1:7)
C $
      IF (E1.GT.128) E1 = -(JKEEP(6)-128)
      IF (E2.GT.128) E2 = -(JKEEP(8)-128)
C $
C $       3.    FIND WHICH SURFACE IS INDICATED BY THE ID WORDS
C $                  AS BEING THE FIRST SURFACE.
C $
      DO 40 I = 1,17
        IF (S1.EQ.JLIST(I)) THEN
          K1 = I
          GO TO 50
        ENDIF
 40   CONTINUE
        K1 = 18
C $
 50   CONTINUE
C $
C $        4.   BEGIN PROCESSING OF A ONE-SURFACE TITLE
C $
      IF (M.EQ.0.OR.M.EQ.8) THEN
        K2 = K1
        CALL VALUE1(S1,C1,E1,INUM1)
        WRITE (KTITLE(1:20),220) INUM1
        GO TO 80
      ENDIF
C $
C $       5.    FIND WHICH SURFACE IS INDICATED BY THE ID WORDS
C $                  AS BEING THE SECOND SURFACE.
C $
      DO 60 I = 1,17
        IF (S2.EQ.JLIST(I)) THEN
          K2 = I
          GO TO 70
        ENDIF
 60   CONTINUE
        K2 = 18
C $
 70   CONTINUE
C $
C $        6.    BEGIN PROCESSING OF A TWO-SURFACE TITLE
C $
      CALL VALUE1(S1,C1,E1,INUM1)
      CALL VALUE1(S2,C2,E2,INUM2)
      WRITE (KTITLE(1:20),200) INUM1 , SNAME(K1) , INUM2
C $
 80   CONTINUE
      QWRITE = QNAME(NN)
C $
      IF (Q.EQ.1 .AND. M.EQ.1.AND. S1.EQ.8)             QWRITE = QNAME1
      IF (Q.EQ.1 .AND. M.EQ.1.AND. S1.EQ.8.AND.KS.EQ.2) QWRITE = QNAME2
      IF (Q.EQ.8 .AND. S1.EQ.128.AND.KS.EQ.2)           QWRITE = QNAME3
      IF (JT.EQ.6) QWRITE(5:6) = DN(1:2)
C $
C $        7.    SET DATE/TIME FIELDS
C $
C $          A.    CHECK IF F1 AND F2 ARE IN HRS, HALF DAYS OR DAYS.
C $
          RF1 = F1
          RF2 = F2
C $
C           B:    IF F1 IN HALF DAYS: CONVERT TO HOURS
C $
        IF (JN.EQ.15.OR.JT.EQ.7) THEN
           RF1 = RF1 * 12.0
           RF2 = RF2 * 12.0
        ENDIF
C $
C           C:    IF F1 IN DAYS: CONVERT TO HOURS
C $
        IF (JT.EQ.10) THEN
           RF1 = RF1 * 24.0
           RF2 = RF2 * 24.0
        ENDIF
C $
C           D:    CONVERT HOURS TO DAYS IF HOURS GREATER THAN 72
C $
        IF (JT.NE.6) THEN
          IF (RF1.GT.72.0.OR.RF2.GT.72.0) THEN
             RF1 = RF1 / 24.0
             RF2 = RF2 / 24.0
             UNIT(1:4) = DAYS(1:4)
          ENDIF
        ENDIF
C $
        IF (JT.EQ.6) THEN
          IF (F1.GT.127) THEN
            F1 = AND(F1,127)
            F1 = -F1
          ENDIF
          CF1 = F1
          CF2 = F2
          CALL CLIMO(CF1,CF2,UNIT,FOR,AFTBEF)
          RF1 = CF1
          CALL SETCL(CF2,UNIT,KTITLE)
        ENDIF
C $
C $        8.   SET GENERATING PROGRAM NAME
C $
      DO 110 K = 1,3
        IF (G.EQ.KK(K)) GO TO 130
  110 CONTINUE
C $
      DO 120 L = 1,3
        KWRITE(L) = KNAME1(L)
  120 CONTINUE
      GO TO 150
C $
  130 CONTINUE
      DO 140 L = 1,3
        KWRITE(L) = KNAME( 3*(K-1) + L)
  140 CONTINUE
C $
C $        9.    ENCODE THE TITLE LINE
C $
C $        9.1   DISTINGUISH BETWEEN ANALYSIS AND ZERO FORECASTS
C                           AND 'REAL' FORECASTS
C $
  150 CONTINUE
         IF (F1.NE.0) GO TO 160
           IF (G.EQ.19.OR.G.EQ.22.OR.G.EQ.43.OR.G.EQ.44.OR.G.EQ.49.OR.
     &         G.EQ.55.OR.G.EQ.56.OR.G.EQ.64) THEN
                 III = 2
                 IF (M.EQ.8.OR.M.EQ.9.OR.M.EQ.10) III = 1
           ELSE
              III = 1
           ENDIF
C $
      WRITE (KTITLE(21:88),240) SNAME(K2), QWRITE, VUNIT(III),
     & YY, DASH, MM, DASH, DD, HH, (KWRITE(L),L=1,3)
      RETURN
C $
 160  CONTINUE
      WRITE (KTITLE(21:88),210) SNAME(K2), QWRITE, FOR, RF1, UNIT,
     & AFTBEF, YY, DASH, MM, DASH, DD, HH, (KWRITE(L),L=1,3)
      RETURN
C $
 170  CONTINUE
        WRITE (KTITLE(1:88),230) Q
      RETURN
      END
       SUBROUTINE VALUE1(S,C,E,NUM)
C $$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C $
C SUBPROGRAM: VALUE1        CREATES VALUE1 OF SURFACE FROM IDS
C   AUTHOR: JONES,R.E.       ORG: W342       DATE: 86-12-03
C $
C ABSTRACT: CREATES THE NUMERICAL VALUE FOR THE SURFACE
C   TO BE BUILT INTO THE FIRST LINE OF THE TITLE.
C $
C PROGRAM HISTORY LOG:
C   88-11-28  R.E.JONES
C   89-11-01  R.E.JONES   CONVERT TO CRAY CFT77 FORTRAN
C $
C USAGE:  CALL VALUE1(S,C,E,NUM)
C $
C   INPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     S      ARG LIST  INTEGER NUMBER OF SURFACE
C     C,E              NUMERICAL VALUE OF THE SURFACE
C                      SURFACE = S * 10 ** E
C $
C   OUTPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     NUM    ARG LIST  7 CHARACTER VALUE OF THE SURFACE FOR THE TITLE
C $
C   SUBPROGRAMS CALLED:
C     NAMES                                                   LIBRARY
C     ------------------------------------------------------- --------
C     INTERNAL (WRITE)                                        SYSLIB
C $
C ATTRIBUTES:
C   LANGUAGE: CRAY CFT77 FORTRAN
C   MACHINE:  CRAY Y-MP8/832
C $
C $$$
C $
      INTEGER        C $
      INTEGER        E
      INTEGER        S
C $
      CHARACTER*8    JNUM
      CHARACTER*8    KNUM
      CHARACTER*7    LTEMP
      CHARACTER*8    NUM
      CHARACTER*1    POINT
      CHARACTER*1    ZERO
C $
      DATA  JNUM  /' 0.0000 '/
      DATA  KNUM  /'        '/
      DATA  POINT /'.'/
      DATA  ZERO  /'0'/
C $
 101  FORMAT ( I6,' ')
C $
      IF (S.GE.128.AND.S.LE.132) GO TO 110
      IF (C.EQ.0) GO TO 100
      WRITE (LTEMP(1:7),101) C $
        J = E + 6
        K = J + 1
        IF (J.EQ.0) GO TO 90
          NUM(1:J)   = LTEMP(1:J)
C $
 90   CONTINUE
        NUM(K:K)   = POINT
        NUM(K+1:8) = LTEMP(K:7)
        IF (J.EQ.0) NUM(2:2) = ZERO
        GO TO 150
C $
 100  CONTINUE
        NUM = JNUM
        GO TO 150
C $
 110  CONTINUE
        NUM = KNUM
C $
 150  CONTINUE
C $
        RETURN
      END
       SUBROUTINE LINE02(ID,MASK,KTITLE)
C $$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C $
C SUBPROGRAM: LINE02         CREATES THE SECOND LINE OF TITLE
C   AUTHOR: JONES,R.E.       ORG: W342       DATE: 86-12-03
C $
C ABSTRACT: CREATES THE SECOND LINE OF THE TITLE FROM THE ID WORDS.
C   CALLED BY W3FP06. WORDS 23 TO 54.
C $
C PROGRAM HISTORY LOG:
C   88-11-28  R.E.JONES
C   89-11-01  R.E.JONES   CONVERT TO CRAY CFT77 FORTRAN
C   91-03-01  R.E.JONES   CHANGES FOR BIG RECORDS
C $
C USAGE:  CALL LINE02(ID,MASK,KTITLE)
C $
C   INPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     ID     ARG LIST  ID WORDS (6 INTEGER WORDS) OFFICE NOTE 84
C     MASK   ARG LIST  MASK FOR UNPACKING ID WORDS (8 WORDS)
C $
C   OUTPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     KTITLE ARG LIST  TITLE CHARACTER*324
C $
C   SUBPROGRAMS CALLED:
C     NAMES                                                   LIBRARY
C     ------------------------------------------------------- --------
C     INTERNAL (WRITE) SHIFT  AND                             SYSLIB
C     q9ie32                                                  W3LIB
C $
C ATTRIBUTES:
C   LANGUAGE: IBM XL FORTRAN
C   MACHINE:  IBM SP
C $
C $$$
C $
      INTEGER(8)          ID(6)
      INTEGER(8)          IKEEP(17)
      INTEGER(4)          MASK(8)
      INTEGER(8)          MASK32,MASKN
      INTEGER(4)          SHFMSK(17)
      integer(8)  irtemp
      real(4)     rtemp(2)
      equivalence (irtemp,rtemp(1))
C $
      CHARACTER * 324  KTITLE
C $
C     IDWORDS:  MASK CONTROL (INTEGER)
C $
      DATA MASKN /X'FFFFFFFFFFFF0000'/
      DATA MASK32/X'00000000FFFFFFFF'/
      DATA SHFMSK( 1)/X'3C010200'/
      DATA SHFMSK( 2)/X'1C010100'/
      DATA SHFMSK( 3)/X'1C010200'/
      DATA SHFMSK( 4)/X'20020100'/
      DATA SHFMSK( 5)/X'20020200'/
      DATA SHFMSK( 6)/X'38020300'/
      DATA SHFMSK( 7)/X'30020300'/
      DATA SHFMSK( 8)/X'28020300'/
      DATA SHFMSK( 9)/X'20020300'/
      DATA SHFMSK(10)/X'3C010300'/
      DATA SHFMSK(11)/X'18020400'/
      DATA SHFMSK(12)/X'10020400'/
      DATA SHFMSK(13)/X'00040400'/
      DATA SHFMSK(14)/X'30040500'/
      DATA SHFMSK(15)/X'00040500'/
      DATA SHFMSK(16)/X'00080500'/
      DATA SHFMSK(17)/X'20040600'/
C $
 100  FORMAT(' M=',I2,' T=',I2,' N=',I2,' F1=',I3,' F2=',I3,' CD=',I3,
     1' CM=',I3,' KS=',I3,' K=',I3,' GES=',I2,' R=',I3,' G=',I3,
     2' J=',I5,' B=',I5,' Z=',I5,' A=',E15.8,' N=',I5,'   ')
C $
C     UNPACK ID WORDS.
C $
      DO 10 N = 1,17
        ITEMP    = SHFMSK(N)
        NSHIFT   = IAND(ISHFT(ITEMP,-24),255)
        NMASK    = IAND(ISHFT(ITEMP,-16),255)
        NID      = IAND(ISHFT(ITEMP,-8),255)
        JTEMP    = MASK(NMASK)
        KTEMP    = ID(NID)
        IKEEP(N) = IAND(JTEMP,ISHFT(KTEMP,-NSHIFT))
 10   CONTINUE
C $
C     CONVERT IBM 32 BIT F.P. NUMBER TO IEEE F.P. NUMBER
C $
C      CALL USSCTC(ID(5),5,A,1)
      irtemp=ID(5)
      call q9ie32(rtemp(2),rtemp(1),1,istat)
      a=rtemp(1)
C $
C     CONVERT 16 BIT SIGNED INTEGER INTO A 64 BIT INTEGER.
C $
      IF (BTEST(IKEEP(17),15_8)) THEN
         IKEEP(17) = IOR(IKEEP(17),MASKN)
      ENDIF
C $
C     TEST FOR BIG RECORD
C $
      IF (IKEEP(13).EQ.0) THEN
        IKEEP(13) = IAND(ID(6),MASK32)
      END IF
C $
      WRITE (KTITLE(89:216),100) (IKEEP(I),I=1,15) , A , IKEEP(17)
      RETURN
      END
       SUBROUTINE LINE03(ID,KTITLE)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C $
C SUBPROGRAM: LINE03         CREATES THE THIRD LINE OF TITLE
C   AUTHOR: JONES,R.E        ORG: W342       DATE: 86-12-03
C $
C ABSTRACT: CREATES THE THIRD LINE OF THE TITLE FROM THE ID WORDS.
C   CALLED BY W3FP06 TO CREATE WORDS 55 TO 81 OF THE TITLE.
C $
C PROGRAM HISTORY LOG:
C   88-11-28  R.E.JONES
C   90-02-03  R.E.JONES   CONVERT TO CRAY CFT77 FORTRAN
C $
C USAGE:  CALL LINE03(ID,KTITLE)
C $
C   INPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     ID     ARG LIST  ID WORDS (6 INTEGER)  OFFICE NOTE 84
C $
C   OUTPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     KTITLE ARG LIST  CHARACTER*324 ARRAY
C $
C   SUBPROGRAMS CALLED:
C     NAMES                                                   LIBRARY
C     ------------------------------------------------------- --------
C     INTERNAL (WRITE)                                        SYSLIB
C $
C $
C ATTRIBUTES:
C   LANGUAGE: CRAY CFT77 FORTRAN
C   MACHINE:  CRAY Y-MP8/832
C $
C $$$
C $
      INTEGER(8)         ID(6)
      INTEGER(8)      MASK32
      INTEGER         ID84(12)
C $
      CHARACTER * 324  KTITLE
C $
      DATA  MASK32/X'00000000FFFFFFFF'/
C $
C     FORTRAN INTERNAL WRITE STATEMENT REPLACES ENCODE
C $
 100  FORMAT ( 12(1X,Z8))
C $
      DO 10 J = 1,11,2
        ID84(J)   = ISHFT(ID(J/2+1),-32_8)
        ID84(J+1) = IAND(ID(J/2+1),MASK32)
 10   CONTINUE
C $
      WRITE (KTITLE(217:324),100) (ID84(I),I=1,12)
      RETURN
      END
       SUBROUTINE CLIMO(CF1,CF2,UNIT,FOR,AFTBEF)
C $$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C $
C SUBPROGRAM: CLIMO          SETS TIME-AVERAGED TITLES
C   AUTHOR: JONES,R.E.       ORG: W342       DATE: 86-12-03
C $
C  ABSTRACT: FILLS IN THE FIRST THIRTEEN CHARACTERS IN THE TITLE
C      TO MAKE THE TITLE A TIME-AVERAGED TITLE.
C $
C PROGRAM HISTORY LOG:
C   88-11-28  R.E.JONES
C   89-11-01  R.E.JONES   CONVERT TO CRAY CFT77 FORTRAN
C $
C  USAGE:  CALL CLIMO(CF1,CF2,UNIT,FOR)
C $
C   INPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     CF1    ARG LIST  FORECAST PERIOD LENGTH
C     CF2    ARG LIST  LENGTH OF THE AVERAGE
C     UNIT   ARG LIST  ORIGINALLY SET TO ' HRS'
C     FOR    ARG LIST  ORIGINALLY SET TO ' FOR '
C     AFTBEF ARG LIST  ORIGINALLY SET TO ' AFTER '
C $
C   OUTPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     UNIT   ARG LIST  SET TO ' DYS' IF NECESSARY
C     FOR    ARG LIST  SET TO ' CTR '
C     AFTBEF ARG LIST  SET TO ' BEFOR ' IF NECESSARY
C $
C  ATTRIBUTES:
C    LANGUAGE: CRAY CFT77 FORTRAN
C    MACHINE:  CRAY Y-MP8/832
C $
C $$$
C $
        REAL         CF1
        REAL         CF2
C $
        CHARACTER*7  AFTBEF
        CHARACTER*7  BEFOR
        CHARACTER*5  FOR
        CHARACTER*5  FOR1
        CHARACTER*4  UNIT
        CHARACTER*4  UNIT1
        CHARACTER*4  UNIT2
C $
        DATA  BEFOR /' BEFOR '/
        DATA  FOR1  /' CTR '/
        DATA  UNIT1 /' DYS'/
        DATA  UNIT2 /' HRS'/
C $
C       SET FOR TO ' CTR '
C $
        FOR(1:5) = FOR1(1:5)
C $
C       DIFFERENCE = CENTERDAY - RUNDATE = F1 + 2 DAYS
C       CHANGE CF1 TO HOURS, ADD 48 HOURS
C $
        DIFF = CF1 * 12.0 + 48.0
C $
C       IF DIFF NEGATIVE, SET AFTBEF TO ' BEFOR '
C $
        IF (DIFF.LT.0.0) AFTBEF(1:7) = BEFOR(1:7)
C $
        CF2 = CF2 * 12.0
C $
        IF (ABS(DIFF).LE.72.0) THEN
          CF1 = ABS(DIFF)
          CF2 = CF2 / 24.0
C $
C        SET UNIT TO ' HRS '
C $
          UNIT(1:4) = UNIT2(1:4)
          GO TO 100
        ENDIF
C $
        CF1 = ABS(DIFF / 24.0 )
        CF2 = CF2 / 24.0
C $
C       SET UNIT TO ' DYS '
C $
        UNIT(1:4) = UNIT1(1:4)
C $
 100   CONTINUE
       RETURN
       END
       SUBROUTINE SETCL(CF2,UNIT,KTITLE)
C$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
C $
C SUBPROGRAM: SETCL          ENCODES TIME-AVERAGED TITLE
C   AUTHOR: JONES,R.E.       ORG: W342       DATE: 86-12-03
C $
C  ABSTRACT: ENCODES THE FIRST THIRTEEN CHARACTERS IN THE TITLE
C      TO MAKE THE TITLE A TIME-AVERAGED TITLE.
C $
C PROGRAM HISTORY LOG:
C   88-11-28  R.E.JONES
C   89-11-01  R.E.JONES   CONVERT TO CRAY CFT77 FORTRAN
C $
C USAGE:  CALL CLIMO(F2,UNIT,KTITLE)
C $
C   INPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     CF2    ARG LIST  LENGTH OF THE FORECAST PERIOD
C     UNIT   ARG LIST  UNITS FOR CF2
C     KTITLE ARG LIST  TITLE TO BE MODIFIED
C $
C   OUTPUT VARIABLES:
C     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C     ------ --------- -----------------------------------------------
C     KTITLE ARG LIST  TITLE WITH THE TIME-AVERAGED INCLUDED
C $
C ATTRIBUTES:
C   LANGUAGE: CRAY CFT77 FORTRAN
C   MACHINE:  CRAY Y-MP8/832
C $
C $$$
C $
        CHARACTER*324 KTITLE
        CHARACTER*13  BLANK
        CHARACTER*4   UNIT
        CHARACTER*4   DUNIT
        CHARACTER*4   HUNIT
C $
        DATA  BLANK /'             '/
        DATA  DUNIT /'-DAY'/
        DATA  HUNIT /'-HR '/
C $
  100   FORMAT (1X, F4.1, A4, ' AVG' )
C $
        KTITLE(1:13) = BLANK(1:13)
C $
        WRITE (KTITLE(1:13),100) CF2 , DUNIT(1:4)
C $
       RETURN
       END
