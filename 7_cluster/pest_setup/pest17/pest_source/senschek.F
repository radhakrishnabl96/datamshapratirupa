

        PROGRAM SNSCHK

C -- PROGRAM SENSCHEK CHECKS THE INTEGRITY OF A SENSAN CONTROL FILE AS
C    WELL AS THE TEMPLATE AND INSTRUCTION FILES CITED THEREIN.

C -- NOTE THAT MAXIMUM PARAMETER AND OBSERVATION LIMITS (LIMITED BY
C    THE FACT THAT TEMPCHEK AND INSCHEK MUST RUN UNDERNEATH) ARE SET IN THE
C    BODY OF THE PROGRAM WHERE NPAR AND NOBS ARE READ.

        INTEGER I,J,IFAIL,IERR,NPAR,NOBS,IPAR,JERR,
     +  MCASE,NINFLE,NOUTFL,ITEMP,K,IS,NRUN,NEXOBS,
     +  MMLINE,
     +  II,WRKLEN,J1,J2,NBLC,MAXWID,NUMERR,N1,N2,N3,NUM1,
     +  NUM2,IOBS,PARDIM,OBSDIM,FLEDIM,
     +  INDIM,OUTDIM
        INTEGER IFAIL4,JLINE,ILINE
        CHARACTER*50 TCLINE

        INTEGER IALERR
        INTEGER LW(10),RW(10),IWORK(:),NUMPAR(:),NUMOBS(:),LLW(:),
     +  RRW(:)
        DOUBLE PRECISION RTEMP
        CHARACTER*10 AFIG,ATEMP*20,ALINE*20,AALINE*200,PARDEL*1,TPAR*12,
     +  FMT*20,AA*1,AMXWID,ATEMP1*20,OTPAR*20
        CHARACTER*12 AVERSION
        CHARACTER*100 CASFL
        CHARACTER*2120 ERRMSG
C -- File size
        CHARACTER*20010 CLINE
        CHARACTER*100 PARFLE,OUTFLE(3),AFILE1,AFILE2
        CHARACTER*100 TMPFLE(:),MODFLE(:),INSFLE(:)
        CHARACTER*12 APAR(:)
        CHARACTER*20 AOBS(:)

        ALLOCATABLE::APAR,AOBS,IWORK,
     +  TMPFLE,NUMPAR,NUMOBS,MODFLE,INSFLE,LLW,RRW

        INTEGER NBLNK

        COMMON /IERR/IERR,MCASE
        COMMON /ERRMSG/ERRMSG
        COMMON /CASFL/ CASFL
        COMMON /ALINE/ ALINE,AALINE
        COMMON /MMLINE/MMLINE

C -- File size
        MAXWID=2000
        CALL WRTINT(AMXWID,MAXWID)

        INCLUDE 'version.inc'
        WRITE(6,1,ERR=9900) TRIM(AVERSION)
1       FORMAT(' SENSCHEK Version ',A,'. ',
     +  'Watermark Numerical Computing.')
        NEXOBS=0


C -- THE COMMAND LINE IS READ AND PARSED

#ifdef NO_CMDLINE
3       WRITE(*,'(A)') ' Enter command line arguments > '
        READ(*,'(A)',ERR=3) CASFL
#else
        CALL PGETCL(CASFL)
#endif
        CALL SPACESUB(CASFL)
        CALL SHIFTL(CASFL)
        CALL REMCHAR(CASFL,CHAR(211))
        IF(CASFL.EQ.' ') THEN
          WRITE(6,20,ERR=9900)
20        FORMAT(/,' SENSCHEK is run using the command:'/,/,T5,
     +    'senschek infile',/,/,' where',/,/,T5,
     +    '"infile" is the name of a SENSAN control file.')
          CALL EXIT(100)
        END IF
#ifdef CAPFILE
        CALL UPCAS(CASFL)
#endif
        MCASE=NBLNK(CASFL)
        DO 50 I=MCASE,1,-1
          IF(CASFL(I:I).EQ.'.')THEN
#ifdef UNIX
            IF(CASFL(I+1:I+1).EQ.'/') GO TO 60
#else
            IF(CASFL(I+1:I+1).EQ.CHAR(92)) GO TO 60
#endif
            GO TO 70
          END IF
50      CONTINUE
#ifdef CAPFILE
60      CASFL=CASFL(1:MCASE)//'.SNS'
#else
60      CASFL=CASFL(1:MCASE)//'.sns'
#endif
        MCASE=MCASE+4
70      CONTINUE

C -- The SENSAN control file is opened

100     WRITE(6,101,ERR=9900)
101     FORMAT(/,' Errors ----->')
        OPEN(UNIT=30,FILE=CASFL(1:MCASE),STATUS='OLD',ERR=110)
        GO TO 150
110     WRITE(ERRMSG,120) CASFL(1:MCASE)
120     FORMAT(' Cannot open SENSAN control file ',A)
        CALL WRTERR
        CALL EXIT(100)

150     ILINE=1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL LOWCAS(CLINE)
        IF(CLINE.NE.'scf') THEN
          WRITE(ERRMSG,130) CASFL(1:MCASE)
130       FORMAT(' File ',A,' does not have correct SENSAN control ',
     +    'file header.')
          CALL WRTERR
          GO TO 9800
        END IF

C -- THE CONTROL DATA SECTION OF THE SENSAN CONTROL FILE IS READ AND CHECKED

        ILINE=ILINE+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL LOWCAS(CLINE)
        CALL SHIFTL(CLINE)
        IF(CLINE.NE.'* control data')THEN
          WRITE(6,140,ERR=9900) CASFL(1:MCASE)
140       FORMAT(' Line 2 of file ',A,' should read:',
     +    /,' * control data')
          GO TO 9800
        END IF

        IERR=0
        ALINE=' Line 3 of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        ILINE=ILINE+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL LINSPL(IFAIL,1,LW,RW,CLINE)
        IF(IFAIL.NE.0) GO TO 9500
        CALL LOWCAS(CLINE)
        IF((CLINE(LW(1):RW(1)).NE.'verbose').AND.
     +     (CLINE(LW(1):RW(1)).NE.'noverbose'))THEN
           WRITE(ERRMSG,190) AALINE(1:MMLINE)
190        FORMAT(A,'character variable SCREENDISP must be ',
     +     '"verbose" or "noverbose".')
           CALL WRTERR
        END IF

200     NPAR=0
        NOBS=0
        ALINE=' Line 4 of file '
        ILINE=ILINE+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL LINSPL(IFAIL,2,LW,RW,CLINE)
        IF(IFAIL.NE.0) GO TO 9500
        CALL NUMRD(IFAIL,0,LW(1),RW(1),NPAR,RTEMP,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(0,'NPAR')
        ELSE
          IF(NPAR.LE.0) THEN
            CALL ZRONEG(0,'NPAR')
#ifdef LAHEY
          ELSE IF(NPAR.GT.2000)THEN
            WRITE(ERRMSG,235)ALINE(1:16),CASFL(1:MCASE)
235         FORMAT(A,A,': number of parameters must not exceed 2000.')
            CALL WRTERR
#endif
          END IF
        END IF
        CALL NUMRD(IFAIL,0,LW(2),RW(2),NOBS,RTEMP,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(0,'NOBS')
        ELSE
          IF(NOBS.LE.0) THEN
            CALL ZRONEG(0,'NOBS')
#ifdef LAHEY
          ELSE IF(NOBS.GT.20000) THEN
            WRITE(ERRMSG,240) ALINE(1:16),CASFL(1:MCASE)
240         FORMAT(A,A,': number of observations must not exceed ',
     +      '20000.')
            CALL WRTERR
#endif
          END IF
        END IF

C -- STORAGE IS ALLOCATED FOR ARRAYS

        IF(IERR.NE.0) GO TO 9800
        PARDIM=MAX(1,NPAR)
        OBSDIM=MAX(1,NOBS)

        ALLOCATE(APAR(PARDIM),LLW(PARDIM),
     +  RRW(PARDIM),STAT=IALERR)
        IF(IALERR.NE.0) GO TO 9400

        ALLOCATE(AOBS(OBSDIM),IWORK(PARDIM),STAT=IALERR)
        IF(IALERR.NE.0) GO TO 9400

        DO 234 IOBS=1,NOBS
          AOBS(IOBS)=' '
234     CONTINUE


C -- MORE OF THE CONTROL DATA SECTION IS READ AND CHECKED

        ALINE=' Line 5 of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        ILINE=ILINE+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL LINSPL(IFAIL,4,LW,RW,CLINE)
        IF(IFAIL.NE.0) GO TO 9500
        CALL NUMRD(IFAIL,0,LW(1),RW(1),NINFLE,RTEMP,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(0,'NTPLFLE')
        ELSE
          IF(NINFLE.LE.0) THEN
            CALL ZRONEG(0,'NTPLFLE')
          END IF
        END IF
        CALL NUMRD(IFAIL,0,LW(2),RW(2),NOUTFL,RTEMP,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(0,'NINSFLE')
        ELSE
          IF(NOUTFL.LE.0) THEN
            CALL ZRONEG(0,'NINSFLE')
          ELSE IF(NOUTFL.GT.NOBS)THEN
            WRITE(ERRMSG,340) ALINE(1:16),CASFL(1:MCASE)
340         FORMAT(A,A,': number of instruction files exceeds ',
     +      'number of observations.')
            CALL WRTERR
          END IF
        END IF

        ATEMP=CLINE(LW(3):RW(3))
        CALL LOWCAS(ATEMP)
        IF((ATEMP.NE.'single').AND.(ATEMP.NE.'double'))THEN
          WRITE(ERRMSG,345) AALINE(1:MMLINE)
345       FORMAT(A,'character variable PRECIS must be ',
     +     '"single" or "double".')
           CALL WRTERR
        END IF
        ATEMP=CLINE(LW(4):RW(4))
        CALL LOWCAS(ATEMP)
        IF((ATEMP.NE.'point').AND.(ATEMP.NE.'nopoint'))THEN
          WRITE(ERRMSG,350) AALINE(1:MMLINE)
350       FORMAT(A,'character variable DPOINT must be ',
     +     '"point" or "nopoint".')
           CALL WRTERR
        END IF

        ITEMP=MAX(NINFLE,NOUTFL)
        FLEDIM=MAX(1,ITEMP)
        INDIM=MAX(1,NINFLE)
        OUTDIM=MAX(1,NOUTFL)

        ALLOCATE(TMPFLE(INDIM),NUMPAR(INDIM),
     +  NUMOBS(OUTDIM),MODFLE(FLEDIM),INSFLE(OUTDIM),STAT=IALERR)
        IF(IALERR.NE.0) GO TO 9400

C -- THE SENSAN FILES SECTION OF THE SENSAN CONTROL FILE IS READ AND CHECKED

        IF(IERR.NE.0) GO TO 9800
        ILINE=ILINE+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL LOWCAS(CLINE)
        CALL SHIFTL(CLINE)
        IF(CLINE.NE.'* sensan files') THEN
          WRITE(6,1750,ERR=9900) CASFL(1:MCASE)
1750      FORMAT(' Line 6 of file ',A,' should read:',
     +    /,' * sensan files')
          GO TO 9800
        END IF

        ALINE=' Line 7 of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        ILINE=ILINE+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
#ifdef CAPFILE
        CALL UPCAS(CLINE)
#endif
        TCLINE=CLINE(1:50)
        CALL LOWCAS(TCLINE)
        IF(TCLINE.EQ.'* model command line')THEN
          WRITE(ERRMSG,8010)AALINE(1:MMLINE)
8010      FORMAT(A,'too few "sensan files" lines - 4 expected.')
          CALL WRTERR
          GO TO 9800
        END IF
        IF(INDEX(CLINE,CHAR(9)).NE.0) THEN
          WRITE(ERRMSG,8020) AALINE(1:MMLINE)
8020      FORMAT(A,'line must not contain tab character.')
          CALL WRTERR
          GO TO 9800
        END IF
        CALL SPACESUB(CLINE)
        CALL LINSPL(IFAIL,1,LW,RW,CLINE)
        IF(IFAIL.NE.0) GO TO 9500
        PARFLE=CLINE(LW(1):RW(1))
        CALL REMCHAR(PARFLE,CHAR(211))
        WRKLEN=NBLNK(PARFLE)
        OPEN(UNIT=40,FILE=PARFLE(1:WRKLEN),STATUS='OLD',IOSTAT=JERR)
        IF(JERR.NE.0) THEN
          WRITE(ERRMSG,8050) AALINE(1:MMLINE),PARFLE(1:WRKLEN)
8050      FORMAT(A,'cannot open parameter variation file ',A)
          CALL WRTERR
          GO TO 9800
        END IF

8100    JLINE=1
        CALL WRTINT(ATEMP,JLINE)
        AALINE=' Line '//ATEMP(1:INDEX(ATEMP,' ')-1)//' of parameter '//
     +  'variation file '//PARFLE(1:WRKLEN)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(40,'(A)',ERR=8900,END=8950) CLINE
        CALL LOWCAS(CLINE)              ! check case sensitivity
        CALL LINSPL(IFAIL,NPAR,LLW,RRW,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL WRTINT(ATEMP,NPAR)
          WRITE(ERRMSG,8150) AALINE(1:MMLINE),ATEMP(1:NBLNK(ATEMP))
8150      FORMAT(A,A,' parameter names expected.')
          CALL WRTERR
          GO TO 8990
        END IF
        DO 8200 IPAR=1,NPAR
          IF(RRW(IPAR)-LLW(IPAR).GT.11)THEN
            WRITE(ERRMSG,8160) AALINE(1:MMLINE),
     +      CLINE(LLW(IPAR):RRW(IPAR))
8160        FORMAT(A,'parameter name "',A,'" greater than 12 ',
     +      'characters long.')
            CALL WRTERR
          END IF
          APAR(IPAR)=CLINE(LLW(IPAR):MIN(RRW(IPAR),LLW(IPAR)+11))
8200    CONTINUE
        DO 8230 IPAR=1,NPAR-1
          DO 8220 I=IPAR+1,NPAR
            IF(RRW(I)-LLW(I).EQ.RRW(IPAR)-LLW(IPAR))THEN
            IF(CLINE(LLW(IPAR):RRW(IPAR)).EQ.CLINE(LLW(I):RRW(I)))THEN
              WRITE(ERRMSG,8210) AALINE(1:MMLINE)
8210          FORMAT(A,'parameter names identical.') !stop befor template
              CALL WRTERR
              GO TO 8260
            END IF
            END IF
8220      CONTINUE
8230    CONTINUE
8260    CALL LINSPL(IFAIL,NPAR+1,LLW,RRW,CLINE)
        IF(IFAIL.EQ.0)THEN
          CALL WRTINT(ATEMP,NPAR)
          WRITE(ERRMSG,8280) AALINE(1:MMLINE),
     +    ATEMP(1:NBLNK(ATEMP))
8280      FORMAT(A,'discrepancy in number of parameters. NPAR ',
     +    'provided as ',A,' in SENSAN control file.')
          CALL WRTERR
        END IF

        NRUN=0
8290    JLINE=JLINE+1
        CALL WRTINT(ATEMP,JLINE)
        AALINE=' Line '//ATEMP(1:INDEX(ATEMP,' ')-1)//' of parameter '//
     +  'variation file '//PARFLE(1:WRKLEN)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(40,'(A)',ERR=8900,END=8400) CLINE
        IF(CLINE.EQ.' ') GO TO 8290
        CALL LINSPL(IFAIL,NPAR,LLW,RRW,CLINE)
        NRUN=NRUN+1
        IF(IFAIL.NE.0) THEN
          CALL WRTINT(ATEMP,NPAR)
          WRITE(ERRMSG,8305) AALINE(1:MMLINE),ATEMP(1:NBLNK(ATEMP))
8305      FORMAT(A,A,' entries expected.')
          CALL WRTERR
        ELSE
          DO 8360 I=1,NPAR
            CALL NUMRD(IFAIL,1,LLW(I),RRW(I),ITEMP,RTEMP,CLINE)
            IF(IFAIL.NE.0)THEN
              WRITE(ERRMSG,8330) AALINE(1:MMLINE)
8330          FORMAT(A,'cannot read parameter value.')
              CALL WRTERR
              GO TO 8290
            END IF
8360      CONTINUE
        END IF
        GO TO 8290

8400    CLOSE(UNIT=40)
        IF(NRUN.EQ.0)THEN
          WRITE(ERRMSG,8405) PARFLE(1:NBLNK(PARFLE))
8405      FORMAT(' No parameter values provided in file ',a)
          CALL WRTERR
          GO TO 8990
        END IF
        IF(IERR.NE.0) GO TO 8990
        GO TO 8500

8900    WRITE(ERRMSG,8910) AALINE(1:MMLINE)
8910    FORMAT(A,'cannot read line.')
        CALL WRTERR
        GO TO 8990
8950    WRITE(ERRMSG,8960) AALINE(1:MMLINE)
8960    FORMAT(A,': unexpected end to file.')
        CALL WRTERR
        GO TO 8990
8990    CLOSE(UNIT=40,IOSTAT=JERR)
        GO TO 9800

8500    DO 8700 I=1,3
          ILINE=ILINE+1
          CALL WRTINT(ATEMP,ILINE)
          ALINE=' Line '//ATEMP(1:NBLNK(ATEMP))//' of file '
          AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
          MMLINE=NBLNK(AALINE)+1
          READ(30,'(A)',ERR=9600,END=9700) OUTFLE(I)
          CALL SPACESUB(OUTFLE(I))
          CALL REMCHAR(OUTFLE(I),CHAR(211))
#ifdef CAPFILE
          CALL UPCAS(OUTFLE(I))
#endif
          TCLINE=OUTFLE(I)(1:50)
          CALL LOWCAS(TCLINE)
          IF(TCLINE.EQ.'* model command line')THEN
            WRITE(ERRMSG,8010)AALINE(1:MMLINE)
            CALL WRTERR
            GO TO 1755
          END IF
          IF(INDEX(OUTFLE(I),CHAR(9)).NE.0) THEN
            WRITE(ERRMSG,8020) AALINE(1:MMLINE)
            CALL WRTERR
          END IF
          IF(OUTFLE(I).EQ.' ')THEN
            WRITE(ERRMSG,8520) AALINE(1:MMLINE)
8520        FORMAT(A,'no filename provided.')
            CALL WRTERR
          END IF
8700    CONTINUE

        AFILE1=PARFLE
#ifdef LAHEY
        CALL LOWCAS(AFILE1)
#endif
        DO 8730 I=1,3
          AFILE2=OUTFLE(I)
          IF(AFILE2.EQ.' ') GO TO 8730
#ifdef LAHEY
          CALL LOWCAS(AFILE2)
#endif
          IF(AFILE1.EQ.AFILE2) THEN
            WRITE(ERRMSG,8710)
8710        FORMAT(' One or more SENSAN output files have identical ',
     +      'name.')
            CALL WRTERR
            GO TO 1745
          END IF
8730    CONTINUE
        DO 8750 I=1,2
          AFILE1=OUTFLE(I)
          IF(AFILE1.EQ.' ') GO TO 8750
#ifdef LAHEY
          CALL LOWCAS(AFILE1)
#endif
          DO 8745 J=I+1,3
            AFILE2=OUTFLE(J)
            IF(AFILE2.EQ.' ') GO TO 8745
#ifdef LAHEY
            CALL LOWCAS(AFILE2)
#endif
            IF(AFILE1.EQ.AFILE2)THEN
              WRITE(ERRMSG,8710)
              CALL WRTERR
              GO TO 1745
            END IF
8745      CONTINUE
8750    CONTINUE

C -- THE MODEL COMMAND LINE SECTION OF THE SENSAN CONTROL FILE IS READ AND
C -- CHECKED

1745    ILINE=ILINE+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL LOWCAS(CLINE)
        CALL SHIFTL(CLINE)
        IF(CLINE.NE.'* model command line') THEN
          CALL WRTINT(ATEMP,ILINE)
          WRITE(6,1746,ERR=9900) ATEMP(1:INDEX(ATEMP,' ')-1),
     +    CASFL(1:MCASE)
1746      FORMAT(' Line ',A,' of file ',A,' should read:',
     +    /,' * model command line')
          GO TO 9800
        END IF
1755    ILINE=ILINE+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL LOWCAS(CLINE)
        CALL SHIFTL(CLINE)
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        IF(CLINE.EQ.' ')THEN
          WRITE(ERRMSG,1760)AALINE(1:MMLINE)
1760      FORMAT(A,'blank model command line.')
          CALL WRTERR
        ELSE IF(CLINE.EQ.'* model input/output')THEN
          WRITE(ERRMSG,1765) AALINE(1:MMLINE)
1765      FORMAT(A,'missing model command line.')
          CALL WRTERR
          GO TO 1800
        END IF

C -- THE MODEL INPUT/OUTPUT SECTION OF THE PEST CONTROL FILE IS READ AND
C -- CHECKED

        ILINE=ILINE+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL LOWCAS(CLINE)
        CALL SHIFTL(CLINE)
        IF(CLINE.NE.'* model input/output') THEN
          CALL WRTINT(ATEMP,ILINE)
          WRITE(6,1770,ERR=9900) ATEMP(1:INDEX(ATEMP,' ')-1),
     +    CASFL(1:MCASE)
1770       FORMAT(' Line ',A,' of file ',A,' should read:',
     +    /,' * model input/output')
          GO TO 9800
        END IF
1800    CONTINUE
        IF(IERR.NE.0) GO TO 9800

        DO 1805 I=1,NPAR
1805    IWORK(I)=1000
        NUMERR=IERR
        DO 2500 I=1,NINFLE
        NUMPAR(I)=0
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL SHIFTL(CLINE)
#ifdef CAPFILE
        CALL UPCAS(CLINE)
#endif
        CALL SPACESUB(CLINE)
        CALL LINSPL(IFAIL,2,LW,RW,CLINE)
        IF(IFAIL.NE.0) GO TO 9500
        TMPFLE(I)=CLINE(LW(1):RW(1))
        CALL REMCHAR(TMPFLE(I),CHAR(211))
        WRKLEN=NBLNK(TMPFLE(I))
        MODFLE(I)=CLINE(LW(2):RW(2))
        CALL REMCHAR(MODFLE(I),CHAR(211))
        IF(I.GT.1) THEN
          DO 1814 J=1,I-1
          IF(MODFLE(J).EQ.MODFLE(I))THEN
        WRITE(ERRMSG,1813)AALINE(1:MMLINE),
     +  MODFLE(I)(:NBLNK(MODFLE(I)))
1813        FORMAT(A,'model input file ',A,' cited more than once.')
            CALL WRTERR
            GO TO 1812
          END IF
1814      CONTINUE
1812      CONTINUE
          DO 1815 J=1,I-1
          IF(TMPFLE(I).EQ.TMPFLE(J))THEN
            NUMPAR(I)=NUMPAR(J)
            GO TO 2500
          END IF
1815      CONTINUE
        END IF

C -- A TEMPLATE FILE IS READ AND CHECKED

        OPEN(UNIT=40,FILE=TMPFLE(I)(1:WRKLEN),STATUS='OLD',ERR=1820)
        GO TO 1840
1820    WRITE(ERRMSG,1830) AALINE(1:MMLINE),TMPFLE(I)(1:WRKLEN)
1830    FORMAT(A,'cannot open model template file ',A,'.')
        CALL WRTERR
        GO TO 2500
1840    JLINE=1
        CALL WRTINT(ATEMP,JLINE)
        AALINE=' Line '//ATEMP(1:INDEX(ATEMP,' ')-1)//' of template '//
     +  'file '//TMPFLE(I)(1:WRKLEN)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(40,'(A)',ERR=2300,END=2400) CLINE
        CALL LOWCAS(CLINE)
        IF((CLINE(1:3).EQ.'pif').OR.(CLINE(1:3).EQ.'jif'))THEN
          WRITE(ERRMSG,1845) TMPFLE(I)(1:WRKLEN)
1845      FORMAT(' File ',A,' is an instruction file: a template ',
     +    'file is expected.')
          CALL WRTERR
          GO TO 2490
        END IF
        IF(((CLINE(1:4).NE.'ptf ').AND.
     +      (CLINE(1:4).NE.'jtf ')).OR.(NBLNK(CLINE).LT.5))THEN
          WRITE(ERRMSG,1850) AALINE(1:MMLINE)
1850      FORMAT(A,'first line must be "ptf" or "jtf" + ',
     +    'parameter delimiter.')
          CALL WRTERR
          GO TO 2490
        END IF
        PARDEL=CLINE(5:5)
        IF(PARDEL.EQ.' ')THEN
          WRITE(ERRMSG,1860) AALINE(1:MMLINE)
1860      FORMAT(A,'parameter delimiter not found in fifth character ',
     +    'position.')
          CALL WRTERR
          GO TO 2490
        END IF
        II=ICHAR(PARDEL)
        IF(((II.GE.48).AND.(II.LE.57)).OR.
     +     ((II.GE.97).AND.(II.LE.122)))THEN
          WRITE(ERRMSG,1870) AALINE(1:MMLINE)
1870      FORMAT(A,'illegal parameter delimiter.')
          CALL WRTERR
          GO TO 2490
        END IF
1880    JLINE=JLINE+1
        CALL WRTINT(ATEMP,JLINE)
        AALINE=' Line '//ATEMP(1:INDEX(ATEMP,' ')-1)//' of template '//
     +  'file '//TMPFLE(I)(1:WRKLEN)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(40,'(A)',ERR=2300,END=2400) CLINE
1900    NBLC=NBLNK(CLINE)
        IF(NBLC.GT.MAXWID)THEN
          WRITE(ERRMSG,1915) AALINE(1:MMLINE),
     +    AMXWID(:NBLNK(AMXWID))
1915      FORMAT(A,'line exceeds ',A,' characters.')
          CALL WRTERR
          GO TO 1880
        END IF
        J2=0
1920    IF(J2.GE.NBLC) GO TO 1880
        J1=INDEX(CLINE(J2+1:NBLC),PARDEL)
        IF(J1.EQ.0) GO TO 1880
        J1=J1+J2
        J2=INDEX(CLINE(J1+1:NBLC),PARDEL)
        IF(J2.EQ.0)THEN
          WRITE(ERRMSG,1930) AALINE(1:MMLINE)
1930      FORMAT(A,'missing parameter delimiter.')
          CALL WRTERR
          GO TO 1880
        END IF
        J2=J2+J1
        IF(INDEX(CLINE(J1:J2),CHAR(9)).NE.0)THEN
          WRITE(ERRMSG,1931) AALINE(1:MMLINE),CLINE(J1:J2)
1931      FORMAT(A,'tab character embedded in parameter space ("',A,
     +    '"); use space characters.')
          CALL WRTERR
          GO TO 1920
        END IF
        CALL PARCHK(IFAIL,J1,J2,TPAR,CLINE)
        IF(IFAIL.NE.0) GO TO 1920
        CALL WHICH1(IFAIL4,NPAR,II,APAR,TPAR)
        IF(IFAIL4.NE.0)THEN
          WRITE(ERRMSG,1940) AALINE(1:MMLINE),TPAR(:NBLNK(TPAR)),
     +    PARFLE(1:NBLNK(PARFLE))
1940      FORMAT(A,'parameter "',A,'" not cited in parameter ',
     +    'variation file ',A)
          CALL WRTERR
          GO TO 1920
        END IF
        IWORK(II)=0
        NUMPAR(I)=NUMPAR(I)+1
        GO TO 1920

2300    WRITE(ERRMSG,2310) AALINE(1:MMLINE)
2310    FORMAT(A,'cannot read line.')
        CALL WRTERR
        GO TO 2490
2400    IF(JLINE.EQ.1)THEN
          WRITE(ERRMSG,2410) AALINE(1:MMLINE)
2410      FORMAT(A,'unexpected end to file.')
          CALL WRTERR
        END IF
2490    CLOSE(UNIT=40)
2500    CONTINUE

        IF(IERR.EQ.NUMERR)THEN
          DO 2520 I=1,NPAR
          IF(IWORK(I).EQ.1000) THEN
            WRITE(ERRMSG,2510) APAR(I)(:NBLNK(APAR(I))),
     +      PARFLE(1:NBLNK(PARFLE))
2510        FORMAT(' Parameter "',A,'" from file ',A,
     +      ' not cited in a template file.')
            CALL WRTERR
          ENDIF
2520      CONTINUE
2521      CONTINUE
        ENDIF

        NUMERR=IERR
        DO 3000 I=1,NOUTFL
        NUMOBS(I)=0
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL SHIFTL(CLINE)
        CALL SPACESUB(CLINE)
#ifdef CAPFILE
        CALL UPCAS(CLINE)
#endif
        CALL LINSPL(IFAIL,2,LW,RW,CLINE)
        IF(IFAIL.NE.0) GO TO 9500
        INSFLE(I)=CLINE(LW(1):RW(1))
        CALL REMCHAR(INSFLE(I),CHAR(211))
        WRKLEN=NBLNK(INSFLE(I))
        MODFLE(I)=CLINE(LW(2):RW(2))
        CALL REMCHAR(MODFLE(I),CHAR(211))
        IF(I.GT.1) THEN
          DO 2600 J=1,I-1
          IF(INSFLE(J).EQ.INSFLE(I))THEN
        WRITE(ERRMSG,2610)AALINE(1:MMLINE),
     +  INSFLE(I)(:NBLNK(INSFLE(I)))
2610        FORMAT(A,'instruction file ',A,' cited more than once.')
            CALL WRTERR
            GO TO 3000
          END IF
2600      CONTINUE
        END IF

C -- AN INSTRUCTION FILE IS READ AND CHECKED

        OPEN(UNIT=40,FILE=INSFLE(I)(1:WRKLEN),STATUS='OLD',ERR=2620)
        GO TO 2640
2620    WRITE(ERRMSG,2630) AALINE(1:MMLINE),INSFLE(I)(1:WRKLEN)
2630    FORMAT(A,'cannot open instruction file ',A,'.')
        CALL WRTERR
        GO TO 3000
2640    JLINE=1
        CALL WRTINT(ATEMP,JLINE)
        AALINE=' Line '//ATEMP(1:INDEX(ATEMP,' ')-1)//' of instruction '
     +  //'file '//INSFLE(I)(1:WRKLEN)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(40,'(A)',ERR=2900,END=2950) CLINE
        CALL TABREM(CLINE)
        CALL LOWCAS(CLINE)
        IF((CLINE(1:3).EQ.'ptf').OR.(CLINE(1:3).EQ.'jtf'))THEN
          WRITE(ERRMSG,2645) INSFLE(I)(1:WRKLEN)
2645      FORMAT(' File ',A,' is a template file: an instruction ',
     +    'file is expected.')
          CALL WRTERR
          GO TO 2990
        END IF
        IF(((CLINE(1:4).NE.'pif ').AND.
     +      (CLINE(1:4).NE.'jif ')).OR.(NBLNK(CLINE).LT.5))THEN
          WRITE(ERRMSG,2650) AALINE(1:MMLINE)
2650      FORMAT(A,'first line must be "pif" or "jif" + ',
     +    'marker delimiter.')
          CALL WRTERR
          GO TO 2990
        END IF
        PARDEL=CLINE(5:5)
        II=ICHAR(PARDEL)
        IF(PARDEL.EQ.' ')THEN
          WRITE(ERRMSG,2660) AALINE(1:MMLINE)
2660      FORMAT(A,'marker delimiter not found in fifth character ',
     +    'position.')
          CALL WRTERR
          GO TO 2990
        ELSE IF((PARDEL.EQ.'(').OR.(PARDEL.EQ.')').OR.(PARDEL.EQ.'[')
     +    .OR.(PARDEL.EQ.']').OR.(PARDEL.EQ.'!').OR.
     +    ((II.GE.48).AND.(II.LE.57)).OR.
     +    ((II.GE.97).AND.(II.LE.122)).OR.
     +    (PARDEL.EQ.'&').OR.(PARDEL.EQ.':'))THEN
          WRITE(ERRMSG,2670) AALINE(1:MMLINE)
2670      FORMAT(A,'illegal marker delimiter.')
          CALL WRTERR
          PARDEL=' '
          GO TO 2990
        END IF
        IS=0
2680    JLINE=JLINE+1
        CALL WRTINT(ATEMP,JLINE)
        AALINE=' Line '//ATEMP(1:INDEX(ATEMP,' ')-1)//
     +  ' of instruction file '//INSFLE(I)(1:WRKLEN)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(40,'(A)',ERR=2900,END=2950) CLINE
        IF(INDEX(CLINE,CHAR(9)).NE.0)THEN
          IF(INDEX(CLINE,PARDEL).EQ.0) CALL TABREM(CLINE)
        ENDIF
        IF(CLINE.EQ.' ') GO TO 2680
        NBLC=NBLNK(CLINE)
        IF(NBLC.GT.MAXWID)THEN
          WRITE(ERRMSG,2705) AALINE(1:MMLINE),
     +    AMXWID(:NBLNK(AMXWID))
2705      FORMAT(A,'line exceeds ',A,' characters.')
          CALL WRTERR
          GO TO 2680
        END IF
        CALL SHIFTL(CLINE)
        IF((IS.EQ.0).AND.(CLINE(1:1).EQ.'&'))THEN
          WRITE(ERRMSG,2706) AALINE(1:MMLINE)
2706      FORMAT(A,'first instruction line in file cannot begin ',
     +    'with continuation character.')
          CALL WRTERR
        END IF
        IS=1
        N2=0
        IF((CLINE(1:1).NE.'l').AND.(CLINE(1:1).NE.'L').AND.
     +  (CLINE(1:1).NE.'&').AND.(CLINE(1:1).NE.PARDEL))THEN
          WRITE(ERRMSG,2685) AALINE(1:MMLINE)
2685      FORMAT(A,'instruction line must begin with "l", a ',
     +    'marker delimiter or "&" (for continuation).')
          CALL WRTERR
        END IF
        IF(CLINE(1:1).NE.'&') J1=0
2690    NBLC=NBLNK(CLINE)
2700    CALL GETINT(IFAIL4,CLINE,N1,N2,NBLC,PARDEL)
        IF(IFAIL4.NE.0)THEN
          WRITE(ERRMSG,2710) AALINE(1:MMLINE)
2710      FORMAT(A,'missing marker delimiter.')
          CALL WRTERR
          GO TO 2680
        END IF
        IF(N1.EQ.0) GO TO 2680
        IF((CLINE(N1:N1).EQ.'l').OR.(CLINE(N1:N1).EQ.'L'))THEN
          IF(N1.EQ.N2)THEN
            CALL ILLINS(CLINE(N1:N2))
            GO TO 2700
          END IF
          WRITE(FMT,2730) N2-N1
2730      FORMAT('(I',I4,')')
          READ(CLINE(N1+1:N2),FMT,ERR=2740) ITEMP
          GO TO 2750
2740      CALL ILLINS(CLINE(N1:N2))
          GO TO 2700
2750      IF(N1.NE.1)THEN
            WRITE(ERRMSG,2720) AALINE(1:MMLINE)
2720        FORMAT(A,'line advance instruction can only occur at the ',
     +      'beginning of an instruction line.')
            CALL WRTERR
          END IF
          IF(ITEMP.LE.0) THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2755)
2755        FORMAT('  (Integer following "l" must be positive.)')
            CALL WRTERR
          END IF
        ELSE IF(CLINE(N1:N1).EQ.PARDEL)THEN
          IF(INDEX(CLINE(N1:N2),CHAR(9)).NE.0)THEN
            WRITE(ERRMSG,2686) AALINE(1:MMLINE)
2686        FORMAT(A,'tab character embedded in marker; space ',
     +      'characters must be used.')
            CALL WRTERR
          END IF
          IF(N2-N1.EQ.1)THEN
            WRITE(ERRMSG,2760)AALINE(1:MMLINE)
2760        FORMAT(A,'marker has zero length.')
            CALL WRTERR
          ELSE
            J1=J1+N2-N1-1
            IF(J1.GT.MAXWID)THEN
              WRITE(ERRMSG,2765) AALINE(1:MMLINE),
     +        AMXWID(:NBLNK(AMXWID))
2765          FORMAT(A,'model output file width cannot exceed ',A,
     +        ' characters.')
              CALL WRTERR
              GO TO 2680
            END IF
          ENDIF
        ELSE IF(CLINE(N1:N1).EQ.'&')THEN
          IF(N1.NE.N2)THEN
            CALL ILLINS(CLINE(N1:N2))
          ELSE
            IF(N1.GT.1)THEN
              WRITE(ERRMSG,2770) AALINE(1:MMLINE)
2770          FORMAT(A,'"&" is a continuation character and can only ',
     +        'occur at the beginning of an instruction line.')
              CALL WRTERR
            END IF
          END IF
        ELSE IF((CLINE(N1:N1).EQ.'w').OR.(CLINE(N1:N1).EQ.'W'))THEN
          IF(N1.NE.N2)THEN
            CALL ILLINS(CLINE(N1:N2))
          ELSE
            J1=J1+1
            IF(J1.GT.MAXWID) THEN
          WRITE(ERRMSG,2765) AALINE(1:MMLINE),
     +    AMXWID(:NBLNK(AMXWID))
              CALL WRTERR
              GO TO 2680
            END IF
          ENDIF
        ELSE IF((CLINE(N1:N1).EQ.'t').OR.(CLINE(N1:N1).EQ.'T'))THEN
          IF(N1.EQ.N2)THEN
            CALL ILLINS(CLINE(N1:N2))
            GO TO 2700
          END IF
          WRITE(FMT,2730)N2-N1
          READ(CLINE(N1+1:N2),FMT,ERR=2780) ITEMP
          GO TO 2790
2780      CALL ILLINS(CLINE(N1:N2))
          GO TO 2700
2790      IF(ITEMP.LE.0)THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2848)
2848        FORMAT('  (Number following "t" must be greater than ',
     +      'zero.)')
            CALL WRTERR
          ELSE IF(ITEMP.LT.J1)THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2800)
2800        FORMAT('  (Tab position results in backward move - model ',
     +      'output file must be read from left to right.)')
            CALL WRTERR
          ELSE IF(ITEMP.GT.MAXWID)THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2847) AMXWID(:NBLNK(AMXWID))
2847        FORMAT('  (Model output file width cannot exceed ',A,
     +      ' characters.)')
            CALL WRTERR
            GO TO 2680
          ELSE
            J1=ITEMP
          END IF
        ELSE IF((CLINE(N1:N1).EQ.'[').OR.(CLINE(N1:N1).EQ.'('))THEN
          AA=CLINE(N1:N1)
          IF(AA.EQ.'[')THEN
            N3=INDEX(CLINE(N1:N2),']')
          ELSE
            N3=INDEX(CLINE(N1:N2),')')
          END IF
          IF(N3.EQ.0)THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2810)
2810        FORMAT('  (Instruction contains unbalanced parentheses.)')
            CALL WRTERR
            GO TO 2700
          END IF
          N3=N3+N1-1
          IF((N3-1.GT.N1).AND.(N3-1-(N1+1)+1.LE.20))
     +    OTPAR=CLINE(N1+1:N3-1)
          CALL LOWCAS(OTPAR)
          IF(N3-1.EQ.N1)THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2815)
2815        FORMAT('  (Instruction missing observation name.)')
            CALL WRTERR
          ELSE IF(N3-1-(N1+1)+1.GT.20)THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2816)
2816        FORMAT('  (Observation name greater than 20 characters ',
     +      'long.)')
            CALL WRTERR
          ELSE IF((N3-1-(N1+1)+1.EQ.3).AND.(OTPAR.EQ.'dum'))THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2817)
2817        FORMAT('  (Dummy observation name "dum" only allowed for ',
     +      'non-fixed observations.)')
            CALL WRTERR
          ELSE
            OTPAR=CLINE(N1+1:N3-1)
            CALL WHICH1(IFAIL4,NOBS,IOBS,AOBS,OTPAR)
            IF(IFAIL4.NE.0)THEN
              DO 2825 J=1,NOBS
                IF(AOBS(J).EQ.' ')THEN
                  AOBS(J)=OTPAR
                  GO TO 2827
                END IF
2825          CONTINUE
              NEXOBS=NEXOBS+1
2827          NUMOBS(I)=NUMOBS(I)+1
            ELSE
              WRITE(ERRMSG,2830)AALINE(1:MMLINE),OTPAR(:NBLNK(OTPAR))
2830          FORMAT(A,'observation "',A,'" already cited in an ',
     +        'instruction file.')
              CALL WRTERR
            END IF
          END IF
          IF(N2.EQ.N3)THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2840)
            CALL WRTERR
            GO TO 2700
          END IF
          CALL GETNUM(IFAIL4,CLINE,N3,N2,NUM1,NUM2,FMT)
          IF(IFAIL4.NE.0)THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2840)
2840        FORMAT('  (Cannot read numbers in format "N1:N2" after ',
     +      'observation name.)')
            CALL WRTERR
          ELSE IF(NUM2.LE.NUM1)THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2845)
2845        FORMAT(
     +      '  (Numbers in format "N1:N2" following observation ',
     +      'name must be in increasing order.)')
            CALL WRTERR
          ELSE IF (NUM2.GT.MAXWID) THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2847) AMXWID(:NBLNK(AMXWID))
            CALL WRTERR
            GO TO 2680
          ELSE
            IF(NUM1.LT.J1)THEN
              WRITE(ERRMSG,2850) AALINE(1:MMLINE)
2850          FORMAT(A,'model output file must be read from left to ',
     +        'right.')
              CALL WRTERR
            END IF
            J1=NUM2
          END IF
        ELSE IF(CLINE(N1:N1).EQ.'!')THEN
          IF(N1.EQ.N2)THEN
            CALL ILLINS(CLINE(N1:N2))
            GO TO 2700
          END IF
          ITEMP=INDEX(CLINE(N1+1:N2),'!')
          IF(ITEMP.EQ.0)THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2860)
2860        FORMAT('  ("!"s not balanced.)')
            CALL WRTERR
            GO TO 2700
          END IF
          IF(CLINE(N2:N2).NE.'!')THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2870)
2870        FORMAT('  (Instruction must end in "!".)')
            CALL WRTERR
            GO TO 2700
          END IF
          IF(N1+1.EQ.N2)THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2815)
            CALL WRTERR
          ELSE IF(N2-1-(N1+1)+1.GT.20)THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2816)
            CALL WRTERR
          ELSE
            OTPAR=CLINE(N1+1:N2-1)
            CALL LOWCAS(OTPAR)
            IF(OTPAR.EQ.'dum') GO TO 2700
            CALL WHICH1(IFAIL4,NOBS,IOBS,AOBS,OTPAR)
            IF(IFAIL4.NE.0)THEN
              DO 2890 J=1,NOBS
                IF(AOBS(J).EQ.' ')THEN
                   AOBS(J)=OTPAR
                   GO TO 2895
                END IF
2890          CONTINUE
              NEXOBS=NEXOBS+1
2895          NUMOBS(I)=NUMOBS(I)+1
            ELSE
              WRITE(ERRMSG,2830) AALINE(1:MMLINE),OTPAR(:NBLNK(OTPAR))
              CALL WRTERR
            END IF
          END IF
        ELSE
          CALL ILLINS(CLINE(N1:N2))
        END IF
        GO TO 2700

2900    WRITE(ERRMSG,2310) AALINE(1:MMLINE)
        CALL WRTERR
        GO TO 2990
2950    IF(JLINE.EQ.1)THEN
          WRITE(ERRMSG,2410) AALINE(1:MMLINE)
          CALL WRTERR
        END IF
2990    CLOSE(UNIT=40)
3000    CONTINUE

        DO 3007 I=1,1000
          ILINE=ILINE+1
          CALL WRTINT(ATEMP,ILINE)
          ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
          AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
          MMLINE=NBLNK(AALINE)+1
          READ(30,'(A)',ERR=3010,END=3010) CLINE
          IF(CLINE.NE.' ')THEN
            WRITE(ERRMSG,3005) AALINE(1:MMLINE)
3005        FORMAT(A,'end of file expected here.')
            CALL WRTERR
            GO TO 9800
          END IF
3007    CONTINUE

3010    IF(IERR.EQ.NUMERR)THEN
        IF(NEXOBS.NE.0)THEN
          CALL WRTINT(ATEMP,NOBS)
          WRITE(ERRMSG,3020) ATEMP(1:NBLNK(ATEMP)),CASFL(1:MCASE)
3020      FORMAT(' NOBS supplied as ',A,' in file ',A,
     +    ': however more observations than this are cited in ',
     +    'instruction file(s).')
          CALL WRTERR
        ELSE
          J=0
          DO 3050 IOBS=1,NOBS
            IF(AOBS(IOBS).NE.' ') J=J+1
3050      CONTINUE
          IF(J.NE.NOBS)THEN
            CALL WRTINT(ATEMP,NOBS)
            CALL WRTINT(ATEMP1,J)
            WRITE(ERRMSG,3080) ATEMP(1:NBLNK(ATEMP)),
     +      CASFL(1:MCASE),ATEMP1(1:NBLNK(ATEMP1))
3080        FORMAT(' NOBS supplied as ',A,' in file ',A,
     +      ': however there are only ',A,' observations cited ',
     +      'in instruction file(s).')
            CALL WRTERR
          END IF
        END IF
        END IF

C -- NOW THAT ALL OF THE PEST CONTROL FILE HAS BEEN READ, FINAL CHECKS ARE
C -- MADE FOR NON-FATAL ERRORS

7999    IF(IERR.NE.0) THEN
          GO TO 9800
        ELSE
          WRITE(6,8000,ERR=9900)
8000      FORMAT(' No errors encountered.',/,/,' Warnings ----->')
        END IF

        DO 8310 I=1,NINFLE
        IF(NUMPAR(I).EQ.0) THEN
          IF(I.NE.1)THEN
            DO 8300 J=1,I-1
            IF(TMPFLE(I).EQ.TMPFLE(J)) GO TO 8310
8300        CONTINUE
          END IF
          WRITE(ERRMSG,8320) TMPFLE(I)(:NBLNK(TMPFLE(I)))
8320      FORMAT(' Template file ',A,' references no parameters.')
          CALL WRTERR
        END IF
8310    CONTINUE

        IF(NINFLE.GT.1) THEN
          DO 8350 I=1,NINFLE-1
          DO 8340 J=I+1,NINFLE
          IF(TMPFLE(I).EQ.TMPFLE(J))THEN
            IF(I.NE.1)THEN
              DO 8335 K=1,I-1
              IF(TMPFLE(K).EQ.TMPFLE(I)) GO TO 8350
8335          CONTINUE
            END IF
            WRITE(ERRMSG,8337) TMPFLE(I)(:NBLNK(TMPFLE(I)))
8337        FORMAT(' Template file ',A,' used for more than one model ',
     +      'input file.')
            CALL WRTERR
            GO TO 8350
          END IF
8340      CONTINUE
8350      CONTINUE
        END IF

        DO 8420 I=1,NOUTFL
        IF(NUMOBS(I).EQ.0) THEN
          WRITE(ERRMSG,8410) INSFLE(I)(:NBLNK(INSFLE(I)))
8410      FORMAT(' Instruction file ',A,' references no observations.')
          CALL WRTERR
        END IF
8420    CONTINUE

        IF(NOUTFL.GT.1) THEN
          DO 8450 I=1,NOUTFL-1
          DO 8440 J=I+1,NOUTFL
          IF(MODFLE(I).EQ.MODFLE(J))THEN
            IF(I.NE.1)THEN
              DO 8435 K=1,I-1
              IF(MODFLE(K).EQ.MODFLE(I)) GO TO 8450
8435          CONTINUE
            END IF
            WRITE(ERRMSG,8430) MODFLE(I)(:NBLNK(MODFLE(I)))
8430        FORMAT(' Model output file ',A,' read using more than one ',
     +      'instruction file.')
            CALL WRTERR
            GO TO 8450
          END IF
8440      CONTINUE
8450      CONTINUE
        END IF

        IF(IERR.EQ.0) WRITE(6,8999,ERR=9900)
8999    FORMAT(' No warnings.')
        GO TO 9998

9400    WRITE(6,9410,ERR=9900)
9410    FORMAT(' Insufficient memory to run SENSCHEK.')
        CALL EXIT(100)
9500    WRITE(AFIG,'(I5)') ILINE
        CALL SHIFTL(AFIG)
        WRITE(ERRMSG,9510) AFIG(:NBLNK(AFIG)),CASFL(1:MCASE)
9510    FORMAT(' Line ',A,' of file ',A,': insufficient data items.')
        CALL WRTERR
        CALL EXIT(100)
9600    WRITE(AFIG,'(I5)') ILINE
        CALL SHIFTL(AFIG)
        WRITE(ERRMSG,9610) AFIG(:NBLNK(AFIG)),CASFL(1:MCASE)
9610    FORMAT(' Line ',A,' of file ',A,': cannot read line.')
        CALL WRTERR
        CALL EXIT(100)
9700    WRITE(AFIG,'(I5)') ILINE
        CALL SHIFTL(AFIG)
        WRITE(ERRMSG,9710) AFIG(:NBLNK(AFIG)),CASFL(1:MCASE)
9710    FORMAT(' Line ',A,' of file ',A,': unexpected end to file.')
        CALL WRTERR
        CALL EXIT(100)
9800    CALL EXIT(100)
9900    CALL EXIT(100)

9998     CALL EXIT(0)

        END

