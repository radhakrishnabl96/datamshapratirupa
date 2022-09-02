        PROGRAM PARAMFIX

C -- PROGRAM PARAMFIX ACCOMODATES "HARD" OR "SOFT" PARAMETER FIXING AND ADJUSTS
C    PRIOR INFORMATION ACCORDINGLY.

        IMPLICIT NONE

        LOGICAL LEXIST,ISPAR
        INTEGER IFAIL,IERR,ILINE,NUMPAR,IIPAR,I,MPRIOR,J,MPR,
     +  NPAR,NOBS,NPRIOR,NPARGP,NOBSGP,IPAR,ITIED,ICOUNT,NOWRITE,
     +  MGROUP,IPH,ICONT,JLFLAG,IEND,IPRIOR,NB,NE,ILOG,NBB,NEE,IPR,
     +  SIGN,ISTAR
        INTEGER NBLNK
        INTEGER LW(20),RW(20)
        DOUBLE PRECISION PRVAL,VALCALC,RHS,RHSCALC,LBOUND,UBOUND
        CHARACTER*150 FIXFILE,PESTFILE1,PESTFILE2,TMPFLE
        CHARACTER*12 AAPAR
        CHARACTER*12 AVERSION
        CHARACTER*20 APRIOR
        CHARACTER*6 ALINE,ATRANS,APOS2,ANUM
        CHARACTER*15 APOS3,APRVAL,AWEIGHT
        CHARACTER*20 ATEMP,PESTMODE
        CHARACTER*1000 CLINE,DLINE,ELINE
        CHARACTER*2120 ERRMSG

        INTEGER FIXPR(:),LOGPR(:),ITRANS(:),COVFLE(:)
        DOUBLE PRECISION VALUE(:),WEIGHT(:)
        CHARACTER*12 APAR(:),OBSGP(:),AOBSGP(:),EXGP(:)

        ALLOCATABLE::FIXPR,LOGPR,VALUE,WEIGHT,APAR,OBSGP,ITRANS,
     +  AOBSGP,COVFLE,EXGP

        COMMON /ERRMSG/ERRMSG


        INCLUDE 'version.inc'
        WRITE(6,10) TRIM(AVERSION)
10      FORMAT(' PARAMFIX Version ',A,'. Watermark Numerical ',
     +  'Computing.')
#ifdef NO_CMDLINE
        WRITE(*,*)
1       WRITE(*,'(A)') ' ENTER COMMAND LINE ARGUMENTS > '
        READ(*,'(A)',ERR=1) CLINE
#else
        CALL PGETCL (CLINE)
#endif

        CALL SPACESUB(CLINE)
        CALL LINSPL(IFAIL,4,LW,RW,CLINE)
        IF(IFAIL.EQ.0) GO TO 9000
        CALL LINSPL(IFAIL,3,LW,RW,CLINE)
        IF(IFAIL.NE.0) GO TO 9000
        FIXFILE=CLINE(LW(1):RW(1))
        PESTFILE1=CLINE(LW(2):RW(2))
        PESTFILE2=CLINE(LW(3):RW(3))
        CALL REMCHAR(FIXFILE,CHAR(211))
        CALL REMCHAR(PESTFILE1,CHAR(211))
        CALL REMCHAR(PESTFILE2,CHAR(211))
#ifdef CAPFILE
        CALL UPCAS(FIXFILE)
        CALL UPCAS(PESTFILE1)
        CALL UPCAS(PESTFILE2)
#endif

C -- VARIABLE INITIALISATION

        ITIED=0
        TMPFLE='t###.tmp'
15      FORMAT(A)
16      FORMAT(A)

        IF(PESTFILE1.EQ.PESTFILE2)THEN
          WRITE(ERRMSG,40)
40        FORMAT(' Input and output PEST control files cannot have ',
     +    'same name.')
          GO TO 9900
        END IF

        OPEN(UNIT=10,FILE=FIXFILE,STATUS='OLD',IOSTAT=IERR)
        IF(IERR.NE.0)THEN
          WRITE(ERRMSG,50) FIXFILE(1:NBLNK(FIXFILE))
50        FORMAT(' Cannot open parameter fix file ',A,'.')
          GO TO 9900
        END IF
        OPEN(UNIT=11,FILE=PESTFILE1,STATUS='OLD',IOSTAT=IERR)
        IF(IERR.NE.0) THEN
          WRITE(ERRMSG,60) PESTFILE1(1:NBLNK(PESTFILE1))
60        FORMAT(' Cannot open PEST control file ',A,'.')
          GO TO 9900
        END IF
        OPEN(UNIT=20,FILE=TMPFLE,IOSTAT=IERR)
        IF(IERR.NE.0)THEN
          WRITE(ERRMSG,61) TMPFLE(1:NBLNK(TMPFLE))
61        FORMAT(' Cannot open temporary file ',A,' for output.')
          GO TO 9900
        END IF

C -- THE PARAMETER FIX FILE IS READ A FIRST TIME TO ESTABLISH NUMBER OF
C    PARAMETERS.

        WRITE(6,120) FIXFILE(1:NBLNK(FIXFILE))
120     FORMAT(/,' Reading parameter fix file ',A,' ....')
        ILINE=0
130     CONTINUE
          ILINE=ILINE+1
          READ(10,*,ERR=9100,END=150)
        GO TO 130
150     CONTINUE

        ALLOCATE(APAR(ILINE),FIXPR(ILINE),VALUE(ILINE),LOGPR(ILINE),
     +  WEIGHT(ILINE),OBSGP(ILINE),ITRANS(ILINE),EXGP(ILINE),STAT=IERR)
        IF(IERR.NE.0)THEN
          WRITE(ERRMSG,170)
170        FORMAT(' Cannot allocate sufficient memory to continue ',
     +    'execution.')
          GO TO 9900
        END IF

        DO 160 I=1,ILINE
          ITRANS(I)=-9999999
160     CONTINUE
        REWIND(UNIT=10,IOSTAT=IERR)
        IF(IERR.NE.0)THEN
          WRITE(ERRMSG,190) FIXFILE(1:NBLNK(FIXFILE))
190       FORMAT(' Cannot rewind parameter fix file ',A,'.')
          GO TO 9900
        END IF

C -- THE PARAMETER FIX FILE IS NOW PERUSED A SECOND TIME AND PARAMETER
C    DATA ARE READ.

        MPRIOR=0
        ILINE=0
        NUMPAR=0
250     ILINE=ILINE+1
        CALL WRTINT(ALINE,ILINE)
        READ(10,15,ERR=9100,END=400) CLINE
        CALL LINSPL(IFAIL,4,LW,RW,CLINE)
        IF(IFAIL.LT.0) GO TO 250
        IF(IFAIL.NE.0)THEN
          WRITE(ERRMSG,255) ALINE(1:NBLNK(ALINE)),
     +    FIXFILE(1:NBLNK(FIXFILE))
255       FORMAT(' Insufficient entries on line ',A,' of ',
     +    'file ',A,'.')
          GO TO 9900
        END IF
        CALL LOWCAS(CLINE)
        IF(RW(1)-LW(1)+1.GT.12)THEN
          WRITE(ERRMSG,260) ALINE(1:NBLNK(ALINE)),
     +    FIXFILE(1:NBLNK(FIXFILE))
260       FORMAT(' Parameter name greater than 12 characters',
     +    ' at line ',A,' of file ',A,'.')
          GO TO 9900
        END IF
        NUMPAR=NUMPAR+1
        APAR(NUMPAR)=CLINE(LW(1):RW(1))
        ATEMP=CLINE(LW(2):RW(2))
        IF(ATEMP(1:3).EQ.'fix')THEN
          FIXPR(NUMPAR)=1
        ELSE IF(ATEMP(1:5).EQ.'prior')THEN
          FIXPR(NUMPAR)=2
          MPRIOR=MPRIOR+1
        ELSE
          WRITE(ERRMSG,270) ALINE(1:NBLNK(ALINE)),
     +    FIXFILE(1:NBLNK(FIXFILE))
270       FORMAT(' Second entry at line ',A,' of file ',A,
     +    ' should be "fix_param" or "prior_info".')
          GO TO 9900
        END IF
        CALL DREALRD(IFAIL,CLINE(LW(3):RW(3)),VALUE(NUMPAR))
        IF(IFAIL.NE.0)THEN
          WRITE(ERRMSG,280) ALINE(1:NBLNK(ALINE)),
     +    FIXFILE(1:NBLNK(FIXFILE))
280       FORMAT(' Cannot read parameter value from ',
     +    'line ',A,' of file ',A,'.')
          GO TO 9900
        END IF
        ATEMP=CLINE(LW(4):RW(4))
        IF(FIXPR(NUMPAR).EQ.1)THEN
          IF(ATEMP(1:6).EQ.'retain')THEN
            LOGPR(NUMPAR)=1
          ELSE IF(ATEMP(1:6).EQ.'remove')THEN
            LOGPR(NUMPAR)=2
          ELSE
            WRITE(ERRMSG,290) ALINE(1:NBLNK(ALINE)),
     +      FIXFILE(1:NBLNK(FIXFILE))
290         FORMAT(' Parameter is fixed at line ',A,' of file ',A,
     +      ' so 4th entry should be "retain_prior" or ',
     +      '"remove_prior".')
            GO TO 9900
          END IF
        ELSE
          IF(ATEMP(1:3).EQ.'log')THEN
            LOGPR(NUMPAR)=1
            IF(VALUE(NUMPAR).LE.0.0)THEN
              WRITE(ERRMSG,295) APAR(NUMPAR)(1:NBLNK(APAR(NUMPAR))),
     +        FIXFILE(1:NBLNK(FIXFILE))
295           FORMAT(' Parameter "',A,'" cannot be log transformed ',
     +        'in prior information as requested in file ',A,
     +        ' because the supplied parameter value is non-positive.')
              GO TO 9900
            END IF
          ELSE IF(ATEMP(1:2).EQ.'no')THEN
            LOGPR(NUMPAR)=2
          ELSE
            WRITE(ERRMSG,297) ALINE(1:NBLNK(ALINE)),
     +      FIXFILE(1:NBLNK(FIXFILE))
297         FORMAT(' Fourth entry on line ',A,' of file ',A,' must be ',
     +      '"log" or "none".')
            GO TO 9900
          END IF
        END IF
        IF(FIXPR(NUMPAR).EQ.2)THEN
          CALL LINSPL(IFAIL,6,LW,RW,CLINE)
          IF(IFAIL.NE.0)THEN
            WRITE(ERRMSG,255) ALINE(1:NBLNK(ALINE)),
     +      FIXFILE(1:NBLNK(FIXFILE))
            GO TO 9900
          END IF
          CALL DREALRD(IFAIL,CLINE(LW(5):RW(5)),WEIGHT(NUMPAR))
          IF(IFAIL.NE.0)THEN
            WRITE(ERRMSG,310) ALINE(1:NBLNK(ALINE)),
     +      FIXFILE(1:NBLNK(FIXFILE))
310         FORMAT(' Cannot read weight from ',
     +      'line ',A,' of file ',A,'.')
            GO TO 9900
          END IF
          IF(WEIGHT(NUMPAR).LT.0.0)THEN
            WRITE(ERRMSG,320) ALINE(1:NBLNK(ALINE)),
     +      FIXFILE(1:NBLNK(FIXFILE))
320         FORMAT(' Weight is negative at line ',A,
     +      ' of file ',A,'.')
            GO TO 9900
          END IF
          IF(RW(6)-LW(6)+1.GT.12)THEN
            WRITE(ERRMSG,330) ALINE(1:NBLNK(ALINE)),
     +      FIXFILE(1:NBLNK(FIXFILE))
330         FORMAT(' Observation group name greater than 12 ',
     +      'characters at line ',A,' of file ',A,'.')
            GO TO 9900
          END IF
          OBSGP(NUMPAR)=CLINE(LW(6):RW(6))
        END IF
        GO TO 250

400     CLOSE(UNIT=10)

C -- A CHECK IS MADE THAT THE SAME PARAMETER HAS NOT BEEN CITED TWICE IN THE
C    PARAMETER FIX FILE.

        IF(NUMPAR.GT.1)THEN
          DO 410 I=1,NUMPAR
            DO 408 J=I+1,NUMPAR
              IF(APAR(I).EQ.APAR(J))THEN
                WRITE(ERRMSG,405) APAR(I)(1:NBLNK(APAR(I))),
     +          FIXFILE(1:NBLNK(FIXFILE))
405             FORMAT(' Parameter "',A,'" has been cited more ',
     +          'than once in parameter fix file ',A,'.')
                GO TO 9900
              END IF
408         CONTINUE
410       CONTINUE
        END IF

        CALL WRTINT(ALINE,NUMPAR)
        WRITE(6,420) ALINE(1:NBLNK(ALINE)), FIXFILE(1:NBLNK(FIXFILE))
420     FORMAT(' - data for ',A,' parameters read from file ',A,'.')
        WRITE(6,450) PESTFILE1(1:NBLNK(PESTFILE1)),
     +  PESTFILE2(1:NBLNK(PESTFILE2))
450     FORMAT(/,' Reading file ',A,' and writing file ',A,' ....')

        IIPAR=0
        ILINE=1
        READ(11,15,END=9200,ERR=9400) CLINE
        DLINE=CLINE
        CALL LOWCAS(DLINE)
        CALL SHIFTL(DLINE)
        IF(DLINE(1:4).NE.'pcf ')THEN
          WRITE(ERRMSG,460) PESTFILE1(1:NBLNK(PESTFILE1))
460       FORMAT(' First line of file ',A,' should be "pcf".')
          GO TO 9900
        END IF
        WRITE(20,16,ERR=9300) CLINE(1:NBLNK(CLINE))

C -- WE READ A LITTLE INFORMATION FROM THE FIRST FEW LINES OF THE PEST
C -- CONTROL FILE

        ILINE=ILINE+1
        READ(11,15,END=9200,ERR=9400) CLINE
        WRITE(20,16,ERR=9300) CLINE(1:NBLNK(CLINE))
        ILINE=ILINE+1
        READ(11,15,END=9200,ERR=9400) CLINE
        CALL LINSPL(IFAIL,2,LW,RW,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL WRTINT(ALINE,ILINE)
          WRITE(ERRMSG,255) ALINE(1:NBLNK(ALINE)),
     +    PESTFILE1(1:NBLNK(PESTFILE1))
          GO TO 9900
        END IF
        PESTMODE=CLINE(LW(2):RW(2))
        CALL LOWCAS(PESTMODE)
        IF((PESTMODE(1:5).NE.'estim').AND.
     +     (PESTMODE(1:5).NE.'predi').AND.
     +     (PESTMODE(1:5).NE.'regul')) THEN
           CALL WRTINT(ALINE,ILINE)
           WRITE(ERRMSG,455) ALINE(1:NBLNK(ALINE)),
     +     PESTFILE1(1:NBLNK(PESTFILE1))
455        FORMAT(' Unknown PESTMODE value at line ',A,' of file ',
     +     A,'.')
           GO TO 9900
        END IF
        WRITE(20,16,ERR=9300) CLINE(1:NBLNK(CLINE))
        ILINE=ILINE+1
        READ(11,15,END=9200,ERR=9400) CLINE
        CALL WRTINT(ALINE,ILINE)
        CALL LINSPL(IFAIL,5,LW,RW,CLINE)
        IF(IFAIL.NE.0)THEN
          WRITE(ERRMSG,255) ALINE(1:NBLNK(ALINE)),
     +    PESTFILE1(1:NBLNK(PESTFILE1))
          GO TO 9900
        END IF
        CALL INTRD(IFAIL,CLINE(LW(1):RW(1)),NPAR)
        IF(IFAIL.NE.0)THEN
          WRITE(ERRMSG,454) ALINE(1:NBLNK(ALINE)),
     +    PESTFILE1(1:NBLNK(PESTFILE1))
454       FORMAT(' Error reading NPAR from line ',A,' of file ',A,'.')
          GO TO 9900
        END IF
        CALL INTRD(IFAIL,CLINE(LW(2):RW(2)),NOBS)
        IF(IFAIL.NE.0)THEN
          CALL WRTINT(ALINE,ILINE)
          WRITE(ERRMSG,456) ALINE(1:NBLNK(ALINE)),
     +    PESTFILE1(1:NBLNK(PESTFILE1))
456       FORMAT(' Error reading NOBS from line ',A,' of file ',A,'.')
          GO TO 9900
        END IF
        CALL INTRD(IFAIL,CLINE(LW(3):RW(3)),NPARGP)
        IF(IFAIL.NE.0)THEN
          WRITE(ERRMSG,457) ALINE(1:NBLNK(ALINE)),
     +    PESTFILE1(1:NBLNK(PESTFILE1))
457       FORMAT(' Error reading NPARGP from line ',A,' of file ',
     +    A,'.')
          GO TO 9900
        END IF
        CALL INTRD(IFAIL,CLINE(LW(4):RW(4)),NPRIOR)
        IF(IFAIL.NE.0)THEN
          WRITE(ERRMSG,462) ALINE(1:NBLNK(ALINE)),
     +    PESTFILE1(1:NBLNK(PESTFILE1))
462       FORMAT(' Error reading NPRIOR from line ',A,' of file ',A,'.')
          GO TO 9900
        END IF
        CALL INTRD(IFAIL,CLINE(LW(5):RW(5)),NOBSGP)
        IF(IFAIL.NE.0)THEN
          WRITE(ERRMSG,463) ALINE(1:NBLNK(ALINE)),
     +    PESTFILE1(1:NBLNK(PESTFILE1))
463       FORMAT(' Error reading NOBSGP from line ',A,' of file ',A,'.')
          GO TO 9900
        END IF
        WRITE(20,16,ERR=9300) CLINE(1:NBLNK(CLINE))

        ALLOCATE(AOBSGP(NOBSGP),COVFLE(NOBSGP),STAT=IERR)
        IF(IERR.NE.0)THEN
          WRITE(ERRMSG,170)
          GO TO 9900
        END IF

470     CONTINUE
          ILINE=ILINE+1
          READ(11,15,ERR=9400,END=9200) CLINE
          IF(CLINE(1:1).EQ.'*') THEN
            DLINE=CLINE
            CALL LOWCAS(DLINE)
            IF(INDEX(DLINE,'parameter').NE.0) THEN
              IF(INDEX(DLINE,'data').NE.0)THEN
                IIPAR=1
              END IF
            END IF
          END IF
          WRITE(20,16,ERR=9300) CLINE(1:NBLNK(CLINE))
          IF(IIPAR.EQ.1) GO TO 500
        GO TO 470

C -- THE "* PARAMETER DATA" OF THE PEST CONTROL FILE IS READ.

500     CONTINUE
        DO 700 IPAR=1,NPAR
520       ILINE=ILINE+1
          CALL WRTINT(ALINE,ILINE)
          READ(11,15,ERR=9400,END=9500) CLINE
          CALL LINSPL(IFAIL,9,LW,RW,CLINE)
          IF(IFAIL.LT.0) GO TO 520
          IF(IFAIL.NE.0)THEN
            WRITE(ERRMSG,255) ALINE(1:NBLNK(ALINE)),
     +      PESTFILE1(1:NBLNK(PESTFILE1))
            GO TO 9900
          END IF
          IF(RW(1)-LW(1)+1.GT.12)THEN
            WRITE(ERRMSG,550) ALINE(1:NBLNK(ALINE)),
     +      PESTFILE1(1:NBLNK(PESTFILE1))
550         FORMAT(' Parameter name greater than 12 characters at ',
     +      'line ',A,' of file ',A,'.')
            GO TO 9900
          END IF
          AAPAR=CLINE(LW(1):RW(1))
          CALL LOWCAS(AAPAR)
          DO 600 I=1,NUMPAR
            IF(AAPAR.EQ.APAR(I)) THEN
              ATRANS=CLINE(LW(2):RW(2))
              CALL LOWCAS(ATRANS)
              IF(ATRANS.EQ.'log')THEN
                ITRANS(I)=1
              ELSE IF(ATRANS.EQ.'none')THEN
                ITRANS(I)=0
              ELSE IF(ATRANS.EQ.'tied')THEN
                ITRANS(I)=-1
                ITIED=ITIED+1
              ELSE IF(ATRANS.EQ.'fixed')THEN
                ITRANS(I)=-1000000
              ELSE
                WRITE(ERRMSG,560) ALINE(1:NBLNK(ALINE)),
     +          PESTFILE1(1:NBLNK(PESTFILE1))
560             FORMAT(' Second entry on line ',A,' of file ',A,
     +          ' should be "tied", "fixed", "none" of "log".')
                GO TO 9900
              END IF
              CALL DREALRD(IFAIL,CLINE(LW(5):RW(5)),LBOUND)
              IF(IFAIL.NE.0)THEN
                WRITE(ERRMSG,561) ALINE(1:NBLNK(ALINE)),
     +          PESTFILE1(1:NBLNK(PESTFILE1))
561             FORMAT(' Cannot read parameter lower bound from ',
     +          'line ',A,' of file ',A,'.')
                GO TO 9900
              END IF
              CALL DREALRD(IFAIL,CLINE(LW(6):RW(6)),UBOUND)
              IF(IFAIL.NE.0)THEN
                WRITE(ERRMSG,562) ALINE(1:NBLNK(ALINE)),
     +          PESTFILE1(1:NBLNK(PESTFILE1))
562             FORMAT(' Cannot read parameter upper bound from ',
     +          'line ',A,' of file ',A,'.')
                GO TO 9900
              END IF
              IF((VALUE(I).GT.UBOUND).OR.(VALUE(I).LT.LBOUND))THEN
                WRITE(ERRMSG,563) AAPAR(1:NBLNK(AAPAR)),
     +          FIXFILE(1:NBLNK(FIXFILE)),
     +          PESTFILE1(1:NBLNK(PESTFILE1))
563             FORMAT(' Value supplied for parameter "',A,
     +          '" in parameter fix file ',A,' is out of range as ',
     +          'defined by the bounds for this parameter supplied ',
     +          'in the PEST control file ',A,'.')
                GO TO 9900
              END IF
              IF(FIXPR(I).EQ.1)THEN
                APOS2='fixed'
                WRITE(APOS3,'(1PG14.7)') VALUE(I)
                CALL SHIFTL(APOS3)
                WRITE(20,565,ERR=9300) CLINE(1:RW(1)),
     +          APOS2(1:NBLNK(APOS2)),
     +          CLINE(LW(3):RW(3)),
     +          APOS3(1:NBLNK(APOS3)),CLINE(LW(5):NBLNK(CLINE))
565             FORMAT(A,2X,A,2X,A,2X,A,2X,A)
                ITRANS(I)=-1000001
                GO TO 700
              ELSE IF(FIXPR(I).EQ.2)THEN
                IF(ITRANS(I).LT.0)THEN
                  WRITE(ERRMSG,570) AAPAR(1:NBLNK(AAPAR)),
     +            PESTFILE1(1:NBLNK(PESTFILE1)),
     +            FIXFILE(1:NBLNK(FIXFILE))
570               FORMAT(' Parameter "',A,'" is tied or fixed in ',
     +            'PEST control file ',A,'. Hence it cannot be ',
     +            'involved in prior information as requested in ',
     +            'parameter fix file ',A,'.')
                  GO TO 9900
                END IF
                IF(ITRANS(I).EQ.1)THEN
                  IF(LOGPR(I).NE.1)THEN
                    WRITE(ERRMSG,580) AAPAR(1:NBLNK(AAPAR)),
     +              PESTFILE1(1:NBLNK(PESTFILE1)),
     +              FIXFILE(1:NBLNK(FIXFILE))
580                 FORMAT(' Parameter "',A,'" is log transformed ',
     +              'in PEST control file ',A,'. Hence its log ',
     +              '(rather than untransformed value) must be ',
     +              'involved in prior information requested in ',
     +              'parameter fix file ',A,'.')
                    GO TO 9900
                  END IF
                ELSE IF(ITRANS(I).EQ.0)THEN
                  IF(LOGPR(I).EQ.1)THEN
                    WRITE(ERRMSG,590) AAPAR(1:NBLNK(AAPAR)),
     +              PESTFILE1(1:NBLNK(PESTFILE1)),
     +              FIXFILE(1:NBLNK(FIXFILE))
590                 FORMAT(' Parameter "',A,'" is untransformed in ',
     +              'PEST control file ',A,'. Hence its log cannot ',
     +              'be involved in prior information as requested ',
     +              'in parameter fix file ',A,'.')
                    GO TO 9900
                  END IF
                END IF
                WRITE(20,16,ERR=9300) CLINE(1:NBLNK(CLINE))
                GO TO 700
              END IF
            END IF
600       CONTINUE
          ATRANS=CLINE(LW(2):RW(2))
          CALL LOWCAS(ATRANS)
          IF(ATRANS.EQ.'tied')THEN
            ITIED=ITIED+1
          END IF
          WRITE(20,16,ERR=9300) CLINE(1:NBLNK(CLINE))
700     CONTINUE

C -- IF ANY PARAMETER FROM THE PARAMETER FIX FILE IS NOT CITED IN THE
C    PEST CONTROL FILE, PARAMFIX CEASES EXECUTION WITH AN ERROR MESSAGE.

        ICOUNT=0
        DO 650 I=1,NUMPAR
          IF(ITRANS(I).EQ.-9999999)THEN
            IF(ICOUNT.EQ.0) WRITE(6,*)
            ICOUNT=ICOUNT+1
            WRITE(ERRMSG,610) APAR(I)(1:NBLNK(APAR(I)))
610         FORMAT(' Parameter "',A,'" in parameter fix file ',
     +      'not cited in PEST control file.')
            CALL WRTERR
          END IF
650    CONTINUE
       IF(ICOUNT.NE.0) GO TO 9995

C -- NOW WE HANDLE THE TIED PARAMETERS.

        IF(ITIED.NE.0)THEN
          DO 850 I=1,ITIED
            NOWRITE=0
655         ILINE=ILINE+1
            READ(11,15,ERR=9400,END=9500) CLINE
            CALL LINSPL(IFAIL,2,LW,RW,CLINE)
            IF(IFAIL.LT.0) GO TO 655
            IF(IFAIL.GT.0)THEN
              CALL WRTINT(ALINE,ILINE)
              WRITE(ERRMSG,255) ALINE(1:NBLNK(ALINE)),
     +        PESTFILE1(1:NBLNK(PESTFILE1))
              GO TO 9900
            END IF
            AAPAR=CLINE(LW(1):RW(1))
            CALL LOWCAS(AAPAR)
            DO 680 J=1,NUMPAR
              IF(AAPAR.EQ.APAR(J)) THEN
                IF(ITRANS(J).EQ.-1000001) THEN
                  NOWRITE=1
                  GO TO 690
                END IF
              END IF
680         CONTINUE
690         CONTINUE
            AAPAR=CLINE(LW(2):RW(2))
            CALL LOWCAS(AAPAR)
            IF(NOWRITE.EQ.0)THEN
              DO 695 J=1,NUMPAR
                IF(AAPAR.EQ.APAR(J))THEN
                  IF(ITRANS(J).EQ.-1000001)THEN
                    CALL WRTINT(ALINE,ILINE)
                    WRITE(ERRMSG,686) AAPAR(1:NBLNK(AAPAR)),
     +              FIXFILE(1:NBLNK(FIXFILE)),ALINE(1:NBLNK(ALINE)),
     +              PESTFILE1(1:NBLNK(PESTFILE1))
686                 FORMAT(' Parameter "',A,'" has been designated as ',
     +              'fixed in parameter fix file ',A,'. However ',
     +              'an unfixed parameter is tied to it at line ',
     +              A,' of PEST control file ',A,'.')
                    GO TO 9900
                  END IF
                END IF
695           CONTINUE
            END IF
            IF(NOWRITE.EQ.0) WRITE(20,16,ERR=9300) CLINE(1:NBLNK(CLINE))
850       CONTINUE
        END IF

C -- NEXT THE NAMES OF EXISTING OBSERVATION GROUPS ARE READ FROM THE PEST
C    CONTROL FILE

710     CONTINUE
        ILINE=ILINE+1
        READ(11,15,ERR=9400,END=9500) CLINE
        DLINE=CLINE
        CALL LOWCAS(DLINE)
        IF(INDEX(DLINE,'* observation gr').EQ.0)THEN
          CALL WRTINT(ALINE,ILINE)
          WRITE(ERRMSG,715) ALINE(1:NBLNK(ALINE)),
     +    PESTFILE1(1:NBLNK(PESTFILE1))
715       FORMAT(' Line ',A,' of file ',A,' should be "',
     +    '* observation groups"')
          GO TO 9900
        END IF
        WRITE(20,16,ERR=9300) CLINE(1:NBLNK(CLINE))
        DO 720 I=1,NOBSGP
716       ILINE=ILINE+1
          READ(11,15,ERR=9400,END=9500) CLINE
          CALL LINSPL(IFAIL,1,LW,RW,CLINE)
          IF(IFAIL.LT.0) GO TO 716
          AOBSGP(I)=CLINE(LW(1):RW(1))
          CALL LOWCAS(AOBSGP(I))
          CALL LINSPL(IFAIL,2,LW,RW,CLINE)
          IF(IFAIL.NE.0) THEN
            COVFLE(I)=0
          ELSE
            COVFLE(I)=1
          END IF
          WRITE(20,16,ERR=9300) CLINE(1:NBLNK(CLINE))
720     CONTINUE

C -- NEXT WE DETERMINE HOW MANY ADDITIONAL OBSERVATION GROUPS WILL BE REQUIRED.

        MGROUP=0
        DO 730 I=1,NUMPAR
          IF(FIXPR(I).EQ.2)THEN
            DO 725 J=1,NOBSGP
              IF(OBSGP(I).EQ.AOBSGP(J)) GO TO 730
725         CONTINUE
            IF(MGROUP.EQ.0)THEN
              MGROUP=MGROUP+1
              EXGP(MGROUP)=OBSGP(I)
            ELSE
              DO 728 J=1,MGROUP
                IF(OBSGP(I).EQ.EXGP(J)) GO TO 730
728           CONTINUE
              MGROUP=MGROUP+1
              EXGP(MGROUP)=OBSGP(I)
            END IF
          END IF
730     CONTINUE

C -- NEXT WE DETERMINE IF ANY EXISTING OBSERVATION GROUPS TO WHICH WE ADD PRIOR
C    INFORMATION HAVE A COVARIANCE FILE ASSIGNED.

        DO 740 I=1,NUMPAR
          IF(FIXPR(I).EQ.2)THEN
            DO 735 J=1,NOBSGP
              IF(OBSGP(I).EQ.AOBSGP(J))THEN
                IF(COVFLE(J).EQ.1)THEN
                  WRITE(ERRMSG,732) APAR(I)(1:NBLNK(APAR(I))),
     +            FIXFILE(1:NBLNK(FIXFILE)),OBSGP(I)(1:NBLNK(OBSGP(I))),
     +            PESTFILE1(1:NBLNK(PESTFILE1))
732               FORMAT(' Prior information has been requested for ',
     +            'parameter "',A,'" in file ',A,'. This prior ',
     +            'information has been assigned to observation ',
     +            'group "',A,'". However this ',
     +            'is not permitted, as this observation group has ',
     +            'been assigned a covariance matrix file in the ',
     +            'PEST control file ',A,'.')
                  GO TO 9900
                END IF
                GO TO 740
              END IF
735         CONTINUE
          END IF
740     CONTINUE

C -- THE EXTRA GROUPS ARE NOW ADDED TO THE PEST CONTROL FILE

        DO 745 I=1,MGROUP
          WRITE(20,16,ERR=9300) EXGP(I)(1:NBLNK(EXGP(I)))
745     CONTINUE

C -- WE NOW SEARCH FOR THE PRIOR INFORMATION SECTION OF THE PEST CONTROL FILE.

        IPH=0
        ICONT=0
705     CONTINUE
          ILINE=ILINE+1
          READ(11,15,ERR=9400,END=750) CLINE
          IF(CLINE.EQ.' ') GO TO 705
          IF(CLINE(1:1).EQ.'*')THEN
            DLINE=CLINE
            CALL LOWCAS(DLINE)
            IF(INDEX(DLINE,'prior information').NE.0) THEN
              IPH=1
              GO TO 750
            ELSE IF((INDEX(DLINE,'regul').NE.0).OR.
     +         (INDEX(DLINE,'predict').NE.0)) THEN
               ICONT=1
               ELINE=CLINE
               GO TO 750
            END IF
          END IF
          WRITE(20,16,ERR=9300) CLINE(1:NBLNK(CLINE))
        GO TO 705
750     CONTINUE

C -- THE PRIOR INFORMATION SECTION OF THE PEST CONTROL FILE IS NOW READ.

        MPR=MPRIOR
        MPRIOR=0
        IF(IPH.EQ.0)THEN
          IF(ICONT.EQ.1)THEN
            IF(NPRIOR.NE.0)THEN
              WRITE(ERRMSG,952) PESTFILE1(1:NBLNK(PESTFILE1))
952           FORMAT(' NPRIOR in "control data" section of PEST ',
     +        'control file ',A,' is non-zero. However there does ',
     +        'not appear to be any prior information in this file.')
              GO TO 9900
            END IF
          END IF
          IF(MPR.EQ.0)THEN
            IF(ICONT.EQ.1) THEN
              GO TO 1100
            ELSE
              GO TO 1200
            END IF
          ELSE
            WRITE(20,951,ERR=9300)
951         FORMAT('* prior information')
            GO TO 905
          END IF
        ELSE IF(IPH.EQ.1)THEN
          IF((NPRIOR.NE.0).OR.(MPR.NE.0))THEN
            WRITE(20,951,ERR=9300)
            IF(NPRIOR.EQ.0) GO TO 905
          ELSE
            GO TO 1100
          END IF
        END IF

        MPRIOR=0
        IEND=0
        IF(NPRIOR.EQ.0) GO TO 905
        JLFLAG=1
759     ILINE=ILINE+1
        READ(11,15,ERR=9400,END=9500) CLINE
        CALL SHIFTL(CLINE)
        IF(CLINE.EQ.' ') GO TO 759
        DO 900 IPRIOR=1,NPRIOR
760       ILINE=ILINE+1
          READ(11,15,ERR=9400,END=770) DLINE
          IF(DLINE.EQ.' ') GO TO 760
          GO TO 775
770       IF(IPRIOR.NE.NPRIOR) THEN
            GO TO 9500
          ELSE
            IEND=1
            DLINE=' '
          END IF
775       CALL SHIFTL(DLINE)
          IF(DLINE(1:1).EQ.'&')THEN
            CLINE=CLINE(1:NBLNK(CLINE))//DLINE(2:NBLNK(DLINE))
            GO TO 760
          END IF
          IF(CLINE(1:1).EQ.'*')THEN
            WRITE(ERRMSG,776) PESTFILE1(1:NBLNK(PESTFILE1))
776         FORMAT(' Less than expected prior information equations ',
     +      'in file ',A,'.')
            GO TO 9900
          END IF
          CALL LOWCAS(CLINE)
          DO 780 I=1,NUMPAR
            IF(FIXPR(I).EQ.1)THEN
              IF(ISPAR(IFAIL,APAR(I),CLINE,NB,NE,ILOG))GO TO 790
              IF(IFAIL.NE.0) GO TO 9850
            END IF
780       CONTINUE
          WRITE(20,16,ERR=9300) CLINE(1:NBLNK(CLINE))
          GO TO 890
790       IF(LOGPR(I).EQ.2) THEN
            MPRIOR=MPRIOR-1
            GO TO 890
          END IF
          PRVAL=VALCALC(IFAIL,CLINE,VALUE(I),NB,NE,ILOG,SIGN)
          IF(IFAIL.NE.0) GO TO 9850
          IF(SIGN.EQ.0)THEN
            CALL SIGNADJ(IFAIL,NB,NE,CLINE)
            IF(IFAIL.NE.0) GO TO 9850
          END IF
          RHS=RHSCALC(IFAIL,CLINE,NBB,NEE)
          IF(IFAIL.NE.0) GO TO 9850
          RHS=RHS-PRVAL
          WRITE(APOS3,'(1PG14.7)')RHS
          CALL SHIFTL(APOS3)
          CLINE=CLINE(1:NB-1)//CLINE(NE+2:NBB-1)//
     +    APOS3(1:NBLNK(APOS3))//CLINE(NEE+1:NBLNK(CLINE))
          ISTAR=INDEX(CLINE,' * ')
          IF(ISTAR.EQ.0)THEN
            MPRIOR=MPRIOR-1
            GO TO 890
          END IF
          GO TO 775
890       CLINE=DLINE
900     CONTINUE
        IF(IEND.EQ.1) JLFLAG=0   ! THERE IS NO LINE TO BE WRITTEN.

C -- AT THIS STAGE EXISTING PRIOR INFORMATION LINES HAVE BEEN MODIFIED TO
C    ACCOUNT FOR NEWLY FIXED PARAMETERS. NEXT EXTRA PRIOR INFORMATION LINES
C    ARE ADDED.

905     IPR=0
        DO 1000 I=1,NUMPAR
          IF(FIXPR(I).EQ.2)THEN
            IF((LOGPR(I).EQ.1).AND.(ITRANS(I).EQ.0))THEN
              WRITE(ERRMSG,1010) APAR(I)(1:NBLNK(APAR(I))),
     +        FIXFILE(1:NBLNK(FIXFILE)),PESTFILE1(1:NBLNK(PESTFILE1))
1010          FORMAT(' Parameter "',A,'" cannot feature as ',
     +        'log-transformed in prior information as requested in ',
     +        'parameter fix file ',A,' because it is not ',
     +        'log-transformed in PEST control file ',A,'.')
              GO TO 9900
            ELSE IF((LOGPR(I).EQ.2).AND.(ITRANS(I).EQ.1))THEN
              WRITE(ERRMSG,1011) APAR(I)(1:NBLNK(APAR(I))),
     +        PESTFILE1(1:NBLNK(PESTFILE1)),FIXFILE(1:NBLNK(FIXFILE))
1011          FORMAT(' Parameter "',A,'" is log-transformed in PEST ',
     +        'control file ',A,' so prior information pertaining to ',
     +        'that parameter must be requested as log-transformed ',
     +        'in parameter fix file ',A,'.')
              GO TO 9900
            END IF
            IPR=IPR+1
            MPRIOR=MPRIOR+1
            WRITE(ANUM,'(I6)')IPR
            CALL SHIFTL(ANUM)
            APRIOR='pi_'//ANUM(1:NBLNK(ANUM))
            IF(ITRANS(I).EQ.1)THEN
              PRVAL=LOG10(VALUE(I))
            ELSE
              PRVAL=VALUE(I)
            END IF
            WRITE(APRVAL,'(1PG14.7)') PRVAL
            WRITE(AWEIGHT,'(1PG14.7)') WEIGHT(I)
            IF(ITRANS(I).EQ.1)THEN
              WRITE(20,1020) APRIOR(1:NBLNK(APRIOR)),
     +        APAR(I)(1:NBLNK(APAR(I))),
     +        APRVAL(1:NBLNK(APRVAL)),
     +        AWEIGHT(1:NBLNK(AWEIGHT)),
     +        OBSGP(I)(1:NBLNK(OBSGP(I)))
1020          FORMAT(1X,A,' 1.0 * log(',A,') = ',A,' ',A,' ',A)
            ELSE
              WRITE(20,1021) APRIOR(1:NBLNK(APRIOR)),
     +        APAR(I)(1:NBLNK(APAR(I))),
     +        APRVAL(1:NBLNK(APRVAL)),
     +        AWEIGHT(1:NBLNK(AWEIGHT)),
     +        OBSGP(I)(1:NBLNK(OBSGP(I)))
1021          FORMAT(1X,A,' 1.0 * ',A,' = ',A,' ',A,' ',A)
            END IF
          END IF
1000    CONTINUE

        IF(JLFLAG.EQ.1)THEN
          WRITE(20,16,ERR=9300) DLINE(1:NBLNK(DLINE))
        END IF

C -- NOW THE REMAINDER OF THE PEST CONTROL FILE IS READ AND WRITTEN.

       IF(IEND.EQ.1) GO TO 1200
1100   CONTINUE
       IF(ICONT.EQ.1)WRITE(20,16,ERR=9300) ELINE(1:NBLNK(ELINE))
1101   ILINE=ILINE+1
       READ(11,15,ERR=9400,END=1200) CLINE
       WRITE(20,16,ERR=9300) CLINE(1:NBLNK(CLINE))
       GO TO 1101

C -- THE TEMPORARY PEST CONTROL FILE IS CLOSED AND RE-WRITTEN TO THE NEW ONE.

1200    CONTINUE
        CLOSE(UNIT=11)
        CLOSE(UNIT=20)

        OPEN(UNIT=10,FILE=TMPFLE,IOSTAT=IERR)
        IF(IERR.NE.0)THEN
          WRITE(ERRMSG,1210) TMPFLE(1:NBLNK(TMPFLE))
1210      FORMAT(' Cannot re-open temporary file ',A,'.')
          GO TO 9900
        END IF
        OPEN(UNIT=20,FILE=PESTFILE2,IOSTAT=IERR)
        IF(IERR.NE.0)THEN
          WRITE(ERRMSG,1220) PESTFILE2(1:NBLNK(PESTFILE2))
1220      FORMAT(' Cannot open file ',A,' for output.')
          GO TO 9900
        END IF

        DO 1250 I=1,3
          READ(10,15,ERR=9700,END=9700) CLINE
          WRITE(20,16,ERR=9800) CLINE(1:NBLNK(CLINE))
1250    CONTINUE
        NPRIOR=NPRIOR+MPRIOR
        NOBSGP=NOBSGP+MGROUP
        READ(10,15,ERR=9700,END=9700) CLINE
        WRITE(20,1260,ERR=9800) NPAR,NOBS,NPARGP,NPRIOR,NOBSGP
1260    FORMAT(5I10)

1270    CONTINUE
          READ(10,15,ERR=9700,END=1400) CLINE
          IF(NPRIOR.EQ.0)THEN
            IF(CLINE(1:1).EQ.'*')THEN
              DLINE=CLINE
              CALL LOWCAS(DLINE)
              IF(INDEX(DLINE,'* prior infor').NE.0) GO TO 1270
            END IF
          END IF
          WRITE(20,16,ERR=9800) CLINE(1:NBLNK(CLINE))
        GO TO 1270

1400    CONTINUE
        CLOSE(UNIT=10)
        CLOSE(UNIT=20)

        WRITE(6,1409) PESTFILE1(1:NBLNK(PESTFILE1))
1409    FORMAT(' - file ',A,' read ok.')
        WRITE(6,1410) PESTFILE2(1:NBLNK(PESTFILE2))
1410    FORMAT(' - file ',A,' written ok.')

        GO TO 9990

9000    WRITE(6,9010)
9010    FORMAT(/,' PARAMFIX is run using the command:',/)
        WRITE(6,9020)
9020    FORMAT('     paramfix fixfile pestfile1 pestfile2',/)
        WRITE(6,9030)
9030    FORMAT(' where',/)
        WRITE(6,9040)
9040    FORMAT('     fixfile   is the name of a parameter ',
     +  'fix file,')
        WRITE(6,9050)
9050    FORMAT('     pestfile1 is the name of the input PEST ',
     +  'control file, and')
        WRITE(6,9060)
9060    FORMAT('     pestfile2 is the name of the output PEST ',
     +  'control file.')
        GO TO 9999

9100    CALL WRTINT(ALINE,ILINE)
        WRITE(ERRMSG,9110) ALINE(1:NBLNK(ALINE)),
     +  FIXFILE(1:NBLNK(FIXFILE))
9110    FORMAT(' Error reading line ',A,
     +  ' of parameter fix file ',A,'.')
        GO TO 9900
9200    WRITE(ERRMSG,9210) PESTFILE1(1:NBLNK(PESTFILE1))
9210    FORMAT(' Unexpected end to file ',A,
     +  '; can''t find "* parameter data" section.')
        GO TO 9900
9300    WRITE(ERRMSG,9310) TMPFLE(1:NBLNK(TMPFLE))
9310    FORMAT(' Error encountered while writing to file ',A,'.')
        GO TO 9900
9400    CALL WRTINT(ALINE,ILINE)
        WRITE(ERRMSG,9410) ALINE(1:NBLNK(ALINE)),
     +  PESTFILE1(1:NBLNK(PESTFILE1))
9410    FORMAT(' Error encountered while reading line ',A,
     +  ' of file ',A,'.')
        GO TO 9900
9500    WRITE(ERRMSG,9510) PESTFILE1(1:NBLNK(PESTFILE1))
9510    FORMAT(' Unexpected end encountered to file ',A,'.')
        GO TO 9900
9700    WRITE(ERRMSG,9710) TMPFLE(1:NBLNK(TMPFLE))
9710    FORMAT(' Error re-reading temporary file ',A,'.')
        GO TO 9900
9800    WRITE(ERRMSG,9810) PESTFILE2(1:NBLNK(PESTFILE2))
9810    FORMAT(' Error writing to new PEST control file ',A,'.')
        GO TO 9900
9850    CALL WRTINT(ATEMP,IPRIOR)
        WRITE(ERRMSG,9860) ATEMP(1:NBLNK(ATEMP)),
     +  PESTFILE1(1:NBLNK(PESTFILE1))
9860    FORMAT(' Error in prior information equation no. ',A,
     +  ' in file ',A,'.')
        GO TO 9900

9900    CONTINUE
        WRITE(6,*)
        CALL WRTERR
9995    WRITE(6,9996)
9996    FORMAT(/,' Execution terminated.')
        GO TO 9990

9990    CONTINUE
        DEALLOCATE(FIXPR,LOGPR,VALUE,WEIGHT,APAR,OBSGP,ITRANS,
     +  AOBSGP,COVFLE,EXGP,STAT=IERR)

        CLOSE(UNIT=10,IOSTAT=IERR)
        CLOSE(UNIT=11,IOSTAT=IERR)
        CLOSE(UNIT=20,IOSTAT=IERR)
        INQUIRE(FILE=TMPFLE,EXIST=LEXIST)
#ifdef UNIX
#ifdef SYS_FUNCTION
          IF(LEXIST) NNN=SYSTEM('/bin/rm '//
     +    TMPFLE(1:NBLNK(TMPFLE))//' > /dev/null')
#else
          IF(LEXIST) CALL SYSTEM('/bin/rm '//
     +    TMPFLE(1:NBLNK(TMPFLE))//' > /dev/null')
#endif
#else
          IF(LEXIST) CALL SYSTEM('del '//
     +    TMPFLE(1:NBLNK(TMPFLE))//' > nul')
#endif

9999    END


        SUBROUTINE LINSPL(IFAIL,NUM,LW,RW,CLINE)

C -- SUBROUTINE LINSPL SPLITS A LINE INTO WHITESPACE-SEPARATED SUBSTRINGS

        INTEGER NBLNK
        INTEGER IFAIL,NW,NBLC,J,I
        INTEGER NUM
        INTEGER LW(NUM),RW(NUM)
        CHARACTER*(*) CLINE

        IFAIL=0
        NW=0
        NBLC=NBLNK(CLINE)
        IF(NBLC.EQ.0) THEN
          IFAIL=-1
          RETURN
        END IF
        J=0
5       IF(NW.EQ.NUM) RETURN
        DO 10 I=J+1,NBLC
        IF((CLINE(I:I).NE.' ').AND.(CLINE(I:I).NE.',')
     +  .AND.(ICHAR(CLINE(I:I)).NE.9)) GO TO 20
10      CONTINUE
        IFAIL=1
        RETURN
20      NW=NW+1
        LW(NW)=I
        DO 30 I=LW(NW)+1,NBLC
        IF((CLINE(I:I).EQ.' ').OR.(CLINE(I:I).EQ.',')
     +  .OR.(ICHAR(CLINE(I:I)).EQ.9)) GO TO 40
30      CONTINUE
        RW(NW)=NBLC
        IF(NW.LT.NUM) IFAIL=1
        RETURN
40      RW(NW)=I-1
        J=RW(NW)
        GO TO 5

        END


        SUBROUTINE LOWCAS(ASTRNG)

C -- SUBROUTINE LOWCAS CONVERTS A STRING TO LOWER CASE

        IMPLICIT NONE

        INTEGER NBLNK
        INTEGER I,J
        CHARACTER*(*) ASTRNG

        DO 10 I=1,NBLNK(ASTRNG)
        J=ICHAR(ASTRNG(I:I))
        IF((J.GE.65).AND.(J.LE.90)) ASTRNG(I:I)=CHAR(J+32)
10      CONTINUE
        RETURN
        END


        SUBROUTINE UPCAS(ASTRNG)

C -- SUBROUTINE UPCAS CONVERTS A STRING TO UPPER CASE

        IMPLICIT NONE

        INTEGER NBLNK
        INTEGER I,J
        CHARACTER*(*) ASTRNG

        DO 10 I=1,NBLNK(ASTRNG)
        J=ICHAR(ASTRNG(I:I))
        IF((J.GE.97).AND.(J.LE.122)) ASTRNG(I:I)=CHAR(J-32)
10      CONTINUE
        RETURN
        END



        SUBROUTINE SHIFTL(AA)

C -- SUBROUTINE SHIFTL REMOVES LEADING BLANKS FROM A STRING

        IMPLICIT NONE

        INTEGER L,I,J,II
        CHARACTER*(*) AA

        L=LEN(AA)
        DO 10 I=1,L
        IF((AA(I:I).NE.' ').AND.(ICHAR(AA(I:I)).NE.9)) GO TO 50
10      CONTINUE
        RETURN
50      IF(I.EQ.1) RETURN
        II=I-1
        DO 100 J=I,L
100     AA(J-II:J-II)=AA(J:J)
        DO 110 J=1,II
110     AA(L-J+1:L-J+1)=' '
        RETURN
        END



        SUBROUTINE WRTINT(ATEMP,IVAL)

C -- SUBROUTINE WRTINT WRITES AN INTEGER TO A STRING

        IMPLICIT NONE

        INTEGER IVAL
        CHARACTER*(*) ATEMP
        CHARACTER*8 AFMT

        AFMT='(I   )'
        WRITE(AFMT(3:5),'(I3)') LEN(ATEMP)
        WRITE(ATEMP,AFMT)IVAL
        CALL SHIFTL(ATEMP)
        RETURN
        END


        SUBROUTINE DREALRD(IFAIL,CLINE,RTEMP)

C -- Subroutine DREALRD reads a real number from a string.

        IMPLICIT NONE

        INTEGER IFAIL
        DOUBLE PRECISION RTEMP
        CHARACTER*8 AFMT
        CHARACTER*(*) CLINE

        IFAIL=0
        AFMT='(F   .0)'
        WRITE(AFMT(3:5),'(I3)') LEN(CLINE)
        READ(CLINE,AFMT,ERR=100) RTEMP
        RETURN

100     IFAIL=1
        RETURN
        END


        SUBROUTINE INTRD(IFAIL,CLINE,ITEMP)

C -- Subroutine INTRD reads an integer from a string.

        IMPLICIT NONE

        INTEGER IFAIL
        INTEGER ITEMP
        CHARACTER*8 AFMT
        CHARACTER*(*) CLINE

        IFAIL=0
        AFMT='(I   )'
        WRITE(AFMT(3:5),'(I3)') LEN(CLINE)
        READ(CLINE,AFMT,ERR=100) ITEMP
        RETURN

100     IFAIL=1
        RETURN
        END


        LOGICAL FUNCTION ISPAR(IFAIL,APAR,CLINE,NB,NE,ILOG)

C -- FUNCTION ISPAR LOCATES A PARAMETER WITHIN A PRIOR INFORMATION LINE.

        IMPLICIT NONE

        INTEGER IFAIL,NB,NE,ILOG,IPOS,L,IE,I,NBB,NBLNK
        CHARACTER*(*) APAR
        CHARACTER*(*) CLINE

        IFAIL=0
        ISPAR=.FALSE.
        ILOG=0
        IPOS=1
10      NB=INDEX(CLINE(IPOS:),APAR(1:NBLNK(APAR)))
        IF(NB.EQ.0) RETURN
        NB=IPOS+NB-1
        IF(NB.NE.1)THEN
          IF((CLINE(NB-1:NB-1).EQ.' ').OR.(CLINE(NB-1:NB-1).EQ.'('))
     +    THEN
            GO TO 20
          ELSE
            IPOS=NB+1
            GO TO 10
          END IF
        ELSE
          IPOS=NB+1
          GO TO 10
        END IF
20      NBB=NB
        L=NBLNK(APAR)
        NE=NB+L-1
        IF(NE.GE.NBLNK(CLINE))RETURN
        IF((CLINE(NE+1:NE+1).EQ.' ').OR.(CLINE(NE+1:NE+1).EQ.')'))
     +  GO TO 30
        IPOS=NB+1
        GO TO 10

C -- WE STILL CANNOT BE SURE THAT THIS IS NOT A NUMBER OR PARAMETER GROUP.
C    FIRST WE RULE OUT THE PARAMETER GROUP POSSIBILITY.

30      IE=INDEX(CLINE,'=')
        IF(IE.EQ.0)THEN
          IFAIL=1
          RETURN
        END IF
        IF(IE.LT.NB)RETURN

C -- IF THE LOG IS TAKEN THEN IT IS A PARAMETER

        IF(NB-5.GE.1)THEN
          IF(CLINE(NB-5:NB-1).EQ.' log(') THEN
            ILOG=1
            NB=NB-4
            NE=NE+1
          END IF
        END IF

C -- IF THERE IS A MULTIPLY SIGN IN FRONT OF IT THEN IT IS A PARAMETER.

        DO 35 I=NB-1,1,-1
          IF(CLINE(I:I).NE.' ') GO TO 40
35      CONTINUE
        IPOS=NBB+1
        GO TO 10
40      IF(CLINE(I:I).EQ.'*') THEN
          NB=I
          GO TO 50
        END IF
        IPOS=NBB+1
        GO TO 10

50      ISPAR=.TRUE.
        RETURN
        END


        DOUBLE PRECISION FUNCTION VALCALC(IFAIL,CLINE,VALUE,NB,NE,
     +  ILOG,SIGN)

C -- FUNCTION VALCALC CALCULATES THE VALUE THAT A PARAMETER MAKES TO THE LEFT
C    SIDE OF A PRIOR INFORMATION EQUATION.

        IMPLICIT NONE

        INTEGER IFAIL,NB,NE,ILOG,I,J,SIGN
        DOUBLE PRECISION VALUE,FACTOR
        CHARACTER*(*) CLINE

        IFAIL=0
        SIGN=0

C -- FIRST WE FIND THE PRIOR INFORMATION FACTOR.

        DO 10 I=NB-1,1,-1
          IF(CLINE(I:I).NE.' ') GO TO 20
10      CONTINUE
        GO TO 100
20      DO 30 J=I-1,1,-1
          IF(CLINE(J:J).EQ.' ') GO TO 40
30      CONTINUE
        GO TO 100
40      NB=J+1
        CALL DREALRD(IFAIL,CLINE(NB:I),FACTOR)
        IF(IFAIL.NE.0) GO TO 100
        IF(ILOG.EQ.0)THEN
          VALCALC=VALUE*FACTOR
        ELSE
          VALCALC=LOG10(VALUE)*FACTOR
        END IF

C -- NOW WE SEE IF A "+" OR "-" SIGN PRECEDES IT.

        DO 50 I=J,1,-1
          IF(CLINE(I:I).NE.' ') GO TO 60
50      CONTINUE
        GO TO 100
60      IF((CLINE(I:I).NE.'+').AND.(CLINE(I:I).NE.'-'))RETURN
        NB=I
        IF(CLINE(I:I).EQ.'-') VALCALC=-VALCALC
        SIGN=1
        RETURN

100     IFAIL=1
        RETURN
        END


        DOUBLE PRECISION FUNCTION RHSCALC(IFAIL,CLINE,NB,NE)

C -- FUNCTION RHSCALC CALCULATES THE VALUE OF THE RIGHT HAND SIDE OF A PRIOR
C    INFORMATION EQUATION.

        IMPLICIT NONE

        INTEGER IFAIL,NB,NE,N,NBLNK,I,J
        DOUBLE PRECISION RTEMP
        CHARACTER*(*) CLINE

        IFAIL=0
        N=NBLNK(CLINE)
        I=INDEX(CLINE,'=')
        IF(I.EQ.0) GO TO 100
        DO 10 J=I+1,N
         IF(CLINE(J:J).NE.' ') GO TO 20
10      CONTINUE
        GO TO 100
20      NB=J
        DO 30 I=J+1,N
         IF(CLINE(I:I).EQ.' ') GO TO 40
30      CONTINUE
        GO TO 100
40      NE=I-1
        CALL DREALRD(IFAIL,CLINE(NB:NE),RTEMP)
        IF(IFAIL.NE.0) GO TO 100
        RHSCALC=RTEMP
        RETURN

100     IFAIL=1
        RETURN
        END


        SUBROUTINE SIGNADJ(IFAIL,NB,NE,CLINE)

C -- SUBROUTINE SIGNADJ ADJUSTS THE SIGN OF A PRIOR INFORMATION TERM
C    THAT FOLLOWS A DELETED PRIOR INFORMATION TERM.

        IMPLICIT NONE

        INTEGER NB,NE,N,NBLNK,I,J,IFAIL
        CHARACTER*(*) CLINE

        IFAIL=0
        N=NBLNK(CLINE)
        IF(NE.EQ.N) GO TO 100
        DO 20 I=NE+1,N
          IF(CLINE(I:I).NE.' ') GO TO 40
20      CONTINUE
        GO TO 100
40      IF(CLINE(I:I).EQ.'=') THEN
          RETURN
        ELSE IF(CLINE(I:I+1).EQ.'+ ') THEN
          CLINE(I:I)=' '
          CALL SHIFTL(CLINE(I:))
          RETURN
        ELSE IF(CLINE(I:I).EQ.'-')THEN
          DO 60 J=I+1,N
            IF(CLINE(J:J).NE.' ')THEN
              IF(CLINE(J:J).EQ.'-')THEN
                CLINE(I:I)=' '
                CLINE(J:J)=' '
                CALL SHIFTL(CLINE(I:))
                RETURN
              ELSE IF(CLINE(J:J).EQ.'+') THEN
                CLINE(J:J)='-'
                CLINE(I:I)=' '
                CALL SHIFTL(CLINE(I:))
                RETURN
              ELSE
                CLINE(I:I)=' '
                CLINE(J-1:J-1)='-'
                CALL SHIFTL(CLINE(I:))
                RETURN
              END IF
            END IF
60        CONTINUE
        ELSE
          GO TO 100
        END IF

100     IFAIL=1
        RETURN
        END



        SUBROUTINE WRTERR

C -- SUBROUTINE WRTERR FORMATS AND WRITES ERROR MESSAGES

        IMPLICIT NONE

        INTEGER NBLNK
        INTEGER J,JEND,I,NBLC,ITAKE
        CHARACTER*2120 ERRMSG

        COMMON /ERRMSG/ERRMSG

        ITAKE=0
        J=0
        NBLC=NBLNK(ERRMSG)
5       JEND=J+78-ITAKE
        IF(JEND.GE.NBLC) GO TO 100
        DO 10 I=JEND,J+1,-1
        IF(ERRMSG(I:I).EQ.' ') THEN
          IF(ITAKE.EQ.0) THEN
            WRITE(6,'(A)',ERR=200) ERRMSG(J+1:I)
            ITAKE=3
          ELSE
            WRITE(6,'(A)',ERR=200) '   '//ERRMSG(J+1:I)
          END IF
          J=I
          GO TO 5
        END IF
10      CONTINUE
        IF(ITAKE.EQ.0)THEN
          WRITE(6,'(A)',ERR=200) ERRMSG(J+1:JEND)
          ITAKE=3
        ELSE
          WRITE(6,'(A)',ERR=200) '   '//ERRMSG(J+1:JEND)
        END IF
        J=JEND
        GO TO 5
100     JEND=NBLC
        IF(ITAKE.EQ.0)THEN
          WRITE(6,'(A)',ERR=200) ERRMSG(J+1:JEND)
        ELSE
          WRITE(6,'(A)',ERR=200) '   '//ERRMSG(J+1:JEND)
        END IF
        RETURN

200     CALL EXIT(100)
        END

