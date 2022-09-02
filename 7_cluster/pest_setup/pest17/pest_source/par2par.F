      PROGRAM PAR2PAR

C -- Program PAR2PAR is a parameter preprocessor and manipulator.

C -- Maximum terms in any mathematical expression:-
      INTEGER MAXTERM
      PARAMETER(MAXTERM=2000)
C -- Maximum number of function types:-
      INTEGER NFUNCT
      PARAMETER(NFUNCT=16)
C -- Maximum number of operators:-
      INTEGER NOPER
      PARAMETER(NOPER=7)

      INTEGER IERR,MCASE,MMLINE,IFAIL,ILINE,MAXPAR,NUMTMP,NPAR,
     +INL,NB,I,JERR,IFIN,NTERM,IPAR,NBLC,J1,J2,NNW,PRECIS,NOPNT,J,
     +IWARN
      INTEGER NBLNK
      INTEGER LS(2),RS(2),IORDER(MAXTERM)
      CHARACTER*7 APRECS
      CHARACTER*9 APOINT
      CHARACTER*12 AVERSION
      CHARACTER*15 ALINE
      CHARACTER*200 COMLIN,INFILE,AFILE,AALINE
      CHARACTER*(1) OPERAT(7)
      CHARACTER*6 FUNCT(NFUNCT)
      CHARACTER*12 TPAR
      CHARACTER*25 ATERM(MAXTERM),BTERM(MAXTERM)

      INTEGER NW(:)
      DOUBLE PRECISION PVAL(:)
      CHARACTER*1 PARDEL(:)
      CHARACTER*12 APAR(:)
      CHARACTER*23 PWORD(:)
      CHARACTER*200 TMPFL(:),MINFL(:)

C -- File size
      CHARACTER*2120 ERRMSG
      CHARACTER*2010 CLINE,DLINE,ELINE

      ALLOCATABLE::APAR,PVAL,TMPFL,MINFL,NW,PARDEL,PWORD

      COMMON /IERR/IERR,MCASE
      COMMON /ERRMSG/ERRMSG

      DATA FUNCT /'abs   ','acos  ','asin  ','atan  ','cos   ','cosh  ',
     +   'exp   ','log   ','log10 ','sin   ','sinh  ','sqrt  ','tan   ',
     +   'tanh  ','neg   ','pos   '/
      DATA OPERAT /'^','/','*','-','+','(',')'/

      IERR=0
      IWARN=0
      NOPNT=0                             !default value
      PRECIS=0                            !default value

c      open(unit=99,file='debug.dat')            !debug

      INCLUDE 'version.inc'
      WRITE(6,1,ERR=8900) TRIM(AVERSION)
1     FORMAT(' PAR2PAR Version ',A,'. ',
     +'Watermark Numerical Computing.',/)

C -- The command line is parsed and analysed.

#ifdef NO_CMDLINE
3     WRITE(*,'(A)') ' Enter command line argument > '
      READ(*,'(A)',ERR=3) COMLIN
#else
      CALL PGETCL(COMLIN)
#endif
      IF(COMLIN.EQ.' ') GO TO 9000
      CALL SPACESUB(COMLIN)
      CALL LNSPL1(IFAIL,2,LS,RS,COMLIN)
      IF(IFAIL.EQ.0) GO TO 9000
      INFILE=COMLIN(LS(1):RS(1))
      INL=NBLNK(INFILE)
      CALL REMCHAR(INFILE,CHAR(211))
#ifdef CAPFILE
      CALL UPCAS(INFILE)
#endif

C -- THE PAR2PAR INPUT FILE IS OPENED.

#ifdef UNICOS
      OPEN(UNIT=10,FILE=INFILE,STATUS='OLD',RECL=2000,ERR=50)
#else
      OPEN(UNIT=10,FILE=INFILE,STATUS='OLD',ERR=50)
#endif
      GO TO 70
50    WRITE(ERRMSG,60) INFILE(1:NBLNK(INFILE))
60    FORMAT(' Cannot open PAR2PAR input file ',A,'.')
      GO TO 9799
70    CONTINUE

C -- THE PAR2PAR INPUT FILE IS READ A FIRST TIME.

      AFILE=INFILE
      ILINE=0
100   ILINE=ILINE+1
      READ(10,'(A)',ERR=9100,END=9050) CLINE
      IF(CLINE.EQ.' ') GO TO 100
      IF(CLINE(1:1).EQ.'#') GO TO 100
      CALL LOWCAS(CLINE)
      IF(CLINE(1:10).NE.'* paramete')THEN
        WRITE(ERRMSG,120) INFILE(1:NBLNK(INFILE))
120     FORMAT(' First data line of file ',A,' should be ',
     +  '"* parameter data"')
        GO TO 9799
      END IF

      MAXPAR=0
150   ILINE=ILINE+1
      READ(10,'(A)',ERR=9100,END=9040) CLINE
      IF(CLINE.EQ.' ') GO TO 150
      IF(CLINE(1:1).EQ.'#') GO TO 150
      CALL LOWCAS(CLINE)
      IF(CLINE(1:10).EQ.'* template') GO TO 200
      MAXPAR=MAXPAR+1
      GO TO 150

200   CONTINUE
      IF(MAXPAR.EQ.0)THEN
        WRITE(ERRMSG,170) INFILE(1:NBLNK(INFILE))
170     FORMAT(' The "parameter data" section of file ',A,
     +  ' contains no parameters.')
        GO TO 9799
      END IF

      NUMTMP=0
220   ILINE=ILINE+1
      READ(10,'(A)',ERR=9100,END=250) CLINE
      CALL REMCHAR(CLINE,CHAR(9))
      IF(CLINE.EQ.' ') GO TO 220
      IF(CLINE(1:1).EQ.'#') GO TO 220
      CALL LOWCAS(CLINE)
      IF(CLINE(1:9).EQ.'* control') GO TO 250
      NUMTMP=NUMTMP+1
      GO TO 220

250   CONTINUE
      IF(NUMTMP.EQ.0)THEN
        WRITE(ERRMSG,260) INFILE(1:NBLNK(INFILE))
260     FORMAT(' The "template and model input files" section of ',
     +  'file ',a,' cites no files.')
        GO TO 9799
      END IF

      REWIND(UNIT=10,IOSTAT=JERR)
      IF(JERR.NE.0)THEN
        WRITE(ERRMSG,270) INFILE(1:NBLNK(INFILE))
270     FORMAT(' Cannot rewind file ',A,'.')
        GO TO 9799
      END IF

C -- Memory is now allocated.

      ALLOCATE(APAR(MAXPAR),PVAL(MAXPAR),TMPFL(NUMTMP),
     +MINFL(NUMTMP),NW(MAXPAR),PARDEL(NUMTMP),PWORD(MAXPAR),STAT=JERR)
      IF(JERR.NE.0) GO TO 9150

      DO 286 I=1,MAXPAR
        APAR(I)=' '
286   CONTINUE

C -- The PAR2PAR input file is now read in detail and relationships evaluated.

      IFIN=0
      NPAR=0
      ILINE=0
300   ILINE=ILINE+1
      READ(10,'(A)',ERR=9100,END=9050) CLINE
      IF(CLINE.EQ.' ') GO TO 300
      IF(CLINE(1:1).EQ.'#') GO TO 300

      CLINE=' '
320   CONTINUE
      ILINE=ILINE+1
      CALL WRTINT(ALINE,ILINE)
      AALINE=' Line '//ALINE(1:INDEX(ALINE,' ')-1)//' of '//
     +'file '//INFILE(1:INL)//': '
      MMLINE=NBLNK(AALINE)+1
      READ(10,'(A)',ERR=9100,END=9050) DLINE
      IF(DLINE.EQ.' ') GO TO 320
      IF(DLINE(1:1).EQ.'#') GO TO 320
      CALL LOWCAS(DLINE)
      CALL SHIFTL(DLINE)
      CALL TABREM(DLINE)
      IF(DLINE(1:10).EQ.'* template') THEN
        IFIN=1
      ELSE IF(DLINE(1:1).EQ.'&')THEN
        DLINE(1:1)=' '
        CALL SHIFTL(DLINE)
        NB=NBLNK(CLINE)
        IF(NB.EQ.0)THEN
          CLINE=DLINE
        ELSE
          CLINE=CLINE(1:NB)//DLINE(1:NBLNK(DLINE))
        END IF
        GO TO 320
      END IF
      IF(CLINE.EQ.' ')THEN
        CLINE=DLINE
        GO TO 320
      END IF

c      write(99,*)                                       !debug
c      write(99,*)                                       !debug
c      write(99,*) ' Here is a new expression:- '        !debug
c      write(99,'(1x,a)') trim(cline)                    !debug

      NPAR=NPAR+1
      CALL GPARNME(IFAIL,APAR(NPAR),CLINE,ELINE)
      IF(NPAR.GT.1)THEN
        IF(APAR(NPAR).NE.' ')THEN
          DO 310 IPAR=1,NPAR-1
            IF(APAR(IPAR).EQ.APAR(NPAR))THEN
              WRITE(ERRMSG,315) APAR(NPAR)(1:NBLNK(APAR(NPAR)))
315           FORMAT(' Parameter "',A,'" appears on left side of more ',
     +        'than one expression.')
              GO TO 9799
            END IF
310       CONTINUE
        END IF
      END IF
      IF(IFAIL.NE.0) GO TO 990

C -- The expression is now parsed.

      CALL PARSE(IFAIL,MAXTERM,NTERM,NOPER,CLINE,ELINE,ATERM,
     +BTERM,NFUNCT,FUNCT,OPERAT)

c      if(nterm.le.maxterm)then                            !debug
c      write(99,*) ' Here is the parsed expression:-'      !debug
c      do i=1,nterm                                        !debug
c        write(99,'(a)') trim(aterm(i))                    !debug
c      end do                                              !debug
c      end if                                              !debug

C -- The values of the parameters are substituted for their names.

      IF(IERR.NE.0) GO TO 990
      CALL PARNUM(IFAIL,NPAR,NTERM,NOPER,APAR,PVAL,ATERM,OPERAT,
     +ELINE)
      IF(IFAIL.NE.0) GO TO 990

c      write(99,*) ' Here is the parsed expression after PARNUM:-'      !debug
c      do i=1,nterm                                        !debug
c        write(99,'(a)') trim(aterm(i))                    !debug
c      end do                                              !debug

C -- The expression is evaluated.

      CALL EVALUATE(IFAIL,MAXTERM,NTERM,NOPER,NFUNCT,ATERM,BTERM,
     +ELINE,OPERAT,FUNCT,IORDER,PVAL(NPAR))


c      write(99,*) ' Here is the evaluated expression: - ', pval(npar)      !debug


990   IF(IFIN.EQ.1)THEN
        GO TO 1000
      ELSE
        CLINE=DLINE
        GO TO 320
      END IF

1000  CONTINUE
      IF(IERR.NE.0) GO TO 9800


C -- All expressions have been evaluated. Numbers must be written
C    to model input files.

C -- First the names of all parameter files and corresponding template
C    files are acquired.

      DO 1100 I=1,NUMTMP
1005    ILINE=ILINE+1
        READ(10,'(A)',ERR=9100,END=9050) CLINE
        CALL REMCHAR(CLINE,CHAR(9))
        IF(CLINE.EQ.' ') GO TO 1005
        IF(CLINE(1:1).EQ.'#') GO TO 1005
        CALL SPACESUB(CLINE)
        CALL LNSPL1(IFAIL,2,LS,RS,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL WRTINT(ALINE,ILINE)
          AALINE=' Line '//ALINE(1:INDEX(ALINE,' ')-1)//' of '//
     +    'file '//INFILE(1:INL)//': '
          MMLINE=NBLNK(AALINE)+1
          WRITE(ERRMSG,1020) AALINE(1:MMLINE)
1020      FORMAT(A,'this line should have 2 entries.')
          GO TO 9799
        END IF
        TMPFL(I)=CLINE(LS(1):RS(1))
        MINFL(I)=CLINE(LS(2):RS(2))
        CALL REMCHAR(TMPFL(I),CHAR(211))
        CALL REMCHAR(MINFL(I),CHAR(211))
#ifdef CAPFILE
        CALL UPCAS(TMPFL(I))
        CALL UPCAS(MINFL(I))
#endif

1100  CONTINUE
      IF(NUMTMP.GT.1)THEN
        DO 1080 I=2,NUMTMP
          DO 1070 J=1,I-1
            IF(MINFL(I).EQ.MINFL(J))THEN
              WRITE(ERRMSG,1050) INFILE(1:INL)
1050          FORMAT(' Two model input files have the same name in ',
     +        'PAR2PAR input file ',A)
              GO TO 9799
            END IF
1070      CONTINUE
1080    CONTINUE
      END IF

C -- If there is a "control data" section, this is now read.

1101  CONTINUE
      ILINE=ILINE+1
      READ(10,'(A)',ERR=9100,END=1190) CLINE
      IF(CLINE.EQ.' ') GO TO 1101
      IF(CLINE(1:1).EQ.'#') GO TO 1101
      CALL LOWCAS(CLINE)
      IF(CLINE(1:9).EQ.'* control')GO TO 1120
      GO TO 1101
1120  CONTINUE
      ILINE=ILINE+1
      READ(10,'(A)',ERR=9100,END=1190) CLINE
      IF(CLINE.EQ.' ') GO TO 1120
      IF(CLINE(1:1).EQ.'#') GO TO 1120
      CALL LOWCAS(CLINE)
      CALL WRTINT(ALINE,ILINE)
      AALINE=' Line '//ALINE(1:INDEX(ALINE,' ')-1)//' of '//
     +'file '//INFILE(1:INL)//': '
      MMLINE=NBLNK(AALINE)+1
      CALL LNSPL1(IFAIL,2,LS,RS,CLINE)
      IF(IFAIL.NE.0)THEN
        WRITE(ERRMSG,1130) AALINE(1:MMLINE)
1130    FORMAT(A,'two entries (for variables PRECIS and DPOINT) are ',
     +  'expected on this line.')
        GO TO 9799
      END IF
      APRECS=CLINE(LS(1):RS(1))
      IF(APRECS(1:6).EQ.'double')THEN
        PRECIS=1
      ELSE IF(APRECS(1:6).EQ.'single')THEN
        PRECIS=0
      ELSE
        WRITE(ERRMSG,1140) AALINE(1:MMLINE)
1140    FORMAT(A,'value expected for variable PRECIS must be ',
     +  '"single" or "double".')
        GO TO 9799
      END IF
      APOINT=CLINE(LS(2):RS(2))
      IF(APOINT(1:5).EQ.'point')THEN
        NOPNT=0
      ELSE IF(APOINT(1:7).EQ.'nopoint') THEN
        NOPNT=1
      ELSE
        WRITE(ERRMSG,1150) AALINE(1:MMLINE)
1150    FORMAT(A,'value expected for variable DPOINT must be ',
     +  '"point" or "nopoint".')
        GO TO 9799
      END IF

1190  CONTINUE
      CLOSE(UNIT=10)

C -- Next all template files are read in order to find out the
C    minumum parameter space width pertaining to each parameter.

      IPAR=1
      DO 1200 I=1,NPAR
        NW(I)=1000
1200  CONTINUE
      DO 1250 I=1,NUMTMP
        AFILE=TMPFL(I)
        OPEN(UNIT=35,FILE=TMPFL(I),STATUS='OLD',IOSTAT=JERR)
        IF(JERR.NE.0)THEN
          WRITE(ERRMSG,1210) TMPFL(I)(1:NBLNK(TMPFL(I))),
     +    INFILE(1:INL)
1210      FORMAT(' Cannot open template file ',A,' cited in ',
     +    'PAR2PAR input file ',A)
          GO TO 9799
        END IF
        READ(35,'(A)',ERR=9250,END=9200) CLINE
        CALL LOWCAS(CLINE(1:3))
        IF((CLINE(1:3).NE.'ptf').AND.
     +     (CLINE(1:3).NE.'jtf')) GO TO 9200
        PARDEL(I)=CLINE(5:5)
        IF(PARDEL(I).EQ.' ') GO TO 9200
        ILINE=1
1230    ILINE=ILINE+1
        READ(35,'(A)',ERR=9250,END=1295) CLINE
        NBLC=NBLNK(CLINE)
        J2=0
1240    IF(J2.GE.NBLC) GO TO 1230
        J1=INDEX(CLINE(J2+1:NBLC),PARDEL(I))
        IF(J1.EQ.0) GO TO 1230
        J1=J1+J2
        J2=INDEX(CLINE(J1+1:NBLC),PARDEL(I))
        IF(J2.EQ.0)THEN
          CALL WRTINT(ALINE,ILINE)
          WRITE(ERRMSG,1245) ALINE(1:NBLNK(ALINE)),
     +    TMPFL(I)(1:NBLNK(TMPFL(I)))
1245      FORMAT(' Unbalanced parameter delimiters at line ',A,
     +    ' of file ',A)
          GO TO 9799
        END IF
        J2=J2+J1
        CALL PARNAM(IFAIL,J1,J2,TPAR,CLINE)
        IF(IFAIL.EQ.1)THEN
          CALL WRTINT(ALINE,ILINE)
          WRITE(ERRMSG,1260) ALINE(1:NBLNK(ALINE)),
     +    TMPFL(I)(1:NBLNK(TMPFL(I)))
1260      FORMAT(' Parameter space less than 3 characters wide at ',
     +    'line ',A,' of template file ',A)
          GO TO 9799
        ELSE IF(IFAIL.EQ.2)THEN
          CALL WRTINT(ALINE,ILINE)
          WRITE(ERRMSG,1280) ALINE(1:NBLNK(ALINE)),
     +    TMPFL(I)(1:NBLNK(TMPFL(I)))
1280      FORMAT(' Blank parameter space at line ',A,' of template ',
     +    'file ',A)
          GO TO 9799
        END IF
        CALL WHICH1(IFAIL,NPAR,IPAR,APAR,TPAR)
        IF(IFAIL.NE.0)THEN
          WRITE(ERRMSG,1290) TPAR(1:NBLNK(TPAR)),
     +    TMPFL(I)(1:NBLNK(TMPFL(I))),INFILE(1:INL)
1290      FORMAT(' Parameter "',A,'" cited in template file ',A,
     +    ' is not cited in PAR2PAR input file ',A)
          GO TO 9799
        END IF
        NNW=J2-J1+1
        IF(NNW.LT.NW(IPAR)) NW(IPAR)=NNW
        GO TO 1240
1295    CLOSE(UNIT=35)
1250  CONTINUE
!      DO 1320 I=1,NPAR
!        IF(NW(I).EQ.1000) THEN
!          WRITE(ERRMSG,1310) APAR(I)(1:NBLNK(APAR(I))),
!     +    INFILE(1:INL)
!1310      FORMAT(' Warning: parameter "',A,'" cited in PAR2PAR ',
!     +    'input file ',A,' is not cited in any template file.')
!          CALL WRTERR
!          IERR=IERR-1
!          IWARN=IWARN+1
!        END IF
!1320  CONTINUE

C -- Parameters are now written to the model input files.

      IF(IWARN.NE.0) WRITE(6,*)
      CALL INWRIT(IFAIL,NPAR,PRECIS,NOPNT,NUMTMP,NW,PVAL,
     +PARDEL,PWORD,MINFL,TMPFL,APAR,CLINE)

      GO TO 9800

8900  CONTINUE
      IERR=IERR+1
      GO TO 9810

9000  WRITE(6,9010,ERR=8900)
9010  FORMAT(' PAR2PAR is run using the command:',/)
      WRITE(6,9020,ERR=8900)
9020  FORMAT('    par2par infile',/,/,' where',/)
      WRITE(6,9030,ERR=8900)
9030  FORMAT('    "infile" is a PAR2PAR input file.')
      GO TO 9800

9040  WRITE(ERRMSG,9045) INFILE(1:NBLNK(INFILE))
9045  FORMAT(' End of file ',a,' encountered before ',
     +'"* template and model input files" section read.')
      GO TO 9799
9050  WRITE(ERRMSG,9060) AFILE(1:NBLNK(AFILE))
9060  FORMAT(' Unexpected end to file ',A,'.')
      GO TO 9799
9100  CALL WRTINT(ALINE,ILINE)
      WRITE(ERRMSG,9110) ALINE(1:NBLNK(ALINE)),
     +AFILE(1:NBLNK(AFILE))
9110  FORMAT(' Error reading line ',A,' of file ',A,'.')
      GO TO 9799
9150  WRITE(ERRMSG,9160)
9160  FORMAT(' Error in allocating memory required for program to run.')
      GO TO 9799
9200  WRITE(ERRMSG,9210) AFILE(1:NBLNK(AFILE))
9210  FORMAT(' File ',A,' is an illegal template file.')
      GO TO 9799
9250  WRITE(ERRMSG,9260) AFILE(1:NBLNK(AFILE))
9260  FORMAT(' Unable to read template file ',A,'.')
      GO TO 9799

9799  CALL WRTERR
9800  WRITE(6,*)

9810  CONTINUE
      DEALLOCATE(APAR,PVAL,TMPFL,MINFL,NW,PARDEL,PWORD,STAT=JERR)
      IF(IERR.NE.0)THEN
        CALL EXIT(50)
      ELSE
        CALL EXIT(0)
      END IF

      END



      SUBROUTINE GPARNME(IFAIL,APAR,CLINE,ELINE)

C -- Subroutine GPARNME extracts the name of a parameter from the front of
C    a relationship statement in a PAR2PAR input file.

      INTEGER IFAIL,IERR,MCASE,J,N,K
      INTEGER NBLNK
      CHARACTER*(*) APAR
      CHARACTER*(*) CLINE,ELINE
      CHARACTER*200 ATEMP

C -- File size
      CHARACTER*2120 ERRMSG

      COMMON /IERR/IERR,MCASE
      COMMON /ERRMSG/ERRMSG

      IFAIL=0

      J=INDEX(CLINE,'=')
      IF(J.EQ.0)THEN
        WRITE(ERRMSG,20) CLINE(1:NBLNK(CLINE))
20      FORMAT(' Cannot locate "=" sign in expression:- "',A,'"')
        CALL WRTERR
        GO TO 9900
      ELSE IF(J.EQ.1)THEN
        WRITE(ERRMSG,30) CLINE(1:NBLNK(CLINE))
30      FORMAT(' The "=" character is in wrong position in ',
     +  'expression:- "',A,'"')
        CALL WRTERR
        GO TO 9900
      END IF
      ATEMP=CLINE(1:J-1)
      CALL SHIFTL(ATEMP)
      N=NBLNK(ATEMP)
      K=INDEX(ATEMP(1:N),' ')
      IF(K.NE.0)THEN
        WRITE(ERRMSG,40) CLINE(1:NBLNK(CLINE))
40      FORMAT(' Space in parameter name in expression:- "',A,'"')
        CALL WRTERR
        GO TO 9900
      END IF
      IF(N.GT.12)THEN
        WRITE(ERRMSG,50) ATEMP(1:NBLNK(ATEMP))
50      FORMAT(' Parameter name "',A,'" greater than 12 characters ',
     +  'in length.')
        CALL WRTERR
        GO TO 9900
      END IF
      APAR=ATEMP
      ELINE=CLINE
      CLINE=CLINE(J+1:)
      CALL SHIFTL(CLINE)

      RETURN

9900  IFAIL=1

      RETURN

      END


      SUBROUTINE PARSE(IFAIL,MAXTERM,NTERM,NOPER,CLINE,ELINE,ATERM,
     +BTERM,NFUNCT,FUNCT,OPERAT)

C -- Subroutine PARSE breaks an expression up into elements.

      INTEGER IFAIL,MAXTERM,NTERM,IERR,MCASE,NFUNCT,NOPER
      INTEGER ICOUNT,NB,I,JFAIL,ITERM,J,IIFUN,NEG
      INTEGER NBLNK
      CHARACTER*25 ATEMP
      CHARACTER*1 AA
      CHARACTER*6 AATERM
      CHARACTER*10 BB
      CHARACTER*(*) ATERM(MAXTERM),BTERM(MAXTERM)
      CHARACTER*(*) CLINE,ELINE
      CHARACTER*(*) FUNCT(NFUNCT)
      CHARACTER*(*) OPERAT(NOPER)

C -- File size
      CHARACTER*2120 ERRMSG

      COMMON /IERR/IERR,MCASE
      COMMON /ERRMSG/ERRMSG

      IFAIL=0
      NTERM=0

C -- First a check is made to see if brackets are balanced.

      IF(CLINE.EQ.' ')GO TO 9000

      ICOUNT=0
      NB=NBLNK(CLINE)
      DO 3 I=1,NB
        IF(CLINE(I:I).EQ.'(')THEN
          ICOUNT=ICOUNT+1
        ELSE IF(CLINE(I:I).EQ.')')THEN
          ICOUNT=ICOUNT-1
        END IF
3     CONTINUE
      IF(ICOUNT.NE.0)THEN
        WRITE(ERRMSG,4) ELINE(1:NBLNK(ELINE))
4       FORMAT(' Unbalanced parantheses in expression:- "',A,
     +  '"')
        GO TO 9015
      END IF

      IF(INDEX(CLINE,'=').NE.0)GO TO 9000
5     CONTINUE
      CALL GETNEXT(JFAIL,NOPER,CLINE,ATEMP,OPERAT)
      IF(JFAIL.LT.0)THEN
        GO TO 50
      ELSE
        NTERM=NTERM+1
        IF(NTERM.GT.MAXTERM)GO TO 9100
        ATERM(NTERM)=ATEMP
        GO TO 5
      END IF
50    CONTINUE

C -- Functions are now dealt with.

      IIFUN=0
      IF(NTERM.LE.2) GO TO 400
      DO 200 ITERM=2,NTERM
        IF(ATERM(ITERM)(1:1).EQ.'(')THEN
          AA=ATERM(ITERM-1)(1:1)
          IF((AA.EQ.'+').OR.(AA.EQ.'-').OR.(AA.EQ.'*').OR.
     +       (AA.EQ.'/').OR.(AA.EQ.'^').OR.(AA.EQ.'(')) GO TO 200
          AATERM=ATERM(ITERM-1)(1:6)
          DO 70 J=1,NFUNCT
            IF(AATERM.EQ.FUNCT(J)) GO TO 80
70        CONTINUE
          WRITE(ERRMSG,75) ATERM(ITERM-1)(1:NBLNK(ATERM(ITERM-1))),
     +    ELINE(1:NBLNK(ELINE))
75        FORMAT(' Illegal function name  "',A,'" in expression ',
     +    '"'A,'"')
          GO TO 9015
80        CALL WRTINT(BB,J)
          ATERM(ITERM-1)='~#str_'//BB(1:NBLNK(BB))
          IIFUN=IIFUN+1
        END IF
200   CONTINUE
      IF(IIFUN.EQ.0) GO TO 400

      DO 300 ITERM=1,NTERM
        IF(ATERM(ITERM)(1:6).EQ.'~#str_') THEN
          ATERM(ITERM+1)(1:1)=CHAR(220)
          ICOUNT=1
          DO 280 J=ITERM+1,NTERM
            IF(ATERM(J)(1:1).EQ.'(')THEN
              ICOUNT=ICOUNT+1
            ELSE IF(ATERM(J)(1:1).EQ.')')THEN
              ICOUNT=ICOUNT-1
              IF(ICOUNT.EQ.0)THEN
                ATERM(J)='~#fin_'
                GO TO 300
              END IF
            END IF
280       CONTINUE
        END IF
300   CONTINUE

      CALL COMPRESS(MAXTERM,NTERM,ATERM,BTERM)

400   CONTINUE

C -- If the last item is an operator then the expression is invalid.

      AA=ATERM(NTERM)(1:1)
      IF((AA.EQ.'+').OR.(AA.EQ.'-').OR.(AA.EQ.'/').OR.(AA.EQ.'*').OR.
     +(AA.EQ.'^')) GO TO 9000


C -- The "-" and the "+" signs are expanded as a function if appropriate.

490   CONTINUE
      DO 500 ITERM=1,NTERM
        IF((ATERM(ITERM)(1:1).EQ.'-').OR.
     +     (ATERM(ITERM)(1:1).EQ.'+'))THEN
          IF(ATERM(ITERM)(1:1).EQ.'+')THEN
            NEG=0
          ELSE
            NEG=1
          END IF
          IF(ITERM.EQ.1) THEN
            IF(NTERM.EQ.MAXTERM) GO TO 9100
            CALL EXPNEG(IFAIL,MAXTERM,NTERM,ITERM,ATERM,NEG)
            IF(IFAIL.NE.0) GO TO 9000
            GO TO 490
          ELSE IF(ATERM(ITERM-1)(1:6).EQ.'~#str_')THEN
            IF(NTERM.EQ.MAXTERM) GO TO 9100
            CALL EXPNEG(IFAIL,MAXTERM,NTERM,ITERM,ATERM,NEG)
            IF(IFAIL.NE.0) GO TO 9000
            GO TO 490
          ELSE
            AA=ATERM(ITERM-1)(1:1)
            IF((AA.EQ.'(').OR.(AA.EQ.'+').OR.(AA.EQ.'-').OR.
     +         (AA.EQ.'*').OR.(AA.EQ.'/').OR.(AA.EQ.'^'))THEN
               IF(NTERM.EQ.MAXTERM) GO TO 9100
               CALL EXPNEG(IFAIL,MAXTERM,NTERM,ITERM,ATERM,NEG)
               IF(IFAIL.NE.0) GO TO 9000
               GO TO 490
            END IF
          END IF
        END IF
500   CONTINUE

      RETURN

9000  WRITE(ERRMSG,9010) ELINE(1:NBLNK(ELINE))
9010  FORMAT(' Illegal expression:- "',A,'"')
9015  CALL WRTERR
      GO TO 9020
9100  WRITE(ERRMSG,9110) ELINE(1:NBLNK(ELINE))
9110  FORMAT(' To many terms in expression:- "',A,'"')
      CALL WRTERR
      GO TO 9020

9020  IFAIL=1

      RETURN
      END


      SUBROUTINE EXPNEG(IFAIL,MAXTERM,NTERM,ITERM,ATERM,NEG)

C -- Subroutine EXPNEG expands a "-" sign into a function.

      INTEGER MAXTERM,NTERM,ITERM,IFAIL,ICOUNT,JTERM,I,NEG
      CHARACTER*(*) ATERM(MAXTERM)

      IFAIL=0
      IF(NEG.EQ.1)THEN
        ATERM(ITERM)='~#str_15'
      ELSE
        ATERM(ITERM)='~#str_16'
      END IF
      ICOUNT=0
      DO 100 JTERM=ITERM+1,NTERM
        IF((ATERM(JTERM)(1:1).EQ.'-').OR.
     +     (ATERM(JTERM)(1:1).EQ.'+'))GO TO 100
        IF(ATERM(JTERM)(1:1).EQ.'(')THEN
          ICOUNT=ICOUNT+1
        ELSE IF(ATERM(JTERM)(1:1).EQ.')')THEN
          ICOUNT=ICOUNT-1
        ELSE IF(ATERM(JTERM)(1:6).EQ.'~#str_')THEN
          ICOUNT=ICOUNT+1
        ELSE IF(ATERM(JTERM)(1:6).EQ.'~#fin_')THEN
          ICOUNT=ICOUNT-1
        END IF
        IF(ICOUNT.LT.0)THEN
          IFAIL=1
          RETURN
        END IF
        IF(ICOUNT.EQ.0)THEN
          IF(JTERM.LT.NTERM)THEN
            DO 40 I=NTERM,JTERM+1,-1
              ATERM(I+1)=ATERM(I)
40          CONTINUE
          END IF
          ATERM(JTERM+1)='~#fin_'
          NTERM=NTERM+1
          RETURN
        END IF
100   CONTINUE

      RETURN
      END



      SUBROUTINE GETNEXT(IFAIL,NOPER,CLINE,ATERM,OPERAT)

C -- Subroutine GETNEXT splits off the next term of an expression.

      INTEGER IFAIL,I,J,NB,NOPER,L,K,IERR
      INTEGER NBLNK
      double precision DVAL
      character*10 AFMT
      CHARACTER*(*) CLINE
      CHARACTER*(*) ATERM
      CHARACTER*(*) OPERAT(NOPER)

      ATERM=' '
      IFAIL=0
      IF(CLINE.EQ.' ')THEN
        IFAIL=-1
        RETURN
      END IF

      DO 10 I=1,NOPER
        IF(CLINE(1:1).EQ.OPERAT(I))THEN
          ATERM(1:1)=OPERAT(I)
          CLINE=CLINE(2:)
          CALL SHIFTL(CLINE)
          GO TO 20
        END IF
10    CONTINUE
      GO TO 50

20    IF(ATERM(1:1).EQ.'*')THEN
        IF(CLINE(1:1).EQ.'*')THEN
          ATERM(1:1)='^'
          CLINE=CLINE(2:)
          CALL SHIFTL(CLINE)
        END IF
      END IF
      RETURN

50    CONTINUE
      NB=NBLNK(CLINE)
      DO 100 I=2,NB
        DO 90 J=1,NOPER
          IF(CLINE(I:I).EQ.OPERAT(J)) THEN
            IF(I.LE.2) go to 120
            IF(I.EQ.NB) go to 120
            IF((J.NE.4).AND.(J.NE.5))go to 120
            IF((CLINE(I-1:I-1).NE.'E').AND.(CLINE(I-1:I-1).NE.'e').AND.
     +         (CLINE(I-1:I-1).NE.'D').AND.(CLINE(I-1:I-1).NE.'d'))
     +         GO TO 120
            DO 190 K=I+1,NB
              DO 180 L=1,NOPER
                IF(CLINE(K:K).EQ.OPERAT(L))go to 200
180           CONTINUE
190         CONTINUE
            K=NB+1
200         K=K-1
            AFMT='(f    .0)'
            WRITE(AFMT(3:6),'(i4)')K
            READ(CLINE(1:K),AFMT,IOSTAT=IERR) DVAL
            if(IERR.NE.0) go to 120
            GO TO 100
          END IF
          IF(CLINE(I:I).EQ.OPERAT(J)) GO TO 120
90      CONTINUE
100   CONTINUE
      ATERM=CLINE(1:MIN(25,NB))
      CLINE=' '
      RETURN

120   ATERM=CLINE(1:I-1)
      CLINE=CLINE(I:)
      RETURN

      END


      SUBROUTINE COMPRESS(MAXTERM,NTERM,ATERM,BTERM)

C -- Subroutine COMPRESS removes "dead terms" from the expression.

      INTEGER MAXTERM,NTERM,I,JTERM
      CHARACTER*(*) ATERM(MAXTERM),BTERM(MAXTERM)

      DO 100 I=1,NTERM
        BTERM(I)=ATERM(I)
100   CONTINUE
      JTERM=0
      DO 200 I=1,NTERM
        IF(BTERM(I)(1:1).NE.CHAR(220))THEN
          JTERM=JTERM+1
          ATERM(JTERM)=BTERM(I)
        END IF
200   CONTINUE
      NTERM=JTERM

      RETURN
      END



      SUBROUTINE PARNUM(IFAIL,NPAR,NTERM,NOPER,APAR,PVAL,ATERM,OPERAT,
     +ELINE)

C -- Subroutine PARNUM replaces parameter names with their values.

      INTEGER IFAIL,NPAR,NTERM,NOPER,IERR,MCASE,ITERM,J,JERR,NB
      INTEGER NBLNK
      DOUBLE PRECISION PVAL(NPAR),DTEMP
      CHARACTER*25 AAPAR
      CHARACTER*(*) ELINE
      CHARACTER*(*) APAR(NPAR)
      CHARACTER*(*) ATERM(NTERM)
      CHARACTER*(*) OPERAT(NOPER)

C -- File size
      CHARACTER*2120 ERRMSG

      COMMON /IERR/IERR,MCASE
      COMMON /ERRMSG/ERRMSG

      IFAIL=0
      DO 200 ITERM=1,NTERM
         IF(ATERM(ITERM)(1:2).EQ.'~#') GO TO 200
         DO 20 J=1,NOPER
           IF(ATERM(ITERM)(1:1).EQ.OPERAT(J)) GO TO 200
20       CONTINUE
         AAPAR=ATERM(ITERM)
         NB=NBLNK(AAPAR)
         IF(INDEX(AAPAR(1:NB),' ').NE.0)THEN
           WRITE(ERRMSG,30) AAPAR(1:NB),ELINE(1:NBLNK(ELINE))
30         FORMAT(' Parameter name "',A,'" cannot include blank ',
     +     'in expression:- "',A,'"')
           CALL WRTERR
           IFAIL=1
           RETURN
         END IF
         CALL RLREAD(JERR,AAPAR,DTEMP)
         IF(JERR.EQ.0) GO TO 200
         IF(NPAR.GT.1)THEN
           DO 40 J=1,NPAR-1
             IF(AAPAR.EQ.APAR(J))GO TO 50
40         CONTINUE
           WRITE(ERRMSG,45) AAPAR(1:NBLNK(AAPAR)),
     +     ELINE(1:NBLNK(ELINE))
45         FORMAT(' A value has not been assigned to parameter "',A,
     +     '" in expression:- "',A,'"')
           CALL WRTERR
           IFAIL=1
           RETURN
         END IF
50       WRITE(ATERM(ITERM),'(E25.14E3)') PVAL(J)
200   CONTINUE

      RETURN
      END



      SUBROUTINE EVALUATE(IFAIL,MAXTERM,NTERM,NOPER,NFUNCT,ATERM,BTERM,
     +ELINE,OPERAT,FUNCT,IORDER,DVAL)

      INTEGER NTERM,NOPER,NFUNCT,MAXTERM,ITERM,JERR,MAXORD,ICOUNT,I,
     +IOPER,IFAIL,IERR,MCASE
      INTEGER NBLNK
      INTEGER IORDER(MAXTERM)
      DOUBLE PRECISION DVAL,DTEMP1,DTEMP2
      CHARACTER*1 AA
      CHARACTER*6 AFUNCT
      CHARACTER*(*) ELINE
      CHARACTER*(*) ATERM(MAXTERM),BTERM(MAXTERM)
      CHARACTER*(*) OPERAT(NOPER),FUNCT(NFUNCT)

C -- File size
      CHARACTER*2120 ERRMSG

      COMMON /IERR/IERR,MCASE
      COMMON /ERRMSG/ERRMSG

      IFAIL=0

C -- IF THERE IS ONLY ONE TERM LEFT, THE EXPRESSION HAS BEEN EVALUATED.

100   CONTINUE
c      write(99,*) (trim(aterm(iterm)),iterm=1,nterm)     !debug

      IF(NTERM.EQ.1)THEN
        CALL RLREAD(JERR,ATERM(1),DVAL)
        IF(JERR.NE.0)THEN
          WRITE(ERRMSG,110) ELINE(1:NBLNK(ELINE))
110       FORMAT(' Cannot evaluate expression: "',A,'" using ',
     +    'current parameter values.')
          CALL WRTERR
          GO TO 9999
        END IF
        RETURN
      END IF

C -- IF THERE ARE ANY NUMBERS SURROUNDED BY BRACKETS, THEN THE BRACKETS ARE
C    REMOVED

      IF(NTERM.GE.3)THEN
        DO 150 ITERM=1,NTERM-2
          IF(ATERM(ITERM)(1:1).EQ.'(') THEN
            IF(ATERM(ITERM+2)(1:1).EQ.')')THEN
              ATERM(ITERM)(1:1)=CHAR(220)
              ATERM(ITERM+2)(1:1)=CHAR(220)
              CALL COMPRESS(MAXTERM,NTERM,ATERM,BTERM)
              GO TO 100
            END IF
          END IF
150     CONTINUE
      END IF

C -- CAN ANY FUNCTION EVALUATIONS NOW BE DONE?

      IF(NTERM.GE.3)THEN
        DO 300 ITERM=1,NTERM-2
          IF(ATERM(ITERM)(1:6).EQ.'~#str_')THEN
            IF(ATERM(ITERM+2)(1:6).EQ.'~#fin_')THEN
              CALL FUNCEVAL(JERR,NFUNCT,ATERM(ITERM),ATERM(ITERM+1),
     +        DVAL,FUNCT)
              IF(JERR.NE.0)THEN
                AFUNCT=FUNCT(JERR)
                WRITE(ERRMSG,170) AFUNCT(1:NBLNK(AFUNCT)),
     +          ELINE(1:NBLNK(ELINE))
170             FORMAT(' Cannot evaluate "',A,'" function in ',
     +          'expression "',A,'" because function argument is ',
     +          'out of range.')
                CALL WRTERR
                GO TO 9999
              END IF
              ATERM(ITERM)(1:1)=CHAR(220)
              WRITE(ATERM(ITERM+1),'(E25.14E3)') DVAL
              ATERM(ITERM+2)(1:1)=CHAR(220)
              CALL COMPRESS(MAXTERM,NTERM,ATERM,BTERM)
              GO TO 100
            END IF
          END IF
300     CONTINUE
      END IF

C -- The operators are now ranked by their level of nesting.

      MAXORD=0
      DO 320 ITERM=1,NTERM
        IORDER(ITERM)=0
320   CONTINUE
      ICOUNT=1
      DO 350 ITERM=1,NTERM
        AA=ATERM(ITERM)(1:1)
        IF(AA.EQ.'(')THEN
          ICOUNT=ICOUNT+1
        ELSE IF(AA.EQ.')')THEN
          ICOUNT=ICOUNT-1
        ELSE IF(ATERM(ITERM)(1:6).EQ.'~#str_')THEN
          ICOUNT=ICOUNT+1
        ELSE IF(ATERM(ITERM)(1:6).EQ.'~#fin_')THEN
          ICOUNT=ICOUNT-1
        ELSE IF((AA.EQ.'+').OR.(AA.EQ.'-').OR.(AA.EQ.'*').OR.
     +  (AA.EQ.'/').OR.(AA.EQ.'^'))THEN
          IORDER(ITERM)=ICOUNT
          IF(ICOUNT.GT.MAXORD)MAXORD=ICOUNT
        ELSE
          IORDER(ITERM)=-1            ! It must be a number.
        END IF
350   CONTINUE

C -- We now look for a calculation to do, starting at the highest level.

      IF(NTERM.GE.3)THEN
        DO 400 I=MAXORD,1,-1
          DO 390 IOPER=1,5
            DO 380 ITERM=2,NTERM-1
              IF(IORDER(ITERM).EQ.I)THEN   !It is an operator
                IF(ATERM(ITERM)(1:1).EQ.OPERAT(IOPER))THEN
                  IF((IORDER(ITERM-1).LT.0).AND.
     +               (IORDER(ITERM+1).LT.0))THEN    !numbers either side
                    CALL RLREAD(IFAIL,ATERM(ITERM-1),DTEMP1)
                    CALL RLREAD(IFAIL,ATERM(ITERM+1),DTEMP2)
                    IF(IOPER.EQ.1)THEN
                      IF(DTEMP1.LT.0.0)THEN
                        IF(DTEMP2.NE.FLOAT(NINT(DTEMP2)))THEN
                          WRITE(ERRMSG,384) ELINE(1:NBLNK(ELINE))
384                       FORMAT(' Negative number raised to ',
     +                    'fractional power in expression:- "',A,'"')
                          CALL WRTERR
                          GO TO 9999
                        END IF
                      END IF
                      DVAL=DTEMP1**DTEMP2
                    ELSE IF(IOPER.EQ.3)THEN
                      DVAL=DTEMP1*DTEMP2
                    ELSE IF(IOPER.EQ.2)THEN
                      IF(DTEMP2.EQ.0.0D0) THEN
                        WRITE(ERRMSG,385) ELINE(1:NBLNK(ELINE))
385                     FORMAT(' Divide by zero in expression:- "',
     +                  A,'"')
                        CALL WRTERR
                        GO TO 9999
                      END IF
                      DVAL=DTEMP1/DTEMP2
                    ELSE IF(IOPER.EQ.5)THEN
                      DVAL=DTEMP1+DTEMP2
                    ELSE IF(IOPER.EQ.4)THEN
                      DVAL=DTEMP1-DTEMP2
                    END IF
                    WRITE(ATERM(ITERM),'(E25.14E3)') DVAL
                    ATERM(ITERM-1)(1:1)=CHAR(220)
                    ATERM(ITERM+1)(1:1)=CHAR(220)
                    CALL COMPRESS(MAXTERM,NTERM,ATERM,BTERM)
                    GO TO 100
                  END IF
                END IF
              END IF
380         CONTINUE
390       CONTINUE
400     CONTINUE
      END IF
      WRITE(ERRMSG,410) ELINE(1:NBLNK(ELINE))
410   FORMAT(' Cannot evaluate expression:- "',A,'"')
      CALL WRTERR
      GO TO 9999

9999  IFAIL=1
      RETURN
      END




      SUBROUTINE FUNCEVAL(JERR,NFUNCT,ATERM1,ATERM2,DVAL,FUNCT)

C -- Subroutine FUNCEVAL evaluates a function.

      INTEGER JERR,NFUNCT,IFN,IFAIL
      DOUBLE PRECISION DVAL,DTEMP
      CHARACTER*(*) ATERM1,ATERM2
      CHARACTER*(*) FUNCT(NFUNCT)

C -- First we find out which function we are evaluating.


      JERR=0
      ATERM1(1:6)=' '
      CALL SHIFTL(ATERM1)
      CALL INREAD(IFAIL,ATERM1,IFN)
      CALL RLREAD(IFAIL,ATERM2,DTEMP)
      IF(IFAIL.NE.0) GO TO 9000
      IF(IFN.EQ.1)THEN
        DVAL=ABS(DTEMP)
      ELSE IF(IFN.EQ.2)THEN
        IF((DTEMP.GT.1.0D0).OR.(DTEMP.LT.-1.0D0))GO TO 9000
        DVAL=ACOS(DTEMP)
      ELSE IF(IFN.EQ.3)THEN
        IF((DTEMP.GT.1.0D0).OR.(DTEMP.LT.-1.0D0))GO TO 9000
        DVAL=ASIN(DTEMP)
      ELSE IF(IFN.EQ.4)THEN
        DVAL=ATAN(DTEMP)
      ELSE IF(IFN.EQ.5)THEN
        IF((DTEMP.GT.1.0D10).OR.(DTEMP.LT.-1.0D10))GO TO 9000
        DVAL=COS(DTEMP)
      ELSE IF(IFN.EQ.6)THEN
        DVAL=COSH(DTEMP)
      ELSE IF(IFN.EQ.7)THEN
        IF(DTEMP.GT.500.0D0) GO TO 9000
        DVAL=EXP(DTEMP)
      ELSE IF(IFN.EQ.8)THEN
        IF(DTEMP.LE.0.0D0) GO TO 9000
        DVAL=LOG(DTEMP)
      ELSE IF(IFN.EQ.9)THEN
        IF(DTEMP.LE.0.0D0) GO TO 9000
        DVAL=LOG10(DTEMP)
      ELSE IF(IFN.EQ.10)THEN
        IF((DTEMP.GT.1.0D10).OR.(DTEMP.LT.-1.0D10))GO TO 9000
        DVAL=SIN(DTEMP)
      ELSE IF(IFN.EQ.11)THEN
        DVAL=SINH(DTEMP)
      ELSE IF(IFN.EQ.12)THEN
        IF(DTEMP.LT.0.0D0) GO TO 9000
        DVAL=SQRT(DTEMP)
      ELSE IF(IFN.EQ.13)THEN
        IF((DTEMP.GT.1.0E10).OR.(DTEMP.LT.-1.0E10))GO TO 9000
        DVAL=TAN(DTEMP)
      ELSE IF(IFN.EQ.14)THEN
        DVAL=TANH(DTEMP)
      ELSE IF(IFN.EQ.15)THEN
        DVAL=-DTEMP
      ELSE IF(IFN.EQ.16)THEN
        DVAL=DTEMP
      END IF

      RETURN

C -- An error condition has occurred.

9000  JERR=IFN
      RETURN
      END



        SUBROUTINE LNSPL1(IFAIL,NUM,LW,RW,CLINE)

C -- SUBROUTINE LNSPL1 SPLITS A STRING INTO SPACE-DELIMITED
C -- SUBSTRINGS

        INTEGER NBLNK
        INTEGER IFAIL,NW,NBLC,J,I
        INTEGER NUM
        INTEGER LW(NUM),RW(NUM)
        CHARACTER*(*) CLINE

        IFAIL=0
        NW=0
        NBLC=NBLNK(CLINE)
        IF((NBLC.NE.0).AND.(INDEX(CLINE,CHAR(9)).NE.0)) THEN
          CALL TABREM(CLINE)
          NBLC=NBLNK(CLINE)
        ENDIF
        IF(NBLC.EQ.0) THEN
          IFAIL=-1
          RETURN
        END IF
        J=0
5       IF(NW.EQ.NUM) RETURN
        DO 10 I=J+1,NBLC
        IF((CLINE(I:I).NE.' ').AND.(CLINE(I:I).NE.',').AND.
     +  (ICHAR(CLINE(I:I)).NE.9)) GO TO 20
10      CONTINUE
        IFAIL=1
        RETURN
20      NW=NW+1
        LW(NW)=I
        DO 30 I=LW(NW)+1,NBLC
        IF((CLINE(I:I).EQ.' ').OR.(CLINE(I:I).EQ.',').OR.
     +  (ICHAR(CLINE(I:I)).EQ.9)) GO TO 40
30      CONTINUE
        RW(NW)=NBLC
        IF(NW.LT.NUM) IFAIL=1
        RETURN
40      RW(NW)=I-1
        J=RW(NW)
        GO TO 5

        END

        SUBROUTINE TABREM(CLINE)

C -- SUBROUTINE TABREM REMOVES TABS FROM A STRING

        INTEGER I
        CHARACTER*(*) CLINE

        DO 10 I=1,LEN(CLINE)
10      IF(ICHAR(CLINE(I:I)).EQ.9) CLINE(I:I)=' '

        RETURN
        END


#ifdef CAPFILE
        SUBROUTINE UPCAS(ASTRNG)

C -- SUBROUTINE UPCAS CONVERTS A STRING TO UPPER CASE

        INTEGER NBLNK
        INTEGER I,J
        CHARACTER*(*) ASTRNG

        DO 10 I=1,NBLNK(ASTRNG)
        J=ICHAR(ASTRNG(I:I))
        IF((J.GE.97).AND.(J.LE.122)) ASTRNG(I:I)=CHAR(J-32)
10      CONTINUE
        RETURN
        END
#endif



        SUBROUTINE SHIFTL(AA)

C -- SUBROUTINE SHIFTL REMOVES LEADING BLANKS FROM A STRING

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

        INTEGER IVAL
        CHARACTER*(*) ATEMP

        WRITE(ATEMP,'(I10)')IVAL
        CALL SHIFTL(ATEMP)
        RETURN
        END



        SUBROUTINE LOWCAS(ASTRNG)

C -- SUBROUTINE LOWCAS CONVERTS A STRING TO LOWER CASE

        INTEGER NBLNK
        INTEGER I,J
        CHARACTER*(*) ASTRNG

        DO 10 I=1,NBLNK(ASTRNG)
        J=ICHAR(ASTRNG(I:I))
        IF((J.GE.65).AND.(J.LE.90)) ASTRNG(I:I)=CHAR(J+32)
10      CONTINUE
        RETURN
        END



        SUBROUTINE WRTERR

C -- SUBROUTINE WRTERR WRITES AN ERROR MESSAGE

        INTEGER NBLNK
        INTEGER IERR,J,JEND,I,NBLC,ITAKE,MCASE
        CHARACTER*2120 ERRMSG

        COMMON /IERR/IERR,MCASE
        COMMON /ERRMSG/ERRMSG

        IERR=IERR+1
        IF(IERR.GT.100)THEN
          WRITE(6,*,ERR=200)
C          OPEN(UNIT=36,FILE='pest.tmp',STATUS='UNKNOWN',ERR=200)
C          WRITE(36,'(A)',ERR=200)'1'
C          CLOSE(UNIT=36,ERR=200)
          CALL EXIT(50)
        END IF
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


        SUBROUTINE RLREAD(IFAIL,CLINE,RTEMP)

C -- Subroutine REALREAD reads a real number from a string.

        INTEGER IFAIL,NBLNK,NB
        DOUBLE PRECISION RTEMP
        CHARACTER*8 AFMT
        CHARACTER*(*) CLINE

        IFAIL=0
        NB=NBLNK(CLINE)
        AFMT='(F   .0)'
        WRITE(AFMT(3:5),'(I3)') NB
        READ(CLINE(1:NB),AFMT,ERR=100) RTEMP
        RETURN

100     IFAIL=1
        RETURN
        END


        SUBROUTINE INREAD(IFAIL,CLINE,ITEMP)

C -- Subroutine INREAD reads a real number from a string.

        INTEGER IFAIL,ITEMP,NB,NBLNK
        CHARACTER*6 AFMT
        CHARACTER*(*) CLINE

        IFAIL=0
        NB=NBLNK(CLINE)
        AFMT='(I   )'
        WRITE(AFMT(3:5),'(I3)') NB
        READ(CLINE(1:NB),AFMT,ERR=100) ITEMP
        RETURN

100     IFAIL=1
        RETURN
        END


        SUBROUTINE PARNAM(IFAIL,J1,J2,TPAR,CLINE)

C -- SUBROUTINE PARNAM EXTRACTS A PARAMETER NAME FROM A STRING

        INTEGER IFAIL
        INTEGER J1,J2,I,J
        CHARACTER*12 TPAR
        CHARACTER*(*) CLINE

        IFAIL=0
        TPAR=' '
        IF(J2-J1.LE.1) THEN
          IFAIL=1
          RETURN
        END IF
        DO 10 I=J1+1,J2-1
        IF(CLINE(I:I).EQ.' ') GO TO 10
        GO TO 30
10      CONTINUE
        IFAIL=2
        RETURN
30      J=MIN(12,J2-I)
        TPAR(1:J)=CLINE(I:I+J-1)
        RETURN
        END


        SUBROUTINE WHICH1(IFAIL,NPAR,IPAR,APAR,TPAR)

C -- SUBROUTINE WHICH1 FINDS A STRING IN AN ARRAY OF STRINGS

        INTEGER NPAR,IPAR,I
        INTEGER IFAIL
        CHARACTER*(*) TPAR
        CHARACTER*(*) APAR(NPAR)

        IFAIL=0
        IF((IPAR.LT.1).OR.(IPAR.GT.NPAR)) IPAR=1
        CALL LOWCAS(TPAR)
        IF(TPAR.EQ.APAR(IPAR)) RETURN
        IF(IPAR.NE.NPAR)THEN
          DO 20 I=IPAR+1,NPAR
          IF(TPAR.EQ.APAR(I))THEN
            IPAR=I
            RETURN
          END IF
20        CONTINUE
        END IF
        IF(IPAR.NE.1)THEN
          DO 40 I=IPAR-1,1,-1
          IF(TPAR.EQ.APAR(I)) THEN
            IPAR=I
            RETURN
          END IF
40        CONTINUE
        END IF
        IFAIL=1
        RETURN
        END


        SUBROUTINE INWRIT(IFAIL,NPAR,PRECIS,NOPNT,NINFL,NW,PVAL,
     +  MARK,PWORD,INFILE,INTEMP,APAR,CLINE)

C -- SUBROUTINE INWRIT WRITES MODEL INPUT FILES PRIOR TO RUNNING THE MODEL

        INTEGER NBLNK
        INTEGER NPAR,PRECIS,NINFL,IFILE,J2,LC,J1,IPAR,NOPNT,
     +  J,IFAIL,IERR,MCASE,JERR,IPP,ILINE
        INTEGER NW(NPAR)
        DOUBLE PRECISION TVAL
        DOUBLE PRECISION PVAL(NPAR)
        CHARACTER*12 TPAR
        CHARACTER*(*) CLINE
        CHARACTER*(*) PWORD(NPAR),APAR(NPAR)
        CHARACTER*(*) INFILE(NINFL),INTEMP(NINFL)
        CHARACTER MARK(NINFL)

C -- File size
      CHARACTER*2120 ERRMSG

      COMMON /IERR/IERR,MCASE
      COMMON /ERRMSG/ERRMSG

C -- EACH OF THE PARAMETER WORDS IS FILLED

        IFAIL=0
        IPAR=1
        DO 100 IPP=1,NPAR
        CALL WRTSIG(IFAIL,PVAL(IPP),PWORD(IPP),NW(IPP),PRECIS,TVAL,
     +  NOPNT)
        IF(IFAIL.NE.0) THEN
          WRITE(ERRMSG,10)
10        FORMAT(' Error in subroutine WRTSIG - contact programmer.')
          CALL WRTERR
          IFAIL=1
          RETURN
        END IF
        PVAL(IPP)=TVAL
100     CONTINUE

C -- NEXT THE SUBSTITUTIONS IN THE TEMPLATE FILES ARE MADE

        DO 500 IFILE=1,NINFL
          OPEN(UNIT=35,FILE=INTEMP(IFILE),STATUS='OLD',IOSTAT=JERR)
          IF(JERR.NE.0)THEN
            WRITE(ERRMSG,105) INTEMP(IFILE)(1:NBLNK(INTEMP(IFILE)))
105         FORMAT(' Cannot re-open template file ',A)
            CALL WRTERR
            IFAIL=1
            RETURN
          END IF
          OPEN(UNIT=40,FILE=INFILE(IFILE),IOSTAT=JERR)
          IF(JERR.NE.0)THEN
            WRITE(ERRMSG,106) INFILE(IFILE)(1:NBLNK(INFILE(IFILE)))
106         FORMAT(' Cannot open model input file ',A,
     +      ' to record parameter values.')
            CALL WRTERR
            IFAIL=1
            RETURN
          END IF
          READ(35,*)
          ILINE=1
120       ILINE=ILINE+1
          READ(35,22,END=400) CLINE
22        FORMAT(A)
          LC=NBLNK(CLINE)
          J2=0
150       IF(J2.GE.LC) GO TO 300
          J1=INDEX(CLINE(J2+1:LC),MARK(IFILE))
          IF(J1.EQ.0) GO TO 300
          J1=J1+J2
          J2=INDEX(CLINE(J1+1:LC),MARK(IFILE))
          J2=J2+J1
          CALL PARNAM(IFAIL,J1,J2,TPAR,CLINE)
          CALL WHICH1(IFAIL,NPAR,IPAR,APAR,TPAR)
c       The following works when space bigger than pword(:nblnk(pword))
c       cline(j1:j2)=pword(ipar)(:nblnk(pword(ipar)))
          DO 160 J=J1,J2
160       CLINE(J:J)=' '
          J=NBLNK(PWORD(IPAR))
          CLINE(J2-J+1:J2)=PWORD(IPAR)(1:J)
          GO TO 150
300       WRITE(40,22,ERR=320) CLINE(:MAX(NBLNK(CLINE),1))
          GO TO 120
320       WRITE(ERRMSG,305) INFILE(IFILE)(1:NBLNK(INFILE(IFILE)))
305       FORMAT(' Cannot write to model input file ',A)
          CALL WRTERR
          IFAIL=1
          RETURN
400       CLOSE(UNIT=35)
          CLOSE(UNIT=40)
          WRITE(6,420) INFILE(IFILE)(1:NBLNK(INFILE(IFILE)))
420       FORMAT(' - File ',a,' written ok.')
500     CONTINUE
        RETURN

        END


