        PROGRAM PSTGEN

C -- Number of parameters; number of observations
        INTEGER ZNPAR, ZNOBS
        PARAMETER (ZNPAR = 9000)
        PARAMETER (ZNOBS = 300000)

        INTEGER LENCAS,LENPAR,LENOBS,IERR,MCASE,MMLINE,NBLC,I,J,
     +  EXTLEN,NPAR,IFAIL,ILINE,II,NOBS,MAXPAR,MAXOBS
        INTEGER L(4),R(4)
        DOUBLE PRECISION PVAL,SCALE,OFFSET,OBSVAL,LBOUND,UBOUND
        CHARACTER*12 AVERSION
        CHARACTER*12 APAR(:),TPAR
        CHARACTER*20 AOBS(:),OTPAR
        CHARACTER*100 CASFL,PARFL,OBSFL,ATEMP*30,AMXPAR*10,
     +  AMXOBS*10,PRECIS*10,DPOINT*10
        CHARACTER*420 ERRMSG
        CHARACTER*100 AALINE*200,COMLIN*130,CLINE*200

        ALLOCATABLE::APAR,AOBS

        INTEGER NBLNK

        COMMON /IERR/IERR,MCASE
        COMMON /MMLINE/MMLINE
        COMMON /ERRMSG/ERRMSG
        COMMON /AALINE/AALINE

        INCLUDE 'version.inc'
        WRITE(6,1,ERR=9700) TRIM(AVERSION)
1       FORMAT(' PESTGEN Version ',A,'. ',
     +  'Watermark Numerical Computing.',/)

        IERR=0
        NPAR=0
        NOBS=0
        MAXPAR=ZNPAR
        CALL WRTINT(AMXPAR,MAXPAR)
        MAXOBS=ZNOBS
        CALL WRTINT(AMXOBS,MAXOBS)

        ALLOCATE(APAR(MAXPAR),AOBS(MAXOBS),STAT=IFAIL)
        IF(IFAIL.NE.0) THEN
          WRITE(ERRMSG,10)
10        FORMAT(' Cannot allocate sufficient memory to run PESTGEN.')
          CALL WRTERR
          GO TO 9998
        END IF

C -- THE PESTGEN COMMAND LINE IS PARSED

#ifdef NO_CMDLINE
3       WRITE(*,'(A)') ' Enter command line arguments > '
        READ(*,'(A)',ERR=3) COMLIN
#else
        CALL PGETCL(COMLIN)
#endif
        CALL SPACESUB(COMLIN)
        NBLC=NBLNK(COMLIN)
        IF(NBLC.EQ.0) THEN
          GO TO 9900
        END IF
        DO 50 I=1,NBLC
        IF(COMLIN(I:I).NE.' ') GO TO 60
50      CONTINUE
        GO TO 9900
60      DO 70 J=I,NBLC
        IF(COMLIN(J:J).EQ.' ') GO TO 90
70      CONTINUE
        GO TO 9900
90      J=J-1
        CASFL=COMLIN(I:J)
#ifdef CAPFILE
        CALL UPCAS(CASFL)
#endif
        LENCAS=J-I+1
        DO 100 I=J+1,NBLC
        IF(COMLIN(I:I).NE.' ') GO TO 110
100     CONTINUE
110     DO 120 J=I,NBLC
        IF(COMLIN(J:J).EQ.' ') GO TO 130
120     CONTINUE
        GO TO 9900
130     J=J-1
        PARFL=COMLIN(I:J)
#ifdef CAPFILE
        CALL UPCAS(PARFL)
#endif
        LENPAR=J-I+1
        DO 150 I=J+1,NBLC
        IF(COMLIN(I:I).NE.' ') GO TO 170
150     CONTINUE
170     DO 180 J=I,NBLC
        IF(COMLIN(J:J).EQ.' ') GO TO 9900
180     CONTINUE
        LENOBS=NBLC-I+1
        OBSFL=COMLIN(I:NBLC)
#ifdef CAPFILE
        CALL UPCAS(OBSFL)
#endif

        CALL REMCHAR(CASFL,CHAR(211))
        CALL REMCHAR(PARFL,CHAR(211))
        CALL REMCHAR(OBSFL,CHAR(211))

        CALL EXTCHK(EXTLEN,LENCAS,CASFL)
        IF(EXTLEN.EQ.LENCAS+1)THEN
          CASFL(EXTLEN:EXTLEN+4)='.pst '
#ifdef CAPFILE
          CALL UPCAS(CASFL)
#endif
          LENCAS=NBLNK(CASFL)
        ELSE
          IF(EXTLEN.EQ.1) GO TO 9900
#ifdef CAPFILE
          IF(CASFL(EXTLEN:EXTLEN+3).NE.'.PST') GO TO 9900
#else
          IF(CASFL(EXTLEN:EXTLEN+3).NE.'.pst') GO TO 9900
#endif
        END IF
c        IF(LENCAS.GT.51) THEN
c          WRITE(ERRMSG,210) CASFL(1:LENCAS)
c210       FORMAT(' The name of the case file (',A,') must be 51 ',
c     +    'characters or less.')
c          CALL WRTERR
c        END IF
c        IF(LENPAR.GT.51) THEN
c          WRITE(ERRMSG,220) PARFL(1:LENPAR)
c220       FORMAT(' The name of the parameter value file (',A,
c     +    ') must be 51 characters or less.')
c          CALL WRTERR
c        END IF
c        IF(LENOBS.GT.51) THEN
c          WRITE(ERRMSG,230) OBSFL(1:LENOBS)
c230       FORMAT(' The name of the observation value file (',A,
c     +    ') must be 51 characters or less.')
c          CALL WRTERR
c        END IF
        IF(IERR.NE.0) GO TO 9998

C -- THE PARAMETER VALUE FILE IS READ AND CHECKED

#ifdef UNICOS
        OPEN(UNIT=20,FILE=PARFL,STATUS='OLD',RECL=1000,ERR=250)
#else
        OPEN(UNIT=20,FILE=PARFL,STATUS='OLD',ERR=250)
#endif
        GO TO 300
250     WRITE(ERRMSG,260) PARFL(1:LENPAR)
260     FORMAT(' Cannot open parameter value file ',A,'.')
        CALL WRTERR
#ifdef UNICOS
300     OPEN(UNIT=30,FILE=OBSFL,STATUS='OLD',RECL=1000,ERR=350)
#else
300     OPEN(UNIT=30,FILE=OBSFL,STATUS='OLD',ERR=350)
#endif
        GO TO 500
350     WRITE(ERRMSG,360) OBSFL(1:LENOBS)
360     FORMAT(' Cannot open observation value file ',A,'.')
        CALL WRTERR

500     IF(IERR.NE.0) GO TO 9998
        ILINE=0
        WRITE(ERRMSG,505) PARFL(1:LENPAR)
505     FORMAT(' Checking parameter value file ',A,' ----->')
        CALL WRTERR
        IERR=IERR-1
510     ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        AALINE=' Line '//ATEMP(1:INDEX(ATEMP,' ')-1)//
     +  ' of parameter value file '//PARFL(1:LENPAR)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(20,'(A)',ERR=520,END=700) CLINE
        GO TO 550
520     WRITE(ERRMSG,530) AALINE(1:MMLINE)
530     FORMAT(A,'cannot read line.')
        CALL WRTERR
        IF(ILINE.EQ.1) GO TO 9998
        GO TO 510
550     IF(ILINE.EQ.1) THEN
          CALL LOWCAS(CLINE)
          CALL LNSPL1(IFAIL,2,L,R,CLINE)
          IF(IFAIL.NE.0) THEN
            WRITE(ERRMSG,560) AALINE(1:MMLINE)
560         FORMAT(A,'line must contain character variables PRECIS',
     +      ' and DPOINT.')
            CALL WRTERR
            GO TO 9998
          END IF
          PRECIS=CLINE(L(1):R(1))
          IF((PRECIS.NE.'single').AND.
     +       (PRECIS.NE.'double')) THEN
             WRITE(ERRMSG,561) AALINE(1:MMLINE)
561          FORMAT(A,'character variable PRECIS must be "single"',
     +       ' or "double".')
             CALL WRTERR
          END IF
          DPOINT=CLINE(L(2):R(2))
          IF((DPOINT.NE.'point').AND.
     +       (DPOINT.NE.'nopoint')) THEN
             WRITE(ERRMSG,562) AALINE(1:MMLINE)
562          FORMAT(A,'character variable DPOINT must be "point" or ',
     +       '"nopoint".')
             CALL WRTERR
          END IF
          IF(IERR.NE.0) GO TO 9998
          GO TO 510
        END IF
        CALL LNSPL1(IFAIL,4,L,R,CLINE)
        IF(IFAIL.EQ.-1) GO TO 510
        IF(IFAIL.NE.0) THEN
          WRITE(ERRMSG,563) AALINE(1:MMLINE)
563       FORMAT(A,'line must have 4 entries.')
          CALL WRTERR
          GO TO 510
        END IF
        IF(R(1)-L(1)+1.GT.12) THEN
          WRITE(ERRMSG,570) AALINE(1:MMLINE),CLINE(L(1):R(1))
570       FORMAT(A,'parameter name "',A,'" greater than 12 characters ',
     +    'long.')
          CALL WRTERR
          GO TO 510
        END IF
        TPAR=CLINE(L(1):R(1))
        CALL LOWCAS(TPAR)
        IF(NPAR.EQ.0) THEN
          APAR(1)=TPAR
          NPAR=1
        ELSE
          CALL WHICH1(IFAIL,NPAR,II,APAR,TPAR)
          IF(IFAIL.NE.0) THEN
            NPAR=NPAR+1
            IF(NPAR.GT.MAXPAR) THEN
              WRITE(ERRMSG,585) AMXPAR(:NBLNK(AMXPAR))
585           FORMAT(' Cannot continue execution: PESTGEN can handle ',
     +        'a maximum of ',A,' parameters.')
              CALL WRTERR
              GO TO 9998
            END IF
            APAR(NPAR)=TPAR
          ELSE
            WRITE(ERRMSG,590) AALINE(1:MMLINE),TPAR(:NBLNK(TPAR))
590         FORMAT(A,'parameter "',A,'" cited previously.')
            CALL WRTERR
            GO TO 510
          END IF
        END IF
        ATEMP=CLINE(L(2):R(2))
        READ(ATEMP,'(F30.0)',ERR=610) PVAL
        GO TO 650
610     WRITE(ERRMSG,620) AALINE(1:MMLINE),TPAR(:NBLNK(TPAR))
620     FORMAT(A,'cannot read value of parameter "',A,'".')
        CALL WRTERR
650     ATEMP=CLINE(L(3):R(3))
        READ(ATEMP,'(F30.0)',ERR=670) SCALE
        GO TO 680
670     WRITE(ERRMSG,675) AALINE(1:MMLINE), TPAR(:NBLNK(TPAR))
675     FORMAT(A,'cannot read scale for parameter "',A,'".')
        CALL WRTERR
680     ATEMP=CLINE(L(4):R(4))
        READ(ATEMP,'(F30.0)',ERR=690) OFFSET
        GO TO 695
690     WRITE(ERRMSG,685) AALINE(1:MMLINE),TPAR(:NBLNK(TPAR))
685     FORMAT(A,'cannot read offset for parameter "',A,'".')
        CALL WRTERR
695     GO TO 510

700     IF(IERR.NE.0) GO TO 9998
        IF(NPAR.EQ.0) THEN
          WRITE(ERRMSG,710) PARFL(1:LENPAR)
710       FORMAT(' No parameters found in file ',A,'.')
          CALL WRTERR
          GO TO 9998
        END IF
        REWIND(UNIT=20,ERR=720)
        GO TO 750
720     WRITE(ERRMSG,730) PARFL(1:LENPAR)
730     FORMAT(' Cannot rewind file ',A,'.')
        CALL WRTERR
        GO TO 9998
750     WRITE(6,751,ERR=9700)
751     FORMAT(' No errors encountered.')

C -- THE OBSERVATION VALUE FILE IS READ AND CHECKED

        WRITE(6,*,ERR=9700)
        WRITE(ERRMSG,755) OBSFL(1:LENOBS)
755     FORMAT(' Checking observation value file ',A,' ----->')
        CALL WRTERR
        IERR=IERR-1
        ILINE=0
760     ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        AALINE=' Line '//ATEMP(1:INDEX(ATEMP,' ')-1)//
     +  ' of observation value file '//OBSFL(1:LENOBS)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=780,END=1000) CLINE
        GO TO 800
780     WRITE(ERRMSG,530) AALINE(1:MMLINE)
        CALL WRTERR
        IF(ILINE.EQ.1) GO TO 9998
        GO TO 760
800     CALL LNSPL1(IFAIL,2,L,R,CLINE)
        IF(IFAIL.EQ.-1) GO TO 760
        IF(IFAIL.NE.0) THEN
          WRITE(ERRMSG,810) AALINE(1:MMLINE)
810       FORMAT(A,'line must have 2 entries.')
          CALL WRTERR
          GO TO 760
        END IF
        IF(R(1)-L(1)+1.GT.20) THEN
          WRITE(ERRMSG,820) AALINE(1:MMLINE),CLINE(L(1):R(1))
820       FORMAT(A,'observation name "',A,
     +    '" greater than 20 characters long.')
          CALL WRTERR
          GO TO 760
        END IF
        OTPAR=CLINE(L(1):R(1))
        CALL LOWCAS(OTPAR)
        IF(NOBS.EQ.0) THEN
          AOBS(1)=OTPAR
          NOBS=1
        ELSE
          CALL WHICH1(IFAIL,NOBS,II,AOBS,OTPAR)
          IF(IFAIL.NE.0) THEN
            NOBS=NOBS+1
            IF(NOBS.GT.MAXOBS) THEN
              WRITE(ERRMSG,840) AMXOBS(:NBLNK(AMXOBS))
840           FORMAT(' Cannot continue execution: PESTGEN can handle ',
     +        'a maximum of ',A,' observations.')
              CALL WRTERR
              GO TO 9998
            END IF
            AOBS(NOBS)=OTPAR
          ELSE
            WRITE(ERRMSG,850) AALINE(1:MMLINE),OTPAR(:NBLNK(OTPAR))
850         FORMAT(A,'observation "',A,'" cited previously.')
            CALL WRTERR
            GO TO 760
          END IF
        END IF
        ATEMP=CLINE(L(2):R(2))
        READ(ATEMP,'(F30.0)',ERR=860) OBSVAL
        GO TO 920
860     WRITE(ERRMSG,870) AALINE(1:MMLINE),OTPAR(:NBLNK(OTPAR))
870     FORMAT(A,'cannot read value of observation "',A,'".')
        CALL WRTERR
920     GO TO 760

1000    IF(IERR.NE.0) GO TO 9998
        IF(NOBS.EQ.0) THEN
          WRITE(ERRMSG,1010) OBSFL(1:LENOBS)
1010      FORMAT(' No observations found in file ',A,'.')
          CALL WRTERR
          GO TO 9998
        END IF
        REWIND(UNIT=30,ERR=1020)
        GO TO 1050
1020    WRITE(ERRMSG,730) OBSFL(1:LENOBS)
        CALL WRTERR
        GO TO 9998
1050    WRITE(6,751,ERR=9700)

        WRITE(6,*,ERR=9700)
        WRITE(ERRMSG,1055) CASFL(1:LENCAS)
1055    FORMAT(' Writing PEST control file ',A,' ----->')
        CALL WRTERR
        IERR=IERR-1
#ifdef UNICOS
        OPEN(UNIT=40,FILE=CASFL,STATUS='UNKNOWN',RECL=1000,
     +  ERR=1070)
#else
        OPEN(UNIT=40,FILE=CASFL,STATUS='UNKNOWN',ERR=1070)
#endif
        GO TO 1100
1070    WRITE(ERRMSG,1080) CASFL(1:LENCAS)
1080    FORMAT(' Cannot open file ',A,' for output.')
        CALL WRTERR
        GO TO 9998

C -- THE PEST CONTROL FILE IS WRITTEN

1100    WRITE(40,'(A)',ERR=9960) 'pcf'
        WRITE(40,'(A)',ERR=9960) '* control data'
        WRITE(40,'(A)',ERR=9960) 'restart  estimation'
        WRITE(40,1105,ERR=9960) NPAR,NOBS,NPAR,0,1
1105    FORMAT(I5,1X,I5,1X,I5,1X,I5,1X,I5)
        WRITE(40,1110,ERR=9960) 1,1,PRECIS(1:6),DPOINT(1:7)
1110    FORMAT(I5,1X,I5,1X,A,1X,A,' 1   0   0')
        WRITE(40,'(A)',ERR=9960)'  5.0   2.0   0.3  0.03    10'
        WRITE(40,'(A)',ERR=9960)'  3.0   3.0 0.001  0'
        WRITE(40,'(A)',ERR=9960)'  0.1'
        WRITE(40,'(A)',ERR=9960)'   30  0.01     3     3  0.01     3'
        WRITE(40,'(A)',ERR=9960)'    1     1     1'
        WRITE(40,1120,ERR=9960)
1120    FORMAT('* singular value decomposition')
        WRITE(40,1130)
1130    FORMAT(' 1')
        WRITE(40,1135)
1135    FORMAT(' 9999   5E-7')
        WRITE(40,1140)
1140    FORMAT(' 0')
        WRITE(40,1190,ERR=9960)
1190    FORMAT('* parameter groups')
        DO 1200 I=1,NPAR
        WRITE(40,1210,ERR=9960) APAR(I)
1210    FORMAT(A12,' relative 0.01  0.0  switch  2.0 parabolic')
1200    CONTINUE

        WRITE(40,1250,ERR=9960)
1250    FORMAT('* parameter data')
        READ(20,*)
        DO 1300 I=1,NPAR
1251    READ(20,'(A)',ERR=9800,END=9800) CLINE
        CALL LNSPL1(IFAIL,4,L,R,CLINE)
        IF(IFAIL.EQ.-1) GO TO 1251
        IF(IFAIL.NE.0) GO TO 9800
        TPAR=CLINE(L(1):R(1))
        CALL LOWCAS(TPAR)
        ATEMP=CLINE(L(2):R(2))
        READ(ATEMP,'(F30.0)',ERR=9800) PVAL
        ATEMP=CLINE(L(3):R(3))
        READ(ATEMP,'(F30.0)',ERR=9800) SCALE
        ATEMP=CLINE(L(4):R(4))
        READ(ATEMP,'(F30.0)',ERR=9800) OFFSET
        LBOUND=DMIN1(-1.0D10,PVAL)
        UBOUND=DMAX1(1.0D10,PVAL)
        WRITE(40,1260,ERR=9960) TPAR,PVAL,LBOUND,
     +  UBOUND,TPAR,SCALE,OFFSET
1260    FORMAT(A12,' none relative ',1PG14.6,1X,1PG14.6,1X,1PG14.6,
     +  1X,A12,1X,1PG13.5,1X,1PG13.5,'  1')
1300    CONTINUE

        WRITE(40,1310,ERR=9960)
1310    FORMAT('* observation groups')
        WRITE(40,1320,ERR=9960)
1320    FORMAT('obsgroup')


        WRITE(40,1350,ERR=9960)
1350    FORMAT('* observation data')
        DO 1400 I=1,NOBS
1351    READ(30,'(A)',ERR=9850,END=9850) CLINE
        CALL LNSPL1(IFAIL,2,L,R,CLINE)
        IF(IFAIL.EQ.-1) GO TO 1351
        IF(IFAIL.NE.0) GO TO 9850
        OTPAR=CLINE(L(1):R(1))
        CALL LOWCAS(OTPAR)
        ATEMP=CLINE(L(2):R(2))
        READ(ATEMP,'(F30.0)',ERR=9850) OBSVAL
        WRITE(40,1360,ERR=9960) OTPAR,OBSVAL
1360    FORMAT(A20,1X,1PG14.6,'  1.0  obsgroup')
1400    CONTINUE

        WRITE(40,1500,ERR=9960)
1500    FORMAT('* model command line')
        WRITE(40,1520,ERR=9960)
1520    FORMAT('model')

        WRITE(40,1600,ERR=9960)
1600    FORMAT('* model input/output')
        WRITE(40,1620,ERR=9960)
1620    FORMAT('model.tpl  model.inp')
        WRITE(40,1630,ERR=9960)
1630    FORMAT('model.ins  model.out')

        WRITE(40,1700,ERR=9960)
1700    FORMAT('* prior information')

        WRITE(6,1800,ERR=9700)
1800    FORMAT(' File written ok.')
        GO TO 9999

C -- SOME ERROR MESSAGES ....

9700    CALL EXIT(100)
9800    WRITE(6,9810,ERR=9700) PARFL(1:LENPAR)
9810    FORMAT(' Error re-reading parameter value file ',A,'.')
        CALL WRTERR
        GO TO 9998
9850    WRITE(6,9860,ERR=9700) OBSFL(1:LENOBS)
9860    FORMAT(' Error re-reading observation value file ',A,'.')
        CALL WRTERR
        GO TO 9998
9900    WRITE(6,9910,ERR=9700)
9910    FORMAT(' PESTGEN is run using the command:',/)
        WRITE(6,9920,ERR=9700)
9920    FORMAT('     pestgen case parfile obsfile',/,/,
     +  ' where',/)
        WRITE(6,9930,ERR=9700)
9930    FORMAT('     "case" is the PEST casename (no extension),')
        WRITE(6,9940,ERR=9700)
9940    FORMAT('     "parfile" is the name of a parameter value ',
     +  'file, and ')
        WRITE(6,9950,ERR=9700)
9950    FORMAT('     "obsfile" is the name of an observation value ',
     +  'file.')
        GO TO 9998
9960    WRITE(ERRMSG,9970) CASFL(1:LENCAS)
9970    FORMAT(' Cannot write PEST control file ',A,': disk full?')
        CALL WRTERR
        GO TO 9998
9998    WRITE(6,*,ERR=9700)
        CALL EXIT(50)
9999    WRITE(6,*,ERR=9700)
        CALL EXIT(0)
        END


        SUBROUTINE EXTCHK(EXTLEN,LENIN,INFLE)

C -- SUBROUTINE EXTCHK DETERMINES THE LOCATION OF THE FILENAME EXTENSION

        INTEGER EXTLEN,LENIN,I
        CHARACTER*(*) INFLE

        DO 10 I=LENIN,1,-1
        IF(INFLE(I:I).EQ.'.') GO TO 30
#ifdef UNIX
        IF(INFLE(I:I).EQ.'/') GO TO 50
#else
        IF(INFLE(I:I).EQ.char(92)) GO TO 50
#endif
10      CONTINUE
        EXTLEN=LENIN+1
        RETURN
30      EXTLEN=I
        RETURN
50      EXTLEN=LENIN+1
        RETURN
        END


        SUBROUTINE WRTERR

C -- SUBROUTINE WRTERR FORMATS AND WRITES AN ERROR MESSAGE

        INTEGER NBLNK
        INTEGER IERR,J,JEND,I,NBLC,ITAKE,MCASE
        CHARACTER*420 ERRMSG

        COMMON /IERR/IERR,MCASE
        COMMON /ERRMSG/ERRMSG

        IERR=IERR+1
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


        SUBROUTINE WRTINT(ATEMP,IVAL)

C -- SUBROUTINE WRTINT WRITES AN INTEGER TO A STRING

        INTEGER IVAL
        CHARACTER*(*) ATEMP

        WRITE(ATEMP,'(I10)')IVAL
        CALL SHFTL1(ATEMP)
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



        SUBROUTINE SHFTL1(AA)

C -- SUBROUTINE SHFTL1 REMOVES LEADING BLANKS FROM A STRING

        INTEGER L,I,J,II
        CHARACTER*(*) AA

        L=LEN(AA)
        DO 10 I=1,L
        IF(AA(I:I).NE.' ') GO TO 50
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

