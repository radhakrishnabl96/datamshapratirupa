        SUBROUTINE READ_PEST_DATA(IFAIL,IRESTART,P1STRT,PFILE,FLENME)

#ifdef BEO
        USE BEOPEST
#endif
        USE PESTDATA
        IMPLICIT NONE

        INTEGER        :: IFAIL
        INTEGER        :: IRESTART
        INTEGER        :: P1STRT
        INTEGER        :: PFILE
        CHARACTER*(*)  :: FLENME

        LOGICAL          :: SKIPLINE
        INTEGER          :: CTLUNIT,ILINE,IERR,JFAIL,NOBSGP1,
     +                      ITEMP,I,IJAC,J,ICOUNT,II,LPOS,IG,
     +                      JCOV,NN,JUNIT,IFIND,NNDIM1,NNDIM2,
     +                      NNCODE,IUNIT,LU,ILINE0,IGTARG
        INTEGER             AUIFLAG,SENREUSEFLAG
        INTEGER          :: IES,IPAR
        INTEGER          :: QUICKPRIOR,JFLAG
        INTEGER          :: NTT,ABSIND
        INTEGER          :: BEOFLAG
        INTEGER          :: JJ
        INTEGER          :: NNN,III,NN1
        INTEGER          :: NEQQ,JJFLAG
        INTEGER          :: LW(11),RW(11)
        DOUBLE PRECISION :: RTEMP,PHIAB
        CHARACTER*2      :: DUMFILE
        CHARACTER*10     :: ALINE
        CHARACTER*10     :: ADO
        CHARACTER*12     :: ATEMP12
        CHARACTER*20     :: ATEMP,ATEMP1,ATEMP20
        CHARACTER*20     :: VARTEXT,UPVARTEXT
        CHARACTER*25     :: ATEMP25
        CHARACTER*50     :: ATEMP50
        CHARACTER*200    :: AFILE,AFILE0,SCHEDULEFILE
        CHARACTER*200, ALLOCATABLE :: COVFLE(:)

#ifdef INTEL
        INTEGER  (8)     :: I8
#else
        INTEGER          :: I8
#endif

C -- Initialisation

        CTLFILE=FLENME
        RESTART=IRESTART
        P1START=P1STRT

        CTLUNIT=30
        CALL ADDQUOTE(CTLFILE,AFILE)
        ILINE=0
        IFAIL=0
        QUICKPRIOR=0
        BEOFLAG=0
#ifdef BEO
        BEOFLAG=1
#endif

C -- The PEST control file is opened.

        OPEN(UNIT=CTLUNIT,FILE=CTLFILE,STATUS='OLD',ACTION='READ',
     +  IOSTAT=IERR)
        IF(IERR.NE.0)THEN
          WRITE(ERRMSG,10) TRIM(AFILE)
10        FORMAT('Cannot open PEST control file ',A,'.')
          GO TO 9890
        END IF

C -- The header is read.

105     ILINE=ILINE+1
        READ(CTLUNIT,110,ERR=9000,END=9050) CLINE
110     FORMAT(A)
        IF(SKIPLINE(CLINE)) GO TO 105
        CALL LOWCAS(CLINE)
        IF(CLINE(1:3).NE.'pcf')THEN
          WRITE(ERRMSG,120) TRIM(AFILE)
120       FORMAT('PEST control file ',A,' should begin with ',
     +    '"pcf".')
          GO TO 9890
        END IF

C -- The control data section is read.

        CALL FIND_HEADER(JFAIL,CTLUNIT,ILINE,'* control data',CLINE,
     +  AFILE)
        IF(JFAIL.NE.0) GO TO 9890

140     ILINE=ILINE+1
        READ(CTLUNIT,110,ERR=9000,END=9050) CLINE
        IF(SKIPLINE(CLINE)) GO TO 140
        CALL LOWCAS(CLINE)
        CALL LINSPL(JFAIL,2,LW,RW,CLINE)
        IF(JFAIL.NE.0) GO TO 9100
        ATEMP=CLINE(LW(1):RW(1))
        IF(ATEMP.EQ.'restart')THEN
          RSTFLE=1
        ELSE IF(ATEMP.EQ.'norestart')THEN
          RSTFLE=0
        ELSE
          GO TO 9150
        END IF
        ATEMP=CLINE(LW(2):RW(2))
        IF(ATEMP.EQ.'estimation')THEN
          PESTMODE=1
        ELSE IF(ATEMP.EQ.'prediction')THEN
          PESTMODE=2
        ELSE IF(ATEMP(1:5).EQ.'regul')THEN
          PESTMODE=3
        ELSE IF(ATEMP(1:6).EQ.'pareto')THEN
          PESTMODE=4
        ELSE
          GO TO 9150
        END IF

142     ILINE=ILINE+1
        CALL WRITINT(ALINE,ILINE)
        NOBSGP1=0
        MAXCOMPDIM=0
        READ(CTLUNIT,110,ERR=9000,END=9050) CLINE
        IF(SKIPLINE(CLINE)) GO TO 142
        VARTEXT='fileparfile'                          ! Must be first so that filename can be case sensitive
        UPVARTEXT='FILEPARFILE'
        FILEPARFILE=' '
        CALL READ_MRUN_VAR(IFAIL,RTEMP,VARTEXT,UPVARTEXT,
     +  ILINE,AFILE,CLINE,ERRMSG,2,FILEPARFILE)
        IF(IFAIL.NE.0) GO TO 9890
        VARTEXT='nparsec'
        UPVARTEXT='NPARSEC'
        RTEMP=0.0D0             ! default value
        CALL READ_MRUN_VAR(IFAIL,RTEMP,VARTEXT,UPVARTEXT,
     +  ILINE,AFILE,CLINE,ERRMSG,1,DUMFILE)
        IF(IFAIL.NE.0) GO TO 9890
        NPARSEC=NINT(RTEMP)
        IF(NPARSEC.LT.0)THEN
          WRITE(ERRMSG,143) TRIM(UPVARTEXT),TRIM(ALINE),TRIM(AFILE)
143       FORMAT(A,' must be either zero or greater ',
     +    'at line ',A,' of file ',A,'.')
          GO TO 9890
        END IF
        VARTEXT='nparfile'
        UPVARTEXT='NPARFILE'
        RTEMP=0.0D0             ! default value
        CALL READ_MRUN_VAR(IFAIL,RTEMP,VARTEXT,UPVARTEXT,
     +  ILINE,AFILE,CLINE,ERRMSG,1,DUMFILE)
        IF(IFAIL.NE.0) GO TO 9890
        NPARFILE=NINT(RTEMP)
        IF(NPARFILE.LT.0)THEN
          WRITE(ERRMSG,143) TRIM(UPVARTEXT),TRIM(ALINE),TRIM(AFILE)
          GO TO 9890
        END IF
        VARTEXT='nequation'
        UPVARTEXT='NEQUATION'
        RTEMP=0.0D0             ! default value
        CALL READ_MRUN_VAR(IFAIL,RTEMP,VARTEXT,UPVARTEXT,
     +  ILINE,AFILE,CLINE,ERRMSG,1,DUMFILE)
        IF(IFAIL.NE.0) GO TO 9890
        NEQUATION=NINT(RTEMP)
        IF(NEQUATION.LT.0)THEN
          WRITE(ERRMSG,143) TRIM(UPVARTEXT),TRIM(ALINE),TRIM(AFILE)
          GO TO 9890
        END IF

        CALL LINSPL(JFAIL,5,LW,RW,CLINE)
        IF(JFAIL.NE.0)THEN
          CALL LINSPL(JFAIL,4,LW,RW,CLINE)
          IF(JFAIL.NE.0) GO TO 9100
          NOBSGP1=-999
          NOBSGP=1
          MAXCOMPDIM=-1
        END IF
        IF(NOBSGP1.EQ.-999)THEN
          CALL WRITINT(ALINE,ILINE)
          WRITE(ERRMSG,145) TRIM(ALINE),TRIM(AFILE)
145       FORMAT('Number of observation groups (NOBSGP) must be',
     +    ' provided on line ',A,' of file ',A,'.')
          GO TO 9890
        END IF
        CALL NUMRD(JFAIL,0,LW(1),RW(1),NPAR,RTEMP,CLINE)
        IF(JFAIL.NE.0) GO TO 9150
        CALL NUMRD(JFAIL,0,LW(2),RW(2),NOBS,RTEMP,CLINE)
        IF(JFAIL.NE.0) GO TO 9150
        CALL NUMRD(JFAIL,0,LW(3),RW(3),NPARGP,RTEMP,CLINE)
        IF(JFAIL.NE.0) GO TO 9150
        CALL NUMRD(JFAIL,0,LW(4),RW(4),NPRIOR,RTEMP,CLINE)
        IF(JFAIL.NE.0) GO TO 9150
        IF(NPRIOR.LT.0)THEN
          QUICKPRIOR=1
          NPRIOR=ABS(NPRIOR)
        END IF
        IF(NOBSGP1.NE.-999) THEN
          CALL NUMRD(JFAIL,0,LW(5),RW(5),NOBSGP,RTEMP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
        END IF
        IF((NPAR.LE.0).OR.(NOBS.LE.0).OR.(NPARGP.LE.0).OR.
     +     (NOBSGP.LE.0)) GO TO 9150
        IF(MAXCOMPDIM.NE.-1)THEN
          CALL LINSPL(JFAIL,6,LW,RW,CLINE)
          IF(JFAIL.NE.0)THEN
            MAXCOMPDIM=0
          ELSE
            CALL NUMRD(JFAIL,0,LW(6),RW(6),MAXCOMPDIM,RTEMP,CLINE)
            IF(JFAIL.NE.0) GO TO 9150
            CALL LINSPL(JFAIL,7,LW,RW,CLINE)
            IF(JFAIL.EQ.0)THEN
              CALL NUMRD(JFAIL,1,LW(7),RW(7),ITEMP,DERZEROLIM,CLINE)
              IF(JFAIL.NE.0) GO TO 9150
            END IF
          END IF
        END IF
        IF(NPAR.GE.1000000)THEN
          WRITE(ERRMSG,141)
141       FORMAT('Number of parameters exceeds upper limit of 1000000.')
          GO TO 9890
        END IF
        IF(MAXCOMPDIM.LT.0) MAXCOMPDIM=0
        IF(PESTMODE.EQ.4)THEN
          IF(NOBSGP.LE.1)THEN
            WRITE(ERRMSG,146)
146         FORMAT('If PEST is run in pareto mode then number ',
     +      'of observation groups must be greater than 1.')
            GO TO 9890
          END IF
        END IF

160     ILINE=ILINE+1
        READ(CTLUNIT,110,ERR=9000,END=9050) CLINE
        IF(SKIPLINE(CLINE)) GO TO 160
        CALL LOWCAS(CLINE)
        IREFOBS=999
        IREFOBS_PAUSE=0
162     CONTINUE
        II=INDEX(CLINE,'noobsreref')
        IF(II.NE.0)THEN
          IREFOBS=0
          DO JJ=II,LEN_TRIM(CLINE)
            IF(CLINE(JJ:JJ).EQ.' ') EXIT
            CLINE(JJ:JJ)=' '
          END DO
          GO TO 162
        END IF
164     CONTINUE
        II=INDEX(CLINE,'obsreref')
        IF(II.NE.0)THEN
          IF(IREFOBS.EQ.0)THEN
            WRITE(ERRMSG,161)
161         FORMAT('Contradictory values have been supplied for ',
     +      'the "obsreref" variable.')
            GO TO 9890
          END IF
          IREFOBS=1
          IF(CLINE(II:II+8).EQ.'obsreref_')THEN
            JJ=INDEX(CLINE(II:),' ')
            IF(JJ.EQ.10)THEN
              WRITE(ERRMSG,165)
165           FORMAT('Cannot read integer following "obseref_" string.')
              GO TO 9890
            END IF
            ATEMP=CLINE(II+9:II+JJ-2)
            CALL INTREAD(IFAIL,ATEMP,IREFOBS_PAUSE)
            IF(IFAIL.NE.0)THEN
              WRITE(ERRMSG,165)
              GO TO 9890
            ELSE
              IF(IREFOBS_PAUSE.LE.0)THEN
                WRITE(ERRMSG,166)
166             FORMAT('Integer following "obsreref_" string must be ',
     +          'positive.')
                GO TO 9890
              END IF
            END IF
            CLINE(II:II+JJ-2)=' '
            IF(P1START.NE.0)THEN
              WRITE(ERRMSG,167)
167           FORMAT('When PEST is started with the "/p1" switch a ',
     +        'number_of_seconds integer must not follow the ',
     +        '"obsreref" switch in the "control data" section ',
     +        'of the PEST control file.')
              go to 9890
            END IF
          ELSE
            CLINE(II:II+8)=' '
          END IF
          GO TO 164
        END IF
        IF(IREFOBS.EQ.999) IREFOBS=0
        ORR_NOT_FIRST=0
1621    CONTINUE
        II=INDEX(CLINE,'orr_not_first')
        IF(II.NE.0)THEN
          ORR_NOT_FIRST=1
          DO JJ=II,LEN_TRIM(CLINE)
            IF(CLINE(JJ:JJ).EQ.' ') EXIT
            CLINE(JJ:JJ)=' '
          END DO
          GO TO 1621
        END IF
        IF(ORR_NOT_FIRST.NE.0)THEN
          IF(IREFOBS.EQ.0)THEN
            CALL WRITINT(ALINE,ILINE)
            WRITE(ERRMSG,1622) TRIM(ALINE)
1622        FORMAT('"ORR_NOT_FIRST appears on line ',A,' of PEST ',
     +      'control file. However observation re-referencing is ',
     +      'not switched on.')
            GO TO 9890
          END IF
        END IF
        CALL LINSPL(JFAIL,4,LW,RW,CLINE)
        IF(JFAIL.NE.0) GO TO 9100
        CALL NUMRD(JFAIL,0,LW(1),RW(1),NTPLFLE,RTEMP,CLINE)
        IF(JFAIL.NE.0) GO TO 9150
        CALL NUMRD(JFAIL,0,LW(2),RW(2),NINSFLE,RTEMP,CLINE)
        IF(JFAIL.NE.0) GO TO 9150
        IF((NTPLFLE.LE.0).OR.(NINSFLE.LE.0)) GO TO 9150
        ATEMP=CLINE(LW(3):RW(3))
        CALL LOWCAS(ATEMP)
        IF(ATEMP.EQ.'double')THEN
          PRECIS=1
        ELSE IF(ATEMP.EQ.'single')THEN
          PRECIS=0
        ELSE
          GO TO 9150
        END IF
        ATEMP=CLINE(LW(4):RW(4))
        CALL LOWCAS(ATEMP)
        IF(ATEMP.EQ.'point')THEN
          NOPNT=0
        ELSE IF(ATEMP.EQ.'nopoint') THEN
          NOPNT=1
        ELSE
          GO TO 9150
        END IF
        CALL LINSPL(JFAIL,7,LW,RW,CLINE)
        IF(JFAIL.NE.0)THEN
          NUMCOM=1
          JACFILE=0
          MESSFILE=0
        ELSE
          CALL NUMRD(JFAIL,0,LW(5),RW(5),NUMCOM,RTEMP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          IF(NUMCOM.LE.0) GO TO 9150
          CALL NUMRD(JFAIL,0,LW(6),RW(6),JACFILE,RTEMP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          IF((JACFILE.NE.0).AND.(ABS(JACFILE).NE.1).AND.(JACFILE.NE.2))
     +    GO TO 9150
          IF(JACFILE.EQ.-1)THEN
            JACFILE=1
            BINJACFILE=1
          END IF
          CALL NUMRD(JFAIL,0,LW(7),RW(7),MESSFILE,RTEMP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          IF((MESSFILE.NE.0).AND.(MESSFILE.NE.1)) GO TO 9150
          IF(NUMCOM.GT.1)THEN
            IF(ORR_NOT_FIRST.GT.0)THEN
              WRITE(ERRMSG,1623)
1623          FORMAT('"ORR_NOT_FIRST cannot be implemented where ',
     +        'more than one model command is used for derivatives ',
     +        'calculation.')
              GO TO 9890
            END IF
          END IF
        END IF

        IF(IRESTART.GT.0)THEN
          IF(IRESTART.NE.1)THEN
            IF((JACFILE.NE.0).AND.(NPRIOR.GT.1))THEN
              IF(MAXCOMPDIM.GT.1)THEN
                WRITE(ERRMSG,199)
199             FORMAT('If MAXCOMPDIM is greater than 1, and if ',
     +          'NPRIOR is greater than 1 and if ',
     +          'derivatives are read from an external derivatives ',
     +          'file then the only allowed PEST restart setting ',
     +          'is "/r".')
                GO TO 9890
              END IF
            END IF
          END IF
        END IF

200     ILINE=ILINE+1
        READ(CTLUNIT,110,ERR=9000,END=9050) CLINE
        IF(SKIPLINE(CLINE)) GO TO 200
        CALL LINSPL(JFAIL,5,LW,RW,CLINE)
        IF(JFAIL.NE.0) GO TO 9100
        CALL NUMRD(JFAIL,1,LW(1),RW(1),ITEMP,RLAMBDA1,CLINE)
        IF(JFAIL.NE.0) GO TO 9150
        CALL NUMRD(JFAIL,1,LW(2),RW(2),ITEMP,RLAMFAC,CLINE)
        IF(JFAIL.NE.0) GO TO 9150
        CALL NUMRD(JFAIL,1,LW(3),RW(3),ITEMP,PHIRATSUF,CLINE)
        IF(JFAIL.NE.0) GO TO 9150
        CALL NUMRD(JFAIL,1,LW(4),RW(4),ITEMP,PHIREDLAM,CLINE)
        IF(JFAIL.NE.0) GO TO 9150
        CALL NUMRD(JFAIL,0,LW(5),RW(5),NUMLAM,RTEMP,CLINE)
        IF(JFAIL.NE.0) GO TO 9150
        IF(ABS(RLAMBDA1).LT.1.0D-101)RLAMBDA1=0.0D0
        IF((RLAMBDA1.EQ.0.0D0).AND.(NUMLAM.NE.1))THEN
          CALL WRITINT(ALINE,ILINE)
          WRITE(ERRMSG,210) TRIM(ALINE),TRIM(AFILE)
210       FORMAT('If RLAMBDA1 is supplied as zero then NUMLAM ',
     +    'must be one at line ',A,' of file ',A,'.')
          GO TO 9890
        END IF
        IF(ABS(RLAMFAC).LE.1.0D0)THEN
          CALL WRITINT(ALINE,ILINE)
          WRITE(ERRMSG,2101) TRIM(ALINE),TRIM(AFILE)
2101      FORMAT('The absolute value of RLAMFAC must be greater ',
     +    'than 1.0 at line ',A,' of file ',A,'.')
          GO TO 9890
        END IF
        IF(RLAMFAC.LT.0.0)THEN
          RLAMFAC_ADJUST=1
          RLAMFACEXP=-RLAMFAC
        END IF
        CALL LOWCAS(CLINE)

        VARTEXT='run_slow_fac'
        UPVARTEXT='RUN_SLOW_FAC'
        RUN_SLOW_FAC_1=-1.1D100          !default value
        CALL READ_MRUN_VAR(IFAIL,RUN_SLOW_FAC_1,VARTEXT,UPVARTEXT,
     +  ILINE,AFILE,CLINE,ERRMSG,1,DUMFILE)
        IF(IFAIL.NE.0) GO TO 9890
#ifdef PARALLEL
        IF(RUN_SLOW_FAC_1.LT.-1.0D10) THEN
          RUN_SLOW_FAC_1=-1.1D100
        ELSE
          IF(RUN_SLOW_FAC_1.LT.1.199999)THEN
            CALL WRITINT(ALINE,ILINE)
            WRITE(ERRMSG,2106) TRIM(ALINE),TRIM(AFILE)
2106        FORMAT('RUN_SLOW_FAC must be greater than 1.2 at ',
     +      'line ',A,' of file ',A,'.')
            GO TO 9890
          END IF
        END IF
#endif

C -- Other variables are read which are not used by PEST and BEOPEST. but they are read anyway for the
C    sake of compatibility with PEST_HP.

        VARTEXT='run_abandon_fac'
        UPVARTEXT='RUN_ABANDON_FAC'
        RUN_ABANDON_FAC=0.0D0           !default value
        CALL READ_MRUN_VAR(IFAIL,RUN_ABANDON_FAC,VARTEXT,UPVARTEXT,
     +  ILINE,AFILE,CLINE,ERRMSG,1,DUMFILE)
        IF(IFAIL.NE.0) GO TO 9890

        VARTEXT='win_mrun_hours'
        UPVARTEXT='WIN_MRUN_HOURS'
        WIN_MRUN_HOURS=-1.0D0
        CALL READ_MRUN_VAR(IFAIL,WIN_MRUN_HOURS,VARTEXT,UPVARTEXT,
     +  ILINE,AFILE,CLINE,ERRMSG,1,DUMFILE)
        IF(IFAIL.NE.0) GO TO 9890
        IF(WIN_MRUN_HOURS.LE.0.0D0) WIN_MRUN_HOURS=-1.0D0

        VARTEXT='uptestlim'
        UPVARTEXT='UPTESTLIM'
        RTEMP=-1.0D0             ! default value
        CALL READ_MRUN_VAR(IFAIL,RTEMP,VARTEXT,UPVARTEXT,
     +  ILINE,AFILE,CLINE,ERRMSG,1,DUMFILE)
        IF(IFAIL.NE.0) GO TO 9890
        UPTESTLIM=NINT(RTEMP)

        VARTEXT='uptestmin'
        UPVARTEXT='UPTESTMIN'
        RTEMP=-1.0D0             ! default value
        CALL READ_MRUN_VAR(IFAIL,RTEMP,VARTEXT,UPVARTEXT,
     +  ILINE,AFILE,CLINE,ERRMSG,1,DUMFILE)
        IF(IFAIL.NE.0) GO TO 9890
        UPTESTMIN=NINT(RTEMP)

        NN=INDEX(CLINE,' nolamforgive')
        IF(NN.NE.0)THEN
          LAMFORGIVE=0
          CLINE(NN+1:NN+12)=' '
        END IF
        NN=INDEX(CLINE,' lamforgive')
        IF(NN.NE.0)THEN
          LAMFORGIVE=1
          CLINE(NN+1:NN+10)=' '
        END IF
        CALL LOWCAS(CLINE)
        NN=INDEX(CLINE,' noderforgive')
        IF(NN.NE.0)THEN
          DERFORGIVE=0
          CLINE(NN+1:NN+12)=' '
        END IF
        NN=INDEX(CLINE,' derforgive')
        IF(NN.NE.0)THEN
          DERFORGIVE=1
          CLINE(NN+1:NN+10)=' '
        END IF
        CALL LINSPL(JFAIL,6,LW,RW,CLINE)
        IF(JFAIL.NE.0) THEN
          JACUPDATE=0
        ELSE
          CALL NUMRD(JFAIL,0,LW(6),RW(6),JACUPDATE,RTEMP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          IF(JACUPDATE.LT.0)THEN
            CALL WRITINT(ALINE,ILINE)
            WRITE(ERRMSG,2102) TRIM(ALINE),TRIM(AFILE)
2102        FORMAT('JACUPDATE must be zero or positive at line ',A,
     +      ' of file ',A,'.')
            GO TO 9890
          END IF
          IF(JACUPDATE.GT.0)THEN
            IF(MAXCOMPDIM.GT.1)THEN
              CALL WRITINT(ALINE,ILINE)
              WRITE(ERRMSG,2103) TRIM(ALINE),TRIM(AFILE)
2103          FORMAT('If JACUPDATE is greater than zero at line ',A,
     +        ' of file ',A,' then MAXCOMPDIM should be set to zero, ',
     +        'thereby disabling Jacobian matrix compression.')
              GO TO 9890
            END IF
          END IF
          CALL LINSPL(JFAIL,7,LW,RW,CLINE)
          IF(JFAIL.EQ.0) GO TO 9150
        END IF

220     ILINE=ILINE+1
        READ(CTLUNIT,110,ERR=9000,END=9050) CLINE
        IF(SKIPLINE(CLINE)) GO TO 220
        CALL LOWCAS(CLINE)
        CALL GET_ABSPARMAX(JFAIL,ILINE,AFILE)
        IF(JFAIL.NE.0) GO TO 9890
        CALL LINSPL(JFAIL,3,LW,RW,CLINE)
        IF(JFAIL.NE.0) THEN
          CALL WRITINT(ALINE,ILINE)
          DO I=1,MAXABSPARMAX
            IF(ABSPARMAX(I).GT.-1.0E35) THEN
              WRITE(ERRMSG,2201) TRIM(ALINE),TRIM(AFILE)
2201          FORMAT('Insufficient non-ABSPARMAX entries on line ',
     +        A,' of file ',A,'.')
              GO TO 9890
            END IF
          END DO
          GO TO 9100
        END IF
        CALL NUMRD(JFAIL,1,LW(1),RW(1),ITEMP,RELPARMAX,CLINE)
        IF(JFAIL.NE.0) GO TO 9150
        CALL NUMRD(JFAIL,1,LW(2),RW(2),ITEMP,FACPARMAX,CLINE)
        IF(JFAIL.NE.0) GO TO 9150
        CALL NUMRD(JFAIL,1,LW(3),RW(3),ITEMP,FACORIG,CLINE)
        IF(JFAIL.NE.0) GO TO 9150
        CALL LINSPL(JFAIL,4,LW,RW,CLINE)
        IF(JFAIL.NE.0)THEN
          IBOUNDSTICK=0
          UPVECBEND=0
        ELSE
          CALL NUMRD(JFAIL,0,LW(4),RW(4),IBOUNDSTICK,RTEMP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          IF(IBOUNDSTICK.LT.0)GO TO 9150
          IF((IBOUNDSTICK.GT.0).AND.(JACFILE.NE.0))THEN
            CALL WRITINT(ALINE,ILINE)
            WRITE(ERRMSG,221) TRIM(ALINE),TRIM(AFILE)
221         FORMAT('IBOUNDSTICK must not be greater than zero '
     +      'at line ',A,' of file ',A,' if derivatives are supplied ',
     +      'externally by the model.')
            GO TO 9890
          END IF
          CALL LINSPL(JFAIL,5,LW,RW,CLINE)
          IF(JFAIL.NE.0)THEN
            UPVECBEND=0
          ELSE
            CALL NUMRD(JFAIL,0,LW(5),RW(5),UPVECBEND,RTEMP,CLINE)
            IF(JFAIL.NE.0) GO TO 9150
            IF((UPVECBEND.NE.0).AND.(UPVECBEND.NE.1))GO TO 9150
          END IF
        END IF

240     ILINE=ILINE+1
        IFIND=0
        READ(CTLUNIT,110,ERR=9000,END=9050) CLINE
        IF(SKIPLINE(CLINE)) GO TO 240

        IBOUNDSCALE=0
        CALL LOWCAS(CLINE)
        NN=INDEX(CLINE,' noboundscale')
        IF(NN.NE.0)THEN
          IBOUNDSCALE=0
          CLINE(NN+1:NN+12)=' '
          IF(INDEX(CLINE,'boundscale').NE.0) GO TO 9150
        END IF
        NN=INDEX(CLINE,' boundscale')
        IF(NN.NE.0)THEN
          IBOUNDSCALE=1
          CLINE(NN+1:NN+10)=' '
          IF(INDEX(CLINE,'boundscale').NE.0) GO TO 9150
        END IF

        VARTEXT='jcowarnthresh'
        UPVARTEXT='JCOWARNTHRESH'
        JCOWARNTHRESH=0.0D0                 !default value
        CALL READ_MRUN_VAR(IFAIL,JCOWARNTHRESH,VARTEXT,UPVARTEXT,
     +  ILINE,AFILE,CLINE,ERRMSG,1,DUMFILE)
        IF(IFAIL.NE.0) GO TO 9890
        IF(JCOWARNTHRESH.LT.0.0D0)THEN
          CALL WRITINT(ALINE,ILINE)
          WRITE(ERRMSG,2205) TRIM(ALINE),TRIM(AFILE)
2205      FORMAT('JCOWARNTHRESH must be zero or greater at ',
     +    'line ',A,' of file ',A,'.')
          GO TO 9890
        END IF
        VARTEXT='jcozerothresh'
        UPVARTEXT='JCOZEROTHRESH'
        JCOZEROTHRESH=0.0D0                 !default value
        CALL READ_MRUN_VAR(IFAIL,JCOZEROTHRESH,VARTEXT,UPVARTEXT,
     +  ILINE,AFILE,CLINE,ERRMSG,1,DUMFILE)
        IF(IFAIL.NE.0) GO TO 9890
        IF(JCOZEROTHRESH.LT.0.0D0)THEN
          CALL WRITINT(ALINE,ILINE)
          WRITE(ERRMSG,2206) TRIM(ALINE),TRIM(AFILE)
2206      FORMAT('JCOZEROTHRESH must be zero or greater at ',
     +    'line ',A,' of file ',A,'.')
          GO TO 9890
        END IF
        IF((JCOWARNTHRESH.GT.0.0D0).AND.
     +     (JCOZEROTHRESH.GT.0.0D0))THEN
          IF(JCOWARNTHRESH.GE.JCOZEROTHRESH)THEN
            WRITE(ERRMSG,2207)
2207        FORMAT(' If values are supplied for both ',
     +      'JCOWARNTHRESH and JCOZEROTHRESH, then the value ',
     +      'supplied for the latter must exceed that supplied ',
     +      'for the former.')
            GO TO 9890
          END IF
        END IF

        NNN=INDEX(CLINE,'zerosenval')
        IF(NNN.EQ.0)THEN
          IZEROSENVAL=0
        ELSE
          ATEMP50=CLINE(NNN:)
          ATEMP50=ADJUSTL(ATEMP50(11:))
          IF(ATEMP50(1:1).NE.'=') GO TO 9600
          ATEMP50=ADJUSTL(ATEMP50(2:))
          NN=INDEX(ATEMP50,' ')
          IF((NN.EQ.1).OR.(NN.EQ.0)) GO TO 9600
          NN1=INDEX(ATEMP50,CHAR(9))
          IF(NN1.NE.0)THEN
            IF(NN1.LT.NN)NN=NN1
          END IF
          ATEMP50(NN:)=' '
          CALL DREALRD1(IFAIL,ATEMP50,ZEROSENVAL)
          IF(IFAIL.NE.0) GO TO 9600
          IF(ABS(ZEROSENVAL).GE.1.0D30)THEN
            CALL WRITINT(ALINE,ILINE)
            WRITE(ERRMSG,241) TRIM(ALINE),TRIM(AFILE)
241         FORMAT('ZEROSENVAL must have an absolute value of ',
     +      'less than 1.0E30 at line ',A,' of file ',A,'.')
            GO TO 9890
          END IF
          DO III=NNN,LEN_TRIM(CLINE)
            IF(CLINE(III:III).EQ.'=')EXIT
          END DO
          DO
            III=III+1
            IF((CLINE(III:III).NE.' ').AND.
     +         (CLINE(III:III).NE.CHAR(9))) EXIT
          END DO
          DO
            III=III+1
            IF((CLINE(III:III).EQ.' ').OR.
     +         (CLINE(III:III).EQ.CHAR(9))) EXIT
          END DO
          CLINE(NNN:)=CLINE(III:)
          IZEROSENVAL=1
          IF(NUMCOM.EQ.1)THEN
            IF(IREFOBS.EQ.0)THEN
            WRITE(ERRMSG,2411)
2411        FORMAT('PEST_HP cannot be provided with a value for ',
     +      'ZEROSENVAL unless it uses multiple model commands for ',
     +      'derivatives calculation or employs observation ',
     +      're-referencing.')
            GO TO 9890
            END IF
          END IF
        END IF

        CALL LINSPL(JFAIL,1,LW,RW,CLINE)
        IF(JFAIL.NE.0) GO TO 9100
        CALL NUMRD(JFAIL,1,LW(1),RW(1),ITEMP,PHIREDSWH,CLINE)
        IF(JFAIL.NE.0) GO TO 9150

C -- Now we see if the SPLITSWH variable is present. If so, it is read and then
C    removed so that the ensuing code does not need to change.

        SPLITSWH=0.0D0
        CALL LINSPL(JFAIL,3,LW,RW,CLINE)
        IF(JFAIL.EQ.0)THEN
          CALL NUMRD(JFAIL,0,LW(2),RW(2),ITEMP,RTEMP,CLINE)
          IF(JFAIL.EQ.0)THEN
            CALL NUMRD(JFAIL,1,LW(3),RW(3),ITEMP,RTEMP,CLINE)
            IF(JFAIL.EQ.0)THEN
              SPLITSWH=RTEMP
              CLINE(LW(3):RW(3))=' '
            END IF
          END IF
        END IF

        DOAUI='noaui'
        DOSENREUSE='nosenreuse'
        AUIFLAG=0
        SENREUSEFLAG=0
        CALL LINSPL(JFAIL,2,LW,RW,CLINE)
        IF(JFAIL.EQ.0)THEN
          CALL NUMRD(JFAIL,0,LW(2),RW(2),ITEMP,RTEMP,CLINE)
          IF(JFAIL.EQ.0)THEN
            IF(ITEMP.LT.0)ITEMP=0
            NOPTSWITCH=ITEMP
            IFIND=1
          ELSE
            ADO=CLINE(LW(2):RW(2))
            CALL LOWCAS(ADO)
            IF((ADO.EQ.'aui').OR.(ADO.EQ.'noaui'))THEN
              DOAUI=ADO
              AUIFLAG=1
            ELSE IF(ADO.EQ.'auid')THEN
              DOAUI=ADO
              AUIFLAG=1
            ELSE IF((ADO.EQ.'senreuse').OR.(ADO.EQ.'nosenreuse'))THEN
              DOSENREUSE=ADO
              SENREUSEFLAG=1
            ELSE
              GO TO 9150
            END IF
            CALL LINSPL(JFAIL,3,LW,RW,CLINE)
            IF(JFAIL.EQ.0)THEN
              ADO=CLINE(LW(3):RW(3))
              CALL LOWCAS(ADO)
              IF((ADO.EQ.'aui').OR.(ADO.EQ.'noaui'))then
                IF(AUIFLAG.NE.0) GO TO 9150
                DOAUI=ADO
              ELSE IF(ADO.EQ.'auid')then
                IF(AUIFLAG.NE.0) GO TO 9150
                DOAUI=ADO
              ELSE IF((ADO.EQ.'senreuse').OR.(ADO.EQ.'nosenreuse'))
     +        THEN
                IF(SENREUSEFLAG.NE.0) GO TO 9150
                DOSENREUSE=ADO
              ELSE
                GO TO 9150
              END IF
            END IF
          END IF
          IF(IFIND.EQ.1)THEN
            CALL LINSPL(JFAIL,3,LW,RW,CLINE)
            IF(JFAIL.EQ.0)THEN
              ADO=CLINE(LW(3):RW(3))
              CALL LOWCAS(ADO)
              IF((ADO.EQ.'aui').or.(ADO.EQ.'noaui'))then
                DOAUI=ADO
                AUIFLAG=1
              ELSE IF(ADO.EQ.'auid')THEN
                DOAUI=ADO
                AUIFLAG=1
              ELSE IF((ADO.EQ.'senreuse').OR.(ADO.EQ.'nosenreuse'))
     +        THEN
                DOSENREUSE=ADO
                SENREUSEFLAG=1
              ELSE
                GO TO 9150
              END IF
              CALL LINSPL(JFAIL,4,LW,RW,CLINE)
              IF(JFAIL.EQ.0)THEN
                ADO=CLINE(LW(4):RW(4))
                CALL LOWCAS(ADO)
                IF((ADO.EQ.'aui').OR.(ADO.EQ.'noaui'))then
                  IF(AUIFLAG.NE.0) GO TO 9150
                  DOAUI=ADO
                ELSE IF(ADO.EQ.'auid')THEN
                  IF(AUIFLAG.NE.0) GO TO 9150
                  DOAUI=ADO
                ELSE IF((ADO.EQ.'senreuse').OR.(ADO.EQ.'nosenreuse'))
     +          THEN
                  IF(SENREUSEFLAG.NE.0) GO TO 9150
                  DOSENREUSE=ADO
                ELSE
                  GO TO 9150
                END IF
              END IF
            END IF
          END IF
        END IF
        DOSENREUSE=ADJUSTL(DOSENREUSE)
        DOAUI=ADJUSTL(DOAUI)
        IF((JACFILE.NE.0).AND.(DOSENREUSE.EQ.'senreuse'))THEN
          WRITE(ERRMSG,249)
249       FORMAT('If JACFILE is non-zero (and hence external ',
     +    'derivatives are supplied), DOSENREUSE must be set to ',
     +    '"nosenreuse".')
          GO TO 9890
        END IF

250     ILINE=ILINE+1
        READ(CTLUNIT,110,ERR=9000,END=9050) CLINE
        IF(SKIPLINE(CLINE)) GO TO 250

C -- Before we do anything else we look for HARDSTOPHOURS and SOFTSTOPHOURS
C    and remove reference to these from the line.

        HARDSTOPHOURS=-1.1D100
        SOFTSTOPHOURS=-1.1D100
        CALL READ_STOPHOURS(IFAIL,ILINE,1,HARDSTOPHOURS,AFILE,
     +  CLINE,ERRMSG)
        IF(IFAIL.NE.0) GO TO 9890
        CALL READ_STOPHOURS(IFAIL,ILINE,0,SOFTSTOPHOURS,AFILE,
     +  CLINE,ERRMSG)
        IF(IFAIL.NE.0) GO TO 9890
        IF(HARDSTOPHOURS.GT.0.0D0)THEN
          IF(SOFTSTOPHOURS.GT.0.0D0)THEN
            CALL WRITINT(ALINE,ILINE)
            WRITE(ERRMSG,251) TRIM(ALINE),TRIM(AFILE)
251         FORMAT('Either "HARDSTOPHOURS" or ',
     +      '"SOFTSTOPHOURS" can appear on line ',A,' of file ',A,
     +      ', but not both.')
            GO TO 9890
          END IF
        END IF

C -- Now the rest of the line is read.

        CALL LINSPL(JFAIL,6,LW,RW,CLINE)
        IF(JFAIL.NE.0) GO TO 9100
        CALL NUMRD(JFAIL,0,LW(1),RW(1),NOPTMAX,RTEMP,CLINE)
        IF(JFAIL.NE.0) GO TO 9150
        IF(NOPTMAX.EQ.0) THEN
          NOPTMAX=-1
          IFINSTOPFLAG=1
        END IF
        CALL NUMRD(JFAIL,1,LW(2),RW(2),ITEMP,PHIREDSTP,CLINE)
        IF(JFAIL.NE.0) GO TO 9150
        CALL NUMRD(JFAIL,0,LW(3),RW(3),NPHISTP,RTEMP,CLINE)
        IF(JFAIL.NE.0) GO TO 9150
        CALL NUMRD(JFAIL,0,LW(4),RW(4),NPHINORED,RTEMP,CLINE)
        IF(JFAIL.NE.0) GO TO 9150
        CALL NUMRD(JFAIL,1,LW(5),RW(5),ITEMP,RELPARSTP,CLINE)
        IF(JFAIL.NE.0) GO TO 9150
        CALL NUMRD(JFAIL,0,LW(6),RW(6),NRELPAR,RTEMP,CLINE)
        IF(JFAIL.NE.0) GO TO 9150
        ITEMP=MAX(NOPTMAX,1)
        ALLOCATE(PHIABANDON(0:ITEMP),STAT=IERR)
        IF(IERR.NE.0) GO TO 9200
        PHIABANDON=-1.1D100
        CALL LINSPL(JFAIL,7,LW,RW,CLINE)
        IF(JFAIL.EQ.0)THEN
          CALL NUMRD(JFAIL,1,LW(7),RW(7),ITEMP,PHISTOPTHRESH,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          IF(PHISTOPTHRESH.LE.0.0D0)PHISTOPTHRESH=-1.1D100
          IF((PESTMODE.EQ.2).OR.(PESTMODE.EQ.4))THEN
            IF(PHISTOPTHRESH.GT.0.0D0)THEN
              WRITE(ERRMSG,259)
259           FORMAT('Objective function termination limit ',
     +        'PHISTOPTHRESH must not be set to a positive value ',
     +        'if PEST is run in PREDICTIVE ANALYSIS or PARETO mode.')
              GO TO 9890
            END IF
          END IF
          CALL LINSPL(JFAIL,8,LW,RW,CLINE)
          IF(JFAIL.EQ.0)THEN
            CALL NUMRD(JFAIL,0,LW(8),RW(8),LASTRUN,RTEMP,CLINE)
            IF(JFAIL.NE.0) GO TO 9150
            IF((LASTRUN.NE.0).AND.(LASTRUN.NE.1)) THEN
              WRITE(ERRMSG,258)
258           FORMAT('LASTRUN control variable must be set to either ',
     +        '1 or 0 on line 9 of PEST control file.')
              GO TO 9890
            END IF
            IF(PESTMODE.EQ.4)THEN
              IF(LASTRUN.EQ.1)THEN
                WRITE(ERRMSG,2581)
2581            FORMAT('If supplied, LASTRUN must be set to zero ',
     +          'if PEST is run in Pareto mode.')
                GO TO 9890
              END IF
            END IF
            CALL SPACESUB(CLINE)
            CALL LINSPL(JFAIL,9,LW,RW,CLINE)
            IF(JFAIL.EQ.0)THEN
              CALL NUMRD(JFAIL,1,LW(9),RW(9),ITEMP,PHIAB,CLINE)
              IF(JFAIL.EQ.0)THEN
                IF(PHIAB.LE.0.0D0)PHIAB=-1.1D100
                PHIABANDON=PHIAB
                IF((PESTMODE.EQ.2).OR.(PESTMODE.EQ.4))THEN
                  IF(PHIAB.GT.0.0D0)THEN
                    WRITE(ERRMSG,257)
257                 FORMAT('Objective function abandonment limit ',
     +              'PHIABANDON must not be set to a positive value ',
     +              'if PEST is run in PREDICTIVE ANALYSIS mode ',
     +              'or PARETO mode.')
                    GO TO 9890
                  END IF
                END IF
              ELSE
                IF(NOPTMAX.LE.0)THEN
                  PHIABANDON=-1.0D100
                ELSE
                  IF(PESTMODE.EQ.4)THEN
                    WRITE(ERRMSG,12571)
12571               FORMAT('Abandonment schedule file must not ',
     +              'be supplied if PEST is run in Pareto mode.')
                    go to 9890
                  END IF
                  SCHEDULEFILE=CLINE(LW(9):RW(9))
                  CALL REMCHAR(SCHEDULEFILE,CHAR(211))
                  IUNIT=31
                  OPEN(UNIT=IUNIT,FILE=SCHEDULEFILE,STATUS='OLD',
     +            IOSTAT=IERR)
                  IF(IERR.NE.0)THEN
                    WRITE(ERRMSG,1257) TRIM(SCHEDULEFILE)
1257                FORMAT('Cannot open file ',a,' cited at line 9 of ',
     +              'PEST control file.')
                    GO TO 9890
                  END IF
                  DO I=0,NOPTMAX
                    READ(IUNIT,*,ERR=1260,END=1300) PHIABANDON(I)
                  END DO
                  GO TO 1270
1260              WRITE(ERRMSG,1265) TRIM(SCHEDULEFILE)
1265              FORMAT('Error reading PHIABANDON values from file ',
     +            a,'.')
                  GO TO 9890
1270              CONTINUE
1300              CLOSE(UNIT=IUNIT)
                  IF(I.EQ.0)THEN
                    WRITE(ERRMSG,1310) TRIM(SCHEDULEFILE)
1310                FORMAT('No PHIABANDON values found in file ',a,'.')
                    GO TO 9890
                  END IF
                  IF(I.LT.NOPTMAX)THEN
                    DO J=I,NOPTMAX
                      PHIABANDON(J)=PHIABANDON(I-1)
                    END DO
                  END IF
                  IF((PESTMODE.EQ.2).OR.(PESTMODE.EQ.4))THEN
                    WRITE(ERRMSG,1320)
1320                FORMAT('PHIABANDON schedule file must not be ',
     +              'supplied if PEST is run in PREDICTIVE ANALYSIS ',
     +              'mode or PARETO mode.')
                    GO TO 9890
                  END IF
                END IF
              END IF
            END IF
          END IF
        END IF

260     ILINE=ILINE+1
        READ(CTLUNIT,110,ERR=9000,END=9050) CLINE
        IF(SKIPLINE(CLINE)) GO TO 260
        IJCO=0
        CALL LOWCAS(CLINE)
        IJCO=INDEX(CLINE,' jcosaveitn ')
        IF(IJCO.NE.0)THEN
          CLINE(IJCO+1:IJCO+10)=' '
          IJCO=1
        END IF
        IF(IJCO.NE.1)THEN
          IJCO=INDEX(CLINE,' nojcosaveitn ')
          IF(IJCO.NE.0)THEN
            CLINE(IJCO+1:IJCO+12)=' '
            IJCO=0
          END IF
        END IF
        IREI=INDEX(CLINE,' reisaveitn ')
        IF(IREI.NE.0)THEN
          CLINE(IREI+1:IREI+10)=' '
          IREI=1
        END IF
        IF(IREI.NE.1)THEN
          IREI=INDEX(CLINE,' noreisaveitn ')
          IF(IREI.NE.0)THEN
            CLINE(IREI+1:IREI+12)=' '
            IREI=0
          END IF
        END IF

        JFLAG=0
        ITEMP=INDEX(CLINE,' jcosave ')
        IF(ITEMP.NE.0)THEN
          CLINE(ITEMP+1:ITEMP+8)=' '
          JCOSAVE=1
          JFLAG=1
        END IF
        ITEMP=INDEX(CLINE,' nojcosave ')
        IF(ITEMP.NE.0)THEN
          IF(JFLAG.EQ.1) GO TO 9020
          CLINE(ITEMP+1:ITEMP+10)=' '
          JCOSAVE=0
        END IF

        JFLAG=0
        ITEMP=INDEX(CLINE,' verboserec ')
        IF(ITEMP.NE.0)THEN
          CLINE(ITEMP+1:ITEMP+11)=' '
          VERBOSEREC=1
          JFLAG=1
        END IF
        ITEMP=INDEX(CLINE,' noverboserec ')
        IF(ITEMP.NE.0)THEN
          IF(JFLAG.EQ.1) GO TO 9020
          CLINE(ITEMP+1:ITEMP+13)=' '
          VERBOSEREC=0
        END IF

        JFLAG=0
        ITEMP=INDEX(CLINE,' parsaveitn ')
        IF(ITEMP.NE.0)THEN
          CLINE(ITEMP+1:ITEMP+11)=' '
          PARSAVEITN=1
          JFLAG=1
        END IF
        ITEMP=INDEX(CLINE,' noparsaveitn ')
        IF(ITEMP.NE.0)THEN
          IF(JFLAG.EQ.1) GO TO 9020
          CLINE(ITEMP+1:ITEMP+13)=' '
          PARSAVEITN=0
        END IF

        JFLAG=0
        ITEMP=INDEX(CLINE,' parsaverun ')
        IF(ITEMP.NE.0)THEN
          CLINE(ITEMP+1:ITEMP+11)=' '
          PARSAVERUN=1
          JFLAG=1
        END IF
        ITEMP=INDEX(CLINE,' noparsaverun ')
        IF(ITEMP.NE.0)THEN
          IF(JFLAG.EQ.1) GO TO 9020
          CLINE(ITEMP+1:ITEMP+13)=' '
          PARSAVERUN=0
        END IF
        IF(PARSAVERUN.NE.0)THEN
          IF(BEOFLAG.EQ.0)THEN
            WRITE(ERRMSG,261)
261         FORMAT('PARSAVERUN can only be set when ',
     +      'using BEOPEST.')
            GO TO 9890
          END IF
        END IF

        JFLAG=0
        ITEMP=INDEX(CLINE,' rrfsave ')
        IF(ITEMP.NE.0)THEN
          CLINE(ITEMP+1:ITEMP+8)=' '
          WRITE_RRF=1
          JFLAG=1
        END IF
        ITEMP=INDEX(CLINE,' norrfsave ')
        IF(ITEMP.NE.0)THEN
          IF(JFLAG.EQ.1) GO TO 9020
          CLINE(ITEMP+1:ITEMP+10)=' '
          WRITE_RRF=0
        END IF

        CALL LINSPL(JFAIL,3,LW,RW,CLINE)
        IF(JFAIL.NE.0) GO TO 9100
        CALL NUMRD(JFAIL,0,LW(1),RW(1),ICOV,RTEMP,CLINE)
        IF(JFAIL.NE.0) GO TO 9150
        CALL NUMRD(JFAIL,0,LW(2),RW(2),ICOR,RTEMP,CLINE)
        IF(JFAIL.NE.0) GO TO 9150
        CALL NUMRD(JFAIL,0,LW(3),RW(3),IEIG,RTEMP,CLINE)
        IF(JFAIL.NE.0) GO TO 9150
        CALL LINSPL(JFAIL,4,LW,RW,CLINE)
        IF(JFAIL.EQ.0)THEN
          CALL NUMRD(JFAIL,0,LW(4),RW(4),IRES,RTEMP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
        END IF
        IF(PFILE.NE.0) NOPTMAX=-3
        IF((NOPTMAX.EQ.-2).OR.(NOPTMAX.EQ.-3))THEN
          ICOV=0
          ICOR=0
          IEIG=0
          IRES=0
        END IF

270     ILINE=ILINE+1
        READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
        IF(SKIPLINE(CLINE)) GO TO 270
        CALL LOWCAS(CLINE)
271     CONTINUE
        IF(CLINE(1:5).EQ.'* aut')THEN
          AUISECT=1
280       ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 280
          CALL LINSPL(JFAIL,4,LW,RW,CLINE)
          IF(JFAIL.NE.0) GO TO 9100
          CALL NUMRD(JFAIL,0,LW(1),RW(1),MAXAUI,RTEMP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          CALL NUMRD(JFAIL,0,LW(2),RW(2),AUISTARTOPT,RTEMP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          CALL NUMRD(JFAIL,1,LW(3),RW(3),ITEMP,NOAUIPHIRAT,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          CALL NUMRD(JFAIL,0,LW(4),RW(4),AUIRESTITN,RTEMP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
290       ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 290
          CALL LINSPL(JFAIL,3,LW,RW,CLINE)
          IF(JFAIL.NE.0) GO TO 9100
          CALL NUMRD(JFAIL,1,LW(1),RW(1),ITEMP,AUISENSRAT,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          CALL NUMRD(JFAIL,0,LW(2),RW(2),AUIHOLDMAXCHG,RTEMP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          CALL NUMRD(JFAIL,0,LW(3),RW(3),AUINUMFREE,RTEMP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
300       ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 300
          CALL LINSPL(JFAIL,3,LW,RW,CLINE)
          IF(JFAIL.NE.0) GO TO 9100
          CALL NUMRD(JFAIL,1,LW(1),RW(1),ITEMP,AUIPHIRATSUF,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          CALL NUMRD(JFAIL,1,LW(2),RW(2),ITEMP,AUIPHIRATACCEPT,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          CALL NUMRD(JFAIL,0,LW(3),RW(3),NAUINOACCEPT,RTEMP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
310       ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 310
          CALL LOWCAS(CLINE)
        ELSE IF(CLINE(1:10).EQ.'* singular')THEN
311       ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 311
          CALL LINSPL(JFAIL,1,LW,RW,CLINE)
          CALL NUMRD(JFAIL,0,LW(1),RW(1),SVDMODE,RTEMP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          IF((SVDMODE.NE.0).AND.(SVDMODE.NE.1).AND.(SVDMODE.NE.2))THEN
            CALL WRITINT(ALINE,ILINE)
            WRITE(ERRMSG,312) TRIM(ALINE),TRIM(AFILE)
312         FORMAT('SVDMODE must be "0", "1" or "2" at line ',A,
     +      ' of file ',A,'.')
            GO TO 9890
          END IF
          IF((SVDMODE.NE.0).AND.(DOAUI(1:3).EQ.'aui'))THEN
            WRITE(ERRMSG,321)
321         FORMAT('If singular value decomposition is activated ',
     +      'automatic user intervention must be de-activated.')
            GO TO 9890
          END IF
          IF((SVDMODE.NE.0).AND.(PESTMODE.EQ.2))THEN
            WRITE(ERRMSG,329)
329         FORMAT('Singular value decomposition must not be ',
     +      'activated if PEST is run in "predictive analysis" ',
     +      'mode.')
            GO TO 9890
          END IF
C          IF((SVDMODE.EQ.2).AND.(PESTMODE.EQ.3))THEN
C            WRITE(ERRMSG,327)
C327         FORMAT('If PEST is running in regularisation mode, ',
C     +      'SVDMODE must not be set to 2; set SVDMODE to 1 or ',
C     +      'use LSQR instead.')
C            GO TO 9890
C          END IF
          IF((LSQRMODE.EQ.1).AND.(SVDMODE.NE.0))THEN
            WRITE(ERRMSG,1131)
            GO TO 9890
          END IF
c          IF((SVDMODE.EQ.2).AND.(RLAMBDA1.NE.0.0D0))THEN
c            WRITE(ERRMSG,328)
c328         FORMAT('SVDMODE must not be set to 2 unless the ',
c     +      'initial Marquardt lambda is set to zero.')
c            go to 9890
c          END IF
313       ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 313
          CALL LINSPL(JFAIL,2,LW,RW,CLINE)
          IF(JFAIL.NE.0) GO TO 9100
          CALL NUMRD(JFAIL,0,LW(1),RW(1),MAXSING,RTEMP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          IF(MAXSING.LE.0) GO TO 9150
          CALL NUMRD(JFAIL,1,LW(2),RW(2),ITEMP,EIGTHRESH,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          IF(EIGTHRESH.GT.1.0)THEN
            CALL WRITINT(ALINE,ILINE)
            WRITE(ERRMSG,314) TRIM(ALINE),TRIM(AFILE)
314         FORMAT('EIGTHRESH must be (significantly) less than 1 ',
     +      'at line ',A,' of file ',A,'.')
            GO TO 9890
          END IF
309       ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 309
          CALL LINSPL(JFAIL,1,LW,RW,CLINE)
          IF(JFAIL.NE.0) GO TO 9100
          CALL NUMRD(JFAIL,0,LW(1),RW(1),EIGWRITE,RTEMP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          IF(EIGWRITE.LE.0)THEN
            EIGWRITE=0
          ELSE
            EIGWRITE=1
          END IF
319       ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 319
          CALL LOWCAS(CLINE)
        ELSE IF(CLINE(1:6).EQ.'* lsqr')THEN
1100      ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 1100
          CALL LINSPL(JFAIL,1,LW,RW,CLINE)
          CALL NUMRD(JFAIL,0,LW(1),RW(1),LSQRMODE,RTEMP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          IF((LSQRMODE.NE.0).AND.(LSQRMODE.NE.1))THEN
            CALL WRITINT(ALINE,ILINE)
            WRITE(ERRMSG,1110) TRIM(ALINE),TRIM(AFILE)
1110        FORMAT('LSQRMODE must be "0" or "1" at line ',A,
     +      ' of file ',A,'.')
            GO TO 9890
          END IF
          IF((LSQRMODE.EQ.1).AND.(DOAUI(1:3).EQ.'aui'))THEN
            WRITE(ERRMSG,1120)
1120        FORMAT('If LSQR is activated ',
     +      'automatic user intervention must be de-activated.')
            GO TO 9890
          END IF
          IF((LSQRMODE.EQ.1).AND.(PESTMODE.EQ.2))THEN
            WRITE(ERRMSG,1130)
1130        FORMAT('LSQR must not be ',
     +      'activated if PEST is run in "predictive analysis" ',
     +      'mode.')
            GO TO 9890
          END IF
          IF((LSQRMODE.EQ.1).AND.(SVDMODE.NE.0))THEN
            WRITE(ERRMSG,1131)
1131        FORMAT('LSQR must not be activated if ',
     +      'singular value decomposition is also activated.')
            GO TO 9890
          END IF
1340      ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 1340
          CALL LINSPL(JFAIL,4,LW,RW,CLINE)
          IF(JFAIL.NE.0) GO TO 9100
          CALL NUMRD(JFAIL,1,LW(1),RW(1),ITEMP,LSQR_ATOL,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          IF(LSQR_ATOL.LT.0.0D0)LSQR_ATOL=0.0D0
          CALL NUMRD(JFAIL,1,LW(2),RW(2),ITEMP,LSQR_BTOL,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          IF(LSQR_BTOL.LT.0.0D0)LSQR_BTOL=0.0D0
          CALL NUMRD(JFAIL,1,LW(3),RW(3),ITEMP,LSQR_CONLIM,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          IF(LSQR_CONLIM.LT.0.0D0)LSQR_CONLIM=0.0D0
          CALL NUMRD(JFAIL,0,LW(4),RW(4),LSQR_ITNLIM,RTEMP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          IF(LSQR_ITNLIM.LE.0)THEN
            CALL WRITINT(ALINE,ILINE)
            WRITE(ERRMSG,1350) TRIM(ALINE),TRIM(AFILE)
1350        FORMAT('LSQR_ITNLIM must be positive at line ',a,' of ',
     +      'file ',A,'.')
            GO TO 9890
          END IF
1360      ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 1360
          CALL LINSPL(JFAIL,1,LW,RW,CLINE)
          IF(JFAIL.NE.0) GO TO 9100
          CALL NUMRD(JFAIL,0,LW(1),RW(1),LSQRWRITE,RTEMP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          IF(LSQRWRITE.LE.0)THEN
            LSQRWRITE=0
          ELSE
            LSQRWRITE=1
          END IF
1370      ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 1370
          CALL LOWCAS(CLINE)
        ELSE IF(CLINE(1:6).EQ.'* sens')THEN
1380      ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 1380
          CALL LINSPL(JFAIL,2,LW,RW,CLINE)
          IF(JFAIL.NE.0) GO TO 9100
          CALL NUMRD(JFAIL,1,LW(1),RW(1),ITEMP,SENRELTHRESH,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          IF(SENRELTHRESH.LT.0.0D0) GO TO 9150
          CALL NUMRD(JFAIL,0,LW(2),RW(2),SENMAXREUSE,RTEMP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          IF(SENMAXREUSE.EQ.0) GO TO 9150
1390      ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 1390
          CALL LINSPL(JFAIL,3,LW,RW,CLINE)
          IF(JFAIL.NE.0) GO TO 9100
          CALL NUMRD(JFAIL,0,LW(1),RW(1),SENALLCALCINT,RTEMP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          IF(SENALLCALCINT.LE.1) GO TO 9150
          CALL NUMRD(JFAIL,1,LW(2),RW(2),ITEMP,SENPREDWEIGHT,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          SENPIEXCLUDE=CLINE(LW(3):RW(3))
          CALL LOWCAS(SENPIEXCLUDE)
          IF((SENPIEXCLUDE.NE.'yes').AND.(SENPIEXCLUDE.NE.'no'))
     +    GO TO 9150
1400      ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 1400
          CALL LOWCAS(CLINE)
        ELSE IF((CLINE(1:8).EQ.'* simult').OR.
     +          (CLINE(1:5).EQ.'* rsi').OR.
     +          (CLINE(1:9).eq.'* randomi'))THEN
          DO
            ILINE=ILINE+1
1401        READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
            IF(SKIPLINE(CLINE)) GO TO 1401
            IF(CLINE(1:1).EQ.'*') EXIT
          END DO
        END IF

        IF(CLINE(1:5).EQ.'* aut') GO TO 271
        IF(CLINE(1:6).EQ.'* lsqr') GO TO 271
        IF(CLINE(1:10).EQ.'* singular') GO TO 271
        IF(CLINE(1:6).EQ.'* sens') GO TO 271
        IF(CLINE(1:8).EQ.'* simult') then
          has_simul_section=1
          GO TO 271
        end if
        if(cline(1:9).eq.'* randomi')then
          has_random_section=1
          go to 271
        end if
        if(cline(1:5).eq.'* rsi')then
          has_rsi_section=1
          go to 271
        end if

        IF(CLINE.EQ.'* svd assist')THEN
C          IF(NUMCOM.GT.1)THEN
C            WRITE(ERRMSG,308)
C308         FORMAT('SVD assist functionality cannot be activated if ',
C     +      'NUMCOM (number of model command lines) is greater than ',
C     +      'one.')
C            GO TO 9890
C          END IF
C          IF(JACFILE.NE.0)THEN
C            WRITE(ERRMSG,307)
C307         FORMAT('SVD assist functionality cannot be activated if ',
C     +      'derivatives are supplied externally by model.')
C            GO TO 9890
C          END IF
          SVDA=1
317       ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 317
          CALL SPACESUB(CLINE)
          CALL LINSPL(JFAIL,1,LW,RW,CLINE)
          BASEPESTFILE=CLINE(LW(1):RW(1))
          CALL REMCHAR(BASEPESTFILE,CHAR(211))
316       ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 316
          CALL SPACESUB(CLINE)
          CALL LINSPL(JFAIL,1,LW,RW,CLINE)
          BASEJACFILE=CLINE(LW(1):RW(1))
          CALL REMCHAR(BASEJACFILE,CHAR(211))
315       ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 315
          CALL LOWCAS(CLINE)
          IF((CLINE(1:15).NE.'* parameter gro').AND.
     +       (CLINE(1:15).NE.'* group definit'))THEN
             CALL LINSPL(JFAIL,1,LW,RW,CLINE)
             CALL NUMRD(JFAIL,0,LW(1),RW(1),SVDA_MULBPA,RTEMP,CLINE)
             IF(JFAIL.NE.0)THEN
               CALL WRITINT(ALINE,ILINE)
               WRITE(ERRMSG,323) TRIM(ALINE),TRIM(AFILE)
323            FORMAT('"Parameter groups" header or SVDA control ',
     +         'variables expected at line ',A,' of file ',A,'.')
               GO TO 9890
             END IF
             IF((SVDA_MULBPA.NE.0).AND.(SVDA_MULBPA.NE.1))THEN
               CALL WRITINT(ALINE,ILINE)
               WRITE(ERRMSG,322) TRIM(ALINE),TRIM(AFILE)
322            FORMAT('Value of SVDA_MULBPA variable must be ',
     +         '"0" or "1" at line ',A,' of file ',a,'.')
               GO TO 9890
             END IF
             CALL LINSPL(JFAIL,2,LW,RW,CLINE)
             IF(JFAIL.EQ.0)THEN
               CALL NUMRD(JFAIL,0,LW(2),RW(2),SVDA_SCALADJ,RTEMP,
     +         CLINE)
               IF(JFAIL.NE.0)THEN
                 CALL WRITINT(ALINE,ILINE)
                 WRITE(ERRMSG,331) TRIM(ALINE),TRIM(AFILE)
331              FORMAT('Cannot read value for SVDA_SCALADJ ',
     +           'variable from line ',A,' of file ',A,'.')
                 GO TO 9890
               END IF
               IF(ABS(SVDA_SCALADJ).GT.4)THEN
                 CALL WRITINT(ALINE,ILINE)
                 WRITE(ERRMSG,332) TRIM(ALINE),TRIM(AFILE)
332              FORMAT('Absolute Value of SVDA_SCALADJ variable ',
     +           'must be less four or less at line ',
     +           A,' of file ',a,'.')
                 GO TO 9890
               END IF
               CALL LINSPL(JFAIL,3,LW,RW,CLINE)
               IF(JFAIL.EQ.0)THEN
                 CALL NUMRD(JFAIL,0,LW(3),RW(3),SVDA_EXTSUPER,RTEMP,
     +           CLINE)
                 IF(JFAIL.NE.0)THEN
                   CALL WRITINT(ALINE,ILINE)
                   WRITE(ERRMSG,333) TRIM(ALINE),TRIM(AFILE)
333                FORMAT('Cannot read value for SVDA_EXTSUPER ',
     +             'variable from line ',A,' of file ',A,'.')
                   GO TO 9890
                 END IF
                 IF((SVDA_EXTSUPER.NE.0).AND.(SVDA_EXTSUPER.NE.1)
     +               .AND.(ABS(SVDA_EXTSUPER).NE.2)
     +               .AND.(SVDA_EXTSUPER.NE.3))THEN
                   CALL WRITINT(ALINE,ILINE)
                   WRITE(ERRMSG,334) TRIM(ALINE),TRIM(AFILE)
334                FORMAT('SVDA_EXTSUPER variable must be supplied ',
     +             'as 0, 1, 2, -2 or 3 at line ',A,' of file ',A,'.')
                   GO TO 9890
                 END IF
                 IF(SVDA_EXTSUPER.EQ.1)SVDA_SCALADJ=0

                 CALL LINSPL(JFAIL,4,LW,RW,CLINE)
                 IF(JFAIL.EQ.0)THEN
                   CALL NUMRD(JFAIL,0,LW(4),RW(4),SVDA_SUPDERCALC,
     +             RTEMP,CLINE)
                   IF(JFAIL.NE.0)THEN
                     CALL WRITINT(ALINE,ILINE)
                     WRITE(ERRMSG,335) TRIM(ALINE),TRIM(AFILE)
335                  FORMAT('Cannot read value for SVDA_SUPDERCALC ',
     +               'variable from line ',A,' of file ',A,'.')
                     GO TO 9890
                   END IF
                   IF((SVDA_SUPDERCALC.NE.0).AND.
     +                (SVDA_SUPDERCALC.NE.1))THEN
                      CALL WRITINT(ALINE,ILINE)
                      WRITE(ERRMSG,336) TRIM(ALINE),TRIM(AFILE)
336                   FORMAT('SVDA_SUPDERCALC variable must be ',
     +                'supplied as 0 or 1 at line ',A,' of file ',A,
     +                '.')
                      GO TO 9890
                   END IF
                   IF(SVDA_SUPDERCALC.EQ.1)THEN
                     IF(SVDA_EXTSUPER.EQ.1)THEN
                       CALL WRITINT(ALINE,ILINE)
                       WRITE(ERRMSG,337) TRIM(ALINE),TRIM(AFILE)
337                    FORMAT('If SVDA_EXTSUPER is supplied as 1',
     +                 ' on line ',A,' of file ',A,' then ',
     +                 'SVDA_SUPDERCALC must be supplied as 0.')
                       GO TO 9890
                     END IF
                   END IF
                   CALL LINSPL(JFAIL,5,LW,RW,CLINE)
                   IF(JFAIL.EQ.0)THEN
                     CALL NUMRD(JFAIL,0,LW(5),RW(5),SVDA_PAR_EXCL,
     +               RTEMP,CLINE)
                     IF(JFAIL.NE.0)THEN
                       CALL WRITINT(ALINE,ILINE)
                       WRITE(ERRMSG,3351) TRIM(ALINE),TRIM(AFILE)
3351                   FORMAT('Cannot read value for SVDA_PAR_EXCL ',
     +                 'variable from line ',A,' of file ',A,'.')
                       GO TO 9890
                     END IF
                     IF((SVDA_PAR_EXCL.NE.0).AND.
     +                  (SVDA_PAR_EXCL.NE.1).AND.
     +                  (SVDA_PAR_EXCL.NE.-1))THEN
                        CALL WRITINT(ALINE,ILINE)
                        WRITE(ERRMSG,3361) TRIM(ALINE),TRIM(AFILE)
3361                    FORMAT('SVDA_PAR_EXCL variable must be ',
     +                  'supplied as 0, 1 or -1 at line ',A,
     +                  ' of file ',A,'.')
                      GO TO 9890
                     END IF
                     IF(ABS(SVDA_PAR_EXCL).EQ.1)THEN
                       IF(PESTMODE.NE.4)THEN
                         CALL WRITINT(ALINE,ILINE)
                         WRITE(ERRMSG,3362) TRIM(ALINE),TRIM(AFILE)
3362                     FORMAT('SVDA_PAR_EXCL must not be set to 1 ',
     +                   'or -1 at line ',A,' of file ',A,
     +                   ' unless PEST is run in "pareto" mode.')
                         GO TO 9890
                       END IF
                       IF(SVDA_EXTSUPER.EQ.1)THEN
                         CALL WRITINT(ALINE,ILINE)
                         WRITE(ERRMSG,3363)TRIM(ALINE),TRIM(AFILE)
3363                     FORMAT('SVDA_PAR_EXCL must not be set to 1 ',
     +                   'or -1 at line ',A,' of file ',A,
     +                   ' if SVDA_EXTSUPER is set to 1.')
                         GO TO 9890
                       END IF
                     END IF
                   END IF
                 END IF
               END IF
             END IF
318          ILINE=ILINE+1
             READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
             IF(SKIPLINE(CLINE)) GO TO 318
          END IF
          IF(JACFILE.NE.0)THEN
            IF(SVDA_SUPDERCALC.EQ.0)SVDA_SUPDERCALC=1
          END IF
        END IF
        IF((CLINE(1:15).NE.'* parameter gro').AND.
     +     (CLINE(1:15).NE.'* group definit'))THEN
          CALL WRITINT(ALINE,ILINE)
          WRITE(ERRMSG,320) TRIM(ALINE),TRIM(AFILE)
320       FORMAT('"Parameter groups" header expected at line ',A,
     +    ' of file ',A,'.')
          GO TO 9890
        END IF

C -- Some storage is allocated.

        ALLOCATE(PARGNME(NPARGP),INCTYP(NPARGP),
     +           DERINC(NPARGP),DERINCLB(NPARGP),DERINCMUL(NPARGP),
     +           FORCEN(NPARGP),DERMTHD(NPARGP),STAT=IERR)
        IF(IERR.NE.0) GO TO 9200

        ALLOCATE(SPLITTHRESH(NPARGP),SPLITRELDIFF(NPARGP),
     +           SPLITACTION(NPARGP),STAT=IERR)
        IF(IERR.NE.0) GO TO 9200
        SPLITTHRESH=0.0D0
        SPLITRELDIFF=0.0D0
        SPLITACTION=0

        ALLOCATE(APAR(NPAR),ITRANS(NPAR),PARCHGLIM(NPAR),PVAL(NPAR),
     +           PARLBND(NPAR),PARUBND(NPAR),IPARGP(NPAR),SCALE(NPAR),
     +           OFFSET(NPAR),DERCOM(NPAR),PVAL_O(NPAR),STAT=IERR)
        IF(IERR.NE.0) GO TO 9200

 ! -- The parameter groups section is read.

        DO I=1,NPARGP
350       ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 350
          CALL LOWCAS(CLINE)
          CALL LINSPL(JFAIL,7,LW,RW,CLINE)
          IF(JFAIL.NE.0) GO TO 9100
          PARGNME(I)=CLINE(LW(1):RW(1))
          IF(PARGNME(I).EQ.'none')THEN
            WRITE(ERRMSG,360)
360         FORMAT('Parameter group "none" is reserved for some ',
     +      'fixed and tied parameters. No information must ',
     +      'be provided for this group in the "parameter ',
     +      'groups" section of the PEST control file.')
            GO TO 9890
          END IF
          ATEMP=CLINE(LW(2):RW(2))
          IF(ATEMP.EQ.'relative') THEN
            INCTYP(I)=1
          ELSE IF(ATEMP.EQ.'absolute')THEN
            INCTYP(I)=2
          ELSE IF(ATEMP.EQ.'rel_to_max')THEN
            INCTYP(I)=3
          ELSE
            GO TO 9150
          END IF
          CALL NUMRD(JFAIL,1,LW(3),RW(3),ITEMP,DERINC(I),CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          CALL NUMRD(JFAIL,1,LW(4),RW(4),ITEMP,DERINCLB(I),CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          ATEMP=CLINE(LW(5):RW(5))
          IF(ATEMP.EQ.'switch')THEN
            FORCEN(I)=1
          ELSE IF(ATEMP.EQ.'always_2')THEN
            FORCEN(I)=2
          ELSE IF(ATEMP.EQ.'always_3')THEN
            FORCEN(I)=3
          ELSE IF(ATEMP.EQ.'switch_5')THEN
            FORCEN(I)=4
          ELSE IF(ATEMP.EQ.'always_5')THEN
            FORCEN(I)=5
          ELSE
            GO TO 9150
          END IF
          CALL NUMRD(JFAIL,1,LW(6),RW(6),ITEMP,DERINCMUL(I),CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          ATEMP=CLINE(LW(7):RW(7))
          IF(ATEMP.EQ.'parabolic') THEN
            DERMTHD(I)=1
          ELSE IF(ATEMP.EQ.'best_fit')THEN
            DERMTHD(I)=2
          ELSE IF(ATEMP.EQ.'outside_pts')THEN
            DERMTHD(I)=3
          ELSE IF(ATEMP.EQ.'minvar')THEN
            DERMTHD(I)=4
          ELSE IF(ATEMP.EQ.'maxprec')THEN
            DERMTHD(I)=5
          ELSE
            GO TO 9150
          END IF
          IF(INCTYP(I).EQ.2) THEN
            DERINCLB(I)=0.0D0
          END IF
          IF((FORCEN(I).EQ.1).OR.(FORCEN(I).EQ.2).OR.(FORCEN(I).EQ.3))
     +    THEN
            IF((DERMTHD(I).EQ.4).OR.(DERMTHD(I).EQ.5))THEN
              CALL WRITINT(ALINE,ILINE)
              WRITE(ERRMSG,364) TRIM(ALINE)
364           FORMAT('If FORCEN is supplied as "always_2", "always_3"',
     +        ' or "switch", then DERMTHD must be "best_fit"',
     +        ' "outside_pts" or "parabolic" in "parameter groups" ',
     +        'section of PEST control file at line ',A,'.')
              GO TO 9890
            END IF
          ELSE
            IF((DERMTHD(I).EQ.1).OR.(DERMTHD(I).EQ.2).OR.
     +         (DERMTHD(I).EQ.3))THEN
               CALL WRITINT(ALINE,ILINE)
               WRITE(ERRMSG,366) TRIM(ALINE)
366            FORMAT('If FORCEN is suplied as "switch_5" or ',
     +         '"always_5" than DERMTHD must be "minvar" or ',
     +         '"maxprec" in "parameter groups" section of PEST ',
     +         'control file at line ',A,'.')
               GO TO 9890
            END IF
          END IF

          CALL LINSPL(JFAIL,8,LW,RW,CLINE)
          IF(JFAIL.EQ.0) THEN
            CALL LINSPL(JFAIL,9,LW,RW,CLINE)
            IF(JFAIL.NE.0)THEN
              WRITE(ERRMSG,362)
362           FORMAT('Each line in the "parameter groups" section of a ',
     +        'PEST control file must have either 7 or 10 entries.')
              GO TO 9890
            END IF
            CALL LINSPL(JFAIL,10,LW,RW,CLINE)
            IF(JFAIL.NE.0)THEN
              WRITE(ERRMSG,362)
              GO TO 9890
            END IF
            CALL NUMRD(JFAIL,1,LW(8),RW(8),ITEMP,SPLITTHRESH(I),CLINE)
            IF(JFAIL.NE.0) GO TO 9150
            IF(SPLITTHRESH(I).LT.0.0D0) GO TO 9150
            IF(SPLITTHRESH(I).GT.0.0D0)THEN
              CALL NUMRD(JFAIL,1,LW(9),RW(9),ITEMP,SPLITRELDIFF(I),
     +        CLINE)
              IF(JFAIL.NE.0) GO TO 9150
              IF(SPLITRELDIFF(I).LE.0.0D0) GO TO 9150
              ATEMP=CLINE(LW(10):RW(10))
              IF(ATEMP(1:5).EQ.'small')THEN
                SPLITACTION(I)=1
              ELSE IF(ATEMP.EQ.'zero')THEN
                SPLITACTION(I)=2
              ELSE IF(ATEMP.EQ.'previous')THEN
                SPLITACTION(I)=3
              ELSE
                GO TO 9150
              END IF
            ELSE
              SPLITRELDIFF(I)=0.0D0
              SPLITACTION(I)=0
            END IF
          ELSE
            SPLITTHRESH(I)=0.0D0
            SPLITRELDIFF(I)=0.0D0
            SPLITACTION(I)=0
          END IF
          IF((SPLITTHRESH(I).GT.0.0D0).AND.(FORCEN(I).GE.4))THEN
            CALL WRITINT(ALINE,ILINE)
            WRITE(ERRMSG,363) TRIM(ALINE)
363         FORMAT('If SPLITTHRESH is set greater than zero in the ',
     +      '"parameter groups" section of a PEST control file, ',
     +      'FORCEN for same group must not be set to "switch_5" or ',
     +      '"always_5". Error at line ',A,'.')
            GO TO 9890
          END IF
        END DO

C -- The "parameter data" section of the PEST control file is read.

        CALL FIND_HEADER(JFAIL,CTLUNIT,ILINE,'* parameter data',
     +  CLINE,AFILE)
        IF(JFAIL.NE.0) GO TO 9890
        DO I=1,NPAR
390       ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 390
          CLINE=ADJUSTL(CLINE)
          CALL WRITINT(ALINE,ILINE)
          NEQQ=INDEX(CLINE,'=')
          IF(NEQQ.EQ.1)THEN
            WRITE(ERRMSG,393) TRIM(ALINE),TRIM(AFILE)
393         FORMAT('Improper equation syntax at line ',A,' of file '
     +      A,'.')
            GO TO 9890
          END IF
          CALL LOWCAS(CLINE)
          DERCOM(I)=1
          IJAC=0
          IF((NEQQ.EQ.0).OR.(NEQUATION.EQ.0))THEN
            CALL LINSPL(JFAIL,2,LW,RW,CLINE)
            IF(JFAIL.NE.0) GO TO 9100
            ATEMP=CLINE(LW(2):RW(2))
            IF(ATEMP.NE.'file_parameter')THEN
              CALL LINSPL(JFAIL,10,LW,RW,CLINE)
              IF(JFAIL.EQ.0) THEN
                IJAC=1
              ELSE
                CALL LINSPL(JFAIL,9,LW,RW,CLINE)
                IF(JFAIL.NE.0) GO TO 9100
              END IF
            ELSE
              GO TO 390
            END IF
          ELSE
            GO TO 390
          END IF
          IF((JACFILE.NE.0).AND.(IJAC.EQ.0))THEN
            WRITE(ERRMSG,400) TRIM(ALINE),TRIM(AFILE)
400         FORMAT('Value expected for DERCOM on line ',A,
     +      ' of file ',A,'.')
            GO TO 9890
          END IF
          APAR(I)=CLINE(LW(1):RW(1))
          ATEMP=CLINE(LW(2):RW(2))
          IF(ATEMP.EQ.'log')THEN
            ITRANS(I)=1
          ELSE IF(ATEMP.EQ.'none')THEN
            ITRANS(I)=0
          ELSE IF(ATEMP.EQ.'tied')THEN
            ITRANS(I)=-1
          ELSE IF(ATEMP.EQ.'fixed') THEN
            ITRANS(I)=-1000000
          ELSE
            GO TO 9150
          END IF
          ATEMP=CLINE(LW(3):RW(3))
          IF(ATEMP.EQ.'relative') THEN
            PARCHGLIM(I)=0
          ELSE IF(ATEMP.EQ.'factor') THEN
            PARCHGLIM(I)=1
          ELSE IF(ATEMP(1:8).EQ.'absolute')THEN
            IF(ATEMP(9:9).NE.'(') GO TO 9150
            ATEMP=ATEMP(10:)
            NTT=LEN_TRIM(ATEMP)
            IF(ATEMP(NTT:NTT).NE.')') GO TO 9150
            ATEMP(NTT:NTT)=' '
            CALL INTRD1(IFAIL,ATEMP,ABSIND)
            IF(IFAIL.NE.0)THEN
              WRITE(ERRMSG,403) TRIM(ALINE),TRIM(AFILE)
403           FORMAT('Cannot read absolute change index at ',
     +        'line ',A,' of file ',A,'.')
              GO TO 9890
            END IF
            IF((ABSIND.LT.1).OR.(ABSIND.GT.MAXABSPARMAX))THEN
              WRITE(ERRMSG,401) TRIM(ALINE),TRIM(AFILE)
401           FORMAT('Absolute change index out of range at line ',A,
     +        ' of file ',A,'.')
              GO TO 9890
            END IF
            IF(ABSPARMAX(ABSIND).LT.-1.0E35)THEN
              CALL WRITINT(ATEMP,ABSIND)
              WRITE(ERRMSG,402) TRIM(ATEMP),TRIM(ALINE),TRIM(AFILE)
402           FORMAT('Absolute change index (',A,') not assigned ',
     +        'a value, yet referenced at line ',A,' of file ',A,'.')
              GO TO 9890
            END IF
            IF(ITRANS(I).EQ.1)THEN
              WRITE(ERRMSG,404) TRIM(ALINE),TRIM(AFILE)
404           FORMAT('Absolute change limit must not be assigned ',
     +        'to log-transformed parameter at line ',A,' of file ',
     +        A,'.')
              GO TO 9890
            END IF
            PARCHGLIM(I)=-ABSIND
          ELSE
            GO TO 9150
          END IF
          CALL NUMRD(JFAIL,1,LW(4),RW(4),ITEMP,PVAL(I),CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          CALL NUMRD(JFAIL,1,LW(5),RW(5),ITEMP,PARLBND(I),CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          CALL NUMRD(JFAIL,1,LW(6),RW(6),ITEMP,PARUBND(I),CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          ATEMP=CLINE(LW(7):RW(7))
          IF(ITRANS(I).EQ.1)THEN
            IF(PVAL(I).LE.0.0D0) GO TO 9150
            IF(PARLBND(I).LE.0.0D0) GO TO 9150
          END IF
          IF(ITRANS(I).GE.0)THEN
            IF(PARLBND(I).GE.PARUBND(I)) GO TO 9150
          END IF
          IF(ATEMP.EQ.'none')THEN
            IF(ITRANS(I).GE.0)THEN
              WRITE(ERRMSG,410) TRIM(ALINE),TRIM(AFILE)
410           FORMAT('Only fixed and tied parameters can be ',
     +        'assigned to the parameter group "none"; error at ',
     +        'line ',A,' of file ',A,'.')
              GO TO 9890
            END IF
            IPARGP(I)=0
          ELSE
            DO J=1,NPARGP
              IF(ATEMP.EQ.PARGNME(J))THEN
                IPARGP(I)=J
                GO TO 420
              END IF
            END DO
            WRITE(ERRMSG,415) TRIM(ATEMP),TRIM(ALINE),TRIM(AFILE)
415         FORMAT('Unknown parameter group "',A,'" at line ',A,
     +      ' of file ',A,'.')
            GO TO 9890
420         CONTINUE
          END IF
          CALL NUMRD(JFAIL,1,LW(8),RW(8),ITEMP,SCALE(I),CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          IF(SCALE(I).EQ.0.0D0)THEN
            WRITE(ERRMSG,440) TRIM(ALINE),TRIM(AFILE)
440         FORMAT('Parameter assigned a scale of zero at line ',A,
     +      ' of file ',A,'.')
            GO TO 9890
          END IF
          CALL NUMRD(JFAIL,1,LW(9),RW(9),ITEMP,OFFSET(I),CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          IF(IJAC.NE.0)THEN
            CALL NUMRD(JFAIL,0,LW(10),RW(10),DERCOM(I),RTEMP,CLINE)
            IF(JFAIL.NE.0) GO TO 9150
            IF(JACFILE.NE.0)THEN
              IF((DERCOM(I).LT.0).OR.(DERCOM(I).GT.NUMCOM))THEN
                WRITE(ERRMSG,445) TRIM(ALINE),TRIM(AFILE)
445             FORMAT('Value of DERCOM out of range at ',
     +          'line ',A,' of PEST control file ',A,'.')
                GO TO 9890
              END IF
              IF(SVDA.EQ.1)THEN
                IF(DERCOM(I).NE.0)THEN
                  WRITE(ERRMSG,446)
446               FORMAT('If external derivatives functionality is ',
     +            'activated for SVD-assisted parameter estimation ',
     +            'then all super parameters must have a DERCOM ',
     +            'setting of zero.')
                  GO TO 9890
                END IF
              END IF
            ELSE IF(JACFILE.EQ.0)THEN
              IF((DERCOM(I).LT.1).OR.(DERCOM(I).GT.NUMCOM))THEN
                WRITE(ERRMSG,445) TRIM(ALINE),TRIM(AFILE)
                GO TO 9890
              END IF
            END IF
C            IF(IREFOBS.EQ.1)THEN
C              IF(DERCOM(I).EQ.1)THEN
C                WRITE(ERRMSG,447) TRIM(ALINE),TRIM(AFILE)
C447             FORMAT('If observation re-referencing is turned on ',
C     +          'then no parameter can have a DERCOM value of 1. ',
C     +          'Error occurs at line ',A,' of file ',A,'.')
C                GO TO 9890
C              END IF
C            END IF
          END IF
        END DO

C        IF(IREFOBS.EQ.1)THEN
C          IF(NUMCOM.GT.1)THEN
C            DO I=1,NPAR
C              IF(ITRANS(I).GE.0)THEN
C                IF(DERCOM(I).EQ.1)THEN
C                  WRITE(ERRMSG,448)
C448               FORMAT('If observation re-referencing is activated ',
C     +            'and NUMCOM is greater than zero, no adjustable ',
C     +            'parameters can be supplied with a DERCOM value ',
C     +            'of 1.')
C                  GO TO 9890
C                END IF
C              END IF
C            END DO
C          END IF
C        END IF

C -- Tied parameter relationships are now read if necessary.

        ICOUNT=0
        DO I=1,NPAR
          IF((ITRANS(I).GE.0).OR.(ITRANS(I).EQ.-1000000)) CYCLE
          ICOUNT=ICOUNT+1
        END DO
        IF(ICOUNT.NE.0)THEN
          J=1
          II=1
          DO I=1,ICOUNT
450         ILINE=ILINE+1
            READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
            IF(SKIPLINE(CLINE)) GO TO 450
            CLINE=ADJUSTL(CLINE)
            CALL WRITINT(ALINE,ILINE)
            CALL LOWCAS(CLINE)
            NEQQ=INDEX(CLINE,'=')
            IF((NEQQ.NE.0).AND.(NEQUATION.NE.0)) GO TO 450
            CALL LINSPL(JFAIL,2,LW,RW,CLINE)
            IF(JFAIL.NE.0) GO TO 9100
            IF(CLINE(LW(2):RW(2)).EQ.'file_parameter')GO TO 450
            ATEMP=CLINE(LW(1):RW(1))
            ATEMP1=CLINE(LW(2):RW(2))
            CALL WHICH1(JFAIL,NPAR,J,APAR,ATEMP)
            IF(JFAIL.NE.0) GO TO 9300
            IF((ITRANS(J).GE.0).OR.(ITRANS(J).EQ.-1000000)) THEN
              WRITE(ERRMSG,460) TRIM(ATEMP),TRIM(ALINE),TRIM(AFILE)
460           FORMAT('Parameter "',A,'" is not a tied parameter at ',
     +        'line ',A,' of file ',A,'.')
              GO TO 9890
            END IF
            CALL WHICH1(JFAIL,NPAR,II,APAR,ATEMP1)
            IF(JFAIL.NE.0)THEN
              ATEMP=ATEMP1
              GO TO 9300
            END IF
            IF(II.EQ.J)THEN
              WRITE(ERRMSG,470) TRIM(ALINE),TRIM(AFILE)
470           FORMAT('Parameter cannot be tied to itself at line ',A,
     +        ' of file ',A,'.')
              GO TO 9890
            END IF
            IF(ITRANS(II).LT.0)THEN
              WRITE(ERRMSG,480) TRIM(ALINE),TRIM(AFILE)
480           FORMAT('Parameter is tied to fixed or tied parameter at ',
     +        'line ',A,' of file ',A,'.')
              GO TO 9890
            END IF
            ITRANS(J)=-II
          END DO
        END IF

C -- We are now in a position to allocate further storage.

        NESPAR=0
        DO I=1,NPAR
          IF(ITRANS(I).GE.0) NESPAR=NESPAR+1
        END DO
        IF(NESPAR.EQ.0)THEN
          WRITE(ERRMSG,325) TRIM(AFILE)
325       FORMAT('There are no adjustable parameters in file ',A,'.')
          GO TO 9890
        END IF
        NXROW=NOBS+NPRIOR
        IF(MAXCOMPDIM.LE.1)THEN
          XDIM1=NXROW
          XDIM2=NESPAR
          MAXCOMPDIM=1
        ELSE
          XDIM1=1
          XDIM2=1
        END IF
C        IF((NOPTMAX.EQ.0).AND.(NPRIOR.EQ.0))THEN
C          XDIM1=1
C          XDIM2=1
C        END IF

#ifdef BEO
        IF(BEOSLAVE)THEN
          XDIM1=1
          XDIM2=1
          MAXCOMPDIM=1
        ENDIF
#endif

        ALLOCATE(X(XDIM1,XDIM2),XC(MAXCOMPDIM),
     +  IXC(MAXCOMPDIM),STAT=IERR)
        IF(IERR.NE.0) GO TO 9200
        IXC_DIM=MAXCOMPDIM

        ALLOCATE(TEMPFLE(NTPLFLE),INFLE(NTPLFLE),INSFLE(NINSFLE),
     +  OUTFLE(NINSFLE),STAT=IERR)
        IF(IERR.NE.0) GO TO 9200
        ALLOCATE(OBGNME(NOBSGP),COVIND(NOBSGP),GTARG(NOBSGP),
     +  STAT=IERR)
        IF(IERR.NE.0) GO TO 9200
        GTARG=-1.1D35

        ALLOCATE(AOBS(NXROW),OVAL(NXROW),OWGHT(NXROW),NOBGNM(NXROW),
     +  STAT=IERR)
        IF(IERR.NE.0) GO TO 9200

        ALLOCATE(COMLIN(NUMCOM),STAT=IERR)
        IF(IERR.NE.0) GO TO 9200

        ALLOCATE(COVFLE(NOBSGP),STAT=IERR)
        IF(IERR.NE.0) GO TO 9200

! -- Even though the following arrays are not used, it is the responsibility of the
!    program calling PEST to allocate/deallocate them.

        ALLOCATE(MODVAL_O(NXROW),STAT=IERR)
        IF(IERR.NE.0) GO TO 9200

C -- Array initialisation

        DO I=1,MAXCOMPDIM
          XC(I)=0.0D0
        END DO
        DO I8=1,MAXCOMPDIM
          IXC(I8)=0
        END DO
        DO I=1,XDIM1
          DO J=1,XDIM2
            X(I,J)=0.0D0
          END DO
        END DO

        IF(JACFILE.NE.0)THEN
          ALLOCATE(IPAR2IES(NPAR),STAT=IERR)
          IF(IERR.NE.0) GO TO 9200
          IES=0
          DO IPAR=1,NPAR
            IF(ITRANS(IPAR).GE.0)THEN
              IES=IES+1
              IPAR2IES(IPAR)=IES
            END IF
          END DO
        END IF

C -- The observation groups section is now read.

        CALL FIND_HEADER(JFAIL,CTLUNIT,ILINE,'* observation groups',
     +  CLINE,AFILE)
        IF(JFAIL.NE.0) GO TO 9890
        IGTARG=0
        DO I=1,NOBSGP
510       ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 510
          CALL SPACESUB(CLINE)
          CALL LINSPL(JFAIL,2,LW,RW,CLINE)
          OBGNME(I)=CLINE(LW(1):RW(1))
          CALL LOWCAS(OBGNME(I))
          IF(JFAIL.NE.0)THEN
            COVFLE(I)=' '
            COVIND(I)=.FALSE.
          ELSE
            COVFLE(I)=CLINE(LW(2):RW(2))
            CALL NUMRD(JFAIL,1,LW(2),RW(2),ITEMP,GTARG(I),CLINE)
            IF(JFAIL.NE.0)THEN
              CALL REMCHAR(COVFLE(I),CHAR(211))
              COVIND(I)=.TRUE.
              GTARG(I)=-1.1D35
            ELSE
              CALL LINSPL(JFAIL,3,LW,RW,CLINE)
              IF(JFAIL.NE.0)THEN
                COVFLE(I)=' '
                COVIND(I)=.FALSE.
              ELSE
                COVFLE(I)=CLINE(LW(3):RW(3))
                CALL REMCHAR(COVFLE(I),CHAR(211))
                COVIND(I)=.TRUE.
              END IF
            END IF
          END IF
          IF(GTARG(I).GT.-1.0D35)THEN
            IF(PESTMODE.NE.3)THEN
              WRITE(ERRMSG,910) TRIM(OBGNME(I))
910           FORMAT('A group-specific target measurement ',
     +        'objective function cannot be supplied to observation ',
     +        'group "',A,'" unless PEST is run in "regularisation" ',
     +        'mode.')
              GO TO 9890
            END IF
            IF(OBGNME(I)(1:5).EQ.'regul')THEN
              WRITE(ERRMSG,911)
911           FORMAT('A group-specific target measurement ',
     +        'objective function must not be supplied to a ',
     +        'regularisation group.')
              GO TO 9890
            END IF
            IF(GTARG(I).LE.0.0D0)THEN
              WRITE(ERRMSG,914) TRIM(OBGNME(I))
914           FORMAT('Target measurement objective function for ',
     +        'group "',A,'" has been supplied as zero or negative.')
              GO TO 9890
            END IF
            IGTARG=IGTARG+1
          END IF
        END DO
        IF(IGTARG.NE.0)THEN
          DO I=1,NOBSGP
            IF(OBGNME(I)(1:5).NE.'regul')THEN
              IF(GTARG(I).LT.-1.0D35)THEN
                WRITE(ERRMSG,912)
912             FORMAT('A group-specific target measurement objective ',
     +          'function must be supplied for all non-regularisation ',
     +          'observation groups or for none of them.')
                GO TO 9890
              END IF
            END IF
          END DO
        END IF
        IF(IGTARG.EQ.1)THEN
          WRITE(ERRMSG,913)
913       FORMAT('Individual target measurement objective functions ',
     +    'can only be supplied if there is more than one ',
     +    'non-regularisation observation group.')
          GO TO 9890
        END IF

C -- The observation data section of the PEST control file is read.
C  This really ugly hack allows observations to be read from a
C  PEST Observation File (pof).  The variable LU is used to read observations
C  from the pst file when * observation data is encountered or from the pof
C  file when * observation files is encountered.

511     CALL FIND_HEADER(JFAIL,CTLUNIT,ILINE,'* observation',
     +  CLINE,AFILE)
        IF(JFAIL.NE.0) GO TO 9890
C -- Read observation data from PST file
        IF(INDEX(CLINE,'data').NE.0)THEN
          LU = CTLUNIT
C -- Read observation data from POF file
        ELSE IF(INDEX(CLINE,'file').NE.0)THEN
5111      CONTINUE
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 5111
          LU = 29
          OPEN(UNIT=LU,FILE=TRIM(CLINE),STATUS='OLD',
     +         ACTION='READ',IOSTAT=IERR)
          IF (IERR.NE.0) THEN
            WRITE(ERRMSG,'(2A)') 'Cannot open file ',TRIM(CLINE)
            GOTO 9890
          ENDIF
C -- Save PST file name and position
          AFILE0 = AFILE
          AFILE  = CLINE
          ILINE0 = ILINE
          ILINE  = 0
C -- Read POF header
512       READ(LU,'(A)',ERR=9000,END=9050) CLINE
          ILINE = ILINE+1
          IF (CLINE.EQ.' ' .or. CLINE(1:1).eq.'#') GOTO 512
          CALL LOWCAS(CLINE)
          IF (CLINE(1:3).NE.'pof')THEN
            ERRMSG = 'PEST observation file must begin with "pof"'
            GO TO 9890
          ENDIF
C -- What's this?
        ELSE
          ERRMSG = 'Unknown section '//TRIM(CLINE)
          GO TO 9890
        ENDIF

        ITEMP=1
        DO I=1,NOBS
550       ILINE=ILINE+1
          READ(LU,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 550
          CALL LOWCAS(CLINE)
          CALL LINSPL(JFAIL,4,LW,RW,CLINE)
          IF(JFAIL.NE.0) GO TO 9100
          AOBS(I)=CLINE(LW(1):RW(1))
          CALL NUMRD(JFAIL,1,LW(2),RW(2),ITEMP,OVAL(I),CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          CALL NUMRD(JFAIL,1,LW(3),RW(3),ITEMP,OWGHT(I),CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          ATEMP=CLINE(LW(4):RW(4))
          CALL WHICH1(JFAIL,NOBSGP,ITEMP,OBGNME,ATEMP)
          IF(JFAIL.NE.0) THEN
            CALL WRITINT(ALINE,ILINE)
            WRITE(ERRMSG,560) TRIM(ATEMP),TRIM(ALINE),TRIM(AFILE)
560         FORMAT('Unknown observation group "',A,'" at line ',A,
     +      ' of file ',A,'.')
            GO TO 9890
          END IF
          NOBGNM(I)=ITEMP
        END DO

C -- Close observation file and resume reading PST file
        IF (LU.NE.CTLUNIT) THEN
          CLOSE(LU)
          AFILE = AFILE0
          ILINE = ILINE0
        ENDIF

C -- A check is made for ordering of observations if run in
C    predictive analyser mode and external derivatives are used.

        IF(PESTMODE.EQ.2)THEN
          IF(JACFILE.EQ.1)THEN
            ATEMP12=OBGNME(NOBGNM(NOBS))
            IF(ATEMP12.NE.'predict')THEN
              WRITE(ERRMSG,562)
562           FORMAT('If PEST is run in predictive analysis mode ',
     +        'and if JACFILE is 1, the observation belonging to ',
     +        'observation group "predict" must be the last one ',
     +        'supplied in the "observation data" section.')
              GO TO 9890
            END IF
          END IF
#ifdef BEO
          ATEMP12=OBGNME(NOBGNM(NOBS))
          IF(ATEMP12.NE.'predict')THEN
            WRITE(ERRMSG,563)
563         FORMAT('If PEST is run in predictive analysis mode ',
     +      'and if BEOPEST is used, the observation belonging to ',
     +      'observation group "predict" must be the last one ',
     +      'supplied in the "observation data" section.')
            GO TO 9890
          END IF
#endif
        END IF

C -- The derivatives command line section is read.

        IF(JACFILE.NE.0)THEN
          CALL FIND_HEADER(JFAIL,CTLUNIT,ILINE,
     +    '* derivatives command line',CLINE,AFILE)
          IF(JFAIL.NE.0) GO TO 9890
580       ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) COMJAC
          IF(SKIPLINE(COMJAC)) GO TO 580
          COMJAC=ADJUSTL(COMJAC)
590       ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 590
          CALL SPACESUB(CLINE)
          CALL LINSPL(JFAIL,1,LW,RW,CLINE)
          EXTDERFLE=CLINE(LW(1):RW(1))
          CALL REMCHAR(EXTDERFLE,CHAR(211))
        END IF

C -- The model command line section is read.

        CALL FIND_HEADER(JFAIL,CTLUNIT,ILINE,
     +  '* model command line',CLINE,AFILE)
        IF(JFAIL.NE.0) GO TO 9890
        DO I=1,NUMCOM
610       ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) COMLIN(I)
          IF(SKIPLINE(COMLIN(I))) GO TO 610
          COMLIN(I)=ADJUSTL(COMLIN(I))
          IF(INDEX(COMLIN(I),'* mod').NE.0) GO TO 9150
        END DO

C -- The model input/output section of the PEST control file is read.

        CALL FIND_HEADER(JFAIL,CTLUNIT,ILINE,
     +  '* model input',CLINE,AFILE)
        IF(JFAIL.NE.0)THEN
          WRITE(ERRMSG,619) TRIM(AFILE)
619       FORMAT('Cannot find "* model input(/output)" section in ',
     +    'file ',A,'.')
          GO TO 9890
        END IF
        JJFLAG=0
        IF(INDEX(CLINE,'/output').EQ.0) JJFLAG=1
        DO I=1,NTPLFLE
620       ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 620
          CALL SPACESUB(CLINE)
          CALL LINSPL(JFAIL,2,LW,RW,CLINE)
          IF(JFAIL.NE.0) GO TO 9100
          TEMPFLE(I)=CLINE(LW(1):RW(1))
          CALL REMCHAR(TEMPFLE(I),CHAR(211))
          INFLE(I)=CLINE(LW(2):RW(2))
          CALL REMCHAR(INFLE(I),CHAR(211))
        END DO
        IF(JJFLAG.EQ.1)THEN
629       ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 629
          CALL LOWCAS(CLINE)
          IF(CLINE.NE.'* model output')THEN
            CALL WRITINT(ALINE,ILINE)
            WRITE(ERRMSG,628) TRIM(ALINE),TRIM(AFILE)
628         FORMAT('"* model output" section header expected at ',
     +      'line ',A,' of file ',A,'.')
            GO TO 9890
          END IF
        END IF
        DO I=1,NINSFLE
630       ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 630
          CALL SPACESUB(CLINE)
          CALL LINSPL(JFAIL,2,LW,RW,CLINE)
          IF(JFAIL.NE.0) GO TO 9100
          INSFLE(I)=CLINE(LW(1):RW(1))
          CALL REMCHAR(INSFLE(I),CHAR(211))
          OUTFLE(I)=CLINE(LW(2):RW(2))
          CALL REMCHAR(OUTFLE(I),CHAR(211))
        END DO

C -- Here we make sure that it is ok to run PEST with the /f switch.

        IF(PFILE.NE.0)THEN
          IF(SVDA.EQ.1)THEN
            WRITE(ERRMSG,1810)
1810        FORMAT('PEST cannot be run with the "/f" switch if it is ',
     +      'undertaking SVD-assisted parameter estimation.')
            GO TO 9890
          END IF
          IF(NUMCOM.GT.1)THEN
            WRITE(ERRMSG,1820)
1820        FORMAT('If multiple model command lines are employed ',
     +      'PEST cannot be run using the "/f" switch.')
            GO TO 9890
          END IF
          IF((PESTMODE.EQ.2).OR.(PESTMODE.EQ.4))THEN
            WRITE(ERRMSG,1831)
1831        FORMAT('PEST cannot be run using the "/f" switch if ',
     +      'PESTMODE is set to "prediction" or "pareto".')
            GO TO 9890
          END IF
          IF(IREFOBS.NE.0)THEN
            WRITE(ERRMSG,1840)
1840        FORMAT('PEST cannot be run using the "/f" switch if ',
     +      'observation re-referencing functionality is implemented.')
            GO TO 9890
          END IF
          IF(IRESTART.EQ.5)THEN
            WRITE(ERRMSG,1845)
1845        FORMAT('PEST cannot be run with both the "/i" and "/f" ',
     +      'switches.')
            GO TO 9890
          END IF
          IF(IRESTART.NE.0)THEN
            IF((IRESTART.NE.5.).AND.(IRESTART.NE.6))THEN
            WRITE(ERRMSG,1850)
1850        FORMAT('PEST cannot be restarted if run with the ',
     +      '"/f" switch. See PEST documentation for other ways in ',
     +      'which a prematurely-terminated "/f-switch" run can be ',
     +      'restarted.')
            GO TO 9890
            ELSE
            WRITE(ERRMSG,1851)
1851        FORMAT('PEST cannot be started with both the "/f" and ',
     +      '"/i" switches.')
            GO TO 9890
            END IF
          END IF
          IF(P1STRT.NE.0)THEN
            WRITE(ERRMSG,1860)
1860        FORMAT('PEST cannot be run with both the "/f" and "/p1" ',
     +      'switches.')
            GO TO 9890
          END IF
        END IF

C -- The prior information section of the PEST control file is read.

#ifdef BEO

        IF(BEOSLAVE)GO TO 6666
#endif

        IF(NPRIOR.GT.0)THEN
          CALL FIND_HEADER(JFAIL,CTLUNIT,ILINE,
     +    '* prior information',CLINE,AFILE)
          IF(JFAIL.NE.0) GO TO 9890
          CALL PRRRD(JFAIL,NPAR,NPRIOR,NESPAR,NOBS,NXROW,NOBSGP,
     +    NOBSGP1,ITRANS,X,NOBGNM,OBGNME,OVAL,OWGHT,APAR,AOBS,CLINE,
     +    ILINE,AFILE,CTLUNIT,QUICKPRIOR)
          IF(JFAIL.NE.0) THEN
            IFAIL=1
            GO TO 9999
          END IF
        END IF

C -- The predictive analysis section of the PEST control file is read.

        IF(PESTMODE.EQ.2)THEN
          CALL FIND_HEADER(JFAIL,CTLUNIT,ILINE,
     +    '* predictive analysis',CLINE,AFILE)
          IF(JFAIL.NE.0) GO TO 9890
650       ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 650
          CALL LINSPL(JFAIL,1,LW,RW,CLINE)
          CALL NUMRD(JFAIL,0,LW(1),RW(1),NPREDMAXMIN,RTEMP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          CALL LINSPL(JFAIL,2,LW,RW,CLINE)
          IF(JFAIL.EQ.0)THEN
            CALL NUMRD(JFAIL,0,LW(2),RW(2),PREDNOISE,RTEMP,CLINE)
            IF(JFAIL.NE.0) GO TO 9150
            IF((PREDNOISE.NE.0).AND.(PREDNOISE.NE.1)) GO TO 9150
          END IF
660       ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 660
          CALL LINSPL(JFAIL,3,LW,RW,CLINE)
          IF(JFAIL.NE.0) GO TO 9100
          CALL NUMRD(JFAIL,1,LW(1),RW(1),ITEMP,PD0,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          CALL NUMRD(JFAIL,1,LW(2),RW(2),ITEMP,PD1,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          CALL NUMRD(JFAIL,1,LW(3),RW(3),ITEMP,PD2,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
670       ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 670
          CALL LINSPL(JFAIL,2,LW,RW,CLINE)
          IF(JFAIL.NE.0) GO TO 9100
          CALL NUMRD(JFAIL,1,LW(1),RW(1),ITEMP,ABSPREDLAM,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          CALL NUMRD(JFAIL,1,LW(2),RW(2),ITEMP,RELPREDLAM,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          INITSCHFAC=1.0D0
          MULSCHFAC=2.0D0
          NSEARCH=0
          CALL LINSPL(JFAIL,3,LW,RW,CLINE)
          IF(JFAIL.EQ.0)THEN
            CALL LINSPL(JFAIL,5,LW,RW,CLINE)
            IF(JFAIL.NE.0)THEN
              GO TO 9100
            ELSE
              CALL NUMRD(JFAIL,1,LW(3),RW(3),ITEMP,INITSCHFAC,CLINE)
              IF(JFAIL.NE.0) GO TO 9150
              CALL NUMRD(JFAIL,1,LW(4),RW(4),ITEMP,MULSCHFAC,CLINE)
              IF(JFAIL.NE.0) GO TO 9150
              CALL NUMRD(JFAIL,0,LW(5),RW(5),NSEARCH,RTEMP,CLINE)
              IF(JFAIL.NE.0) GO TO 9150
            END IF
          END IF
          IF(NSEARCH.LT.1) NSEARCH=1
          IF(INITSCHFAC.LE.0.0D0) INITSCHFAC=1.0D0
          IF(MULSCHFAC.LE.1.0D0) MULSCHFAC=2.0D0
680       ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 680
          CALL LINSPL(JFAIL,2,LW,RW,CLINE)
          IF(JFAIL.NE.0) GO TO 9100
          CALL NUMRD(JFAIL,1,LW(1),RW(1),ITEMP,ABSPREDSWH,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          CALL NUMRD(JFAIL,1,LW(2),RW(2),ITEMP,RELPREDSWH,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
690       ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 690
          CALL LINSPL(JFAIL,4,LW,RW,CLINE)
          IF(JFAIL.NE.0) GO TO 9100
          CALL NUMRD(JFAIL,0,LW(1),RW(1),NPREDNORED,RTEMP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          CALL NUMRD(JFAIL,1,LW(2),RW(2),ITEMP,ABSPREDSTP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          CALL NUMRD(JFAIL,1,LW(3),RW(3),ITEMP,RELPREDSTP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          CALL NUMRD(JFAIL,0,LW(4),RW(4),NPREDSTP,RTEMP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
        END IF

C -- The regularisation section is read.

        IF(PESTMODE.EQ.3)THEN
          CALL FIND_HEADER(JFAIL,CTLUNIT,ILINE,
     +    '* regularisation',CLINE,AFILE)
          IF(JFAIL.NE.0) GO TO 9890
710       ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 710
          CALL WRITINT(ALINE,ILINE)
          VARTEXT='reg2measrat'
          UPVARTEXT='REG2MEASRAT'
          REG2MEASRAT=0.0                !default value
          CALL READ_MRUN_VAR(IFAIL,REG2MEASRAT,VARTEXT,UPVARTEXT,
     +    ILINE,AFILE,CLINE,ERRMSG,1,DUMFILE)
          IF(IFAIL.NE.0) GO TO 9890
          CALL LINSPL(JFAIL,2,LW,RW,CLINE)
          IF(JFAIL.NE.0) GO TO 9100
          CALL NUMRD(JFAIL,1,LW(1),RW(1),ITEMP,PHIMLIM,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          CALL NUMRD(JFAIL,1,LW(2),RW(2),ITEMP,PHIMACCEPT,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          FRACPHIM=-1.0D30
          LPOS=0
          CALL LINSPL(JFAIL,3,LW,RW,CLINE)
          IF(JFAIL.EQ.0)THEN
            ATEMP=CLINE(LW(3):RW(3))
            CALL UPCAS(ATEMP)
            IF(ATEMP.EQ.'MEMSAVE')THEN
              MEMSAVE=1
              LPOS=3
            ELSE IF(ATEMP.EQ.'NOMEMSAVE')THEN
              MEMSAVE=0
              LPOS=3
            ELSE
              CALL NUMRD(JFAIL,1,LW(3),RW(3),ITEMP,FRACPHIM,CLINE)
              IF(JFAIL.NE.0) THEN
                GO TO 9150
              ELSE
                IF(FRACPHIM.LE.0.0) FRACPHIM=-1.0E30
                IF(FRACPHIM.GE.1.0)THEN
                  WRITE(ERRMSG,720) TRIM(ALINE),TRIM(AFILE)
720               FORMAT('FRACPHIM must be less than one at line ',
     +            A,' of file ',A,'.')
                  GO TO 9890
                END IF
              END IF
              CALL LINSPL(JFAIL,4,LW,RW,CLINE)
              IF(JFAIL.EQ.0)THEN
                ATEMP=CLINE(LW(4):RW(4))
                CALL UPCAS(ATEMP)
                IF(ATEMP.EQ.'MEMSAVE')THEN
                  MEMSAVE=1
                  LPOS=4
                ELSE IF(ATEMP.EQ.'NOMEMSAVE')THEN
                  MEMSAVE=0
                  LPOS=4
                ELSE
                  GO TO 9150
                END IF
              END IF
            END IF
          END IF
          IF(LPOS.NE.0)THEN
            CALL LINSPL(JFAIL,LPOS+1,LW,RW,CLINE)
            IF(JFAIL.EQ.0)THEN
              ATEMP=CLINE(LW(LPOS+1):RW(LPOS+1))
              CALL UPCAS(ATEMP)
              IF(ATEMP.EQ.'CG')THEN
                WRITE(ERRMSG,729) TRIM(ALINE),TRIM(AFILE)
729             FORMAT('Use of the CG solver is no longer ',
     +          'supported by PEST: error occurs at line ',A,
     +          ' of file ',A,'.')
                GO TO 9890
              ELSE IF(ATEMP.EQ.'NOCG')THEN
                CONJGRAD=0
              ELSE
                GO TO 9150
              END IF
            END IF
          END IF
          IF((PHIMLIM.LE.0.0).OR.(PHIMACCEPT.LE.0.0)) THEN
            WRITE(ERRMSG,750) TRIM(ALINE),TRIM(AFILE)
750         FORMAT('PHIMLIM and PHIMACCEPT must be positive on line ',
     +      A,' of file ',A,'.')
            GO TO 9890
          END IF
          IF((PHIMLIM.GT.0.0).AND.
     +       (PHIMACCEPT.GT.PHIMLIM*1.2)) THEN
             WRITE(ERRMSG,7501) TRIM(ALINE),TRIM(AFILE)
7501         FORMAT('PHIMLIM cannot be greater than 1.2 times ',
     +       'PHIMACCEPT at line ',A,' of file ',A,'.')
             GO TO 9890
          END IF

760       ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 760
          CALL WRITINT(ALINE,ILINE)
          CALL LINSPL(JFAIL,3,LW,RW,CLINE)
          IF(JFAIL.NE.0) GO TO 9100
          CALL NUMRD(JFAIL,1,LW(1),RW(1),ITEMP,WFINIT,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          CALL NUMRD(JFAIL,1,LW(2),RW(2),ITEMP,WFMIN,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          CALL NUMRD(JFAIL,1,LW(3),RW(3),ITEMP,WFMAX,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          LINREG=0
          REGCONTINUE=0
          CALL LINSPL(JFAIL,4,LW,RW,CLINE)
          IF(JFAIL.EQ.0)THEN
            CALL LOWCAS(CLINE)
            IF(CLINE(LW(4):RW(4)).EQ.'linreg')THEN
              LINREG=1
            ELSE IF(CLINE(LW(4):RW(4)).EQ.'nonlinreg')then
              LINREG=0
            ELSE IF(CLINE(LW(4):RW(4)).EQ.'continue')then
              REGCONTINUE=1
            ELSE IF(CLINE(LW(4):RW(4)).EQ.'nocontinue')then
              REGCONTINUE=0
            ELSE
              GO TO 9150
            END IF
            CALL LINSPL(JFAIL,5,LW,RW,CLINE)
            IF(JFAIL.EQ.0)THEN
              IF(CLINE(LW(5):RW(5)).EQ.'linreg')THEN
                LINREG=1
              ELSE IF(CLINE(LW(5):RW(5)).EQ.'nonlinreg')then
                LINREG=0
              ELSE IF(CLINE(LW(5):RW(5)).EQ.'continue')then
                REGCONTINUE=1
              ELSE IF(CLINE(LW(5):RW(5)).EQ.'nocontinue')then
                REGCONTINUE=0
              ELSE
                GO TO 9150
              END IF
            END IF
          END IF

770       ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 770
          CALL WRITINT(ALINE,ILINE)
          CALL LINSPL(JFAIL,2,LW,RW,CLINE)
          IF(JFAIL.NE.0) GO TO 9100
          CALL NUMRD(JFAIL,1,LW(1),RW(1),ITEMP,WFFAC,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          CALL NUMRD(JFAIL,1,LW(2),RW(2),ITEMP,WFTOL,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          IREGADJ=0
          CALL LINSPL(JFAIL,3,LW,RW,CLINE)
          IF(JFAIL.EQ.0)THEN
            CALL NUMRD(JFAIL,0,LW(3),RW(3),IREGADJ,RTEMP,CLINE)
            IF(JFAIL.NE.0) THEN
              GO TO 9150
            ELSE
              IF((IREGADJ.NE.0).AND.(IREGADJ.NE.1).AND.
     +           (IREGADJ.NE.2).AND.(IREGADJ.NE.3).AND.
     +           (IREGADJ.NE.4).AND.(IREGADJ.NE.5))THEN
                 WRITE(ERRMSG,780) TRIM(ALINE),TRIM(AFILE)
780              FORMAT('IREGADJ must be 0, 1, 2, 3, 4 or 5 at line ',
     +           A,' of file ',A,'.')
                 GO TO 9890
              END IF
              IF((IREGADJ.EQ.4).OR.(IREGADJ.EQ.5))THEN
                CALL LINSPL(JFAIL,5,LW,RW,CLINE)
                IF(JFAIL.NE.0)THEN
                  IF(IREGADJ.EQ.4)THEN
                  WRITE(ERRMSG,781) TRIM(ALINE),TRIM(AFILE)
781               FORMAT('If IREGADJ is set to 4, two entries must ',
     +            'follow its value on line ',A,' of file ',a,'.')
                  ELSE
                  WRITE(ERRMSG,789) TRIM(ALINE),TRIM(AFILE)
                  END IF
                  GO TO 9890
                END IF
                CALL NUMRD(JFAIL,0,LW(4),RW(4),NOPTREGADJ,RTEMP,CLINE)
                IF(JFAIL.NE.0) GO TO 9150
                IF(NOPTREGADJ.LE.0)THEN
                  WRITE(ERRMSG,782) TRIM(ALINE),TRIM(AFILE)
782               FORMAT('NOPTREGADJ must be greater than zero at ',
     +            'line ',A,' of file ',A,'.')
                  GO TO 9890
                END IF
                CALL NUMRD(JFAIL,1,LW(5),RW(5),ITEMP,REGWEIGHTRAT,CLINE)
                IF(JFAIL.NE.0) GO TO 9150
                IF(ABS(REGWEIGHTRAT).LT.1.0D0)THEN
                  WRITE(ERRMSG,783)
783               FORMAT('Illegal value supplied for REGWEIGHTRAT ',
     +            'regularisation control variable.')
                  GO TO 9890
                END IF
                IF(IREGADJ.EQ.5)THEN
                  CALL LINSPL(JFAIL,6,LW,RW,CLINE)
                  IF(JFAIL.NE.0)THEN
                    WRITE(ERRMSG,789) TRIM(ALINE),TRIM(AFILE)
789                 FORMAT('If IREGADJ is set to 5, three entries must ',
     +              'follow its value on line ',A,' of file ',a,'.')
                    GO TO 9890
                  END IF
                  CALL NUMRD(JFAIL,1,LW(6),RW(6),ITEMP,REGSINGTHRESH,
     +            CLINE)
                  IF(JFAIL.NE.0) GO TO 9150
                  IF((REGSINGTHRESH.LE.0.0D0).OR.
     +               (REGSINGTHRESH.GE.1.0D0))THEN
                     WRITE(ERRMSG,792) TRIM(ALINE),TRIM(AFILE)
792                  FORMAT('Illegal value supplied for REGSINGTHRESH ',
     +               'at line ',a,' of file ',a,'.')
                     GO TO 9890
                  END IF
                  REGWEIGHTRAT=ABS(REGWEIGHTRAT)
                END IF
              END IF
            END IF
          END IF
C          IF((IREGADJ.NE.0).AND.(REGCONTINUE.NE.0))THEN
C            WRITE(ERRMSG,793)
C793         FORMAT('PEST does not allow REGCONTINUE to be ',
C     +      'set to "continue" if IREGADJ is nonzero.')
C            GO TO 9890
C          END IF
        END IF

C -- The pareto section is read.

        IF(PESTMODE.EQ.4)THEN

C -- But first we verify that there are no parameters for which a switch
C    will take place in derivatives calculation.

          DO I=1,NPARGP
            DO J=1,NPAR
              IF(DERCOM(J).EQ.0) CYCLE
              IF(IPARGP(J).EQ.I)THEN
                IF((ITRANS(J).EQ.0).OR.(ITRANS(J).EQ.1)) GO TO 1700
              END IF
            END DO
            GO TO 1710
1700        CONTINUE
            IF((FORCEN(I).EQ.1).OR.(FORCEN(I).EQ.4))THEN
              IF(FORCEN(I).EQ.1)THEN
                ATEMP12='switch'
              ELSE
                ATEMP12='switch_5'
              END IF
              WRITE(ERRMSG,1702) TRIM(PARGNME(I)),TRIM(ATEMP12)
1702          FORMAT('Parameter group "',A,'" has a FORCEN setting ',
     +        'of "',A,'". Derivative increment switching is not ',
     +        'allowed when PEST is run in PARETO mode.')
              go to 9890
            END IF
1710        CONTINUE
          END DO

          CALL FIND_HEADER(JFAIL,CTLUNIT,ILINE,
     +    '* pareto',CLINE,AFILE)
          IF(JFAIL.NE.0) GO TO 9890
1510      ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 1510
          CALL SPACESUB(CLINE)
          CALL WRITINT(ALINE,ILINE)
          CLINE=ADJUSTL(CLINE)
          IF((CLINE(1:1).EQ.'''').OR.(CLINE(1:1).EQ.'"'))THEN
            CLINE(1:1)=' '
            CLINE=ADJUSTL(CLINE)
          END IF
          NN=LEN_TRIM(CLINE)
          IF((CLINE(NN:NN).EQ.'''').OR.(CLINE(NN:NN).EQ.'"'))THEN
            CLINE(NN:NN)=' '
          END IF
          CALL LOWCAS(CLINE)
          ATEMP20=CLINE(1:20)
          CALL REMCHAR(ATEMP20,CHAR(211))
          ATEMP12=ADJUSTL(ATEMP20)
          DO I=1,NOBSGP
            IF(ATEMP12.EQ.OBGNME(I))THEN
              PAR_NOBGNM=I
              GO TO 1520
            END IF
          END DO
          WRITE(ERRMSG,1515) TRIM(ATEMP12),TRIM(ALINE),TRIM(AFILE)
1515      FORMAT('Unknown observation group "',A,'" at line ',A,
     +    ' of file ',A,'.')
          GO TO 9890
1520      CONTINUE
          DO I=1,NXROW
            IF(NOBGNM(I).EQ.PAR_NOBGNM) GO TO 1521
          END DO
          WRITE(ERRMSG,1522) TRIM(ATEMP12),TRIM(ALINE),TRIM(AFILE)
1522      FORMAT('Observation group "',A,'" cited at line ',A,
     +    ' of file ',A,' has no members. It must not be used ',
     +    'for Pareto weights adjustment.')
          GO TO 9890
1521      CONTINUE
          NN=0
          IF(COVIND(PAR_NOBGNM)) GO TO 1524
          DO I=1,NXROW
            IF(NOBGNM(I).EQ.PAR_NOBGNM) THEN
              IF(OWGHT(I).GT.0.0D0) GO TO 1524
            END IF
          END DO
          WRITE(ERRMSG,1523) TRIM(ATEMP12),TRIM(ALINE),TRIM(AFILE)
1523      FORMAT('Observation group "',A,'" cited at line ',A,
     +    ' of file ',A,' has no non-zero-weighted members. It ',
     +    'must not be used for Pareto weights adjustment.')
          GO TO 9890
1524      CONTINUE

1530      ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 1530
          CALL WRITINT(ALINE,ILINE)
          CALL LINSPL(JFAIL,3,LW,RW,CLINE)
          IF(JFAIL.NE.0) GO TO 9100
          CALL NUMRD(JFAIL,1,LW(1),RW(1),ITEMP,PAR_WTFAC_START,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          IF(PAR_WTFAC_START.LT.0) GO TO 9150
          CALL NUMRD(JFAIL,1,LW(2),RW(2),ITEMP,PAR_WTFAC_FIN,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          IF(PAR_WTFAC_FIN.LT.PAR_WTFAC_START) GO TO 9150
          CALL NUMRD(JFAIL,0,LW(3),RW(3),NUM_WTFAC_INC,RTEMP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          IF(NUM_WTFAC_INC.LE.0) GO TO 9150

1540      ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 1540
          CALL WRITINT(ALINE,ILINE)
          CALL LINSPL(JFAIL,3,LW,RW,CLINE)
          IF(JFAIL.NE.0) GO TO 9100
          CALL NUMRD(JFAIL,0,LW(1),RW(1),NUM_ITER_START,RTEMP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          IF(NUM_ITER_START.LT.0) GO TO 9150
          CALL NUMRD(JFAIL,0,LW(2),RW(2),NUM_ITER_GEN,RTEMP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          IF(NUM_ITER_GEN.LT.1) GO TO 9150
          CALL NUMRD(JFAIL,0,LW(3),RW(3),NUM_ITER_FIN,RTEMP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          IF(NUM_ITER_FIN.LT.0) GO TO 9150

1550      ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 1550
          CALL WRITINT(ALINE,ILINE)
          CALL LINSPL(JFAIL,1,LW,RW,CLINE)
          CALL NUMRD(JFAIL,0,LW(1),RW(1),ALT_TERM,RTEMP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          IF((ALT_TERM.NE.0).AND.(ALT_TERM.NE.1)) GO TO 9150

          IF(ALT_TERM.NE.0)THEN
1560        ILINE=ILINE+1
            READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
            IF(SKIPLINE(CLINE)) GO TO 1560
            CALL SPACESUB(CLINE)
            CALL WRITINT(ALINE,ILINE)
1570        CONTINUE
            NN=INDEX(CLINE,'''')
            IF(NN.NE.0)THEN
              CLINE(NN:NN)=' '
              GO TO 1570
            END IF
1580        CONTINUE
            NN=INDEX(CLINE,'"')
            IF(NN.NE.0)THEN
              CLINE(NN:NN)=' '
              GO TO 1580
            END IF
            CALL LOWCAS(CLINE)
            CALL LINSPL(JFAIL,4,LW,RW,CLINE)
            IF(JFAIL.NE.0) GO TO 9100
            ATEMP25=CLINE(LW(1):RW(1))
            CALL REMCHAR(ATEMP25,CHAR(211))
            ATEMP20=ADJUSTL(ATEMP25)
            DO I=1,NXROW
              IF(ATEMP20.EQ.AOBS(I))THEN
                NOBS_TERM=I
                GO TO 1600
              END IF
            END DO
            WRITE(ERRMSG,1590) TRIM(ATEMP20),TRIM(ALINE),TRIM(AFILE)
1590        FORMAT('Unknown observation/prior information item "',
     +      A,'" at line ',A,' of file ',A,'.')
            GO TO 9890
1600        CONTINUE
            IF(CLINE(LW(2):RW(2)).EQ.'above')THEN
              N_ABOVE_BELOW=1
            ELSE IF(CLINE(LW(2):RW(2)).EQ.'below')THEN
              N_ABOVE_BELOW=-1
            ELSE
              GO TO 9150
            END IF
            CALL NUMRD(JFAIL,1,LW(3),RW(3),ITEMP,OBS_THRESH,CLINE)
            IF(JFAIL.NE.0) GO TO 9150
            CALL NUMRD(JFAIL,0,LW(4),RW(4),NUM_ITER_THRESH,RTEMP,CLINE)
            IF(JFAIL.NE.0) GO TO 9150
            IF(NUM_ITER_THRESH.LT.1) GO TO 9150
          END IF

1610      ILINE=ILINE+1
          READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
          IF(SKIPLINE(CLINE)) GO TO 1610
          CALL WRITINT(ALINE,ILINE)
          CALL LINSPL(JFAIL,1,LW,RW,CLINE)
          CALL NUMRD(JFAIL,0,LW(1),RW(1),NOBS_REPORT,RTEMP,CLINE)
          IF(JFAIL.NE.0) GO TO 9150
          IF(NOBS_REPORT.LT.0) GO TO 9150

          IF(NOBS_REPORT.GT.0)THEN
            ALLOCATE(IOBS_REPORT(NOBS_REPORT),STAT=IERR)
            IF(IERR.NE.0) GO TO 9200
1650        ILINE=ILINE+1
            READ(CTLUNIT,'(A)',ERR=9000,END=9050) CLINE
            IF(SKIPLINE(CLINE)) GO TO 1650
            CALL WRITINT(ALINE,ILINE)
            CALL LOWCAS(CLINE)
            CALL SPACESUB(CLINE)
            DO I=1,NOBS_REPORT
              CALL LINSPL(JFAIL,1,LW,RW,CLINE)
              IF(JFAIL.NE.0) GO TO 9100
              ATEMP25=CLINE(LW(1):RW(1))
              CALL REMCHAR(ATEMP25,CHAR(211))
              ATEMP20=ADJUSTL(ATEMP25)
              DO J=1,NXROW
                IF(ATEMP20.EQ.AOBS(J))THEN
                  IOBS_REPORT(I)=J
                  GO TO 1660
                END IF
              END DO
              WRITE(ERRMSG,1590) TRIM(ATEMP20),TRIM(ALINE),TRIM(AFILE)
              GO TO 9890
1660          CONTINUE
              CLINE=CLINE(RW(1)+1:)
            END DO
          END IF
        END IF

C -- If necessary, covariance matrix files are read.

        NUMCOV=0
        DO I=1,NOBSGP
          IF(COVIND(I))NUMCOV=NUMCOV+1
        END DO
        IF(NUMCOV.NE.0)THEN
          IF(MAXCOMPDIM.GT.1)THEN
            WRITE(ERRMSG,790)
790         FORMAT('Jacobian matrix compression cannot be activated ',
     +      'if any observation covariance matrices are supplied.')
            GO TO 9890
          END IF
          IF(DOSENREUSE.EQ.'senreuse')THEN
            IF(NPRIOR.EQ.0)THEN
              WRITE(ERRMSG,791)
791           FORMAT('Sensitivity re-use functionality must not be ',
     +        'activated if observation covariance matrices are ',
     +        'assigned in PEST control file to observation groups ',
     +        'which are not exclusively prior information.')
              GO TO 9890
            ELSE
              DO I=1,NOBSGP
                IF(COVIND(I))THEN
                  DO J=1,NOBS
                    IF(NOBGNM(J).EQ.I) THEN
                      WRITE(ERRMSG,791)
                      GO TO 9890
                    END IF
                  END DO
                END IF
              END DO
            END IF
          END IF
        END IF
        ITEMP=MAX(NUMCOV,1)
        ALLOCATE(COVGP(ITEMP),NOBSCOV(ITEMP),COVAR(ITEMP),STAT=IERR)
        IF(IERR.NE.0) GO TO 9200

        ICOUNT=0
        DO I=1,NOBSGP
          IF(COVIND(I))THEN
            ICOUNT=ICOUNT+1
            COVGP(ICOUNT)=I
          END IF
        END DO

        IF(NUMCOV.GT.0)THEN
          DO I=1,NUMCOV
            IG=COVGP(I)
            ICOUNT=0
            DO J=1,NXROW
              IF(NOBGNM(J).EQ.IG) ICOUNT=ICOUNT+1
            END DO
            NOBSCOV(I)=ICOUNT
            COVAR(I)%NDIM=ICOUNT
            ICOUNT=MAX(ICOUNT,1)
            ALLOCATE(COVAR(I)%DVAL(ICOUNT,ICOUNT),STAT=IERR)
            IF(IERR.NE.0) GO TO 9200
          END DO
        END IF

        JCOV=0
        DO IG=1,NOBSGP
          IF(.NOT.COVIND(IG)) CYCLE
          JCOV=JCOV+1
          NN=NOBSCOV(JCOV)
          IF(NN.NE.0)THEN
            JUNIT=31
            OPEN(UNIT=JUNIT,FILE=COVFLE(IG),STATUS='OLD',IOSTAT=IERR)
            IF(IERR.NE.0)THEN
              CALL ADDQUOTE(COVFLE(IG),AFILE)
              WRITE(ERRMSG,820) TRIM(AFILE)
820           FORMAT('Cannot open covariance matrix file ',A,'.')
              GO TO 9890
            END IF

C -- Is the file a "matrix file"? If so it should be read as such.

            READ(JUNIT,'(A)',ERR=9400,END=9450) CLINE
            IF(CLINE.EQ.' ') GO TO 825
            CALL LINSPL(JFAIL,4,LW,RW,CLINE)
            IF(JFAIL.EQ.0)THEN
              CALL NUMRD(JFAIL,1,LW(4),RW(4),ITEMP,RTEMP,CLINE)
              IF(JFAIL.EQ.0) GO TO 825
            END IF
            CALL LINSPL(JFAIL,3,LW,RW,CLINE)
            IF(JFAIL.NE.0) GO TO 825
            CALL NUMRD(JFAIL,0,LW(1),RW(1),NNDIM1,RTEMP,CLINE)
            IF(JFAIL.NE.0) GO TO 825
            IF(NNDIM1.LE.0) GO TO 825
            CALL NUMRD(JFAIL,0,LW(2),RW(2),NNDIM2,RTEMP,CLINE)
            IF(JFAIL.NE.0) GO TO 825
            IF(NNDIM2.LE.0) GO TO 825
            CALL NUMRD(JFAIL,0,LW(3),RW(3),NNCODE,RTEMP,CLINE)
            IF(JFAIL.NE.0) GO TO 825
            IF((NNCODE.NE.-1).AND.(NNCODE.NE.1).AND.(NNCODE.NE.2))
     +      GO TO 825
            DO
              READ(JUNIT,'(A)',ERR=9400,END=814) CLINE
              CLINE=ADJUSTL(CLINE)
              IF(CLINE(1:1).EQ.'*') GO TO 812
            END DO
814         CONTINUE
            IF(NNCODE.EQ.2) GO TO 825
            GO TO 8121
812         CALL LOWCAS(CLINE)
            IF((INDEX(CLINE,'row').EQ.0).AND.
     +         (INDEX(CLINE,'name').EQ.0)) GO TO 825
8121        CONTINUE
            REWIND(UNIT=JUNIT)
            READ(JUNIT,'(A)',ERR=9400,END=9450) CLINE
            IF(NNDIM1.NE.NNDIM2)THEN
              CALL ADDQUOTE(COVFLE(IG),AFILE)
              WRITE(ERRMSG,815) TRIM(AFILE)
815           FORMAT('File ',A,' appears to be in PEST or PLPROC ',
     +        'matrix file format. The matrix contained in it is ',
     +        'not square.')
              GO TO 9890
            END IF
            IF(NNDIM1.NE.NN)THEN
              CALL ADDQUOTE(COVFLE(IG),AFILE)
              WRITE(ERRMSG,816) TRIM(AFILE)
816           FORMAT('File ',A,' appears to be in PEST or PLPROC ',
     +        'matrix file ',
     +        'format. The dimensions of the matrix contained in it',
     +        ' are not the same as the number of observations in ',
     +        'the observation group to which it is assigned.')
              GO TO 9890
            END IF
            IF(NNCODE.GT.0)THEN
              DO I=1,NN
                READ(JUNIT,*,ERR=9500,END=9550)
     +          (COVAR(JCOV)%DVAL(I,J),J=1,NN)
              END DO
            ELSE
              DO I=1,NN
                DO J=1,NN
                  COVAR(JCOV)%DVAL(J,I)=0.0D0
                END DO
              END DO
              DO I=1,NN
                READ(JUNIT,*,ERR=9500,END=9550) COVAR(JCOV)%DVAL(I,I)
              END DO
            END IF
            GO TO 6621
825         REWIND(UNIT=JUNIT)
            DO I=1,NN
              READ(JUNIT,*,ERR=9400,END=9450)
     +        (COVAR(JCOV)%DVAL(I,J),J=1,NN)
            END DO
            DO I=1,NN
              IF(COVAR(JCOV)%DVAL(I,I).LE.0.0D0) THEN
                CALL ADDQUOTE(COVFLE(IG),AFILE)
                WRITE(ERRMSG,830) TRIM(AFILE)
830             FORMAT('File ',A,' contains an illegal covariance ',
     +          'matrix.')
                GO TO 9890
              END IF
            END DO
            DO
              READ(JUNIT,'(A)',ERR=6621,END=6621) CLINE
              CLINE=ADJUSTL(CLINE)
              IF(CLINE(1:1).EQ.'*')THEN
                CALL LOWCAS(CLINE)
                IF(INDEX(CLINE,'row').NE.0) GO TO 9400
              END IF
            END DO
          END IF
6621      CONTINUE
          CLOSE(UNIT=JUNIT)
        END DO

6666    CONTINUE

C -- All supplied observation weights are squared.

        DO I=1,NXROW
          OWGHT(I)=OWGHT(I)*OWGHT(I)
        END DO

C -- The PEST control file is closed.

        CLOSE(UNIT=CTLUNIT,IOSTAT=IERR)

C -- If necessary, the data from a JUPITER external derivatives file is read.

        IF(JACFILE.EQ.2)THEN
          IF(SVDA.EQ.1)THEN
            WRITE(ERRMSG,6623)
6623        FORMAT('If SVD-Assist functionality is operative then, ',
     +      'as presently programmed, JACFILE must not be set to 2 ',
     +      'for external derivatives computation.')
            GO TO 9890
          END IF
          CALL READ_DERIV_INT_FILE(JFAIL,CTLUNIT)
          IF(JFAIL.NE.0) GO TO 9890
        ELSE
          ALLOCATE(DI_PAR(1),DI_OBS(1),DI_IES(1),
     +    DI_PARENT(1),STAT=IERR)
          IF(IERR.NE.0) GO TO 9200
        END IF

        GO TO 9999

9000    CALL WRITINT(ALINE,ILINE)
        WRITE(ERRMSG,9010) TRIM(ALINE),TRIM(AFILE)
9010    FORMAT('Error reading line ',A,' of PEST control file ',A,'.')
        GO TO 9890
9020    CALL WRITINT(ALINE,ILINE)
        WRITE(ERRMSG,9030) TRIM(ALINE),TRIM(AFILE)
9030    FORMAT('Contradictory entries on line ',A,' of PEST ',
     +  'control file ',A,'.')
        GO TO 9890
9050    WRITE(ERRMSG,9060) TRIM(AFILE)
9060    FORMAT('Unexpected end encountered to file ',A,'.')
        GO TO 9890
9100    CALL WRITINT(ALINE,ILINE)
        WRITE(ERRMSG,9110) TRIM(ALINE), TRIM(AFILE)
9110    FORMAT('Insufficient entries on line ',A,' of file ',A,'.')
        GO TO 9890
9150    CALL WRITINT(ALINE,ILINE)
        WRITE(ERRMSG,9160) TRIM(ALINE),TRIM(AFILE)
9160    FORMAT('Illegal entry on line ',A,' of file ',A,
     +  '. Check with PESTCHEK.')
        GO TO 9890
9200    WRITE(ERRMSG,9210)
9210    FORMAT('Cannot allocate sufficient memory to continue ',
     +  'execution.')
        GO TO 9890
9300    WRITE(ERRMSG,9310) TRIM(ATEMP),TRIM(ALINE),TRIM(AFILE)
9310    FORMAT('Unrecognised parameter "',A,'" at line ',A,
     +  ' of file ',A,'.')
        GO TO 9890
9400    CALL ADDQUOTE(COVFLE(IG),AFILE)
        WRITE(ERRMSG,9410) TRIM(AFILE)
9410    FORMAT('Error reading covariance matrix file ',A,'.')
        GO TO 9890
9450    CALL ADDQUOTE(COVFLE(IG),AFILE)
        WRITE(ERRMSG,9460) TRIM(AFILE)
9460    FORMAT('Unexpected end encountered to covariance matrix file ',
     +  A,'.')
        GO TO 9890
9500    CALL ADDQUOTE(COVFLE(IG),AFILE)
        WRITE(ERRMSG,9510) TRIM(AFILE)
9510    FORMAT('File ',A,' appears to be in PEST matrix file ',
     +  'format. Error reading covariance matrix from this file.')
        GO TO 9890
9550    CALL ADDQUOTE(COVFLE(IG),AFILE)
        WRITE(ERRMSG,9560) TRIM(AFILE)
9560    FORMAT('File ',A,' appears to be in PEST matrix file ',
     +  'format. Unexpected end encountered to this file. ')
        GO TO 9890
9600    CONTINUE
        CALL WRITINT(ALINE,ILINE)
        CALL ADDQUOTE(CTLFILE,AFILE)
        WRITE(ERRMSG,9610) TRIM(ALINE),TRIM(AFILE)
9610    FORMAT('Error reading value for ZEROSENVAL variable from ',
     +  'line ',A,' of file ',A,'.')
        GO TO 9890

9890   IFAIL=1

9999   IF(ALLOCATED(COVFLE))THEN
         DEALLOCATE(COVFLE,STAT=IERR)
       END IF
       RETURN

       END


       SUBROUTINE FIND_HEADER(JFAIL,IUNIT,ILINE,HEADER,CLINE,AFILE)

C -- Subroutine FIND_HEADER finds a specified header in the PEST control file.

       USE PESTDATA, ONLY: ERRMSG
       IMPLICIT NONE
       INTEGER       JFAIL,IUNIT,ILINE,N
       CHARACTER*(*) HEADER,CLINE,AFILE
       LOGICAL SKIPLINE

       JFAIL=0
       DO
         ILINE=ILINE+1
         READ(IUNIT,'(A)',ERR=9000,END=9000) CLINE
         IF(SKIPLINE(CLINE)) CYCLE
         CALL LOWCAS(CLINE)
         IF(CLINE.EQ.'* regularization')CLINE='* regularisation'
         N=MIN(15,LEN_TRIM(HEADER))
         IF(INDEX(CLINE,HEADER(1:N)).NE.0) RETURN
       END DO

9000   CONTINUE
       JFAIL=1
       WRITE(ERRMSG,9010) TRIM(HEADER(3:)),TRIM(AFILE)
9010   FORMAT('Cannot find "',A,'" section header in PEST control ',
     + 'file ',A,'.')
       RETURN

       END



       SUBROUTINE READ_DERIV_INT_FILE(IFAIL,IUNIT)

C -- Subroutine READ_DERIV_INT_FILE reads a JUPITER derivatives interface file.

       USE PESTDATA
       IMPLICIT NONE

       INTEGER  IFAIL,IUNIT,IERR,JFAIL,LW(1),RW(1),IPAR,IDEP,
     + J,IES,I,K,LTEMP,II,JJ
       CHARACTER*12 ATEMP12
       CHARACTER*20 ATEMP20
       CHARACTER*25 ATEMP,VARIABLE
       CHARACTER*256 AFILE
       CHARACTER*(20), ALLOCATABLE :: DI_TEMP(:)

       IFAIL=0

       DI_DERFILE=EXTDERFLE
       CALL ADDQUOTE(DI_DERFILE,AFILE)
       OPEN(UNIT=IUNIT,FILE=DI_DERFILE,STATUS='OLD',IOSTAT=IERR)
       IF(IERR.NE.0)THEN
         WRITE(ERRMSG,10) TRIM(AFILE)
10       FORMAT('Cannot open JUPITER derivatives interface file ',
     +   A,'.')
         GO TO 9890
       END IF

       VARIABLE='DERFILE'
20     CONTINUE
       READ(IUNIT,'(A)',ERR=9000,END=9050) CLINE
       IF(CLINE.EQ.' ') GO TO 20
       CLINE=ADJUSTL(CLINE)
       IF(CLINE(1:1).EQ.'#') GO TO 20
       CALL SPACESUB(CLINE)
       CALL LINSPL(JFAIL,1,LW,RW,CLINE)
       EXTDERFLE=CLINE(LW(1):RW(1))
       CALL REMCHAR(EXTDERFLE,CHAR(211))

       VARIABLE='NSKIP'
       READ(IUNIT,*,ERR=9000,END=9050) DI_NSKIP
       VARIABLE='NDEP or NPAR'
       READ(IUNIT,*,ERR=9000,END=9050) DI_NDEP,DI_NPAR
       IF((DI_NDEP.LE.0).OR.(DI_NPAR.LE.0))THEN
         WRITE(ERRMSG,25) TRIM(AFILE)
25       FORMAT('Illegal value supplied for NDEP or NPAR in ',
     +   'JUPITER derivatives interface file ',A,'.')
         GO TO 9890
       END IF
       LTEMP=MAX(NPAR,DI_NDEP)
       ALLOCATE(DI_PAR(DI_NPAR),DI_OBS(DI_NDEP),DI_IES(DI_NPAR),
     + DI_PARENT(DI_NPAR),DI_TEMP(LTEMP),STAT=IERR)
       IF(IERR.NE.0)THEN
         WRITE(ERRMSG,26)
26       FORMAT('Error allocating memory for handling external ',
     +   'derivatives supplied in JUPITER format.')
         GO TO 9890
       END IF
       VARIABLE='ORIENTATION'
       READ(IUNIT,'(A)',ERR=9000,END=9050) DI_ORIENTATION
       CALL LOWCAS(DI_ORIENTATION)
       IF((INDEX(DI_ORIENTATION,'row/dep').EQ.0).AND.
     +    (INDEX(DI_ORIENTATION,'row/par').EQ.0))THEN
         WRITE(ERRMSG,30) TRIM(AFILE)
30       FORMAT('ORIENTATION variable in JUPITER derivatives ',
     +   'interface file ',A,' must be "ROW/DEP" or "ROW/PAR".')
         GO TO 9890
       END IF
       VARIABLE='DERFORMAT'
       READ(IUNIT,*,ERR=9000,END=9050) DI_DERFORMAT
       CALL LOWCAS(DI_DERFORMAT)

       VARIABLE='parameter names'
       READ(IUNIT,*,ERR=9000,END=9050) ATEMP
       CALL LOWCAS(ATEMP)
       IF(INDEX(ATEMP,'param').EQ.0) GO TO 9000
       READ(IUNIT,*,ERR=9000,END=9050) (DI_TEMP(I),I=1,DI_NPAR)
       DO IPAR=1,DI_NPAR
         ATEMP12=DI_TEMP(IPAR)
         CALL LOWCAS(ATEMP12)
         CALL WHICH1(JFAIL,NPAR,J,APAR,ATEMP12)
         IF(JFAIL.NE.0)THEN
           WRITE(ERRMSG,50) TRIM(ATEMP12),TRIM(AFILE)
50         FORMAT('Unrecognized parameter "',A,'" cited in ',
     +     'JUPITER derivatives interface file ',A,'.')
           GO TO 9890
         END IF
         DI_PAR(IPAR)=J
         IES=0
         DO I=1,J
           IF(ITRANS(I).GE.0)IES=IES+1
         END DO
         DI_IES(IPAR)=IES
         DI_PARENT(IPAR)=0
         DO K=1,NPAR
           IF(-ITRANS(K).EQ.J)THEN
             DI_PARENT(IPAR)=DI_PARENT(IPAR)+1
           END IF
         END DO
60       CONTINUE
       END DO

       VARIABLE='dependent names'
       READ(IUNIT,*,ERR=9000,END=9050) ATEMP
       CALL LOWCAS(ATEMP)
       IF(INDEX(ATEMP,'depen').EQ.0) GO TO 9000
       READ(IUNIT,*,ERR=9000,END=9050) (DI_TEMP(I),I=1,DI_NDEP)
       DO IDEP=1,DI_NDEP
         ATEMP20=DI_TEMP(IDEP)
         CALL LOWCAS(ATEMP20)
         CALL WHICH1(JFAIL,NOBS,J,AOBS,ATEMP20)
         IF(JFAIL.NE.0)THEN
           WRITE(ERRMSG,150) TRIM(ATEMP20),TRIM(AFILE)
150        FORMAT('Unrecognized dependent (observation) "',A,
     +     '" cited in JUPITER derivatives interface file ',A,'.')
           GO TO 9890
         END IF
         DI_OBS(IDEP)=J
       END DO

C -- It is now established that tied and parent parameters are both in,
C    or both out of the JUPITER derivatives file.

       DO I=1,DI_NPAR
         IF(DI_PARENT(I).EQ.0) CYCLE
         II=DI_PAR(I)
         DO J=1,NPAR
           IF(-ITRANS(J).EQ.II)THEN
             DO K=1,DI_NPAR
               IF(DI_PAR(K).EQ.J) GO TO 160
             END DO
             WRITE(ERRMSG,170)
170          FORMAT('Both a tied parameter and its parent ',
     +       'parameter must be cited in the JUPITER ',
     +       'derivatives interface file, or neither must be cited.')
             GO TO 9890
160          CONTINUE
           END IF
         END DO
       END DO

       DO I=1,DI_NPAR
         II=DI_PAR(I)
         JJ=ITRANS(II)
         IF((JJ.LT.0).AND.(JJ.GT.-10000))THEN
           JJ=-JJ
           DO J=1,DI_NPAR
             IF(DI_PAR(J).EQ.JJ)GO TO 165
           END DO
           WRITE(ERRMSG,170)
           GO TO 9890
165        CONTINUE
         END IF
       END DO

C -- Other tests are made to ensure integrity of the dataset.

       IF(DI_NDEP.LT.NOBS)THEN
         DO I=1,NPAR
           IF(ITRANS(I).GE.0)THEN
             IF(DERCOM(I).EQ.0)THEN
               WRITE(ERRMSG,180)
180            FORMAT('If a JUPITER-protocol external derivatives ',
     +         'file is supplied in which some dependents ',
     +         '(ie. observations) are ommitted, then DERCOM must be ',
     +         'non-zero for all adjustable parameters.')
               GO TO 9890
             END IF
           END IF
         END DO
       ELSE
         IF(DI_NPAR.LT.NPAR)THEN
           DO I=1,NPAR
             DI_TEMP(I)=' '
           END DO
           DO I=1,DI_NPAR
             II=DI_PAR(I)
             DI_TEMP(II)='x'
           END DO
           DO I=1,NPAR
             IF(ITRANS(I).LT.0) CYCLE
             IF(DI_TEMP(I).EQ.' ')THEN
               IF(DERCOM(I).EQ.0)THEN
                 WRITE(ERRMSG,230) TRIM(APAR(I))
230              FORMAT('Parameter "',A,'" is not cited in ',
     +           'JUPITER derivatives interface file and must thus ',
     +           'have a non-zero DERCOM value.')
                 GO TO 9890
               END IF
             ELSE
               IF(DERCOM(I).NE.0)THEN
                 WRITE(ERRMSG,240) TRIM(APAR(I))
240              FORMAT('All dependents (observations) in ',
     +           '(J_)PEST control file are cited in JUPITER ',
     +           'derivatives interface file. Parameter "',A,
     +           '" cited in this file must have DERCOM value ',
     +           'of zero.')
                 GO TO 9890
               END IF
             END IF
           END DO
         ELSE
           DO I=1,NPAR
             IF(ITRANS(I).GE.-1000000)THEN
               IF(DERCOM(I).NE.0)THEN
                 WRITE(ERRMSG,250)
250              FORMAT('All dependents (observations) and ',
     +           'adjustable parameters in (J_)PEST control file ',
     +           'are cited in JUPITER derivatives interface ',
     +           'file. All parameters (except for fixed ones) ',
     +           'must therefore have a DERCOM value of zero.')
                 GO TO 9890
               END IF
             END IF
           END DO
         END IF
       END IF


200    CONTINUE
       CLOSE(UNIT=IUNIT)

       GO TO 9998

9000   WRITE(ERRMSG,9010) TRIM(AFILE),TRIM(VARIABLE)
9010   FORMAT('Error reading JUPITER derivatives ',
     + 'interface file ',A,'; trying to read ',A,'.')
       GO TO 9890
9050   WRITE(ERRMSG,9060) TRIM(AFILE),TRIM(VARIABLE)
9060   FORMAT('Unexpected end encountered to JUPITER derivatives ',
     + 'interface file ',A,'; trying to read ',A,'.')
       GO TO 9890

9890   IFAIL=1

9998   IF(ALLOCATED(DI_TEMP))DEALLOCATE(DI_TEMP,STAT=IERR)
9999   RETURN
       END


        SUBROUTINE PRRRD(JFAIL,NPAR,NPRIOR,NESPAR,NOBS,NXROW,NOBSGP,
     +  NOBSGP1,ITRANS,X,NOBGNM,AOBSGP,OVAL,OWGHT,APAR,AOBS,CLINE,
     +  ILINE,AFILE,IUNIT,QUICKPRIOR)

C -- SUBROUTINE PRREAD READS THE PRIOR INFORMATION SECTION OF THE
C -- PEST CONTROL FILE

        USE PESTDATA,  ONLY : ERRMSG,XDIM1,XDIM2,MAXCOMPDIM,
     +                        NCOMPDIM,XC,IXC,ISTART_C,IFOUND_C
        IMPLICIT NONE

        LOGICAL SKIPLINE
        INTEGER IFAIL,ILINE,NOBSGP,NOBSGP1,IOBSGP,JFAIL,IUNIT
        INTEGER NPAR,IPAR,NPRIOR,NESPAR,IES,NOBS,N1,N2,NBLC,ILOG,J,
     +  NXROW,IROW,JSTART,JOBS,JWGHT,JNAME,JX,I,NEG,JMATH,JMUL,JOGNME
        INTEGER QUICKPRIOR,LW(5),RW(5),NCOUNT,ICOL
        INTEGER ITRANS(NPAR)
        INTEGER NOBGNM(NXROW)
        DOUBLE PRECISION RTEMP
        DOUBLE PRECISION X(XDIM1,XDIM2),OVAL(NXROW),OWGHT(NXROW)
        CHARACTER*(*) AOBS(NXROW),APAR(NPAR),AOBSGP(NOBSGP)
        CHARACTER*10 FMT,AAPAR*12
        CHARACTER*12 ATEMP,ALINE
        CHARACTER*(*) CLINE,AFILE

        IF(QUICKPRIOR.EQ.1)THEN
          JFAIL=0
          ISTART_C=0
          IF(MAXCOMPDIM.LE.1)THEN
            DO IROW=NOBS+1,NOBS+NPRIOR
              DO IES=1,NESPAR
                 X(IROW,IES)=0.0D0
              END DO
            END DO
          END IF
          DO IROW=NOBS+1,NOBS+NPRIOR
            ILINE=ILINE+1
            READ(IUNIT,*,ERR=9000,END=9100) AOBS(IROW),OVAL(IROW),
     +      OWGHT(IROW),ATEMP
            CALL LOWCAS(AOBS(IROW))
            CALL LOWCAS(ATEMP)
            CALL WHICH1(IFAIL,NOBSGP,IOBSGP,AOBSGP,ATEMP)
            IF(IFAIL.NE.0) GO TO 9700
            NOBGNM(IROW)=IOBSGP
          END DO
          ILINE=ILINE+1
          READ(IUNIT,*,ERR=9000,END=9100) NCOUNT
          DO I=1,NCOUNT
            ILINE=ILINE+1
            READ(IUNIT,*,ERR=9000,END=9100) ICOL,IROW,RTEMP
            IF((ICOL.LT.1).OR.(ICOL.GT.NESPAR))THEN
              CALL WRITINT(ALINE,ILINE)
              WRITE(ERRMSG,6) TRIM(ALINE),TRIM(AFILE)
6             FORMAT('Adjustable parameter index out of range ',
     +        'at or near line ',A,' of file ',A,'.')
              GO TO 9891
            END IF
            IF((IROW.LT.NOBS+1).OR.(IROW.GT.NXROW))THEN
              CALL WRITINT(ALINE,ILINE)
              WRITE(ERRMSG,7) TRIM(ALINE),TRIM(AFILE)
7             FORMAT('Row index out of range at or near ',
     +        'line ',A,' of file ',A,'.')
              GO TO 9891
            END IF
            IF(MAXCOMPDIM.LE.1)THEN
              X(IROW,ICOL)=RTEMP
            ELSE
              CALL STORE_VALUE(IFAIL,NCOMPDIM,XC,IXC,RTEMP,IROW,ICOL)
              IF(IFAIL.NE.0) GO TO 9750
              ISTART_C=IFOUND_C
            END IF
          END DO
          RETURN
        END IF

        IOBSGP=0
        JFAIL=0
        IPAR=1
        JMUL=0
        JMATH=0
        JOBS=0
        JWGHT=0
        JNAME=0
        JOGNME=0
        JX=0
        NEG=0
        ISTART_C=0
        DO 10 I=1,NPRIOR
        IROW=I+NOBS
        JSTART=1
        IF(MAXCOMPDIM.LE.1)THEN
          DO 30 IES=1,NESPAR
30        X(IROW,IES)=0.0D0
        END IF

35      N2=0
351     CONTINUE
        READ(IUNIT,36,END=9100) CLINE
36      FORMAT(A)
        ILINE=ILINE+1
        IF(SKIPLINE(CLINE)) GO TO 351
        NBLC=LEN_TRIM(CLINE)

38      CALL GETINT(IFAIL,CLINE,N1,N2,NBLC,' ')
        IF(N1.EQ.0) GO TO 35
        IF(CLINE(N1:N1).EQ.'&') GO TO 38
        IF(JSTART.EQ.1) THEN
          AOBS(IROW)=CLINE(N1:N2)
          CALL LOWCAS(AOBS(IROW))
          JSTART=0
          JX=1
          NEG=0
        ELSE IF(CLINE(N1:N2).EQ.'=') THEN
          IF(JMATH.EQ.0) GO TO 9200
          JMATH=0
          JOBS=1
        ELSE IF(CLINE(N1:N2).EQ.'+') THEN
          IF(JMATH.EQ.0) GO TO 9200
          JMATH=0
          JX=1
          NEG=0
        ELSE IF(CLINE(N1:N2).EQ.'-') THEN
          IF(JMATH.EQ.0) GO TO 9200
          JMATH=0
          JX=1
          NEG=1
        ELSE IF(CLINE(N1:N2).EQ.'*') THEN
          IF(JMUL.EQ.0) GO TO 9200
          JMUL=0
          JNAME=1
        ELSE IF(JX.EQ.1) THEN
          WRITE(FMT,20,ERR=9200) N2-N1+1
20        FORMAT('(F',I3,'.0)')
          READ(CLINE(N1:N2),FMT,ERR=9200) RTEMP
          IF(NEG.EQ.1) RTEMP=-RTEMP
          JX=0
          JMUL=1
          NEG=0
        ELSE IF(JNAME.EQ.1) THEN
          CALL LOWCAS(CLINE(N1:N2))
          IF(CLINE(N1:N1+3).EQ.'log(') THEN
            AAPAR=CLINE(N1+4:N2-1)
            ILOG=1
          ELSE
            AAPAR=CLINE(N1:N2)
            ILOG=0
          END IF
          CALL WHICH1(IFAIL,NPAR,IPAR,APAR,AAPAR)
          IF(IFAIL.NE.0) GO TO 9300
          IF(ITRANS(IPAR).LT.0) GO TO 9400
          IF((ILOG.EQ.0).AND.(ITRANS(IPAR).EQ.1)) GO TO 9500
          IF((ILOG.EQ.1).AND.(ITRANS(IPAR).EQ.0)) GO TO 9600
          IES=0
          DO 50 J=1,NPAR
          IF(ITRANS(J).LT.0) GO TO 50
          IES=IES+1
          IF(J.EQ.IPAR) GO TO 60
50        CONTINUE
60        CONTINUE
          IF(RTEMP.NE.0.0D0) THEN
            IF(MAXCOMPDIM.LE.1)THEN
              X(IROW,IES)=RTEMP
            ELSE
              CALL STORE_VALUE(IFAIL,NCOMPDIM,XC,IXC,RTEMP,IROW,IES)
              IF(IFAIL.NE.0) GO TO 9750
              ISTART_C=IFOUND_C
            END IF
          END IF
          JNAME=0
          JMATH=1
        ELSE IF(JOBS.EQ.1) THEN
          WRITE(FMT,20,ERR=9200) N2-N1+1
          READ(CLINE(N1:N2),FMT,ERR=9200) OVAL(IROW)
          JOBS=0
          JWGHT=1
        ELSE IF(JWGHT.EQ.1) THEN
          WRITE(FMT,20,ERR=9200) N2-N1+1
          READ(CLINE(N1:N2),FMT,ERR=9200) OWGHT(IROW)
          JWGHT=0
          IF(CLINE(N2+1:).EQ.' ')THEN
            NOBGNM(IROW)=-1
            GO TO 10
          ELSE
            JOGNME=1
          END IF
        ELSE IF(JOGNME.EQ.1)THEN
          IF(NOBSGP1.EQ.-999)THEN
            NOBGNM(IROW)=-1
            GO TO 10
          END IF
          ATEMP=CLINE(N1:N2)
          CALL LOWCAS(ATEMP)
          CALL WHICH1(IFAIL,NOBSGP,IOBSGP,AOBSGP,ATEMP)
          IF(IFAIL.NE.0) GO TO 9700
          NOBGNM(IROW)=IOBSGP
          JOGNME=0
          GO TO 10
        ELSE
          GO TO 9200
        END IF
        GO TO 38
10      CONTINUE
        RETURN


9000    CALL WRITINT(ALINE,ILINE)
        WRITE(ERRMSG,9010) TRIM(ALINE),TRIM(AFILE)
9010    FORMAT('Error reading compressed prior information on or ',
     +  'near line ',A,' of file ',A)
        GO TO 9891
9100    WRITE(ERRMSG,9110) TRIM(AFILE)
9110    FORMAT('Unexpected end encountered to file ',A,' while ',
     +  'reading prior information.')
        GO TO 9891
9200    CALL WRITINT(ALINE,ILINE)
        WRITE(ERRMSG,9210) TRIM(ALINE),TRIM(AFILE)
9210    FORMAT('Error reading prior information from line ',A,
     +  ' of file ',A)
        GO TO 9891
9300    CALL WRITINT(ALINE,ILINE)
        WRITE(ERRMSG,9310) TRIM(ALINE),TRIM(AFILE)
9310    FORMAT('Incorrect parameter name or improper syntax in prior ',
     +  'information: line ',A,' of file ',A)
        GO TO 9891
9400    CALL WRITINT(ALINE,ILINE)
        WRITE(ERRMSG,9410) TRIM(APAR(IPAR)),TRIM(ALINE),TRIM(AFILE)
9410    FORMAT('Parameter cited in prior information is fixed or ',
     +  'tied: parameter "',A,'" on line ',A,' of file ',A)
        GO TO 9891
9500    CALL WRITINT(ALINE,ILINE)
        WRITE(ERRMSG,9510) TRIM(APAR(IPAR)),TRIM(ALINE),TRIM(AFILE)
9510    FORMAT('Log transformed parameter referenced as ',
     +  'untransformed in prior information: parameter "',A,
     +  '" on line ',A,' of file ',A)
        GO TO 9891
9600    CALL WRITINT(ALINE,ILINE)
        WRITE(ERRMSG,9610) TRIM(APAR(IPAR)),TRIM(ALINE),TRIM(AFILE)
9610    FORMAT('Untransformed parameter referenced as ',
     +  'log-transformed in prior information: parameter "',A,
     +  '" on line ',A,' of file ',A)
        GO TO 9891
9700    CALL WRITINT(ALINE,ILINE)
        WRITE(ERRMSG,9710) TRIM(ATEMP),TRIM(ALINE),TRIM(AFILE)
9710    FORMAT('Unrecognised observation group ("',A,'") on line ',A,
     +  ' of file ',A)
        GO TO 9891
9750    WRITE(ERRMSG,9760)
9760    FORMAT('MAXCOMPDIM must be set larger for greater compressed ',
     +  'storage capacity.')
        GO TO 9891

9891    JFAIL=1
        RETURN

        END



       SUBROUTINE PEST_DATA_DEALLOCATE(IFAIL)

C -- Subroutine PESTDATA_DEALLOCATE de-allocates memory
C    used by the PESTDATA module.

       USE PESTDATA
       IMPLICIT NONE

       INTEGER :: IFAIL
       INTEGER :: IERR
       INTEGER :: I

       IFAIL=0
       DEALLOCATE(COMLIN,STAT=IERR)
       DEALLOCATE(TEMPFLE,INFLE,INSFLE,OUTFLE,STAT=IERR)
       DEALLOCATE(PARGNME,INCTYP,DERINC,DERINCLB,DERINCMUL,FORCEN,
     + DERMTHD,STAT=IERR)
       DEALLOCATE(SPLITTHRESH,SPLITRELDIFF,SPLITACTION,STAT=IERR)
       DEALLOCATE(APAR,ITRANS,PARCHGLIM,PVAL,PARLBND,PARUBND,IPARGP,
     + SCALE,OFFSET,DERCOM,STAT=IERR)
       DEALLOCATE(OBGNME,COVIND,GTARG,STAT=IERR)
       IF(NUMCOV.GT.0)THEN
         DO I=1,NUMCOV
           IF(ASSOCIATED(COVAR(I)%DVAL))THEN
             DEALLOCATE(COVAR(I)%DVAL,STAT=IERR)
             NULLIFY(COVAR(I)%DVAL)
           END IF
         END DO
       END IF
       DEALLOCATE(COVGP,NOBSCOV,COVAR,STAT=IERR)
       DEALLOCATE(AOBS,OVAL,OWGHT,NOBGNM,STAT=IERR)
       DEALLOCATE(X,STAT=IERR)
       DEALLOCATE(DI_PAR,DI_OBS,DI_IES,DI_PARENT,STAT=IERR)
       DEALLOCATE(IXC,XC,STAT=IERR)
       DEALLOCATE(MODVAL_O,PVAL_O,STAT=IERR)
       DEALLOCATE(IXPR,IXPC,XPVAL,IESTRANS,IXP_RIGHT,IXP_FIRST,
     + STAT=IERR)
       DEALLOCATE(PHIABANDON,STAT=IERR)
       DEALLOCATE(IOBS_REPORT,STAT=IERR)
       DEALLOCATE(IPAR2IES,IES2IPAR,IESTRANS,PI_PAR,STAT=IERR)


       RETURN

       END




       subroutine get_absparmax(ifail,iline,afile)

       use pestdata
       implicit none

       integer, intent(out)      :: ifail
       integer, intent(in)       :: iline
       character*(*), intent(in) :: afile

       integer                   :: n,m,absind,i
       double precision          :: dtemp
       character*20              :: aline,atemp

       ifail=0
       call writint(aline,iline)
       do i=1,MAXABSPARMAX
         absparmax(i)=-1.1e35
       end do

10     continue
       n=index(cline,'absparmax')
       if(n.eq.0) go to 9990

       cline(n:)=adjustl(cline(n+9:))
       if(cline(n:n).ne.'(') go to 9000
       m=index(cline(n:),')')
       if(m.eq.0) go to 9000
       if(m.eq.2) go to 9000
       atemp=cline(n+1:n+m-2)
       call intrd1(ifail,atemp,absind)
       if(ifail.ne.0) go to 9000
       if((absind.lt.1).or.(absind.gt.MAXABSPARMAX)) go to 9100
       if(absparmax(absind).gt.-1.0e35) go to 9200
       cline(n:)=adjustl(cline(n+m:))
       m=index(cline(n:),'=')
       if(m.eq.0) go to 9000
       if(m.gt.1)then
         if(cline(1:m-1).ne.' ') go to 9000
       end if
       cline(n:)=adjustl(cline(n+m:))
       m=index(cline(n:),' ')
       if(m.le.1) go to 9300
       atemp=cline(n:n+m-1)
       call drealrd1(ifail,atemp,dtemp)
       if(ifail.ne.0) go to 9300
       if(dtemp.le.0.0d0) go to 9400
       absparmax(absind)=dtemp
       cline(n:)=cline(n+m:)
       go to 10

9000   write(errmsg,9010) trim(aline),trim(afile)
9010   format('Incorrect specification of ABSPARMAX data ',
     + 'line ',a,' of file ',a,'.')
       go to 9890
9100   write(errmsg,9110) trim(aline),trim(afile)
9110   format('ABSPARMAX index out of range at line ',a,
     + ' of file ',a,'.')
       go to 9890
9200   write(errmsg,9210) trim(aline),trim(afile)
9210   format('ABSPARMAX index referenced twice at line ',a,
     + ' of file ',a,'.')
       go to 9890
9300   write(errmsg,9310) trim(aline),trim(afile)
9310   format('Cannot read value of ABSPARMAX variable at ',
     + 'line ',a,' of file ',a,'.')
       go to 9890
9400   write(errmsg,9410) trim(aline),trim(afile)
9410   format('ABSPARMAX variable negative at line ',a,
     + ' of file ',a,'.')
       go to 9890

9890   continue
       ifail=1

9990   continue

       return

       end


        SUBROUTINE INTRD1(IFAIL,CLINE,ITEMP)

C -- Subroutine INTRD reads an integer from a string.

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
#ifdef PESTMOD
        END SUBROUTINE INTRD
#else
        END
#endif



        SUBROUTINE DREALRD1(IFAIL,CLINE,RTEMP)

C -- Subroutine DREALRD reads a real number from a string.

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
#ifdef PESTMOD
        END SUBROUTINE DREALRD1
#else
        END
#endif



        SUBROUTINE READ_STOPHOURS(IFAIL,ILINE,ITYPE,STOPHOURS,AFILE,
     +  CLINE,ERRMSG)

        IMPLICIT NONE

        INTEGER IFAIL,ILINE,ITYPE
        DOUBLE PRECISION STOPHOURS
        CHARACTER*(*) AFILE
        CHARACTER*(*) CLINE
        CHARACTER*(*) ERRMSG

        INTEGER NN,I
        CHARACTER*10 ALINE
        CHARACTER*13 AVAR,AVARL
        CHARACTER*25 ATEMP25
        CHARACTER*1000 DLINE

        IFAIL=0
        IF(ITYPE.EQ.1)THEN
          AVAR='HARDSTOPHOURS'
        ELSE
          AVAR='SOFTSTOPHOURS'
        END IF
        AVARL=AVAR
        CALL LOWCAS(AVARL)
        DLINE=CLINE
        CALL LOWCAS(DLINE)
        NN = INDEX(DLINE,TRIM(AVARL))
        IF(NN.EQ.0)THEN
          STOPHOURS=-1.1D100
          RETURN
        END IF

        ATEMP25=ADJUSTL(DLINE(NN+13:))
        IF(ATEMP25(1:1).NE.'=')THEN
          CALL WRITINT(ALINE,ILINE)
          WRITE(ERRMSG,10) TRIM(AVAR),TRIM(ALINE),TRIM(AFILE)
10        FORMAT('Cannot read value of ',A,' variable ',
     +    'from line ',A,' of file ',A,'.')
          GO TO 9890
        END IF
        ATEMP25=ADJUSTL(ATEMP25(2:))
        NN=INDEX(ATEMP25,' ')
        IF((NN.EQ.0).OR.(NN.EQ.1))THEN
          CALL WRITINT(ALINE,ILINE)
          WRITE(ERRMSG,10) TRIM(AVAR),TRIM(ALINE),TRIM(AFILE)
          GO TO 9890
        END IF
        ATEMP25(NN:)=' '
        CALL DREALRD1(IFAIL,ATEMP25,STOPHOURS)
        IF(IFAIL.NE.0) THEN
          CALL WRITINT(ALINE,ILINE)
          WRITE(ERRMSG,10) TRIM(AVAR),TRIM(ALINE),TRIM(AFILE)
          GO TO 9890
        END IF
        IF(STOPHOURS.LE.0.0D0)THEN
          CALL WRITINT(ALINE,ILINE)
          WRITE(ERRMSG,20) TRIM(AVAR),TRIM(ALINE),TRIM(AFILE)
20        FORMAT(A,' must be greater than zero at ',
     +    'line ',A,' of file ',a,'.')
          GO TO 9890
        END IF
        NN=INDEX(DLINE,TRIM(AVARL))
        CLINE(NN:)=CLINE(NN+13:)
        DO I=NN,LEN(CLINE)
          IF(CLINE(I:I).EQ.'=')THEN
            CLINE(NN:)=CLINE(I+1:)
            GO TO 30
          END IF
        END DO
30      CONTINUE
        CLINE(NN:)=ADJUSTL(CLINE(NN:))
        DO I=NN,LEN(CLINE)
          IF(CLINE(I:I).EQ.' ')THEN
            CLINE(NN:I-1)=' '
            GO TO 40
          END IF
        END DO
40      CONTINUE
        DLINE=CLINE
        CALL LOWCAS(DLINE)
        NN=INDEX(DLINE,TRIM(AVARL))
        IF(NN.NE.0)THEN
          CALL WRITINT(ALINE,ILINE)
          WRITE(ERRMSG,50) TRIM(AVAR),TRIM(ALINE),TRIM(AFILE)
50        FORMAT(A,' variable appears twice on line ',A,
     +    ' of file ',A,'.')
          GO TO 9890
        END IF
        GO TO 9990

9890    CONTINUE
        IFAIL=1

9990    CONTINUE
        RETURN
        END




        SUBROUTINE READ_MRUN_VAR(IFAIL,DVAL,VARTEXT,UPVARTEXT,ILINE,
     +  AFILE,CLINE,ERRMSG,ITYPE,BFILE)

        IMPLICIT NONE
        INTEGER IFAIL,ILINE,ITYPE
        DOUBLE PRECISION DVAL
        CHARACTER*1 AA
        CHARACTER*(*) VARTEXT,UPVARTEXT
        CHARACTER*(*) AFILE,CLINE,ERRMSG,BFILE

        INTEGER  NN,NNLT,I
        CHARACTER*20 ALINE
        CHARACTER*25 ATEMP25
        CHARACTER*256 ATEMP256
        CHARACTER*2000 DLINE

        IFAIL=0
        DLINE=CLINE
        CALL LOWCAS(DLINE)
        AA=' '
        IF(ITYPE.EQ.2)BFILE=' '
        NNLT=LEN_TRIM(VARTEXT)
        NN = INDEX(DLINE,TRIM(VARTEXT))
        IF(NN.EQ.0) THEN
          GO TO 9900
        ELSE
          ATEMP256=ADJUSTL(CLINE(NN+NNLT:))
          IF(ATEMP256(1:1).NE.'=')THEN
            CALL WRITINT(ALINE,ILINE)
            WRITE(ERRMSG,2105) TRIM(UPVARTEXT),TRIM(ALINE),TRIM(AFILE)
2105        FORMAT('Cannot read value of ',A,' variable ',
     +      'on line ',A,' of file ',A,'.')
            GO TO 9890
          END IF
          ATEMP256=ADJUSTL(ATEMP256(2:))
          NN=INDEX(ATEMP256,' ')
          IF((NN.EQ.0).OR.(NN.EQ.1))THEN
            CALL WRITINT(ALINE,ILINE)
            WRITE(ERRMSG,2105) TRIM(UPVARTEXT),TRIM(ALINE),TRIM(AFILE)
            GO TO 9890
          END IF
          IF(ITYPE.EQ.1)THEN
            ATEMP25=ATEMP256
            ATEMP25(NN:)=' '
            CALL DREALRD1(IFAIL,ATEMP25,DVAL)
            IF(IFAIL.NE.0) THEN
              CALL WRITINT(ALINE,ILINE)
              WRITE(ERRMSG,2105) TRIM(UPVARTEXT),TRIM(ALINE),TRIM(AFILE)
              GO TO 9890
            END IF
          ELSE
            AA=' '
            IF(ATEMP256(1:1).EQ.'''')THEN
              AA=''''
            ELSE IF(ATEMP256(1:1).EQ.'"')THEN
              AA='"'
            END IF
            IF(AA.NE.' ')THEN
              NN=INDEX(ATEMP256(2:),AA)
              IF(NN.EQ.0)THEN
                CALL WRITINT(ALINE,ILINE)
                WRITE(ERRMSG,2106) TRIM(UPVARTEXT),TRIM(ALINE),
     +                             TRIM(AFILE)
2106            FORMAT('Cannot read ',a,' filename on line ',A,
     +          ' of file ',A,'.')
                GO TO 9890
              END IF
              ATEMP256(NN+1:)=' '
              BFILE=ATEMP256(2:)
            ELSE
              BFILE=ADJUSTL(ATEMP256(1:NN-1))
            END IF
            IF(BFILE.EQ.' ')THEN
              CALL WRITINT(ALINE,ILINE)
              WRITE(ERRMSG,2106) TRIM(UPVARTEXT),TRIM(ALINE),
     +                           TRIM(AFILE)
              GO TO 9890
            END IF
          END IF
          NN=INDEX(DLINE,TRIM(VARTEXT))
          DLINE(NN:)=DLINE(NN+NNLT:)
          DO I=NN,LEN(DLINE)
            IF(DLINE(I:I).EQ.'=')THEN
              DLINE(NN:)=DLINE(I+1:)
              GO TO 21061
            END IF
          END DO
21061     CONTINUE
          DLINE(NN:)=ADJUSTL(DLINE(NN:))
          IF(AA.NE.' ')DLINE(NN:)=ADJUSTL(DLINE(NN+1:))
          DO I=NN,LEN(DLINE)
            IF(DLINE(I:I).EQ.AA)THEN
              IF(AA.NE.' ')DLINE(I:I)=' '
              DLINE(NN:I-1)=' '
              GO TO 2108
            END IF
          END DO
2108      CONTINUE
          NN=INDEX(DLINE,TRIM(VARTEXT))
          IF(NN.NE.0)THEN
            CALL WRITINT(ALINE,ILINE)
            WRITE(ERRMSG,2107) TRIM(UPVARTEXT),TRIM(ALINE),TRIM(AFILE)
2107        FORMAT(A,' variable appears twice on line ',A,
     +      ' of file ',A,'.')
            GO TO 9890
          END IF
        END IF
        GO TO 9900

9890    CONTINUE
        IFAIL=1
9900    CONTINUE
        CLINE=DLINE
        RETURN
        END


       logical function skipline(cline)

       implicit none
       character*(*) cline
       integer nn,ll,icount,jcount,i

       skipline=.FALSE.
       cline=adjustl(cline)
       if((cline.eq.' ').or.(cline(1:2).eq.'++')) then
         skipline=.TRUE.
         go to 200
       end if
       nn=index(cline,'#')
       if(nn.eq.0)then
         go to 200
       else if(nn.eq.1)then
         skipline=.TRUE.
         go to 200
       end if
       ll=len_trim(cline)
       icount=0
       jcount=0
       do i=1,ll
         if(cline(i:i).eq.'''') then
           icount=1-icount
         else if(cline(i:i).eq.'"') then
           jcount=1-jcount
         else if(cline(i:i).eq.'#') then
           if((cline(i-1:i-1).eq.' ').or.                 ! Notice that we require the space before #
     +        (cline(i-1:i-1).eq.char(9))) then           ! This allows a filename to have a # in it (mostly)
             if((icount.eq.0).and.(jcount.eq.0)) then
               cline(i:)=' '
               if(cline.eq.' ')then
                 skipline=.TRUE.
               else
                 skipline=.FALSE.
               end if
               go to 200
             end if
           end if
         end if
       end do

200    continue
       return
       end
