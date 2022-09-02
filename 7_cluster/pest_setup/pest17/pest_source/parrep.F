	PROGRAM PARREP

C -- PROGRAM PARREP REPLACES PARAMETER DATA IN A PEST CONTROL FILE BY THOSE
C    IN A PARAMETER VALUE FILE.

        IMPLICIT NONE

        LOGICAL SKIPLINE
        LOGICAL LEXIST,LOPENED
        INTEGER IFAIL,IERR,ILINE,NUMPAR,IIPAR,I,IOK,IIWARN
        INTEGER INEEDED,IPASS,NPAR,NOBS,NPARGP,NOBSGP,NPRIOR,IPAR,JPAR
        INTEGER NBLNK
        INTEGER NEQQ
        INTEGER LT4
        INTEGER NEW_NOPTMAX,LINE_COUNT_LOC
        INTEGER LW(20),RW(20)
        DOUBLE PRECISION RVAL,RSCALE,ROFFSET
        CHARACTER*256 PARFILE,PESTFILE1,PESTFILE2
        CHARACTER*12 ATRANS,AAPAR1,AAPAR2
        CHARACTER*12 AVERSION
        CHARACTER*10 ALINE
        CHARACTER*10 ATEMP
        CHARACTER*500 CLINE,DLINE

        INTEGER ITRANS(:)
	DOUBLE PRECISION VALUE(:),SCALE(:),OFFSET(:),PVAL(:),NEWPVAL(:)
	CHARACTER*12 APAR(:),AAPAR(:)

	ALLOCATABLE::VALUE,SCALE,OFFSET,APAR,PVAL,NEWPVAL,ITRANS,
     +  AAPAR

        INCLUDE 'version.inc'
	WRITE(6,10) TRIM(AVERSION)
10	FORMAT(' PARREP Version ',A,'. Watermark Numerical ',
     +  'Computing.')
	WRITE(6,*)
#ifdef NO_CMDLINE
        WRITE(*,*)
1       WRITE(*,'(A)') ' ENTER COMMAND LINE ARGUMENTS > '
	READ(*,'(A)',ERR=1) CLINE
#else
	CALL PGETCL (CLINE)
#endif

        NEW_NOPTMAX=-99999999
        CALL SPACESUB(CLINE)
        CALL LINSPL(IFAIL,5,LW,RW,CLINE)
        IF(IFAIL.EQ.0) GO TO 9000
        CALL LINSPL(IFAIL,4,LW,RW,CLINE)
        IF(IFAIL.EQ.0) THEN
          CALL INTREAD(IFAIL,CLINE(LW(4):RW(4)),NEW_NOPTMAX)
          IF(IFAIL.NE.0) GO TO 9000
          CLINE(LW(4):RW(4))=' '
        END IF
        CALL LINSPL(IFAIL,3,LW,RW,CLINE)
        IF(IFAIL.NE.0) GO TO 9000
        PARFILE=CLINE(LW(1):RW(1))
        PESTFILE1=CLINE(LW(2):RW(2))
        PESTFILE2=CLINE(LW(3):RW(3))
        CALL REMCHAR(PARFILE,CHAR(211))
        CALL REMCHAR(PESTFILE1,CHAR(211))
        CALL REMCHAR(PESTFILE2,CHAR(211))
#ifdef CAPFILE
        CALL UPCAS(PARFILE)
        CALL UPCAS(PESTFILE1)
        CALL UPCAS(PESTFILE2)
#endif

	IF(PESTFILE1.EQ.PESTFILE2)THEN
	  WRITE(6,40)
40	  FORMAT(/,' Input and output PEST control files cannot have ',
     +    'same name.')
	  GO TO 9900
	END IF

C -- INITIALISATION

	IOK=0
        IIWARN=0
        INEEDED=0
        IPASS=1

	OPEN(UNIT=10,FILE=PARFILE,STATUS='OLD',IOSTAT=IERR)
	IF(IERR.NE.0)THEN
	  WRITE(6,50) PARFILE(1:NBLNK(PARFILE))
50	  FORMAT(/,' Cannot open parameter value file ',A,'.')
	  GO TO 9900
	END IF
	OPEN(UNIT=11,FILE=PESTFILE1,STATUS='OLD',IOSTAT=IERR)
	IF(IERR.NE.0) THEN
	  WRITE(6,60) PESTFILE1(1:NBLNK(PESTFILE1))
60	  FORMAT(/,' Cannot open PEST control file ',A,'.')
	  GO TO 9900
	END IF
	OPEN(UNIT=20,FILE=PESTFILE2)

C -- THE PARAMETER VALUE FILE IS READ A FIRST TIME TO ESTABLISH NUMBER OF
C    PARAMETERS.

	WRITE(6,120) PARFILE(1:NBLNK(PARFILE))
120	FORMAT(' Reading parameter value file ',A,' ----->')
	ILINE=0
130     CONTINUE
	  READ(10,'(A)',ERR=9100,END=150) CLINE
	  IF(CLINE.EQ.' ') GO TO 130
	  CALL LINSPL(IFAIL,4,LW,RW,CLINE)
	  IF(IFAIL.NE.0) GO TO 130
	  ILINE=ILINE+1
        GO TO 130
150	CONTINUE
        IF(ILINE.EQ.0)THEN
          WRITE(6,151)
151       FORMAT(/,' Parameter value file appears to hold no ',
     +    'parameters.')
          WRITE(6,152)
152       FORMAT(' Note: a line with 4 entries is assumed to hold ',
     +    'the value of a parameter.')
          GO TO 9900
        END IF

	ALLOCATE(APAR(ILINE),VALUE(ILINE),SCALE(ILINE),OFFSET(ILINE),
     +  STAT=IERR)
	IF(IERR.NE.0)THEN
	  WRITE(6,170)
170	  FORMAT(/,' Cannot allocate sufficient memory to continue ',
     +    'execution.')
	  GO TO 9900
	END IF

	REWIND(UNIT=10,IOSTAT=IERR)
	IF(IERR.NE.0)THEN
	  WRITE(6,190) PARFILE(1:NBLNK(PARFILE))
190	  FORMAT(' Cannot rewind parameter value file ',A,'.')
	  GO TO 9900
	END IF

C -- THE PARAMETER VALUE FILE IS NOW PERUSED A SECOND TIME AND PARAMETER
C    VALUES ARE READ

	ILINE=0
200	ILINE=ILINE+1
	READ(10,'(A)',ERR=9100,END=400) CLINE
	CALL LINSPL(IFAIL,3,LW,RW,CLINE)
	IF(IFAIL.LT.0) GO TO 200
	IF(IFAIL.EQ.0) THEN
	  WRITE(6,210) PARFILE(1:NBLNK(PARFILE))
210	  FORMAT(/,' Two entries expected on first line of ',
     +    'file ',A,'.')
	  GO TO 9900
	END IF
	CALL LINSPL(IFAIL,2,LW,RW,CLINE)
	IF(IFAIL.NE.0)THEN
	  WRITE(6,210) PARFILE(1:NBLNK(PARFILE))
	  GO TO 9900
	END IF
	CALL LOWCAS(CLINE)
	ATEMP=CLINE(LW(1):RW(1))
	IF((ATEMP(1:6).NE.'single').AND.(ATEMP(1:6).NE.'double'))THEN
	  WRITE(6,220) PARFILE(1:NBLNK(PARFILE))
220	  FORMAT(/,' First entry in parameter value file ',A,
     +    ' must be "single" or "double".')
	  GO TO 9900
	END IF
	ATEMP=CLINE(LW(2):RW(2))
	IF((ATEMP(1:5).NE.'point').AND.(ATEMP(1:7).NE.'nopoint'))THEN
	  WRITE(6,230) PARFILE(1:NBLNK(PARFILE))
230	  FORMAT(/,' Second entry of parameter value file ',A,
     +    ' must be "point" or "nopoint".')
	  GO TO 9900
	END IF

	LT4=0
	NUMPAR=0
250	ILINE=ILINE+1
	READ(10,'(A)',ERR=9100,END=400) CLINE
	CALL LINSPL(IFAIL,4,LW,RW,CLINE)
	IF(IFAIL.LT.0) GO TO 250
	IF(IFAIL.NE.0)THEN
	  LT4=LT4+1
	  GO TO 250
!	  CALL WRTINT(ALINE,ILINE)
!	  WRITE(6,255) ALINE(1:NBLNK(ALINE)),PARFILE(1:NBLNK(PARFILE))
!255	  FORMAT(/,' Insufficient entries at line ',A,' of ',
!     +    'file ',A,'.')
!	  GO TO 9900
	END IF
	CALL LOWCAS(CLINE)
	IF(RW(1)-LW(1).GT.11)THEN
	  CALL WRTINT(ALINE,ILINE)
	  WRITE(6,260) ALINE(1:NBLNK(ALINE)),
     +    PARFILE(1:NBLNK(PARFILE))
260	  FORMAT(/,' Parameter name greater than 12 characters',
     +    ' at line ',A,' of file ',A,'.')
	  GO TO 9900
	END IF
	NUMPAR=NUMPAR+1
	APAR(NUMPAR)=CLINE(LW(1):RW(1))
	CALL DREALRD(IFAIL,CLINE(LW(2):RW(2)),VALUE(NUMPAR))
	IF(IFAIL.EQ.0) CALL DREALRD(IFAIL,CLINE(LW(3):RW(3)),
     +  SCALE(NUMPAR))
	IF(IFAIL.EQ.0) CALL DREALRD(IFAIL,CLINE(LW(4):RW(4)),
     +  OFFSET(NUMPAR))
	IF(IFAIL.NE.0)THEN
	  CALL WRTINT(ALINE,ILINE)
	  WRITE(6,280) ALINE(1:NBLNK(ALINE)),PARFILE(1:NBLNK(PARFILE))
280	  FORMAT(/,' Input data error',
     +    ' at line ',A,' of file ',A,'.')
	  GO TO 9900
	END IF
	GO TO 250

400	CLOSE(UNIT=10)
	CALL WRTINT(ALINE,NUMPAR)
	WRITE(6,420) ALINE(1:NBLNK(ALINE)), PARFILE(1:NBLNK(PARFILE))
420	FORMAT(' Data for ',A,' parameters read from file ',A,'.')
        IF(LT4.GT.0)THEN
          WRITE(6,421)
421       FORMAT(' Warning: some lines ignored as they have less than ',
     +    'four entries.')
        END IF
	WRITE(6,450) PESTFILE1(1:NBLNK(PESTFILE1)),
     +  PESTFILE2(1:NBLNK(PESTFILE2))
450	FORMAT(/,' Reading file ',A,' and writing file ',A,' ----->')

451     CONTINUE
	IIPAR=0
	ILINE=1
	READ(11,'(A)',END=9200,ERR=9400) CLINE
	CALL LOWCAS(CLINE)
	CALL SHIFTL(CLINE)
	IF(CLINE(1:4).NE.'pcf ')THEN
	  WRITE(6,460) PESTFILE1(1:NBLNK(PESTFILE1))
460	  FORMAT(/,' First line of file ',A,' should be "pcf".')
	  GO TO 9900
	END IF
        IF(IPASS.EQ.2)THEN
          WRITE(20,'(A)',ERR=9300) TRIM(CLINE)
        END IF
        DO I=1,2
465       CONTINUE
          ILINE=ILINE+1
          READ(11,'(A)',ERR=9400,END=9200) CLINE
          IF(IPASS.EQ.2) WRITE(20,'(A)',ERR=9300) TRIM(CLINE)
          IF(SKIPLINE(CLINE)) GO TO 465
        END DO
        IF(IPASS.EQ.1)THEN
468       CONTINUE
          ILINE=ILINE+1
          READ(11,'(A)',ERR=9400,END=9200) CLINE
          IF(SKIPLINE(CLINE)) GO TO 468
          CALL LINSPL(IFAIL,5,LW,RW,CLINE)
          IF(IFAIL.NE.0) GO TO 9400
          CALL INTREAD(IFAIL,CLINE(LW(1):RW(1)),NPAR)
          IF(IFAIL.NE.0) GO TO 9400
          CALL INTREAD(IFAIL,CLINE(LW(2):RW(2)),NOBS)
          IF(IFAIL.NE.0) GO TO 9400
          CALL INTREAD(IFAIL,CLINE(LW(3):RW(3)),NPARGP)
          IF(IFAIL.NE.0) GO TO 9400
          CALL INTREAD(IFAIL,CLINE(LW(4):RW(4)),NPRIOR)
          IF(IFAIL.NE.0) GO TO 9400
          CALL INTREAD(IFAIL,CLINE(LW(5):RW(5)),NOBSGP)
          IF(IFAIL.NE.0) GO TO 9400
          IF(NPAR.LE.0) GO TO 9400
          ALLOCATE(PVAL(NPAR),NEWPVAL(NPAR),ITRANS(NPAR),AAPAR(NPAR),
     +    STAT=IERR)
          IF(IERR.NE.0) THEN
            WRITE(6,170)
            GO TO 9900
          END IF
          ITRANS=0
          PVAL=-1.1D37
          NEWPVAL=0.0D0
        ELSE
469       CONTINUE
          ILINE=ILINE+1
          READ(11,'(A)',ERR=9400,END=9200) CLINE
          WRITE(20,'(A)',ERR=9300) CLINE(1:NBLNK(CLINE))
          IF(SKIPLINE(CLINE)) GO TO 469
        END IF
        LINE_COUNT_LOC=0
470     CONTINUE
	  ILINE=ILINE+1
	  READ(11,'(A)',ERR=9400,END=9200) CLINE
	  IF(CLINE(1:1).EQ.'*') THEN
	    CALL LOWCAS(CLINE)
	    IF(INDEX(CLINE,'parameter').NE.0) THEN
	      IF(INDEX(CLINE,'data').NE.0)THEN
	        IIPAR=1
	      END IF
	    END IF
	  END IF
	  IF(IPASS.EQ.2)THEN
	    IF(NEW_NOPTMAX.EQ.-99999999) THEN
              WRITE(20,'(A)',ERR=9300) CLINE(1:NBLNK(CLINE))
            ELSE
              DLINE=CLINE
              IF(.NOT.SKIPLINE(CLINE))LINE_COUNT_LOC=LINE_COUNT_LOC+1
              IF(LINE_COUNT_LOC.EQ.5)THEN
                CALL LINSPL(IFAIL,1,LW,RW,DLINE)
                CALL WRITINT(ATEMP,NEW_NOPTMAX)
                DLINE=TRIM(ATEMP)//TRIM(DLINE(RW(1)+1:))
                NEW_NOPTMAX=-99999999
              END IF
              WRITE(20,'(A)',ERR=9300) TRIM(DLINE)
            END IF
          END IF
	  IF(IIPAR.EQ.1) GO TO 500
        GO TO 470

C -- THE "* PARAMETER DATA" OF THE PEST CONTROL FILE IS READ AND SUBSTITUTIONS
C    MADE.

500     CONTINUE
        DO IPAR=1,NPAR
501       CONTINUE
          ILINE=ILINE+1
          READ(11,'(A)',ERR=9400,END=9500) CLINE
          CALL LOWCAS(CLINE)
          IF(CLINE(1:1).EQ.'*') GO TO 9400
          DLINE=CLINE
          IF(SKIPLINE(CLINE))THEN
            IF(IPASS.EQ.2)WRITE(20,'(A)') TRIM(DLINE)
            GO TO 501
          END IF
          NEQQ=INDEX(CLINE,'=')
          IF(NEQQ.GT.1)THEN
            IF(IPASS.EQ.2) WRITE(20,'(A)',ERR=9300) TRIM(DLINE)
            GO TO 501
          END IF
          CALL LINSPL(IFAIL,2,LW,RW,CLINE)
          IF(IFAIL.NE.0) GO TO 9400
          IF(CLINE(LW(2):RW(2)).EQ.'file_parameter')THEN
            IF(IPASS.EQ.2) WRITE(20,'(A)',ERR=9300) TRIM(DLINE)
            GO TO 501
          END IF
          DLINE=ADJUSTL(DLINE)
          CALL LINSPL(IFAIL,9,LW,RW,CLINE)
	  IF(IFAIL.NE.0) GO TO 9400
          IF(RW(1)-LW(1).GT.11)THEN
	    CALL WRTINT(ALINE,ILINE)
	    WRITE(6,550) ALINE(1:NBLNK(ALINE)),
     +      PESTFILE1(1:NBLNK(PESTFILE1))
550	    FORMAT(/,' Parameter name greater than 12 characters at ',
     +      'line ',A,' of file ',A,'.')
	    GO TO 9900
	  END IF
	  AAPAR(IPAR)=CLINE(LW(1):RW(1))
          DO I=1,NUMPAR
	    IF(AAPAR(IPAR).EQ.APAR(I)) GO TO 570
          END DO
          ATRANS=CLINE(LW(2):RW(2))
          CALL LOWCAS(ATRANS)
          IF(ATRANS.EQ.'tied')THEN
            IF(IPASS.EQ.1)THEN
              CALL DREALRD(IFAIL,CLINE(LW(4):RW(4)),PVAL(IPAR))
              IF(IFAIL.NE.0) THEN
                CALL WRTINT(ALINE,ILINE)
                WRITE(6,600) TRIM(ALINE),TRIM(PESTFILE1)
                GO TO 9900
              END IF
              ITRANS(IPAR)=-1
              INEEDED=INEEDED+1
            ELSE IF(IPASS.EQ.2)THEN
              WRITE(20,630,ERR=9300) CLINE(LW(1):RW(3)),NEWPVAL(IPAR),
     +        DLINE(LW(5):NBLNK(DLINE))
            END IF
            CYCLE
          ELSE IF(ATRANS.EQ.'fixed')THEN
            IF(IPASS.EQ.2)THEN
              WRITE(20,'(A)',ERR=9300) TRIM(DLINE)
            END IF
            CYCLE
          ELSE
            CALL WRTINT(ALINE,ILINE)
            WRITE(6,560) ALINE(1:NBLNK(ALINE)),
     +      PESTFILE1(1:NBLNK(PESTFILE1)),TRIM(AAPAR(IPAR)),
     +      PARFILE(1:NBLNK(PARFILE))
560	    FORMAT(/,' Line ',A,' of file ',A,':-',/,
     +  '   parameter "',A,'" not cited in parameter file ',A,'.')
	    GO TO 9900
          END IF
570	  CALL DREALRD(IFAIL,CLINE(LW(4):RW(4)),RVAL)
	  IF(IFAIL.EQ.0)
     +    CALL DREALRD(IFAIL,CLINE(LW(8):RW(8)),RSCALE)
	  IF(IFAIL.EQ.0)
     +    CALL DREALRD(IFAIL,CLINE(LW(9):RW(9)),ROFFSET)
	  IF(IFAIL.NE.0) THEN
	    CALL WRTINT(ALINE,ILINE)
	    WRITE(6,600) ALINE(1:NBLNK(ALINE)),
     +      PESTFILE1(1:NBLNK(PESTFILE1))
600	    FORMAT(/,' Data input error ',
     +      'at line ',A,' of file ',A,'.')
	    GO TO 9900
	  END IF
          IF((ABS(SCALE(I)-RSCALE).GT.1.0D-6*ABS(SCALE(I))).OR.
     +      (ABS(OFFSET(I)-ROFFSET).GT.1.0D-6*ABS(OFFSET(I))))
     +      IIWARN=1
          IF(IPASS.EQ.1)THEN
            PVAL(IPAR)=RVAL
            NEWPVAL(IPAR)=VALUE(I)
          ELSE IF(IPASS.EQ.2)THEN
            WRITE(20,630,ERR=9300) CLINE(LW(1):RW(3)),VALUE(I),
     +      DLINE(LW(5):NBLNK(DLINE))
630         FORMAT(A,2X,1PG14.7,2X,A)
          END IF
	END DO

C -- IF REQUIRED, THE NATURE OF TIED PARAMETERS IS ASCERTAINED.

        IF(IPASS.EQ.1)THEN
          IF(INEEDED.NE.0)THEN
            DO
631           CONTINUE
              ILINE=ILINE+1
              READ(11,'(A)',ERR=9400,END=9400) CLINE
              CLINE=ADJUSTL(CLINE)
              IF(CLINE(1:1).EQ.'*') GO TO 640
              IF(SKIPLINE(CLINE)) GO TO 631
              NEQQ=INDEX(CLINE,'=')
              IF(NEQQ.GT.1) GO TO 631
              CALL LINSPL(IFAIL,2,LW,RW,CLINE)
              IF(IFAIL.NE.0) THEN
                CALL WRTINT(ALINE,ILINE)
                WRITE(6,600) TRIM(ALINE),TRIM(PESTFILE1)
                GO TO 9900
              END IF
              IF(CLINE(LW(2):RW(2)).EQ.'file_parameter')GO TO 631
              AAPAR1=CLINE(LW(1):RW(1))
              CALL LOWCAS(AAPAR1)
              AAPAR2=CLINE(LW(2):RW(2))
              CALL LOWCAS(AAPAR2)
              IF(AAPAR1.EQ.AAPAR2) THEN
                CALL WRTINT(ALINE,ILINE)
                WRITE(6,600) TRIM(ALINE),TRIM(PESTFILE1)
                GO TO 9900
              END IF
              DO IPAR=1,NPAR
                IF(AAPAR1.EQ.AAPAR(IPAR))THEN
                  IF(ITRANS(IPAR).EQ.-1)THEN
                    DO JPAR=1,NPAR
                      IF(AAPAR2.EQ.AAPAR(JPAR))THEN
                        IF(ITRANS(JPAR).LT.0) THEN
                          CALL WRTINT(ALINE,ILINE)
                          WRITE(6,600) TRIM(ALINE),TRIM(PESTFILE1)
                          GO TO 9900
                        END IF
                        IF(PVAL(JPAR).LT.-1.0D37)THEN
                          WRITE(6,639) TRIM(PESTFILE1)
639                       FORMAT(' File ',a,' is in error - check ',
     +                    'it with PESTCHEK.')
                          GO TO 9900
                        END IF
                        NEWPVAL(IPAR)=
     +                  NEWPVAL(JPAR)/PVAL(JPAR)*PVAL(IPAR)
                        ITRANS(IPAR)=-2
                        GO TO 641
                      END IF
                    END DO
                    CALL WRTINT(ALINE,ILINE)
                    WRITE(6,638) TRIM(ALINE),TRIM(PESTFILE1)
                    GO TO 9900
                  ELSE
                    GO TO 641
                  END IF
                END IF
              END DO
              CALL WRTINT(ALINE,ILINE)
              WRITE(6,638) TRIM(ALINE),TRIM(PESTFILE1)
638           FORMAT(' Unknown parameter at line ',a,' of file ',a,'.')
              GO TO 9900
641           CONTINUE
            END DO
640         DO IPAR=1,NPAR
              IF(ITRANS(IPAR).EQ.-1)THEN
                WRITE(6,660) TRIM(AAPAR(IPAR)),TRIM(PESTFILE1)
660             FORMAT(' No parent for parameter "',a,'" in file ',
     +          a,'.')
                GO TO 9900
              END IF
            END DO
          END IF
          IPASS=2
          ILINE=0
          REWIND(UNIT=11)
          GO TO 451
        END IF


C -- THE REMAINDER OF THE CONTROL DATA IS TRANSFERRED FROM ONE FILE TO THE
C    OTHER.

700	CONTINUE
720	ILINE=ILINE+1
	READ(11,'(A)',ERR=9400,END=800) CLINE
	WRITE(20,'(A)',ERR=9300) CLINE(1:NBLNK(CLINE))
	GO TO 720

800	WRITE(6,810) PESTFILE2(1:NBLNK(PESTFILE2))
810	FORMAT(' File ',A,' written ok.')
	IOK=1

        IF(IIWARN.NE.0)THEN
          WRITE(6,*)
          WRITE(6,820)
820       FORMAT(' Warning: at least one parameter scale and offset ',
     +    'recorded in parameter value',/,' file differs from that ',
     +    'recorded for same parameter in PEST control file.')
        END IF

	GO TO 9990

9000	WRITE(6,9010)
9010	FORMAT(' Parrep is run using the command:',/)
	WRITE(6,9020)
9020	FORMAT('     PARREP parfile pestfile1 pestfile2 [noptmax]',/)
	WRITE(6,9030)
9030	FORMAT(' where',/)
	WRITE(6,9040)
9040	FORMAT('     parfile    is the name of a parameter ',
     +  'value file,')
	WRITE(6,9050)
9050	FORMAT('     pestfile1  is the name of the input PEST ',
     +  'control file,')
	WRITE(6,9060)
9060	FORMAT('     pestfile2  is the name of the output PEST ',
     +  'control file, and')
        WRITE(6,9061)
9061    FORMAT('     noptmax    is an optional value for NOPTMAX ',
     +  'in new PEST control file.')
	GO TO 9999

9100	CALL WRTINT(ALINE,ILINE)
	WRITE(6,9110) ALINE(1:NBLNK(ALINE)),PARFILE(1:NBLNK(PARFILE))
9110	FORMAT(' Error reading line ',A,
     +  ' of parameter value file ',A,'.')
	GO TO 9900
9200	WRITE(6,9210) PESTFILE1(1:NBLNK(PESTFILE1))
9210	FORMAT(' Unexpected end to file ',A,
     +  '; can''t find "* parameter data" section.')
	GO TO 9900
9300	WRITE(6,9310) PESTFILE2(1:NBLNK(PESTFILE2))
9310	FORMAT(' Error encountered while writing to file ',A,'.')
	GO TO 9900
9400	WRITE(6,9410) PESTFILE1(1:NBLNK(PESTFILE1))
9410	FORMAT(' Error reading file ',A,' - check it with PESTCHEK.')
	GO TO 9900
9500	WRITE(6,9510) PESTFILE1(1:NBLNK(PESTFILE1))
9510	FORMAT(' Unexpected end encountered to file ',A,'.')
	GO TO 9900



9900	CONTINUE
9995	WRITE(6,9996)
9996	FORMAT(/,' Execution terminated.')
	GO TO 9990



9990	CONTINUE
	DEALLOCATE(APAR,VALUE,SCALE,OFFSET,STAT=IERR)
        DEALLOCATE(PVAL,NEWPVAL,ITRANS,AAPAR,STAT=IERR)

	CLOSE(UNIT=10,IOSTAT=IERR)
	CLOSE(UNIT=11,IOSTAT=IERR)
        IF(IOK.EQ.0)THEN
          INQUIRE(UNIT=20,OPENED=LOPENED)
          IF(LOPENED) THEN
            CLOSE(UNIT=20,STATUS='DELETE',IOSTAT=IERR)
          ENDIF
        ELSE
          CLOSE(UNIT=20,IOSTAT=IERR)
        END IF

9999	END


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
        CHARACTER*8 AFMT

        AFMT='(I   )'
        WRITE(AFMT(3:5),'(I3)') LEN(ATEMP)
        WRITE(ATEMP,AFMT)IVAL
        CALL SHIFTL(ATEMP)
        RETURN
        END


        SUBROUTINE DREALRD(IFAIL,CLINE,RTEMP)

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
        END


        subroutine intread(ifail,cline,itemp)

C -- Subroutine INTREAD reads a real number from a string.

        integer ifail
        integer itemp
        character*6 afmt
        character*(*) cline

        ifail=0
        afmt='(i   )'
        write(afmt(3:5),'(i3)') len(cline)
        read(cline,afmt,err=100) itemp
        return

100     ifail=1
        return
        end


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



	subroutine writint(atemp,ival)

c	Subroutine WRITINT writes an integer to a character variable.

	integer*4 ival
	character*6 afmt
	character*(*) atemp

	afmt='(i   )'
	write(afmt(3:5),'(i3)') len(atemp)
	write(atemp,afmt)ival
	atemp=adjustl(atemp)
	return
	end



