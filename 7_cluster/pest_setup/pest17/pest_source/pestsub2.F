        SUBROUTINE PRRCLC(JFAIL,NPAR,NOBS,NXROW,NESPAR,PVAL,ITRANS,
     +  REFOBS,X,CLINE,SCALE,NREGADJPAR,PRIORPAR)

C -- SUBROUTINE PRRCALC CALCULATES THE CONTRIBUTION OF PRIOR INFORMATION
C -- TO THE JACOBIAN MATRIX

        USE PESTDATA,  ONLY: XDIM1,XDIM2,MAXCOMPDIM,NCOMPDIM,
     +                       XC,IXC,ISTART_C,IFOUND_C,
     +                       IXPR,IXPC,IES2IPAR,PR_INDEX,XPVAL,
     +                       NPR_NONZERO
        IMPLICIT NONE

        INTEGER NPAR,NOBS,NXROW,NESPAR,IROW,IPP,IES,NREGADJPAR,
     +  IFLAG,I,JFAIL
        INTEGER ITRANS(NPAR),PRIORPAR(NREGADJPAR)
        DOUBLE PRECISION RRTEMP,RTEMP,TX
        DOUBLE PRECISION PVAL(NPAR),REFOBS(NXROW),X(XDIM1,XDIM2),
     +  SCALE(NPAR)
        CHARACTER*(*) CLINE

        JFAIL=0
        IF(PR_INDEX.EQ.1)THEN
          DO IROW=NOBS+1,NXROW
            REFOBS(IROW)=0.0D0
          END DO
          DO I=1,NPR_NONZERO
            IROW=IXPR(I)
            IES=IXPC(I)
            IPP=IES2IPAR(IES)
            TX=XPVAL(I)
            IF(TX.EQ.0.0) CYCLE
            IF((ITRANS(IPP).EQ.1).OR.(ITRANS(IPP).EQ.-1999999)) THEN
              IF(PVAL(IPP).LE.0.0D0) THEN
                CALL STPERR(4,0,' ',0,' ',CLINE)
                GO TO 9891
              END IF
              RTEMP=LOG10(PVAL(IPP))
            ELSE IF((ITRANS(IPP).EQ.0).OR.(ITRANS(IPP).EQ.-2000000))THEN
              RTEMP=PVAL(IPP)
            ELSE
              CYCLE
            END IF
            REFOBS(IROW)=REFOBS(IROW)+TX*RTEMP
          END DO
          GO TO 200
        END IF

        ISTART_C=1
        DO 100 IROW=NOBS+1,NXROW
        IFLAG=0
        IF(NREGADJPAR.GT.0)THEN
          DO I=1,NREGADJPAR
            IF(PRIORPAR(I).EQ.IROW)THEN
              IFLAG=1
              GO TO 10
            END IF
          END DO
10        CONTINUE
        END IF
        RRTEMP=0.0
        IES=0
        DO 80 IPP=1,NPAR
        IF((ITRANS(IPP).LT.0).AND.(ITRANS(IPP).GT.-1999999)) GO TO 80
        IES=IES+1
        IF(IFLAG.EQ.0)THEN
          IF(SCALE(IPP).LT.-1.0D35) GO TO 80
        END IF
        IF(MAXCOMPDIM.EQ.1)THEN
          TX=X(IROW,IES)
        ELSE
          CALL GET_VALUE(NCOMPDIM,XC,IXC,TX,IROW,IES)
          ISTART_C=IFOUND_C
        END IF
        IF(TX.EQ.0.0) GO TO 80
        IF((ITRANS(IPP).EQ.1).OR.(ITRANS(IPP).EQ.-1999999)) THEN
          IF(PVAL(IPP).LE.0.0D0) THEN
            CALL STPERR(4,0,' ',0,' ',CLINE)
            GO TO 9891
          END IF
          RTEMP=LOG10(PVAL(IPP))
        ELSE IF((ITRANS(IPP).EQ.0).OR.(ITRANS(IPP).EQ.-2000000))THEN
          RTEMP=PVAL(IPP)
        ELSE
          GO TO 80
        END IF
        RRTEMP=RRTEMP+TX*RTEMP
80      CONTINUE
100     REFOBS(IROW)=RRTEMP

200     CONTINUE
        RETURN

9891    JFAIL=1
        RETURN

#ifdef PESTMOD
        END SUBROUTINE PRRCLC
#else
        END
#endif


        SUBROUTINE OBJCLC(ITYPE,SUM,NXROW,NPRIOR,NOBS,MODEL,OBSERV,
     +  WEIGHT,NOBGNM,NOBSGP,IRGP,SUM1)

C -- SUBROUTINE OBJCLC CALCULATES THE CURRENT VALUE OF THE OBJECTIVE
C -- FUNCTION AS WELL AS OTHER QUANTITIES WHOSE CALCULATION IS SIMILAR

        IMPLICIT NONE

        INTEGER ITYPE,NPRIOR,NOBS,ITEMP,ICOUNT,NOBSGP
        INTEGER NXROW,I,II
        INTEGER NOBGNM(NXROW),IRGP(NOBSGP)
        DOUBLE PRECISION SUM,RTEMP,SUM1
        DOUBLE PRECISION MODEL(NXROW),OBSERV(NXROW),WEIGHT(NXROW)

        SUM=0.0
        SUM1=0.0
        IF(ITYPE.EQ.1) THEN
          DO 100 I=1,NXROW
          RTEMP=MODEL(I)-OBSERV(I)
          IF(ABS(RTEMP).GE.1.0D150) GO TO 500
100       SUM=SUM+WEIGHT(I)*RTEMP*RTEMP
        ELSE IF(ITYPE.EQ.2) THEN
          DO 200 I=1,NXROW
          IF(ABS(MODEL(I)).GE.1.0D150) GO TO 500
200       SUM=SUM+WEIGHT(I)*MODEL(I)*MODEL(I)
        ELSE IF (ITYPE.EQ.3) THEN
          DO 300 I=1,NXROW
          IF(ABS(MODEL(I)).GE.1.0D150) GO TO 500
300       SUM=SUM+MODEL(I)*MODEL(I)
        ELSE IF (ITYPE.EQ.4)THEN
          IF(NPRIOR.EQ.0) THEN
            SUM=0.0D0
          ELSE
            DO I=NOBS+1,NXROW
              RTEMP=MODEL(I)-OBSERV(I)
              IF(ABS(RTEMP).GE.1.0D150) GO TO 500
              SUM=SUM+WEIGHT(I)*RTEMP*RTEMP
            END DO
          END IF
        ELSE IF(ITYPE.EQ.-999)THEN
          ICOUNT=0
          DO 400 I=NOBS+1,NXROW
            IF(NOBGNM(I).EQ.-1)THEN
              ICOUNT=ICOUNT+1
              RTEMP=MODEL(I)-OBSERV(I)
              IF(ABS(RTEMP).GE.1.0D150) GO TO 500
              SUM=SUM+WEIGHT(I)*RTEMP*RTEMP
            END IF
400       CONTINUE
          IF(ICOUNT.EQ.0) SUM=-1.0D300
        ELSE IF(ITYPE.EQ.888888)THEN
          DO 750 I=1,NXROW
            RTEMP=MODEL(I)-OBSERV(I)
            IF(ABS(RTEMP).GE.1.0D150) RTEMP=1.0D150
            II=NOBGNM(I)
            IF(IRGP(II).NE.0)THEN
              SUM=SUM+WEIGHT(I)*RTEMP*RTEMP
            ELSE
              SUM1=SUM1+WEIGHT(I)*RTEMP*RTEMP
            END IF
750       CONTINUE
        ELSE IF((ITYPE.LT.0).AND.(ITYPE.GT.-99999)) THEN
          ITEMP=-ITYPE
          DO 450 I=1,NXROW
            IF(ITEMP.EQ.NOBGNM(I))THEN
              RTEMP=MODEL(I)-OBSERV(I)
              IF(ABS(RTEMP).GE.1.0D150) GO TO 500
              SUM=SUM+WEIGHT(I)*RTEMP*RTEMP
            END IF
450       CONTINUE
        ELSE IF(ITYPE.LT.-99999)THEN
          ITEMP=-ITYPE-100000
          DO 550 I=1,NXROW
            IF(ITEMP.NE.NOBGNM(I))THEN
              RTEMP=MODEL(I)-OBSERV(I)
              IF(ABS(RTEMP).GE.1.0D150) GO TO 500
              SUM=SUM+WEIGHT(I)*RTEMP*RTEMP
            END IF
550       CONTINUE
        ELSE IF(ITYPE.GT.99999)THEN
          ITEMP=ITYPE-100000
          DO 600 I=1,NXROW
            IF(ITEMP.EQ.NOBGNM(I))THEN
              RTEMP=MODEL(I)-OBSERV(I)
              IF(ABS(RTEMP).GE.1.0D150) GO TO 500
              SUM=SUM+RTEMP*RTEMP*WEIGHT(I)
            END IF
600       CONTINUE
        END IF
        RETURN
500     SUM=1.0D299
        RETURN

#ifdef PESTMOD
        END SUBROUTINE OBJCLC
#else
        END
#endif



        SUBROUTINE OBJCLC1(SUM,NXROW,MODEL)

        IMPLICIT NONE

        INTEGER NXROW,I
        DOUBLE PRECISION SUM
        DOUBLE PRECISION MODEL(NXROW)

        SUM=0.0

        DO 300 I=1,NXROW
          IF(ABS(MODEL(I)).GE.1.0D150) GO TO 500
          SUM=SUM+MODEL(I)*MODEL(I)
300     CONTINUE
        RETURN
500     SUM=1.0D299
        RETURN

#ifdef PESTMOD
        END SUBROUTINE OBJCLC1
#else
        END
#endif


        SUBROUTINE PRMWRT(JFAIL,UNIT,NPAR,APAR,PVAL,LSTVAL,CLINE)

C -- SUBROUTINE PRMWRT WRITES CURRENT PARAMETER VALUES TO THE PEST RUN
C -- RECORD FILE

        IMPLICIT NONE

        INTEGER NPAR,I,JFAIL
        INTEGER UNIT
        DOUBLE PRECISION PVAL(NPAR),LSTVAL(NPAR)
        CHARACTER*12 APAR(NPAR)
        CHARACTER*(*) CLINE

        JFAIL=0
        IF(LSTVAL(1).LT.1.0D300)THEN
          WRITE(UNIT,5,ERR=100)
5         FORMAT(/,T8,'Current parameter values',
     +    T49,'Previous parameter values')
          DO 40 I=1,NPAR
            WRITE(UNIT,30,ERR=100) APAR(I)(:LEN_TRIM(APAR(I))),PVAL(I),
     +      APAR(I)(:LEN_TRIM(APAR(I))),LSTVAL(I)
40          CONTINUE
        ELSE
          WRITE(UNIT,6,ERR=100)
6         FORMAT(T8,'Current parameter values')
          DO 20 I=1,NPAR
          WRITE(UNIT,30,ERR=100) APAR(I)(:LEN_TRIM(APAR(I))),PVAL(I)
30        FORMAT(T8,A,T23,1PG13.6,T49,A,T65,1PG13.6)
20        CONTINUE
        END IF

#ifdef GMS
          WRITE(6,62) (TRIM(APAR(I)),I=1,NPAR)
62        FORMAT(' Parameter names',1000(1X,A))
          WRITE(6,61,ERR=100) (PVAL(I),I=1,NPAR)
61        FORMAT(' New parameter values',1000(1X,1PG12.5))
#endif

        RETURN

100     CALL STPERR(26,0,' ',0,' ',CLINE)
        JFAIL=1
        RETURN

#ifdef PESTMOD
        END SUBROUTINE PRMWRT
#else
        END
#endif


        SUBROUTINE PSIWRT(JFAIL,UNIT,PSI,FRCPSI,CLINE,FAILUREFLAG)

C -- SUBROUTINE PSIWRT WRITES THE CURRENT VALUE OF THE OBJECTIVE FUNCTION
C -- TO A SPECIFIED UNIT NUMBER

        IMPLICIT NONE

        INTEGER UNIT,JFAIL,FAILUREFLAG
        DOUBLE PRECISION PSI,FRCPSI
        CHARACTER*6 TIMES
        CHARACTER*7 ATEMP7
        CHARACTER*(*) CLINE

        JFAIL=0
        IF(FAILUREFLAG.NE.0)THEN
          WRITE(UNIT,30)
30        FORMAT(T12,'Phi = cannot be calculated: model run failure')
          RETURN
        END IF
        IF(FRCPSI.LT.0.0D0)THEN
          WRITE(UNIT,10) PSI
10        FORMAT(T12,'Phi = ',1PG12.5)
        ELSE
          IF(FRCPSI.GE.1.0D0)THEN
            TIMES=' times'
            IF(FRCPSI.GT.999.0)THEN
              ATEMP7='*******'
            ELSE
              WRITE(ATEMP7,'(F7.3)') FRCPSI
            END IF
          ELSE
            TIMES=' of'
            WRITE(ATEMP7,'(F7.3)') FRCPSI
          END IF
          WRITE(UNIT,20) PSI,TRIM(ATEMP7),TRIM(TIMES)
20        FORMAT(T12,'Phi = ',1PG12.5,
     +      '  (',A,A,' starting phi)')
        END IF
        RETURN

100     CALL STPERR(26,0,' ',0,' ',CLINE)
        JFAIL=1
        RETURN

#ifdef PESTMOD
        END SUBROUTINE PSIWRT
#else
        END
#endif



        SUBROUTINE FFOPEN(JFAIL,N,AHOW,OTHFL,ERRNO,CLINE)

C -- SUBROUTINE FFOPEN OPENS CERTAIN FILES USED BY PEST

#ifdef INTEL
        USE COMMON_MOD, ONLY: FLENME,IPERR,JPERR
#endif
        USE PESTDATA, ONLY: IREC
        IMPLICIT NONE

#ifdef INTEL
        integer JFAIL
#else
        integer iperr,jperr,JFAIL
#endif
        INTEGER N,ERRNO
        CHARACTER*1 AHOW,HOW,ASTAT*9,AACCES*11
#ifdef INTEL
        CHARACTER*200 TMPFL
#else
        CHARACTER*200 FLENME,TMPFL
#endif
#ifdef LAHEY
        CHARACTER CHRCON*8
#endif
        CHARACTER*(*) OTHFL
        CHARACTER*(*) CLINE

#ifndef INTEL
        COMMON /FLENME/FLENME
        common /pperr/ iperr,jperr
#endif

        JFAIL=0
        HOW=AHOW
        iperr=0
5       ASTAT='OLD'
        AACCES='SEQUENTIAL'
#ifdef LAHEY
        CHRCON='LIST'
        IF(N.EQ.IREC) CHRCON='FORTRAN'
#endif
        TMPFL=FLENME(1:MIN(LEN(FLENME),200))
        IF(OTHFL.NE.' ') TMPFL=OTHFL(1:MIN(LEN(OTHFL),200))

! -- If the file is to be written, a previous file of the same name is deleted.

        IF(HOW.EQ.'w')THEN
          CALL DELFILE1(JFAIL,TMPFL,CLINE)
C          IF(JFAIL.NE.0) RETURN
        END IF

        IF(HOW.EQ.'w') THEN
          ASTAT='UNKNOWN'
        ELSE IF(HOW.EQ.'r') THEN
          CONTINUE
        ELSE IF(HOW.EQ.'a')THEN
          AACCES='APPEND'
        END IF
        IF(N.GE.0) THEN
#ifdef LAHEY
#ifdef LF90
          OPEN(UNIT=N,FILE=TMPFL(:LEN_TRIM(TMPFL)),STATUS=ASTAT,
     +    ACCESS=AACCES,CARRIAGECONTROL=CHRCON,
     +    ACTION='READWRITE,DENYNONE',ERR=10)
#else
#ifdef INTEL
          OPEN(UNIT=N,FILE=TMPFL(:LEN_TRIM(TMPFL)),STATUS=ASTAT,
     +    ACCESS=AACCES,CARRIAGECONTROL=CHRCON,SHARED,ERR=10)
#else
          OPEN(UNIT=N,FILE=TMPFL(:LEN_TRIM(TMPFL)),STATUS=ASTAT,
     +    ACCESS=AACCES,CARRIAGECONTROL=CHRCON,ERR=10)
#endif
#endif
#else
#ifdef UNICOS
          IF(HOW.EQ.'a')THEN
            OPEN(UNIT=N,FILE=TMPFL(:LEN_TRIM(TMPFL)),STATUS=ASTAT,
     +      ACCESS='SEQUENTIAL',POSITION='APPEND',RECL=2000,ERR=10)
          ELSE
            OPEN(UNIT=N,FILE=TMPFL(:LEN_TRIM(TMPFL)),STATUS=ASTAT,
     +      ACCESS='SEQUENTIAL',RECL=2000,ERR=10)
          END IF
#else
          IF(HOW.EQ.'a')THEN
            OPEN(UNIT=N,FILE=TMPFL(:LEN_TRIM(TMPFL)),STATUS=ASTAT,
     +      ACCESS='SEQUENTIAL',POSITION='APPEND',ERR=10)
          ELSE
            OPEN(UNIT=N,FILE=TMPFL(:LEN_TRIM(TMPFL)),STATUS=ASTAT,
     +      ACCESS='SEQUENTIAL',ERR=10)
          END IF
#endif
#endif
        ELSE
#ifdef UNIX
          OPEN(UNIT=-N,FILE=TMPFL(:LEN_TRIM(TMPFL)),STATUS=ASTAT,
     +    FORM='UNFORMATTED',ACCESS='STREAM',ERR=10)
#else
#ifdef LAHEY
#ifdef LF90
          OPEN(UNIT=-N,FILE=TMPFL(:LEN_TRIM(TMPFL)),STATUS=ASTAT,
     +    FORM='UNFORMATTED',ACCESS='TRANSPARENT',
     +    ACTION='READWRITE,DENYNONE',ERR=10)
#else
          OPEN(UNIT=-N,FILE=TMPFL(:LEN_TRIM(TMPFL)),STATUS=ASTAT,
     +    FORM='BINARY',ERR=10)
#endif
#else
#ifdef INTEL
          OPEN(UNIT=-N,FILE=TMPFL(:LEN_TRIM(TMPFL)),STATUS=ASTAT,
     +    FORM='UNFORMATTED',ACCESS='STREAM',SHARED,ERR=10)
#else
          OPEN(UNIT=-N,FILE=TMPFL(:LEN_TRIM(TMPFL)),STATUS=ASTAT,
     +    FORM='UNFORMATTED',ERR=10)
#endif
#endif
#endif
        END IF
        RETURN

10      CONTINUE
        if(jperr.eq.1) then
          CALL STPERR(ERRNO,1,TMPFL,0,' ',CLINE)
          iperr=1
          return
        end if
        IF(HOW.EQ.'a') THEN
          HOW='w'
          GO TO 5
        END IF

        CALL STPERR(ERRNO,1,TMPFL,0,' ',CLINE)
9891    JFAIL=1
        RETURN

#ifdef PESTMOD
        END SUBROUTINE FFOPEN
#else
        END
#endif



        SUBROUTINE STPERR(NE,NT,AAA,II,BBB,CLINE)

C -- SUBROUTINE STPERR CARRIES OUT PEST ERROR HANDLING

#ifdef INTEL
        USE COMMON_MOD, ONLY: FLENME
#endif
        IMPLICIT NONE

        INTEGER NE,NT,II,NA
        CHARACTER*(*) AAA,BBB,CLINE
#ifndef INTEL
        CHARACTER*200 FLENME
#endif
        CHARACTER*200 AA(3)

#ifndef INTEL
        COMMON /FLENME/FLENME
#endif

        IF((NT.EQ.0).OR.(NT.EQ.5))THEN
          NA=0
        ELSE IF(NT.EQ.1) THEN
          NA=1
          AA(1)=AAA(1:MIN(LEN(AAA),200))
          IF(AAA.EQ.' ')AA(1)=FLENME
        ELSE IF((NT.EQ.2).OR.(NT.EQ.4))THEN
          NA=2
          AA(2)=BBB(1:MIN(LEN(BBB),200))
          IF(BBB.EQ.' ') AA(2)=FLENME
          WRITE(AA(1),10) II
10        FORMAT(I10)
          CALL SHIFTL(AA(1))
        ELSE IF(NT.EQ.3) THEN
          NA=3
          AA(1)=AAA(1:MIN(LEN(AAA),200))
          AA(3)=BBB(1:MIN(LEN(BBB),200))
          IF(BBB.EQ.' ') AA(3)=FLENME
          WRITE(AA(2),10) II
          CALL SHIFTL(AA(2))
        ELSE IF(NT.EQ.6) THEN
          NA=1
          AA(1)=BBB(1:MIN(LEN(BBB),200))
          IF(BBB.EQ.' ') AA(1)=FLENME
        ELSE IF(NT.EQ.7)THEN
          NA=3
          AA(1)=AAA
          AA(2)=BBB
          AA(3)=FLENME
        END IF

        IF((NT.LT.4).OR.(NT.EQ.7))THEN
          CALL PSTERR(NE,NA,AA,CLINE,' ')
        ELSE
          CALL PSTERR(NE,NA,AA,CLINE,AAA)
        END IF
        RETURN

#ifdef PESTMOD
        END SUBROUTINE STPERR
#else
        END
#endif



        SUBROUTINE PSTERR(NE,NA,AA,CLINE,CCLINE)

C -- SUBROUTINE PSTERR CONTROLS THE WRITING OF A PEST ERROR MESSAGE

#ifdef INTEL
        USE COMMON_MOD, ONLY: FLENME
#endif
        USE PESTDATA, ONLY: ERRMSG,ERRMSG_ADD,IREC,INST,IRSF
        IMPLICIT NONE

        LOGICAL LOPENED
        INTEGER NE,NA,IFAIL
        INTEGER NERR,I
#ifndef INTEL
        CHARACTER*200 FLENME
#endif
        CHARACTER*(*) CLINE,CCLINE
        CHARACTER*(*) AA(3)

#ifndef INTEL
        COMMON /FLENME/FLENME
#endif

        include 'errmsg.inc'

13      I = 0
60      I=I+1
        IF(I.GT.MAXERR) GO TO 9600
        IF(ERRNO(I).EQ.NE) GO TO 61
        GO TO 60

61      CLINE = ERRFMT(I)
300     WRITE(ERRMSG,CLINE,ERR=9500) (AA(I)(:LEN_TRIM(AA(I))),I=1,NA)
        ERRMSG=ADJUSTL(ERRMSG)
        IF(CCLINE.NE.' ') THEN
          WRITE(ERRMSG_ADD,301) '"',ccline(:LEN_TRIM(ccline)),'"'
301       FORMAT(3A)
        END IF
310     GO TO 9900


9500    WRITE(ERRMSG,9510) NE
9510    FORMAT('Error writing error message',I4,
     +  ': contact programmer.')
        GO TO 9900

9600    WRITE(ERRMSG,9610) NE
9610    FORMAT('Pertinent error message not in error message list; ',
     +  'please contact programmer. Error number =',I4)
        GO TO 9900

9900    CONTINUE

        IF((NE.EQ.10).OR.(NE.EQ.26)) THEN
          DO 9915 I=1,10
9915      BACKSPACE(UNIT=IREC,ERR=9921)
9921      DO 9916 I=1,10
9916      BACKSPACE(UNIT=INST,ERR=9922)
9922      DO 9917 I=1,10
9917      BACKSPACE(UNIT=IRSF,ERR=9920)
        END IF
9920    CONTINUE
        RETURN

#ifdef PESTMOD
        END SUBROUTINE PSTERR
#else
        END
#endif


        SUBROUTINE PRMSAV(JFAIL,NPAR,APAR,VAL,CLINE,APRCIS,APOINT,
     +  SCALE,OFFSET,ITERATION,RUN_PACKET_INDEX,IRUN)

#ifdef INTEL
        USE COMMON_MOD, ONLY: CASEFL,IPERR,JPERR
#endif
        USE PESTDATA, ONLY : IPFL
        IMPLICIT NONE

C -- SUBROUTINE PRMSAV SAVES CURRENT PARAMETER VALUES TO A PEST PARAMETER
C -- VALUE FILE

        INTEGER NPAR,I,JFAIL,ITERATION
        INTEGER RUN_PACKET_INDEX,IRUN
#ifndef INTEL
        INTEGER IPERR,JPERR
#endif
        DOUBLE PRECISION DSCALE,DOFFSET
        DOUBLE PRECISION VAL(NPAR),SCALE(NPAR),OFFSET(NPAR)
        CHARACTER*10 ANUM
        CHARACTER*10 ATEMP1,ATEMP2
#ifndef INTEL
        CHARACTER*100 CASEFL
#endif
        CHARACTER*200 AFILE
        CHARACTER*(*) CLINE,APRCIS,APOINT
        CHARACTER*(*) APAR(NPAR)

#ifndef INTEL
        COMMON /CASE/CASEFL
        common /pperr/ iperr,jperr
#endif

        JFAIL=0
        IF(RUN_PACKET_INDEX.LE.0)THEN
        IF(ITERATION.LT.0)THEN
#ifdef CAPFILE
          AFILE=TRIM(CASEFL)//'.PAR'
#else
          AFILE=TRIM(CASEFL)//'.par'
#endif
        ELSE
          CALL WRITINT(ANUM,ITERATION)
#ifdef CAPFILE
          AFILE=TRIM(CASEFL)//'.PAR'//'.'//TRIM(ANUM)
#else
          AFILE=TRIM(CASEFL)//'.par'//'.'//TRIM(ANUM)
#endif
        END IF
        ELSE
          AFILE=TRIM(CASEFL)//'.par'
          CALL WRITINT(ATEMP1,RUN_PACKET_INDEX)
          CALL WRITINT(ATEMP2,IRUN)
          AFILE=TRIM(AFILE)//'.'//TRIM(ATEMP1)//'_'//TRIM(ATEMP2)
        END IF

        JPERR=1
        CALL FFOPEN(JFAIL,IPFL,'w',AFILE,6,CLINE)
        IF(JFAIL.NE.0) GO TO 9891
        JPERR=0
        WRITE(IPFL,9,ERR=100) APRCIS(:LEN_TRIM(APRCIS)),
     +  APOINT(:LEN_TRIM(APOINT))
9       FORMAT(2X,A,1X,A)
        DO 20 I=1,NPAR
          DSCALE=SCALE(I)
          DOFFSET=OFFSET(I)
          IF(DSCALE.LT.-1.0D35)THEN
            DSCALE=1.0
            DOFFSET=0.0D0
          END IF
          WRITE(IPFL,10,ERR=100)
     +    APAR(I)(:LEN_TRIM(APAR(I))),VAL(I),DSCALE,DOFFSET
10        FORMAT(2X,A12,3X,1PG23.16,3X,1PG14.7,3X,1PG14.7)
20      CONTINUE
        CLOSE(UNIT=IPFL)
        RETURN

100     CALL STPERR(10,1,AFILE,0,' ',CLINE)
        GO TO 9891

9891    JFAIL=1
        RETURN

#ifdef PESTMOD
        END SUBROUTINE PRMSAV
#else
        END
#endif



        SUBROUTINE PRMOBSSAV(JFAIL,NPAR,APAR,VAL,CLINE,APRCIS,APOINT,
     +  SCALE,OFFSET,NOBS,AOBS,REFOBS,ITERATION)

#ifdef INTEL
        USE COMMON_MOD, ONLY: CASEFL,IPERR,JPERR
#endif
        USE PESTDATA, ONLY : IPFL
        IMPLICIT NONE

C -- SUBROUTINE PRMOBSSAV SAVES CURRENT PARAMETER AND OBSERVATION VALUES TO A PEST PARAMETER
C -- VALUE FILE

        INTEGER NPAR,I,JFAIL
        INTEGER NOBS,ITERATION
#ifndef INTEL
        INTEGER IPERR,JPERR
#endif
        DOUBLE PRECISION DSCALE,DOFFSET
        DOUBLE PRECISION VAL(NPAR),SCALE(NPAR),OFFSET(NPAR)
        DOUBLE PRECISION REFOBS(NOBS)
        CHARACTER*10 ANUM
        CHARACTER*10 ATEMP1,ATEMP2
#ifndef INTEL
        CHARACTER*100 CASEFL
#endif
        CHARACTER*200 AFILE
        CHARACTER*(*) CLINE,APRCIS,APOINT
        CHARACTER*(*) APAR(NPAR)
        CHARACTER*(*) AOBS(NOBS)

#ifndef INTEL
        COMMON /CASE/CASEFL
        common /pperr/ iperr,jperr
#endif

        JFAIL=0
        IF(ITERATION.LT.0)THEN
#ifdef CAPFILE
          AFILE=TRIM(CASEFL)//'.PAROBS'
#else
          AFILE=TRIM(CASEFL)//'.parobs'
#endif
        ELSE
          CALL WRITINT(ANUM,ITERATION)
#ifdef CAPFILE
          AFILE=TRIM(CASEFL)//'.PAROBS'//'.'//TRIM(ANUM)
#else
          AFILE=TRIM(CASEFL)//'.parobs'//'.'//TRIM(ANUM)
#endif
        END IF

        JPERR=1
        CALL FFOPEN(JFAIL,IPFL,'w',AFILE,6,CLINE)
        IF(JFAIL.NE.0) GO TO 9891
        JPERR=0
        WRITE(IPFL,9,ERR=100) APRCIS(:LEN_TRIM(APRCIS)),
     +  APOINT(:LEN_TRIM(APOINT))
9       FORMAT(2X,A,1X,A)
        DO 20 I=1,NPAR
          DSCALE=SCALE(I)
          DOFFSET=OFFSET(I)
          IF(DSCALE.LT.-1.0D35)THEN
            DSCALE=1.0
            DOFFSET=0.0D0
          END IF
          WRITE(IPFL,10,ERR=100)
     +    APAR(I)(:LEN_TRIM(APAR(I))),VAL(I),DSCALE,DOFFSET
10        FORMAT(2X,A12,3X,1PG23.16,3X,1PG14.7,3X,1PG14.7)
20      CONTINUE
        DO I=1,NOBS
          WRITE(IPFL,30,ERR=100) TRIM(AOBS(I)),REFOBS(I)
30        FORMAT(2X,A20,3X,1PG23.16)
        END DO
        CLOSE(UNIT=IPFL)
        RETURN

100     CALL STPERR(10,1,AFILE,0,' ',CLINE)
        GO TO 9891

9891    JFAIL=1
        RETURN

#ifdef PESTMOD
        END SUBROUTINE PRMOBSSAV
#else
        END
#endif


        SUBROUTINE ERRPRMSAV(JFAIL,NPAR,APAR,VAL,CLINE,APRCIS,APOINT,
     +  SCALE,OFFSET,ERRORCOUNT,ERRORFILE)

#ifdef INTEL
        USE COMMON_MOD, ONLY: CASEFL,IPERR,JPERR
#endif
        USE PESTDATA, ONLY : IPFL
        IMPLICIT NONE

C -- SUBROUTINE ERRPRMSAV SAVES PARAMETER VALUES WHICH CAUSED AN ERROR TO A PEST PARAMETER
C -- VALUE FILE

        INTEGER NPAR,I,JFAIL
        INTEGER ERRORCOUNT
#ifndef INTEL
        INTEGER IPERR,JPERR
#endif
        DOUBLE PRECISION DSCALE,DOFFSET
        DOUBLE PRECISION VAL(NPAR),SCALE(NPAR),OFFSET(NPAR)
        CHARACTER*10 ATEMP1
#ifndef INTEL
        CHARACTER*100 CASEFL
#endif
        CHARACTER*(*) ERRORFILE
        CHARACTER*200 AFILE
        CHARACTER*(*) CLINE,APRCIS,APOINT
        CHARACTER*(*) APAR(NPAR)

#ifndef INTEL
        COMMON /CASE/CASEFL
        common /pperr/ iperr,jperr
#endif

        JFAIL=0

        ERRORCOUNT=ERRORCOUNT+1
        CALL WRITINT(ATEMP1,ERRORCOUNT)
        AFILE=TRIM(ERRORFILE)//'.'//TRIM(ATEMP1)

        JPERR=1
        CALL FFOPEN(JFAIL,IPFL,'w',AFILE,6,CLINE)
        IF(JFAIL.NE.0) GO TO 9891
        JPERR=0
        WRITE(IPFL,9,ERR=100) APRCIS(:LEN_TRIM(APRCIS)),
     +  APOINT(:LEN_TRIM(APOINT))
9       FORMAT(2X,A,1X,A)
        DO 20 I=1,NPAR
          DSCALE=SCALE(I)
          DOFFSET=OFFSET(I)
          IF(DSCALE.LT.-1.0D35)THEN
            DSCALE=1.0
            DOFFSET=0.0D0
          END IF
          WRITE(IPFL,10,ERR=100)
     +    APAR(I)(:LEN_TRIM(APAR(I))),VAL(I),DSCALE,DOFFSET
10        FORMAT(2X,A12,3X,1PG23.16,3X,1PG14.7,3X,1PG14.7)
20      CONTINUE
        CLOSE(UNIT=IPFL)
        RETURN

100     CALL STPERR(10,1,AFILE,0,' ',CLINE)
        GO TO 9891

9891    JFAIL=1
        RETURN

#ifdef PESTMOD
        END SUBROUTINE ERRPRMSAV
#else
        END
#endif



C#ifndef PARALLEL

        SUBROUTINE EXTJAC(KFAIL,NXDIM,NOBS,NESPAR,NPAR,
     +  DIM_DI_PAR,DIM_DI_OBS,DIM_DI_IES,DIM_DI_PARENT,
     +  REFPAR,ORGVAL,
     +  ITRANS,SCALE,TEMPJAC,X,COMJAC,AJACFLE,CLINE,APAR,AOBS,PARJAC,
     +  PRECIS,NOPNT,NINFLE,NW,OFFSET,PARDEL,PWORD,INFLE,TMPFLE,
     +  IMSFLE,DI_NSKIP,DI_NDEP,DI_NPAR,DI_ORIENTATION,DI_DERFORMAT,
     +  DI_PAR,DI_IES,DI_PARENT,DI_OBS,JACFILE,TEMPJACDIM,NUMTIED,
     +  W1,IMODE)

#ifdef INTEL
        USE COMMON_MOD, ONLY: FLENME,NCALL,MCALL,JCALL
#endif
        USE PESTDATA,  ONLY: ERRMSG,XDIM1,XDIM2,MAXCOMPDIM,NCOMPDIM,
     +                       XC,IXC,ISTART_C,IFOUND_C,INST,EXTJAC_CALL,
     +                       COMPRESS_EASY,NXROW,IPAR2IES,PI_PAR,NPRIOR,
     +                       BINJACFILE,IXC_DIM,PR_INDEX

        IMPLICIT NONE

        LOGICAL LEXIST,LOPENED,LOPENED1
        INTEGER NXDIM,NESPAR,NPAR,IROW,NOBS,IPP,IES,PRECIS,NOPNT,
     +  NINFLE,IMSFLE,IFAIL,IPAROLD,IESS,IPAR,IOBS,KFAIL
#ifdef INTEL
        INTEGER NNPAR,NNOBS,IT,JFAIL,II,JJ,IFLAG,
     +  NREAD,NENTRY,IP,NUMTIED,JCOUNT,JJCOUNT,IMODE
#else
        INTEGER NCALL,MCALL,JCALL,NNPAR,NNOBS,IT,JFAIL,II,JJ,IFLAG,
     +  NREAD,NENTRY,IP,NUMTIED,JCOUNT,JJCOUNT,IMODE
#endif
#ifdef INTEL
        INTEGER (8) :: ICOMPRESS
        INTEGER (8) :: KK,ICOUNT,IJ
#else
        INTEGER     :: ICOMPRESS
        INTEGER     :: KK,ICOUNT,IJ
#endif

        INTEGER DIM_DI_PAR,DIM_DI_OBS,DIM_DI_IES,DIM_DI_PARENT
        INTEGER DI_NSKIP,DI_NDEP,DI_NPAR,JACFILE,TEMPJACDIM
        INTEGER NNCOL,IERR
        INTEGER ITRANS(NPAR),PARJAC(NPAR),NW(NPAR),LW(3),RW(3)
        INTEGER DI_PAR(DIM_DI_PAR),DI_OBS(DIM_DI_OBS),
     +  DI_IES(DIM_DI_IES),DI_PARENT(DIM_DI_PARENT)
        DOUBLE PRECISION RTEMP,LOGFAC,RRTEMP,RTEMP1
        DOUBLE PRECISION SUM
        DOUBLE PRECISION X(XDIM1,XDIM2),REFPAR(NPAR),
     +  ORGVAL(NPAR),SCALE(NPAR),OFFSET(NPAR),TEMPJAC(TEMPJACDIM),
     +  W1(NPAR)
        CHARACTER PARDEL(NINFLE)
        CHARACTER*(*) COMJAC,AJACFLE,CLINE
        CHARACTER*(*) DI_ORIENTATION,DI_DERFORMAT
        CHARACTER*(*) APAR(NPAR),AOBS(NOBS),PWORD(NPAR),
     +  INFLE(NINFLE),TMPFLE(NINFLE)
#ifndef INTEL
        CHARACTER*200 FLENME
#endif

#ifdef INTEL
        INTEGER (8) :: NTEMP,NNTEMP2,NNTEMP3
        INTEGER (8) :: I,J,IC,JC,ITEMP
#else
        INTEGER     :: NTEMP,NNTEMP2,NNTEMP3
        INTEGER     :: I,J,IC,JC,ITEMP
#endif


#ifdef INTEL
        INTEGER NNN
        INTEGER SYSTEM
#else
        COMMON /MODCAL/ NCALL,MCALL,JCALL
        COMMON /FLENME/ FLENME
#endif

#ifdef UNIX
        CHARACTER*120 CMDLIN
#ifdef SYS_FUNCTION
        INTEGER NNN,SYSTEM
#endif
#endif

        KFAIL=0
        LOGFAC=LOG(10.0D0)
        LEXIST=.TRUE.
        JCALL=JCALL+1
        EXTJAC_CALL=EXTJAC_CALL+1

C -- We check whether the saving of the compressed Jacobian can be made super-efficient.

        IF(EXTJAC_CALL.EQ.1)THEN
          IF(MAXCOMPDIM.GT.1)THEN
            COMPRESS_EASY=0
            NTEMP=NESPAR*NOBS+NCOMPDIM
            IF(MAXCOMPDIM.GE.NTEMP)THEN
              COMPRESS_EASY=1
              ALLOCATE(PI_PAR(NESPAR),STAT=IERR)
              IF(IERR.NE.0) GO TO 9450
              IF(NCOMPDIM.EQ.0)THEN
                DO I=1,NTEMP
                  IXC(I)=I
                END DO
                DO I=1,NESPAR
                  PI_PAR(I)=0
                END DO
              ELSE
                PI_PAR=0                            !an array
                IC=NESPAR*NOBS+NCOMPDIM+1
                JC=NESPAR*NXROW+1
                NNTEMP2=NESPAR*NXROW-(NXROW-NOBS)
                NNTEMP3=NESPAR*NXROW-NXROW
                NNCOL=NESPAR
                DO I=NCOMPDIM,1,-1
11                CONTINUE
                  ITEMP=IXC(I)
                  IF(ITEMP.GT.NNTEMP2)THEN
                    IC=IC-1
                    IXC(IC)=IXC(I)
                    XC(IC)=XC(I)
                    PI_PAR(NNCOL)=PI_PAR(NNCOL)+1
                  ELSE
                    DO J=NNTEMP2,NNTEMP3+1,-1
                      IC=IC-1
                      IXC(IC)=J
                      XC(IC)=0.0D0
                    END DO
                    NNTEMP3=NNTEMP3-NXROW
                    NNTEMP2=NNTEMP2-NXROW
                    NNCOL=NNCOL-1
                    GO TO 11
                  END IF
                END DO
                IF(IC.GT.1)THEN
                  J=0
                  NNTEMP3=NXROW
                  NNTEMP2=NOBS
                  DO I=1,IC-1
                    J=J+1
                    IF(J.GT.NNTEMP2)THEN
                      J=NNTEMP3+1
                      NNTEMP3=NNTEMP3+NXROW
                      NNTEMP2=NNTEMP2+NXROW
                    END IF
                    IXC(I)=J
                    XC(I)=0.0D0
                  END DO
                END IF
                SUM=0.0D0
                DO NNCOL=1,NESPAR
                  SUM=SUM+PI_PAR(NNCOL)
                  PI_PAR(NNCOL)=SUM
                END DO
                DO NNCOL=NESPAR,2,-1                    ! Staggered by 1 to facilitate later calcs.
                  PI_PAR(NNCOL)=PI_PAR(NNCOL-1)
                END DO
                PI_PAR(1)=0
              END IF
              NCOMPDIM=NTEMP
              IF((NPRIOR.EQ.0).OR.(PR_INDEX.NE.0))THEN
                DEALLOCATE(IXC,STAT=IERR)
                IF(IERR.NE.0)THEN
                  WRITE(ERRMSG,5010)
5010              FORMAT('Cannot deallocate IXC vector.')
                  GO TO 9991
                END IF
                ALLOCATE(IXC(2),STAT=IERR)
                IF(IERR.NE.0)THEN
                  WRITE(ERRMSG,5010)
                  GO TO 9991
                END IF
                IXC_DIM=2
              END IF
            END IF
          END IF
        END IF

C        write(6,*) ' compress_easy = ',compress_easy             !debug
C        write(6,*) ' nprior        = ',nprior                    !debug
C        write(6,*) ' ixc_dim       = ',ixc_dim                   !debug
C        write(6,*) ' maxcomdim     = ',maxcompdim                !debug

        CALL INWRIT(JFAIL,NPAR,PRECIS,NOPNT,NINFLE,NW,REFPAR,SCALE,
     +  OFFSET,PARDEL,PWORD,INFLE,TMPFLE,APAR,CLINE,IMSFLE,-10000,
     +  ITRANS,NOBS)
        IF(JFAIL.NE.0) GO TO 9891

C        IF(JCALL.EQ.1)THEN
          CALL DELFILE1(JFAIL,AJACFLE,CLINE)
          IF(JFAIL.NE.0) GO TO 9891
C        END IF

        IF(COMPRESS_EASY.NE.0)THEN
          ICOUNT=0
          DO KK=1,MAXCOMPDIM
            IF(XC(KK).NE.0.0D0) ICOUNT=ICOUNT+1
          END DO
          IF(ICOUNT.GT.0)THEN
            DO I=20,100
              INQUIRE(UNIT=I,OPENED=LOPENED1)
              IF(.NOT.LOPENED1)THEN
#ifdef UNIX
                OPEN(UNIT=I,FILE='tempjac.###',FORM='UNFORMATTED',
     +          ACCESS='STREAM')
#else
#ifdef LAHEY
#ifdef LF90
                OPEN(UNIT=I,FILE='tempjac.###',FORM='UNFORMATTED',
     +          ACCESS='TRANSPARENT')
#else
                OPEN(UNIT=I,FILE='tempjac.###',FORM='BINARY')
#endif
#else
#ifdef INTEL
                OPEN(UNIT=I,FILE='tempjac.###',FORM='UNFORMATTED',
     +          ACCESS='STREAM')
#else
                OPEN(UNIT=I,FILE='tempjac.###',FORM='UNFORMATTED')
#endif
#endif
#endif
                WRITE(I) ICOUNT
                DO KK=1,MAXCOMPDIM
                 IF(XC(KK).NE.0.0D0) WRITE(I) KK,XC(KK)
                END DO
                CLOSE(UNIT=I)
                GO TO 5000
              END IF
            END DO
5000        CONTINUE
          END IF
          DEALLOCATE(XC,STAT=IERR)
          IF(IERR.NE.0)THEN
            WRITE(ERRMSG,5001)
5001        FORMAT('External derivatives error: ',
     +      'cannot deallocate XC matrix.')
            GO TO 9991
          END IF
        END IF

#ifdef UNIX
#ifdef SYS_FUNCTION
        NNN=SYSTEM(COMJAC(1:LEN_TRIM(COMJAC)))
#else
        CALL SYSTEM(COMJAC(1:LEN_TRIM(COMJAC)))
#endif
#else
#ifdef LAHEY
#ifdef AT
        CALL SYSTEM(COMJAC(1:LEN_TRIM(COMJAC)),.false.)
#else
        CALL SYSTEM(COMJAC(1:LEN_TRIM(COMJAC)))
#endif
#endif
#endif
#ifdef INTEL
        NNN=SYSTEM(TRIM(COMJAC))
#endif

        IF(COMPRESS_EASY.NE.0)THEN
          ALLOCATE(XC(MAXCOMPDIM),STAT=IERR)
          IF(IERR.NE.0)THEN
            WRITE(ERRMSG,5002)
5002        FORMAT('External derivatives error: ',
     +      'cannot reallocate XC matrix.')
            GO TO 9991
          END IF
          XC=0.0D0
          IF(ICOUNT.NE.0)THEN
            DO I=20,100
              INQUIRE(UNIT=I,OPENED=LOPENED1)
              IF(.NOT.LOPENED1)THEN
#ifdef LAHEY
#ifdef LF90
                OPEN(UNIT=I,FILE='tempjac.###',FORM='UNFORMATTED',
     +          ACCESS='TRANSPARENT',STATUS='OLD',IOSTAT=IERR)
#else
                OPEN(UNIT=I,FILE='tempjac.###',FORM='BINARY',
     +          STATUS='OLD',IOSTAT=IERR)
#endif
#else
                OPEN(UNIT=I,FILE='tempjac.###',FORM='UNFORMATTED',
     +          ACCESS='STREAM',STATUS='OLD',IOSTAT=IERR)
#endif
                IF(IERR.NE.0)THEN
                  WRITE(ERRMSG,5003)
5003              FORMAT('External derivatives error: cannot ',
     +            're-open temporary compressed Jacobian file.')
                  GO TO 9991
                END IF
                READ(I,IOSTAT=IERR)ICOUNT
                IF(IERR.NE.0)THEN
                  WRITE(ERRMSG,5004)
5004              FORMAT('External derivatives error: cannot ',
     +            're-read temporary compressed Jacobian file.')
                  GO TO 9991
                END IF
                DO IJ=1,ICOUNT
                  READ(I,IOSTAT=IERR) KK,XC(KK)
                  IF(IERR.NE.0)THEN
                    WRITE(ERRMSG,5004)
                    GO TO 9991
                  END IF
                END DO
                CLOSE(UNIT=I,STATUS='DELETE')
                GO TO 5005
              END IF
            END DO
5005        CONTINUE
          END IF
        END IF

        IF(IMODE.EQ.1) RETURN

        FLENME=AJACFLE
        IF(BINJACFILE.EQ.0)THEN
          CALL FFOPEN(JFAIL,INST,'r',' ',152,CLINE)
          IF(JFAIL.NE.0) GO TO 9891
        ELSE
          CALL FFOPEN(JFAIL,-INST,'r',' ',152,CLINE)
          IF(JFAIL.NE.0) THEN
            WRITE(ERRMSG,12) TRIM(AJACFLE)
12          FORMAT('Cannot open binary external derivatives ',
     +      'file ',A,': did derivative-calculation model run ',
     +      'correctly?')
            GO TO 9991
        END IF
        END IF
        IF(JACFILE.EQ.2) GO TO 1000
        IF(BINJACFILE.EQ.0)THEN
          READ(INST,'(A)',ERR=9000,END=9100) CLINE
          CALL LINSPL(IFAIL,2,LW,RW,CLINE)
          IF(IFAIL.NE.0) GO TO 9000
          CALL NUMRD(IFAIL,0,LW(1),RW(1),NNPAR,RTEMP,CLINE)
          IF(IFAIL.NE.0) GO TO 9000
          CALL NUMRD(IFAIL,0,LW(2),RW(2),NNOBS,RTEMP,CLINE)
          IF(IFAIL.NE.0) GO TO 9000
        ELSE
          READ(INST,ERR=9600,END=9650) NNPAR,NNOBS,ICOMPRESS   ! Note that IMCOMPRESS is integer (8) for INTEL and not for LAHEY
          IF(ICOMPRESS.LT.0)THEN
            CALL STPERR(184,1,' ',0,' ',CLINE)
            GO TO 9891
          END IF
        END IF
        IF((NNPAR.NE.NPAR).OR.(NNOBS.NE.NOBS))THEN
          CALL STPERR(164,1,' ',0,' ',CLINE)
          GO TO 9891
        END IF
        IF(BINJACFILE.EQ.0)THEN
          CALL LINSPL(IFAIL,3,LW,RW,CLINE)
          IF(IFAIL.NE.0)THEN
            ICOMPRESS=0
          ELSE
            CALL NUMRD(IFAIL,0,LW(3),RW(3),ITEMP,RTEMP,CLINE)
            IF(IFAIL.NE.0) THEN
              ICOMPRESS=0
            ELSE IF(ITEMP.LT.0)THEN
              CALL STPERR(184,1,' ',0,' ',CLINE)
              GO TO 9891
            END IF
            ICOMPRESS=ITEMP
          END IF
        END IF

C -- To save time in later processing we list parameters which are parent parameters using
C    a spare array.

        DO IPP=1,NPAR
          W1(IPP)=0.0D0
          IF(ITRANS(IPP).GE.0)THEN
            DO I=1,NPAR
              IF(-ITRANS(I).EQ.IPP)THEN
                W1(IPP)=W1(IPP)+1.0D0
              END IF
            END DO
          END IF
        END DO

C -- Now we read the derivatives file.

        ISTART_C=1
        IF(ICOMPRESS.EQ.0)THEN
          DO 100 IROW=1,NOBS
            IF(BINJACFILE.EQ.0)THEN
              READ(INST,*,ERR=9000,END=9100) (TEMPJAC(IPP),IPP=1,NPAR)
            ELSE
              READ(INST,ERR=9000,END=9100) (TEMPJAC(IPP),IPP=1,NPAR)
            END IF
            IES=0
            DO 80 IPP=1,NPAR
              IF(ITRANS(IPP).LT.0) GO TO 80
              IES=IES+1
              RTEMP=TEMPJAC(IPP)
              IF((RTEMP.LT.-1.1D33).AND.(RTEMP.GT.-1.2D33))THEN
                IF(PARJAC(IPP).EQ.0) GO TO 9200
                GO TO 80
              END IF
              RTEMP=RTEMP*SCALE(IPP)
              IF(W1(IPP).GT.0.0D0)THEN
                JCOUNT=NINT(W1(IPP))
                JJCOUNT=0
                DO 70 I=1,NPAR
                  IF(-ITRANS(I).EQ.IPP)THEN
                    JJCOUNT=JJCOUNT+1
                    RRTEMP=TEMPJAC(I)
                    IF((RRTEMP.LT.-1.1D33).AND.(RRTEMP.GT.-1.2D33))
     +              GO TO 9300
                    RRTEMP=RRTEMP*SCALE(I)
                    RTEMP=RTEMP+RRTEMP*ORGVAL(I)/ORGVAL(IPP)
                    IF(JJCOUNT.EQ.JCOUNT) GO TO 71
                  END IF
70              CONTINUE
71              CONTINUE
              END IF
              IF(MAXCOMPDIM.LE.1)THEN
                IF(ITRANS(IPP).EQ.0) THEN
                  X(IROW,IES)=RTEMP
                ELSE IF(ITRANS(IPP).EQ.1)THEN
                  X(IROW,IES)=REFPAR(IPP)*RTEMP*LOGFAC
                END IF
              ELSE
                IF(ITRANS(IPP).EQ.1) RTEMP=REFPAR(IPP)*RTEMP*LOGFAC
                CALL STORE_VALUE(IFAIL,NCOMPDIM,XC,IXC,RTEMP,IROW,IES)
                IF(IFAIL.NE.0) GO TO 9400
                ISTART_C=IFOUND_C
              END IF
80          CONTINUE
100       CONTINUE
        ELSE
          IF(NUMTIED.GT.0)THEN
            WRITE(ERRMSG,101) TRIM(AJACFLE)
101         FORMAT('File ',A,' contains derivatives in compressed ',
     +      'format. This is not allowed if there are any tied ',
     +      'parameters.')
            GO TO 9991
          END IF
          IPAROLD=0
          DO I=1,ICOMPRESS
            IF(BINJACFILE.EQ.0)THEN
              READ(INST,*,ERR=9000,END=9100) IPAR,IOBS,RTEMP
            ELSE
              READ(INST,ERR=9000,END=9100) IPAR,IOBS,RTEMP
            END IF
            IF((RTEMP.LT.-1.1D33).AND.(RTEMP.GT.-1.2D33))CYCLE
            IF(ITRANS(IPAR).EQ.-1000000) THEN
              CYCLE
            ELSE IF(ITRANS(IPAR).GE.0)THEN
              IF(IPAR.NE.IPAROLD)THEN
                IES=IPAR2IES(IPAR)
                IPAROLD=IPAR
              END IF
              RTEMP=RTEMP*SCALE(IPAR)
              IF(ITRANS(IPAR).EQ.1) RTEMP=REFPAR(IPAR)*RTEMP*LOGFAC
              IF(MAXCOMPDIM.LE.1)THEN
                X(IOBS,IES)=RTEMP
              ELSE
                CALL STORE_VALUE(IFAIL,NCOMPDIM,XC,IXC,RTEMP,IOBS,IES)
                IF(IFAIL.NE.0) GO TO 9400
                ISTART_C=IFOUND_C
              END IF
            ELSE
              IESS=0
              IT=-ITRANS(IPAR)
              DO IPP=1,NPAR
                IF(ITRANS(IPP).LT.0) CYCLE
                IESS=IESS+1
                IF(IT.EQ.IPP) EXIT
              END DO
              IF(MAXCOMPDIM.LE.1)THEN
                RTEMP1=X(IOBS,IESS)
              ELSE
                CALL GET_VALUE(NCOMPDIM,XC,IXC,RTEMP1,IOBS,IESS)
                ISTART_C=IFOUND_C
              END IF
              RTEMP=RTEMP*SCALE(IPAR)
              RTEMP=RTEMP1+RTEMP*ORGVAL(IPAR)/ORGVAL(IPP)
              IF(MAXCOMPDIM.LE.1)THEN
                X(IOBS,IESS)=RTEMP
              ELSE
              CALL STORE_VALUE(IFAIL,NCOMPDIM,XC,IXC,RTEMP,IOBS,IESS)
              IF(IFAIL.NE.0) GO TO 9400
              END IF
            END IF
          END DO
        ENDIF

C -- The above code for tied parameters has three problems.
C     (a) It assumes that X has been zeroed prior to derivatives calculation (which
C         may lead to incorrect results if IBNDSTK is set).
C     (b) For parameters tied to a log-transformed parameter, multiplication by
C         the parameter value and LOGFAC must take place after all the tied
C         parameter sensitivities have been added
C     (d) for tied parameters no option is given for adding their effect when
C         MAXCOMPDIM is less than 1.
C     The use of tied parameters with an compressed derivatives file is now not allowed.

C -- Note that derivative values of zero are stored anyway. It is assumed that
C    non-zero values will arrive later.

        GO TO 9999

1000    CONTINUE

C -- The following code pertains to reading a derivatives file in JUPITER
C    format.

C -- First we zero the elements of any parameters that are parent parameters.

        ISTART_C=1
        DO I=1,DI_NPAR
          IF(DI_PARENT(I).EQ.0) CYCLE
          IES=DI_IES(I)
          DO J=1,DI_NDEP
            JJ=DI_OBS(J)
            IF(MAXCOMPDIM.LE.1)THEN
              X(JJ,IES)=0.0D0
            ELSE
              RTEMP=0.0D0
              CALL STORE_VALUE(JFAIL,NCOMPDIM,XC,IXC,RTEMP,JJ,IES)
              IF(IFAIL.NE.0) GO TO 9400
              ISTART_C=IFOUND_C
            END IF
          END DO
        END DO

        IF(DI_NSKIP.GT.0) THEN
          DO I=1,DI_NSKIP
            READ(INST,*,ERR=9500,END=9550)
          END DO
        END IF
        IF(INDEX(DI_ORIENTATION,'row/dep').NE.0) THEN
          IFLAG=1
          NREAD=DI_NDEP
          NENTRY=DI_NPAR
        ELSE
          IFLAG=2
          NREAD=DI_NPAR
          NENTRY=DI_NDEP
        END IF
        DO J=1,NREAD
          IF(IFLAG.EQ.1)THEN
            IOBS=DI_OBS(J)
          ELSE
            IES=DI_IES(J)
            IPAR=DI_PAR(J)
            IP=DI_PARENT(J)
          END IF
          IF(DI_DERFORMAT.EQ.'(free)') THEN
            READ(INST,*,ERR=9500,END=9550)
     +      (TEMPJAC(I),I=1,NENTRY)
          ELSE
            READ(INST,DI_DERFORMAT,ERR=9500,END=9550)
     +      (TEMPJAC(I),I=1,NENTRY)
          END IF
          DO I=1,NENTRY
            RTEMP=TEMPJAC(I)
            IF(IFLAG.EQ.1)THEN
              IES=DI_IES(I)
              IPAR=DI_PAR(I)
              IP=DI_PARENT(I)
            ELSE
              IOBS=DI_OBS(I)
            END IF
            IF(ITRANS(IPAR).EQ.-1000000) THEN
              CYCLE
            ELSE IF(ITRANS(IPAR).GE.0)THEN
              RTEMP=RTEMP*SCALE(IPAR)
              IF(ITRANS(IPAR).EQ.1) THEN
                IF(IP.EQ.0) RTEMP=REFPAR(IPAR)*RTEMP*LOGFAC
              END IF
              IF(MAXCOMPDIM.LE.1)THEN
                IF(IP.EQ.0)THEN
                  X(IOBS,IES)=RTEMP
                ELSE
                  X(IOBS,IES)=RTEMP+X(IOBS,IES)
                END IF
              ELSE
                IF(IP.EQ.0)THEN
                  CALL STORE_VALUE(IFAIL,NCOMPDIM,XC,IXC,RTEMP,IOBS,
     +            IES)
                  IF(IFAIL.NE.0) GO TO 9400
                  ISTART_C=IFOUND_C
                ELSE
                  CALL GET_VALUE(NCOMPDIM,XC,IXC,RTEMP1,IOBS,IES)
                  ISTART_C=IFOUND_C
                  RTEMP=RTEMP+RTEMP1
                  CALL STORE_VALUE(IFAIL,NCOMPDIM,XC,IXC,RTEMP,IOBS,
     +            IES)
                  IF(IFAIL.NE.0) GO TO 9400
                  ISTART_C=IFOUND_C
                END IF
              END IF
            ELSE
              RTEMP=RTEMP*SCALE(IPAR)
              IESS=0
              IT=-ITRANS(IPAR)
              DO IPP=1,NPAR
                IF(ITRANS(IPP).LT.0) CYCLE
                IESS=IESS+1
                IF(IT.EQ.IPP) EXIT
              END DO
              IF(MAXCOMPDIM.LE.1)THEN
                X(IOBS,IESS)=X(IOBS,IESS)+RTEMP*ORGVAL(IPAR)/ORGVAL(IPP)
              ELSE
                CALL GET_VALUE(NCOMPDIM,XC,IXC,RTEMP1,IOBS,IESS)
                ISTART_C=IFOUND_C
                RTEMP=RTEMP1+RTEMP*ORGVAL(IPAR)/ORGVAL(IPP)
                CALL STORE_VALUE
     +          (IFAIL,NCOMPDIM,XC,IXC,RTEMP,IOBS,IESS)
                IF(IFAIL.NE.0) GO TO 9400
              END IF
            END IF
          END DO
        END DO

C -- Now the logarithmic status of parent parameters is taken care of.

        ISTART_C=1
        DO I=1,DI_NPAR
          IF(DI_PARENT(I).EQ.0) CYCLE
          II=DI_PAR(I)
          IF(ITRANS(II).NE.1) CYCLE
          RTEMP=REFPAR(II)*LOGFAC
          IES=DI_IES(I)
          DO J=1,DI_NDEP
            JJ=DI_OBS(J)
            IF(MAXCOMPDIM.LE.1)THEN
              X(JJ,IES)=X(JJ,IES)*RTEMP
            ELSE
              CALL GET_VALUE(NCOMPDIM,XC,IXC,RTEMP1,JJ,IES)
              ISTART_C=IFOUND_C
              RTEMP1=RTEMP1*RTEMP
              CALL STORE_VALUE(JFAIL,NCOMPDIM,XC,IXC,RTEMP1,JJ,IES)
              IF(IFAIL.NE.0) GO TO 9400
              ISTART_C=IFOUND_C
            END IF
          END DO
        END DO

        GO TO 9999

9000    CALL STPERR(153,1,' ',0,' ',CLINE)
        GO TO 9891
9100    CALL STPERR(154,1,' ',0,' ',CLINE)
        GO TO 9891
9200    CALL STPERR(155,7,AOBS(IROW),0,APAR(IPP),CLINE)
        GO TO 9891
9300    CALL STPERR(156,1,' ',0,' ',CLINE)
        GO TO 9891
9400    CALL STPERR(181,0,' ',0,' ',CLINE)
        GO TO 9891

9891    KFAIL=1
        GO TO 9999

9450    WRITE(ERRMSG,9460)
9460    FORMAT('Error in external derivatives functionality: cannot ',
     +  'allocate sufficient memory to continue execution.')
        GO TO 9991
9500    WRITE(ERRMSG,9510) TRIM(AJACFLE)
9510    FORMAT('Error reading derivatives file ',A,'.')
        GO TO 9991
9550    WRITE(ERRMSG,9560) TRIM(AJACFLE)
9560    FORMAT('Unexpected end to derivatives file ',A,'.')
        GO TO 9991

9600    WRITE(ERRMSG,9610) TRIM(AJACFLE)
9610    FORMAT('Error reading binary derivatives file ',A,'.')
        GO TO 9991
9650    WRITE(ERRMSG,9660) TRIM(AJACFLE)
9660    FORMAT('Unexpected end to binary derivatives file ',A,'.')

9991    KFAIL=2     ! ALLOW MAIN PROGRAM TO ACCOMODATE THIS

9999    INQUIRE(UNIT=INST,OPENED=LOPENED)
        IF(LOPENED) CLOSE(UNIT=INST)
        RETURN

#ifdef PESTMOD
        END SUBROUTINE EXTJAC
#else
        END
#endif

C#endif


        SUBROUTINE PESTMESS(JFAIL,IMMF,ITYPE,NPAR,NOBS,INCPAR,ITRANS,
     +  APAR,PVAL,CLINE,AFILE)

C -- THIS SUBROUTINE WRITES A PEST-TO-MODEL MESSAGE BEFORE A MODEL RUN.

#ifdef INTEL
        USE COMMON_MOD, ONLY: FLENME
#endif
        IMPLICIT NONE

        INTEGER ITYPE,NPAR,INCPAR,IPP,ITEMP,NOBS,JFAIL,IMMF
        INTEGER ITRANS(NPAR)
        DOUBLE PRECISION PVAL(NPAR)
        CHARACTER*(*) APAR(NPAR)
        CHARACTER*(*) CLINE,AFILE
#ifndef INTEL
        CHARACTER*200 FLENME

        COMMON /FLENME/ FLENME
#endif

        JFAIL=0
        IF(ITYPE.EQ.1)THEN
          WRITE(IMMF,10,ERR=9000)
10        FORMAT(' forward_model_run')
          WRITE(IMMF,40,ERR=9000) 0
        ELSE IF(ITYPE.EQ.2)THEN
          WRITE(IMMF,20,ERR=9000)
20        FORMAT(' external_derivatives')
          WRITE(IMMF,40,ERR=9000) 0
        ELSE IF(ITYPE.EQ.3)THEN
          WRITE(IMMF,30,ERR=9000)
30        FORMAT(' derivative_increment')
          WRITE(IMMF,40,ERR=9000) INCPAR
40        FORMAT(I7,1X,I7)
        ELSE IF(ITYPE.EQ.-3)THEN
          WRITE(IMMF,31,ERR=9000)
31        FORMAT(' extended_derivative_increment')
          WRITE(IMMF,40,ERR=9000) INCPAR
        END IF
        WRITE(IMMF,40) NPAR,NOBS
        DO 100 IPP=1,NPAR
          IF(ITRANS(IPP).LT.-1000001)THEN
            ITEMP=ITRANS(IPP)+2000000
          ELSE
            ITEMP=ITRANS(IPP)
          END IF
          WRITE(IMMF,50) APAR(IPP)(1:LEN_TRIM(APAR(IPP))),PVAL(IPP),
     +    ITEMP
50        FORMAT(1X,A12,2X,1PG14.7,2X,I8)
100     CONTINUE

        RETURN

9000    CALL STPERR(162,0,AFILE,0,' ',CLINE)
        JFAIL=1
        RETURN

#ifdef PESTMOD
        END SUBROUTINE PESTMESS
#else
        END
#endif



        SUBROUTINE ROTATE(ITYPE,LDIM,NDIM,MATDIM1,MATDIM2,NSTART,
     +  ROTMAT,VECTOR,MATRIX,WORK,IGROUP,NXROW,NOBGNM)

C -- SUBROUTINE ROTATE ROTATES A VECTOR OR MATRIX USING EIGENVECTORS OF THE
C    PARTIAL COVARIANCE MATRIX.

         IMPLICIT NONE

         INTEGER ITYPE,NDIM,MATDIM1,MATDIM2,NSTART,I,J,ICOL,LDIM,
     +   IGROUP,NXROW,JJ
         INTEGER NOBGNM(NXROW)
         DOUBLE PRECISION SUM
         DOUBLE PRECISION VECTOR(NDIM),WORK(NDIM)
         DOUBLE PRECISION ROTMAT(LDIM,NDIM),MATRIX(MATDIM1,MATDIM2)

         IF(ITYPE.EQ.1)THEN
           DO 100 I=1,NDIM
             SUM=0.0D0
             DO 50 J=1,NDIM
               SUM=SUM+ROTMAT(J,I)*VECTOR(J)
50           CONTINUE
             WORK(I)=SUM
100        CONTINUE
           DO 120 J=1,NDIM
             VECTOR(J)=WORK(J)
120        CONTINUE
         ELSE IF(ITYPE.EQ.-1)THEN
           DO 200 I=1,NDIM
             SUM=0.0D0
             DO 150 J=1,NDIM
               SUM=SUM+ROTMAT(I,J)*VECTOR(J)
150          CONTINUE
             WORK(I)=SUM
200        CONTINUE
           DO 220 J=1,NDIM
             VECTOR(J)=WORK(J)
220        CONTINUE
         ELSE IF(ITYPE.EQ.2)THEN
           DO 350 ICOL=1,MATDIM2
             DO 300 I=1,NDIM
               SUM=0.0D0
               JJ=0
               DO 250 J=1,NDIM
225              JJ=JJ+1
                 IF(NOBGNM(NSTART+JJ-1).EQ.IGROUP)THEN
                   SUM=SUM+ROTMAT(J,I)*MATRIX(NSTART+JJ-1,ICOL)
                 ELSE
                   GO TO 225
                 END IF
250            CONTINUE
               WORK(I)=SUM
300          CONTINUE
             JJ=0
             DO 320 J=1,NDIM
305            JJ=JJ+1
               IF(NOBGNM(NSTART+JJ-1).EQ.IGROUP)THEN
                 MATRIX(NSTART+JJ-1,ICOL)=WORK(J)
               ELSE
                 GO TO 305
               END IF
320          CONTINUE
350        CONTINUE
         ELSE IF(ITYPE.EQ.-2)THEN
           DO 450 ICOL=1,MATDIM2
             DO 400 I=1,NDIM
               SUM=0.0D0
               JJ=0
               DO 360 J=1,NDIM
355              JJ=JJ+1
                 IF(NOBGNM(NSTART+JJ-1).EQ.IGROUP)THEN
                   SUM=SUM+ROTMAT(I,J)*MATRIX(NSTART+JJ-1,ICOL)
                 ELSE
                   GO TO 355
                 END IF
360            CONTINUE
               WORK(I)=SUM
400          CONTINUE
             JJ=0
             DO 420 J=1,NDIM
405            JJ=JJ+1
               IF(NOBGNM(NSTART+JJ-1).EQ.IGROUP)THEN
                 MATRIX(NSTART+JJ-1,ICOL)=WORK(J)
               ELSE
                 GO TO 405
               END IF
420          CONTINUE
450        CONTINUE
         END IF

         RETURN

#ifdef PESTMOD
         END SUBROUTINE ROTATE
#else
         END
#endif


         LOGICAL FUNCTION SAME(DTEMP1,DTEMP2,TOL)

         IMPLICIT NONE
         DOUBLE PRECISION DTEMP1,DTEMP2,TOL

         SAME=.FALSE.
         IF(DTEMP1.EQ.0.0D0)THEN
           IF(DTEMP2.EQ.0.0D0)SAME=.TRUE.
           RETURN
         ELSE IF(DTEMP2.EQ.0.0D0)THEN
           IF(DTEMP1.EQ.0.0D0)SAME=.TRUE.
           RETURN
         ELSE
           IF(ABS(DTEMP1-DTEMP2).LE.ABS(DTEMP1+DTEMP2)*0.5*TOL)
     +     SAME=.TRUE.
           RETURN
         END IF

#ifdef PESTMOD
         END FUNCTION SAME
#else
         END
#endif


        SUBROUTINE HLDREAD(JFAIL,NPAR,NPARGP,NESPAR,NLAMBDA,NRELPMX,
     +  NFACPMX,APAR,AGROUP,PHOLD,GHOLD,EHOLD,CLINE,IREG,NVECBND,IREC,
     +  NRELPREDSTP,NABSPREDSTP,NINITSCHFAC,NMULSCHFAC,NNSEARCH,IPRED)

#ifdef INTEL
        USE COMMON_MOD, ONLY: FLENME,CASEFL
#endif
        USE PESTDATA, ONLY : IGNF
        IMPLICIT NONE

        INTEGER NPAR,NPARGP,IREG,NESPAR,NVECBND,JFAIL,IREC,NNSEARCH,
     +  IPRED
        DOUBLE PRECISION NLAMBDA,NRELPMX,NFACPMX,NRELPREDSTP,
     +  NABSPREDSTP,NINITSCHFAC,NMULSCHFAC
        CHARACTER*(*) APAR(NPAR),AGROUP(NPARGP)
        INTEGER PHOLD(NPAR),EHOLD(NESPAR)
        DOUBLE PRECISION GHOLD(NPARGP)
        CHARACTER*(*) CLINE
        INTEGER IPP,IGP,IERR,LW(5),RW(5),IFAIL,IPAR,IMESS,ITEMP,IINT,
     +  ITEMP1,IES
        DOUBLE PRECISION RTEMP
#ifndef INTEL
        CHARACTER*100 CASEFL
        CHARACTER*200 FLENME
#endif
        CHARACTER*12 TPAR,ATEMP,ATEMP1,ATEMPA*2

#ifndef INTEL
        COMMON /CASE/ CASEFL
        COMMON /FLENME/ FLENME
#endif

        JFAIL=0
        NLAMBDA=0.0D0
        NRELPMX=0.0D0
        NFACPMX=0.0D0
        NRELPREDSTP=-1.0D0
        NABSPREDSTP=-1.0D0
        NINITSCHFAC=-1.0D0
        NMULSCHFAC=-1.0D0
        NNSEARCH=-1
        NVECBND=-1
        IMESS=0
        IINT=0

        DO 100 IPP=1,NPAR
          PHOLD(IPP)=0
100     CONTINUE
        DO 150 IGP=1,NPARGP
          GHOLD(IGP)=0.0D0
150     CONTINUE
        DO 160 IES=1,NESPAR
          EHOLD(IES)=0
160     CONTINUE

#ifdef CAPFILE
        FLENME=CASEFL(:LEN_TRIM(CASEFL))//'.HLD'
#else
        FLENME=CASEFL(:LEN_TRIM(CASEFL))//'.hld'
#endif
        OPEN(UNIT=IGNF,FILE=FLENME,STATUS='OLD',IOSTAT=IERR)
        IF(IERR.NE.0) GO TO 500
        WRITE(IREC,*,ERR=9000)
        WRITE(IREC,300,ERR=9000) FLENME(1:LEN_TRIM(FLENME))
300     FORMAT(  '    File ',a,' detected - user intervention:-')
C        IF(IREG.EQ.1)THEN
C          WRITE(IREC,301,ERR=9000)
C301       FORMAT('    Note: user intervention functionality does not ',
C     +    'work well when PEST is ')
C          WRITE(IREC,302,ERR=9000)
C302       FORMAT('    used in regularisation mode.')
C          WRITE(6,300) FLENME(1:LEN_TRIM(FLENME))
C          WRITE(6,301)
C          WRITE(6,302)
C        END IF
        IINT=1
200     READ(IGNF,'(A)',END=500) CLINE
        CALL LOWCAS(CLINE)
        CALL LINSPL(IFAIL,2,LW,RW,CLINE)
        IF(IFAIL.NE.0) GO TO 200
        IF(CLINE(LW(1):RW(1)).EQ.'lambda')THEN
          CALL DREALRD(IFAIL,CLINE(LW(2):RW(2)),RTEMP)
          IF(IFAIL.EQ.0) THEN
            IF(RTEMP.GT.0.0) THEN
              NLAMBDA=RTEMP
              IMESS=IMESS+1
              WRITE(IREC,310,ERR=9000) NLAMBDA
310           FORMAT('       Set LAMBDA to ',1pg11.5)
            END IF
          END IF
        ELSE IF(CLINE(LW(1):LW(3)).EQ.'rem') THEN
          GO TO 200
        ELSE IF(CLINE(LW(1):LW(1)).EQ.'#')THEN
          GO TO 200
        ELSE IF(CLINE(LW(1):RW(1)).EQ.'relparmax')THEN
          CALL DREALRD(IFAIL,CLINE(LW(2):RW(2)),RTEMP)
          IF(IFAIL.EQ.0)THEN
            IF(RTEMP.GT.0.0) THEN
              NRELPMX=RTEMP
              IMESS=IMESS+1
              WRITE(IREC,320,ERR=9000)NRELPMX
320           FORMAT('       Set RELPARMAX to ',1pg11.5)
            END IF
          END IF
        ELSE IF(CLINE(LW(1):RW(1)).EQ.'facparmax')THEN
          CALL DREALRD(IFAIL,CLINE(LW(2):RW(2)),RTEMP)
          IF(IFAIL.EQ.0)THEN
            IF(RTEMP.GT.1.0) THEN
              NFACPMX=RTEMP
              IMESS=IMESS+1
              WRITE(IREC,330,ERR=9000) NFACPMX
330           FORMAT('       Set FACPARMAX to ',1pg11.5)
            END IF
          END IF
        ELSE IF(CLINE(LW(1):RW(1)).EQ.'upvecbend')THEN
          CALL INTRD(IFAIL,CLINE(LW(2):RW(2)),ITEMP)
          IF(IFAIL.EQ.0)THEN
            IF((ITEMP.EQ.1).OR.(ITEMP.EQ.0))THEN
              NVECBND=ITEMP
              IMESS=IMESS+1
              WRITE(IREC,321,ERR=9000)NVECBND
321           FORMAT('       Set UPVECBEND to ',I1)
            END IF
          END IF
        ELSE IF(CLINE(LW(1):RW(1)).EQ.'relpredstp')THEN
          IF(IPRED.NE.0)THEN
            CALL DREALRD(IFAIL,CLINE(LW(2):RW(2)),RTEMP)
            IF(IFAIL.EQ.0)THEN
              IF((RTEMP.LT.1.0D0).AND.(RTEMP.GE.0.0D0))THEN
                NRELPREDSTP=RTEMP
                IMESS=IMESS+1
                WRITE(IREC,324,ERR=9000) NRELPREDSTP
324             FORMAT('       Set RELPREDSTP to ',1pg11.5)
              END IF
            END IF
          END IF
        ELSE IF(CLINE(LW(1):RW(1)).EQ.'abspredstp')THEN
          IF(IPRED.NE.0)THEN
            CALL DREALRD(IFAIL,CLINE(LW(2):RW(2)),RTEMP)
            IF(IFAIL.EQ.0)THEN
              IF(RTEMP.GE.0.0D0)THEN
                NABSPREDSTP=RTEMP
                IMESS=IMESS+1
                WRITE(IREC,326,ERR=9000) NABSPREDSTP
326             FORMAT('       Set ABSPREDSTP to ',1pg11.5)
              END IF
            END IF
          END IF
        ELSE IF(CLINE(LW(1):RW(1)).EQ.'initschfac')THEN
          IF(IPRED.NE.0)THEN
            CALL DREALRD(IFAIL,CLINE(LW(2):RW(2)),RTEMP)
            IF(IFAIL.EQ.0)THEN
              IF(RTEMP.GE.0.0D0)THEN
                NINITSCHFAC=RTEMP
                IMESS=IMESS+1
                WRITE(IREC,327,ERR=9000) NINITSCHFAC
327             FORMAT('       Set INITSCHFAC to ',1pg11.5)
              END IF
            END IF
          END IF
        ELSE IF(CLINE(LW(1):RW(1)).EQ.'nmulschfac')THEN
          IF(IPRED.NE.0)THEN
            CALL DREALRD(IFAIL,CLINE(LW(2):RW(2)),RTEMP)
            IF(IFAIL.EQ.0)THEN
              IF(RTEMP.GT.1.0D0)THEN
                NMULSCHFAC=RTEMP
                IMESS=IMESS+1
                WRITE(IREC,328,ERR=9000) NMULSCHFAC
328             FORMAT('       Set MULSCHFAC to ',1pg11.5)
              END IF
            END IF
          END IF
        ELSE IF(CLINE(LW(1):RW(1)).EQ.'nsearch')THEN
          IF(IPRED.NE.0)THEN
            CALL INTRD(IFAIL,CLINE(LW(2):RW(2)),ITEMP)
            IF(IFAIL.EQ.0)THEN
              IF(ITEMP.GE.1)THEN
                NNSEARCH=ITEMP
                IMESS=IMESS+1
                WRITE(IREC,329,ERR=9000)NNSEARCH
329             FORMAT('       Set NSEARCH to ',I2)
              END IF
            END IF
          END IF
        ELSE IF(CLINE(LW(1):RW(1)).EQ.'hold')THEN
          CALL LINSPL(IFAIL,3,LW,RW,CLINE)
          IF(IFAIL.NE.0) GO TO 200
          IF(CLINE(LW(2):RW(2)).EQ.'parameter') THEN
            TPAR=CLINE(LW(3):RW(3))
            CALL WHICH1(IFAIL,NPAR,IPAR,APAR,TPAR)
            IF(IFAIL.NE.0) GO TO 200
            PHOLD(IPAR)=1
            IMESS=IMESS+1
            WRITE(IREC,340,ERR=9000) TPAR(1:LEN_TRIM(TPAR))
340         FORMAT('       Hold parameter "',a,'" at present value.')
          ELSE IF(CLINE(LW(2):RW(2)).EQ.'group') THEN
            TPAR=CLINE(LW(3):RW(3))
            CALL WHICH1(IFAIL,NPARGP,IGP,AGROUP,TPAR)
            IF(IFAIL.NE.0) GO TO 200
            CALL LINSPL(IFAIL,5,LW,RW,CLINE)
            IF(IFAIL.NE.0) GO TO 200
            CALL DREALRD(IFAIL,CLINE(LW(5):RW(5)),RTEMP)
            IF(IFAIL.NE.0) GO TO 200
            IF(RTEMP.LE.0.0D0) GO TO 200
            IF(CLINE(LW(4):RW(4)).EQ.'<')THEN
              GHOLD(IGP)=RTEMP
              IMESS=IMESS+1
              WRITE(IREC,350,ERR=9000) TPAR(1:LEN_TRIM(TPAR)),RTEMP
350           FORMAT('       Hold parameters in group "',a,'" if ',
     +        'sensitivity less than ',1PG11.5)
            ELSE IF(CLINE(LW(4):RW(4)).EQ.'lowest')THEN
              ITEMP=NINT(RTEMP)
              IF(ITEMP.LT.1) GO TO 200
              GHOLD(IGP)=-ITEMP
              IMESS=IMESS+1
              IF(ITEMP.EQ.1)THEN
                WRITE(IREC,355) TPAR(1:LEN_TRIM(TPAR))
355             FORMAT('       Hold least sensitive parameter ',
     +        'of group "',A,'" at present value.')
              ELSE
                CALL WRITINT(ATEMP,ITEMP)
                WRITE(IREC,360,ERR=9000) ATEMP(1:LEN_TRIM(ATEMP)),
     +          TPAR(1:LEN_TRIM(TPAR))
360             FORMAT('       Hold ',A,' least sensitive parameters ',
     +          'of group "',A,'" at present value.')
              END IF
            END IF
          ELSE IF(CLINE(LW(2):RW(2)).EQ.'eigenvector') THEN
            CALL LINSPL(IFAIL,5,LW,RW,CLINE)
            IF(IFAIL.NE.0) GO TO 200
            CALL DREALRD(IFAIL,CLINE(LW(3):RW(3)),RTEMP)
            IF(IFAIL.NE.0) GO TO 200
            ITEMP=NINT(RTEMP)
            IF(ITEMP.LT.1) GO TO 200
            IF(ITEMP.GT.NESPAR) GO TO 200
            IF(CLINE(LW(4):RW(4)).EQ.'highest')THEN
              CALL DREALRD(IFAIL,CLINE(LW(5):RW(5)),RTEMP)
              IF(IFAIL.NE.0) GO TO 200
              ITEMP1=NINT(RTEMP)
              IF(ITEMP1.LT.1) GO TO 200
              IF(ITEMP1.GT.NESPAR) GO TO 200
              EHOLD(ITEMP)=ITEMP1
              IMESS=IMESS+1
              CALL WRITINT(ATEMP,ITEMP)
              CALL WRITINT(ATEMP1,ITEMP1)
              IF(ITEMP.EQ.2)THEN
                ATEMPA='nd'
              ELSE IF(ITEMP.EQ.3)THEN
                ATEMPA='rd'
              ELSE
                ATEMPA='th'
              END IF
              IF(ITEMP1.EQ.1)THEN
                IF(ITEMP.EQ.1)THEN
                  WRITE(IREC,374,ERR=9000)
374               FORMAT('       Hold largest component of ',
     +            'eigenvector corresponding to largest',/,
     +            '            eigenvalue.')
                ELSE
                  WRITE(IREC,375,ERR=9000) ATEMP(1:LEN_TRIM(ATEMP)),
     +                          ATEMPA(1:LEN_TRIM(ATEMPA))
375               FORMAT('       Hold largest component of ',
     +            'eigenvector corresponding to ',A,A,' largest',/,
     +            '            eigenvalue.')
                END IF
              ELSE
                IF(ITEMP.EQ.1)THEN
                  WRITE(IREC,376,ERR=9000) ATEMP1(1:LEN_TRIM(ATEMP1))
376               FORMAT('       Hold ',A,' largest components ',
     +            'of eigenvector corresponding to',/,
     +            '            largest eigenvalue.')
                ELSE
                  WRITE(IREC,377,ERR=9000) ATEMP1(1:LEN_TRIM(ATEMP1)),
     +            ATEMP(1:LEN_TRIM(ATEMP)),ATEMPA(1:LEN_TRIM(ATEMPA))
377               FORMAT('       Hold ',A,' largest components ',
     +            'of eigenvector corresponding to ',A,A,/,
     +            '            largest eivenvalue.')
                END IF
              END IF
            END IF
          END IF
        END IF
        GO TO 200

500     CLOSE(UNIT=IGNF,IOSTAT=IERR)
        IF(IMESS.EQ.0)THEN
          IF(IINT.NE.0) WRITE(IREC,370,ERR=9000)
370       FORMAT('    File empty or in error: no user intervention.')
        END IF
        RETURN
9000    CALL STPERR(26,0,' ',0,' ',CLINE)
        JFAIL=1
        RETURN

#ifdef PESTMOD
        END SUBROUTINE HLDREAD
#else
        END
#endif



        subroutine delfile1(JFAIL,afile,cline)

C -- Subroutine DELFILE1 deletes a file. It is is similar to DEFFILE from
C    parpest.f; however it deletes a file using FORTRAN OPEN/CLOSE
C    functionality. It was introduced to overcome the LAHEY problem with
C    the "false" argument to the SYSTEM call. It is not, at this stage,
C    used with Parallel PEST for fear of unforeseen consequences. Also,
C    it does not take the same trouble as does DELFILE to ensure that the
C    file is actually deleted.

#ifdef INTEL
        USE COMMON_MOD, ONLY: FLENME
#endif
        use pestdata, only : idfl
        implicit none
        logical lexist
        integer iunit,JFAIL,itemp
#ifndef INTEL
        character*200 flenme
#endif
        character*(*) afile,cline
#ifdef UNIX
        integer unlink
#endif

#ifndef INTEL
        common /flenme/flenme
#endif

        JFAIL=0
        inquire(file=afile,exist=lexist)
        if(.not.lexist) return
        open(unit=idfl,file=afile,status='old',err=8000)
        close(unit=idfl,status='delete',err=8000)
        return

8000    continue
#ifdef LAHEY
#ifdef LF90
       open(unit=idfl,file=afile,form='unformatted',
     + access='transparent',status='old')
#else
       open(unit=idfl,file=afile,form='binary',status='old')
#endif
#else
       open(unit=idfl,file=afile,form='unformatted',
     + access='stream',status='old')
#endif
        close(unit=idfl,status='delete',err=8500)
        return

8500    continue
        open(unit=idfl,file=afile,status='old',form='unformatted',
     +  err=9000)
        close(unit=idfl,status='delete',err=9000)
        return

9000    continue

C -- Another attempt is made to delete the file, this time using a
C    system call.

#ifdef UNIX
10      itemp=unlink(trim(afile))
#else
10      call system('del "'//trim(afile)//'" > nul')
#endif
        call pestwait(100)
        inquire(file=afile,exist=lexist)
        if(.not.lexist) return

        flenme=afile
        call stperr(125,1,' ',0,' ',cline)
        JFAIL=1
        RETURN

#ifdef PESTMOD
        end subroutine delfile1
#else
        end
#endif


#ifdef AT
        subroutine copyfile(file1,file2)

c -- Subroutine copyfile copies one file to another. It is meant for
C    the aquaterra version of PEST where the .false. argument doesn't
c    work and hence copying is not done.

        use pestdata, only : idfl
        implicit none
        character*(*) file1,file2
        character*200 commandline

        open(unit=idfl,file='t###.bat')
        write(idfl,'(a)')'@echo off'
        commandline='copy '//file1(1:LEN_TRIM(file1))//' '
     +  //file2(1:LEN_TRIM(file2))//' > nul'
        write(idfl,'(a)') commandline(1:LEN_TRIM(commandline))
        close(unit=idfl)
        call system('t###.bat',.false.)

        return

#ifdef PESTMOD
        end subroutine copyfile
#else
        end
#endif
#endif



        subroutine write_pest_message(icode)

C -- Subroutine write_pest_message formats and writes a PEST error message.

        use pestdata, only: errmsg,errmsg_add,irec,irmr
        implicit none

        integer, intent(in)     :: icode
        logical                 :: lopened
        integer                 :: junit
        integer                 :: jend,i,nblc,ierr,j

        junit=6
1       continue
        if(icode.eq.0)then
          write(junit,*)
          write(junit,10)
10        FORMAT(1X,78('*'))
          WRITE(junit,12,iostat=ierr)
12        FORMAT(1x,'Error condition prevents continued PEST ',
     +    'execution:-',/)
        else
          write(junit,11)
11        FORMAT(1X,78('-'))
        end if

        j=0
        nblc=len_trim(errmsg)
5       jend=j+78
        if(jend.ge.nblc) go to 100
        do i=jend,j+1,-1
          if(errmsg(i:i).eq.' ') then
            write(junit,50,iostat=ierr) errmsg(j+1:i)
50          format(1x,a)
            j=i
            go to 5
          end if
        end do
        write(junit,50,iostat=ierr) errmsg(j+1:jend)
        j=jend
        go to 5
100     jend=nblc
        write(junit,50,iostat=ierr) errmsg(j+1:jend)
        if(errmsg_add.ne.' ')then
          write(junit,50,iostat=ierr) trim(errmsg_add)
        end if
        if(icode.eq.0)then
          write(junit,10)
          if(junit.eq.6)then
            inquire(unit=irec,opened=lopened)
            if(lopened)then
              junit=irec
              go to 1
            end if
          else if(junit.eq.irec)then
            inquire(unit=irmr,opened=lopened)
            if(lopened)then
              junit=irmr
              go to 1
            end if
          end if
        else
          write(junit,11)
          if(junit.eq.6)then
            inquire(unit=irmr,opened=lopened)
            if(lopened)then
              junit=irmr
              go to 1
            end if
          end if
        end if

        return

#ifdef PESTMOD
        end subroutine write_pest_message
#else
        end
#endif



        SUBROUTINE PEST_FILES_CLOSE

C -- Subroutine PEST_FILES_CLOSE ensures that all PEST files are closed.

        USE PESTDATA
        IMPLICIT NONE
        LOGICAL                 :: LOPENED

C -- JUPITER files are closed.

        INQUIRE(UNIT=J_PE,OPENED=LOPENED)
        IF(LOPENED) CLOSE(UNIT=J_PE)
        INQUIRE(UNIT=J_SS,OPENED=LOPENED)
        IF(LOPENED) CLOSE(UNIT=J_SS)
        INQUIRE(UNIT=IREC,OPENED=LOPENED)
        IF(LOPENED) CLOSE(UNIT=IREC)

C -- Now all other files are closed.

        INQUIRE(UNIT=ISDU,OPENED=LOPENED)
        IF(LOPENED) CLOSE(UNIT=ISDU)
        INQUIRE(UNIT=ISDA,OPENED=LOPENED)
        IF(LOPENED) CLOSE(UNIT=ISDA)
        INQUIRE(UNIT=IDFL,OPENED=LOPENED)
        IF(LOPENED) CLOSE(UNIT=IDFL)
        INQUIRE(UNIT=INST,OPENED=LOPENED)
        IF(LOPENED) CLOSE(UNIT=INST)
        INQUIRE(UNIT=IMMF,OPENED=LOPENED)
        IF(LOPENED) CLOSE(UNIT=IMMF)
        INQUIRE(UNIT=ISTP,OPENED=LOPENED)
        IF(LOPENED) CLOSE(UNIT=ISTP)
        INQUIRE(UNIT=IMIF,OPENED=LOPENED)
        IF(LOPENED) CLOSE(UNIT=IMIF)
        INQUIRE(UNIT=IRSF,OPENED=LOPENED)
        IF(LOPENED) CLOSE(UNIT=IRSF)
        INQUIRE(UNIT=IPFL,OPENED=LOPENED)
        IF(LOPENED) CLOSE(UNIT=IPFL)
        INQUIRE(UNIT=ISNS,OPENED=LOPENED)
        IF(LOPENED) CLOSE(UNIT=ISNS)
        INQUIRE(UNIT=IGNF,OPENED=LOPENED)
        IF(LOPENED) CLOSE(UNIT=IGNF)
        INQUIRE(UNIT=ISDF,OPENED=LOPENED)
        IF(LOPENED) CLOSE(UNIT=ISDF)
        INQUIRE(UNIT=ILSQ,OPENED=LOPENED)
        IF(LOPENED) CLOSE(UNIT=ILSQ)
        INQUIRE(UNIT=ICND,OPENED=LOPENED)
        IF(LOPENED) CLOSE(UNIT=ICND)
        INQUIRE(UNIT=IPCM,OPENED=LOPENED)
        IF(LOPENED) CLOSE(UNIT=IPCM)
        INQUIRE(UNIT=IRMR,OPENED=LOPENED)
        IF(LOPENED) CLOSE(UNIT=IRMR)
        INQUIRE(UNIT=IRMF,OPENED=LOPENED)
        IF(LOPENED) CLOSE(UNIT=IRMF)
        INQUIRE(UNIT=IJRF,OPENED=LOPENED)
        IF(LOPENED) CLOSE(UNIT=IJRF)
        INQUIRE(UNIT=IPTF,OPENED=LOPENED)
        IF(LOPENED) CLOSE(UNIT=IPTF)
        INQUIRE(UNIT=IREC,OPENED=LOPENED)
        IF(LOPENED) CLOSE(UNIT=IREC)
        INQUIRE(UNIT=IPPD,OPENED=LOPENED)
        IF(LOPENED) CLOSE(UNIT=IPPD)
        INQUIRE(UNIT=IPOD,OPENED=LOPENED)
        IF(LOPENED) CLOSE(UNIT=IPOD)
        INQUIRE(UNIT=IRRF,OPENED=LOPENED)
        IF(LOPENED) CLOSE(UNIT=IRRF)

        RETURN

#ifdef PESTMOD
        END SUBROUTINE PEST_FILES_CLOSE
#else
        END
#endif


        SUBROUTINE READ_JCO_FIRST_FILE(IFAIL)

        USE PESTDATA
        IMPLICIT NONE

        integer          :: ifail,jfail,itemp1,itemp2,nespar1,nxrow1,
     +                      i,j,icount,ies,irow
        double precision :: dtemp
        character*12     :: aapar
        character*20     :: aaobs

        ifail=0

        if(maxcompdim.gt.1)then
          write(errmsg,10)
10        format('PEST is not presently programmed for restart ',
     +    'with "/i" switch with MAXCOMPDIM set greater than 1.')
          go to 9800
        end if
        write(6,30) trim(jcofirstfile)
C        write(irec,30) trim(jcofirstfile)
30      format('    Reading Jacobian matrix from file ',a,'....')

        call ffopen(jfail,-irsf,'r',jcofirstfile,22,cline)
        if(jfail.ne.0)then
          write(errmsg,40) trim(jcofirstfile)
40        format('Cannot open Jacobian matrix file ',a,'.')
          go to 9800
        end if

        read(irsf,err=9350,end=9350)itemp1,itemp2
        if(itemp1.ge.0)then
          write(errmsg,60) trim(jcofirstfile)
60        format('Jacobian matrix file ',a,' uses old format; ',
     +    'use JCOTRANS utility to translate it to new format.')
          go to 9800
        end if
        nespar1=abs(itemp1)
        nxrow1=abs(itemp2)
        if((nespar1.ne.nespar).or.(nxrow1.lt.nobs))then
          write(errmsg,65) trim(jcofirstfile)
65        format('Dimensions of Jacobian matrix in file ',a,
     +    ' are inconsistent with present PEST case.')
          go to 9800
        end if
        do i=1,nespar
          do j=1,nobs
            x(j,i)=0.0d0
          end do
        end do
        read(irsf,err=9350,end=9350)icount
        do i=1,icount
          read(irsf,err=9350,end=9350) j,dtemp
          ies=(j-1)/nxrow1+1
          irow=j-(ies-1)*nxrow1
          if(irow.le.nobs) x(irow,ies)=dtemp
        end do
        ies=0
        do i=1,npar
          if(itrans(i).ge.0)then
            ies=ies+1
            read(irsf,err=9350,end=9350) aapar
            call lowcas(aapar)
            if(aapar.ne.apar(i))then
              write(errmsg,70) trim(jcofirstfile),trim(aapar),
     +        trim(apar(i))
70            format('Adjustable parameter names recorded in file ',a,
     +        ' are inconsistent with current PEST case; for example ',
     +        '"',a,'" in JCO file does not match "',a,'" in current ',
     +        'case.')
              go to 9800
            end if
          end if
        end do
        do irow=1,nobs
          read(irsf,err=9350,end=9350) aaobs
          call lowcas(aaobs)
          if(aaobs.ne.aobs(irow))then
            write(errmsg,80) trim(jcofirstfile),trim(aaobs),
     +      trim(aobs(irow))
80          format('Observation names recorded in file ',a,
     +      ' are inconsistent with current PEST case; for example ',
     +      '"',a,'" in JCO file does not match "',a,'" in current ',
     +      'case.')
            go to 9800
          end if
        end do
        close(unit=irsf)
        write(6,90) trim(jcofirstfile)
        write(irec,90) trim(jcofirstfile)
90      format('    Jacobian matrix read from file ',a)
        return

9350    write(errmsg,9360) trim(jcofirstfile)
9360    format('Error reading Jacobian matrix from file ',a,'.')
        go to 9800
9800    ifail=1
        return

        end




       subroutine x_update(ifail,nxrow,nobs,npar,nespar,itrans,
     + tmpval,pval,tmpobs,refobs,x,w1,w2)

       implicit none

       integer ifail,nxrow,npar,nespar,nobs
       integer itrans(npar)
       double precision tmpval(npar),pval(npar),
     + tmpobs(nxrow),refobs(nxrow),x(nxrow,nespar),
     + w1(nespar),w2(nxrow)

       integer ies,ipp,irow,iess
       double precision sum,dtemp,hth

       ifail=0

       ies=0
       sum=0.0d0
       do ipp=1,npar
         if(itrans(ipp).ge.0)then
           ies=ies+1
           dtemp=tmpval(ipp)-pval(ipp)
           if(itrans(ipp).eq.1)then
             dtemp=log10(tmpval(ipp)/pval(ipp))
           end if
           w1(ies)=dtemp
           sum=sum+dtemp*dtemp
         end if
       end do
       hth=sum
       if(hth.le.0.0d0)then
         ifail=1
         return
       else
         hth=1.0d0/hth
       end if

       do irow=1,nxrow
         sum=0.0d0
         ies=0
         iess=0
         do ipp=1,npar
           if(itrans(ipp).lt.0)then
             if(itrans(ipp).lt.-1000001) iess=iess+1
             go to 100
           end if
           ies=ies+1
           iess=iess+1
           sum=sum+x(irow,iess)*w1(ies)
100        continue
         end do
         w2(irow)=(tmpobs(irow)-refobs(irow)-sum)*hth
       end do

       do irow=1,nobs     ! Was nxrow - but prior info sensitivities do not need changing.
         ies=0
         iess=0
         do ipp=1,npar
           if(itrans(ipp).lt.0)then
             if(itrans(ipp).lt.-1000001) iess=iess+1
             go to 200
           end if
           ies=ies+1
           iess=iess+1
           x(irow,iess)=x(irow,iess)+w2(irow)*w1(ies)
200        continue
         end do
       end do

       return

       end


       subroutine sc_update(ifail,nxrow,nprior,nobs,npar,nespar,nobsgp,
     + itrans,nobgnm,oval,owght,irgp,sc,x)

       implicit none

       integer nxrow,nprior,nobs,npar,nobsgp,ifail,nespar
       integer nobgnm(nxrow),irgp(nobsgp),itrans(npar)
       double precision oval(nxrow),owght(nxrow),sc(nespar),
     + x(nxrow,nespar)

       integer ies,iess,ipp
       double precision sum1

C -- A new sc vector is calculated (but only for those adjustable parameters
C    for which the scale is already nonzero.

       ifail=0
       IES=0
       IESS=0
       DO 300 IPP=1,NPAR
         IF(ITRANS(IPP).LT.0) THEN
           IF(ITRANS(IPP).LT.-1000001) IESS=IESS+1
           GO TO 300
         END IF
         IES=IES+1
         IESS=IESS+1
         if(sc(ies).ne.0.0d0)then
           CALL OBJCLC(2,SC(IES),NXROW,NPRIOR,NOBS,X(1,IESS),OVAL,OWGHT,
     +     NOBGNM,NOBSGP,IRGP,SUM1)
           if(sc(ies).gt.0.0d0)then
             sc(ies)=1.0d0/sqrt(sc(ies))
           else
             ifail=1
             return
           end if
         end if
300    continue

       return

       end




        subroutine grad_update(ifail,nxrow,nobs,nprior,npar,nespar,
     +  nobsgp,itrans,x,owght,oval,refobs,grad,nobgnm,irgp,
     +  iboundscale,boundscale)

        implicit none

        integer ifail,nxrow,nobs,nprior,npar,nespar,nobsgp
        integer iboundscale
        integer itrans(npar),nobgnm(nobsgp),irgp(nobsgp)
        double precision x(nxrow,nespar),owght(nxrow),oval(nxrow),
     +  refobs(nxrow),grad(nespar)
        double precision boundscale(nespar)

        integer ies,iess,ipp,nestmp,irow
        double precision rtemp,grdnrm,sum1

        ifail=0
        IES=0
        IESS=0
        DO 150 IPP=1,NPAR
          IF(ITRANS(IPP).LT.0)THEN
            IF(ITRANS(IPP).LT.-1000001) IESS=IESS+1
            GO TO 150
          END IF
          IES=IES+1
          IESS=IESS+1
          RTEMP=0.0D0
          DO 170 IROW=1,NXROW
170       RTEMP=RTEMP+X(IROW,IESS)*OWGHT(IROW)*(OVAL(IROW)-REFOBS(IROW))
          GRAD(IES)=-2.0D0*RTEMP
          IF(IBOUNDSCALE.NE.0)THEN
            GRAD(IES)=GRAD(IES)*BOUNDSCALE(IESS)
          END IF
150     CONTINUE
        NESTMP=IES
        IF(NESTMP.EQ.0) THEN
          ifail=1
          return
        end if

        CALL OBJCLC1(GRDNRM,NESTMP,GRAD)
        IF(GRDNRM.LE.0.0D0) THEN
          ifail=1
          return
        end if

        return

        end


        subroutine rhs_update(nxrow,npar,nespar,svdmode,itrans,rhs,
     +  x,oval,refobs,owght,sc)

        implicit none
        integer npar,nespar,svdmode,nxrow
        integer itrans(npar)
        double precision rhs(nespar),x(nxrow,nespar),oval(nxrow),
     +  refobs(nxrow),owght(nxrow),sc(nespar)

        integer ies,iess,ipp,irow
        double precision rtemp

        IES=0
        IESS=0
        DO 250 IPP=1,NPAR
        IF(ITRANS(IPP).LT.0) THEN
          IF(ITRANS(IPP).LT.-1000001) IESS=IESS+1
          GO TO 250
        END IF
        IES=IES+1
        IESS=IESS+1
        RTEMP=0.0D0
        DO 270 IROW=1,NXROW
270     RTEMP=RTEMP+X(IROW,IESS)*OWGHT(IROW)*(OVAL(IROW)-REFOBS(IROW))
        IF(SVDMODE.EQ.0)THEN
          RHS(IES)=RTEMP*SC(IES)
        ELSE IF((SVDMODE.EQ.1).OR.(SVDMODE.EQ.2))THEN
          RHS(IES)=RTEMP
        END IF
250     CONTINUE

        return
        end


        subroutine lhs_update(nxrow,npar,nespar,svdmode,nolambda,
     +  itrans,x,owght,sc,lhs,workvec3dim,workvec3)

        implicit none

        integer nxrow,npar,nespar,svdmode,nolambda,workvec3dim
        integer itrans(npar)
        double precision x(nxrow,nespar),owght(nxrow),sc(nespar),
     +  lhs(nespar,nespar),workvec3(workvec3dim)

        integer ies,iess,ipp,jes,jess,irow,i,nestmp
        double precision  rtemp

        IES=0
        IESS=0
        DO 400 IPP=1,NPAR
          IF(ITRANS(IPP).LT.0) THEN
            IF(ITRANS(IPP).LT.-1000001) IESS=IESS+1
            GO TO 400
          END IF
          IES=IES+1
          IESS=IESS+1
          JES=0
          JESS=0
          DO 390 I=1,IPP
          IF(ITRANS(I).LT.0) THEN
            IF(ITRANS(I).LT.-1000001) JESS=JESS+1
            GO TO 390
          END IF
          JES=JES+1
          JESS=JESS+1
          RTEMP=0.0D0
          DO 360 IROW=1,NXROW
360       RTEMP=RTEMP+OWGHT(IROW)*X(IROW,IESS)*X(IROW,JESS)
          IF(SVDMODE.EQ.0)THEN
             LHS(JES,IES)=RTEMP*SC(IES)*SC(JES)
          ELSE IF(SVDMODE.EQ.1)THEN
             LHS(JES,IES)=RTEMP
          END IF
390       CONTINUE
400     CONTINUE
        NESTMP=IES
        DO IES=1,NESTMP-1
          DO JES=IES+1,NESTMP
            LHS(JES,IES)=LHS(IES,JES)
          END DO
        END DO
        IF((SVDMODE.EQ.1).AND.(NOLAMBDA.EQ.0))THEN
          DO IES=1,NESTMP
            WORKVEC3(IES)=LHS(IES,IES)
          END DO
        END IF

        return

        end




        SUBROUTINE JCOWRITE(NPAR,NESPAR,NXROW,IRSF,X,ITRANS,APAR,
     +  AOBS,REFOBS,OVAL,PVAL,SCALE,OFFSET)

c -- Subroutine JCOWRITE writes a JCO file.

        USE PESTDATA,  ONLY: XDIM1,XDIM2,MAXCOMPDIM,NCOMPDIM,
     +                       XC,IXC,ISTART_C,IFOUND_C,
     +                       IXPR,IXPC,NPR_NONZERO,COMPRESS_EASY,
     +                       XPVAL,PI_PAR,NOBS,NPRIOR
        IMPLICIT NONE
        INTEGER NPAR,NXROW,NESPAR,IRSF
        INTEGER ICOUNT,I,J,IPP,IOBS
        INTEGER ITEMP
        INTEGER ITEMP_IXP,ICOL
        INTEGER ITRANS(NPAR)
        CHARACTER*(*) APAR(NPAR)
        CHARACTER*(*) AOBS(NXROW)
        DOUBLE PRECISION DTEMP
        DOUBLE PRECISION REFOBS(NXROW),OVAL(NXROW)
        DOUBLE PRECISION PVAL(NPAR),SCALE(NPAR),OFFSET(NPAR)
        DOUBLE PRECISION X(XDIM1,XDIM2)
#ifdef INTEL
        INTEGER (8) :: I8
#else
        INTEGER     :: I8
#endif

        WRITE(IRSF) -NESPAR,-NXROW
        IF(MAXCOMPDIM.LE.1)THEN
          ICOUNT=0
          DO I=1,NESPAR
            DO J=1,NXROW
              IF(X(J,I).NE.0.0D0) ICOUNT=ICOUNT+1
            END DO
          END DO
          WRITE(IRSF) ICOUNT
          DO I=1,NESPAR
            DO J=1,NXROW
              IF(X(J,I).NE.0.0D0)THEN
                ICOUNT=(I-1)*NXROW+J
                WRITE(IRSF)ICOUNT,X(J,I)
              END IF
            END DO
          END DO
        ELSE
          ITEMP=NCOMPDIM                      ! Really should be i8 but would cause incompatibility with other versions.
          WRITE(IRSF) ITEMP
          IF(COMPRESS_EASY.EQ.1)THEN
            ITEMP_IXP=1
            DO I=1,NESPAR
              I8=(I-1)*NOBS+PI_PAR(I)
              DO J=1,NOBS
                I8=I8+1
#ifdef INTEL
                ITEMP=I8
#ifdef SINGLE_XC
                DTEMP=XC(I8)
                WRITE(IRSF) ITEMP,DTEMP
#else
                WRITE(IRSF) ITEMP,XC(ITEMP)
#endif
#else
#ifdef SINGLE_XC
                DTEMP=XC(I8)
                WRITE(IRSF) I8,DTEMP
#else
                WRITE(IRSF) I8,XC(ITEMP)
#endif
#endif
              END DO
              IF(NPRIOR.NE.0)THEN
401             CONTINUE
                IF(ITEMP_IXP.LE.NPR_NONZERO)THEN
                  ICOL=IXPC(ITEMP_IXP)
                  IF(ICOL.EQ.I)THEN
                    I8=(I-1)*NXROW+IXPR(ITEMP_IXP)
#ifdef INTEL
                    ITEMP=I8
                    WRITE(IRSF)ITEMP,XPVAL(ITEMP_IXP)
#else
                    WRITE(IRSF)I8,XPVAL(ITEMP_IXP)
#endif
                    ITEMP_IXP=ITEMP_IXP+1
                    GO TO 401
                  END IF
                END IF
              END IF
            END DO
          ELSE
            DO I8=1,NCOMPDIM
#ifdef INTEL
              ITEMP=IXC(I8)
#ifdef SINGLE_XC
              DTEMP=XC(I8)
              WRITE(IRSF) ITEMP,DTEMP
#else
              WRITE(IRSF) ITEMP,XC(I8)
#endif
#else
#ifdef SINGLE_XC
              DTEMP=XC(I8)
              WRITE(IRSF) IXC(I8),DTEMP
#else
              WRITE(IRSF) IXC(I8),XC(I8)
#endif
#endif
            END DO
          END IF
        END IF
        DO 1235 IPP=1,NPAR
          IF((ITRANS(IPP).EQ.0).OR.(ITRANS(IPP).EQ.1).OR.
     +       (ITRANS(IPP).LE.-1000001))THEN
             WRITE(IRSF) APAR(IPP)
          END IF
1235    CONTINUE
        DO 1236 IOBS=1,NXROW
          WRITE(IRSF) AOBS(IOBS)
1236    CONTINUE
        DO IOBS=1,NXROW
          WRITE(IRSF) REFOBS(IOBS),OVAL(IOBS)
        END DO
        DO IPP=1,NPAR
          WRITE(IRSF) PVAL(IPP),ITRANS(IPP),SCALE(IPP),OFFSET(IPP)
        END DO
        CLOSE(UNIT=IRSF)

        RETURN

        END


        SUBROUTINE REIWRITE(IFAIL,IGNF,IOPT,IREG,NXROW,IPRED,NOBS,
     +  NOBSGP,NUMCOV,OWGHT,NOBGNM,NOBSCOV,AOBS,OVAL,MODVAL_O,OBGNME,
     +  COVGP,IOPTFLAG)

C -- Subroutine REIWRITE writes an REI file.

        IMPLICIT NONE

        INTEGER   IFAIL,IGNF,IOPT,IREG,NXROW,IPRED,NOBS,NUMCOV,NOBSGP
        INTEGER   NOBGNM(NXROW),NOBSCOV(NUMCOV),COVGP(NUMCOV)
        DOUBLE PRECISION OWGHT(NXROW),OVAL(NXROW),MODVAL_O(NXROW)
        CHARACTER*(*) AOBS(NXROW),OBGNME(NOBSGP)
        INTEGER   IOPTFLAG

        INTEGER   I,ITEMP,JCOV,IERR
        DOUBLE PRECISION RTEMP
        CHARACTER*20 ATEMP3

        IFAIL=0
        IF(IOPTFLAG.EQ.0)THEN
          WRITE(IGNF,6849) IOPT-1
6849      FORMAT(/,' MODEL OUTPUTS AT END OF OPTIMISATION ',
     +    'ITERATION NO.',I4,':-')
        ELSE
          WRITE(IGNF,6849) IOPT
        END IF
        IF(NUMCOV.NE.0)THEN
          WRITE(IGNF,6848)
6848      FORMAT(/,' Note that model outputs, measurements and ',
     +    'residuals are "rotated" for those ',/,' observation ',
     +    'groups for which a covariance matrix is supplied.')
        END IF
        IF(IREG.NE.0)THEN
          WRITE(IGNF,6847)
6847      FORMAT(/,' Note that weights have been adjusted in ',
     +    'accordance with regularisation target objective function.')
        END IF
        WRITE(IGNF,*)
        WRITE(IGNF,*)
        WRITE(IGNF,6851,ERR=9300)
6851    FORMAT(1x,'Name',T23,'Group',T38,'Measured',T55,'Modelled',
     +  T72,'Residual',t89,'Weight')
        DO 6852 I=1,NXROW
          IF((IPRED.NE.0).AND.(I.EQ.NOBS)) GO TO 6852
          IF(OWGHT(I).LE.0.0D0) THEN
            RTEMP=0.0D0
          ELSE
            RTEMP=SQRT(OWGHT(I))
          END IF
          ITEMP=NOBGNM(I)
          IF(ITEMP.EQ.-1)THEN
            ATEMP3='pr_info'
          ELSE
            ATEMP3=OBGNME(ITEMP)
          END IF
          IF(NUMCOV.GT.0)THEN
            DO 6853 JCOV=1,NUMCOV
              IF(NOBSCOV(JCOV).EQ.0)GO TO 6853
              IF(ITEMP.EQ.COVGP(JCOV))THEN
                WRITE(IGNF,6855,ERR=9300)AOBS(I)(1:LEN_TRIM(AOBS(I))),
     +          ATEMP3(1:LEN_TRIM(ATEMP3)),OVAL(I),MODVAL_O(I),
     +          OVAL(I)-MODVAL_O(I)
6855            FORMAT(1X,A,T23,A,T36,1PG14.7,T53,1PG14.7,T70,1PG14.7,
     +          T87,'  Cov. Mat.')
                GO TO 6852
              END IF
6853        CONTINUE
          END IF
          WRITE(IGNF,6866,ERR=9300) AOBS(I)(1:LEN_TRIM(AOBS(I))),
     +    ATEMP3(1:LEN_TRIM(ATEMP3)),OVAL(I),MODVAL_O(I),
     +    OVAL(I)-MODVAL_O(I),RTEMP
6866      FORMAT(1X,A,T23,A,T36,1PG14.7,T53,1PG14.7,T70,1PG14.7,
     +    T87,1PG14.7)
6852    CONTINUE
        CLOSE(UNIT=IGNF)
        RETURN

9300    IFAIL=1
        CLOSE(UNIT=IGNF,IOSTAT=IERR)

        RETURN
        END


       logical function equals(r1,r2)

         double precision,  intent(in)      :: r1,r2
         double precision                   :: rtemp

         rtemp=abs(7.0*spacing(r1))
         if(abs(r1-r2).lt.rtemp)then
           equals=.true.
         else
           equals=.false.
         end if

       end



        subroutine intread(ifail,cline,itemp)

C -- Subroutine INTREAD reads a real number from a string.

        integer ifail
        integer itemp
        character*6 afmt
        character*(*) cline

        ifail=0
        if(cline.eq.' ') go to 100
        afmt='(i   )'
        write(afmt(3:5),'(i3)') len(cline)
        read(cline,afmt,err=100) itemp
        return

100     ifail=1
        return
        end




        SUBROUTINE DREALREAD(IFAIL,CLINE,RTEMP)

C -- Subroutine DREALREAD reads a real number from a string.

        INTEGER IFAIL
        double precision RTEMP
        CHARACTER*8 AFMT
        CHARACTER*(*) CLINE

        IFAIL=0
        IF(CLINE.EQ.' ') GO TO 100
        AFMT='(F   .0)'
        WRITE(AFMT(3:5),'(I3)') LEN(CLINE)
        READ(CLINE,AFMT,ERR=100) RTEMP
        RETURN

100     IFAIL=1
        RETURN
        END



        SUBROUTINE WRITE_JACOB_ELEMS(IUNIT)

        USE PESTDATA, ONLY: PI_PAR,IXPC,IXPR,NPR_NONZERO,XPVAL,
     +                      NESPAR,PI_PAR,NOBS,XC,NPRIOR,NXROW,
     +                      NCOMPDIM

        IMPLICIT NONE

        INTEGER IUNIT
        INTEGER ITEMP,I,J,ICOL
        DOUBLE PRECISION DTEMP
#ifdef INTEL
        INTEGER (8) I8,ITEMP_IXP
#else
        INTEGER     I8,ITEMP_IXP
#endif

        ITEMP=NCOMPDIM                      ! Really should be i8 but would cause incompatibility with other versions.
        WRITE(IUNIT) ITEMP
        ITEMP_IXP=1
        DO I=1,NESPAR
          I8=(I-1)*NOBS+PI_PAR(I)
          DO J=1,NOBS
            I8=I8+1
#ifdef INTEL
            ITEMP=I8
#ifdef SINGLE_XC
            DTEMP=XC(I8)
            WRITE(IUNIT) ITEMP,DTEMP
#else
            WRITE(IUNIT) ITEMP,XC(ITEMP)
#endif
#else
#ifdef SINGLE_XC
            DTEMP=XC(I8)
            WRITE(IUNIT) I8,DTEMP
#else
            WRITE(IUNIT) I8,XC(ITEMP)
#endif
#endif
          END DO
          IF(NPRIOR.NE.0)THEN
401         CONTINUE
            IF(ITEMP_IXP.LE.NPR_NONZERO)THEN
              ICOL=IXPC(ITEMP_IXP)
              IF(ICOL.EQ.I)THEN
                I8=(I-1)*NXROW+IXPR(ITEMP_IXP)
#ifdef INTEL
                ITEMP=I8
                WRITE(IUNIT)ITEMP,XPVAL(ITEMP_IXP)
#else
                WRITE(IUNIT)I8,XPVAL(ITEMP_IXP)
#endif
                ITEMP_IXP=ITEMP_IXP+1
                GO TO 401
              END IF
            END IF
          END IF
        END DO

        RETURN
        END



        SUBROUTINE READ_JACOB_ELEMS(IFAIL,IUNIT)

        USE PESTDATA, ONLY: PI_PAR,IXPC,IXPR,NPR_NONZERO,XPVAL,
     +                      NESPAR,PI_PAR,NOBS,XC,NPRIOR,NXROW,
     +                      NCOMPDIM,ERRMSG

        IMPLICIT NONE

        INTEGER   IFAIL,IUNIT
        INTEGER   I,J,ICOL,ITEMP
        DOUBLE PRECISION DTEMP

#ifdef INTEL
        INTEGER (8)  ITEMP_IXP,I8
#else
        INTEGER      ITEMP_IXP,I8
#endif

        IFAIL=0

        READ(IUNIT,ERR=9300,END=9300) ITEMP
        NCOMPDIM=ITEMP
        ITEMP_IXP=1
        DO I=1,NESPAR
          I8=(I-1)*NOBS+PI_PAR(I)
          DO J=1,NOBS
            I8=I8+1
#ifdef SINGLE_XC
            READ(IUNIT,ERR=9300,END=9300) ITEMP,DTEMP
            XC(I8)=DTEMP
#else
            READ(IUNIT,ERR=9300,END=9300) ITEMP,XC(I8)
#endif
          END DO
          IF(NPRIOR.NE.0)THEN
401         CONTINUE
            IF(ITEMP_IXP.LE.NPR_NONZERO)THEN
              ICOL=IXPC(ITEMP_IXP)
              IF(ICOL.EQ.I)THEN
#ifdef SINGLE_XC
                READ(IUNIT,ERR=9300,END=9300)ITEMP,DTEMP
#else
                READ(IUNIT,ERR=9300,END=9300)ITEMP,DTEMP
#endif
                ITEMP_IXP=ITEMP_IXP+1
                GO TO 401
              END IF
            END IF
          END IF
        END DO
        GO TO 9999

9300    WRITE(ERRMSG,9310)
9310    FORMAT('Error reading Jacobian matrix from restart ',
     +  'file.')
        IFAIL=1

9999    CONTINUE
        RETURN

        END



        SUBROUTINE READ_PARVAL_FILE(JFAIL,INFILE)

C -- Subroutine READ_PARVAL_FILE reads parameter values from a parameter value file.

        USE PESTDATA, ONLY: NPAR,IPFL,APAR,PVAL,ITRANS,ERRMSG,CLINE
        IMPLICIT NONE

        INTEGER, INTENT(OUT)       :: JFAIL
        CHARACTER*(*), INTENT(IN)  :: INFILE

        INTEGER                    :: IERR,ILINE,IPAR,IFAIL
        INTEGER                    :: LW(6),RW(6)
        DOUBLE PRECISION           :: DTEMP,VALUE
        CHARACTER*10               :: ALINE
        CHARACTER*12               :: AAPAR

        JFAIL=0
        OPEN(UNIT=IPFL,FILE=INFILE,STATUS='OLD',IOSTAT=IERR)
        IF(IERR.NE.0)THEN
          WRITE(ERRMSG,10) TRIM(INFILE)
10        FORMAT('Cannot open file ',a,' to read parameter values.')
          GO TO 9890
        END IF
        ILINE=0
        ILINE=ILINE+1
        READ(IPFL,'(A)',ERR=9000,END=9100) CLINE
        CALL LINSPL(IFAIL,2,LW,RW,CLINE)
        IF(IFAIL.NE.0)THEN
          WRITE(ERRMSG,20) TRIM(INFILE)
20        FORMAT('Two entries expected on first line of parameter ',
     +    'value file ',a,'.')
          GO TO 9890
        END IF
        CALL LOWCAS(CLINE)
        IF((CLINE(LW(1):RW(1)).NE.'single').AND.
     +     (CLINE(LW(1):RW(1)).NE.'double')) GO TO 9200
        IF((CLINE(LW(2):RW(2)).NE.'point').AND.
     +     (CLINE(LW(2):RW(2)).NE.'nopoint')) GO TO 9200

	IPAR=1
	PVAL=-1.1E35     ! An array


250	ILINE=ILINE+1
	READ(IPFL,'(A)',ERR=9000,END=500) CLINE
	IF(CLINE.EQ.' ') GO TO 250
	CALL LINSPL(IFAIL,4,LW,RW,CLINE)
	IF(IFAIL.LT.0) GO TO 250
	IF(IFAIL.NE.0)THEN
	  CALL WRITINT(ALINE,ILINE)
	  WRITE(ERRMSG,255) TRIM(ALINE),TRIM(INFILE)
255	  FORMAT('Insufficient entries on line ',A,' of ',
     +    'parameter value file ',A,'.')
	  GO TO 9890
	END IF
	CALL LOWCAS(CLINE)
	IF(RW(1)-LW(1).GT.11)THEN
	  CALL WRITINT(ALINE,ILINE)
	  WRITE(ERRMSG,260) TRIM(ALINE),TRIM(INFILE)
260	  FORMAT('Parameter name greater than 12 characters',
     +    ' at line ',A,' of parameter value file ',A,'.')
	  GO TO 9890
	END IF
	AAPAR=CLINE(LW(1):RW(1))
	CALL DREALRD(IFAIL,CLINE(LW(2):RW(2)),VALUE)
	IF(IFAIL.EQ.0) CALL DREALRD(IFAIL,CLINE(LW(3):RW(3)),
     +  DTEMP)
	IF(IFAIL.EQ.0) CALL DREALRD(IFAIL,CLINE(LW(4):RW(4)),
     +  DTEMP)
	IF(IFAIL.NE.0)THEN
	  CALL WRITINT(ALINE,ILINE)
	  WRITE(ERRMSG,280) TRIM(ALINE),TRIM(INFILE)
280	  FORMAT('Error encountered in reading line ',A,
     +    ' of parameter value file ',A,'.')
	  GO TO 9890
	END IF
        CALL WHICH1(IFAIL,NPAR,IPAR,APAR,AAPAR)
        IF(IFAIL.NE.0) GO TO 250
	IF(PVAL(IPAR).GT.-1.0E35)THEN
	  WRITE(ERRMSG,290) TRIM(AAPAR),TRIM(INFILE)
290       FORMAT('Parameter "',a,'" cited more than once in ',
     +    'parameter value file ',A,'.')
          GO TO 9890
        END IF
        PVAL(IPAR)=VALUE
        IF(ITRANS(IPAR).EQ.1)THEN
          IF(PVAL(IPAR).LE.0.0)THEN
            WRITE(ERRMSG,300) TRIM(AAPAR),TRIM(INFILE)
300         FORMAT('Parameter "',A,'" is log-transformed in PEST ',
     +      'control file but has a zero or negative value in ',
     +      'parameter value file ',A,'.')
            GO TO 9890
          END IF
        END IF
        GO TO 250

500     CONTINUE
        DO IPAR=1,NPAR
          IF(PVAL(IPAR).LT.-1.0E35)THEN
            WRITE(ERRMSG,510) TRIM(APAR(IPAR)),TRIM(INFILE)
510         FORMAT('Parameter "',a,'" cited in PEST control file ',
     +      'not found in parameter value file ',a,'.')
            GO TO 9890
          END IF
        END DO
        GO TO 9900

9000    CONTINUE
        CALL WRITINT(ALINE,ILINE)
        WRITE(ERRMSG,9010) TRIM(ALINE),TRIM(INFILE)
9010    FORMAT('Error encountered reading line ',a,' of parameter',
     +  'value file ',a,'.')
        GO TO 9890

9100    WRITE(ERRMSG,9110) TRIM(INFILE)
9110    FORMAT('Premature end encountered to parameter value file '
     +  ,a,'.')
        GO TO 9890

9200    WRITE(ERRMSG,9210) TRIM(INFILE)
9210    FORMAT('Entries on line 1 of file ',A,' indicate that it is ',
     +  'not a parameter value file.')
        GO TO 9890

9890    JFAIL=1
9900    RETURN
        END


       integer function nextunit()

	logical::lopen

	do nextunit=10,100
	  inquire(unit=nextunit,opened=lopen)
	  if(.not.lopen) return
	end do
	write(6,10)
10      format(' *** No more unit numbers to open files ***')
	stop

        end



       subroutine read_rrf_obs(ifail,npar,nobs,apar,pval,aobs,oval,
     + rrffile,cline,errmsg)

C -- This subroutine reads model outputs corresponding to a set of observations
C    from a PEST_HP run results file.

       implicit none
       integer ifail,npar,nobs
       double precision pval(npar),oval(nobs)
       character*12 apar(npar)
       character*20 aobs(nobs)
       character*(*) rrffile,cline,errmsg

       integer nextunit
       integer iunit,ierr,iline,iflag,i
       integer ipar,iobs
       integer itemp1,itemp2
       integer lw(10),rw(10)
       double precision ppval
       character*10 aline
       character*12 aapar
       character*20 aaobs

C -- Initialization

       ifail=0

C -- Find a unit to open the file.

       iunit=nextunit()

! -- Open the file.

20     continue
       open(unit=iunit,file=rrffile,status='old',action='read',
     + iostat=ierr)
       if(ierr.ne.0)then
         write(errmsg,30) trim(rrffile)
30       format('Cannot open file ',a,'.')
         go to 9890
       end if

! -- The header to the file is read.

       iline=1
       read(iunit,'(a)',err=9000,end=9020) cline
       call lowcas(cline)
       if(cline.ne.'* case dimensions')then
         write(errmsg,40) trim(rrffile)
40       format('Line 1 of file ',a,' expected to be ',
     +   '"* case dimensions".')
         go to 9890
       end if
       iline=iline+1
       read(iunit,'(a)',err=9000,end=9020) cline
       call linspl(ifail,2,lw,rw,cline)
       if(ifail.ne.0) go to 9000
       call intread(ifail,cline(lw(1):rw(1)),itemp1)
       if(ifail.ne.0) go to 9000
       call intread(ifail,cline(lw(2):rw(2)),itemp2)
       if(ifail.ne.0) go to 9000
       if((itemp1.ne.npar).or.(itemp2.ne.nobs)) go to 9060
       iline=iline+1
       read(iunit,'(a)',err=9000,end=9020) cline
       do ipar=1,npar
         iline=iline+1
         read(iunit,'(a)',err=9000,end=9020) cline
         if(cline.eq.' ') go to 9000
         call linspl(ifail,1,lw,rw,cline)
         aapar=cline(lw(1):rw(1))
         call lowcas(aapar)
         if(aapar.ne.apar(ipar)) go to 9061
       end do
       iline=iline+1
       read(iunit,'(a)',err=9000,end=9020) cline
       do iobs=1,nobs
         iline=iline+1
         read(iunit,'(a)',err=9000,end=9020) cline
         if(cline.eq.' ') go to 9000
         call linspl(ifail,1,lw,rw,cline)
         aaobs=cline(lw(1):rw(1))
         call lowcas(aaobs)
         if(aaobs.ne.aobs(iobs)) go to 9062
       end do

! -- We now look for an appropriate parameter set.

       do
         iline=iline+1
         read(iunit,'(a)',err=9000,end=80) cline
         if(cline.ne.'* parameter set index')then
           call writint(aline,iline)
           write(errmsg,50) trim(aline),trim(rrffile)
50         format('"* parameter set index" expected at line ',a,
     +     ' of file ',a,'.')
           go to 9890
         end if
         do i=1,3
           iline=iline+1
           read(iunit,'(a)',err=9000,end=80) cline
         end do
         iline=iline+1
         read(iunit,'(a)',err=9000,end=80) cline
         if(cline.ne.'* parameter values')then
           call writint(aline,iline)
           write(errmsg,65)trim(aline),trim(rrffile)
65         format('Line ',a,' of file ',a,' should be ',
     +     '"* parameter values".')
           go to 9890
         end if
         iflag=1
         do ipar=1,npar
           iline=iline+1
           read(iunit,'(a)',err=9000,end=80) cline
           if(cline.eq.' ') go to 9000
           if(iflag.eq.1)then
             call linspl(ifail,1,lw,rw,cline)
             call drealrd(ifail,cline(lw(1):rw(1)),ppval)
             if(ifail.ne.0) go to 9000
             if(abs(ppval-pval(ipar)).gt.
     +         (abs(ppval)+abs(pval(ipar)))*1.0e-4) iflag=0    ! arbitrary
           end if
         end do
         iline=iline+1
         read(iunit,'(a)',err=9000,end=80) cline
         if(cline.ne.'* model output values')then
           call writint(aline,iline)
           write(errmsg,66)trim(aline),trim(rrffile)
66         format('Line ',a,' of file ',a,' should be ',
     +     '"* model output values".')
           go to 9890
         end if
         do iobs=1,nobs
           iline=iline+1
           read(iunit,'(a)',err=9000,end=80) cline
           if(cline.eq.' ') go to 9000
           if(iflag.eq.1)then
             call linspl(ifail,1,lw,rw,cline)
             call drealrd(ifail,cline(lw(1):rw(1)),oval(iobs))
             if(ifail.ne.0) go to 9000
             if(oval(iobs).lt.-1.0d35)then
               call writint(aline,iline)
               write(errmsg,70) trim(rrffile),trim(aline)
70             format('A matching initial parameter set was found ',
     +         'in file ',a,'. However at least one model output ',
     +         'value (see line ',a,' of this file) indicates ',
     +         'model run failure.')
               go to 9890
             end if
           end if
         end do
         if(iflag.eq.1)then
           iflag=2
           go to 80
         end if
       end do
80     continue
       close(unit=iunit)
       if(iflag.ne.2)then
         write(errmsg,90) trim(rrffile)
90       format('Parameter set corresponding to initial PEST ',
     +   'parameter set not found in file ',a,
     +   '. Hence PEST cannot read outcomes ',
     +   'of initial model run from this file.')
         go to 9890
       end if
       go to 9990

9000   call writint(aline,iline)
       write(errmsg,9010) trim(aline),trim(rrffile)
9010   format('Error reading line ',a,' of file ',a,'.')
       go to 9890

9020   write(errmsg,9030) trim(rrffile)
9030   format('Unexpected end encountered to file ',a,' while ',
     + 'reading header to this file.')
       go to 9890

9060   write(errmsg,9070)
9070   format('Run results file is not compatible with PEST control ',
     + 'file.')
       go to 9890

9061   call writint(aline,iline)
       write(errmsg,9071) trim(aline),trim(rrffile)
9071   format('Run results file is not compatible with PEST control ',
     + 'file - parameter mismatch at line ',a,' of file ',a,'.')
       go to 9890

9062   call writint(aline,iline)
       write(errmsg,9072) trim(aline),trim(rrffile)
9072   format('Run results file is not compatible with PEST control ',
     + 'file - observation mismatch at line ',a,' of file ',a,'.')
       go to 9890

9890   continue
       ifail=1

9990   continue
       return
       end






