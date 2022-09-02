

        SUBROUTINE DERCLC(IFAIL,DERFLAG,NSCALL,MMCALL,NSCALLP,MMCALLP,
     +  SOPDIM,N2OR3,IPP,NPAR,NOBS,
     +  NPARGP,ASIZE,NINSTR,NINFLE,NOUTFL,NUML,NBLBMX,PRECIS,NOPNT,
     +  REFPAR,LBOUND,UBOUND,ITRANS,SCALE,OFFSET,IPARGP,TPAR,NW,PWORD,
     +  APAR,AOBS,DERVAL,IDBND,IDERIV,DERBND,J2OR3,I3CALC,DERMUL,
     +  REFOBS,TMOBS,TTMOBS,LCINS,LL,OBSN1,OBSN2,IIOBS,INSFLE,TMPFL,
     +  INFLE,OUTFLE,PARDEL,MRKDEL,A,CLINE,BUF,MODFLE,
     +  NRUN,IRUN,ICALL,
     +  INCPAR,IMSFLE,IIRUN,NORMRETURN,XCOLDIM,XCOL,SPLITFLAG)

#ifdef INTEL
        USE COMMON_MOD, ONLY: ISTOP
#endif
        USE PESTDATA, ONLY : IJRF,SPLITTHRESH,SPLITRELDIFF,SPLITACTION,
     +                       IPPP,IPPO,PARREGFILE,OBSREGFILE,DERCOM,
     +                       IREFOBS,NUMCOM,DERFORGIVE,MS_STATUS,ERRMSG
#ifdef BEO
        USE BEOPEST, ONLY  : RUNCOM,IREFOBS_RUNCOM,RUN_REASON
#endif

C -- SUBROUTINE DERCLC CALCULATES THE DERIVATIVES OF ALL OBSERVATIONS
C -- WITH RESPECT TO ALL ADJUSTABLE PARAMETERS

        IMPLICIT NONE
        INTEGER ASIZE,NINSTR,IFAIL,NORMRETURN
        INTEGER NOBS,NPAR,NPARGP,IPP,IPG,IGO,I,JDBND,ZINC,IADJ,
     +  ZROMAX,I2OR3,NOCHNG,NOINC,N2OR3,MID2,I3CLC,NINFLE,NOUTFL,
#ifdef INTEL
     +  NUML,PRECIS,NBLBMX,NOPNT,NSCALL,MMCALL,SOPDIM,
#else
     +  NUML,PRECIS,NBLBMX,ISTOP,NOPNT,NSCALL,MMCALL,SOPDIM,
#endif
     +  NRUN,IRUN,ICALL,IIRUN,DERFLAG,
     +  NSCALLP,MMCALLP,IMSFLE,JFAIL
        INTEGER XCOLDIM,ACTFLAG,SPLITFLAG
        INTEGER REPEATFLAG
        INTEGER FLAGPLL,FLAG5,JJFAIL
        INTEGER DF_FLAG
        INTEGER IDBND(NPARGP),IDERIV(NPARGP),
     +  J2OR3(NPARGP),I3CALC(NPARGP)
        INTEGER ITRANS(NPAR),NW(NPAR),LCINS(NINSTR),LL(NUML),
     +  OBSN1(NOBS),OBSN2(NOBS),IIOBS(NOBS),IPARGP(NPAR)
        DOUBLE PRECISION RTEMP,X1,X2,X3,RDIST,AINCR,TDRVAL,
     +  X3X1,X3X2,X2X1,Y1,Y2,Y3,RFP,TDRBND,TTPAR,RPMAX,
     +  ZROINC,LOGFAC,DEN
        DOUBLE PRECISION MINSLOPE,SLOPEDIFF,RELSLOPEDIFF,SLOPE1,SLOPE2
        DOUBLE PRECISION HH,AA,PV1,PV2,PV3,PV4,TWOH,AA2,ALPHA,BETA
        DOUBLE PRECISION REFPAR(NPAR),LBOUND(NPAR),UBOUND(NPAR),
     +  REFOBS(NOBS),
     +  DERVAL(NPARGP),DERBND(NPARGP),TPAR(NPAR),SCALE(NPAR),
     +  TMOBS(NOBS),TTMOBS(NOBS),DERMUL(NPARGP),OFFSET(NPAR)
        DOUBLE PRECISION XCOL(XCOLDIM)

#ifdef PARALLEL
        INTEGER INCPAR(NRUN)
#else
        INTEGER INCPAR(1)
#endif

        CHARACTER*(*) PWORD(NPAR),APAR(NPAR),AOBS(NOBS),INSFLE(NOUTFL),
     +  TMPFL(NINFLE),INFLE(NINFLE),OUTFLE(NOUTFL)
        CHARACTER A(ASIZE),PARDEL(NINFLE),MRKDEL(NOUTFL)
        CHARACTER*(*) CLINE,BUF,MODFLE
        CHARACTER*12 AAPAR,ATEMP*12

#ifndef INTEL
        COMMON /SSTOP/ISTOP
#endif

        FLAGPLL=0
#ifdef PARALLEL
        FLAGPLL=1
#endif

        IFAIL=0
        NOCHNG=0
        NORMRETURN=0
        REPEATFLAG=1
        DF_FLAG=0
        IF(DERFORGIVE.EQ.1)DF_FLAG=1

        X1=-1.0D250
        LOGFAC=LOG(10.0D0)
        IADJ=0
        ZINC=0
        ZROINC=2.0
        ZROMAX=6
        AAPAR=APAR(IPP)
        RFP=REFPAR(IPP)
        IPG=IPARGP(IPP)
        TDRVAL=DERVAL(IPG)
        TDRBND=DERBND(IPG)
        JDBND=IDBND(IPG)
        I3CLC=I3CALC(IPG)
        I2OR3=J2OR3(IPG)
        IF(I2OR3.EQ.1) THEN
          I2OR3=N2OR3
        ELSE
          IF(I2OR3.EQ.4)THEN
            I2OR3=N2OR3
            IF(I2OR3.EQ.3) I2OR3=5
          END IF
        END IF
        IF((SPLITFLAG.GT.0).AND.(I2OR3.EQ.5)) SPLITFLAG=0
        IF(SPLITFLAG.GT.0)THEN
          IF((SPLITTHRESH(IPG).GT.0.0D0).AND.(SPLITACTION(IPG).EQ.3))
     +      THEN
            DO I=1,NOBS
              XCOL(I)=TMOBS(I)
            END DO
          END IF
        END IF

        IF(IDERIV(IPG).EQ.3) THEN
          RPMAX=0.0
          DO 5 I=1,NPAR
          IF(IPARGP(I).NE.IPG) GO TO 5
          IF(ABS(REFPAR(I)).GT.RPMAX) RPMAX=ABS(REFPAR(I))
5         CONTINUE
          IF((RPMAX.EQ.0.0).AND.(JDBND.EQ.0))THEN
            CALL STPERR(101,1,AAPAR,0,' ',CLINE)
            GO TO 9891
          END IF
        END IF

        FLAG5=0
        IF(I2OR3.EQ.5)THEN
          FLAG5=1
          IF(IDERIV(IPG).EQ.1)THEN
            IF(RFP.EQ.0.0)THEN
              IF(ITRANS(IPP).EQ.1)THEN
                CALL STPERR(102,1,AAPAR,0,' ',CLINE)
                GO TO 9891
              END IF
              IF(JDBND.EQ.0)THEN
                CALL STPERR(103,1,AAPAR,0,' ',CLINE)
                GO TO 9891
              ELSE IF(JDBND.EQ.1)THEN
                AINCR=DERBND(IPG)
              END IF
            ELSE
              IF((RFP.LE.0.0).AND.(ITRANS(IPP).EQ.1))THEN
                CALL STPERR(102,1,AAPAR,0,' ',CLINE)
                GO TO 9891
              END IF
              AINCR=ABS(RFP*DERVAL(IPG))
              IF(JDBND.EQ.1)THEN
                IF(AINCR.LT.DERBND(IPG))AINCR=DERBND(IPG)
              END IF
            END IF
          ELSE IF(IDERIV(IPG).EQ.2)THEN
            AINCR=DERVAL(IPG)
          ELSE
            AINCR=ABS(DERVAL(IPG)*RPMAX)
            IF(JDBND.EQ.1)THEN
              IF(AINCR.LT.DERBND(IPG))AINCR=DERBND(IPG)
            END IF
          END IF

          HH=AINCR*0.5*DERMUL(IPG)
          AA=AINCR*DERMUL(IPG)
          PV1=RFP-HH
          PV2=RFP+HH
          PV3=RFP-HH-AA
          PV4=RFP+HH+AA
          AA=(HH+AA)/HH
          IF((PV4.GT.UBOUND(IPP)).OR.(PV3.LT.LBOUND(IPP)))THEN
            IF(PV4.GT.UBOUND(IPP))PV4=UBOUND(IPP)
            IF(PV3.LT.LBOUND(IPP))PV3=LBOUND(IPP)
            RTEMP=PV4-PV3
            RTEMP=RTEMP/3.0D0
            PV1=PV3+RTEMP
            PV2=PV1+RTEMP
            HH=RTEMP/2.0D0
            AA=3.0D0
          END IF
        ELSE
          IF(I2OR3.EQ.3) TDRVAL=TDRVAL*DERMUL(IPG)
          RDIST=UBOUND(IPP)-LBOUND(IPP)
          RTEMP=RDIST/3.21
          IF(IDERIV(IPG).EQ.1) THEN
            IF(ABS(RFP*TDRVAL).GT.RTEMP) TDRVAL=ABS(RTEMP/RFP)
          ELSE IF(IDERIV(IPG).EQ.2) THEN
            IF(TDRVAL.GT.RTEMP) TDRVAL=RTEMP
          ELSE
            IF(ABS(RPMAX*TDRVAL).GT.RTEMP) TDRVAL=ABS(RTEMP/RPMAX)
          END IF
          IF(JDBND.EQ.1) THEN
            IF(TDRBND.GT.RTEMP) TDRBND=RTEMP
          END IF
        END IF

9       IGO=0
10      IGO=IGO+1
11      IF(((IGO.GE.2).AND.(I2OR3.EQ.2)).OR.
     +     ((IGO.EQ.3).AND.(I2OR3.EQ.3)).OR.
     +      (IGO.EQ.5))GO TO 1000

        IF(FLAG5.EQ.1)THEN
          IF(IGO.EQ.1)THEN
            TTPAR=PV2
          ELSE IF(IGO.EQ.2)THEN
            TTPAR=PV1
          ELSE IF(IGO.EQ.3)THEN
            TTPAR=PV4
          ELSE
            TTPAR=PV3
          END IF
          GO TO 45
        END IF

30      IF(IDERIV(IPG).EQ.1)THEN

          IF(RFP.EQ.0.0) THEN

            IF(IGO.EQ.1) THEN
              IF(ITRANS(IPP).EQ.1) THEN
                CALL STPERR(102,1,AAPAR,0,' ',CLINE)
                GO TO 9891
              END IF
              IF(JDBND.EQ.0) THEN
                CALL STPERR(103,1,AAPAR,0,' ',CLINE)
                GO TO 9891
              ELSE IF(JDBND.EQ.1) THEN
                AINCR=TDRBND
              END IF
              TTPAR=RFP+AINCR
              IF(TTPAR.GT.UBOUND(IPP)) THEN
                IADJ=1
                TTPAR=RFP-AINCR
              END IF
            ELSE IF(IGO.EQ.2) THEN
              IF(IADJ.EQ.0) THEN
                TTPAR=RFP-AINCR
                IF(TTPAR.LT.LBOUND(IPP))THEN
                  TTPAR=RFP+2.0*AINCR
                END IF
              ELSE IF(IADJ.EQ.1) THEN
                TTPAR=RFP-2.0*AINCR
              END IF
            END IF

          ELSE

            IF(IGO.EQ.1) THEN
              IF((RFP.LE.0.0).AND.(ITRANS(IPP).EQ.1))THEN
                CALL STPERR(102,1,AAPAR,0,' ',CLINE)
                GO TO 9891
              END IF
              AINCR=ABS(RFP*TDRVAL)
              IF(JDBND.EQ.1) THEN
                IF(AINCR.LT.TDRBND) AINCR=TDRBND
              END IF
              TTPAR=RFP+AINCR
              IF(TTPAR.GT.UBOUND(IPP))THEN
                IADJ=1
                TTPAR=RFP-AINCR
              END IF
            ELSE IF(IGO.EQ.2) THEN
              IF(IADJ.EQ.0) THEN
                TTPAR=RFP-AINCR
                IF(TTPAR.LT.LBOUND(IPP)) THEN
                  TTPAR=RFP+2.0*AINCR
                END IF
              ELSE
                TTPAR=RFP-2.0*AINCR
              END IF
            END IF

          END IF

        ELSE IF((IDERIV(IPG).EQ.2).OR.(IDERIV(IPG).EQ.3))THEN

          IF(IGO.EQ.1) THEN

            IF(IDERIV(IPG).EQ.3) THEN
              AINCR=ABS(TDRVAL*RPMAX)
              IF(JDBND.EQ.1) THEN
                IF(AINCR.LT.TDRBND) AINCR=TDRBND
              END IF
            ELSE
              AINCR=TDRVAL
            END IF
            TTPAR=RFP+AINCR
            IF(TTPAR.GT.UBOUND(IPP)) THEN
              IADJ=1
              TTPAR=RFP-AINCR
            END IF
          ELSE IF(IGO.EQ.2) THEN
            IF(IADJ.EQ.0) THEN
              TTPAR=RFP-AINCR
              IF(TTPAR.LT.LBOUND(IPP)) THEN
                TTPAR=RFP+2.0*AINCR
              END IF
            ELSE IF(IADJ.EQ.1) THEN
              TTPAR=RFP-2.0*AINCR
            END IF
          END IF

        END IF

        IF((TTPAR.GT.UBOUND(IPP)).OR.(TTPAR.LT.LBOUND(IPP)))THEN
          CALL STPERR(106,1,AAPAR,0,' ',CLINE)
          GO TO 9891
        END IF
        IF(TTPAR-RFP.EQ.0.0) GO TO 220
45      CONTINUE
        TPAR(IPP)=TTPAR

        DO 150 I=1,NPAR
        IF(I.EQ.IPP) GO TO 150
        IF(-ITRANS(I).NE.IPP) THEN
          TPAR(I)=REFPAR(I)
        ELSE
          TPAR(I)=TTPAR*REFPAR(I)/REFPAR(IPP)
        END IF
150     CONTINUE

#ifdef PARALLEL
        IF(FLAG5.EQ.0)THEN
          CALL ZINCTEST(JFAIL,NOCHNG,NPAR,PRECIS,NOPNT,NW,IPP,TPAR,
     +    REFPAR,SCALE,OFFSET,PWORD,APAR,CLINE,IGO,X1)
          IF(JFAIL.NE.0) GO TO 9891
        END IF
#else
        IF((MS_STATUS.EQ.1).OR.(MS_STATUS.EQ.4))THEN
          CALL MODSUB_MRUN(JFAIL,TPAR,SCALE,OFFSET,TMOBS,ERRMSG)
          IF(IFAIL.NE.0) GO TO 9891
        ELSE
          IF(IGO.EQ.1)THEN
            INCPAR(1)=IPP
          ELSE IF(IGO.EQ.2)THEN
            INCPAR(1)=-IPP
          ELSE IF(IGO.EQ.3)THEN
            INCPAR(1)=10000+IPP
          ELSE
            INCPAR(1)=-10000-IPP
          END IF
          IF((ISTOP.EQ.2).OR.(ISTOP.EQ.1)) RETURN
          CALL MODEL(IFAIL,NPAR,NOBS,ASIZE,NINSTR,NINFLE,NOUTFL,NUML,
     +    NBLBMX,SCALE,OFFSET,TPAR,NW,PWORD,APAR,TMOBS,AOBS,IPP,REFPAR,
     +    NOCHNG,PRECIS,NOPNT,LCINS,LL,OBSN1,OBSN2,IIOBS,INSFLE,TMPFL,
     +    INFLE,OUTFLE,PARDEL,MRKDEL,A,CLINE,BUF,MODFLE,IGO,X1,IMSFLE,
     +    INCPAR(1),ITRANS,REPEATFLAG,FLAG5,DF_FLAG)
          IF(IFAIL.NE.0) GO TO 9891
        END IF

#ifdef LAHEY
#ifndef GMS
        WRITE(6,*)
#endif
#endif
#endif

159     IF(NOCHNG.EQ.1) GO TO 220

#ifdef PARALLEL
        IRUN=IRUN+1
        IF(ICALL.EQ.1)THEN
          CALL STORE_PARALLEL_REGISTER
     +    (JJFAIL,NPAR,IPPP,IRUN,TPAR,PARREGFILE)
          IF(JJFAIL.NE.0) GO TO 9891
#ifdef BEO
          RUNCOM(IRUN)=DERCOM(IPP)
          IF(RUNCOM(IRUN).EQ.0) RUNCOM(IRUN)=1
          IF(IREFOBS.EQ.0)THEN
            IREFOBS_RUNCOM(IRUN)=0
          ELSE
            IF(NUMCOM.GT.1)THEN
              IREFOBS_RUNCOM(IRUN)=0
            ELSE
              IREFOBS_RUNCOM(IRUN)=1
            END IF
          END IF
          RUN_REASON(IRUN)=IPP                       ! Derivatives for parameter IPP
#endif
          IF(IGO.EQ.1)THEN
            INCPAR(IRUN)=IPP
          ELSE IF(IGO.EQ.2)THEN
            INCPAR(IRUN)=-IPP
          ELSE IF(IGO.EQ.3)THEN
            INCPAR(IRUN)=10000+IPP
          ELSE
            INCPAR(IRUN)=-10000-IPP
          END IF
        ELSE
          CALL RETRIEVE_PARALLEL_REGISTER
     +    (JJFAIL,NPAR,IPPP,IRUN,TPAR,PARREGFILE)
          IF(JJFAIL.NE.0) GO TO 9891
          CALL RETRIEVE_PARALLEL_REGISTER
     +    (JJFAIL,NOBS,IPPO,IRUN,TMOBS,OBSREGFILE)
          IF(JJFAIL.NE.0) GO TO 9891
        END IF
#else
        IIRUN=IIRUN+1
#ifdef UNIX
        WRITE(IJRF,2060) IIRUN
2060    FORMAT('    Number of Jacobian runs completed =',i3)

#ifdef FLUSHFILE
        CALL FLUSH(IJRF)
#endif
#endif
#ifdef LAHEY
        IF((DERFLAG.EQ.0).OR.(DERFLAG.EQ.2))THEN
          CALL WRITINT(ATEMP,IIRUN)
#ifdef GMS
        WRITE(6,2061) IIRUN
2061    FORMAT('    Number of Jacobian runs completed =',i3)
#else
          WRITE(6,255,advance='no') TRIM(ATEMP)
#endif

255       FORMAT('+   ',a,' runs completed.')
        END IF
#endif

#ifdef INTEL
        IF((DERFLAG.EQ.0).OR.(DERFLAG.EQ.2))THEN
           IF(IIRUN.EQ.1)THEN
             WRITE(6,2561)
2561         FORMAT('    - number of runs completed...')
             WRITE(6,'(a)',ADVANCE='NO') '     '
           END IF
           WRITE(6,2571) IIRUN
2571       FORMAT(I6,$)
           IF((IIRUN.EQ.NRUN).OR.(MOD(IIRUN,12).EQ.0))THEN
             WRITE(6,*)
             IF(IIRUN.LT.NRUN)THEN
               WRITE(6,'(A)',ADVANCE='NO') '     '
             END IF
           END IF
        END IF
#endif
#endif

        IF(FLAG5.EQ.0)THEN
          IF(IGO.EQ.1) THEN
            X1=TPAR(IPP)
            IF(DERFLAG.NE.1)THEN
              DO 160 I=1,NOBS
160           TTMOBS(I)=TMOBS(I)
            ELSE
              TTMOBS(NOBS)=TMOBS(NOBS)
            END IF
          ELSE
            X3=TPAR(IPP)
          END IF
        ELSE
          IF(IGO.EQ.1)THEN
            PV2=TPAR(IPP)
            IF((FLAGPLL.EQ.0).OR.
     +        ((FLAGPLL.EQ.1).AND.(ICALL.EQ.2)))THEN
                 DO I=1,NOBS
                   TTMOBS(I)=TMOBS(I)
                 END DO
            END IF
          ELSE IF(IGO.EQ.2)THEN
            PV1=TPAR(IPP)
            IF((FLAGPLL.EQ.0).OR.
     +         ((FLAGPLL.EQ.1).AND.(ICALL.EQ.2)))THEN
               IF(DERFORGIVE.EQ.0)THEN
                 DO I=1,NOBS
                   TTMOBS(I)=TTMOBS(I)-TMOBS(I)
                 END DO
               ELSE
                 DO I=1,NOBS
                   IF(TTMOBS(I).LT.-1.0D300)THEN
                     CONTINUE
                   ELSE IF(TMOBS(I).LT.-1.0D300)THEN
                     TTMOBS(I)=-1.1D300
                   ELSE
                     TTMOBS(I)=TTMOBS(I)-TMOBS(I)
                   END IF
                 END DO
               END IF
            END IF
          ELSE IF(IGO.EQ.3)THEN
            PV4=TPAR(IPP)
            IF((FLAGPLL.EQ.0).OR.
     +         ((FLAGPLL.EQ.1).AND.(ICALL.EQ.2)))THEN
               DO I=1,NOBS
                 XCOL(I)=TMOBS(I)
               END DO
            END IF
          ELSE IF(IGO.EQ.4)THEN
            PV3=TPAR(IPP)
          END IF
        END IF

        GO TO 10

220     CONTINUE

        IF(FLAG5.EQ.1)THEN
          CALL STPERR(105,1,AAPAR,0,' ',CLINE)
          GO TO 9891
        ENDIF

        ZINC=ZINC+1
        IF(ZINC.GT.ZROMAX) THEN
          CALL STPERR(105,1,AAPAR,0,' ',CLINE)
          GO TO 9891
        END IF
        IF(IGO.EQ.1)THEN
          NOINC=0
          RTEMP=TDRVAL*ZROINC
          IF(((IDERIV(IPG).EQ.1).AND.
     +    (ABS(RTEMP*RFP).GT.RDIST/3.2)).OR.
     +    ((IDERIV(IPG).EQ.2).AND.(RTEMP.GT.RDIST/3.2)).OR.
     +    ((IDERIV(IPG).EQ.3).AND.(RTEMP*RPMAX.GT.RDIST/3.2)))THEN
            NOINC=NOINC+1
            GO TO 230
          END IF
          TDRVAL=RTEMP
230       RTEMP=TDRBND*ZROINC
          IF(JDBND.NE.1) THEN
            NOINC=NOINC+1
            GO TO 250
          ELSE
            IF(RTEMP.GT.RDIST/3.2) THEN
              NOINC=NOINC+1
              GO TO 250
            END IF
            TDRBND=RTEMP
          END IF
250       IF(NOINC.EQ.2) THEN
            CALL STPERR(105,1,AAPAR,0,' ',CLINE)
            GO TO 9891
          END IF
          GO TO 9
        ELSE
          AINCR=AINCR*ZROINC
          IF(AINCR.GT.RDIST/3.2) GO TO 9900
          GO TO 11
        END IF

1000    CONTINUE

#ifdef PARALLEL
        IF(ICALL.EQ.1) RETURN
#endif

        IF(FLAG5.EQ.1)THEN
          TWOH=PV2-PV1
          IF(TWOH.EQ.0.0D0) GO TO 220
          AA=(PV4-PV3)/(PV2-PV1)
          IF(AA.EQ.0.0D0) GO TO 220
          AA2=AA*AA
          IF(I3CLC.EQ.4)THEN
            ALPHA=1.0D0/(TWOH*(AA2+1.0D0))
            BETA=AA/(TWOH*(AA2+1.0D0))
          ELSE
            ALPHA=AA2/(TWOH*(AA2-1.0D0))
            BETA=-1.0D0/(TWOH*AA*(AA2-1.0D0))
          END IF
          IF(DERFORGIVE.EQ.0)THEN
            DO I=1,NOBS
              TMOBS(I)=ALPHA*TTMOBS(I)+BETA*(XCOL(I)-TMOBS(I))
              IF(ITRANS(IPP).EQ.1)
     +        TMOBS(I)=TMOBS(I)*RFP*LOGFAC
            END DO
          ELSE
            DO I=1,NOBS
              IF((TTMOBS(I).LT.-1.0D300).OR.(XCOL(I).LT.-1.0D300).OR.
     +          (TMOBS(I).LT.-1.0D300))THEN
                TMOBS(I)=0.0D0
              ELSE
                TMOBS(I)=ALPHA*TTMOBS(I)+BETA*(XCOL(I)-TMOBS(I))
                IF(ITRANS(IPP).EQ.1)
     +          TMOBS(I)=TMOBS(I)*RFP*LOGFAC
              END IF
            END DO
          END IF
          GO TO 1105
        END IF

        IF(I2OR3.EQ.2) THEN
          IF(X1-RFP.EQ.0.0D0) GO TO 9900
          IF(DERFLAG.NE.1)THEN
            IF(DERFORGIVE.EQ.0)THEN
              DO 1050 I=1,NOBS
              TMOBS(I)=(TTMOBS(I)-REFOBS(I))/(X1-RFP)
              IF(ITRANS(IPP).EQ.1) TMOBS(I)=TMOBS(I)*RFP*LOGFAC
1050          CONTINUE
            ELSE
              DO I=1,NOBS
                IF(TTMOBS(I).GT.-1.0D300)THEN
                  TMOBS(I)=(TTMOBS(I)-REFOBS(I))/(X1-RFP)
                  IF(ITRANS(IPP).EQ.1) TMOBS(I)=TMOBS(I)*RFP*LOGFAC
                ELSE
                  TMOBS(I)=0.0D0
                END IF
              END DO
            END IF
          ELSE
            IF(DERFORGIVE.EQ.0)THEN
              TMOBS(NOBS)=(TTMOBS(NOBS)-REFOBS(NOBS))/(X1-RFP)
              IF(ITRANS(IPP).EQ.1) TMOBS(NOBS)=TMOBS(NOBS)*RFP*LOGFAC
            ELSE
              IF(TTMOBS(NOBS).GT.-1.0D300)THEN
                TMOBS(NOBS)=(TTMOBS(NOBS)-REFOBS(NOBS))/(X1-RFP)
                IF(ITRANS(IPP).EQ.1) TMOBS(NOBS)=TMOBS(NOBS)*RFP*LOGFAC
              ELSE
                TMOBS(NOBS)=0.0D0
              END IF
            END IF
          END IF
        ELSE
          X2=RFP
          X2X1=X2-X1
          X3X2=X3-X2
          X3X1=X3-X1
          IF((X2X1.EQ.0.0D0).OR.(X3X2.EQ.0.0D0).OR.(X3X1.EQ.0.0D0))
     +    GO TO 9900
          DEN=X3X1*X3X1+X2X1*X2X1+X3X2*X3X2
          IF(((X2X1.GT.0.0D0).AND.(X3X2.GT.0.0D0)).OR.
     +       ((X2X1.LT.0.0D0).AND.(X3X2.LT.0.0D0)))THEN
                MID2=1
          ELSE
                MID2=0
          END IF
          DO 1100 I=1,NOBS
          IF((DERFLAG.EQ.1).AND.(I.NE.NOBS)) GO TO 1100
          Y2=REFOBS(I)
          Y1=TTMOBS(I)
          Y3=TMOBS(I)
          IF((Y3.LT.-1.0D300).OR.(Y1.LT.-1.0D300))THEN
            TMOBS(I)=0.0D0
            GO TO 1100
          END IF
          IF(SPLITFLAG.GT.0)THEN
            IF(SPLITTHRESH(IPG).GT.0.0D0)THEN
              SLOPE1=(Y2-Y1)/X2X1
              IF(MID2.EQ.1)THEN
                SLOPE2=(Y3-Y2)/X3X2
              ELSE
                SLOPE2=(Y3-Y1)/X3X1
              END IF
              IF(ITRANS(IPP).EQ.1) THEN
                SLOPE1=SLOPE1*RFP*LOGFAC
                SLOPE2=SLOPE2*RFP*LOGFAC
              END IF
              IF((ABS(SLOPE2).LT.SPLITTHRESH(IPG)).AND.
     +           (ABS(SLOPE1).LT.SPLITTHRESH(IPG))) THEN
                 GO TO 2010
              ELSE
                SLOPEDIFF=ABS(SLOPE2-SLOPE1)
                MINSLOPE=MIN(ABS(SLOPE2),ABS(SLOPE1))
                ACTFLAG=0
                IF(MINSLOPE.LE.1.0D-35)THEN
                  ACTFLAG=1
                ELSE
                  RELSLOPEDIFF=ABS(SLOPEDIFF/MINSLOPE)
                  IF(RELSLOPEDIFF.GT.SPLITRELDIFF(IPG))ACTFLAG=1
                END IF
                IF(ACTFLAG.EQ.0) GO TO 2010
                IF(SPLITACTION(IPG).EQ.2)THEN
                  TMOBS(I)=0.0D0
                ELSE IF(SPLITACTION(IPG).EQ.1)THEN
                  IF(ABS(SLOPE2).LT.ABS(SLOPE1))THEN
                    TMOBS(I)=SLOPE2
                  ELSE
                    TMOBS(I)=SLOPE1
                  END IF
                ELSE IF(SPLITACTION(IPG).EQ.3)THEN
                  TMOBS(I)=XCOL(I)
                END IF
                GO TO 1100
              END IF
            END IF
          END IF
2010      CONTINUE
          IF((I3CLC.EQ.1).OR.((I3CLC.EQ.3).AND.(MID2.EQ.0)))THEN
            TMOBS(I)=-Y2*(2.0*X2-X1-X3)/X2X1/X3X2
     +               -Y1*X3X2/X2X1/X3X1
     +               +Y3*X2X1/X3X2/X3X1
          ELSE IF(I3CLC.EQ.2) THEN
            TMOBS(I)=(X1*(Y1-Y2)+X2*(Y2-Y3)+X3*(Y3-Y1)
     +               -Y1*X2X1-Y2*X3X2+Y3*X3X1)/DEN
          ELSE
            TMOBS(I)=(Y3-Y1)/X3X1
          END IF
          IF(ITRANS(IPP).EQ.1) TMOBS(I)=TMOBS(I)*RFP*LOGFAC
1100      CONTINUE
        END IF

1105    CONTINUE
        NORMRETURN=1
        RETURN

9900    CALL STPERR(107,1,AAPAR,0,' ',CLINE)
        GO TO 9891

9891    IFAIL=1
        RETURN

#ifdef PESTMOD
        END SUBROUTINE DERCLC
#else
        END
#endif



