
        SUBROUTINE WRTSIG(IFAIL,VAL,WORD,NW,PRECIS,TVAL,NOPNT)
C --
C -- SUBROUTINE WRTSIG WRITES A NUMBER INTO A CONFINED SPACE WITH MAXIMUM
C -- PRECISION
C --

c       failure criteria:
c           ifail= 1 ...... number too large or small for single precision type
c           ifail= 2 ...... number too large or small for double precision type
c           ifail= 3 ...... field width too small to represent number
c           ifail=-1 ...... internal error type 1
c           ifail=-2 ...... internal error type 2
c           ifail=-3 ...... internal error type 3

        INTEGER PRECIS,LW,POS,INC,D,P,W,J,JJ,K,JEXP,N,JFAIL,NW,
     +  EPOS,PP,NOPNT,KEXP,IFLAG,LEXP
        INTEGER IFAIL
        DOUBLE PRECISION VAL,TVAL
        CHARACTER*29 TWORD,TTWORD,FMT*14
        CHARACTER*(*) WORD

C       The following line overcomes what appears to be a bug in the LF90
C       compiler

#ifdef LAHEY
        if(abs(val).lt.1.0d-300) val=0.0d0
#endif

        LEXP=0
        IFLAG=0
        WORD=' '
        POS=1
        IF(VAL.LT.0.0D0)POS=0
#ifdef USE_D_FORMAT
        WRITE(TWORD,'(1PD23.15D3)') VAL
#else
        WRITE(TWORD,'(1PE23.15E3)') VAL
#endif
        READ(TWORD(20:23),'(I4)') JEXP
        EPOS=1
        IF(JEXP.LT.0)EPOS=0

        JFAIL=0
        IFAIL=0
        IF(PRECIS.EQ.0)THEN
          LW=MIN(15,NW)
        ELSE
          LW=MIN(23,NW)
        END IF

        N=0
        IF(NOPNT.EQ.1)N=N+1
        IF(POS.EQ.1)N=N+1
        IF(PRECIS.EQ.0)THEN
          IF(ABS(JEXP).GT.38)THEN
            IFAIL=1
            RETURN
          END IF
          IF(POS.EQ.1) THEN
            IF(LW.GE.13) THEN
              WRITE(WORD,'(1PE13.7)',ERR=80) VAL
              GO TO 200
            END IF
          ELSE
            IF(LW.GE.14)THEN
              WRITE(WORD,'(1PE14.7)',ERR=80) VAL
              GO TO 200
            END IF
          END IF
          IF(LW.GE.14-N) THEN
            LW=14-N
            GO TO 80
          END IF
        ELSE
          IF(ABS(JEXP).GT.275)THEN
            IFAIL=2
            RETURN
          END IF
          IF(POS.EQ.1) THEN
            IF(LW.GE.22) THEN
#ifdef USE_D_FORMAT
              WRITE(WORD,'(1PD22.15D3)',ERR=80) VAL
#else
              WRITE(WORD,'(1PE22.15E3)',ERR=80) VAL
#endif
              GO TO 200
            END IF
          ELSE
            IF(LW.GE.23) THEN
#ifdef USE_D_FORMAT
              WRITE(WORD,'(1PD23.15D3)',ERR=80) VAL
#else
              WRITE(WORD,'(1PE23.15E3)',ERR=80) VAL
#endif
              GO TO 200
            END IF
          END IF
          IF(LW.GE.23-N)THEN
            LW=23-N
            GO TO 80
          END IF
        END IF

        IF(NOPNT.EQ.1)THEN
          IF((JEXP.EQ.LW-2+POS).OR.(JEXP.EQ.LW-3+POS))THEN
            WRITE(FMT,15)LW+1
15          FORMAT('(F',I2,'.0)')
            WRITE(WORD,FMT,ERR=19) VAL
            IF(INDEX(WORD,'*').NE.0) GO TO 19
            IF(WORD(1:1).EQ.' ') GO TO 19
            WORD(LW+1:LW+1)=' '
            GO TO 200
          END IF
        END IF
19      D=MIN(LW-2+POS,LW-JEXP-3+POS)
20      IF(D.LT.0) GO TO 80
        WRITE(FMT,30) LW,D
30      FORMAT('(F',I2,'.',I2,')')
        WRITE(WORD,FMT,ERR=80) VAL
        IF(INDEX(WORD,'*').NE.0) THEN
          D=D-1
          GO TO 20
        END IF
        K=INDEX(WORD,'.')
        IF(K.EQ.0)THEN
          IFAIL=-1
          RETURN
        END IF
        IF((K.EQ.1).OR.((POS.EQ.0).AND.(K.EQ.2)))THEN
          DO 70 J=1,3
          IF(K+J.GT.LW) GO TO 75
          IF(WORD(K+J:K+J).NE.'0') GO TO 200
70        CONTINUE
          GO TO 80
75        IFAIL=3
          RETURN
        END IF
        GO TO 200

80      WORD=' '
        IF(NOPNT.EQ.0)THEN
          D=LW-7
          IF(POS.EQ.1) D=D+1
          IF(EPOS.EQ.1) D=D+1
          IF(ABS(JEXP).LT.100) D=D+1
          IF(ABS(JEXP).LT.10) D=D+1
          IF((JEXP.GE.100).AND.(JEXP-(D-1).LT.100))THEN
            P=1+(JEXP-99)
            D=D+1
            LEXP=99
          ELSE IF((JEXP.GE.10).AND.(JEXP-(D-1).LT.10))THEN
            P=1+(JEXP-9)
            D=D+1
            LEXP=9
          ELSE IF((JEXP.EQ.-10).OR.(JEXP.EQ.-100)) THEN
            IFLAG=1
            D=D+1
          ELSE
            P=1
          END IF
          INC=0
85        IF(D.LE.0) GO TO 300
          IF(IFLAG.EQ.0)THEN
            WRITE(FMT,100,ERR=300) P,D+7,D-1
          ELSE
            WRITE(FMT,100,ERR=300) 0,D+8,D
          END IF
          WRITE(TWORD,FMT) VAL
          IF(IFLAG.EQ.1) GO TO 87
          READ(TWORD(D+4:D+7),'(I4)',ERR=500) KEXP
          IF(((KEXP.EQ.10).AND.((JEXP.EQ.9).OR.(LEXP.EQ.9))).OR.
     +    ((KEXP.EQ.100).AND.((JEXP.EQ.99).OR.LEXP.EQ.99))) THEN
            IF(INC.EQ.0)THEN
              IF(LEXP.EQ.0)THEN
                IF(D-1.EQ.0) THEN
                  D=D-1
                ELSE
                  P=P+1
                END IF
              ELSE IF(LEXP.EQ.9)THEN
                IF(JEXP-(D-2).LT.10) THEN
                  P=P+1
                ELSE
                  D=D-1
                END IF
              ELSE IF(LEXP.EQ.99)THEN
                IF(JEXP-(D-2).LT.100)THEN
                  P=P+1
                ELSE
                  D=D-1
                END IF
              END IF
              INC=INC+1
              GO TO 85
            END IF
          END IF
#ifdef USE_D_FORMAT
87        J=INDEX(TWORD,'D')
#else
87        J=INDEX(TWORD,'E')
#endif
          GO TO 151
        END IF
        INC=0
        P=LW-2
        PP=JEXP-(P-1)
        IF(PP.GE.10)THEN
          P=P-1
          IF(PP.GE.100)P=P-1
        ELSE IF(PP.LT.0)THEN
          P=P-1
          IF(PP.LE.-10)THEN
            P=P-1
            IF(PP.LE.-100)P=P-1
          END IF
        END IF
        IF(POS.EQ.0)P=P-1
90      CONTINUE
        D=P-1
        W=D+8
        WRITE(FMT,100) P,W,D
        IF(D.LT.0)THEN
          IF(JFAIL.EQ.1) GO TO 300
          JFAIL=1
          P=P+1
          GO TO 90
        END IF
#ifdef USE_D_FORMAT
100     FORMAT('(',I2,'pD',I2,'.',I2,'D3)')
#else
100     FORMAT('(',I2,'pE',I2,'.',I2,'E3)')
#endif
        WRITE(TWORD,FMT) VAL
#ifdef USE_D_FORMAT
        J=INDEX(TWORD,'D')
#else
        J=INDEX(TWORD,'E')
#endif
        IF(TWORD(J-1:J-1).NE.'.')THEN
          IFAIL=-1
          RETURN
        END IF
        N=1
        IF(TWORD(J+1:J+1).EQ.'-') N=N+1
        IF(TWORD(J+2:J+2).NE.'0') THEN
          N=N+2
          GO TO 120
        END IF
        IF(TWORD(J+3:J+3).NE.'0') N=N+1
120     N=N+1
        IF(J+N-2-POS.LT.LW)THEN
          IF(INC.EQ.-1) GO TO 150
          TTWORD=TWORD
          P=P+1
          INC=1
          GO TO 90
        ELSE IF(J+N-2-POS.EQ.LW) THEN
          GO TO 150
        ELSE
          IF(INC.EQ.1)THEN
            TWORD=TTWORD
            GO TO 150
          END IF
          IF(JFAIL.EQ.1) GO TO 300
          P=P-1
          INC=-1
          GO TO 90
        END IF

150     J=INDEX(TWORD,'.')
151     IF(POS.EQ.0)THEN
          K=1
        ELSE
         K=2
        END IF
        WORD(1:J-K)=TWORD(K:J-1)
        JJ=J
        J=J-K+1
        IF(PRECIS.EQ.0)THEN
          WORD(J:J)='E'
        ELSE
#ifdef USE_D_FORMAT
          WORD(J:J)='D'
#else
          WORD(J:J)='E'
#endif
        END IF
        JJ=JJ+2
        IF(NOPNT.EQ.0) JJ=JJ-1
        IF(TWORD(JJ:JJ).EQ.'-')THEN
          J=J+1
          WORD(J:J)='-'
        END IF
        IF(TWORD(JJ+1:JJ+1).NE.'0')THEN
          J=J+2
          WORD(J-1:J)=TWORD(JJ+1:JJ+2)
          GO TO 180
        END IF
        IF(TWORD(JJ+2:JJ+2).NE.'0')THEN
          J=J+1
          WORD(J:J)=TWORD(JJ+2:JJ+2)
        END IF
180     J=J+1
        WORD(J:J)=TWORD(JJ+3:JJ+3)
        IF(IFLAG.EQ.1)THEN
          IF(POS.EQ.1)THEN
            JJ=1
          ELSE
            JJ=2
          END IF
          N=LEN_TRIM(WORD)
          DO 190 J=JJ,N-1
190       WORD(J:J)=WORD(J+1:J+1)
          WORD(N:N)=' '
        END IF

200     IF(LEN_TRIM(WORD).GT.LW)THEN
          IFAIL=-2
          RETURN
        END IF
        WRITE(FMT,30) LW,0
        READ(WORD,FMT,ERR=400) TVAL
        RETURN
300     IFAIL=3
        RETURN
400     continue
!        write(6,*) 'WORD = ',trim(word)   !debug
        IFAIL=-3
        RETURN
500     IFAIL=-2
        RETURN
#ifdef PESTMOD
        END SUBROUTINE WRTSIG
#else
        END
#endif
