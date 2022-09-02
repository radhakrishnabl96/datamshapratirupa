


        SUBROUTINE LINSPL(IFAIL,NUM,LW,RW,CLINE)

C -- SUBROUTINE LINSPL SPLITS A STRING INTO BLANK-DELIMITED SUBSTRINGS

        INTEGER NBLNK
        INTEGER IFAIL,NW,NBLC,J,I
        INTEGER NUM
        INTEGER LW(NUM),RW(NUM)
        CHARACTER*(*) CLINE

        IFAIL=0
        NW=0
        NBLC=NBLNK(CLINE)
        IF(NBLC.EQ.0) THEN
          IFAIL=1
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


        SUBROUTINE WRTRL(ATEMP,RVAL,N)

C -- SUBROUTINE WRTRL WRITES A REAL NUMBER TO A STRING

        INTEGER N
        DOUBLE PRECISION RVAL
        CHARACTER*15 FFMT
        CHARACTER*(*) ATEMP

        WRITE(FFMT,10) N+7,N
10      FORMAT('(1PG',I2,'.',I2,')')
        WRITE(ATEMP,FFMT) RVAL
        CALL SHIFTL(ATEMP)
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


        SUBROUTINE WRTERR

C -- SUBROUTINE WRTERR FORMATS AND WRITES ERROR MESSAGES

        INTEGER NBLNK
        INTEGER IERR,J,JEND,I,NBLC,ITAKE,MCASE
        CHARACTER*2120 ERRMSG

        COMMON /IERR/IERR,MCASE
        COMMON /ERRMSG/ERRMSG

        IERR=IERR+1
        IF(IERR.GT.100) CALL EXIT(100)
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


        SUBROUTINE RDERR(NTYP,AVAR)

C -- SUBROUTINE RDERR ASSISTS IN THE WRITING OF ERRORS ARISING OUT OF
C -- AN INABILITY TO READ CERTAIN VARIABLES

        INTEGER NBLNK
        INTEGER NTYP,IERR,MCASE
        CHARACTER*(*) AVAR
        CHARACTER*2120 ERRMSG,ALINE*20,CASFL*100,AALINE*200

        COMMON /IERR/ IERR,MCASE
        COMMON /ERRMSG/ ERRMSG
        COMMON /CASFL/ CASFL
        COMMON /ALINE/ ALINE,AALINE

        IF(NTYP.EQ.0) THEN
          WRITE(ERRMSG,10) ALINE(:NBLNK(ALINE)),CASFL(1:MCASE),AVAR
10        FORMAT(A,' ',A,': cannot read integer variable ',A,'.')
        ELSE IF (NTYP.EQ.1) THEN
          WRITE(ERRMSG,20) ALINE(:NBLNK(ALINE)),CASFL(1:MCASE),AVAR
20        FORMAT(A,' ',A,': cannot read real variable ',A,'.')
        ELSE IF(NTYP.EQ.2) THEN
          WRITE(ERRMSG,30) ALINE(:NBLNK(ALINE)),CASFL(1:MCASE),AVAR
30        FORMAT(A,' ',A,': cannot read character variable ',A,'.')
        END IF
        CALL WRTERR

        RETURN
        END


        SUBROUTINE ZRONEG(NTYP,AVAR)

C -- SUBROUTINE ZRONEG ASSISTS IN THE WRITING OF OUT-OF-BOUNDS ERRORS

        INTEGER NBLNK
        INTEGER NTYP,IERR,MCASE
        CHARACTER*(*) AVAR
        CHARACTER*2120 ERRMSG,ALINE*20,CASFL*100,AALINE*200

        COMMON /IERR/ IERR,MCASE
        COMMON /ERRMSG/ ERRMSG
        COMMON /CASFL/ CASFL
        COMMON /ALINE/ ALINE,AALINE

        IF(NTYP.EQ.0) THEN
          WRITE(ERRMSG,10) ALINE(:NBLNK(ALINE)),CASFL(1:MCASE),AVAR
10        FORMAT(A,' ',A,': integer variable ',A,' zero or negative.')
        ELSE IF(NTYP.EQ.1) THEN
          WRITE(ERRMSG,20) ALINE(:NBLNK(ALINE)),CASFL(1:MCASE),AVAR
20        FORMAT(A,' ',A,': real variable ',A,' zero or negative.')
        END IF
        CALL WRTERR
        RETURN
        END


        SUBROUTINE ZROONE(AVAR)

C -- SUBROUTINE ZROONE ASSISTS IN THE WRITING OF CERTAIN OUT-OF-BOUNDS
C -- ERRORS

        INTEGER NBLNK
        INTEGER IERR,MCASE
        CHARACTER*(*) AVAR
        CHARACTER*2120 ERRMSG,ALINE*20,CASFL*100,AALINE*200

        COMMON /IERR/ IERR,MCASE
        COMMON /ERRMSG/ ERRMSG
        COMMON /CASFL/ CASFL
        COMMON /ALINE/ ALINE,AALINE

        WRITE(ERRMSG,10) ALINE(:NBLNK(ALINE)),CASFL(1:MCASE),AVAR
10      FORMAT(A,' ',A,': integer variable ',A,' must be 0 or 1.')
        CALL WRTERR
        RETURN
        END


        SUBROUTINE LOHI(NTYP,AVAR,ILOHI,IVAL,RVAL,NSIG)

C -- SUBROUTINE LOHI ASSISTS IN THE WRITING OF CERTAIN OUT-OF-BOUNDS ERRORS

        INTEGER IERR,MCASE,IVAL
        INTEGER ILOHI,NSIG,NTYP
        DOUBLE PRECISION RVAL
        CHARACTER*(*) AVAR
        CHARACTER*2120 ERRMSG,ALINE*20,CASFL*100,ATEMP*20,ALOHI*5,
     +  AALINE*200

        COMMON /IERR/ IERR,MCASE
        COMMON /ERRMSG/ ERRMSG
        COMMON /CASFL/ CASFL
        COMMON /ALINE/ ALINE,AALINE

        IF(ILOHI.EQ.0) THEN
          ALOHI='low  '
        ELSE
          ALOHI='high '
        END IF
        IF(NTYP.EQ.0) THEN
          CALL WRTINT(ATEMP,IVAL)
          WRITE(ERRMSG,10) AVAR,ALOHI(1:INDEX(ALOHI,' ')-1),
     +    ATEMP(1:INDEX(ATEMP,' ')-1)
10        FORMAT(' Integer variable ',A,' unusually ',A,' (',A,').')
        ELSE IF (NTYP.EQ.1) THEN
          CALL WRTRL(ATEMP,RVAL,NSIG)
          WRITE(ERRMSG,20) AVAR,ALOHI(1:INDEX(ALOHI,' ')-1),
     +    ATEMP(1:INDEX(ATEMP,' ')-1)
20        FORMAT(' Real variable ',A,' unusually ',A,' (',A,').')
        END IF
        CALL WRTERR
        RETURN
        END


        SUBROUTINE PARCHK(IFAIL,J1,J2,TPAR,CLINE)

C -- SUBROUTINE PARCHK CHECKS THE INTEGRITY OF PARAMETER SPACES ON TEMPLATE
C -- FILES

        INTEGER J1,J2,I,J,IFAIL,MMLINE,K,L
        CHARACTER*12 TPAR
        CHARACTER*100 ERRMSG*2120,ALINE*20,AALINE*200
        CHARACTER*(*) CLINE

        COMMON /ERRMSG/ ERRMSG
        COMMON /ALINE/ ALINE,AALINE
        COMMON /MMLINE/MMLINE

        IFAIL=0
        TPAR=' '
        IF(J2-J1.LE.1) THEN
          WRITE(ERRMSG,5) AALINE(1:MMLINE)
5         FORMAT(A,'parameter space less than 3 characters wide.')
          CALL WRTERR
          IFAIL=1
          RETURN
        END IF
        DO 10 I=J1+1,J2-1
        IF(CLINE(I:I).NE.' ') GO TO 30
10      CONTINUE
        WRITE(ERRMSG,20) AALINE(1:MMLINE)
20      FORMAT(A,'blank parameter space.')
        CALL WRTERR
        IFAIL=2
        RETURN
30      DO 40 J=I+1,J2-1
        IF(CLINE(J:J).EQ.' ') GO TO 50
40      CONTINUE
        J=J2
50      J=J-1
        IF(J-I+1.GT.12)THEN
          WRITE(ERRMSG,60) AALINE(1:MMLINE),CLINE(I:J)
60        FORMAT(A,'parameter name "',A,'" greater than 12 characters ',
     +    'long.')
          CALL WRTERR
          IFAIL=3
          RETURN
        END IF
        IF(J.NE.J2-1)THEN
          DO 80 K=J+1,J2-1
          IF(CLINE(K:K).NE.' ') THEN
            DO 65 L=J2-1,K,-1
            IF(CLINE(L:L).NE.' ')THEN
              WRITE(ERRMSG,70) AALINE(1:MMLINE),CLINE(I:L)
70            FORMAT(A,'illegal parameter name - "',A,'".')
              CALL WRTERR
              IFAIL=4
              RETURN
            END IF
65          CONTINUE
          END IF
80        CONTINUE
        END IF
        TPAR=CLINE(I:J)
        RETURN
        END


        SUBROUTINE ILLINS(AINSTR)

C -- SUBROUTINE ILLINS ASSISTS IN THE WRITING OF INSTRUCTION FILE ERRORS

        INTEGER MMLINE
        CHARACTER*100 ERRMSG*2120,ALINE*20,AALINE*200
        CHARACTER*(*) AINSTR

        COMMON /ERRMSG/ ERRMSG
        COMMON /ALINE/ ALINE,AALINE
        COMMON /MMLINE/MMLINE

        WRITE(ERRMSG,10) AALINE(1:MMLINE),AINSTR
10      FORMAT(A,'illegal instruction - "',A,'".')
        CALL WRTERR

        RETURN
        END




        SUBROUTINE TABREM(CLINE)

C -- SUBROUTINE TABREM REMOVES TABS FROM STRINGS

        INTEGER I
        CHARACTER*(*) CLINE

        DO 10 I=1,LEN(CLINE)
10      IF(ICHAR(CLINE(I:I)).EQ.9) CLINE(I:I)=' '

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



        SUBROUTINE WHICH1(IFAIL,NPAR,IPAR,APAR,TPAR)

C -- SUBROUTINE WHICH1 LOCATES A STRING IN AN ARRAY

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


        SUBROUTINE GETINT(IFAIL,BUF,N1,N2,NBLB,MRKDEL)

C -- SUBROUTINE GETINT GETS THE NEXT STORED INSTRUCTION FOR PROCESSING

        INTEGER N1,N2,NBLB,I,II
        INTEGER IFAIL
        CHARACTER MRKDEL
        CHARACTER*(*) BUF

        IFAIL=0
        IF(N2.GE.NBLB) THEN
          N1=0
          RETURN
        END IF
        DO 10 I=N2+1,NBLB
        IF((BUF(I:I).NE.' ').AND.(ICHAR(BUF(I:I)).NE.9)) GO TO 50
10      CONTINUE
        N1=0
        RETURN
50      N1=I
        IF(BUF(N1:N1).NE.MRKDEL)THEN
          I=INDEX(BUF(N1:NBLB),' ')
          II=INDEX(BUF(N1:NBLB),CHAR(9))
          IF((I.EQ.0).AND.(II.EQ.0))THEN
            I=0
          ELSE IF(I.EQ.0)THEN
            I=II
          ELSE IF(II.EQ.0) THEN
            I=I
          ELSE
            I=MIN(I,II)
          END IF
          IF(I.NE.0) THEN
            N2=N1+I-2
          ELSE
            N2=NBLB
          END IF
        ELSE
          IF(N1.EQ.NBLB)THEN
            IFAIL=1
            RETURN
          END IF
          I=INDEX(BUF(N1+1:NBLB),MRKDEL)
          IF(I.EQ.0) THEN
            IFAIL=1
            RETURN
          END IF
          N2=N1+I
        END IF

        RETURN
        END


        SUBROUTINE GETNUM(IFAIL,BUF,N3,N2,NUM1,NUM2,FMT)

C -- SUBROUTINE GETNUM RETRIEVES CHARACTER POSITIONS FROM FIXED AND
C -- SEMI-FIXED OBSERVATION INSTRUCTIONS

        INTEGER N3,NUM1,NUM2,I,N2
        INTEGER IFAIL
        CHARACTER*(*) BUF
        CHARACTER*(*) FMT

        IFAIL=0
        I=INDEX(BUF(N3+1:N2),':')
        IF(I.EQ.0) GO TO 100
        WRITE(FMT,20) I-1
20      FORMAT('(I',I3,')')
        READ(BUF(N3+1:N3+I-1),FMT,ERR=100) NUM1
        N3=N3+I
        I=N2-N3
        IF(I.LT.1) GO TO 100
        WRITE(FMT,20) I
        READ(BUF(N3+1:N2),FMT,ERR=100) NUM2
        RETURN
100     IFAIL=1
        RETURN
        END




        SUBROUTINE NUMRD(IFAIL,ITYPE,LW,RW,IVAL,RVAL,CLINE)

C -- SUBROUTINE NUMRD READS A NUMBER FROM PART OF A STRING

        INTEGER IFAIL,RW,LW,LN,IVAL
        INTEGER ITYPE
        DOUBLE PRECISION RVAL
        CHARACTER*(*) CLINE
        CHARACTER*10 FMT

        IFAIL=0
        LN=RW-LW+1
        IF(ITYPE.EQ.0) THEN
          WRITE(FMT,10) LN
10        FORMAT('(I',I2,')')
          READ(CLINE(LW:RW),FMT,ERR=50) IVAL
        ELSE IF(ITYPE.EQ.1) THEN
          WRITE(FMT,20) LN
20        FORMAT('(F',I2,'.0)')
          READ(CLINE(LW:RW),FMT,ERR=50) RVAL
        END IF
        RETURN

50      IFAIL=1
        RETURN
        END
