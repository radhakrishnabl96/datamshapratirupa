	PROGRAM JACWRIT


#ifdef LAHEY
        IMPLICIT NONE
#endif

C -- Program JACWRIT translates a binary Jacobian "jco" file to an ascii file.

        INTEGER NESPAR,NXROW,IERR,J,I,IPP,IOBS,NBLC,NEWFLAG,IROW,IES,
     +  ICOUNT
        INTEGER NBLNK
        DOUBLE PRECISION DTEMP
        CHARACTER*12 AVERSION
        CHARACTER*120 JACFLE,OUTFLE
        CHARACTER*200 COMLIN


        DOUBLE PRECISION X(:,:)
        CHARACTER*12 APAR(:)
        CHARACTER*12 AOBS1(:)
        CHARACTER*20 AOBS(:)

        ALLOCATABLE::X,APAR,AOBS,AOBS1

        INCLUDE 'version.inc'
	WRITE(6,1) TRIM(AVERSION)
1       FORMAT(' JACWRIT Version ',A,'. ',
     +  'Watermark Numerical Computing.',/)

        NEWFLAG=0

#ifdef NO_CMDLINE
        WRITE(*,*)
        WRITE(*,3) ' Enter command line arguments > '
        READ(*,3) COMLIN
3       FORMAT(A)
#else
	CALL PGETCL(COMLIN)
#endif
        CALL SPACESUB(COMLIN)
        CALL SHIFTL(COMLIN)

        NBLC=NBLNK(COMLIN)
        IF(NBLC.EQ.0) THEN
          GO TO 9900
        END IF
        DO 10 J=1,NBLC
        IF(COMLIN(J:J).EQ.' ') GO TO 30
10      CONTINUE
        GO TO 9900
30      JACFLE=COMLIN(1:J-1)
        DO 40 I=J+1,NBLC
        IF(COMLIN(I:I).NE.' ') GO TO 50
40      CONTINUE
        GO TO 9900
50      DO 60 J=I,NBLC
        IF(COMLIN(J:J).EQ.' ') GO TO 9900
60      CONTINUE
        OUTFLE=COMLIN(I:NBLC)
        CALL REMCHAR(JACFLE,CHAR(211))
        CALL REMCHAR(OUTFLE,CHAR(211))

#ifdef CAPFILE
        CALL UPCAS(JACFLE)
        CALL UPCAS(OUTFLE)
#endif
#ifdef LAHEY
#ifdef LF90
        OPEN(UNIT=10,FILE=JACFLE,FORM='UNFORMATTED',
     +  ACCESS='TRANSPARENT',STATUS='OLD')
#else
        OPEN(UNIT=10,FILE=JACFLE,FORM='BINARY',STATUS='OLD')
#endif
#else
        OPEN(UNIT=10,FILE=JACFLE,FORM='UNFORMATTED',
     +  ACCESS='STREAM',STATUS='OLD')
#endif
        IF(IERR.NE.0)THEN
          WRITE(6,125) JACFLE(1:NBLNK(JACFLE))
125       FORMAT(/,' *** Cannot open file ',A,' ***',/)
          GO TO 9990
        END IF
        READ(10,ERR=9000,END=9100)NESPAR,NXROW
        IF(NESPAR.LT.0)THEN
          NESPAR=-NESPAR
          NXROW=-NXROW
          NEWFLAG=1
        ELSE
          NEWFLAG=0
        END IF

        IF(NEWFLAG.EQ.1)THEN
          ALLOCATE(X(NXROW,NESPAR),AOBS(NXROW),APAR(NESPAR),
     +    STAT=IERR)
        ELSE
          ALLOCATE(X(NXROW,NESPAR),AOBS1(NXROW),APAR(NESPAR),
     +    AOBS(NXROW),STAT=IERR)
        END IF
        IF(IERR.NE.0)THEN
          WRITE(6,200)
200       FORMAT(/,' *** Cannot allocate sufficient memory to ',
     +    'continue execution ***',/)
          GO TO 9990
        END IF

        IF(NEWFLAG.EQ.1)THEN
          X=0.0D0                ! AN ARRAY
          READ(10,ERR=9000,END=9100)ICOUNT
          DO I=1,ICOUNT
            READ(10,ERR=9000,END=9100) J,DTEMP
            IES=(J-1)/NXROW+1
            IROW=J-(IES-1)*NXROW
            X(IROW,IES)=DTEMP
          END DO
        ELSE
          READ(10,ERR=9000,END=9100) ((X(J,I),J=1,NXROW),I=1,NESPAR)
        END IF
        DO IPP=1,NESPAR
          READ(10,ERR=9000,END=9100) APAR(IPP)
        END DO
        IF(NEWFLAG.EQ.1)THEN
          DO IOBS=1,NXROW
            READ(10,ERR=9000,END=9100) AOBS(IOBS)
          END DO
        ELSE
          DO IOBS=1,NXROW
            READ(10,ERR=9000,END=9100) AOBS1(IOBS)
            AOBS(IOBS)=AOBS1(IOBS)
          END DO
        END IF
        CLOSE(UNIT=10)

        OPEN(UNIT=20,FILE=OUTFLE)
        I=0
331     CONTINUE
        WRITE(20,330) (APAR(IPP)(1:NBLNK(APAR(IPP))),
     +  IPP=I+1,MIN(NESPAR,I+8))
330     FORMAT(1X,22X,8(A12,2X))
        IF(I.GE.NESPAR) GO TO 335
        I=I+8
        GO TO 331
335     CONTINUE

        DO IOBS=1,NXROW
          WRITE(20,350) AOBS(IOBS)(1:NBLNK(AOBS(IOBS))),
     +    (X(IOBS,IPP),IPP=1,MIN(NESPAR,8))
350       FORMAT(1X,A20,2X,8(1X,1PG13.6))
          I=8
          IF(I.GE.NESPAR) GO TO 355
351       CONTINUE
          WRITE(20,360) (X(IOBS,IPP),IPP=I+1,MIN(I+8,NESPAR))
360       FORMAT(1X,22X,8(1X,1PG13.6))
          IF(I.GE.NESPAR) GO TO 355
          I=I+8
          GO TO 351
355       CONTINUE
        END DO
        CLOSE(UNIT=20)
        WRITE(6,370) OUTFLE(1:NBLNK(OUTFLE))
370     FORMAT(' - File ',A,' written ok.')
        GO TO 9995

9000    WRITE(6,9010) JACFLE(1:NBLNK(JACFLE))
9010    FORMAT(/,' *** Error encountered in reading file ',A,
     +  ' ***',/)
        GO TO 9990
9100    WRITE(6,9110) JACFLE(1:NBLNK(JACFLE))
9110    FORMAT(/,' *** Unexpected end encountered to file ',A,
     +  ' ***',/)
        GO TO 9990


9900    WRITE(6,9910,ERR=9000)
9910    FORMAT(' JACWRIT is run using the command:',/)
        WRITE(6,9920,ERR=9000)
9920    FORMAT('    jacwrit jacfile1 jacfile2',/,/,
     +  ' where',/)
        WRITE(6,9930,ERR=9000)
9930    FORMAT('    "jacfile1" is a PEST binary Jacobian file ',
     +  '(ext ".jco"), and')
        WRITE(6,9940,ERR=9000)
9940    FORMAT('    "jacfile2" is a text Jacobian file to be ',
     +  'written by JACWRIT.')

        GO TO 9990


9990    CONTINUE

        DEALLOCATE(X,APAR,AOBS,STAT=IERR)
        IF(NEWFLAG.EQ.0)THEN
          DEALLOCATE(AOBS1,STAT=IERR)
        END IF
        CALL EXIT(50)

9995    CONTINUE

        DEALLOCATE(X,APAR,AOBS,STAT=IERR)
        CALL EXIT(0)

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




