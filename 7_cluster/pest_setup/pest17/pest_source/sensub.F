


        SUBROUTINE EXTCHEK(EXTLEN,LENIN,INFLE)
 
C -- Subroutine EXTCHEK locates a filename extension.
 
        INTEGER EXTLEN,LENIN,I
        CHARACTER*(*) INFLE
 
        DO 10 I=LENIN,1,-1
        IF(INFLE(I:I).EQ.'.') GO TO 30
        IF((INFLE(I:I).EQ.CHAR(92)).OR.(INFLE(I:I).EQ.CHAR(47)))
     +  GO TO 50
10      CONTINUE
        EXTLEN=LENIN+1
        RETURN
30      EXTLEN=I
        RETURN
50      EXTLEN=LENIN+1
        RETURN
        END




        SUBROUTINE INTREAD(IFAIL,CLINE,ITEMP)
 
C -- Subroutine INTREAD reads an integer from a string.
 
        INTEGER IFAIL,ITEMP
        CHARACTER*8 AFMT
        CHARACTER*(*) CLINE
 
        IFAIL=0
        AFMT='(I     )'
        WRITE(AFMT(3:5),'(I3)') LEN(CLINE)
        READ(CLINE,AFMT,ERR=100) ITEMP
        RETURN
 
100     IFAIL=1
        RETURN
        END




         SUBROUTINE LINESPLIT(IFAIL,NUM,LW,RW,CLINE)
 
C -- Subroutine LINESPLIT splits a string into blank-delimited fragments.
 
        INTEGER IFAIL,NW,NBLC,J,I
        INTEGER NUM,NBLNK
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
     +    (ICHAR(CLINE(I:I)).NE.9)) GO TO 20
10      CONTINUE
        IFAIL=1
        RETURN
20      NW=NW+1
        LW(NW)=I
        DO 30 I=LW(NW)+1,NBLC
          IF((CLINE(I:I).EQ.' ').OR.(CLINE(I:I).EQ.',').OR.
     +    (ICHAR(CLINE(I:I)).EQ.9)) GO TO 40
30      CONTINUE
        RW(NW)=NBLC
        IF(NW.LT.NUM) IFAIL=1
        RETURN
40      RW(NW)=I-1
        J=RW(NW)
        GO TO 5
 
        END
 
 
        SUBROUTINE TABREM(CLINE)
 
C -- Subroutine TABREM removes tabs from a string.
 
        INTEGER I
        CHARACTER*(*) CLINE
 
        DO 10 I=1,LEN(CLINE)
10      IF(ICHAR(CLINE(I:I)).EQ.9) CLINE(I:I)=' '
 
        RETURN
        END




        SUBROUTINE LOWCASE(ASTRNG)
 
C -- Subroutine LOWCASE converts a string to lower case.
 
        INTEGER I,J,NBLNK
        CHARACTER*(*) ASTRNG
 
        DO 10 I=1,NBLNK(ASTRNG)
        J=ICHAR(ASTRNG(I:I))
        IF((J.GE.65).AND.(J.LE.90)) ASTRNG(I:I)=CHAR(J+32)
10      CONTINUE
        RETURN
        END




        SUBROUTINE LSHIFT(AA)
 
C -- Subroutine LSHIFT left-justifies text within a string.
 
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




        SUBROUTINE REALREAD(IFAIL,CLINE,RTEMP)
 
C -- Subroutine REALREAD reads a real number from a string.
 
        INTEGER IFAIL
        REAL RTEMP
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



	subroutine pestwait(nsec)

	implicit none
	
	integer nsec


	integer ddate(8),iticks,iticks1

	call date_and_time(values=ddate)
	iticks=ddate(5)*360000+ddate(6)*6000+ddate(7)*100+ddate(8)/10
10	call date_and_time(values=ddate)
	iticks1=ddate(5)*360000+ddate(6)*6000+ddate(7)*100+ddate(8)/10
	if(iticks1.lt.iticks) iticks1=iticks1+8640000
	if(iticks1.lt.iticks+nsec) go to 10

	return
	end





        SUBROUTINE WRITERR
 
C -- Subroutine WRITERR formats and writes an error message.
 
        INTEGER IERR,J,JEND,I,NBLC,ITAKE,MCASE,NBLNK
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
#ifdef SPACE 
            WRITE(6,'(A)',ERR=200) ' '//ERRMSG(J+1:I)
#else
            WRITE(6,'(A)',ERR=200) ERRMSG(J+1:I)
#endif
            ITAKE=4
          ELSE
#ifdef SPACE
            WRITE(6,'(A)',ERR=200) '     '//ERRMSG(J+1:I)
#else
            WRITE(6,'(A)',ERR=200) '    '//ERRMSG(J+1:I)
#endif
          END IF
          J=I
          GO TO 5
        END IF
10      CONTINUE
        IF(ITAKE.EQ.0)THEN
#ifdef SPACE
          WRITE(6,'(A)',ERR=200) ' '//ERRMSG(J+1:JEND)
#else
          WRITE(6,'(A)',ERR=200) ERRMSG(J+1:JEND)
#endif
          ITAKE=4
        ELSE
#ifdef SPACE
          WRITE(6,'(A)',ERR=200) '     '//ERRMSG(J+1:JEND)
#else
          WRITE(6,'(A)',ERR=200) '    '//ERRMSG(J+1:JEND)
#endif
        END IF
        J=JEND
        GO TO 5
100     JEND=NBLC
        IF(ITAKE.EQ.0)THEN
#ifdef SPACE
          WRITE(6,'(A)',ERR=200) ' '//ERRMSG(J+1:JEND)
#else
          WRITE(6,'(A)',ERR=200) ERRMSG(J+1:JEND)
#endif
        ELSE
#ifdef SPACE
          WRITE(6,'(A)',ERR=200) '     '//ERRMSG(J+1:JEND)
#else
          WRITE(6,'(A)',ERR=200) '    '//ERRMSG(J+1:JEND)
#endif
        END IF
        RETURN
 
200     STOP
        END




        SUBROUTINE WRITINT(ATEMP,IVAL)
 
C -- Subroutine WRITINT writes an integer to a string.
 
        INTEGER IVAL
        CHARACTER*6 AFMT
        CHARACTER*(*) ATEMP
 
        AFMT='(I   )'
        WRITE(AFMT(3:5),'(I3)') LEN(ATEMP)
        WRITE(ATEMP,AFMT)IVAL
        CALL LSHIFT(ATEMP)
        RETURN
        END
	subroutine writlin(iline,mmline,afile,aaline)

	integer iline,mmline,nblnk
	character*(*) aaline,afile
	character*10 anum

	call writint(anum,iline)
	write(aaline,10) anum(1:nblnk(anum)),afile(1:nblnk(afile))
10	format('Line ',a,' of file ',a,':')
	mmline=nblnk(aaline)+1

	return
	end
