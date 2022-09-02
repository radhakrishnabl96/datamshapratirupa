
        PROGRAM TMPCHK

C -- Number of parameters
        INTEGER ZNPAR
        PARAMETER (ZNPAR = 40000)

        INTEGER I,J,NBLC,MAXWID,IERR,MCASE,LENIN,LENOUT,CHKTYP,II,
     +  J2,J1,IFAIL,EXTLEN,LLW,LENMOD,MMLINE,NPAR,MAXPAR,LENPAR,PRECIS,
     +  NOPNT,JERR
        INTEGER LW(:),L(4),R(4)
        INTEGER JLINE,IFAIL4,ILINE
        DOUBLE PRECISION PVAL(:),SCALE(:),OFFSET(:)
        CHARACTER*12 AVERSION
        CHARACTER*10 AMXWID,ATEMP*30,TPAR*12,AMXPAR,PARDEL*1,FILADD*1
        CHARACTER*130 COMLIN,TMPFL*80,OUTFLE*80,MODFLE*80,
     +  PARFLE*80
        CHARACTER*23 PWORD(:)
        CHARACTER*200 AALINE
        CHARACTER*12 APAR(:)
C -- File size
        CHARACTER*2120 ERRMSG
        CHARACTER*3010 CLINE

        ALLOCATABLE:: APAR,LW,PVAL,SCALE,OFFSET,PWORD
        INTEGER NBLNK

        COMMON /IERR/IERR,MCASE
        COMMON /MMLINE/MMLINE
        COMMON /ERRMSG/ERRMSG
        COMMON /AALINE/AALINE

        FILADD=' '
        NPAR=0
        IERR=0
C -- File size
        MAXWID=3000
        CALL WRTINT(AMXWID,MAXWID)
        MAXPAR=ZNPAR
        CALL WRTINT(AMXPAR,MAXPAR)

        INCLUDE 'version.inc'
        WRITE(6,1,ERR=9000) TRIM(AVERSION)
1       FORMAT(' TEMPCHEK Version ',A,'. ',
     +  'Watermark Numerical Computing.',/)



        ALLOCATE(APAR(MAXPAR),LW(MAXPAR),STAT=IFAIL4)
        IF(IFAIL4.NE.0) THEN
          WRITE(ERRMSG,5)
5         FORMAT(' Cannot allocate sufficient memory to run ',
     +    'TEMPCHEK.')
          CALL WRTERR
          GO TO 9800
        END IF

        DO 10 I=1,MAXPAR
10      LW(I)=1000

C -- THE COMMAND LINE IS PARSED AND ANALYSED

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
        DO 20 I=NBLC,NBLC-2,-1
        IF(COMLIN(I:I).EQ.'/')THEN
          FILADD=COMLIN(I+1:I+1)
          COMLIN(I:NBLC)=' '
          NBLC=NBLNK(COMLIN)
          GO TO 25
        END IF
20      CONTINUE
25      CONTINUE
        DO 50 I=1,NBLC
        IF(COMLIN(I:I).NE.' ') GO TO 60
50      CONTINUE
        GO TO 9900
60      DO 70 J=I,NBLC
        IF(COMLIN(J:J).EQ.' ') GO TO 90
70      CONTINUE
        CHKTYP=1
        TMPFL=COMLIN(I:NBLC)
        LENIN=NBLC-I+1
        GO TO 200
90      J=J-1
        TMPFL=COMLIN(I:J)
        LENIN=J-I+1
        DO 100 I=J+1,NBLC
        IF(COMLIN(I:I).NE.' ') GO TO 110
100     CONTINUE
110     DO 120 J=I,NBLC
        IF(COMLIN(J:J).EQ.' ') GO TO 130
120     CONTINUE
        CHKTYP=2
        MODFLE=COMLIN(I:NBLC)
        LENMOD=NBLC-I+1
        GO TO 200
130     J=J-1
        MODFLE=COMLIN(I:J)
        LENMOD=J-I+1
        DO 150 I=J+1,NBLC
        IF(COMLIN(I:I).NE.' ') GO TO 170
150     CONTINUE
170     DO 180 J=I,NBLC
        IF(COMLIN(J:J).EQ.' ') GO TO 9900
180     CONTINUE
        CHKTYP=3
        LENPAR=NBLC-I+1
        PARFLE=COMLIN(I:NBLC)

200     CONTINUE
        CALL REMCHAR(TMPFL,CHAR(211))
c        CALL SHIFTL(TMPFL)
        CALL REMCHAR(MODFLE,CHAR(211))
c        CALL SHIFTL(MODFLE)
        CALL REMCHAR(PARFLE,CHAR(211))
c        CALL SHIFTL(PARFLE)

#ifdef CAPFILE
        CALL UPCAS(TMPFL)
        CALL UPCAS(MODFLE)
        CALL UPCAS(PARFLE)
#endif
        CALL EXTCHK(EXTLEN,LENIN,TMPFL)
        IF(EXTLEN.LT.LENIN)THEN
#ifdef CAPFILE
          IF(TMPFL(EXTLEN:LENIN).EQ.'.PMT')THEN
            WRITE(ERRMSG,202)
202         FORMAT(' Template file must not have an extension of ',
     +      '".PMT".')
#else
          IF(TMPFL(EXTLEN:LENIN).EQ.'.pmt')THEN
            WRITE(ERRMSG,202)
202         FORMAT(' Template file must not have an extension of ',
     +      '".pmt".')
#endif
            CALL WRTERR
            GO TO 9800
          END IF
        END IF
c        IF(LENIN.GT.51) THEN
c          WRITE(ERRMSG,220)
c220       FORMAT(' Name of template file must not exceed 51 ',
c     +    'characters.')
c          CALL WRTERR
c          GO TO 9800
c        END IF

C -- THE TEMPLATE FILE IS OPENED AND CHECKED

#ifdef UNICOS
        OPEN(UNIT=40,FILE=TMPFL,STATUS='OLD',RECL=3000,ERR=230)
#else
        OPEN(UNIT=40,FILE=TMPFL,STATUS='OLD',ERR=230)
#endif
        GO TO 250
230     WRITE(ERRMSG,240) TMPFL(:NBLNK(TMPFL))
240     FORMAT(' Cannot open template file ',A,'.')
        CALL WRTERR
        GO TO 9800
250     CONTINUE
        IF(CHKTYP.EQ.2) THEN
          PARFLE=TMPFL
          DO 205 I=EXTLEN,LEN(TMPFL)
          PARFLE(I:I)=' '
205       CONTINUE
          PARFLE(EXTLEN:EXTLEN+3)='.par'
#ifdef CAPFILE
          CALL UPCAS(PARFLE)
#endif
          LENPAR=NBLNK(PARFLE)
        END IF

C -- IF REQUIRED, THE PARAMETER VALUE FILE IS OPENED

        IF((CHKTYP.EQ.2).OR.(CHKTYP.EQ.3))THEN
          IF(MODFLE(:NBLNK(MODFLE)).EQ.TMPFL(:NBLNK(TMPFL)))THEN
            WRITE(ERRMSG,212)
212         FORMAT(' Model input file must not have the same name as ',
     +      'the template file.')
            CALL WRTERR
            GO TO 9800
          END IF
          IF(MODFLE(:NBLNK(MODFLE)).EQ.PARFLE(:NBLNK(PARFLE)))THEN
            WRITE(ERRMSG,213)
213         FORMAT(' Model input file must not have same name as the ',
     +      'parameter value file.')
            CALL WRTERR
            GO TO 9800
          END IF
#ifdef UNICOS
          OPEN(UNIT=30,FILE=PARFLE,STATUS='OLD',RECL=2000,
     +    ERR=207)
#else
          OPEN(UNIT=30,FILE=PARFLE,STATUS='OLD',ERR=207)
#endif
          GO TO 209
207       WRITE(ERRMSG,208) PARFLE(:NBLNK(PARFLE))
208       FORMAT(' Cannot open parameter value file ',A,'.')
          CALL WRTERR
          GO TO 9800
        END IF
209     CONTINUE

C -- THE TEMPLATE FILE IS READ AND CHECKED

        WRITE(6,210,ERR=9000) TMPFL(:NBLNK(TMPFL))
210     FORMAT(' Errors in file ',A,' ----->')
1840    JLINE=1
        CALL WRTINT(ATEMP,JLINE)
        AALINE=' Line '//ATEMP(1:INDEX(ATEMP,' ')-1)//' of template '//
     +  'file '//TMPFL(1:LENIN)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(40,'(A)',ERR=2300,END=2400) CLINE
        CALL LOWCAS(CLINE)
        IF((CLINE(1:3).EQ.'pif').OR.(CLINE(1:3).EQ.'jif'))THEN
          WRITE(ERRMSG,1845) TMPFL(1:LENIN)
1845      FORMAT(' File ',A,' is an instruction file: a template ',
     +    'file is expected.')
          CALL WRTERR
          GO TO 9800
        END IF
        IF(((CLINE(1:4).NE.'ptf ').AND.
     +      (CLINE(1:4).NE.'jtf ')).OR.(NBLNK(CLINE).LT.5))THEN
          WRITE(ERRMSG,1850) AALINE(1:MMLINE)
1850      FORMAT(A,'first line must be "ptf" or "jtf" + ',
     +    'parameter delimiter.')
          CALL WRTERR
          GO TO 9800
        END IF
        PARDEL=CLINE(5:5)
        IF(PARDEL.EQ.' ')THEN
          WRITE(ERRMSG,1860) AALINE(1:MMLINE)
1860      FORMAT(A,'parameter delimiter not found in fifth character ',
     +    'position.')
          CALL WRTERR
          GO TO 9800
        END IF
        II=ICHAR(PARDEL)
        IF(((II.GE.48).AND.(II.LE.57)).OR.
     +     ((II.GE.97).AND.(II.LE.122)))THEN
          WRITE(ERRMSG,1870) AALINE(1:MMLINE)
1870      FORMAT(A,'illegal parameter delimiter.')
          CALL WRTERR
          GO TO 9800
        END IF
1880    JLINE=JLINE+1
        CALL WRTINT(ATEMP,JLINE)
        AALINE=' Line '//ATEMP(1:INDEX(ATEMP,' ')-1)//' of template '//
     +  'file '//TMPFL(1:LENIN)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(40,'(A)',ERR=2300,END=2400) CLINE
1900    NBLC=NBLNK(CLINE)
        IF(NBLC.GT.MAXWID)THEN
          WRITE(ERRMSG,1915) AALINE(1:MMLINE),AMXWID(:NBLNK(AMXWID))
1915      FORMAT(A,'line exceeds ',A,' characters.')
          CALL WRTERR
          GO TO 9800
        END IF
        J2=0
1920    IF(J2.GE.NBLC) GO TO 1880
        J1=INDEX(CLINE(J2+1:NBLC),PARDEL)
        IF(J1.EQ.0) GO TO 1880
        J1=J1+J2
        J2=INDEX(CLINE(J1+1:NBLC),PARDEL)
        IF(J2.EQ.0)THEN
          WRITE(ERRMSG,1930) AALINE(1:MMLINE)
1930      FORMAT(A,'missing parameter delimiter.')
          CALL WRTERR
          GO TO 1880
        END IF
        J2=J2+J1
        IF(INDEX(CLINE(J1:J2),CHAR(9)).NE.0)THEN
          WRITE(ERRMSG,1931) AALINE(1:MMLINE),CLINE(J1:J2)
1931      FORMAT(A,'tab character embedded in parameter space ("',A,
     +    '"); use space characters.')
          CALL WRTERR
          GO TO 1920
        END IF
        CALL PRCHK1(IFAIL,J1,J2,TPAR,CLINE)
        IF(IFAIL.NE.0) GO TO 1920
        IF(NPAR.EQ.0) THEN
          NPAR=1
          CALL LOWCAS(TPAR)
          APAR(1)=TPAR
          II=1
        ELSE
          CALL WHICH1(IFAIL4,NPAR,II,APAR,TPAR)
          IF(IFAIL4.NE.0) THEN
            NPAR=NPAR+1
            IF(NPAR.GT.MAXPAR) THEN
              WRITE(ERRMSG,1935) AMXPAR(:NBLNK(AMXPAR)),
     +        TMPFL(1:LENIN)
1935          FORMAT(' Program TEMPCHEK can handle only ',A,
     +        ' different parameters: more than this are cited in ',
     +        'file ',A,'.')
              CALL WRTERR
              GO TO 9800
            END IF
            APAR(NPAR)=TPAR
            II=NPAR
          END IF
        END IF
        LLW=J2-J1+1
        IF(LLW.LT.LW(II))LW(II)=LLW
        GO TO 1920

2300    WRITE(ERRMSG,2310) AALINE(1:MMLINE)
2310    FORMAT(A,'cannot read line.')
        CALL WRTERR
        IF(JLINE.EQ.1) GO TO 9800
        GO TO 1880
2400    IF(JLINE.EQ.1)THEN
          WRITE(ERRMSG,2410) AALINE(1:MMLINE)
2410      FORMAT(A,'unexpected end to file.')
          CALL WRTERR
          GO TO 9800
        END IF

C -- PARAMETER NAMES ARE WRITTEN TO THE OUTPUT FILE

        IF(NPAR.EQ.0)THEN
          WRITE(ERRMSG,295) TMPFL(:NBLNK(TMPFL))
295       FORMAT(' No parameters identified in file ',A,'.')
          CALL WRTERR
        END IF
        IF(IERR.NE.0) GO TO 9800
        WRITE(6,300,ERR=9000)
300     FORMAT(' No errors encountered.')
        IF(CHKTYP.GE.2) GO TO 500
        OUTFLE=TMPFL
        DO 310 I=EXTLEN,LEN(TMPFL)
        OUTFLE(I:I)=' '
310     CONTINUE
        OUTFLE(EXTLEN:EXTLEN+3)='.pmt'
        IF(FILADD.NE.' ') OUTFLE='pest'//FILADD//'.tmp'
#ifdef CAPFILE
        CALL UPCAS(OUTFLE)
#endif
        LENOUT=NBLNK(OUTFLE)
c        IF(LENOUT.GT.51) THEN
c          WRITE(ERRMSG,330) OUTFLE(:NBLNK(OUTFLE))
c330       FORMAT(' Cannot write file ',A,': name length limited ',
c     +    'to 51 characters.')
c          CALL WRTERR
c          IF(CHKTYP.GE.2) GO TO 500
c          GO TO 9800
c        END IF
        OPEN(UNIT=50,FILE=OUTFLE,STATUS='UNKNOWN',ERR=335)
        GO TO 350
335     WRITE(ERRMSG,340) OUTFLE(:NBLNK(OUTFLE)),
     +  TMPFL(:NBLNK(TMPFL))
340     FORMAT(' Cannot open file ',A,' to list parameters ',
     +  'identified in template file ',A,'.')
        CALL WRTERR
        IF(CHKTYP.GE.2) GO TO 500
        GO TO 9800
350     DO 370 I=1,NPAR
        WRITE(50,'(1X,A)',ERR=9700) APAR(I)
370     CONTINUE
        CLOSE(UNIT=50)
        WRITE(6,*,ERR=9000)
        CALL WRTINT(ATEMP,NPAR)
        WRITE(ERRMSG,380) ATEMP(:NBLNK(ATEMP)),
     +  TMPFL(:NBLNK(TMPFL)),
     +  OUTFLE(:NBLNK(OUTFLE))
380     FORMAT(' ',A,' parameters identified in file ',A,
     +  ': these are listed in file ',A,'.')
        CALL WRTERR
        WRITE(6,*,ERR=9000)
        IERR=IERR-1
        IF(CHKTYP.GE.2) GO TO 500
        GO TO 9999

500     ALLOCATE(PVAL(NPAR),SCALE(NPAR),OFFSET(NPAR),PWORD(NPAR),
     +  STAT=IFAIL4)
        IF(IFAIL4.NE.0)THEN
          WRITE(ERRMSG,502)
502       FORMAT(' Insufficient memory available to continue ',
     +    'TEMPCHEK execution.')
          CALL WRTERR
          GO TO 9820
        END IF

C -- THE PARAMETER VALUE FILE IS READ AND CHECKED

        ILINE=0
        WRITE(6,*,ERR=9000)
        WRITE(6,505,ERR=9000) PARFLE(:NBLNK(PARFLE))
505     FORMAT(' Errors in parameter value file ',A,' ----->')
510     ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        AALINE=' Line '//ATEMP(1:INDEX(ATEMP,' ')-1)//
     +  ' of parameter value file '//PARFLE(1:LENPAR)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=520,END=700) CLINE
        GO TO 550
520     WRITE(ERRMSG,530) AALINE(1:MMLINE)
530     FORMAT(A,'cannot read line.')
        CALL WRTERR
        IF(ILINE.EQ.1) GO TO 9820
        GO TO 510
550     IF(ILINE.EQ.1) THEN
          CALL LOWCAS(CLINE)
          CALL LNSPL1(IFAIL,2,L,R,CLINE)
          IF(IFAIL.NE.0) THEN
            WRITE(ERRMSG,560) AALINE(1:MMLINE)
560         FORMAT(A,'line must contain character variables PRECIS',
     +      ' and DPOINT.')
            CALL WRTERR
            GO TO 510
          END IF
          JERR=0
          IF(CLINE(L(1):R(1)).EQ.'single') THEN
            PRECIS=0
          ELSE IF(CLINE(L(1):R(1)).EQ.'double') THEN
            PRECIS=1
          ELSE
            WRITE(ERRMSG,561) AALINE(1:MMLINE)
561         FORMAT(A,'character variable PRECIS must be "single"',
     +      ' or "double".')
            CALL WRTERR
            JERR=1
          END IF
          IF(CLINE(L(2):R(2)).EQ.'point') THEN
            NOPNT=0
          ELSE IF(CLINE(L(2):R(2)).EQ.'nopoint') THEN
            NOPNT=1
          ELSE
            WRITE(ERRMSG,562) AALINE(1:MMLINE)
562         FORMAT(A,'character variable DPOINT must be "point" or ',
     +      '"nopoint".')
            CALL WRTERR
            JERR=1
          END IF
          IF(JERR.EQ.1) GO TO 9820
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
        CALL WHICH1(IFAIL4,NPAR,II,APAR,TPAR)
        IF(IFAIL4.NE.0) THEN
          WRITE(ERRMSG,580) TPAR(:NBLNK(TPAR)),
     +    PARFLE(:NBLNK(PARFLE)),
     +    TMPFL(:NBLNK(TMPFL))
580       FORMAT(' Warning: parameter "',A,'" from parameter value ',
     +    'file ',A,' not cited in template file ',A,'.')
          CALL WRTERR
          IERR=IERR-1
          GO TO 510
        ELSE
          IF(LW(II).LT.0) THEN
            WRITE(ERRMSG,590) AALINE(1:MMLINE),TPAR(:NBLNK(TPAR))
590         FORMAT(A,'parameter "',A,'" cited previously.')
            CALL WRTERR
            GO TO 510
          ELSE
            LW(II)=-LW(II)
            ATEMP=CLINE(L(2):R(2))
            READ(ATEMP,'(F30.0)',ERR=610) PVAL(II)
            GO TO 650
610         WRITE(ERRMSG,620) AALINE(1:MMLINE),TPAR(:NBLNK(TPAR))
620         FORMAT(A,'cannot read value of parameter "',A,'".')
            CALL WRTERR
650         ATEMP=CLINE(L(3):R(3))
            READ(ATEMP,'(F30.0)',ERR=670) SCALE(II)
            GO TO 680
670         WRITE(ERRMSG,675) AALINE(1:MMLINE), TPAR(:NBLNK(TPAR))
675         FORMAT(A,'cannot read scale for parameter "',A,'".')
            CALL WRTERR
680         IF(SCALE(II).EQ.0.0D0)THEN
              WRITE(ERRMSG,681) AALINE(1:MMLINE),TPAR(:NBLNK(TPAR))
681           FORMAT(A,'scale for parameter "',A,'" cannot be zero.')
              CALL WRTERR
            END IF
            ATEMP=CLINE(L(4):R(4))
            READ(ATEMP,'(F30.0)',ERR=690) OFFSET(II)
            GO TO 695
690         WRITE(ERRMSG,685) AALINE(1:MMLINE),TPAR(:NBLNK(TPAR))
685         FORMAT(A,'cannot read offset for parameter "',A,'".')
            CALL WRTERR
695         GO TO 510
          END IF
        END IF

700     IF(IERR.NE.0) GO TO 9820
        DO 750 I=1,NPAR
        IF(LW(I).GT.0) THEN
          WRITE(ERRMSG,710) APAR(I)(:NBLNK(APAR(I))),
     +    TMPFL(:NBLNK(TMPFL)),
     +    PARFLE(:NBLNK(PARFLE))
710       FORMAT(' Parameter "',A,'" cited in template file ',
     +    A,' not listed in parameter value file ',A,'.')
          CALL WRTERR
        ELSE
          LW(I)=-LW(I)
        END IF
750     CONTINUE
        CLOSE(UNIT=30)
        IF(IERR.NE.0) GO TO 9820
        WRITE(6,760,ERR=9000)
760     FORMAT(' No errors encountered.')
        REWIND(UNIT=40,ERR=9100)

C -- THE MODEL INPUT FILE IS WRITTEN USING VALUES FROM THE PARAMETER
C -- VALUE FILE

        CALL INWRT1(IFAIL,NPAR,PRECIS,NOPNT,LW,PVAL,SCALE,OFFSET,
     +  PARDEL,PWORD,MODFLE,TMPFL,APAR,CLINE)
        IF(IFAIL.NE.0) GO TO 9820
        WRITE(6,780,ERR=9000) MODFLE(:NBLNK(MODFLE))
780     FORMAT(' File ',A,' written ok.')
        WRITE(6,*,ERR=9000)
        GO TO 9999

9000    CALL EXIT(100)
9100    WRITE(ERRMSG,9110) TMPFL(:NBLNK(TMPFL))
9110    FORMAT(' Cannot rewind file ',A,'.')
        CALL WRTERR
        GO TO 9820
9700    WRITE(ERRMSG,9710) OUTFLE(:NBLNK(OUTFLE))
9710    FORMAT(' Cannot write file ',A,': disk full?')
        CALL WRTERR
        GO TO 9800
9800    WRITE(6,*,ERR=9000)
C        OPEN(UNIT=35,FILE='pest.tmp',STATUS='UNKNOWN',ERR=9000)
C        WRITE(35,'(A)',ERR=9000)'1'
C        CLOSE(UNIT=35,ERR=9000)
9820    CALL EXIT(50)
9900    WRITE(6,9910,ERR=9000)
9910    FORMAT(' TEMPCHEK is run using the command:',/)
        WRITE(6,9920,ERR=9000)
9920    FORMAT('    tempchek tempfile [modfile [parfile]]',/,/,
     +  ' where',/)
        WRITE(6,9930,ERR=9000)
9930    FORMAT('    "tempfile" is a PEST template file,')
        WRITE(6,9940,ERR=9000)
9940    FORMAT('    "modfile" is an [optional] model input file ',
     +  'to be written by TEMPCHEK, and')
        WRITE(6,9950,ERR=9000)
9950    FORMAT('    "parfile" is an [optional] parameter value file.')
        GO TO 9800

9999    CONTINUE
C        OPEN(UNIT=35,FILE='pest.tmp',STATUS='UNKNOWN',ERR=9000)
C        WRITE(35,'(A)',ERR=9000)'0'
C        CLOSE(UNIT=35,ERR=9000)
9998    CALL EXIT(0)
        END


        SUBROUTINE EXTCHK(EXTLEN,LENIN,INFLE)

C -- SUBROUTINE EXTCHK DETERMINES THE LOCATION OF THE FILENAME
C -- EXTENSION

        INTEGER EXTLEN,LENIN,I
        CHARACTER*(*) INFLE

        DO 10 I=LENIN,1,-1
        IF(INFLE(I:I).EQ.'.') GO TO 30
#ifdef UNIX
        IF(INFLE(I:I).EQ.'/') GO TO 50
#else
        IF(INFLE(I:I).EQ.CHAR(92)) GO TO 50
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


        SUBROUTINE SHFTL1(AA)

C -- SUBROUTINE SHFTL1 REMOVES LEADING BLANK CHARACTERS FROM A STRING

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



        SUBROUTINE PRCHK1(IFAIL,J1,J2,TPAR,CLINE)

C -- SUBROUTINE PRCHK1 CHECKS THE INTEGRITY OF A PARAMETER SPACE

        INTEGER J1,J2,I,J,IFAIL,MMLINE,K,L
        CHARACTER*12 TPAR
        CHARACTER*100 ERRMSG*2120,AALINE*200
        CHARACTER*(*) CLINE

        COMMON /ERRMSG/ ERRMSG
        COMMON /AALINE/ AALINE
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


        SUBROUTINE INWRT1(JFAIL,NPAR,PRECIS,NOPNT,NW,PVAL,SCALE,
     +  OFFSET,MARK,PWORD,INFILE,INTEMP,APAR,CLINE)

C -- SUBROUTINE INWRT1 WRITES A MODEL INPUT FILE USING CURRENT
C -- PARAMETER VALUES

        INTEGER NBLNK
        INTEGER NPAR,PRECIS,J2,LC,J1,IPAR,ISC,NOPNT,MMLINE,
     +  J,IERR,MCASE,JFAIL
        INTEGER NW(NPAR)
        INTEGER IFAIL,IPP,ILINE
        DOUBLE PRECISION TVAL
        DOUBLE PRECISION PVAL(NPAR),SCALE(NPAR),OFFSET(NPAR)
        CHARACTER*12 TPAR
        CHARACTER*(*) CLINE
        CHARACTER*(*) PWORD(NPAR),APAR(NPAR)
        CHARACTER*(*) INFILE,INTEMP
        CHARACTER MARK
        CHARACTER*2120 ERRMSG,AALINE*200

        COMMON /IERR/ IERR,MCASE
        COMMON /ERRMSG/ ERRMSG
        COMMON /AALINE/ AALINE
        COMMON /MMLINE/ MMLINE

        JFAIL=0
        WRITE(6,*,ERR=9900)
        WRITE(6,10,ERR=9900) INFILE(:NBLNK(INFILE))
10      FORMAT(' Writing model input file ',A,' ----->')

C -- FIRST EACH OF THE PARAMETER WORDS IS FILLED

        IPAR=1
        DO 100 IPP=1,NPAR
c       isc=0
        IF((SCALE(IPP).NE.1.0D0).OR.(OFFSET(IPP).NE.0.0D0))THEN
          PVAL(IPP)=PVAL(IPP)*SCALE(IPP)+OFFSET(IPP)
c         isc=1
        END IF
        CALL WRTSIG(IFAIL,PVAL(IPP),PWORD(IPP),NW(IPP),PRECIS,TVAL,
     +  NOPNT)
        IF(IFAIL.NE.0)THEN
          WRITE(AALINE,15) APAR(IPP)(:NBLNK(APAR(IPP)))
15        FORMAT(' Error writing parameter "',A,'": ')
          MMLINE=NBLNK(AALINE)+1
          IF(IFAIL.EQ.1)THEN
            WRITE(ERRMSG,30) AALINE(1:MMLINE)
30          FORMAT(A,'number too large or small for single precision',
     +      ' protocol.')
          ELSE IF(IFAIL.EQ.2) THEN
            WRITE(ERRMSG,40) AALINE(1:MMLINE)
40          FORMAT(A,'number too large or small for double precision',
     +      ' protocol.')
          ELSE IF(IFAIL.EQ.3) THEN
            WRITE(ERRMSG,50) AALINE(1:MMLINE)
50          FORMAT(A,'field width too small to represent number.')
          ELSE
            WRITE(ERRMSG,20) AALINE(1:MMLINE)
20          FORMAT(A,'internal error.')
          END IF
          CALL WRTERR
        END IF
c       if(isc.eq.1) then
c         pval(ipp)=(tval-offset(ipp))/scale(ipp)
c       else
c         pval(ipp)=tval
c       end if
100     CONTINUE
        IF(IERR.NE.0) GO TO 9800

C -- NEXT THE SUBSTITUTIONS ARE MADE IN THE TEMPLATE FILES

#ifdef UNICOS
        OPEN(UNIT=50,FILE=INFILE,STATUS='UNKNOWN',RECL=3000,
     +  ERR=110)
#else
        OPEN(UNIT=50,FILE=INFILE,STATUS='UNKNOWN',ERR=110)
#endif
        GO TO 115
110     WRITE(ERRMSG,111) INFILE(:NBLNK(INFILE))
111     FORMAT(' Cannot open file ',A,' for output.')
        CALL WRTERR
        GO TO 9800
115     READ(40,*)
        ILINE=1
120     ILINE=ILINE+1
        READ(40,'(A)',ERR=310,END=400) CLINE
        LC=NBLNK(CLINE)
        J2=0
150     IF(J2.GE.LC) GO TO 300
        J1=INDEX(CLINE(J2+1:LC),MARK)
        IF(J1.EQ.0) GO TO 300
        J1=J1+J2
        J2=INDEX(CLINE(J1+1:LC),MARK)
        J2=J2+J1
        CALL PARNAM(IFAIL,J1,J2,TPAR,CLINE)
        CALL WHICH1(IFAIL,NPAR,IPAR,APAR,TPAR)
c       The following works when space bigger than pword(:nblnk(pword))
c       cline(j1:j2)=pword(ipar)(:nblnk(pword(ipar)))
        DO 160 J=J1,J2
160     CLINE(J:J)=' '
        J=NBLNK(PWORD(IPAR))
        CLINE(J2-J+1:J2)=PWORD(IPAR)(1:J)
        GO TO 150

300     WRITE(50,'(A)',ERR=320) CLINE(:NBLNK(CLINE))
        GO TO 120
310     WRITE(ERRMSG,315) INTEMP(:NBLNK(INTEMP))
315     FORMAT(' Error re-reading template file ',A,'.')
        CALL WRTERR
        GO TO 9800
320     WRITE(ERRMSG,330) INFILE(:NBLNK(INFILE))
330     FORMAT(' Cannot write to file ',A,': disk full?')
        CALL WRTERR
        GO TO 9800
400     CLOSE(UNIT=50)
        CLOSE(UNIT=40)
        RETURN
9800    JFAIL=1
        RETURN
9900    CALL EXIT(100)
        END


        SUBROUTINE PARNAM(IFAIL,J1,J2,TPAR,CLINE)

C -- SUBROUTINE PARNAM READS A PARAMETER NAME FROM A STRING

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


