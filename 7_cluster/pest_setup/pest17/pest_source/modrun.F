
C -- Required since UNICOS system equivalent is a function. LJB 27/7/95

        SUBROUTINE IOCTL(JFAIL,NPAR,NINFLE,NOUFL,NUML,ASIZE,NINSTR,
     +  NBLBMX,NW,TMPFLE,INFLE,INSFLE,OUTFLE,PARDEL,MRKDEL,APAR,
     +  CLINE,JLINE,ICODE)

        USE PESTDATA, ONLY : INST

C -- SUBROUTINE IOCTL READS THE INSTRUCTION FILES AND ALLOCATES SPACE FOR
C -- INTERNAL STORAGE OF INSTRUCTIONS AS WELL AS TEMPLATE FILES, CHECKING
C -- THEN FOR INTEGRITY

        INTEGER ASIZE,NINSTR,I,ILINE,IFAIL,JLINE,JFAIL
        INTEGER NINFLE,NOUFL,J,NBLC,NUML,NPAR,J1,J2,IPAR,NNW,
     +  NBLBMX
        INTEGER ICODE
        INTEGER NW(NPAR)
        CHARACTER*(*) TMPFLE(NINFLE),INFLE(NINFLE),PARDEL(NINFLE),
     +  INSFLE(NOUFL),OUTFLE(NOUFL),MRKDEL(NOUFL),APAR(NPAR)
        CHARACTER*12 TPAR
        CHARACTER*(*) CLINE

        JFAIL=0
        IF(ICODE.EQ.2) GO TO 301

        NBLBMX=0
        ASIZE=0
        NUML=0
        NINSTR=0
        DO 200 I=1,NOUFL
        CALL FFOPEN(JFAIL,INST,'r',INSFLE(I),78,CLINE)
        IF(JFAIL.NE.0) GO TO 9891
        READ(INST,'(A)',END=9300) CLINE
        CALL TABREM(CLINE)
        CALL LOWCAS(CLINE(1:3))
        IF((CLINE(1:3).NE.'pif').AND.(CLINE(1:3).NE.'jif'))GO TO 9300
        MRKDEL(I)=CLINE(5:5)
        IF(MRKDEL(I).EQ.' ') GO TO 9300
50      READ(INST,'(A)',END=180) CLINE
        CALL TABREM(CLINE)
        IF(INDEX(CLINE,MRKDEL(I)).EQ.0) CALL CMPRSS(CLINE)
        NBLC=LEN_TRIM(CLINE)
        IF(NBLC.EQ.0) GO TO 50
        IF(NBLC.GT.NBLBMX)NBLBMX=NBLC
        NINSTR=NINSTR+1
        DO 60 J=1,NBLC
        IF(CLINE(J:J).NE.' ') THEN
          IF((CLINE(J:J).EQ.'l').OR.(CLINE(J:J).EQ.'L')) NUML=NUML+1
          GO TO 100
        END IF
60      CONTINUE
100     ASIZE=ASIZE+NBLC
        GO TO 50
180     CLOSE(UNIT=INST)
200     CONTINUE
        NBLBMX=NBLBMX+1
        DO 300 I=1,NOUFL
300     ASIZE=ASIZE+2+LEN_TRIM(OUTFLE(I))
        NINSTR=NINSTR+NOUFL
        IF(ICODE.EQ.1) RETURN

301     CONTINUE
        IPAR=1
        DO 400 I=1,NPAR
400     NW(I)=1000
        DO 500 I=1,NINFLE
        CALL FFOPEN(JFAIL,INST,'r',TMPFLE(I),37,CLINE)
        IF(JFAIL.NE.0) GO TO 9891
        READ(INST,'(A)',END=9200) CLINE
        CALL LOWCAS(CLINE(1:3))
        IF((CLINE(1:3).NE.'ptf').AND.
     +     (CLINE(1:3).NE.'jtf')) GO TO 9200
        PARDEL(I)=CLINE(5:5)
        IF(PARDEL(I).EQ.' ') GO TO 9200
        ILINE=1
520     ILINE=ILINE+1
        READ(INST,'(A)',END=680) CLINE
        NBLC=LEN_TRIM(CLINE)
        J2=0
550     IF(J2.GE.NBLC) GO TO 520
        J1=INDEX(CLINE(J2+1:NBLC),PARDEL(I))
        IF(J1.EQ.0) GO TO 520
        J1=J1+J2
        J2=INDEX(CLINE(J1+1:NBLC),PARDEL(I))
        IF(J2.EQ.0) THEN
          CALL STPERR(33,2,' ',ILINE,TMPFLE(I),CLINE)
          GO TO 9891
        END IF
        J2=J2+J1
        CALL PARNAM(IFAIL,J1,J2,TPAR,CLINE)
        IF(IFAIL.EQ.1) THEN
          CALL STPERR(39,2,' ',ILINE,TMPFLE(I),CLINE)
          GO TO 9891
        END IF
        IF(IFAIL.EQ.2) THEN
          CALL STPERR(47,2,' ',ILINE,TMPFLE(I),CLINE)
          GO TO 9891
        END IF
        CALL WHICH1(IFAIL,NPAR,IPAR,APAR,TPAR)
        IF(IFAIL.NE.0) THEN
          CALL STPERR(50,3,TPAR,ILINE,TMPFLE(I),CLINE)
          GO TO 9891
        END IF
        NNW=J2-J1+1
        IF(NNW.LT.NW(IPAR)) NW(IPAR)=NNW
        GO TO 550
680     CLOSE(UNIT=INST)
500     CONTINUE
        DO 800 I=1,NPAR
        IF(NW(I).EQ.1000) THEN
          IF(APAR(I)(1:3).NE.'iw_')THEN
            CALL STPERR(90,1,APAR(I),0,' ',CLINE)
            GO TO 9891
          END IF
        END IF
800     CONTINUE

        RETURN

9200    CALL STPERR(92,1,TMPFLE(I),0,' ',CLINE)
        GO TO 9891
9300    CALL STPERR(93,1,INSFLE(I),0,' ',CLINE)
        GO TO 9891

9891    JFAIL=1
        RETURN
#ifdef PESTMOD
        END SUBROUTINE IOCTL
#else
        END
#endif


C#ifndef PARALLEL

        SUBROUTINE MODEL(JFAIL,NPAR,NOBS,ASIZE,NINSTR,NINFLE,NOUFL,
     +  NUML,NBLBMX,SCALE,OFFSET,PVAL,NW,PWORD,APAR,MOBS,AOBS,IPP,
     +  TPVAL,NOCHNG,PRECIS,NOPNT,LCINS,LL,OBSN1,OBSN2,IIOBS,INSFLE,
     +  TMPFLE,INFLE,OUTFLE,PARDEL,MRKDEL,A,CLINE,BUF,MODFLE,IGO,X1,
     +  IMSFLE,INCPAR,ITRANS,REPEATFLAG,FLAG5,DF_FLAG)

C -- SUBROUTINE MODEL SUPERVISES THE WRITING OF MODEL INPUT FILES, THE
C -- RUNNING OF THE MODEL, AND THE READING OF MODEL OUTPUT FILES

#ifdef INTEL
        USE COMMON_MOD, ONLY: NCALL,MCALL
#endif
        USE PESTDATA, ONLY : INST,IREC,RUNREPEAT

        LOGICAL LEXIS
#ifdef INTEL
        INTEGER ASIZE,NINSTR,JFAIL
#else
        INTEGER NCALL,ASIZE,NINSTR,MCALL,JFAIL
#endif
        INTEGER NPAR,NOBS,IPP,NOCHNG,PRECIS,NOUFL,NINFLE,
     +  INS,ISUM,I,J,NBLC,NUML,NBLBMX,NOPNT,IGO,IMSFLE,INCPAR,
     +  IRPT
        INTEGER REPEATFLAG,FLAG5
        INTEGER DF_FLAG
        INTEGER LCINS(NINSTR),NW(NPAR),LL(NUML),OBSN1(NOBS),
     +  OBSN2(NOBS),IIOBS(NOBS),ITRANS(NPAR)
        DOUBLE PRECISION X1
        DOUBLE PRECISION PVAL(NPAR),MOBS(NOBS),TPVAL(NPAR),SCALE(NPAR),
     +  OFFSET(NPAR)
        CHARACTER*10 ATEMP10
        CHARACTER*(*) INSFLE(NOUFL),PWORD(NPAR),INFLE(NINFLE),
     +  OUTFLE(NOUFL),TMPFLE(NINFLE),APAR(NPAR),AOBS(NOBS)
        CHARACTER*(*) CLINE,BUF,MODFLE
        CHARACTER A(ASIZE),PARDEL(NINFLE),MRKDEL(NOUFL)
#ifdef UNIX
        CHARACTER*120 CMDLIN
#endif
#ifdef SYS_FUNCTION
        INTEGER NNN
        INTEGER SYSTEM
#endif
#ifdef INTEL
        INTEGER NNN
        INTEGER SYSTEM
#endif

#ifndef INTEL
        COMMON /MODCAL/NCALL,MCALL
#endif

        JFAIL=0
        IRPT=0
        NOCHNG=0
        IF(MCALL.GT.0) GO TO 500
        INS=0
        ISUM=0
        DO 200 I=1,NOUFL
        CALL FFOPEN(JFAIL,INST,'r',INSFLE(I),78,CLINE)
        IF(JFAIL.NE.0) GO TO 9891
        READ(INST,'(A)') ATEMP10
        INS=INS+1
        CLINE(1:1)=CHAR(2)
        CLINE(2:2)=' '
        CLINE(3:LEN(CLINE))=OUTFLE(I)
        LCINS(INS)=ISUM+1
        NBLC=LEN_TRIM(CLINE)
        DO 20 J=1,NBLC
20      A(J+ISUM)=CLINE(J:J)
        ISUM=ISUM+NBLC
50      READ(INST,22,END=180) CLINE
22      FORMAT(A)
        CALL TABREM(CLINE)
        IF(INDEX(CLINE,MRKDEL(I)).EQ.0) CALL CMPRSS(CLINE)
        NBLC=LEN_TRIM(CLINE)
        IF(NBLC.EQ.0) GO TO 50
        INS=INS+1
        LCINS(INS)=ISUM+1
        DO 70 J=1,NBLC
70      A(J+ISUM)=CLINE(J:J)
        ISUM=ISUM+NBLC
        GO TO 50
180     CLOSE(UNIT=INST)
200     CONTINUE

500     CALL INWRIT(JFAIL,NPAR,PRECIS,NOPNT,NINFLE,NW,PVAL,SCALE,
     +  OFFSET,PARDEL,PWORD,INFLE,TMPFLE,APAR,CLINE,IMSFLE,INCPAR,
     +  ITRANS,NOBS)
        IF(JFAIL.NE.0) GO TO 9891

        IF(FLAG5.EQ.0)THEN
          IF(IPP.NE.-1000) THEN
            IF(IPP.GT.0) THEN
              IF((TPVAL(IPP).EQ.PVAL(IPP)).OR.
     +          ((IGO.EQ.2).AND.(PVAL(IPP).EQ.X1)))THEN
                NOCHNG=1
                RETURN
              END IF
            ELSE
              DO 510 I=1,NPAR
              IF(TPVAL(I).NE.PVAL(I)) GO TO 520
510           CONTINUE
              NOCHNG=1
              RETURN
            END IF
          END IF
        END IF

520     NCALL=NCALL+1
        MCALL=MCALL+1
599     CONTINUE
        DO 600 I=1,NOUFL
          CALL DELFILE1(JFAIL,OUTFLE(I),CLINE)
          IF(JFAIL.NE.0) GO TO 9891
600     CONTINUE
601     CONTINUE

#ifdef LAHEY
#ifdef AT
        CALL SYSTEM(MODFLE(:LEN_TRIM(MODFLE)),.false.)
#else
        CALL SYSTEM(MODFLE(:LEN_TRIM(MODFLE)))
#endif
        CALL STOPRESS(0)
#endif

#ifdef UNIX
#ifdef SYS_FUNCTION
        NNN=SYSTEM(MODFLE(:LEN_TRIM(MODFLE)))
#else
        CALL SYSTEM(MODFLE(:LEN_TRIM(MODFLE)))
#endif
        CALL STOPRESS(0)
#endif

#ifdef INTEL
        NNN=SYSTEM(MODFLE(:LEN_TRIM(MODFLE)))
        CALL STOPRESS(0)
#endif

        CALL OUTRD(JFAIL,NINSTR,NOUFL,ASIZE,NUML,NOBS,NBLBMX,LCINS,
     +  LL,OBSN1,OBSN2,IIOBS,MOBS,AOBS,A,MRKDEL,CLINE,BUF,DF_FLAG)
        IF(JFAIL.NE.0) THEN
          IF(RUNREPEAT.EQ.'runrepeat')THEN
            IF(REPEATFLAG.NE.0)THEN
              IF(IRPT.EQ.0)THEN
                IRPT=1
                WRITE(IREC,610)
                WRITE(6,*)
                WRITE(6,610)
610             FORMAT(T5,'Run failure - PEST is repeating model run.')
                WRITE(6,*)
                GO TO 601
              END IF
            END IF
          END IF
          GO TO 9891
        END IF

        RETURN

9891    JFAIL=1
        RETURN

#ifdef PESTMOD
        END SUBROUTINE MODEL
#else
        END
#endif

C#endif

C#ifndef PARALLEL

        SUBROUTINE INWRIT(JFAIL,NPAR,PRECIS,NOPNT,NINFL,NW,PVAL,SCALE,
     +  OFFSET,MARK,PWORD,INFILE,INTEMP,APAR,CLINE,IMSFLE,INCPAR,
     +  ITRANS,NOBS)

C -- SUBROUTINE INWRIT WRITES MODEL INPUT FILES PRIOR TO RUNNING THE MODEL

        USE PESTDATA, ONLY : INST,IMIF,IMMF

        INTEGER NPAR,PRECIS,NINFL,IFILE,J2,LC,J1,IPAR,ISC,NOPNT,
     +  J,ITYPE,NOBS,IERR,JFAIL
        INTEGER NSP,IFAILCOUNT
        INTEGER NW(NPAR)
        INTEGER IFAIL,IPP,ILINE,IMSFLE,INCPAR
        INTEGER ITRANS(NPAR)
        DOUBLE PRECISION TVAL
        DOUBLE PRECISION PVAL(NPAR),SCALE(NPAR),OFFSET(NPAR)
        CHARACTER*12 TPAR
        CHARACTER*(*) CLINE
        CHARACTER*(*) PWORD(NPAR),APAR(NPAR)
        CHARACTER*(*) INFILE(NINFL),INTEMP(NINFL)
        CHARACTER MARK(NINFL)

C -- FIRST A MESSAGE FILE IS WRITTEN (IF APPROPRIATE)

        JFAIL=0
        IFAILCOUNT=0
        IF(IMSFLE.EQ.1)THEN
#ifdef UNIX
          OPEN(UNIT=IMMF,FILE='pest.mmf',ACTION='WRITE',IOSTAT=IERR)
#else
#ifdef LF90
          OPEN(UNIT=IMMF,FILE='pest.mmf',ACTION='write,denynone',
     +    IOSTAT=IERR)
#else
#ifdef INTEL
          OPEN(UNIT=IMMF,FILE='pest.mmf',SHARED,IOSTAT=IERR)
#else
          OPEN(UNIT=IMMF,FILE='pest.mmf',IOSTAT=IERR)
#endif
#endif
#endif
          IF(IERR.NE.0)THEN
            WRITE(*,10)
10          FORMAT(/,' Cannot open message file pest.mmf for output.',/)
            STOP
          END IF
          IF(INCPAR.EQ.-10000)THEN
            ITYPE=2
          ELSE IF(INCPAR.EQ.0)THEN
            ITYPE=1
          ELSE IF(ABS(INCPAR).LT.10000)THEN
            ITYPE=3
          ELSE
            IF(INCPAR.GT.10000) THEN
              INCPAR=INCPAR-10000
            ELSE IF(INCPAR.LT.-10000)THEN
              INCPAR=INCPAR+10000
            END IF
            ITYPE=-3
          END IF
          CALL PESTMESS(JFAIL,IMMF,ITYPE,NPAR,NOBS,INCPAR,ITRANS,APAR,
     +    PVAL,CLINE,'pest.mmf')
          IF(JFAIL.NE.0) GO TO 9891
          CLOSE(UNIT=IMMF)
        END IF

C -- NEXT EACH OF THE PARAMETER WORDS IS FILLED

        IPAR=1
        DO 100 IPP=1,NPAR
        IF(SCALE(IPP).LT.-1.0D35)CYCLE
        ISC=0
        IF((SCALE(IPP).NE.1.0D0).OR.(OFFSET(IPP).NE.0.0D0))THEN
          PVAL(IPP)=PVAL(IPP)*SCALE(IPP)+OFFSET(IPP)
          ISC=1
        END IF
        CALL WRTSIG(IFAIL,PVAL(IPP),PWORD(IPP),NW(IPP),PRECIS,TVAL,
     +  NOPNT)
        IF(IFAIL.NE.0) THEN
          CALL STPERR(33+IFAIL,1,APAR(IPP),0,' ',CLINE)
          GO TO 9891
        END IF
        IF(ISC.EQ.1) THEN
          PVAL(IPP)=(TVAL-OFFSET(IPP))/SCALE(IPP)
        ELSE
          PVAL(IPP)=TVAL
        END IF
100     CONTINUE

C -- NEXT THE SUBSTITUTIONS IN THE TEMPLATE FILES ARE MADE

        NSP=0
        DO IPP=1,NPAR
          IF((ITRANS(IPP).GE.0).OR.
     +       (ITRANS(IPP).LT.-1000001))NSP=NSP+1
        END DO
        DO 500 IFILE=1,NINFL
        IF(INTEMP(IFILE).EQ.'par###.tpl')THEN
          CALL FFOPEN(JFAIL,IMIF,'w',INFILE(IFILE),38,CLINE)
          IF(JFAIL.NE.0) GO TO 9891
          WRITE(IMIF,1030,ERR=320) NSP,1,2
1030      FORMAT(3I10)
          DO IPP=1,NPAR
            IF((ITRANS(IPP).GE.0).OR.
     +         (ITRANS(IPP).LT.-1000001))THEN
              WRITE(IMIF,1050,ERR=320) PVAL(IPP)
1050          FORMAT(1X,1PG14.7)
            END IF
          END DO
          WRITE(IMIF,1060,ERR=320)
1060      FORMAT('* row names')
          DO IPP=1,NPAR
            IF((ITRANS(IPP).GE.0).OR.
     +         (ITRANS(IPP).LT.-1000001))THEN
              WRITE(IMIF,1070,ERR=320) TRIM(APAR(IPP))
1070          FORMAT(A)
            END IF
          END DO
          WRITE(IMIF,1090,ERR=320)
1090      FORMAT('* column names')
          WRITE(IMIF,1190,ERR=320)
1190      FORMAT('col1')
          CLOSE(UNIT=IMIF)
          GO TO 500
        END IF
        CALL FFOPEN(JFAIL,INST,'r',INTEMP(IFILE),37,CLINE)
        IF(JFAIL.NE.0) GO TO 9891
        IFAILCOUNT=0
1191    CONTINUE
        CALL FFOPEN(JFAIL,IMIF,'w',INFILE(IFILE),38,CLINE)
        IF(JFAIL.NE.0) THEN
          IF(IFAILCOUNT.EQ.3) GO TO 9891
          IFAILCOUNT=IFAILCOUNT+1
          CALL PESTWAIT(100)
          GO TO 1191
        END IF
        READ(INST,*)
        ILINE=1
120     ILINE=ILINE+1
        READ(INST,22,END=400) CLINE
22      FORMAT(A)
        LC=LEN_TRIM(CLINE)
        J2=0
150     IF(J2.GE.LC) GO TO 300
        J1=INDEX(CLINE(J2+1:LC),MARK(IFILE))
        IF(J1.EQ.0) GO TO 300
        J1=J1+J2
        J2=INDEX(CLINE(J1+1:LC),MARK(IFILE))
        J2=J2+J1
        CALL PARNAM(IFAIL,J1,J2,TPAR,CLINE)
        CALL WHICH1(IFAIL,NPAR,IPAR,APAR,TPAR)
        IF(SCALE(IPAR).LT.-1.0D35)THEN
          CALL STPERR(186,0,' ',0,' ',CLINE)
          GO TO 9891
        END IF
c       The following works when space bigger than pword(:LEN_TRIM(pword))
c       cline(j1:j2)=pword(ipar)(:LEN_TRIM(pword(ipar)))
        DO 160 J=J1,J2
160     CLINE(J:J)=' '
        J=LEN_TRIM(PWORD(IPAR))
        CLINE(J2-J+1:J2)=PWORD(IPAR)(1:J)
        GO TO 150

300     WRITE(IMIF,22,ERR=320) CLINE(:MAX(LEN_TRIM(CLINE),1))
        GO TO 120
320     CALL STPERR(52,1,INFILE(IFILE),0,' ',CLINE)
        GO TO 9891
400     CLOSE(UNIT=INST)
        CLOSE(UNIT=IMIF)
500     CONTINUE
        RETURN

9891    JFAIL=1
        RETURN
#ifdef PESTMOD
        END SUBROUTINE INWRIT
#else
        END
#endif

C#endif


        SUBROUTINE PARNAM(IFAIL,J1,J2,TPAR,CLINE)

C -- SUBROUTINE PARNAM EXTRACTS A PARAMETER NAME FROM A STRING

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
#ifdef PESTMOD
        END SUBROUTINE PARNAM
#else
        END
#endif

C#ifndef PARALLEL


        SUBROUTINE OUTRD(JFAIL,NINSTR,NOUFL,ASIZE,NUML,NOBS,NBLBMX,
     +  LCINS,LL,OBSN1,OBSN2,IIOBS,OBS,AOBS,A,MRKDEL,CLINE,BUF,DF_FLAG)

C -- SUBROUTINE OUTRD READS MODEL OUTPUT FILES USING INTRUCTIONS

#ifdef BEO
        USE BEOPEST, ONLY : MCALL_NOT_DF_FLAG
#endif
        USE PESTDATA, ONLY : INST
#ifdef INTEL
        USE COMMON_MOD, ONLY:FLENME,NCALL,MCALL
#endif
        LOGICAL LOPENED
#ifdef INTEL
        INTEGER CIL,IFAIL,ASIZE,NINSTR,JFAIL
#else
        INTEGER MCALL,CIL,IFAIL,ASIZE,NINSTR,NCALL,JFAIL
#endif
        INTEGER INS,NBLB,NBLC,I,J,N1,N2,NOL,MRKTYP,J1,J2,
     +  NUML,JOBS,IOBS,NOBS,N3,NUM1,NUM2,IFILE,IL,NOUFL,NBLBMX,
     +  INSNUM,ALMARK,BEGINS,INSFLE,DUMFLG
        INTEGER DE_FLAG,DF_FLAG,IERR,NUMIERR
        INTEGER LCINS(NINSTR)
        INTEGER LL(NUML),OBSN1(NOBS),OBSN2(NOBS),IIOBS(NOBS)
        DOUBLE PRECISION OBS(NOBS),RTEMP
        CHARACTER*15 FMT,OBSNAM*20,MKRDEL*1,AA*1
        CHARACTER MRKDEL(NOUFL)
        CHARACTER A(ASIZE)
        CHARACTER*(*) AOBS(NOBS)
        CHARACTER*(*) CLINE,BUF
#ifndef INTEL
        CHARACTER*200 FLENME

        COMMON /FLENME/FLENME
        COMMON /MODCAL/NCALL,MCALL
#endif

        JFAIL=0
        IFILE=0
        IL=0
        JOBS=0
        MKRDEL=MRKDEL(1)
        CIL=0
        IOBS=1
        BEGINS=0
        BUF=' '
        DE_FLAG=0
        NUMIERR=0

        DO I=1,NOBS
          OBS(I)=-1.1D300
        END DO

        INS=1
10      IF(INS.LT.NINSTR)THEN
          NBLB=LCINS(INS+1)-LCINS(INS)
        ELSE
          NBLB=ASIZE-LCINS(INS)+1
        END IF
c        BUF(1:NBLBMX)=' '
        BUF(1:200)=' '
        DO 20 I=1,NBLB
20      BUF(I:I)=A(LCINS(INS)+I-1)
25      N2=0
        INSNUM=0

50      CALL GETINT(IFAIL,BUF,N1,N2,NBLB,MKRDEL)
        IF(IFAIL.NE.0) THEN
           CALL STPERR(68,5,BUF,0,' ',CLINE)
           GO TO 9892
        END IF
51      IF(N1.EQ.0) GO TO 1000
        INSNUM=INSNUM+1
        IF(INSNUM.EQ.1)THEN
          IF(BUF(N1:N1).NE.'&') THEN
            MRKTYP=0
            ALMARK=1
            BEGINS=0
          ELSE
            IF(INS.EQ.INSFLE+1) THEN
              CALL STPERR(73,5,BUF,0,' ',CLINE)
              GO TO 9892
            END IF
            IF(BEGINS.EQ.1)THEN
              INS=INS-1
              GO TO 10
            END IF
          END IF
        END IF
        IF((DE_FLAG.NE.0).AND.(ICHAR(BUF(N1:N1)).NE.2)) THEN
          IF((BUF(N1:N1).EQ.'[').OR.(BUF(N1:N1).EQ.'('))THEN
            JOBS=JOBS+1
          ELSE IF(BUF(N1:N1).EQ.'!') THEN
            CALL LOWCAS(BUF(N1+1:N2-1))
            IF((N2-N1.NE.4).OR.(BUF(N1+1:N2-1).NE.'dum'))THEN
              JOBS=JOBS+1
            END IF
          ELSE IF((BUF(N1:N1).EQ.'l').OR.(BUF(N1:N1).EQ.'L'))THEN
            IL=IL+1
          END IF
          GO TO 50
        END IF
        IF(ICHAR(BUF(N1:N1)).EQ.2)THEN
          IF(IFILE.NE.0) CLOSE(UNIT=INST)
          DE_FLAG=0
          DO 60 I=N1+1,NBLB
          IF(BUF(I:I).NE.' ') GO TO 70
60        CONTINUE
70        FLENME=BUF(I:NBLB)
          CALL FFOPEN(JFAIL,INST,'r',' ',65,CLINE)
          IF(JFAIL.NE.0)THEN
            IF(DF_FLAG.EQ.1)THEN
              JFAIL=0
              DE_FLAG=1
#ifdef LAHEY
              IF(NUMIERR.EQ.0)THEN
                WRITE(6,*)
              END IF
#endif
              NUMIERR=NUMIERR+1
              WRITE(6,71)TRIM(FLENME)
71            FORMAT(' *** File ',a,' not written by model ***')
              GO TO 50
            ELSE
              GO TO 9891
            END IF
          END IF
          IFILE=IFILE+1
          CIL=0
          MKRDEL=MRKDEL(IFILE)
          INSFLE=INS
          GO TO 1000
        ELSE IF((BUF(N1:N1).EQ.'l').OR.(BUF(N1:N1).EQ.'L'))THEN
          ALMARK=0
          IL=IL+1
          IF(MCALL.EQ.1)THEN
            WRITE(FMT,150) N2-N1
150         FORMAT('(I',I4,')')
            READ(BUF(N1+1:N2),FMT,ERR=9050) NOL
            LL(IL)=NOL
          ELSE
            NOL=LL(IL)
          END IF
          IF(NOL.GT.1) THEN
            DO 160 I=1,NOL-1
            READ(INST,*,END=9100)
160         CIL=CIL+1
          END IF
          READ(INST,22,END=9100) CLINE
22        FORMAT(A)
          IF(INDEX(CLINE,CHAR(9)).NE.0) CALL TABREP(CLINE)
          CIL=CIL+1
          NBLC=LEN_TRIM(CLINE)
          MRKTYP=1
          J1=0
        ELSE IF(BUF(N1:N1).EQ.MKRDEL)THEN
          IF(MRKTYP.EQ.0)THEN
200         READ(INST,22,END=9100) CLINE
            IF(INDEX(CLINE,CHAR(9)).NE.0) CALL TABREP(CLINE)
            CIL=CIL+1
            J1=INDEX(CLINE,BUF(N1+1:N2-1))
            IF(J1.EQ.0) GO TO 200
            NBLC=LEN_TRIM(CLINE)
            J1=J1+N2-N1-2
            MRKTYP=1
          ELSE
            IF(J1.GE.NBLC) THEN
              IF(ALMARK.EQ.1) THEN
                BEGINS=1
                GO TO 25
              END IF
              GO TO 9200
            END IF
            J2=INDEX(CLINE(J1+1:NBLC),BUF(N1+1:N2-1))
            IF(J2.EQ.0) THEN
              IF(ALMARK.EQ.1) THEN
                BEGINS=1
                GO TO 25
              END IF
              GO TO 9200
            END IF
            J1=J1+J2
            J1=J1+N2-N1-2
          END IF
        ELSE IF(BUF(N1:N1).EQ.'&')THEN
          IF(INSNUM.NE.1) THEN
            CALL STPERR(72,5,BUF,0,' ',CLINE)
            GO TO 9892
          END IF
        ELSE IF((BUF(N1:N1).EQ.'w').OR.(BUF(N1:N1).EQ.'W'))THEN
          ALMARK=0
          IF(J1.GE.NBLC) GO TO 9400
          J2=INDEX(CLINE(J1+1:NBLC),' ')
          IF(J2.EQ.0) GO TO 9400
          J1=J1+J2
          DO 210 I=J1,NBLC
          IF(CLINE(I:I).NE.' ') GO TO 220
210       CONTINUE
          I=NBLC+1
220       J1=I-1
        ELSE IF((BUF(N1:N1).EQ.'t').OR.(BUF(N1:N1).EQ.'T'))THEN
          ALMARK=0
          WRITE(FMT,150) N2-N1
          READ(BUF(N1+1:N2),FMT,ERR=9000) J2
          IF(J2.LT.J1) THEN
            CALL STPERR(81,4,BUF,CIL,' ',CLINE)
            GO TO 9892
          END IF
          J1=J2
          IF(J1.GT.NBLC) THEN
            CALL STPERR(70,4,BUF,CIL,' ',CLINE)
            GO TO 9891
          END IF
        ELSE IF((BUF(N1:N1).EQ.'[').OR.(BUF(N1:N1).EQ.'('))THEN
          ALMARK=0
          AA=BUF(N1:N1)
          JOBS=JOBS+1
          IF(MCALL.EQ.1)THEN
            IF(AA.EQ.'[')THEN
              N3=INDEX(BUF(N1:N2),']')
            ELSE
              N3=INDEX(BUF(N1:N2),')')
            END IF
            N3=N3+N1-1
            OBSNAM=BUF(N1+1:N3-1)
            CALL WHICH1(IFAIL,NOBS,IOBS,AOBS,OBSNAM)
            IF(IFAIL.NE.0) GO TO 9700
            CALL GETNUM(IFAIL,BUF,N3,N2,NUM1,NUM2,FMT)
            IF(IFAIL.NE.0) THEN
              CALL STPERR(64,5,BUF,0,' ',CLINE)
              GO TO 9892
            END IF
            IF(NUM1.LE.0)THEN
              CALL STPERR(64,5,BUF,0,' ',CLINE)
              GO TO 9892
            END IF
            OBSN1(JOBS)=NUM1
            OBSN2(JOBS)=NUM2
            IIOBS(JOBS)=IOBS
          ELSE
            NUM1=OBSN1(JOBS)
            NUM2=OBSN2(JOBS)
            IOBS=IIOBS(JOBS)
          END IF
          IF(AA.EQ.'(') THEN
            CALL GETTOT(IFAIL,CLINE,NUM1,NUM2,NBLC)
            IF(IFAIL.NE.0) THEN
              CALL STPERR(88,3,AOBS(IOBS)(:LEN_TRIM(AOBS(IOBS))),
     +        CIL,' ',CLINE)
              GO TO 9891
            END IF
          ELSE
           IF(NUM1.GT.NBLC) THEN
             CALL STPERR(88,3,
     +       AOBS(IOBS)(:LEN_TRIM(AOBS(IOBS))),CIL,' ',CLINE)
             GO TO 9891
           END IF
           IF(NUM2.GT.NBLC) NUM2=NBLC
           IF(CLINE(NUM1:NUM2).EQ.' ') THEN
             CALL STPERR(88,3,
     +       AOBS(IOBS)(:LEN_TRIM(AOBS(IOBS))),CIL,' ',CLINE)
             GO TO 9891
           END IF
          END IF
          WRITE(FMT,250) NUM2-NUM1+1
250       FORMAT('(F',I4,'.0)')
          READ(CLINE(NUM1:NUM2),FMT,ERR=260) OBS(IOBS)
          J1=NUM2
          GO TO 50
260       CALL STPERR(82,3,AOBS(IOBS)(:LEN_TRIM(AOBS(IOBS))),
     +      CIL,' ',CLINE)
            GO TO 9891
        ELSE IF(BUF(N1:N1).EQ.'!') THEN
          ALMARK=0
          CALL LOWCAS(BUF(N1+1:N2-1))
          IF((N2-N1.NE.4).OR.(BUF(N1+1:N2-1).NE.'dum'))THEN
            JOBS=JOBS+1
            IF(MCALL.EQ.1) THEN
              OBSNAM=BUF(N1+1:N2-1)
              CALL WHICH1(IFAIL,NOBS,IOBS,AOBS,OBSNAM)
              IF(IFAIL.NE.0) GO TO 9700
              IIOBS(JOBS)=IOBS
            ELSE
              IOBS=IIOBS(JOBS)
            END IF
            DUMFLG=0
          ELSE
            DUMFLG=1
          END IF
          CALL GETNXT(IFAIL,CLINE,J1,NUM1,NUM2,NBLC)
          IF(IFAIL.NE.0) THEN
            IF(DUMFLG.EQ.0) THEN
              CALL STPERR(88,3,AOBS(IOBS)(:LEN_TRIM(AOBS(IOBS))),
     +          CIL,' ',CLINE)
                GO TO 9891
            ELSE
              CALL STPERR(88,3,'dum',CIL,' ',CLINE)
              GO TO 9891
            END IF
          END IF
          WRITE(FMT,250) NUM2-NUM1+1
          READ(CLINE(NUM1:NUM2),FMT,ERR=270) RTEMP
          IF(DUMFLG.EQ.0) OBS(IOBS)=RTEMP
          J1=NUM2
          GO TO 50
270       CALL GETINT(IFAIL,BUF,N1,N2,NBLB,MKRDEL)
          IF(IFAIL.NE.0) THEN
            CALL STPERR(68,5,BUF,0,' ',CLINE)
            GO TO 9892
          END IF
          IF(N1.EQ.0)THEN
            IF(DUMFLG.EQ.1) GO TO 9900
            GO TO 9800
          END IF
          IF(BUF(N1:N1).NE.MKRDEL) THEN
            IF(DUMFLG.EQ.1) GO TO 9900
            GO TO 9800
          END IF
          J2=INDEX(CLINE(J1+1:NBLC),BUF(N1+1:N2-1))
          IF(J2.EQ.0) THEN
            IF(DUMFLG.EQ.1) GO TO 9900
            GO TO 9800
          END IF
          NUM2=J1+J2-1
          IF(NUM2.LT.NUM1)THEN
            IF(DUMFLG.EQ.1) GO TO 9900
            GO TO 9800
          END IF
          WRITE(FMT,250) NUM2-NUM1+1
          IF(DUMFLG.EQ.1)THEN
            READ(CLINE(NUM1:NUM2),FMT,ERR=9900) RTEMP
          ELSE
            READ(CLINE(NUM1:NUM2),FMT,ERR=9800) OBS(IOBS)
          END IF
          J1=NUM2
          GO TO 51
        ELSE
          CALL STPERR(64,5,BUF,0,' ',CLINE)
          GO TO 9892
        END IF
        GO TO 50
1000    INS=INS+1
        IF(INS.LE.NINSTR) GO TO 10

#ifdef BEO
        IF(MCALL_NOT_DF_FLAG.EQ.1)THEN
#else
        IF(MCALL.EQ.1)THEN
#endif
          DO I=1,NOBS
            IF(IIOBS(I).EQ.0) THEN
              CALL STPERR(45,1,AOBS(I)(:LEN_TRIM(AOBS(I))),0,' ',CLINE)
              GO TO 9892
            END IF
          END DO
        END IF

        CLOSE(UNIT=INST,IOSTAT=IERR)
        RETURN

9000    CALL STPERR(74,5,BUF,0,' ',CLINE)
        GO TO 9892
9050    CALL STPERR(75,5,BUF,0,' ',CLINE)
        GO TO 9892
9100    CALL STPERR(66,6,BUF,0,' ',CLINE)
        GO TO 9891
9200    CALL STPERR(67,4,BUF,CIL,' ',CLINE)
        GO TO 9891
9400    CALL STPERR(69,4,BUF,CIL,' ',CLINE)
        GO TO 9891
9700    CALL STPERR(84,6,BUF,0,OBSNAM(:LEN_TRIM(OBSNAM)),CLINE)
        GO TO 9892
9800    CALL STPERR(82,3,AOBS(IOBS)(:LEN_TRIM(AOBS(IOBS))),
     +    CIL,' ',CLINE)
        GO TO 9891
9900    CALL STPERR(82,3,'dum',CIL,' ',CLINE)
        GO TO 9891

9891    JFAIL=1
        INQUIRE(UNIT=INST,OPENED=LOPENED)
        IF(LOPENED)CLOSE(UNIT=INST)
        RETURN

9892    JFAIL=2
        INQUIRE(UNIT=INST,OPENED=LOPENED)
        IF(LOPENED)CLOSE(UNIT=INST)
        RETURN

#ifdef PESTMOD
        END SUBROUTINE OUTRD
#else
        END
#endif

C#endif


        SUBROUTINE GETNXT(IFAIL,CLINE,J1,NUM1,NUM2,NBLC)

C -- SUBROUTINE GETNXT GETS THE NEXT SPACE-DELIMITED WORD

        INTEGER IFAIL
        INTEGER J1,NUM1,NUM2,NBLC,I,II
        CHARACTER*(*) CLINE

        IFAIL=0
        DO 20 I=J1+1,NBLC
        IF(CLINE(I:I).NE.' ') GO TO 50
20      CONTINUE
        IFAIL=1
        RETURN
50      NUM1=I
        I=INDEX(CLINE(NUM1:NBLC),' ')
        II=INDEX(CLINE(NUM1:NBLC),',')
        IF(II.NE.0)THEN
          IF(I.EQ.0)THEN
            I=II
          ELSE
            I=MIN(I,II)
          END IF
        END IF
        IF(I.EQ.0) THEN
          NUM2=NBLC
        ELSE
          NUM2=NUM1+I-2
        END IF

        RETURN
#ifdef PESTMOD
        END SUBROUTINE GETNXT
#else
        END
#endif


        SUBROUTINE GETTOT(IFAIL,CLINE,J1,J2,NBLC)

C -- SUBROUTINE GETTOT DETERMINES THE EXACT POSITION OCCUPIED BY A NUMBER

        INTEGER IFAIL
        INTEGER J1,J2,NBLC,I
        CHARACTER*(*) CLINE

        IFAIL=0
        IF(J1.GT.NBLC)THEN
          IFAIL=1
          RETURN
        END IF
        IF(J2.GT.NBLC)J2=NBLC
        IF(CLINE(J2:J2).EQ.' ') THEN
          DO 10 I=J2,J1,-1
          IF(CLINE(I:I).NE.' ')THEN
            J2=I
            GO TO 100
          END IF
10        CONTINUE
          IFAIL=1
          RETURN
        ELSE
          IF(J2.EQ.NBLC) GO TO 100
          DO 20 I=J2,NBLC
          IF(CLINE(I:I).EQ.' ') THEN
            J2=I-1
            GO TO 100
          END IF
20        CONTINUE
          J2=NBLC
        END IF
100     IF(J1.EQ.1) GO TO 200
        DO 120 I=J1,1,-1
        IF(CLINE(I:I).EQ.' ') THEN
          J1=I+1
          GO TO 200
        END IF
120     CONTINUE
        J1=1
200     RETURN

#ifdef PESTMOD
        END SUBROUTINE GETTOT
#else
        END
#endif



        SUBROUTINE STOPRESS(IUNIT)

#ifdef INTEL
        USE COMMON_MOD, ONLY: ISTOP,REASON
#endif
        USE PESTDATA, ONLY : ISTP
#ifdef BEO
        use beopest, only : beoslave
#endif

#ifdef INTEL
        INTEGER IPAUSE,IUNIT,IERR
#else
        INTEGER ISTOP,IPAUSE,IUNIT,IERR
        INTEGER*4 REASON

        COMMON /WSTOP/ REASON
        COMMON /SSTOP/ISTOP
#endif

        IPAUSE=0
10      CONTINUE

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        reason=0
        open(unit=istp,file='pest.stp',status='old',err=5)
        read(istp,*,err=6,end=6) reason
6       close(unit=istp,err=5)
5       continue
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

#ifdef BEO
        if(reason.eq.10)then
          if(beoslave)then
            reason=1
          end if
        end if
#endif

        IF(REASON.EQ.3)THEN
          IF(IPAUSE.EQ.0)THEN
            WRITE(6,20)
20          FORMAT(' PEST execution has been PAUSED...')
            IPAUSE=1
          END IF
          CALL PESTWAIT(100)
          GO TO 10
        ELSE IF((REASON.EQ.1).OR.(REASON.EQ.2))THEN
          ISTOP=REASON
#ifdef BEO
          IF((ISTOP.EQ.1).OR.(ISTOP.EQ.2))THEN
            IF(.NOT.BEOSLAVE)THEN
              WRITE(6,21)
21            FORMAT(/,' PEST execution has been STOPPED: ',
     +        'tidying up.....')
            ELSE
              WRITE(6,211)
211           FORMAT(/,' Agent execution has been terminated ',
     +        'by user.')
            END IF
#else
          IF(ISTOP.EQ.1) THEN
            WRITE(6,21)
21          FORMAT(/,' PEST execution has been STOPPED: ',
     +      'tidying up.....')
#endif
            IF(IUNIT.NE.0) CLOSE(UNIT=IUNIT,IOSTAT=IERR)
            RETURN
          END IF
        ELSE IF(REASON.LT.0)THEN
          CONTINUE
#ifdef BEO
          ISTOP=REASON
#endif
        ELSE
          IF(IPAUSE.EQ.1)THEN
            WRITE(6,30)
30          FORMAT(' PEST execution has been UNPAUSED.',/)
          END IF
        END IF
        RETURN
#ifdef PESTMOD
        END SUBROUTINE STOPRESS
#else
        END
#endif


        SUBROUTINE TABREM(CLINE)

C -- SUBROUTINE TABREM REMOVES TABS FROM A STRING

        INTEGER I
        CHARACTER*(*) CLINE

        DO 10 I=1,LEN(CLINE)
10      IF(ICHAR(CLINE(I:I)).EQ.9) CLINE(I:I)=' '

        RETURN
#ifdef PESTMOD
        END SUBROUTINE TABREM
#else
        END
#endif

        SUBROUTINE TABREP(CLINE)

C -- SUBROUTINE TABREP REPLACES A TAB BY BLANK SPACE(S)

        INTEGER LLEN,I,J,K,NBLC
        CHARACTER*(*) CLINE

        LLEN=LEN(CLINE)
        DO 10 I=LLEN,1,-1
        IF(CLINE(I:I).NE.' ') GO TO 20
10      CONTINUE
        RETURN
20      NBLC=I

        I=0
30      I=I+1
        IF(I.GT.NBLC)RETURN
        IF(ICHAR(CLINE(I:I)).NE.9) GO TO 30
        J=((I-1)/8+1)*8-I
        IF(J.EQ.0) THEN
          CLINE(I:I)=' '
        ELSE
          CLINE(I:I)=' '
          NBLC=NBLC+J
          IF(NBLC.GT.LLEN) NBLC=LLEN
          DO 50 K=NBLC,((I-1)/8+1)*8,-1
          CLINE(K:K)=CLINE(K-J:K-J)
50        CONTINUE
          DO 60 K=I+1,MIN(NBLC,I+J)
          CLINE(K:K)=' '
60        CONTINUE
          I=I+J
        END IF
        GO TO 30

#ifdef PESTMOD
        END SUBROUTINE TABREP
#else
        END
#endif


        SUBROUTINE CMPRSS(CLINE)


C -- SUBROUTINE CMPRSS COMPRESSES AN INSTRUCTION LINE BY REMOVING EXCESS
C -- BLANK CHARACTERS

        INTEGER NBLC,J
        CHARACTER*(*) CLINE

        IF(CLINE.EQ.' ') RETURN
10      NBLC=LEN_TRIM(CLINE)
        J=INDEX(CLINE(1:NBLC),'  ')
        IF(J.NE.0) THEN
          CALL SHIFTL(CLINE(J+1:NBLC))
          GO TO 10
        END IF
        RETURN

#ifdef PESTMOD
        END SUBROUTINE CMPRSS
#else
        END
#endif


#ifdef UNICOS
C -- Function system to call UNICOS ishell function. LJB 27/7/95
      INTEGER FUNCTION SYSTEM(STRING)
      INTEGER ISHELL
      CHARACTER*(*) STRING

      SYSTEM = ISHELL(STRING)

      RETURN
#ifdef PESTMOD
      END FUNCTION SYSTEM
#else
      END
#endif
#endif
