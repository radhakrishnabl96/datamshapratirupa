        PROGRAM WTFACTOR

C -- PROGRAM WTFACTOR MULTIPLIES WEIGHTS FOR OBSERVATIONS AND PRIOR INFORMATION
C -- EQUATIONS BELONGING TO A GIVEN OBSERVATION GROUP BY A USER-SUPPLIED
C -- FACTOR.

        IMPLICIT NONE

        LOGICAL LEXIST,SKIPLINE
        INTEGER, PARAMETER :: MAXIFCOUNT=100
        INTEGER IFAIL,IERR,ILINE,IOK,IOBG,IFLAG,I,J,K,N,IEND
        INTEGER IFCOUNT
        INTEGER LW(10),RW(10)
        DOUBLE PRECISION FACTOR,WEIGHT
        CHARACTER*200 PESTFILE1,PESTFILE2
        CHARACTER*5 ALINE
        CHARACTER*12 AVERSION
        CHARACTER*30 AFACTOR,ATEMP,ATEMP1,ATEMP2,AOBSGP
        CHARACTER*600 CLINE,DLINE,ELINE,FLINE
        CHARACTER*300 FFLINE(MAXIFCOUNT)
#ifdef SYS_FUNCTION
        INTEGER NNN
        INTEGER SYSTEM
#endif

        INCLUDE 'version.inc'
        WRITE(6,10) TRIM(AVERSION)
10      FORMAT(' WTFACTOR Version ',A,'. ',
     +  'Watermark Numerical Computing.')
        WRITE(6,*)
#ifdef NO_CMDLINE
        WRITE(*,*)
1       WRITE(*,'(A)') ' ENTER COMMAND LINE ARGUMENTS > '
        READ(*,'(A)',ERR=1) CLINE
#else
        CALL PGETCL (CLINE)
#endif

        CALL SPACESUB(CLINE)
        CALL LINSPL(IFAIL,5,LW,RW,CLINE)
        IF(IFAIL.EQ.0) GO TO 9000
        CALL LINSPL(IFAIL,4,LW,RW,CLINE)
        IF(IFAIL.NE.0) GO TO 9000
        PESTFILE1=CLINE(LW(1):RW(1))
        PESTFILE2=CLINE(LW(4):RW(4))
        AOBSGP=CLINE(LW(2):RW(2))
        AFACTOR=CLINE(LW(3):RW(3))
        CALL REMCHAR(PESTFILE1,CHAR(211))
        CALL REMCHAR(PESTFILE2,CHAR(211))
        CALL REMCHAR(AOBSGP,CHAR(211))
        CALL REMCHAR(AFACTOR,CHAR(211))
        CALL LOWCAS(AOBSGP)
        CALL DREALRD(IFAIL,AFACTOR,FACTOR)
        IF(IFAIL.NE.0) THEN
          WRITE(6,35)
35        FORMAT(/,' Cannot read weight factor from command line.')
          GO TO 9900
        END IF
#ifdef CAPFILE
        CALL UPCAS(PESTFILE1)
        CALL UPCAS(PESTFILE2)
#endif
        IF(PESTFILE1.EQ.PESTFILE2)THEN
          WRITE(6,40)
40        FORMAT(/,' Input and output PEST control files cannot have ',
     +    'same name.')
          GO TO 9900
        END IF

        OPEN(UNIT=11,FILE=PESTFILE1,STATUS='OLD',IOSTAT=IERR)
        IF(IERR.NE.0) THEN
          WRITE(6,60) TRIM(PESTFILE1)
60        FORMAT(/,' Cannot open PEST control file ',A,'.')
          GO TO 9900
        END IF
        OPEN(UNIT=20,FILE=PESTFILE2,IOSTAT=IERR)
        IF(IERR.NE.0)THEN
          WRITE(6,61) TRIM(PESTFILE2)
61        FORMAT(/,' Cannot open file ',A,' for output.')
          GO TO 9900
        END IF
        IF(LEN_TRIM(AOBSGP).GT.12)THEN
          WRITE(6,65) TRIM(AOBSGP)
65        FORMAT(/,' Observation group name "',A,
     +    '" greater than 12 characters in length.')
          GO TO 9900
        END IF
        DO 66 I=1,LEN_TRIM(AOBSGP)
          IF(AOBSGP(I:I).EQ.' ')THEN
            WRITE(6,67)
67          FORMAT(/,' Observation group name cannot contain a blank ',
     +      'character.')
            GO TO 9900
          END IF
66      CONTINUE

C -- SOME VARIABLES ARE INITIALISED

        IFLAG=0
        IOK=0
        IEND=0
        IOBG=0
        ILINE=1

C -- THE FIRST PEST CONTROL FILE IS READ AND THE SECOND IS WRITTEN
C    UNTIL THE "* OBSERVATION GROUPS" SECTION IS ENCOUNTERED.

        WRITE(6,450) TRIM(PESTFILE1),TRIM(PESTFILE2)
450     FORMAT(' Reading file ',A,' and writing file ',A,' ....')

        READ(11,'(A)',END=9200,ERR=9400) CLINE
        CALL LOWCAS(CLINE)
        CLINE=ADJUSTL(CLINE)
        IF(CLINE(1:4).NE.'pcf ')THEN
          WRITE(6,460) TRIM(PESTFILE1)
460       FORMAT(/,' First line of file ',A,' should be "pcf".')
          GO TO 9900
        END IF
        WRITE(20,'(A)',ERR=9300) TRIM(CLINE)
470     CONTINUE
          ILINE=ILINE+1
          READ(11,'(A)',ERR=9400,END=9200) CLINE
          CLINE=ADJUSTL(CLINE)
          IF(CLINE(1:1).EQ.'*') THEN
            DLINE=CLINE
            CALL LOWCAS(DLINE)
            IF(INDEX(DLINE,'observation').NE.0) THEN
              IF(INDEX(DLINE,'group').NE.0)THEN
                IOBG=1
              END IF
            END IF
          END IF
          WRITE(20,'(A)',ERR=9300) TRIM(CLINE)
          IF(IOBG.EQ.1) GO TO 300
        GO TO 470

C -- THE "* OBSERVATION GROUPS" SECTION OF THE PEST CONTROL FILE IS READ,
C -- LOOKING FOR THE PERTINENT OBSERVATION GROUP NAME.

300     CONTINUE
          ILINE=ILINE+1
          READ(11,'(A)',ERR=9400,END=9500) CLINE
          CLINE=ADJUSTL(CLINE)
          IF(CLINE(1:1).EQ.'*')THEN
            DLINE=CLINE
            CALL LOWCAS(DLINE)
            IF(INDEX(DLINE,'observation').NE.0)THEN
              IF(INDEX(DLINE,'data').NE.0)THEN
                WRITE(20,'(A)',ERR=9300) TRIM(CLINE)
                GO TO 400
              END IF
            END IF
          END IF
          CALL LINSPL(IFAIL,1,LW,RW,CLINE)
          IF(IFAIL.NE.0) GO TO 9400
          ATEMP=CLINE(LW(1):RW(1))
          CALL LOWCAS(ATEMP)
          IF(ATEMP.EQ.AOBSGP) THEN
            IFLAG=1
            CALL LINSPL(IFAIL,2,LW,RW,CLINE)
            IF(IFAIL.EQ.0)THEN
              WRITE(6,310) TRIM(AOBSGP),TRIM(PESTFILE1)
310           FORMAT(/,' A covariance matrix has been ',
     +        'supplied for observation group "',A,
     +        '"',/,' in PEST control file ',A,'. WTFACTOR does not ',
     +        'permit this for ',/,' groups whose weights are to be ',
     +        'multiplied.')
              GO TO 9900
            END IF
          END IF
315       WRITE(20,'(A)',ERR=9300) TRIM(CLINE)
        GO TO 300

400     CONTINUE
        IF(IFLAG.EQ.0)THEN
          WRITE(6,410) TRIM(AOBSGP),TRIM(PESTFILE1)
410       FORMAT(/,' Group "',A,'" not cited in ',
     +    '"* observation groups" section of ',/,
     +    ' PEST control file ',A)
          GO TO 9900
        END IF

C -- THE "* OBSERVATION DATA" SECTION OF THE EXISTING PEST CONTROL FILE
C    IS READ AND THE WEIGHT FACTOR IS APPLIED.

500     CONTINUE
          ILINE=ILINE+1
          READ(11,'(A)',ERR=9400,END=9500) CLINE
          DLINE=CLINE
          IF(SKIPLINE(CLINE)) THEN
            WRITE(20,'(A)') TRIM(DLINE)
            GO TO 500
          END IF
          IF(CLINE(1:1).EQ.'*')THEN
            WRITE(20,'(A)',ERR=9300) TRIM(DLINE)
            GO TO 600
          END IF
          CALL LINSPL(IFAIL,4,LW,RW,DLINE)
          IF(IFAIL.NE.0)THEN
            CALL WRTINT(ALINE,ILINE)
            WRITE(6,530) TRIM(ALINE),TRIM(PESTFILE1)
530         FORMAT(/,' Insufficient entries on line ',A,
     +      ' of file ',A)
            GO TO 9900
          END IF
          CALL DREALRD(IFAIL,DLINE(LW(3):RW(3)),WEIGHT)
          IF(IFAIL.NE.0) GO TO 9650
          ATEMP=DLINE(LW(4):RW(4))
          ATEMP1=ATEMP
          CALL LOWCAS(ATEMP1)
          IF(LEN_TRIM(ATEMP1).GT.12) GO TO 9700
          IF(ATEMP1.EQ.AOBSGP)THEN
            WRITE(ATEMP2,'(1PG14.7)') WEIGHT*FACTOR
          ELSE
            ATEMP2=DLINE(LW(3):RW(3))
          END IF
          IF(DLINE(RW(4)+1:).EQ.' ')THEN
            WRITE(20,550,ERR=9300) DLINE(1:RW(2)),
     +      TRIM(ATEMP2),TRIM(ATEMP)
550         FORMAT(1X,A,3X,A15,1X,A12)
          ELSE
            WRITE(20,551,ERR=9300) DLINE(1:RW(2)),
     +      TRIM(ATEMP2),TRIM(ATEMP),TRIM(DLINE(RW(4)+1:))
551         FORMAT(1X,A,3X,A15,1X,A12,1X,A)
          END IF
        GO TO 500

C -- A SEARCH IS MADE FOR THE "* PRIOR INFORMATION" SECTION OF THE PEST
C    CONTROL FILE.

600     CONTINUE
          ILINE=ILINE+1
          READ(11,'(A)',ERR=9400,END=800) CLINE
          IF(CLINE(1:1).EQ.'*')THEN
            DLINE=CLINE
            CALL LOWCAS(DLINE)
            IF(INDEX(DLINE,'prior').NE.0)THEN
              WRITE(20,'(A)',ERR=9300) TRIM(CLINE)
              GO TO 700
            END IF
          END IF
          WRITE(20,'(A)',ERR=9300) TRIM(CLINE)
        GO TO 600

C -- THE "* PRIOR INFORMATION" SECTION IS NOW DEALT WITH

        IFCOUNT=0
700     CONTINUE
        ILINE=ILINE+1
        READ(11,'(A)',ERR=9400,END=800) CLINE
        ELINE=CLINE
        IF(SKIPLINE(CLINE))THEN
          WRITE(20,'(A)',ERR=9300) TRIM(ELINE)
          GO TO 700
        END IF
        IF(CLINE(1:1).EQ.'*') THEN
          WRITE(20,'(A)',ERR=9300) TRIM(ELINE)
          GO TO 810
        END IF
        ELINE=ADJUSTL(ELINE)
720     CONTINUE
          CALL WRTINT(ALINE,ILINE)
          ILINE=ILINE+1
          READ(11,'(A)',ERR=9400,END=750) DLINE
          FLINE=DLINE
          IF(SKIPLINE(DLINE))THEN
            IFCOUNT=IFCOUNT+1
            IF(IFCOUNT.LE.MAXIFCOUNT)THEN
              FFLINE(IFCOUNT)=FLINE
            END IF
            GO TO 720
          END IF
          FLINE=ADJUSTL(FLINE)
          IF(DLINE(1:1).EQ.'*') THEN
            IEND=2
            GO TO 766
          END IF
          GO TO 760
750       IEND=1
          GO TO 766
760       CONTINUE
          IF(DLINE(1:1).EQ.'&')THEN
            WRITE(20,'(A)',ERR=9300) TRIM(ELINE)
            IF(IFCOUNT.GT.0)THEN
              DO I=1,IFCOUNT
                WRITE(20,'(A)') TRIM(FFLINE(I))
              END DO
              IFCOUNT=0
            END IF
            CLINE=DLINE
            ELINE=FLINE
            GO TO 720
          END IF
766       CONTINUE
          N=LEN_TRIM(CLINE)
          DO 770 I=N,1,-1
            IF(CLINE(I:I).EQ.' ') GO TO 780
770       CONTINUE
          WRITE(6,774) TRIM(ALINE),TRIM(PESTFILE1)
774       FORMAT(/,' Error in prior information equation at line ',A,
     +    ' of file ',A)
          GO TO 9900
780       ATEMP=CLINE(I+1:N)
          CALL LOWCAS(ATEMP)
          IF(ATEMP.NE.AOBSGP)THEN
            WRITE(20,'(A)',ERR=9300) TRIM(ELINE)
            IF(IFCOUNT.GT.0)THEN
              DO I=1,IFCOUNT
                WRITE(20,'(A)') TRIM(FFLINE(I))
              END DO
              IFCOUNT=0
            END IF
            IF(IEND.EQ.1) THEN
              GO TO 800
            ELSE IF(IEND.EQ.2)THEN
              WRITE(20,'(A)',ERR=9300) TRIM(FLINE)
              GO TO 810
            END IF
            CLINE=DLINE
            ELINE=FLINE
            GO TO 720
          END IF
          DO 790 J=I,1,-1
            IF(CLINE(J:J).NE.' ') GO TO 805
790       CONTINUE
          WRITE(6,774) TRIM(ALINE),TRIM(PESTFILE1)
          GO TO 9900
805       CONTINUE
          IF((J.EQ.1).AND.(CLINE(1:1).EQ.'&'))THEN
            WRITE(6,775) TRIM(ALINE),TRIM(PESTFILE1)
775         FORMAT(/,' WTFACTOR cannot alter the weight assigned to ',
     +      'a prior information equation ',/,' if the observation ',
     +      'group name and the prior information weight are on ',/,
     +      ' different lines; see line ',A,' of file ',A)
            GO TO 9900
          END IF
          DO 815 K=J,1,-1
            IF(CLINE(K:K).EQ.' ') GO TO 820
815       CONTINUE
          WRITE(6,774) TRIM(ALINE),TRIM(PESTFILE1)
          GO TO 9900
820       K=K+1
          ATEMP=CLINE(K:J)
          CALL DREALRD(IFAIL,ATEMP,WEIGHT)
          IF(IFAIL.NE.0)GO TO 9651
          WRITE(ATEMP2,'(1PG14.7)') WEIGHT*FACTOR
          IF(ELINE(N+1:).EQ.' ')THEN
            WRITE(20,840) CLINE(1:K-1),TRIM(ATEMP2),
     +      CLINE(I:N)
840         FORMAT(A,2X,A,2X,A)
          ELSE
            WRITE(20,841) CLINE(1:K-1),TRIM(ATEMP2),
     +      CLINE(I:N),TRIM(ELINE(N+1:))
841         FORMAT(A,2X,A,2X,A,1X,A)
          END IF
          IF(IFCOUNT.GT.0)THEN
            DO I=1,IFCOUNT
              WRITE(20,'(A)') TRIM(FFLINE(I))
            END DO
            IFCOUNT=0
          END IF
850       IF(IEND.EQ.1)THEN
            GO TO 800
          ELSE IF(IEND.EQ.2)THEN
            WRITE(20,'(A)',ERR=9300) TRIM(FLINE)
            GO TO 810
          ELSE
            CLINE=DLINE
            ELINE=FLINE
            GO TO 720
          END IF
        CONTINUE

C -- THE REMAINDER OF THE PEST CONTROL FILE IS WRITTEN.

810     CONTINUE
        ILINE=ILINE+1
        READ(11,'(A)',ERR=9400,END=800) CLINE
        WRITE(20,'(A)',ERR=9300) TRIM(CLINE)
        GO TO 810

800     CONTINUE

900        WRITE(6,920) TRIM(PESTFILE2)
920        FORMAT(/,' File ',A,' written ok.')
        IOK=1
        GO TO 9990

9000    WRITE(6,9010)
9010    FORMAT(' WTFACTOR is run using the command:',/)
        WRITE(6,9020)
9020    FORMAT('     wtfactor pestfile1 obsgroup factor pestfile2',/)
        WRITE(6,9030)
9030    FORMAT(' where',/)
        WRITE(6,9050)
9050    FORMAT('     pestfile1 is the name of the input PEST ',
     +  'control file,')
        WRITE(6,9051)
9051    FORMAT('     obsgroup  is the name of an observation group,')
        WRITE(6,9052)
9052    FORMAT('     factor    is the multiplier for weights of ',
     +  'this group, and')
        WRITE(6,9060)
9060    FORMAT('     pestfile2 is the name of the output PEST ',
     +  'control file.')
        GO TO 9999

9200    WRITE(6,9210) TRIM(PESTFILE1)
9210    FORMAT(/,' Unexpected end encountered to file ',A,
     +  ' while looking for ',/,' "* observation groups" section.')
        GO TO 9900
9300    WRITE(6,9310) TRIM(PESTFILE2)
9310    FORMAT(/,' Error encountered while writing to file ',A)
        GO TO 9900
9400    CALL WRTINT(ALINE,ILINE)
        WRITE(6,9410) TRIM(ALINE),TRIM(PESTFILE1)
9410    FORMAT(/,' Error in line ',A,' of PEST control file ',A)
        GO TO 9900
9500    WRITE(6,9510) TRIM(PESTFILE1)
9510    FORMAT(/,' Unexpected end encountered to file ',A,'.')
        GO TO 9900
9650    CALL WRTINT(ALINE,ILINE)
9651    WRITE(6,9660) TRIM(ALINE),TRIM(PESTFILE1)
9660    FORMAT(/,' Cannot read weight from line ',A,
     +  ' of file ',A)
        GO TO 9900
9700    CALL WRTINT(ALINE,ILINE)
        WRITE(6,9710) TRIM(ALINE),TRIM(PESTFILE1)
9710    FORMAT(/,' Observation group name greater than 12 characters ',
     +  'at line ',A,/,' of PEST control file ',A)
        go to 9900

9900    CONTINUE
9995    WRITE(6,9996)
9996    FORMAT(/,' Execution terminated.',/)
        GO TO 9990

9990    CONTINUE
        CLOSE(UNIT=11,IOSTAT=IERR)
        CLOSE(UNIT=20,IOSTAT=IERR)
        IF(IOK.EQ.0)THEN
          INQUIRE(FILE=PESTFILE2,EXIST=LEXIST)
#ifdef UNIX
#ifdef SYS_FUNCTION
          IF(LEXIST) NNN=SYSTEM('/bin/rm '//
     +    TRIM(PESTFILE2))
#else
          IF(LEXIST) CALL SYSTEM('/bin/rm '//
     +    TRIM(PESTFILE2))
#endif
#else
          IF(LEXIST) CALL SYSTEM('del "'//
     +    TRIM(PESTFILE2)//'"')
#endif
        END IF

9999    END


        SUBROUTINE LINSPL(IFAIL,NUM,LW,RW,CLINE)

C -- SUBROUTINE LINSPL SPLITS A LINE INTO WHITESPACE-SEPARATED SUBSTRINGS

        INTEGER IFAIL,NW,NBLC,J,I
        INTEGER NUM
        INTEGER LW(NUM),RW(NUM)
        CHARACTER*(*) CLINE

        IFAIL=0
        NW=0
        NBLC=LEN_TRIM(CLINE)
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

        INTEGER I,J
        CHARACTER*(*) ASTRNG

        DO 10 I=1,LEN_TRIM(ASTRNG)
        J=ICHAR(ASTRNG(I:I))
        IF((J.GE.65).AND.(J.LE.90)) ASTRNG(I:I)=CHAR(J+32)
10      CONTINUE
        RETURN
        END


        SUBROUTINE UPCAS(ASTRNG)

C -- SUBROUTINE UPCAS CONVERTS A STRING TO UPPER CASE

        INTEGER I,J
        CHARACTER*(*) ASTRNG

        DO 10 I=1,LEN_TRIM(ASTRNG)
        J=ICHAR(ASTRNG(I:I))
        IF((J.GE.97).AND.(J.LE.122)) ASTRNG(I:I)=CHAR(J-32)
10      CONTINUE
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
        ATEMP=ADJUSTL(ATEMP)
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
