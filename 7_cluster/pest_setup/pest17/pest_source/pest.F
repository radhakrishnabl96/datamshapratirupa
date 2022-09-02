      PROGRAM PEST

#ifdef BEO
      USE BEOPEST
      use svd_parm_data, only : svalue,bapar,bitrans,sval,blbound,
     +                   bubound,bscale,boffset
      USE BEORUN_DATA, only : ialloc_beorun_data
#endif

      USE PESTDATA, ONLY : NOPTMAX,IFINSTOPFLAG,HPSTART,PARSAVEOBS,
     +                     JACUPDATE,IBOUNDSCALE,JACUPDATE_RETAIN

      IMPLICIT NONE

      LOGICAL LEXIST
      INTEGER I,IFAIL,RESTART,P1START
      INTEGER IERR
      INTEGER PF_START,PF_END
      INTEGER PFILE,NB_RRF
      INTEGER PACKETSIZE
      INTEGER IPAROBS
      INTEGER NN
      CHARACTER*1   AA,AYN
      CHARACTER*4   AEXT
      CHARACTER*5   ATEMP
      CHARACTER*10  ANUM
      CHARACTER*12  AVERSION
      CHARACTER*256 PEST_COMMAND_LINE,CTLFILE,JCOFIRSTFILE
      CHARACTER*256 RRFFIRSTFILE
      CHARACTER*256 APVFILE,PF_BASE,RRFILE
      CHARACTER*256 ATEXT

C -- The version number is written to the screen.

        INCLUDE 'version.inc'
#ifdef PARALLEL
#ifdef BEO
      WRITE(6,'(/,3A)') ' BeoPEST Version ',TRIM(AVERSION),
     +  '. Watermark Numerical Computing & Principia Mathematica.'

#else
#ifdef MPEST
        WRITE(6,1) TRIM(AVERSION)
1       FORMAT(' MPEST Version ',A,'. ',
     +  'Watermark Numerical Computing.')
#else
#ifdef GENIE
        WRITE(6,1) TRIM(AVERSION)
1       FORMAT(' Parallel PEST Version ',A,'. ',
     +  'Watermark Numerical Computing.')
        WRITE(6,2) TRIM('1.0.0')
2       FORMAT(/,' Parallelization via Genie Version ',A,'.',
     +  /,3x,'S.S. Papadopulos & Assoc. Inc.,',
     +  /,3x,'Principia Mathematica, & ',
     +  /,3x,'Watermark Numerical Computing.')
#else
        WRITE(6,1) TRIM(AVERSION)
1       FORMAT(' Parallel PEST Version ',A,'. ',
     +  'Watermark Numerical Computing.')
#endif
#endif
#endif
#else
        WRITE(6,1) TRIM(AVERSION)
1       FORMAT(' PEST Version ',A,'. ',
     +  'Watermark Numerical Computing.')
#endif


        P1START=0
        PACKETSIZE=0

C -- The command line is obtained.

#ifdef NO_CMDLINE
        WRITE(*,*)
2       WRITE(*,'(A)') ' ENTER COMMAND LINE ARGUMENTS > '
        READ(*,'(A)',ERR=2) PEST_COMMAND_LINE
#else
      CALL GET_COMMAND_LINE(PEST_COMMAND_LINE)
#endif

C -- The command line is parsed.

      ATEXT=' '
      CALL PARSE_COMMAND_LINE(IFAIL,PEST_COMMAND_LINE,CTLFILE,RESTART,
     +                        P1START,PFILE,HPSTART,IPAROBS,ATEXT)
      IF(IFAIL.EQ.1) THEN
        WRITE(6,50)
#ifdef BEO
50      FORMAT(/,' BEOPEST is run using the command:- ',/,/,
     +  ' BEOPEST pestfile [/r] [/j] [/s] [/d] [/i] [/p1] [/t] [/f] ',
     +  '/h [manhost]:port',/,/,' where ',/,/,
     +  ' pestfile is the name of a PEST control file and ',
     +  '/r /j /s /d /i /p1 /t /f',/,
     +  ' are restart and control switches.')
        WRITE(6,51)
51      FORMAT(/,' Omit manhost to run as BEO-manager; ',
     +  'include to run as BEO-agent.')
        go to 9999
#else
#ifdef MPEST
50      FORMAT(/,' MPEST is run using the command:- ',/,/,
     +  '     MPEST pestfile [/r] [/j] [/i]',/,/,' where ',/,/,
     +  '     pestfile is the name of a PEST control file and ',
     +  '/r /j /i',/,
     +  '     are restart switches.')
        GO TO  9999
#else
#ifdef GENIE
50      FORMAT(/,' GPEST[32/64] is run using the command:- ',/,/,
     +  '     GPEST[32/64] pestfile [/r] [/j] [/s] [/d] [/f]',
     +        '[/i] [/p1] [/t] ',
     +        '/h [masterhost]:port',/,/,' where ',/,/,
     +  '     pestfile is the name of a PEST control file and ',
     +  '/r /j /s /d /i /p1 /t /f',/,
     +  '     are restart and control switches.')
        go to 9999
#else
50      FORMAT(/,' PEST is run using the command: ',/,/,
     +  '     PEST pestfile [/r] [/j] [/s] [/d] [/i] [/p1] [/t] ',
     +  '[/f] [/hpstart]'
     +  ,/,/,' where ',/,/,
     +  '     pestfile is the name of a PEST control file,',/,
     +  '     /r /j /s /d /i /p1 /t /f are restart and control ',
     +  'switches, and',/
     +  '     /hpstart instructs PEST to do a starter run for ',
     +  'PEST_HP.')
        GO TO  9999
#endif
#endif
#endif
      END IF
      IF(IFAIL.EQ.2) GO TO 9999
      PARSAVEOBS=0
      IF(IPAROBS.EQ.1)PARSAVEOBS=1

#ifdef BEO
      IF(HPSTART.NE.0)THEN
        WRITE(6,503)
503     FORMAT(/,' BEOPEST cannot be run using the "/hpstart" ',
     +  'option. Use serial PEST',/,' (or I64PEST) instead.')
        GO TO 9999
      END IF
#else
      IF(HPSTART.NE.0)THEN
        IF((RESTART.NE.0).OR.(P1START.NE.0).OR.(PFILE.NE.0))THEN
          WRITE(6,504)
504       FORMAT(/,' The "/hpstart" command-line option cannot ',
     +    'be used with any other ',/,' command-line options.')
          GO TO 9999
        END IF
      END IF
#endif

#ifdef MPEST
        IF((RESTART.EQ.3).OR.(RESTART.EQ.4))THEN
          WRITE(6,50)
          GO TO 9999
        END IF
#endif

C -- An extension of ".pst" is added to the PEST control file if necessary.

      I=LEN_TRIM(CTLFILE)
      IF(I.GE.5) THEN
        ATEMP=CTLFILE(I-3:I)
        CALL LOWCAS(ATEMP)
        IF(ATEMP.EQ.'.pst') GO TO 110
      END IF
      CTLFILE(I+1:)='.pst'
110   CONTINUE

C -- The PEST input dataset is read.

      CALL READ_PEST_DATA(IFAIL,RESTART,P1START,PFILE,CTLFILE)
      IF(IFAIL.GT.0)THEN
        CALL WRITE_PEST_MESSAGE(0)
        GO TO 9990
      END IF

#ifdef BEO
      IF(BEOMASTER)THEN
        IF((NOPTMAX.EQ.-1).AND.(IFINSTOPFLAG.EQ.1))THEN
          WRITE(6,111)
111       FORMAT(/,' NOPTMAX is set to zero.')
113       WRITE(6,112,ADVANCE='NO')
112       FORMAT(' Are you sure you want to run BEOPEST? [y/n]: ')
          READ(5,'(A)') AA
          IF((AA.EQ.'Y').OR.(AA.EQ.'Y'))THEN
            AA='Y'
          ELSE IF((AA.EQ.'N').OR.(AA.EQ.'n'))THEN
            AA='N'
          ELSE
            GO TO 113
          END IF
          IF(AA.EQ.'N')GO TO 9990
        END IF
      END IF
#endif


#ifdef BEO
      if (BEOSLAVE) then
        CALL RUNSLAVE(IFAIL)
        IF(IFAIL.GT.0)THEN
          CALL WRITE_PEST_MESSAGE(0)
        END IF
      else
#endif
C -- The name of the JCO file to read Jacobian data for "/i" restart
C    option is provided.

      IF((RESTART.EQ.5).OR.(RESTART.EQ.6))THEN
        IF(PFILE.NE.0)THEN
          WRITE(6,148)
148       FORMAT(' PEST cannot be started with both the ',
     +    '"/f" and "/i" switches.')
          GO TO 9990
        END IF
        WRITE(6,*)
149     WRITE(6,150,ADVANCE='NO')
150     FORMAT(' Enter name of JCO file for first iteration ',
     +  'sensitivities: ')
        READ(5,*) JCOFIRSTFILE
#ifndef UNIX
        CALL LOWCAS(JCOFIRSTFILE)
#endif
        INQUIRE(FILE=JCOFIRSTFILE,EXIST=LEXIST)
        IF(.NOT.LEXIST)THEN
          WRITE(6,151)
151       FORMAT('  *** File does not exist - try again ***')
          GO TO 149
        END IF
        CALL PUT_JCOFIRST(JCOFIRSTFILE)
        IF(RESTART.EQ.6)THEN
          RRFFIRSTFILE=' '
159       WRITE(6,*)
          WRITE(6,160,ADVANCE='NO')
160       FORMAT(' Name of RRF file to get first iteration ',
     +    'results (<Enter> if none): ')
          READ(5,'(A)') RRFFIRSTFILE
          IF(RRFFIRSTFILE.NE.' ')THEN
            RRFFIRSTFILE=ADJUSTL(RRFFIRSTFILE)
            AA=RRFFIRSTFILE(1:1)
            IF((AA.EQ.'''').OR.(AA.EQ.'"'))THEN
              RRFFIRSTFILE=ADJUSTL(RRFFIRSTFILE(2:))
              NN=INDEX(RRFFIRSTFILE,AA)
              IF(NN.EQ.0) THEN
                GO TO 159
              ELSE IF(NN.EQ.1)THEN
                RRFFIRSTFILE=' '
              ELSE
                RRFFIRSTFILE=RRFFIRSTFILE(1:NN-1)
              END IF
            END IF
          END IF
          IF(RRFFIRSTFILE.NE.' ')THEN
#ifndef UNIX
            CALL LOWCAS(RRFFIRSTFILE)
#endif
            INQUIRE(FILE=RRFFIRSTFILE,EXIST=LEXIST)
            IF(.NOT.LEXIST)THEN
              WRITE(6,151)
              GO TO 159
            END IF
            CALL PUT_RRFFIRST(RRFFIRSTFILE)
          END IF
        END IF
        JACUPDATE_RETAIN=0
        IF(RESTART.EQ.6)THEN
          IF((JACUPDATE.GT.0).AND.(IBOUNDSCALE.EQ.0))THEN
162         WRITE(6,*)
            WRITE(6,161,ADVANCE='NO')
161         FORMAT(' Persist Broyden improvements between ',
     +      'iterations? [y/n]: ')
            READ(5,*) AYN
            IF((AYN.EQ.'Y').OR.(AYN.EQ.'y'))THEN
              JACUPDATE_RETAIN=1
            ELSE IF((AYN.EQ.'N').OR.(AYN.EQ.'n'))THEN
              CONTINUE
            ELSE
              GO TO 162
            END IF
          END IF
        END IF
      END IF

      IF(PFILE.NE.0)THEN
        WRITE(6,210)
210     FORMAT(/,' PEST has been called with the "/f" switch.')
        WRITE(6,215)
215     FORMAT(/,' PEST will run the model repeatedly, using ',
     +  'parameter values recorded ',/,' in a sequence of ',
     +  'parameter value files. It will record all ',
     +   'model-calculated',/,' observations in a run results file.')
        WRITE(6,*)
199     WRITE(6,200,ADVANCE='NO')
#ifdef LAHEY
#ifdef UNIX
200     FORMAT(' Enter filename base of parameter value files: ')
#else
200     FORMAT('  Enter filename base of parameter value files: ')
#endif
#else
200     FORMAT(' Enter filename base of parameter value files: ')
#endif
        READ(5,*,ERR=199) PF_BASE
#ifndef UNIX
        CALL LOWCAS(PF_BASE)
#endif
219     CONTINUE
        WRITE(6,220,ADVANCE='NO')
220     FORMAT(' Enter first index to use: ')
        READ(5,*,ERR=219) PF_START
C        IF(PF_START.LT.0)THEN
C          WRITE(6,*)
#ifdef LAHEY
#ifndef UNIX
C          WRITE(6,'(A)',ADVANCE='NO') ' '
#endif
#endif
C          WRITE(6,230)
C230       FORMAT(' *** Must be zero or greater - try again ***',/)
#ifdef LAHEY
#ifndef UNIX
C          WRITE(6,'(A)',ADVANCE='NO') ' '
#endif
#endif
C          GO TO 219
C        END IF
239     CONTINUE
        WRITE(6,249,ADVANCE='NO')
249     FORMAT(' Enter last index to use: ')
        READ(5,*,ERR=239) PF_END
        IF(PF_END.LT.PF_START)THEN
          WRITE(6,*)
#ifdef LAHEY
#ifndef UNIX
          WRITE(6,'(A)',ADVANCE='NO') ' '
#endif
#endif
          WRITE(6,250)
250       FORMAT(' *** Must be greater than first index ',
     +    '- try again ***',/)
#ifdef LAHEY
#ifndef UNIX
          WRITE(6,'(A)',ADVANCE='NO') ' '
#endif
#endif
          GO TO 239
        END IF
        DO I=PF_START,PF_END
          CALL WRITINT(ANUM,I)
          APVFILE=TRIM(PF_BASE)//TRIM(ANUM)//'.par'
          INQUIRE(FILE=APVFILE,EXIST=LEXIST)
          IF(.NOT.LEXIST)THEN
            WRITE(6,260)
260         FORMAT(/,' At least one indexed parameter value ',
     +      'file cannot be opened.')
            WRITE(6,270) TRIM(APVFILE)
270         FORMAT(' Missing file = ',a)
            GO TO 9990
          END IF
        END DO

#ifdef PARALLEL
        WRITE(6,*)
272     WRITE(6,271,ADVANCE='NO')
#ifdef LAHEY
#ifdef UNIX
271     FORMAT(' Enter parallel run packet size: ')
#else
271     FORMAT('  Enter parallel run packet size: ')
#endif
#else
271     FORMAT(' Enter parallel run packet size: ')
#endif
        READ(5,*,ERR=272) PACKETSIZE
        IF(PACKETSIZE.LT.1)THEN
#ifdef LAHEY
#ifndef UNIX
          WRITE(6,'(A)',ADVANCE='NO') ' '
#endif
#endif
          WRITE(6,274)
274       FORMAT(' *** Must be greater than zero - try again ***',/)
          GO TO 272
        END IF
        IF(PACKETSIZE.GT.PF_END-PF_START+1)PACKETSIZE=PF_END-PF_START+1
#endif

        WRITE(6,*)
279     WRITE(6,280,ADVANCE='NO')
#ifdef LAHEY
#ifdef UNIX
280     FORMAT(' Enter name for run results file: ')
#else
280     FORMAT('  Enter name for run results file: ')
#endif
#else
280     FORMAT(' Enter name for run results file: ')
#endif
        READ(5,*,ERR=279) RRFILE
#ifndef UNIX
        CALL LOWCAS(RRFILE)
#endif
        NB_RRF=LEN_TRIM(RRFILE)
        NB_RRF=NB_RRF-3
        IF(NB_RRF.LT.1)NB_RRF=1
        AEXT=RRFILE(NB_RRF:)
        CALL LOWCAS(AEXT)
        IF(AEXT.NE.'.rrf')THEN
          WRITE(6,290)
#ifdef LAHEY
#ifdef UNIX
290       FORMAT(/,' Run record file must have an extension of ',
     +    '".rrf" - try again.',/)
#else
290       FORMAT(/,'  Run record file must have an extension of ',
     +    '".rrf" - try again.',/)
#endif
#else
290       FORMAT(/,' Run record file must have an extension of ',
     +    '".rrf" - try again.',/)
#endif
          GO TO 279
        END IF

        CALL PUT_RRF_DETAILS(PF_BASE,PF_START,PF_END,PACKETSIZE,
     +  RRFILE)
      END IF

C -- Note, when returning from run_pest:-
C          ifail=0   Normal return
C          ifail=1   Return with error - new format.
C          ifail=2   Return with error - old format
C          ifail=-1  User requested immediate stop.

#ifdef BEO
      IALLOC_BEORUN_DATA=0
#endif
      CALL RUN_PEST(IFAIL,AVERSION,ATEXT)
      IF(IFAIL.GT.0)THEN
        CALL WRITE_PEST_MESSAGE(0)
      END IF
#ifdef BEO
      endif
#endif

C -- Memory from the PEST_DATA module is deallocated.

9990  CONTINUE
      CALL PEST_DATA_DEALLOCATE(IFAIL)
#ifdef BEO
C
      call BEOFIN
#endif

C -- Now close PEST files. (Some of these may be open after PEST has
C    finished execution - particularly the run record file so that
C    an error message can be written to it.)

      CALL PEST_FILES_CLOSE()

9999  CONTINUE

       END


      SUBROUTINE GET_COMMAND_LINE(COMMAND_LINE)

C -- Subroutine GET_COMMAND_LINE retreives any text supplied on the command line.

      IMPLICIT NONE
      CHARACTER (LEN=*), INTENT(OUT)  :: COMMAND_LINE

      INTEGER             :: IARGC
      INTEGER             :: LLEN,NARG,IB,I,NB,IBB
      CHARACTER (LEN=150) :: ARG(10)

      COMMAND_LINE=' '
#ifdef LAHEY
       CALL GETCL(COMMAND_LINE)
       GO TO 100
#else
       LLEN=LEN(COMMAND_LINE)
       NARG=IARGC()
       IF(NARG.EQ.0) GO TO 100
       IB=0
       DO I=1,MIN(NARG,10)
         CALL GETARG(I,ARG(I))
         IF(I.GT.1)THEN
           IF((ARG(I-1).EQ.'/T').OR.(ARG(I-1).EQ.'/t'))THEN
             ARG(I)='"'//TRIM(ARG(I))//'"'
           ENDIF
         END IF
         NB=LEN_TRIM(ARG(I))
         IBB=MIN(IB+NB+1,LLEN)
         COMMAND_LINE(IB+1:IBB)= ARG(I)(1:NB)
         IB=IBB
         IF(IB.GE.LLEN) GO TO 100
       END DO
#endif

100   CONTINUE
      RETURN

      END



      SUBROUTINE PARSE_COMMAND_LINE(IFAIL,COMMAND_LINE,CTLFILE,RESTART,
     +P1START,PFILE,HPSTART,IPAROBS,ATEXT)

C -- Subroutine PARSE_COMMAND_LINE parses the PEST command line.
#ifdef BEO
      USE BEOPEST
#else
#ifdef GENIE
      USE GENIE_DATA
#endif
#endif

      IMPLICIT NONE

      INTEGER, INTENT(OUT)              :: IFAIL
      CHARACTER (LEN=*), INTENT(INOUT)  :: COMMAND_LINE
      CHARACTER (LEN=*), INTENT(OUT)    :: CTLFILE
      INTEGER, INTENT(OUT)              :: RESTART
      INTEGER, INTENT(OUT)              :: P1START
      INTEGER, INTENT(OUT)              :: PFILE
      INTEGER, INTENT(OUT)              :: HPSTART
      INTEGER, INTENT(OUT)              :: IPAROBS
      CHARACTER*(*), INTENT(OUT)        :: ATEXT

      INTEGER                           :: K,IR,IS,IJ,ID,I,IO,IP1,IPF
      INTEGER                           :: IH,IM,IL,NP
      INTEGER                           :: IOO,IPO
      INTEGER                           :: IT,JJ,NN
      INTEGER                           :: MINSLASH,IHP,IHP1
      CHARACTER (LEN=1)                 :: AA,BB
      CHARACTER (LEN=10)                :: ATEMP

      INTEGER J
      REAL CPU_SLAVE_TIME_INDEX
      REAL TIME1,TIME2
      DOUBLE PRECISION RTEMP1,RTEMP2,RTEMP3

      IFAIL=0
      IF(COMMAND_LINE.EQ.' ') GO TO 9000
      AA=' '
      COMMAND_LINE=ADJUSTL(COMMAND_LINE)
      CTLFILE=COMMAND_LINE
      IF(CTLFILE(1:1).EQ.'"')THEN
        AA='"'
      ELSE IF(CTLFILE(1:1).EQ.'''')THEN
        AA=''''
      END IF
      IF(AA.NE.' ') CTLFILE=CTLFILE(2:)
      I=INDEX(CTLFILE,AA)
      IF(I.LE.1) GO TO 9000
      CTLFILE=CTLFILE(1:I-1)
      RESTART=0
      PFILE=0
      IR=0
      IJ=0
      IS=0
      ID=0
      IO=0
      IOO=0
      IPO=0
      IH=0
      IM=0
      IP1=0
      IT=0
      IPF=0
      HPSTART=0
      IPAROBS=0
      IR=INDEX(COMMAND_LINE,' /r ')
      IF(IR.EQ.0) IR=INDEX(COMMAND_LINE,' /R ')
      IJ=INDEX(COMMAND_LINE,' /j ')
      IF(IJ.EQ.0) IJ=INDEX(COMMAND_LINE,' /J ')
      IS=INDEX(COMMAND_LINE,' /s ')
      IF(IS.EQ.0) IS=INDEX(COMMAND_LINE,' /S ')
      ID=INDEX(COMMAND_LINE,' /d ')
      IF(ID.EQ.0) ID=INDEX(COMMAND_LINE,' /D ')
      IO=INDEX(COMMAND_LINE,' /i ')
      IF(IO.EQ.0) IO=INDEX(COMMAND_LINE,' /I ')
      IOO=INDEX(COMMAND_LINE,' /ii ')
      IF(IOO.EQ.0) IOO=INDEX(COMMAND_LINE,' /II ')
      IPO=INDEX(COMMAND_LINE,' /po ')
      IF(IPO.EQ.0) IPO=INDEX(COMMAND_LINE,' /PO ')
      IH=INDEX(COMMAND_LINE,' /h ')
      IF(IH.EQ.0) IH=INDEX(COMMAND_LINE,' /H ')
      IM=INDEX(COMMAND_LINE,' /m ')
      IF(IM.EQ.0) IM=INDEX(COMMAND_LINE,' /M ')
      IL=INDEX(COMMAND_LINE,' /l ')
      IF(IL.EQ.0) IL=INDEX(COMMAND_LINE,' /L ')
      IP1=INDEX(COMMAND_LINE,' /p1 ')
      IF(IP1.EQ.0) IP1=INDEX(COMMAND_LINE,' /P1 ')
      IT=INDEX(COMMAND_LINE,' /t ')
      IF(IT.EQ.0) IT=INDEX(COMMAND_LINE,' /T ')
      IPF=INDEX(COMMAND_LINE,' /f ')
      IF(IPF.EQ.0) IPF=INDEX(COMMAND_LINE,' /F ')
      IHP=INDEX(COMMAND_LINE,' /h')
      IHP1=INDEX(COMMAND_LINE,' /H')
      IF(IHP.EQ.0)THEN
        IHP=IHP1
      ELSE IF(IHP1.EQ.0)THEN
        CONTINUE
      ELSE
        IHP=MIN(IHP,IHP1)
      END IF
      IF(IHP.NE.0)THEN
        ATEMP=COMMAND_LINE(IHP+1:IHP+9)
        CALL LOWCAS(ATEMP)
        IF(ATEMP(1:9).NE.'/hpstart ') THEN
          IHP=0
        ELSE
          COMMAND_LINE(IHP+1:IHP+8)=' '
          HPSTART=1
        END IF
      END IF
      MINSLASH=0
      MINSLASH=MAX(IR,IJ,IS,ID,IO,IH,IM,IL,IP1,IT,IPF,IHP)
      IF(MINSLASH.NE.0)THEN
        IF(IR.NE.0) MINSLASH=MIN(MINSLASH,IR)
        IF(IJ.NE.0) MINSLASH=MIN(MINSLASH,IJ)
        IF(IS.NE.0) MINSLASH=MIN(MINSLASH,IS)
        IF(ID.NE.0) MINSLASH=MIN(MINSLASH,ID)
        IF(IO.NE.0) MINSLASH=MIN(MINSLASH,IO)
        IF(IOO.NE.0) MINSLASH=MIN(MINSLASH,IOO)
        IF(IPO.NE.0) MINSLASH=MIN(MINSLASH,IPO)
        IF(IH.NE.0) MINSLASH=MIN(MINSLASH,IH)
        IF(IM.NE.0) MINSLASH=MIN(MINSLASH,IM)
        IF(IL.NE.0) MINSLASH=MIN(MINSLASH,IL)
        IF(IP1.NE.0) MINSLASH=MIN(MINSLASH,IP1)
        IF(IT.NE.0) MINSLASH=MIN(MINSLASH,IT)
        IF(IPF.NE.0) MINSLASH=MIN(MINSLASH,IPF)
        IF(IHP.NE.0) MINSLASH=MIN(MINSLASH,IHP)
      END IF

      IF(IR.NE.0)THEN
        RESTART=1
        COMMAND_LINE(IR+1:IR+2)='  '
        IR=1
      END IF
      IF(IJ.NE.0)THEN
        RESTART=2
        COMMAND_LINE(IJ+1:IJ+2)='  '
        IJ=1
      END IF
      IF(IS.NE.0)THEN
        RESTART=3
        COMMAND_LINE(IS+1:IS+2)='  '
        IS=1
      END IF
      IF(ID.NE.0)THEN
        RESTART=4
        COMMAND_LINE(ID+1:ID+2)='  '
        ID=1
      END IF
      IF(IO.NE.0)THEN
        RESTART=5
        COMMAND_LINE(IO+1:IO+2)='  '
        IO=1
      END IF
      IF(IOO.NE.0)THEN
        RESTART=6
        COMMAND_LINE(IOO+1:IOO+3)='   '
        IOO=1
      END IF
      IF(IPO.NE.0)THEN
        IPAROBS=1
        COMMAND_LINE(IPO+1:IPO+3)='   '
        IPO=1
      END IF
      IF(IP1.NE.0)THEN
        P1START=1
        COMMAND_LINE(IP1+1:IP1+3)='  '
        IP1=1
      END IF
      IF(IT.NE.0)THEN
        ATEXT=COMMAND_LINE(IT+3:)
        ATEXT=ADJUSTL(ATEXT)
        BB=ATEXT(1:1)
        IF(BB.NE.'"') GO TO 9200
        ATEXT=ADJUSTL(ATEXT(2:))
        NN=INDEX(ATEXT,BB)
        IF(NN.EQ.0) GO TO 9200
        ATEXT=ATEXT(1:NN-1)
        JJ=0
        DO I=IT,LEN_TRIM(COMMAND_LINE)
          IF(COMMAND_LINE(I:I).EQ.'"') JJ=JJ+1
          COMMAND_LINE(I:I)=' '
          IF(JJ.EQ.2) GO TO 15
        END DO
15      CONTINUE
        NN=LEN_TRIM(COMMAND_LINE)
        IF(NN.GT.IT)THEN
          DO I=IT,NN
            BB=COMMAND_LINE(I:I)
            IF(BB.NE.' ')THEN
              IF(BB.NE.'/') GO TO 9200
              GO TO 16
            END IF
          END DO
        END IF
16      CONTINUE
      END IF
      IF(IPF.NE.0)THEN
        PFILE=1
        COMMAND_LINE(IPF+1:IPF+2)='  '
        IPF=0
      END IF

#ifdef BEO

C -------------------------------------------------------------------
C -- The following is temporary as the MPI option does not presently
C    exist for BEOPEST when using SVDA. Also I think that, for the
C    moment at least, if the user uses BEOPEST they should use the
C    /H switch. Its absence will be detected and information on
C    how to run BEOPEST will be provided on the command line.
C     if(ih.eq.0) go to 9000
C -------------------------------------------------------------------

      if((im.eq.0).and.(ih.eq.0))then
        write(6,*)
        write(6,17)
17      format(' BEOPEST needs control file and "/h" or "/m" ',
     +  'on command line.')
        stop
      end if
      BEOLASTLOC = (IL.ne.0)
      if (IL.ne.0) COMMAND_LINE(IL+1:IL+2) = '  '
      if (IH.ne.0 .and. IM.ne.0) then
        STOP '/H and /M are mutually exclusive'
      else if (IH.ne.0 .or. IM.ne.0) then
        if (IH.ne.0) then
          NP = 8192
          IL = IH
        else
          NP = 0
          IL = IM
        endif
        COMMAND_LINE(IL+1:IL+2) = '  '
        do K=IL+4,LEN(COMMAND_LINE)
          if (COMMAND_LINE(K:K).ne.' ') goto 100
        enddo
        if (IH.ne.0) then
          IF(IT.EQ.0)THEN
            STOP 'Missing host:port'
          ELSE
            write(6,*)
            write(6,*) 'BEOPEST - missing host:port'
            write(6,*)
            write(6,*) 'or'
            GO TO 9200
          END IF
        else
          STOP 'Missing directory'
        endif
100     IL = K
        do K=IL+1,LEN(COMMAND_LINE)
          if (COMMAND_LINE(K:K).eq.' ') goto 199
        enddo
199     K = K-1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       The speed test is done in TCPopen which is
C       called from BEOINIT  (WAS 2/9/2016)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       call slave_or_master(ifail,command_line(il:k))
C
C       if(beoslave)then
C         write(6,111)
C111      format(/,' Performing numerical speed test...')
C         rtemp3=1.0
C         rtemp1=1.0
C         call speed_test(rtemp3,rtemp1,time1,cpu_slave_time_index)
C       end if

c        if(ifail.ne.0) go to 9000                                !was     !!! commented out 21/9/2011 as per willem
c        if(beomaster)then                                        !was     !!! commented out 21/9/2011 as per willem
          call BEOINIT(COMMAND_LINE(IL:K),NP)
c        end if                                                   !was     !!! commented out 21/9/2011 as per willem
C        if(beoslave)then
C          call tcpsend(0,cpu_slave_time_index,4,1)
C        end if
                                                                           !!! if uncomment also uncomment !was lines in beorun.f
        COMMAND_LINE(IL:K) = '  '
      else
        BEOMASTER = .false.
        BEOSLAVE  = .false.
      endif
#else
#ifdef GENIE
      if(IH.eq.0) go to 9000
c     get start of next argument
      IL = IH
      COMMAND_LINE(IL+1:IL+2) = '  '
      do K=IL+4,LEN(COMMAND_LINE)
        if (COMMAND_LINE(K:K).ne.' ') goto 100
      end do
      write(6,'(/,1x,a,/)') 'STOP: Missing HOST:PORT'
c     get end of argument
100   IL = K
      do K=IL+1,LEN(COMMAND_LINE)
        if (COMMAND_LINE(K:K).eq.' ') goto 199
      end do
199   K = K-1
      call checkhost(ifail,command_line(il:k))
      if(ifail.ne.0) go to 9000
      COMMAND_LINE(IL:K) = '  '

#endif
#endif

      IF(IO+IOO.GT.1) GO TO 9250
      IF(IR+IJ+IS+ID+IO.GT.1) GO TO 9000
      IF(INDEX(COMMAND_LINE,' /').NE.0) GO TO 9000
      IF(MINSLASH.NE.0) COMMAND_LINE(MINSLASH:)=' '

C -- The following is used to handle spaces in filenames because of idiosyncracies
C    in the command line argument functionality.

      IF(AA.EQ.' ')THEN
        CTLFILE=COMMAND_LINE
      END IF

#ifndef UNIX
      CALL LOWCAS(CTLFILE)
#endif

      RETURN

9000  IFAIL=1
      RETURN

9200  CONTINUE
      WRITE(6,*)
      WRITE(6,9210)
9210  FORMAT(' If "/t" switch is used, it must be followed by ',
     +'text enclosed in double quotes.')
      IFAIL=2
      RETURN

9250  CONTINUE
      WRITE(6,*)
      WRITE(6,9260)
9260  FORMAT(' "/i" and "/ii" cannot appear on the same command line.')
      IFAIL=2
      RETURN

      END


      SUBROUTINE PUT_JCOFIRST(AFILE)

      USE PESTDATA
      IMPLICIT NONE
      CHARACTER*(*) AFILE

      JCOFIRSTFILE=AFILE

      RETURN
      END


      SUBROUTINE PUT_RRFFIRST(AFILE)

      USE PESTDATA
      IMPLICIT NONE
      CHARACTER*(*) AFILE

      RRFFIRSTFILE=AFILE

      RETURN
      END



      SUBROUTINE PUT_RRF_DETAILS(PPFILE,I1,I2,PSIZE,RRFILE)

      USE PESTDATA
      IMPLICIT NONE
      CHARACTER*(*) PPFILE,RRFILE
      INTEGER       I1,I2,PSIZE

      PARFILEBASE=PPFILE
      PARFILE_START=I1
      PARFILE_END=I2
      PACKETSIZE=PSIZE
      RUN_RESULTS_FILE=RRFILE

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  I think this is now redundant - BEOINIT takes care of this  (WAS 2/9/2016)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC#ifdef BEO
CCC
CCC      subroutine slave_or_master(ifail,astring)
CCC
CCCC -- Subroutine SLAVE_OR_MASTER parses the command line segment to ascertain
CCCC    whether BEOPEST is beinf run as a slave or master.
CCC
CCC      use beopest
CCC      implicit none
CCC
CCC      integer, intent(out)        :: ifail
CCC      character*(*), intent(in)   :: astring
CCC      integer                     :: n
CCC
CCC      ifail=0
CCC      n=index(astring,':')
CCC      if(n.eq.0) go to 9890
CCC      beocommand=astring
CCC      if(n.eq.1) go to 20
CCC      if(astring(1:n-1).eq.' ') go to 20
CCC      beomaster=.false.
CCC      beoslave=.true.
CCC      return
CCC20    continue
CCC      beoslave=.false.
CCC      beomaster=.true.
CCC      return
CCC
CCC9890  continue
CCC      ifail=1
CCC      return
CCC      end
CCC
CCC#endif
