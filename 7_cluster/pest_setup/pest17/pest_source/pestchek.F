        PROGRAM PSTCHK

        INTEGER, PARAMETER :: MAXABSPARMAX=10

        LOGICAL MAYBE_PLPROC
        INTEGER I,J,NCASE,IFAIL,IERR,NPAR,NOBS,NPRIOR,NPARGP,
     +          MCASE,NINFLE,NOUTFL,NUMRUN,ITEMP,NOPTMX,K,IS,
     +          NPSILO,NLOPT,NUMCHG,ICOV,ICOR,IEIG,IRES,MMLINE,
     +          II,WRKLEN,J1,J2,NBLC,MAXWID,NUMERR,N1,N2,N3,NUM1,
     +          NUM2,IOBS,ALREDY,NESPAR,GPDIM,PARDIM,OBSDIM,FLEDIM,
     +          INDIM,OUTDIM,NESDIM,PRRDIM,OZROWT,PZROWT
        INTEGER IFAIL4,JLINE,ILINE,NOBSGP,NOBSGP1,OBGDIM,OBNDIM
        INTEGER IPRED,IPROLD,JPREDICT,IPCOUNT,NPMXMN,NPRNRD,
     +          NPRDLO,IFINFLAG,IPFLAG1,NSEARCH,NPRWRN
        INTEGER IREG,JREGOBS,IREGFLAG,NUMCOM,JACFILE,IMSFLE,ICOM,
     +          JCWARN,NUMCOV,MXOBSCOV,ICOUNT,IGROUP,NNCOV,JERR
        INTEGER AUIOPT,MAXAUI,NAUINOACC,AUINUMFREE,AUIHOLDMAXCHG,
     +          AUIREST,IASECT
        INTEGER IVECBND,IBNDSTK,IREGADJ,IPCG,MEMSAVE,CGITNLIM,LPOS,
     +          FPOS,SVDMODE,MAXSING,EIGWRITE,ISVD,ISVDA,NOMOREIW_CHECK,
     +          MAXCOMPDIM
        INTEGER IALERR, NBLNK
        INTEGER KLINE,DI_NSKIP,DI_NDEP,DI_NPAR,IPAR,IDEP,JCOUNT,JJ,
     +          ISTOP,NOPTSWITCH,IFIND
        INTEGER IMCOVFLAG,NNCODE,NNDIM1,NNDIM2,SVDA_MULBPA,
     +          SVDA_NLSCALADJ,SVDA_EXTSUPER,SVDA_SUPDERCALC
        INTEGER PREDNOISE
        INTEGER ILSQR,LSQRWRITE,LSQRMODE,LSQR_ITNLIM
        INTEGER JACUPDATE
        INTEGER AUIFLAG,SENREUSEFLAG,ISENREUSE,SENMAXREUSE,
     +          SENALLCALCINT
        INTEGER ILINREG,IREGCONTINUE
        INTEGER MM1,MM2,MM3,MM4,MM5
        INTEGER LASTRUN,IUNIT
        INTEGER IJCO,IREI,IGTARG,IDELETE,ISPLIT
        INTEGER NOPTREGADJ
        INTEGER NB,CHECKFLAG
        INTEGER NN,LAMFORGIVE,DERFORGIVE
        INTEGER PESTMODE
        INTEGER NUM_ITER_THRESH,NOBS_REPORT,PAR_OBSGROUP,
     +          NUM_WTFAC_INC,NUM_ITER_START,NUM_ITER_GEN,
     +          NUM_ITER_FIN,ALT_TERM,NXROW
        INTEGER SVDA_PAR_EXCL
        INTEGER FLAG_ZERO_WEIGHT
        INTEGER JFLAG,VERBOSEREC,JCOSAVE,QUICKPRIOR,PARSAVEITN
        INTEGER PARSAVERUN,WRITE_RRF
        INTEGER REGCONTINUE
        INTEGER IREFOBS,IREFOBSCOUNT,IREFOBSCONT,IREFOBS_PAUSE
        INTEGER IBOUNDSCALE,IDOSENREUSE
        INTEGER IIFAIL
        INTEGER NNN,IZEROSENVAL,III,NN1
        INTEGER RSQR_ITNLIM,LSQR_STOPITER
        INTEGER ORR_NOT_FIRST
        INTEGER UPTESTLIM,UPTESTMIN
        INTEGER ISECWARN,IPRIMWARN
        INTEGER JJFLAG,NNLINE
        INTEGER SIMINCFLAG,RANDOMFLAG,RSIFLAG

        INTEGER MAX_DISTRIB_FILES
        INTEGER NUM_DISTRIB_FILES
        INTEGER NPARFILE,NPARSEC,NPARTOT,NEQUATION
        INTEGER DISTRIB_TYPE(5)

        INTEGER IDERIV(:),J2OR3(:),I3CALC(:),IDBND(:),CHGTYP(:),
     +                   DERCOM(:),SPLITACTION(:)
        INTEGER LW(20),RW(20),ITRANS(:),IWORK(:),NUMPAR(:),NUMOBS(:),
     +          IPARGP(:),IOBGNM(:),IRGP(:),IW_PR(:)
        INTEGER DI_PAR(:),DI_PARENT(:)
        INTEGER NNUMOBS(:)

        DOUBLE PRECISION RTEMP,LMBDA1,LAMFAC,LAMPSI,FRCLAM,
     +                   DMAX0,DMAX1,FACORG,PSI3,PSISTP,ENDCHG,OVAL,
     +                   WEIGHT
        DOUBLE PRECISION PD0,PD1,PD2,ABPRED,REPRED,ASWPRD,RSWPRD,
     +                   PRSTPA,PRSTPR,LINITFAC,LFACFAC,RTEMP1,RTEMP2
        DOUBLE PRECISION WFINIT,WFFAC,WFMIN,WFMAX,REGTOL,PD1R,PHIMLIM,
     +                   FRACPHIM,CGRTOL
        DOUBLE PRECISION ALAMPSI,APHIRATACC,AUISENSRAT,NOAUIPHIRAT,
     +                   EIGTHRESH,DERZEROLIM,PWTKP
        DOUBLE PRECISION LSQR_ATOL,LSQR_BTOL,LSQR_CONLIM
        DOUBLE PRECISION SENRELTHRESH,SENPREDWEIGHT
        DOUBLE PRECISION PHISTOPTHRESH,PHIAB
        DOUBLE PRECISION SPLITSWH
        DOUBLE PRECISION REGWEIGHTRAT,REGSINGTHRESH
        DOUBLE PRECISION OBS_THRESH,PAR_WTFAC_START,
     +                   PAR_WTFAC_FIN
        DOUBLE PRECISION RUN_SLOW_FAC,RUN_ABANDON_FAC
        DOUBLE PRECISION WIN_MRUN_HOURS
        DOUBLE PRECISION SOFTSTOPHOURS,HARDSTOPHOURS
        DOUBLE PRECISION ZEROSENVAL
        DOUBLE PRECISION RSQR_ATOL,RSQR_BTOL,RSQR_CONLIM
        DOUBLE PRECISION REG2MEASRAT,JCOWARNTHRESH,JCOZEROTHRESH

        DOUBLE PRECISION ABSPARMAX(MAXABSPARMAX)

        DOUBLE PRECISION DERVAL(:),DERMUL(:),DERBND(:),PVAL(:),
     +                   LBOUND(:),UBOUND(:),X(:,:),Y(:),COVAR(:,:)
        DOUBLE PRECISION PHIABANDON(:)
        DOUBLE PRECISION GTARG(:)
        DOUBLE PRECISION SPLITTHRESH(:),SPLITRELDIFF(:)
        DOUBLE PRECISION OWGHT(:)

        CHARACTER*10 AFIG,ATEMP*20,ALINE*20,AALINE*200,PARDEL*1,TPAR*12,
     +               FMT*20,AA*1,AMXWID,PRECIS*8,OBSNAM*40
        CHARACTER*12   AVERSION,AK
        CHARACTER*20   OTPAR
        CHARACTER*1    ACODE, ACODEP,MODPRED
        CHARACTER*10   AMEMSAVE
        CHARACTER*12   ASUFFIX,APLACE
        CHARACTER*3    A3
        CHARACTER*5    APCG
        CHARACTER*100  CASFL
        CHARACTER*2120 ERRMSG
        CHARACTER*50   TCLINE
        CHARACTER*50   ATEMP50
        CHARACTER*256  AJACFLE
        CHARACTER*100  DCOMLIN
        CHARACTER*300  DLINE
        CHARACTER*12   ATEMP12,ATEMP20*20,DI_DERFORMAT*50,
     +                 DI_ORIENTATION*10
        CHARACTER*200  MODDERFILE,ATEMP1,ATEMP2
        CHARACTER*10   DOSENREUSE
        CHARACTER*4    SENPIEXCLUDE
        CHARACTER*200  SCHEDULEFILE
        CHARACTER*3    ASWITCH
        CHARACTER*25   ATEMP25,ADO
        CHARACTER*20   OBS_TERM
        CHARACTER*256  SOURCEFILE_AGENT(5)
        CHARACTER*256  TARGETFILE_MASTER(5)
        CHARACTER*256  SOURCEFILE_MASTER(5)
        CHARACTER*256  TARGETFILE_AGENT(5)
        CHARACTER*256  DISTRIB_COMLIN
        CHARACTER*256  AFILE
        CHARACTER*2    DUMFILE
        CHARACTER*20   VARTEXT,UPVARTEXT
        CHARACTER*256  FILEPARFILE

        CHARACTER*100  TMPFLE(:),MODFLE(:),INSFLE(:),COMLIN(:),
     +                 COVFLE(:)
        CHARACTER*5    DOAUI
        CHARACTER*12   AGROUP(:),APAR(:),AOBSGP(:)
        CHARACTER*20   AOBS(:),APRIOR(:),TEMPOBS(:)
        CHARACTER*20   DI_TEMP(:)
        CHARACTER*20   OBS_REPORT(:)

C -- PEST++ variables.

        integer          idumpp
        integer          ippcount
        integer          NALINE
        integer          N_ITER_BASE, N_ITER_SUPER,MAX_N_SUPER,
     +                   MAX_REG_ITER
        integer          MAX_SUPER_FRZ_ITER,AUTO_NORM,MAX_RUN_FAIL
        double precision SUPER_EIGTHRES,SUPER_RELPARMAX
        double precision LAMBDAS(100)
        character*10     SVD_PACK,MAT_INV
        character*10     ITERATION_SUMMARY,DER_FORGIVE
        character*20     PPVAR(15)

C -- File size
        CHARACTER*2010 CLINE

        ALLOCATABLE::AGROUP,IDERIV,DERVAL,DERBND,J2OR3,DERMUL,I3CALC,
     +  IDBND,APAR,ITRANS,CHGTYP,PVAL,LBOUND,UBOUND,IPARGP,AOBS,IWORK,
     +  TMPFLE,NUMPAR,NUMOBS,MODFLE,INSFLE,APRIOR,X,Y,AOBSGP,IOBGNM,
     +  COMLIN,COVFLE,COVAR,IRGP,IW_PR,DERCOM,PHIABANDON,GTARG,NNUMOBS,
     +  TEMPOBS

        ALLOCATABLE::SPLITTHRESH,SPLITRELDIFF,SPLITACTION
        ALLOCATABLE::OBS_REPORT
        ALLOCATABLE::OWGHT

        ALLOCATABLE::DI_TEMP,DI_PAR,DI_PARENT

        COMMON /IERR/IERR,MCASE
        COMMON /ERRMSG/ERRMSG
        COMMON /CASFL/ CASFL
        COMMON /ALINE/ ALINE,AALINE
        COMMON /MMLINE/MMLINE

C -- File size
        MAXWID=2000
        CALL WRTINT(AMXWID,MAXWID)

        II=0
        IOBS=0
        JPREDICT=0
        IFINFLAG=0
        ACODE='m'
        ACODEP=' '
        MODPRED=' '
        OZROWT=0
        PZROWT=1
        DMAX0=0.0D0
        NPRWRN=0
        JCWARN=0
        NUMCOV=0
        IASECT=0
        MAXAUI=0
        IVECBND=-999
        IBNDSTK=-999
        ISVDA=0
        NOMOREIW_CHECK=0
        ISTOP=0
        AJACFLE=' '
        JREGOBS=0
        IRES=-9999
        IMCOVFLAG=0
        DERZEROLIM=0
        SVDA_MULBPA=0
        SVDA_NLSCALADJ=0
        SVDA_EXTSUPER=0
        SVDA_SUPDERCALC=0
        PREDNOISE=0
        ISENREUSE=0
        SCHEDULEFILE=' '
        SPLITSWH=-1.1D301
        PHISTOPTHRESH=0.0D0
        IGTARG=0
        CHECKFLAG=0
        PESTMODE=0
        FLAG_ZERO_WEIGHT=0
        VERBOSEREC=1
        JCOSAVE=1
        REGCONTINUE=0
        IBOUNDSCALE=0
        RUN_SLOW_FAC=-1.1D301
        RUN_ABANDON_FAC=-1.1D301
        WIN_MRUN_HOURS=-1.1D301
        IZEROSENVAL=0
        RSQR_ITNLIM=-999
        UPTESTLIM=-1
        UPTESTMIN=-1
        ISECWARN=0
        IPRIMWARN=0
        SIMINCFLAG=0
        RANDOMFLAG=0
        REG2MEASRAT=0.0D0
        JCOWARNTHRESH=0.0D0
        JCOZEROTHRESH=0.0D0

        MAX_DISTRIB_FILES=5
        NUM_DISTRIB_FILES=0
        DISTRIB_TYPE=0        ! An array
        SOURCEFILE_AGENT=' '  ! An array
        TARGETFILE_MASTER=' ' ! An array
        SOURCEFILE_MASTER=' ' ! An array
        TARGETFILE_AGENT=' '  ! An array
        DISTRIB_COMLIN=' '

C -- Note that the PEST++ variables are never used. The checking of these
C    can be programmed at some later stage.

        ierr=0
        idumpp=-99999999
        ITERATION_SUMMARY=' '
        DER_FORGIVE=' '
        N_ITER_BASE=idumpp
        N_ITER_SUPER=idumpp
        MAX_N_SUPER=idumpp
        MAX_REG_ITER=idumpp
        MAX_SUPER_FRZ_ITER=idumpp
        AUTO_NORM=idumpp
        MAX_RUN_FAIL=idumpp
        SUPER_EIGTHRES=-1.1d300
        SUPER_RELPARMAX=-1.1d300
        LAMBDAS(100)=-1.1d300                       ! an array
        ppvar(1)='SVD_PACK'
        ppvar(2)='MAT_INV'
        ppvar(3)='ITERATION_SUMMARY'
        ppvar(4)='DER_FORGIVE'
        ppvar(5)='N_ITER_BASE'
        ppvar(6)='N_ITER_SUPER'
        ppvar(7)='MAX_N_SUPER'
        ppvar(8)='MAX_REG_ITER'
        ppvar(9)='MAX_SUPER_FRZ_ITER'
        ppvar(10)='AUTO_NORM'
        ppvar(11)='MAX_RUN_FAIL'
        ppvar(12)='SUPER_EIGTHRES'
        ppvar(13)='SUPER_RELPARMAX'
        ppvar(14)='LAMBDAS(100)'
        ppvar(15)='SVD_PACK,MAT_INV'

        INCLUDE 'version.inc'
        WRITE(6,1,ERR=9900) TRIM(AVERSION)
1       FORMAT(' PESTCHEK Version ',A,'. ',
     +  'Watermark Numerical Computing.',/)

C -- THE COMMAND LINE IS READ AND PARSED

#ifdef NO_CMDLINE
3       WRITE(*,'(A)') ' Enter command line arguments > '
        READ(*,'(A)',ERR=3) CASFL
#else
        CALL PGETCL(CASFL)
#endif

C -- The presense of a switch is checked for.

        NB=LEN_TRIM(CASFL)
        IF(NB.GT.3)THEN
          ASWITCH=CASFL(NB-2:NB)
          CALL LOWCAS(ASWITCH)
          IF(ASWITCH(1:1).EQ.' ')THEN
            IF(ASWITCH(2:2).EQ.'/')THEN
              IF(ASWITCH(3:3).EQ.'s')THEN
                CHECKFLAG=1
                CASFL(NB-2:)=' '
              ELSE
                GO TO 77
              END IF
            ELSE
C              GO TO 77
            END IF
          END IF
        END IF

        CALL SPACESUB(CASFL)
        CALL SHIFTL(CASFL)
        IF(CASFL.EQ.' ') THEN
          WRITE(6,20,ERR=9900)
20        FORMAT(' PESTCHEK is run using the command:'/,/,T5,
     +    'pestchek case [/s]',/,/,' where',/,/,T5,
     +    '"case" is a case name (no filename extension), and',/
     +    T5,'/s stops warnings and template/instruction/cov. matrix ',
     +    'file checking.')
          ISTOP=1
          GO TO 9998
        END IF
        NCASE=NBLNK(CASFL)
C        DO 73 I=1,NCASE
C          IF(CASFL(I:I).EQ.' ') GO TO 77
C73      CONTINUE
        CALL REMCHAR(CASFL,CHAR(211))
#ifdef CAPFILE
        CALL UPCAS(CASFL)
#endif
        IF(NCASE.GE.5)THEN
#ifdef CAPFILE
          IF(CASFL(NCASE-3:NCASE).EQ.'.PST') CASFL(NCASE-3:NCASE)=' '
#else
          IF(CASFL(NCASE-3:NCASE).EQ.'.pst') CASFL(NCASE-3:NCASE)=' '
#endif
          NCASE=NBLNK(CASFL)
        END IF
        MCASE=NCASE+4
        DO 76 I=NCASE,1,-1
        IF(CASFL(I:I).EQ.'.') THEN
#ifdef UNIX
C[jed]          IF(CASFL(I+1:I+1).NE.'/') GO TO 77
#else
          IF(CASFL(I+1:I+1).NE.CHAR(92)) GO TO 77
#endif
          GO TO 100
        END IF
76      CONTINUE
        GO TO 100
77      WRITE(6,78,ERR=9900)
78      FORMAT(' *** command line error ***')
        WRITE(6,20,ERR=9900)
        ISTOP=1
        GO TO 9998

C -- The PEST control file is opened

100     WRITE(6,101,ERR=9900)
101     FORMAT(' Errors ----->')
#ifdef CAPFILE
        CASFL(NCASE+1:NCASE+4)='.PST'
#else
        CASFL(NCASE+1:NCASE+4)='.pst'
#endif
#ifdef UNICOS
        OPEN(UNIT=30,FILE=CASFL(1:MCASE),STATUS='OLD',RECL=1000,
     +  ERR=110)
#else
        OPEN(UNIT=30,FILE=CASFL(1:MCASE),STATUS='OLD',ERR=110)
#endif
        GO TO 150
110     WRITE(ERRMSG,120) CASFL(1:MCASE)
120     FORMAT(' Cannot open PEST control file ',A,'.')
        CALL WRTERR
        ISTOP=1
        GO TO 9998

150     CONTINUE
        ILINE=1
151     CONTINUE
        READ(30,'(A)',ERR=9600,END=9700) CLINE
c        CALL PPROC(CLINE,IPPCOUNT)
c        IF(CLINE.EQ.' ') THEN
c          ILINE=ILINE+1
c          GO TO 151
c        END IF
        CALL LOWCAS(CLINE)
        IF(CLINE.NE.'pcf') THEN
          WRITE(ERRMSG,130) CASFL(1:MCASE)
130       FORMAT(' File ',A,' does not have correct PEST control file ',
     +    'header.')
          CALL WRTERR
          GO TO 9800
        END IF

C -- THE CONTROL DATA SECTION OF THE PEST CONTROL FILE IS READ AND CHECKED

7601    CONTINUE
        ILINE=ILINE+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7601
        CALL LOWCAS(CLINE)
        CALL SHIFTL(CLINE)
        IF(CLINE.NE.'* control data')THEN
          CALL WRTINT(ALINE,ILINE)
          WRITE(6,140,ERR=9900) TRIM(ALINE),CASFL(1:MCASE)
140       FORMAT(' Line ',A,' of file ',A,' should read:',
     +    /,' * control data')
          GO TO 9800
        END IF

        IERR=0
7602    CONTINUE
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7602
        CALL LOWCAS(CLINE)
        CALL LINSPL(IFAIL,1,LW,RW,CLINE)
        IF(IFAIL.NE.0)THEN
          WRITE(ERRMSG,145) AALINE(1:MMLINE)
145       FORMAT(A,'character variable RSTFLE missing.')
          CALL WRTERR
          GO TO 9800
        END IF

        IF((CLINE(LW(1):RW(1)).NE.'restart').AND.
     +     (CLINE(LW(1):RW(1)).NE.'norestart'))THEN
           WRITE(ERRMSG,190) AALINE(1:MMLINE)
190        FORMAT(A,'character variable RSTFLE must be ',
     +     '"restart" or "norestart".')
           CALL WRTERR
        END IF

        CALL LINSPL(IFAIL,2,LW,RW,CLINE)
        I=2

        IPRED=0
        IREG=0
        PESTMODE=0
        IF(IFAIL.NE.0)THEN
          IPRED=0
          IPROLD=1
        ELSE
          IPROLD=0
          IF(CLINE(LW(I):RW(I)).EQ.'prediction')THEN
            IPRED=1
            PESTMODE=2
          ELSE IF(CLINE(LW(I):RW(I)).EQ.'estimation')THEN
            IPRED=0
            PESTMODE=1
          ELSE IF(INDEX(CLINE(LW(I):RW(I)),'regula').NE.0)THEN
            IREG=1
            PESTMODE=3
          ELSE IF(INDEX(CLINE(LW(I):RW(I)),'pareto').NE.0)THEN
            PESTMODE=4
          ELSE
            WRITE(ERRMSG,196) AALINE(1:MMLINE)
196         FORMAT(A,'character variable PESTMODE must be ',
     +      '"estimation", "prediction", "regularisation" or ',
     +      '"pareto".')
            CALL WRTERR
            GO TO 9800
          END IF
        END IF

        NPAR=0
        NPARSEC=0
        NPARFILE=0
        NPARTOT=0
        NPARGP=0
        NOBS=0
        NPRIOR=0
        NOBSGP=0
        NXROW=0

200     CONTINUE
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        NALINE=LEN_TRIM(ALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 200

        VARTEXT='fileparfile'                          ! Must be first so that filename can be case sensitive
        UPVARTEXT='FILEPARFILE'
        FILEPARFILE=' '
        CALL READ_MRUN_VAR_PC(IFAIL,RTEMP,VARTEXT,UPVARTEXT,
     +  CLINE,2,FILEPARFILE,NALINE)
        VARTEXT='nparsec'
        UPVARTEXT='NPARSEC'
        RTEMP=0.0D0             ! default value
        CALL READ_MRUN_VAR_PC(IFAIL,RTEMP,VARTEXT,UPVARTEXT,
     +  CLINE,1,DUMFILE,NALINE)
        NPARSEC=NINT(RTEMP)
        IF(NPARSEC.LT.0)THEN
          WRITE(ERRMSG,143) ALINE(1:NALINE),CASFL(1:MCASE),
     +    TRIM(UPVARTEXT)
143       FORMAT(A,A,': ',A,' must be zero or greater.')
          CALL WRTERR
        END IF
        VARTEXT='nparfile'
        UPVARTEXT='NPARFILE'
        RTEMP=0.0D0             ! default value
        CALL READ_MRUN_VAR_PC(IFAIL,RTEMP,VARTEXT,UPVARTEXT,
     +  CLINE,1,DUMFILE,NALINE)
        NPARFILE=NINT(RTEMP)
        IF(NPARFILE.LT.0)THEN
          WRITE(ERRMSG,143) ALINE(1:NALINE),CASFL(1:MCASE),
     +    TRIM(UPVARTEXT)
          CALL WRTERR
        END IF
        VARTEXT='nequation'
        UPVARTEXT='NEQUATION'
        RTEMP=0.0D0             ! default value
        CALL READ_MRUN_VAR_PC(IFAIL,RTEMP,VARTEXT,UPVARTEXT,
     +  CLINE,1,DUMFILE,NALINE)
        NEQUATION=NINT(RTEMP)
        IF(NEQUATION.LT.0)THEN
          WRITE(ERRMSG,143) ALINE(1:NALINE),CASFL(1:MCASE),
     +    TRIM(UPVARTEXT)
          CALL WRTERR
        END IF
        IF(IERR.NE.0) GO TO 9800
        IF(((NPARSEC.GT.0).AND.(NEQUATION.EQ.0)).OR.
     +     ((NPARSEC.EQ.0).AND.(NEQUATION.GT.0)))THEN
            WRITE(ERRMSG,1431)  ALINE(1:NALINE),CASFL(1:MCASE)
1431        FORMAT(A,A,': if one of NEQUATION or NPARSEC is ',
     +      'greater than ',
     +      'zero then both must be greater than zero.')
            CALL WRTERR
        END IF
        IF(NPARSEC.GT.NEQUATION)THEN
          IF(NEQUATION.NE.0)THEN
            WRITE(ERRMSG,1432) ALINE(1:NALINE),CASFL(1:MCASE)
1432        FORMAT(A,A,': NPARSEC cannot exceed NEQUATION.')
            CALL WRTERR
          END IF
        END IF
        IF(NPARFILE.GT.0)THEN
          IF(FILEPARFILE.EQ.' ')THEN
            WRITE(ERRMSG,1433) ALINE(1:NALINE),CASFL(1:MCASE)
1433        FORMAT(A,A,': if NPARFILE is greater than zero then a ',
     +      'FILEPARFILE filename must be supplied.')
            CALL WRTERR
          END IF
        END IF
        IF(FILEPARFILE.NE.' ')THEN
          IF(NPARFILE.EQ.0)THEN
            WRITE(ERRMSG,1429) ALINE(1:NALINE),CASFL(1:MCASE)
1429        FORMAT(A,A,': if the name of a file-parameter file ',
     +      'is supplied ',
     +      'then NPARFILE must be greater than zero.')
            CALL WRTERR
          END IF
        END IF

        NOBSGP1=0
        CALL LINSPL(IFAIL,5,LW,RW,CLINE)
        IF(IFAIL.NE.0)THEN
          NOBSGP1=-999
          WRITE(ERRMSG,201)ALINE(1:NALINE),CASFL(1:MCASE)
201       FORMAT(A,A,': a value must be supplied for the NOBSGP ',
     +    '(number of observation groups) variable.')
          CALL WRTERR
          NOBSGP=1
          CALL LINSPL(IFAIL,4,LW,RW,CLINE)
          IF(IFAIL.NE.0) GO TO 9500
        END IF
        CALL NUMRD(IFAIL,0,LW(1),RW(1),NPAR,RTEMP,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(0,'NPAR')
        ELSE
          IF(NPAR.LE.0) THEN
            CALL ZRONEG(0,'NPAR')
          ELSE IF(NPAR.GT.999900)THEN
            WRITE(ERRMSG,235)ALINE(1:NALINE),CASFL(1:MCASE)
235         FORMAT(A,A,': number of parameters must not exceed 999900.')
            CALL WRTERR
          END IF
        END IF
        CALL NUMRD(IFAIL,0,LW(2),RW(2),NOBS,RTEMP,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(0,'NOBS')
        ELSE
          IF(NOBS.LT.0) THEN
            WRITE(ERRMSG,230) ALINE(1:NALINE),CASFL(1:MCASE)
230         FORMAT(A,A,': integer variable NOBS negative.')
            CALL WRTERR
          ELSE IF(NOBS.EQ.0) THEN
            WRITE(ERRMSG,240) ALINE(1:NALINE),CASFL(1:MCASE)
240         FORMAT(A,A,': integer variable NOBS given as zero.')
            CALL WRTERR
            WRITE(6,245,ERR=9900)
245         FORMAT('   (Give all observations zero weight to ',
     +      'optimize using prior info only.)')
          END IF
        END IF
        CALL NUMRD(IFAIL,0,LW(3),RW(3),NPARGP,RTEMP,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(0,'NPARGP')
        ELSE
          IF(NPARGP.LE.0) CALL ZRONEG(0,'NPARGP')
        END IF
        CALL NUMRD(IFAIL,0,LW(4),RW(4),NPRIOR,RTEMP,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(0,'NPRIOR')
        ELSE
          IF(NPRIOR.LT.0) THEN
            QUICKPRIOR=1
            NPRIOR=ABS(NPRIOR)
          ELSE
            QUICKPRIOR=0
          END IF
        ENDIF
        IF(NOBSGP1.NE.-999)THEN
          CALL NUMRD(IFAIL,0,LW(5),RW(5),NOBSGP,RTEMP,CLINE)
          IF(IFAIL.NE.0)THEN
            CALL RDERR(0,'NBOSGP')
          ELSE
            IF(NOBSGP.LT.1)THEN
              WRITE(ERRMSG,295) ALINE(1:NALINE),CASFL(1:MCASE)
295           FORMAT(A,A,': integer variable NOBSGP zero or negative.')
              CALL WRTERR
            ELSE IF(IPRED.NE.0)THEN
              IF(NOBSGP.EQ.1)THEN
                WRITE(ERRMSG,297) ALINE(1:NALINE),CASFL(1:MCASE)
297             FORMAT(A,A,': NOBSGP must be greater than 1 if PEST ',
     +          'is to be run in predictive analysis mode.')
                CALL WRTERR
              END IF
            ELSE IF(IREG.NE.0)THEN
              IF(NOBSGP.EQ.1)THEN
                WRITE(ERRMSG,293) ALINE(1:NALINE),CASFL(1:MCASE)
293             FORMAT(A,A,': NOBSGP must be greater than 1 if PEST ',
     +          'is run in regularisation mode.')
                CALL WRTERR
              END IF
            ELSE IF(PESTMODE.EQ.4)THEN
              IF(NOBSGP.LT.2)THEN
                WRITE(ERRMSG,2971) ALINE(1:NALINE),CASFL(1:MCASE)
2971            FORMAT(A,A,': NOBSGP must be greater than 1 if PEST ',
     +         'is run in pareto mode.')
                CALL WRTERR
              END IF
            END IF
          END IF
        ELSE
          IF((IPROLD.EQ.0).AND.(IPRED.NE.0))THEN
            WRITE(ERRMSG,296) ALINE(1:NALINE),CASFL(1:MCASE)
296         FORMAT(A,A,': a value for NOBSGP must be supplied if ',
     +      'PEST is run in predictive analysis mode.')
            CALL WRTERR
          END IF
          IF((IPROLD.EQ.0).AND.(IREG.NE.0))THEN
            WRITE(ERRMSG,298)  ALINE(1:NALINE),CASFL(1:MCASE)
298         FORMAT(A,A,': a value for NOBSGP must be supplied if ',
     +      'PEST is run in regularisation mode.')
            CALL WRTERR
          END IF
          IF((IPROLD.EQ.0).AND.(PESTMODE.EQ.4))THEN
            WRITE(ERRMSG,2981) ALINE(1:NALINE),CASFL(1:MCASE)
2981         FORMAT(A,A,': a value for NOBSGP must be supplied if ',
     +      'PEST is run in pareto mode.')
            CALL WRTERR
          END IF
        END IF

C -- WE NOW TEST FOR THE PRESENCE OF MAXCOMPDIM AND DERZEROLIM.

        IF(NOBSGP1.NE.-999)THEN
          CALL LINSPL(IFAIL,6,LW,RW,CLINE)
          IF(IFAIL.NE.0)THEN
            MAXCOMPDIM=0
          ELSE
            CALL NUMRD(IFAIL,0,LW(6),RW(6),MAXCOMPDIM,RTEMP,CLINE)
            IF(IFAIL.NE.0) THEN
              CALL RDERR(0,'MAXCOMPDIM')
            ELSE
              IF(MAXCOMPDIM.LT.0)THEN
                WRITE(ERRMSG,299)ALINE(1:NALINE),CASFL(1:MCASE)
299             FORMAT(A,A,': MAXCOMPDIM must not be negative.')
                CALL WRTERR
              END IF
            END IF
            CALL LINSPL(IFAIL,7,LW,RW,CLINE)
            IF(IFAIL.NE.0)THEN
              DERZEROLIM=-1.1D35
            ELSE
              CALL NUMRD(IFAIL,1,LW(7),RW(7),ITEMP,DERZEROLIM,CLINE)
              IF(IFAIL.NE.0)THEN
                CALL RDERR(1,'DERZEROLIM')
              ELSE
                IF(DERZEROLIM.LT.0.0D0)THEN
                  WRITE(ERRMSG,301) ALINE(1:NALINE),CASFL(1:MCASE)
301               FORMAT(A,A,' DERZEROLIM must not be negative.')
                  CALL WRTERR
                END IF
              END IF
            END IF
          END IF
        END IF

C -- STORAGE IS ALLOCATED FOR ARRAYS

        NPARTOT=NPAR+NPARSEC+NPARFILE
        GPDIM=MAX(1,NPARGP)
        PARDIM=MAX(1,NPARTOT)
        OBSDIM=MAX(1,NOBS)
        PRRDIM=MAX(1,NPRIOR)
        OBGDIM=MAX(1,NOBSGP)
        OBNDIM=MAX(1,NPRIOR+NOBS)
        NXROW=NPRIOR+NOBS

        ALLOCATE(IDERIV(GPDIM),DERVAL(GPDIM),
     +  DERBND(GPDIM),J2OR3(GPDIM),DERMUL(GPDIM),I3CALC(GPDIM),
     +  IDBND(GPDIM),AGROUP(GPDIM),STAT=IALERR)
        IF(IALERR.NE.0) GO TO 9400
        ALLOCATE(SPLITTHRESH(GPDIM),SPLITRELDIFF(GPDIM),
     +  SPLITACTION(GPDIM),STAT=IALERR)
        IF(IALERR.NE.0) GO TO 9400
        SPLITTHRESH=-1.0D301                    ! An array
        ALLOCATE(APAR(PARDIM),ITRANS(PARDIM),
     +  CHGTYP(PARDIM),PVAL(PARDIM),LBOUND(PARDIM),UBOUND(PARDIM),
     +  IPARGP(PARDIM),DERCOM(PARDIM),STAT=IALERR)
        IF(IALERR.NE.0) GO TO 9400
        ALLOCATE(OWGHT(OBNDIM),STAT=IALERR)
        IF(IALERR.NE.0) GO TO 9400

        ALLOCATE(AOBS(OBSDIM),IWORK(MAX(OBSDIM,PARDIM)),
     +  IOBGNM(OBNDIM),IRGP(OBGDIM),TEMPOBS(OBSDIM),STAT=IALERR)
        IF(IALERR.NE.0) GO TO 9400

        IF(NPRIOR.GT.0) ALLOCATE(APRIOR(PRRDIM),STAT=IALERR)
        IF(IALERR.NE.0) GO TO 9400
        ALLOCATE(AOBSGP(OBGDIM),COVFLE(OBGDIM),GTARG(OBGDIM),
     +  NNUMOBS(OBGDIM),STAT=IALERR)
        IF(IALERR.NE.0) GO TO 9400
        GTARG=-1.1D35
        NNUMOBS=0

C -- MORE OF THE CONTROL DATA SECTION IS READ AND CHECKED

7603    CONTINUE
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        NALINE=LEN_TRIM(ALINE)+1
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7603
        CALL LOWCAS(CLINE)
        IREFOBS=999
        IREFOBSCOUNT=0
        IREFOBSCONT=0
        IREFOBS_PAUSE=999
320     CONTINUE
        II=INDEX(CLINE,'noobsreref')
        IF(II.NE.0)THEN
          IREFOBSCOUNT=IREFOBSCOUNT+1
          IF(IREFOBSCOUNT.EQ.2)THEN
            WRITE(ERRMSG,323) ALINE(1:NALINE),CASFL(1:MCASE)
323         FORMAT(A,A,': "obsereref" and/or "noobsreref" have been ',
     +      'cited more than once on this line.')
            CALL WRTERR
          END IF
          IREFOBS=0
          DO JJ=II,LEN_TRIM(CLINE)
            IF(CLINE(JJ:JJ).EQ.' ') EXIT
            CLINE(JJ:JJ)=' '
          END DO
          GO TO 320
        END IF
321     CONTINUE
        II=INDEX(CLINE,'obsreref')
        IF(II.NE.0)THEN
          IREFOBSCOUNT=IREFOBSCOUNT+1
          IF(IREFOBSCOUNT.EQ.2)THEN
            WRITE(ERRMSG,323) ALINE(1:NALINE),CASFL(1:MCASE)
            CALL WRTERR
          END IF
          IF(IREFOBS.EQ.0)THEN
            IREFOBSCONT=IREFOBSCONT+1
            IF(IREFOBSCONT.EQ.1)THEN
              WRITE(ERRMSG,322) ALINE(1:NALINE),CASFL(1:MCASE)
322           FORMAT(A,A,': contradictory values have been supplied ',
     +        'for the "obsreref" variable.')
              CALL WRTERR
            END IF
          END IF
          IREFOBS=1
          IF(CLINE(II+8:II+8).EQ.' ')THEN
            CLINE(II:II+7)=' '
            GO TO 321
          END IF
          IF(CLINE(II+8:II+8).NE.'_')THEN
            WRITE(ERRMSG,319) ALINE(1:NALINE),CASFL(1:MCASE)
319         FORMAT(A,A,': only an underscore followed by an ',
     +      'integer can follow "obsreref" string.')
            CALL WRTERR
          ELSE
            ATEMP=CLINE(II+9:)
            DO JJ=1,LEN_TRIM(ATEMP)
              IF(ATEMP(JJ:JJ).EQ.' ') THEN
                ATEMP(JJ:)=' '
                GO TO 324
              END IF
            END DO
324         CONTINUE
            CALL INTREAD(IFAIL,ATEMP,IREFOBS_PAUSE)
            IF(IFAIL.NE.0)THEN
              WRITE(ERRMSG,319) ALINE(1:NALINE),CASFL(1:MCASE)
              CALL WRTERR
            ELSE
              IF(IREFOBS_PAUSE.LE.0)THEN
                WRITE(ERRMSG,325) ALINE(1:NALINE),CASFL(1:MCASE)
325             FORMAT(A,A,': if an integer denoting seconds ',
     +          'follows the "obsreref" string it must be greater ',
     +          'than zero.')
                CALL WRTERR
              END IF
            END IF
          END IF
          DO JJ=II,LEN_TRIM(CLINE)
            IF(CLINE(JJ:JJ).EQ.' ') EXIT
            CLINE(JJ:JJ)=' '
          END DO
          GO TO 321
        END IF
        IF(IREFOBS.EQ.999) IREFOBS=0
        ORR_NOT_FIRST=0
1621    CONTINUE
        II=INDEX(CLINE,'orr_not_first')
        IF(II.NE.0)THEN
          ORR_NOT_FIRST=1
          DO JJ=II,LEN_TRIM(CLINE)
            IF(CLINE(JJ:JJ).EQ.' ') EXIT
            CLINE(JJ:JJ)=' '
          END DO
          GO TO 1621
        END IF
        IF(ORR_NOT_FIRST.NE.0)THEN
          IF(IREFOBS.EQ.0)THEN
            WRITE(ERRMSG,1622) ALINE(1:NALINE),CASFL(1:MCASE)
1622        FORMAT(A,A,': "orr_not_first" cannot be supplied on this ',
     +      'line if observation re-referencing is ',
     +      'not switched on.')
            CALL WRTERR
          END IF
        END IF
        CALL LINSPL(IFAIL,4,LW,RW,CLINE)
        IF(IFAIL.NE.0) GO TO 9500
        CALL NUMRD(IFAIL,0,LW(1),RW(1),NINFLE,RTEMP,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(0,'NTPLFLE')
        ELSE
          IF(NINFLE.LE.0) THEN
            CALL ZRONEG(0,'NTPLFLE')
          END IF
        END IF
        CALL NUMRD(IFAIL,0,LW(2),RW(2),NOUTFL,RTEMP,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(0,'NINSFLE')
        ELSE
          IF(NOUTFL.LE.0) THEN
            CALL ZRONEG(0,'NINSFLE')
          ELSE IF(NOUTFL.GT.NOBS)THEN
            WRITE(ERRMSG,340) ALINE(1:NALINE),CASFL(1:MCASE)
340         FORMAT(A,A,': number of instruction files exceeds ',
     +      'number of observations.')
            CALL WRTERR
          END IF
        END IF
        ATEMP=CLINE(LW(3):RW(3))
        CALL LOWCAS(ATEMP)
        PRECIS=ATEMP(1:8)
        IF((ATEMP.NE.'single').AND.(ATEMP.NE.'double'))THEN
          WRITE(ERRMSG,345) AALINE(1:MMLINE)
345       FORMAT(A,'character variable PRECIS must be ',
     +     '"single" or "double".')
           CALL WRTERR
        END IF
        ATEMP=CLINE(LW(4):RW(4))
        CALL LOWCAS(ATEMP)
        IF((ATEMP.NE.'point').AND.(ATEMP.NE.'nopoint'))THEN
          WRITE(ERRMSG,350) AALINE(1:MMLINE)
350       FORMAT(A,'character variable DPOINT must be ',
     +     '"point" or "nopoint".')
           CALL WRTERR
        END IF

        ITEMP=MAX(NINFLE,NOUTFL)
        FLEDIM=MAX(1,ITEMP)
        INDIM=MAX(1,NINFLE)
        OUTDIM=MAX(1,NOUTFL)

        NUMCOM=1
        JACFILE=0
        IMSFLE=0
        CALL LINSPL(IFAIL,7,LW,RW,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL LINSPL(IFAIL,6,LW,RW,CLINE)
          IF(IFAIL.EQ.0)THEN
            WRITE(ERRMSG,4300) AALINE(1:MMLINE)
4300        FORMAT(A,'either 4 variables (old style) or 7 variables ',
     +      'are expected on this line, with the possible addition ',
     +      'of a value for OBSREREF.')
            CALL WRTERR
          ELSE
            CALL LINSPL(IFAIL,5,LW,RW,CLINE)
            IF(IFAIL.EQ.0)THEN
              WRITE(ERRMSG,4300) AALINE(1:MMLINE)
              CALL WRTERR
              CALL NUMRD(IFAIL,0,LW(5),RW(5),ITEMP,RTEMP,CLINE)
              IF(IFAIL.NE.0)THEN
                CALL NUMRD(IFAIL,1,LW(5),RW(5),ITEMP,RTEMP,CLINE)
                IF(IFAIL.NE.0)THEN
                  WRITE(ERRMSG,4301) AALINE(1:MMLINE),
     +            CLINE(LW(5):RW(5))
4301              FORMAT(A,'unrecognized variable "',a,'".')
                  CALL WRTERR
                END IF
              END IF
            ELSE
              JCWARN=1
            END IF
          END IF
          GO TO 4400
        END IF
        CALL NUMRD(IFAIL,0,LW(5),RW(5),NUMCOM,RTEMP,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(0,'NUMCOM')
        ELSE
          IF(NUMCOM.LE.0) THEN
            CALL ZRONEG(0,'NUMCOM')
          END IF
        END IF
        CALL NUMRD(IFAIL,0,LW(6),RW(6),JACFILE,RTEMP,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(0,'JACFILE')
        ELSE
          IF((JACFILE.NE.0).AND.(ABS(JACFILE).NE.1).AND.
     +       (JACFILE.NE.2))THEN
            WRITE(ERRMSG,4309) AALINE(1:MMLINE)
4309        FORMAT(A,'variable JACFILE must be 0, 1, -1 or 2.')
            CALL WRTERR
          END IF
        END IF
        CALL NUMRD(IFAIL,0,LW(7),RW(7),IMSFLE,RTEMP,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(0,'MESSFILE')
        ELSE
          IF((IMSFLE.NE.0).AND.(IMSFLE.NE.1)) THEN
            WRITE(ERRMSG,4311) AALINE(1:MMLINE)
4311        FORMAT(A,'variable MESSFILE must be 0 or 1.')
            CALL WRTERR
          END IF
        END IF
        CALL LINSPL(IFAIL,8,LW,RW,CLINE)
        IF(IFAIL.EQ.0)THEN
          WRITE(ERRMSG,4301) AALINE(1:MMLINE),
     +    CLINE(LW(8):RW(8))
          CALL WRTERR
        END IF
        IF(NUMCOM.GT.1)THEN
          IF(ORR_NOT_FIRST.GT.0)THEN
            WRITE(ERRMSG,1623) AALINE(1:MMLINE)
1623        FORMAT(A,'"orr_not_first cannot be implemented where ',
     +      'more than one model command is used for derivatives ',
     +      'calculation.')
            CALL WRTERR
          END IF
        END IF

4400    CONTINUE

        ALLOCATE(TMPFLE(INDIM),NUMPAR(INDIM),
     +  NUMOBS(OUTDIM),MODFLE(FLEDIM),INSFLE(OUTDIM),
     +  COMLIN(NUMCOM),STAT=IALERR)
        IF(IALERR.NE.0) GO TO 9400

7604    CONTINUE
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        NALINE=LEN_TRIM(ALINE)+1
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7604
        CALL LINSPL(IFAIL,5,LW,RW,CLINE)
        IF(IFAIL.NE.0) GO TO 9500
        CALL NUMRD(IFAIL,1,LW(1),RW(1),ITEMP,LMBDA1,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(1,'RLAMBDA1')
        ELSE
          IF(LMBDA1.LT.0.0D0) THEN
            WRITE(ERRMSG,394) ALINE(1:NALINE),CASFL(1:MCASE)
394         FORMAT(A,A,': RLAMBDA1 must not be negative.')
            CALL WRTERR
          END IF
        ENDIF
        CALL NUMRD(IFAIL,1,LW(2),RW(2),ITEMP,LAMFAC,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(1,'RLAMFAC')
        ELSE
          IF(ABS(LAMFAC).LE.1.0D0) THEN
            WRITE(ERRMSG,395) ALINE(1:NALINE),CASFL(1:MCASE)
395         FORMAT(A,A,': real variable RLAMFAC must have an ',
     +      'absolute value greater than one.')
            CALL WRTERR
          END IF
        END IF
        CALL NUMRD(IFAIL,1,LW(3),RW(3),ITEMP,LAMPSI,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(1,'PHIRATSUF')
        ELSE
          IF((LAMPSI.LE.0.0D0).OR.(LAMPSI.GE.1.0D0))THEN
            WRITE(ERRMSG,410) ALINE(1:NALINE),CASFL(1:MCASE)
410         FORMAT(A,A,': real variable PHIRATSUF must be between ',
     +      'zero and one.')
            CALL WRTERR
          END IF
        END IF
        CALL NUMRD(IFAIL,1,LW(4),RW(4),ITEMP,FRCLAM,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(1,'PHIREDLAM')
        ELSE
          IF((FRCLAM.LE.0.0D0).OR.(FRCLAM.GE.1.0D0))THEN
            WRITE(ERRMSG,440) ALINE(1:NALINE),CASFL(1:MCASE)
440         FORMAT(A,A,': real variable PHIREDLAM must be between ',
     +      'zero and one.')
            CALL WRTERR
          END IF
        END IF
        CALL NUMRD(IFAIL,0,LW(5),RW(5),NUMRUN,RTEMP,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(0,'NUMLAM')
        END IF
        IF(NUMRUN.LT.0)THEN
          IF(NUMRUN.EQ.-1)THEN
            WRITE(ERRMSG,4401) ALINE(1:NALINE),CASFL(1:MCASE)
4401        FORMAT(A,A,': if NUMLAM is negative, it must be set ',
     +      'to a number less than -1.')
            CALL WRTERR
          END IF
        END IF
        IF((LMBDA1.EQ.0.0D0).AND.(NUMRUN.NE.1))THEN
          WRITE(ERRMSG,441) ALINE(1:NALINE),CASFL(1:MCASE)
441       FORMAT(A,A,': if RLAMDBA1 is zero, NUMLAM must be one.')
          CALL WRTERR
        END IF
        CALL LOWCAS(CLINE)
        NN = INDEX(CLINE,'run_slow_fac')
        IF(NN.NE.0)THEN
          RUN_SLOW_FAC=-1.1D100
          ATEMP25=ADJUSTL(CLINE(NN+12:))
          IF(ATEMP25(1:1).NE.'=')THEN
            WRITE(ERRMSG,2105) ALINE(1:NALINE),CASFL(1:MCASE)
2105        FORMAT(A,A,': cannot read value of RUN_SLOW_FAC variable.')
            CALL WRTERR
            GO TO 2109
          END IF
          ATEMP25=ADJUSTL(ATEMP25(2:))
          NN=INDEX(ATEMP25,' ')
          IF((NN.EQ.0).OR.(NN.EQ.1))THEN
            WRITE(ERRMSG,2105) ALINE(1:NALINE),CASFL(1:MCASE)
            CALL WRTERR
            GO TO 2109
          END IF
          ATEMP25(NN:)=' '
          READ(ATEMP25,'(F25.0)',IOSTAT=IFAIL) RUN_SLOW_FAC
          IF(IFAIL.NE.0) THEN
            WRITE(ERRMSG,2105) ALINE(1:NALINE),CASFL(1:MCASE)
            CALL WRTERR
            RUN_SLOW_FAC=-1.1D100
            GO TO 2109
          END IF
          IF(RUN_SLOW_FAC.LT.1.199999)THEN
            WRITE(ERRMSG,2106) ALINE(1:NALINE),CASFL(1:MCASE)
2106        FORMAT(A,A,': RUN_SLOW_FAC must be greater than 1.2.')
            CALL WRTERR
          END IF
          NN=INDEX(CLINE,'run_slow_fac')
          CLINE(NN:)=CLINE(NN+12:)
          DO I=NN,LEN(CLINE)
            IF(CLINE(I:I).EQ.'=')THEN
              CLINE(NN:)=CLINE(I+1:)
              GO TO 21061
            END IF
          END DO
21061     CONTINUE
          CLINE(NN:)=ADJUSTL(CLINE(NN:))
          DO I=NN,LEN(CLINE)
            IF(CLINE(I:I).EQ.' ')THEN
              CLINE(NN:I-1)=' '
              GO TO 2108
            END IF
          END DO
2108      CONTINUE
          NN=INDEX(CLINE,'run_slow_fac')
          IF(NN.NE.0)THEN
            WRITE(ERRMSG,2107) ALINE(1:NALINE),CASFL(1:MCASE)
2107        FORMAT(A,A,': RUN_SLOW_FAC variable appears twice.')
            CALL WRTERR
            RUN_SLOW_FAC=-1.1D100
          END IF
2109      CONTINUE
        END IF
        NN = INDEX(CLINE,'run_abandon_fac')
        IF(NN.NE.0)THEN
          RUN_ABANDON_FAC=-1.1D100
          ATEMP25=ADJUSTL(CLINE(NN+15:))
          IF(ATEMP25(1:1).NE.'=')THEN
            WRITE(ERRMSG,21091) ALINE(1:NALINE),CASFL(1:MCASE)
21091       FORMAT(A,A,': cannot read value of RUN_ABANDON_FAC ',
     +      'variable.')
            CALL WRTERR
            GO TO 2113
          END IF
          ATEMP25=ADJUSTL(ATEMP25(2:))
          NN=INDEX(ATEMP25,' ')
          IF((NN.EQ.0).OR.(NN.EQ.1))THEN
            WRITE(ERRMSG,21091) ALINE(1:NALINE),CASFL(1:MCASE)
            CALL WRTERR
            GO TO 2113
          END IF
          ATEMP25(NN:)=' '
          READ(ATEMP25,'(F25.0)',IOSTAT=IFAIL) RUN_ABANDON_FAC
          IF(IFAIL.NE.0) THEN
            WRITE(ERRMSG,21091) ALINE(1:NALINE),CASFL(1:MCASE)
            CALL WRTERR
            RUN_ABANDON_FAC=-1.1D100
            GO TO 2113
          END IF
          IF(RUN_ABANDON_FAC.NE.0.0D0)THEN
            IF(RUN_ABANDON_FAC.LT.1.19999D0)THEN
              WRITE(ERRMSG,21111) ALINE(1:NALINE),CASFL(1:MCASE)
21111         FORMAT(A,A,': RUN_ABANDON_FAC must be either zero or ',
     +        'greater than 1.2.')
              CALL WRTERR
              RUN_ABANDON_FAC=-1.1D100
              GO TO 2113
            END IF
          END IF
          NN=INDEX(CLINE,'run_abandon_fac')
          CLINE(NN:)=CLINE(NN+15:)
          DO I=NN,LEN(CLINE)
            IF(CLINE(I:I).EQ.'=')THEN
              CLINE(NN:)=CLINE(I+1:)
              GO TO 21062
            END IF
          END DO
21062     CONTINUE
          CLINE(NN:)=ADJUSTL(CLINE(NN:))
          DO I=NN,LEN(CLINE)
            IF(CLINE(I:I).EQ.' ')THEN
              CLINE(NN:I-1)=' '
              GO TO 21081
            END IF
          END DO
21081     CONTINUE
          NN=INDEX(CLINE,'run_abandon_fac')
          IF(NN.NE.0)THEN
            WRITE(ERRMSG,2112) ALINE(1:NALINE),CASFL(1:MCASE)
2112        FORMAT(A,A,': RUN_ABANDON_FAC variable appears twice on ',
     +      'this line.')
            CALL WRTERR
            RUN_ABANDON_FAC=-1.1D100
            GO TO 2113
          END IF
2113      CONTINUE
        END IF
        NN = INDEX(CLINE,'win_mrun_hours')
        IF(NN.NE.0)THEN
          WIN_MRUN_HOURS=-1.1D100
          ATEMP25=ADJUSTL(CLINE(NN+14:))
          IF(ATEMP25(1:1).NE.'=')THEN
            WRITE(ERRMSG,21191) ALINE(1:NALINE),CASFL(1:MCASE)
21191       FORMAT(A,A,': cannot read value of WIN_MRUN_HOURS ',
     +      'variable.')
            CALL WRTERR
            GO TO 21131
          END IF
          ATEMP25=ADJUSTL(ATEMP25(2:))
          NN=INDEX(ATEMP25,' ')
          IF((NN.EQ.0).OR.(NN.EQ.1))THEN
            WRITE(ERRMSG,21191) ALINE(1:NALINE),CASFL(1:MCASE)
            CALL WRTERR
            GO TO 21131
          END IF
          ATEMP25(NN:)=' '
          READ(ATEMP25,'(F25.0)',IOSTAT=IFAIL) WIN_MRUN_HOURS
          IF(IFAIL.NE.0) THEN
            WRITE(ERRMSG,21191) ALINE(1:NALINE),CASFL(1:MCASE)
            CALL WRTERR
            WIN_MRUN_HOURS=-1.1D100
            GO TO 21131
          END IF
          IF(WIN_MRUN_HOURS.LT.0.0D0)THEN
            WRITE(ERRMSG,21161) ALINE(1:NALINE),CASFL(1:MCASE)
21161       FORMAT(A,A,': WIN_MRUN_HOURS must be zero or greater.')
            CALL WRTERR
            WIN_MRUN_HOURS=-1.1D100
            GO TO 21131
          END IF
          NN=INDEX(CLINE,'win_mrun_hours')
          CLINE(NN:)=CLINE(NN+14:)
          DO I=NN,LEN(CLINE)
            IF(CLINE(I:I).EQ.'=')THEN
              CLINE(NN:)=CLINE(I+1:)
              GO TO 21162
            END IF
          END DO
21162     CONTINUE
          CLINE(NN:)=ADJUSTL(CLINE(NN:))
          DO I=NN,LEN(CLINE)
            IF(CLINE(I:I).EQ.' ')THEN
              CLINE(NN:I-1)=' '
              GO TO 21181
            END IF
          END DO
21181     CONTINUE
          NN=INDEX(CLINE,'win_mrun_hours')
          IF(NN.NE.0)THEN
            WRITE(ERRMSG,21112) ALINE(1:NALINE),CASFL(1:MCASE)
21112       FORMAT(A,A,': WIN_MRUN_HOURS variable appears twice on ',
     +      'this line.')
            CALL WRTERR
            WIN_MRUN_HOURS=-1.1D100
            GO TO 21131
          END IF
21131     CONTINUE
        END IF
        NN = INDEX(CLINE,'uptestlim')
        IF(NN.NE.0)THEN
          UPTESTLIM=-9999
          ATEMP25=ADJUSTL(CLINE(NN+9:))
          IF(ATEMP25(1:1).NE.'=')THEN
            WRITE(ERRMSG,2110) ALINE(1:NALINE),CASFL(1:MCASE)
2110        FORMAT(A,A,': cannot read value of UPTESTLIM variable.')
            CALL WRTERR
            GO TO 21109
          END IF
          ATEMP25=ADJUSTL(ATEMP25(2:))
          NN=INDEX(ATEMP25,' ')
          IF((NN.EQ.0).OR.(NN.EQ.1))THEN
            WRITE(ERRMSG,2110) ALINE(1:NALINE),CASFL(1:MCASE)
            CALL WRTERR
            GO TO 21109
          END IF
          ATEMP25(NN:)=' '
          READ(ATEMP25,'(I25)',IOSTAT=IFAIL) UPTESTLIM
          IF(IFAIL.NE.0) THEN
            WRITE(ERRMSG,2110) ALINE(1:NALINE),CASFL(1:MCASE)
            CALL WRTERR
            UPTESTLIM=-9999
            GO TO 21109
          END IF
          IF((UPTESTLIM.LT.3).OR.(UPTESTLIM.GT.150))THEN
            WRITE(ERRMSG,21106) ALINE(1:NALINE),CASFL(1:MCASE)
21106       FORMAT(A,A,': UPTESTLIM must be greater than 2 and ',
     +      'less than 150.')
            CALL WRTERR
          END IF
          NN=INDEX(CLINE,'uptestlim')
          CLINE(NN:)=CLINE(NN+9:)
          DO I=NN,LEN(CLINE)
            IF(CLINE(I:I).EQ.'=')THEN
              CLINE(NN:)=CLINE(I+1:)
              GO TO 21107
            END IF
          END DO
21107     CONTINUE
          CLINE(NN:)=ADJUSTL(CLINE(NN:))
          DO I=NN,LEN(CLINE)
            IF(CLINE(I:I).EQ.' ')THEN
              CLINE(NN:I-1)=' '
              GO TO 21108
            END IF
          END DO
21108     CONTINUE
          NN=INDEX(CLINE,'uptestlim')
          IF(NN.NE.0)THEN
            WRITE(ERRMSG,21105) ALINE(1:NALINE),CASFL(1:MCASE)
21105       FORMAT(A,A,': UPTESTLIM variable appears twice.')
            CALL WRTERR
            UPTESTLIM=-9999
          END IF
21109     CONTINUE
        END IF

        NN = INDEX(CLINE,'uptestmin')
        IF(NN.NE.0)THEN
          UPTESTMIN=-9999
          ATEMP25=ADJUSTL(CLINE(NN+9:))
          IF(ATEMP25(1:1).NE.'=')THEN
            WRITE(ERRMSG,21122) ALINE(1:NALINE),CASFL(1:MCASE)
21122        FORMAT(A,A,': cannot read value of UPTESTMIN variable.')
            CALL WRTERR
            GO TO 21119
          END IF
          ATEMP25=ADJUSTL(ATEMP25(2:))
          NN=INDEX(ATEMP25,' ')
          IF((NN.EQ.0).OR.(NN.EQ.1))THEN
            WRITE(ERRMSG,21122) ALINE(1:NALINE),CASFL(1:MCASE)
            CALL WRTERR
            GO TO 21119
          END IF
          ATEMP25(NN:)=' '
          READ(ATEMP25,'(I25)',IOSTAT=IFAIL) UPTESTMIN
          IF(IFAIL.NE.0) THEN
            WRITE(ERRMSG,21122) ALINE(1:NALINE),CASFL(1:MCASE)
            CALL WRTERR
            UPTESTMIN=-9999
            GO TO 21119
          END IF
          IF((UPTESTMIN.LT.3).OR.(UPTESTMIN.GT.70))THEN
            WRITE(ERRMSG,21116) ALINE(1:NALINE),CASFL(1:MCASE)
21116       FORMAT(A,A,': UPTESTMIN must be greater than 2 and ',
     +      'less than 70.')
            CALL WRTERR
          END IF
          NN=INDEX(CLINE,'uptestmin')
          CLINE(NN:)=CLINE(NN+9:)
          DO I=NN,LEN(CLINE)
            IF(CLINE(I:I).EQ.'=')THEN
              CLINE(NN:)=CLINE(I+1:)
              GO TO 21117
            END IF
          END DO
21117     CONTINUE
          CLINE(NN:)=ADJUSTL(CLINE(NN:))
          DO I=NN,LEN(CLINE)
            IF(CLINE(I:I).EQ.' ')THEN
              CLINE(NN:I-1)=' '
              GO TO 21118
            END IF
          END DO
21118     CONTINUE
          NN=INDEX(CLINE,'uptestmin')
          IF(NN.NE.0)THEN
            WRITE(ERRMSG,21115) ALINE(1:NALINE),CASFL(1:MCASE)
21115       FORMAT(A,A,': UPTESTMIN variable appears twice.')
            CALL WRTERR
            UPTESTMIN=-9999
          END IF
21119     CONTINUE
        END IF

        NN=INDEX(CLINE,' nolamforgive')
        IF(NN.NE.0)THEN
          LAMFORGIVE=0
          CLINE(NN+1:NN+12)=' '
        END IF
        NN=INDEX(CLINE,' lamforgive')
        IF(NN.NE.0)THEN
          LAMFORGIVE=1
          CLINE(NN+1:NN+10)=' '
        END IF
        IF((IPRED.NE.0).AND.(LAMFORGIVE.EQ.1))THEN
          WRITE(ERRMSG,446) ALINE(1:NALINE),CASFL(1:MCASE)
446       FORMAT(A,A,': LAMFORGIVE must be set to "nolamforgive" ',
     +    'if PEST is run in predictive analysis mode.')
          CALL WRTERR
        END IF
        CALL LOWCAS(CLINE)
        NN=INDEX(CLINE,' noderforgive')
        IF(NN.NE.0)THEN
          DERFORGIVE=0
          CLINE(NN+1:NN+12)=' '
        END IF
        NN=INDEX(CLINE,' derforgive')
        IF(NN.NE.0)THEN
          DERFORGIVE=1
          CLINE(NN+1:NN+10)=' '
        END IF
        IF((RUN_SLOW_FAC.GT.-1.0D300).AND.
     +     (RUN_SLOW_FAC.LT.-1.0D100)) GO TO 2111
        IF((RUN_ABANDON_FAC.GT.-1.0D300).AND.
     +     (RUN_ABANDON_FAC.LT.-1.0D100)) GO TO 2111
        IF((WIN_MRUN_HOURS.GT.-1.0D300).AND.
     +     (WIN_MRUN_HOURS.LT.-1.0D100)) GO TO 2111
        IF(UPTESTLIM.EQ.-9999) GO TO 2111
        IF(UPTESTMIN.EQ.-9999) GO TO 2111
        CALL LINSPL(IFAIL,6,LW,RW,CLINE)
        IF(IFAIL.NE.0)THEN
          JACUPDATE=0
        ELSE
          CALL NUMRD(IFAIL,0,LW(6),RW(6),JACUPDATE,RTEMP,CLINE)
          IF(IFAIL.NE.0)THEN
            CALL RDERR(0,'JACUPDATE')
          ELSE
            IF(JACUPDATE.LT.0) THEN
              WRITE(ERRMSG,442) ALINE(1:NALINE),CASFL(1:MCASE)
442           FORMAT(A,A,': JACUPDATE must not be negative.')
              CALL WRTERR
            END IF
          END IF
          CALL LINSPL(IFAIL,7,LW,RW,CLINE)
          IF(IFAIL.EQ.0)THEN
            ATEMP=CLINE(LW(7):RW(7))
            WRITE(ERRMSG,444) ALINE(1:NALINE),CASFL(1:MCASE),TRIM(ATEMP)
444         FORMAT(A,A,': illegal value "',a,'" for LAMFORGIVE or ',
     +      'DERFORGIVE. These must be either "lamforgive", ',
     +      '"nolamforgive", "derforgive" or "noderforgive".')
            CALL WRTERR
          END IF
        END IF
2111    CONTINUE
        IF((MAXCOMPDIM.GT.1).AND.(JACUPDATE.GT.0))THEN
          WRITE(ERRMSG,443) ALINE(1:NALINE),CASFL(1:MCASE)
443       FORMAT(A,A,': JACUPDATE must be set to zero if ',
     +    'MAXCOMPDIM is greater than zero, thereby activating ',
     +    'compressed Jacobian storage.')
          CALL WRTERR
        END IF
        IF((UPTESTLIM.NE.-1).AND.(UPTESTMIN.NE.-1))THEN
          IF(UPTESTMIN.GT.UPTESTLIM)THEN
            WRITE(ERRMSG,44311) ALINE(1:NALINE),CASFL(1:MCASE)
44311       FORMAT(A,A,': UPTESTMIN must not exceed UPTESTLIM.')
            CALL WRTERR
          END IF
        END IF

7605    CONTINUE
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        NALINE=LEN_TRIM(ALINE)+1
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7605
        CALL LOWCAS(CLINE)
        CALL GET_ABSPARMAX(IFAIL,MAXABSPARMAX,ABSPARMAX,CLINE)
        IF(IFAIL.NE.0) GO TO 529
        CALL LINSPL(IFAIL,3,LW,RW,CLINE)
        IF(IFAIL.NE.0) THEN
          DO I=1,MAXABSPARMAX
            IF(ABSPARMAX(I).GT.-1.0E35) THEN
              WRITE(ERRMSG,449) ALINE(1:NALINE),CASFL(1:MCASE)
449           FORMAT(A,A,': insufficient non-ABSPARMAX entries.')
              ISTOP=1
              GO TO 9998
            END IF
          END DO
          GO TO 9500
        END IF
        CALL NUMRD(IFAIL,1,LW(1),RW(1),ITEMP,DMAX0,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(1,'RELPARMAX')
        ELSE
          IF(DMAX0.LE.0.0D0)THEN
            CALL ZRONEG(1,'RELPARMAX')
          END IF
        END IF
        CALL NUMRD(IFAIL,1,LW(2),RW(2),ITEMP,DMAX1,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(1,'FACPARMAX')
        ELSE
          IF(DMAX1.LE.1.0D0) THEN
            WRITE(ERRMSG,500) ALINE(1:NALINE),CASFL(1:MCASE)
500         FORMAT(A,A,': real variable FACPARMAX must be greater ',
     +      'than one.')
            CALL WRTERR
          END IF
        END IF
        CALL NUMRD(IFAIL,1,LW(3),RW(3),ITEMP,FACORG,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(1,'FACORIG')
        ELSE
          IF((FACORG.LE.0.0D0).OR.(FACORG.GT.1.0D0))THEN
            WRITE(ERRMSG,520) ALINE(1:NALINE),CASFL(1:MCASE)
520         FORMAT(A,A,': real variable FACORIG must not be less than',
     +      ' or equal to zero or greater than one.')
            CALL WRTERR
          ENDIF
        ENDIF
        CALL LINSPL(IFAIL,4,LW,RW,CLINE)
        IF(IFAIL.EQ.0)THEN
          CALL NUMRD(IFAIL,0,LW(4),RW(4),IBNDSTK,RTEMP,CLINE)
          IF(IFAIL.NE.0) THEN
            CALL RDERR(0,'IBOUNDSTICK')
          ELSE
            IF(IBNDSTK.LT.0)THEN
              WRITE(ERRMSG,522) ALINE(1:NALINE),CASFL(1:MCASE)
522           FORMAT(A,A,': integer variable IBOUNDSTICK must not ',
     +        'be negative.')
              CALL WRTERR
            END IF
            IF((JACFILE.NE.0).AND.(IBNDSTK.GT.0))THEN
              WRITE(ERRMSG,523) ALINE(1:NALINE),CASFL(1:MCASE)
523           FORMAT(A,A,': IBOUNDSTICK must be set to zero if ',
     +        'derivatives are supplied externally by the model.')
              CALL WRTERR
            END IF
          END IF
          CALL LINSPL(IFAIL,5,LW,RW,CLINE)
          IF(IFAIL.EQ.0)THEN
            CALL NUMRD(IFAIL,0,LW(5),RW(5),IVECBND,RTEMP,CLINE)
            IF(IFAIL.NE.0) THEN
              CALL RDERR(0,'UPVECBEND')
            ELSE
              IF((IVECBND.NE.0).AND.(IVECBND.NE.1))THEN
                WRITE(ERRMSG,521) ALINE(1:NALINE),CASFL(1:MCASE)
521             FORMAT(A,A,': integer variable UPVECBEND must be ',
     +          'set to either one or zero.')
                CALL WRTERR
              END IF
            END IF
          END IF
        END IF

529     CONTINUE
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        NALINE=LEN_TRIM(ALINE)+1
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 529
        CALL LOWCAS(CLINE)
        VARTEXT='jcowarnthresh'
        UPVARTEXT='JCOWARNTHRESH'
        JCOWARNTHRESH=0.0
        CALL READ_MRUN_VAR_PC(IFAIL,JCOWARNTHRESH,VARTEXT,UPVARTEXT,
     +  CLINE,1,DUMFILE,NALINE)
        IF(IFAIL.NE.0) GO TO 9998
        VARTEXT='jcozerothresh'
        UPVARTEXT='JCOZEROTHRESH'
        JCOZEROTHRESH=0.0
        CALL READ_MRUN_VAR_PC(IFAIL,JCOZEROTHRESH,VARTEXT,UPVARTEXT,
     +  CLINE,1,DUMFILE,NALINE)
        IF(IFAIL.NE.0) GO TO 9998
        CLINE=ADJUSTL(CLINE)
        CALL LINSPL(IFAIL,1,LW,RW,CLINE)
        IF(IFAIL.NE.0) GO TO 9500
        IF((ACODE.EQ.'m').OR.(ACODEP.EQ.'m'))THEN
        CALL NUMRD(IFAIL,1,LW(1),RW(1),ITEMP,PSI3,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(1,'PHIREDSWH')
        ELSE
          IF((PSI3.LE.0.0D0).OR.(PSI3.GE.1.0D0))THEN
            WRITE(ERRMSG,540) ALINE(1:NALINE),CASFL(1:MCASE)
540         FORMAT(A,A,': real variable PHIREDSWH must be greater ',
     +      'than zero and less than one.')
            CALL WRTERR
          END IF
        END IF
        END IF
        DOAUI='AAAA'
        DOSENREUSE='AAAA'

        IF((CLINE(1:12).EQ.'noboundscale').OR.
     +     (CLINE(1:10).EQ.'boundscale'))then
           WRITE(ERRMSG,5405) ALINE(1:NALINE),CASFL(1:MCASE)
5405       FORMAT(A,A,': the BOUNDSCALE variable must not be the ',
     +     'first variable on this line.')
           CALL WRTERR
        END IF
        IBOUNDSCALE=0
        NN=INDEX(CLINE,' noboundscale')
        IF(NN.NE.0)THEN
          IBOUNDSCALE=0
          CLINE(NN+1:NN+12)=' '
          IF(INDEX(CLINE,'boundscale').NE.0) THEN
            WRITE(ERRMSG,5401) ALINE(1:NALINE),CASFL(1:MCASE)
5401        FORMAT(A,A,': repeated specification of the BOUNDSCALE ',
     +      'variable.')
            CALL WRTERR
          END IF
        END IF
        NN=INDEX(CLINE,' boundscale')
        IF(NN.NE.0)THEN
          IBOUNDSCALE=1
          CLINE(NN+1:NN+10)=' '
          IF(INDEX(CLINE,'boundscale').NE.0) THEN
            WRITE(ERRMSG,5401) ALINE(1:NALINE),CASFL(1:MCASE)
            CALL WRTERR
          END IF
        END IF
5402    CONTINUE
        NN=INDEX(CLINE,' noboundscale')
        IF(NN.NE.0)THEN
          CLINE(NN+1:NN+12)=' '
          GO TO 5402
        END IF
5403    CONTINUE
        NN=INDEX(CLINE,' boundscale')
        IF(NN.NE.0)THEN
          CLINE(NN+1:NN+10)=' '
          GO TO 5403
        END IF
        IF(IBOUNDSCALE.NE.0)THEN
          IF(MAXCOMPDIM.GT.1)THEN
            WRITE(ERRMSG,5404)
5404        FORMAT(' As presently programmed parameter bounds scaling ',
     +      'cannot be employed if MAXCOMPDIM is set greater than ',
     +      '1.')
            CALL WRTERR
          END IF
        END IF

        NNN=INDEX(CLINE,'zerosenval')
        IF(NNN.EQ.0)THEN
          IZEROSENVAL=0
        ELSE
          IF((NNN.EQ.1).OR.(CLINE(1:NNN-1).EQ.' '))THEN
            WRITE(ERRMSG,5412) ALINE(1:NALINE),CASFL(1:MCASE)
5412        FORMAT(A,A,': the ZEROSENVAL variable must not be ',
     +      'the first item on this line.')
            CALL WRTERR
          END IF
          ATEMP50=CLINE(NNN:)
          ATEMP50=ADJUSTL(ATEMP50(11:))
          IF(ATEMP50(1:1).NE.'=') THEN
            WRITE(ERRMSG,5411) ALINE(1:NALINE),CASFL(1:MCASE)
5411        FORMAT(A,A,': cannot read the value of the ZEROSENVAL ',
     +      'variable.')
            CALL WRTERR
            GO TO 9800
          END IF
          ATEMP50=ADJUSTL(ATEMP50(2:))
          NN=INDEX(ATEMP50,' ')
          IF((NN.EQ.1).OR.(NN.EQ.0))THEN
            WRITE(ERRMSG,5411) ALINE(1:NALINE),CASFL(1:MCASE)
            CALL WRTERR
            GO TO 9800
          END IF
          NN1=INDEX(ATEMP50,CHAR(9))
          IF(NN1.NE.0)THEN
            IF(NN1.LT.NN)NN=NN1
          END IF
          ATEMP50(NN:)=' '
          READ(ATEMP50,'(F50.0)',IOSTAT=IFAIL) ZEROSENVAL
          IF(IFAIL.NE.0)THEN
            WRITE(ERRMSG,5411) ALINE(1:NALINE),CASFL(1:MCASE)
            CALL WRTERR
          END IF
          IF(ABS(ZEROSENVAL).GE.1.0D30)THEN
            WRITE(ERRMSG,5413) ALINE(1:NALINE),CASFL(1:MCASE)
5413        FORMAT(A,A,': ZEROSENVAL must have an absolute value of ',
     +      'less than 1.0E30.')
            CALL WRTERR
          END IF
          DO III=NNN,LEN_TRIM(CLINE)
            IF(CLINE(III:III).EQ.'=')EXIT
          END DO
          DO
            III=III+1
            IF((CLINE(III:III).NE.' ').AND.
     +         (CLINE(III:III).NE.CHAR(9))) EXIT
          END DO
          DO
            III=III+1
            IF((CLINE(III:III).EQ.' ').OR.
     +         (CLINE(III:III).EQ.CHAR(9))) EXIT
          END DO
          CLINE(NNN:)=CLINE(III:)
          IZEROSENVAL=1
          IF(NUMCOM.EQ.1)THEN
            IF(IREFOBS.EQ.0)THEN
            WRITE(ERRMSG,5414) ALINE(1:NALINE),CASFL(1:MCASE)
5414        FORMAT(A,A,': a value for ZEROSENVAL must not be supplied ',
     +      'unless PEST uses multiple model commands for ',
     +      'derivatives calculation or employs observation ',
     +      're-referencing.')
            CALL WRTERR
            END IF
          END IF
          NNN=INDEX(CLINE,'zerosenval')
          IF(NNN.NE.0)THEN
            WRITE(ERRMSG,5415) ALINE(1:NALINE),CASFL(1:MCASE)
5415        FORMAT(A,A,': ZEROSENVAL string is repeated.')
            CALL WRTERR
            GO TO 9800
          END IF
        END IF

        IDELETE=0
        CALL LINSPL(IFAIL,3,LW,RW,CLINE)
        IF(IFAIL.EQ.0)THEN
          CALL NUMRD(IFAIL,0,LW(2),RW(2),ITEMP,RTEMP,CLINE)
          IF(IFAIL.EQ.0)THEN
            CALL NUMRD(IFAIL,1,LW(3),RW(3),ITEMP,RTEMP,CLINE)
            IF(IFAIL.EQ.0)THEN
              SPLITSWH=RTEMP
              CLINE(LW(3):RW(3))=' '
              IDELETE=1
            END IF
          END IF
        END IF

        CALL LINSPL(IFAIL,2,LW,RW,CLINE)
        IF(IFAIL.EQ.0)THEN
          CALL NUMRD(IFAIL,0,LW(2),RW(2),NOPTSWITCH,RTEMP,CLINE)
          IF(IFAIL.EQ.0)THEN
            IF(NOPTSWITCH.LT.1)THEN
              WRITE(ERRMSG,541)
     +        ALINE(1:NALINE),CASFL(1:MCASE)
541           FORMAT(A,A,': integer variable NOPTSWITCH must be ',
     +        '1 or greater.')
              CALL WRTERR
            ELSE
              IFIND=1
            END IF
          ELSE
            ADO=CLINE(LW(2):RW(2))
            CALL LOWCAS(ADO)
            IF((ADO.EQ.'aui').OR.(ADO.EQ.'noaui').OR.
     +         (ADO.eq.'auid'))THEN
              DOAUI=ADO
              AUIFLAG=1
            ELSE IF((ADO.EQ.'senreuse').OR.(ADO.EQ.'nosenreuse'))THEN
              DOSENREUSE=ADO
              SENREUSEFLAG=1
            ELSE
              WRITE(ERRMSG,544) ALINE(1:NALINE),CASFL(1:MCASE),TRIM(ADO)
544           FORMAT(a,a,': unrecognized variable "',A,'".')
              CALL WRTERR
            END IF
            CALL LINSPL(IFAIL,3,LW,RW,CLINE)
            IF(IFAIL.EQ.0)THEN
              ADO=CLINE(LW(3):RW(3))
              CALL LOWCAS(ADO)
              IF((ADO.EQ.'aui').OR.(ADO.EQ.'noaui').OR.
     +           (ADO.EQ.'auid'))THEN
                IF(AUIFLAG.NE.0)THEN
                  WRITE(ERRMSG,545) ALINE(1:NALINE),CASFL(1:MCASE)
545               FORMAT(A,A,': DOAUI variable provided twice.')
                  CALL WRTERR
                ELSE
                  DOAUI=ADO
                END IF
              ELSE IF((ADO.EQ.'senreuse').OR.(ADO.EQ.'nosenreuse'))THEN
                IF(SENREUSEFLAG.NE.0)THEN
                  WRITE(ERRMSG,546) ALINE(1:NALINE),CASFL(1:MCASE)
546               FORMAT(A,A,': SENREUSE variable provided twice.')
                  CALL WRTERR
                ELSE
                  DOSENREUSE=ADO
                END IF
              ELSE
                IF(IDELETE.EQ.0)THEN
                  APLACE='third'
                  WRITE(ERRMSG,544) ALINE(1:NALINE),CASFL(1:MCASE),
     +            TRIM(ADO)
                  CALL WRTERR
                ELSE
                  APLACE='fourth'
                  WRITE(ERRMSG,544) ALINE(1:NALINE),CASFL(1:MCASE),
     +            TRIM(ADO)
                  CALL WRTERR
                END IF
              END IF
            END IF
          END IF
          IF(IFIND.EQ.1)THEN
            CALL LINSPL(IFAIL,3,LW,RW,CLINE)
            IF(IFAIL.EQ.0)THEN
              ADO=CLINE(LW(3):RW(3))
              CALL LOWCAS(ADO)
              IF((ADO.EQ.'aui').OR.(ADO.EQ.'noaui').OR.
     +           (ADO.EQ.'auid'))THEN
                DOAUI=ADO
                AUIFLAG=1
              ELSE IF((ADO.EQ.'senreuse').OR.(ADO.EQ.'nosenreuse'))THEN
                DOSENREUSE=ADO
                SENREUSEFLAG=1
              ELSE
                IF(IDELETE.EQ.0)THEN
                  APLACE='third'
                  WRITE(ERRMSG,544) ALINE(1:NALINE),CASFL(1:MCASE),
     +            TRIM(ADO)
                ELSE
                  APLACE='fourth'
                  WRITE(ERRMSG,544) ALINE(1:NALINE),CASFL(1:MCASE),
     +            TRIM(ADO)
                END IF
                CALL WRTERR
              END IF
              CALL LINSPL(IFAIL,4,LW,RW,CLINE)
              IF(IFAIL.EQ.0)THEN
                ADO=CLINE(LW(4):RW(4))
                CALL LOWCAS(ADO)
                IF((ADO.EQ.'aui').OR.(ADO.EQ.'noaui').OR.
     +             (ADO.EQ.'auid'))THEN
                  IF(AUIFLAG.NE.0)THEN
                    WRITE(ERRMSG,545) ALINE(1:NALINE),CASFL(1:MCASE)
                    CALL WRTERR
                  ELSE
                    DOAUI=ADO
                  END IF
                ELSE IF((ADO.EQ.'senreuse').OR.(ADO.EQ.'nosenreuse'))
     +          THEN
                  IF(SENREUSEFLAG.NE.0)THEN
                    WRITE(ERRMSG,546) ALINE(1:NALINE),CASFL(1:MCASE)
                    CALL WRTERR
                  ELSE
                    DOSENREUSE=ADO
                  END IF
                ELSE
                  IF(IDELETE.EQ.0)THEN
                    APLACE='fourth'
                  ELSE
                    APLACE='fifth'
                  END IF
                  WRITE(ERRMSG,544) ALINE(1:NALINE),CASFL(1:MCASE),
     +            TRIM(ADO)
                  CALL WRTERR
                END IF
              END IF
            END IF
          END IF
        END IF
        IF(DOAUI.EQ.'aui')THEN
          IF((IREG.EQ.1).OR.(IPRED.NE.0))THEN
            WRITE(ERRMSG,542) ALINE(1:NALINE),CASFL(1:MCASE)
542         FORMAT(A,A,': DOAUI is set to "aui" ',
     +      ' - however automatic ',
     +      'user intervention is not permitted when PEST runs in ',
     +      'regularisation or predictive analysis modes.')
            CALL WRTERR
          END IF
        END IF
        IDOSENREUSE=0
        IF(DOSENREUSE.EQ.'senreuse')THEN
          IDOSENREUSE=1
          IF(JACFILE.NE.0)THEN
            WRITE(ERRMSG,543) ALINE(1:NALINE),CASFL(1:MCASE)
543         FORMAT(A,A,': DOSENREUSE is set to "senreuse" - however ',
     +      'sensitivity re-use must not be activated when JACFILE ',
     +      'is non-zero, thereby requesting external derivatives.')
            CALL WRTERR
          END IF
        END IF
        IF(IBOUNDSCALE.NE.0)THEN
          IF(PESTMODE.EQ.2)THEN
            WRITE(ERRMSG,5431)
5431        FORMAT(' Parameter bounds scaling cannot be activated ',
     +      'if PEST is run in predictive analysis mode.')
            CALL WRTERR
          END IF
        END IF

        NOPTMX=0
7606    CONTINUE
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        NALINE=LEN_TRIM(ALINE)+1
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7606

C -- Before we do anything else we look for HARDSTOPHOURS and SOFTSTOPHOURS
C    and remove reference to these from the line.

        HARDSTOPHOURS=-1.1D100
        SOFTSTOPHOURS=-1.1D100
        CALL PC_READ_STOPHOURS(IIFAIL,NALINE,1,HARDSTOPHOURS,CLINE)
        IF(IIFAIL.EQ.0)THEN
          CALL PC_READ_STOPHOURS(IIFAIL,NALINE,0,SOFTSTOPHOURS,CLINE)
          IF(HARDSTOPHOURS.GT.0.0D0)THEN
            IF(SOFTSTOPHOURS.GT.0.0D0)THEN
              WRITE(ERRMSG,76061) ALINE(1:NALINE),CASFL(1:MCASE)
76061         FORMAT(A,A,': a value can be supplied for either ',
     +        'HARDSTOPHOURS or SOFTSTOPHOURS, but not for both.')
              CALL WRTERR
            END IF
          END IF
        END IF

C -- Now the rest of the line is read.

        CALL LINSPL(IFAIL,6,LW,RW,CLINE)
        IF(IFAIL.NE.0) GO TO 9500
        CALL NUMRD(IFAIL,0,LW(1),RW(1),NOPTMX,RTEMP,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(0,'NOPTMAX')
        ELSE
          IF(NOPTMX.LT.-2) THEN
            WRITE(ERRMSG,550) ALINE(1:NALINE),CASFL(1:MCASE)
550         FORMAT(A,A,': integer variable NOPTMAX less than -2.')
            CALL WRTERR
          END IF
        END IF
        ITEMP=MAX(NOPTMX,1)
        ALLOCATE(PHIABANDON(0:ITEMP),STAT=IALERR)
        IF(IALERR.NE.0) GO TO 9400
        PHIABANDON=-1.1D100
        CALL NUMRD(IFAIL,1,LW(2),RW(2),ITEMP,PSISTP,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(1,'PHIREDSTP')
        ELSE
          IF((PSISTP.LE.0.0D0).OR.(PSISTP.GE.1.0D0)) THEN
            WRITE(ERRMSG,570) ALINE(1:NALINE),CASFL(1:MCASE)
570         FORMAT(A,A,': real variable PHIREDSTP must be between ',
     +      'zero and one.')
            CALL WRTERR
          END IF
        END IF
        CALL NUMRD(IFAIL,0,LW(3),RW(3),NPSILO,RTEMP,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(0,'NPHISTP')
        ELSE
          IF(NPSILO.LE.1)THEN
            WRITE(ERRMSG,575) ALINE(1:NALINE),CASFL(1:MCASE)
575         FORMAT(A,A,': integer variable NPHISTP must be 2 or ',
     +      'greater.')
            CALL WRTERR
          ENDIF
        END IF
        CALL NUMRD(IFAIL,0,LW(4),RW(4),NLOPT,RTEMP,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(0,'NPHINORED')
        ELSE
          IF(NLOPT.LE.0)THEN
            CALL ZRONEG(0,'NPHINORED')
          END IF
        END IF
        CALL NUMRD(IFAIL,1,LW(5),RW(5),ITEMP,ENDCHG,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(1,'RELPARSTP')
        ELSE
          IF(ENDCHG.LE.0.0D0) CALL ZRONEG(1,'RELPARSTP')
        END IF
        IF((DMAX0.GT.0.0D0).AND.(ENDCHG.GE.DMAX0))THEN
          WRITE(ERRMSG,631) ALINE(1:NALINE), CASFL(1:MCASE)
631       FORMAT(A,A,': RELPARSTP must be less than RELPARMAX.')
          CALL WRTERR
        END IF
        CALL NUMRD(IFAIL,0,LW(6),RW(6),NUMCHG,RTEMP,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(0,'NRELPAR')
        ELSE
          IF(NUMCHG.LE.0)THEN
            CALL ZRONEG(0,'NRELPAR')
          END IF
        ENDIF
        IF(IIFAIL.NE.0) GO TO 46501
        CALL LINSPL(IFAIL,7,LW,RW,CLINE)
        IF(IFAIL.EQ.0)THEN
          CALL NUMRD(IFAIL,1,LW(7),RW(7),ITEMP,PHISTOPTHRESH,CLINE)
          IF(IFAIL.NE.0)THEN
            CALL RDERR(1,'PHISTOPTHRESH')
          ELSE
            IF(PHISTOPTHRESH.GT.0.0D0)THEN
              IF(IPRED.NE.0)THEN
                WRITE(ERRMSG,1632) ALINE(1:NALINE), CASFL(1:MCASE)
1632            FORMAT(A,A,': PHISTOPTHRESH must not be positive if ',
     +          'PEST is run in predictive analysis mode.')
                CALL WRTERR
              ELSE IF(PESTMODE.EQ.4)THEN
                WRITE(ERRMSG,16321) ALINE(1:NALINE), CASFL(1:MCASE)
16321           FORMAT(A,A,': PHISTOPTHRESH must not be positive if ',
     +          'PEST is run in pareto mode.')
                CALL WRTERR
              END IF
            END IF
          END IF
          CALL LINSPL(IFAIL,8,LW,RW,CLINE)
          IF(IFAIL.EQ.0)THEN
            CALL NUMRD(IFAIL,0,LW(8),RW(8),LASTRUN,RTEMP,CLINE)
            IF(IFAIL.NE.0)THEN
              CALL RDERR(0,'LASTRUN')
            ELSE
              IF((LASTRUN.NE.1).AND.(LASTRUN.NE.0))THEN
                WRITE(ERRMSG,1633) ALINE(1:NALINE), CASFL(1:MCASE)
1633            FORMAT(A,A,': LASTRUN must be zero or one.')
                CALL WRTERR
              END IF
              IF(PESTMODE.EQ.4)THEN
                IF(LASTRUN.EQ.1)THEN
                  WRITE(ERRMSG,16331) ALINE(1:NALINE), CASFL(1:MCASE)
16331             FORMAT(A,A,': LASTRUN must be set to zero if PEST ',
     +            'is run in pareto mode.')
                  CALL WRTERR
                END IF
              END IF
            END IF
            CALL SPACESUB(CLINE)
            CALL LINSPL(IFAIL,9,LW,RW,CLINE)
            IF(IFAIL.EQ.0)THEN
              CALL NUMRD(IFAIL,1,LW(9),RW(9),ITEMP,PHIAB,CLINE)
              IF(IFAIL.EQ.0)THEN
                IF(PHIAB.GT.0.0D0)THEN
                  IF(IPRED.NE.0)THEN
                    WRITE(ERRMSG,1634) ALINE(1:NALINE), CASFL(1:MCASE)
1634                FORMAT(A,A,': PHIABANDON must not be positive if ',
     +              'PEST is run in predictive analysis mode.')
                    CALL WRTERR
                  ELSE IF(PESTMODE.EQ.4)THEN
                    WRITE(ERRMSG,16341) ALINE(1:NALINE), CASFL(1:MCASE)
16341               FORMAT(A,A,': PHIABANDON must not be positive if ',
     +              'PEST is run in pareto mode.')
                    CALL WRTERR
                  END IF
                END IF
              ELSE
                IF(PESTMODE.EQ.4)THEN
                  WRITE(ERRMSG,16342) ALINE(1:NALINE), CASFL(1:MCASE)
16342             FORMAT(A,A,': an abandonment schedule file must not ',
     +            'be supplied if PEST is run in pareto mode.')
                  CALL WRTERR
                  GO TO 46501
                END IF
                SCHEDULEFILE=CLINE(LW(9):RW(9))
                CALL REMCHAR(SCHEDULEFILE,CHAR(211))
                IUNIT=31
                OPEN(UNIT=IUNIT,FILE=SCHEDULEFILE,STATUS='OLD',
     +          IOSTAT=IFAIL)
                IF(IFAIL.NE.0)THEN
                  WRITE(ERRMSG,4500) TRIM(SCHEDULEFILE),
     +            CASFL(1:MCASE)
4500              FORMAT(' Cannot open PHIABANDON schedule file ',
     +            A,' cited on line 9 of file ',A,'.')
                  CALL WRTERR
                  GO TO 4650
                END IF
                DO K=0,NOPTMX
                  READ(IUNIT,*,ERR=4530,END=4600) PHIABANDON(K)
                  GO TO 4550
4530              WRITE(AK,'(I6)') K+1
                  AK=ADJUSTL(AK)
                  WRITE(ERRMSG,4540) TRIM(AK),TRIM(SCHEDULEFILE),
     +            CASFL(1:MCASE)
4540              FORMAT(' Cannot read value number ',a,
     +            ' from PHIABANDON schedule file ',a,' cited on ',
     +            'line 9 of PEST control file ',a,'.')
                  CALL WRTERR
4550              CONTINUE
                END DO
4600            CONTINUE
                CLOSE(UNIT=IUNIT,IOSTAT=JERR)
                IF(K.EQ.0)THEN
                  WRITE(ERRMSG,4610) TRIM(SCHEDULEFILE),CASFL(1:MCASE)
4610              FORMAT(' No PHIABANDON values found in file ',a,
     +            ' cited on line 9 of PEST control file ',a,'.')
                  CALL WRTERR
                END IF
4650            CONTINUE
              END IF
            END IF
          END IF
        END IF
46501   CONTINUE

7607    CONTINUE
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7607
        CALL LINSPL(IFAIL,3,LW,RW,CLINE)
        IF(IFAIL.NE.0) GO TO 9500
        CALL LOWCAS(CLINE)
        IJCO=-9999
        IREI=-9999
        JCOSAVE=-9999
        VERBOSEREC=-9999
        PARSAVEITN=-9999
        PARSAVERUN=-9999
        WRITE_RRF=-9999
        ITEMP=INDEX(CLINE,' jcosaveitn ')
        IF(ITEMP.NE.0)THEN
          IF(IJCO.EQ.0)THEN
            WRITE(ERRMSG,46504) AALINE(1:MMLINE)
            CALL WRTERR
          END IF
          CLINE(ITEMP+1:ITEMP+10)=' '
          IJCO=1
        END IF
        ITEMP=INDEX(CLINE,' nojcosaveitn ')
        IF(ITEMP.NE.0)THEN
          IF(IJCO.EQ.1)THEN
            WRITE(ERRMSG,46504) AALINE(1:MMLINE)
46504       FORMAT(A,'contradictory JCOSAVEITN settings.')
            CALL WRTERR
          END IF
          CLINE(ITEMP+1:ITEMP+12)=' '
          IJCO=0
        END IF
        IF(IJCO.EQ.-9999)IJCO=0

        ITEMP=INDEX(CLINE,' reisaveitn ')
        IF(ITEMP.NE.0)THEN
          IF(IREI.EQ.0) THEN
            WRITE(ERRMSG,46505) AALINE(1:MMLINE)
            CALL WRTERR
          END IF
          CLINE(ITEMP+1:ITEMP+10)=' '
          IREI=1
        END IF
        ITEMP=INDEX(CLINE,' noreisaveitn ')
        IF(ITEMP.NE.0)THEN
          IF(IREI.EQ.1) THEN
            WRITE(ERRMSG,46505) AALINE(1:MMLINE)
46505       FORMAT(A,'contradictory REISAVEITN settings.')
            CALL WRTERR
          END IF
          CLINE(ITEMP+1:ITEMP+12)=' '
          IREI=0
        END IF
        IF(IREI.EQ.-9999)IREI=1

        ITEMP=INDEX(CLINE,' jcosave ')
        IF(ITEMP.NE.0)THEN
          IF(JCOSAVE.EQ.0) THEN
            WRITE(ERRMSG,46502) AALINE(1:MMLINE)
            CALL WRTERR
          END IF
          CLINE(ITEMP+1:ITEMP+8)=' '
          JCOSAVE=1
        END IF
        ITEMP=INDEX(CLINE,' nojcosave ')
        IF(ITEMP.NE.0)THEN
          IF(JCOSAVE.EQ.1) THEN
            WRITE(ERRMSG,46502) AALINE(1:MMLINE)
46502       FORMAT(A,'contradictory JCOSAVE settings.')
            CALL WRTERR
          END IF
          CLINE(ITEMP+1:ITEMP+10)=' '
          JCOSAVE=0
        END IF
        IF(JCOSAVE.EQ.-9999)JCOSAVE=1

        ITEMP=INDEX(CLINE,' verboserec ')
        IF(ITEMP.NE.0)THEN
          IF(VERBOSEREC.EQ.0) THEN
            WRITE(ERRMSG,46503) AALINE(1:MMLINE)
            CALL WRTERR
          END IF
          CLINE(ITEMP+1:ITEMP+11)=' '
          VERBOSEREC=1
        END IF
        ITEMP=INDEX(CLINE,' noverboserec ')
        IF(ITEMP.NE.0)THEN
          IF(VERBOSEREC.EQ.1) THEN
            WRITE(ERRMSG,46503) AALINE(1:MMLINE)
46503       FORMAT(A,'contradictory VERBOSEREC settings.')
            CALL WRTERR
          END IF
          CLINE(ITEMP+1:ITEMP+13)=' '
          VERBOSEREC=0
        END IF
        IF(VERBOSEREC.EQ.-9999)VERBOSEREC=1

        ITEMP=INDEX(CLINE,' parsaveitn ')
        IF(ITEMP.NE.0)THEN
          IF(PARSAVEITN.EQ.0) THEN
            WRITE(ERRMSG,46506) AALINE(1:MMLINE)
            CALL WRTERR
          END IF
          CLINE(ITEMP+1:ITEMP+11)=' '
          PARSAVEITN=1
        END IF
        ITEMP=INDEX(CLINE,' noparsaveitn ')
        IF(ITEMP.NE.0)THEN
          IF(PARSAVEITN.EQ.1) THEN
            WRITE(ERRMSG,46506) AALINE(1:MMLINE)
46506       FORMAT(A,'contradictory PARSAVEITN settings.')
            CALL WRTERR
          END IF
          CLINE(ITEMP+1:ITEMP+13)=' '
          PARSAVEITN=0
        END IF
        IF(PARSAVEITN.EQ.-9999)PARSAVEITN=0

        ITEMP=INDEX(CLINE,' parsaverun ')
        IF(ITEMP.NE.0)THEN
          IF(PARSAVERUN.EQ.0) THEN
            WRITE(ERRMSG,46507) AALINE(1:MMLINE)
            CALL WRTERR
          END IF
          CLINE(ITEMP+1:ITEMP+11)=' '
          PARSAVERUN=1
        END IF
        ITEMP=INDEX(CLINE,' noparsaverun ')
        IF(ITEMP.NE.0)THEN
          IF(PARSAVERUN.EQ.1) THEN
            WRITE(ERRMSG,46507) AALINE(1:MMLINE)
46507       FORMAT(A,'contradictory PARSAVERUN settings.')
            CALL WRTERR
          END IF
          CLINE(ITEMP+1:ITEMP+13)=' '
          PARSAVERUN=0
        END IF

        ITEMP=INDEX(CLINE,' rrfsave ')
        IF(ITEMP.NE.0)THEN
          IF(WRITE_RRF.EQ.0) THEN
            WRITE(ERRMSG,46508) AALINE(1:MMLINE)
            CALL WRTERR
          END IF
          CLINE(ITEMP+1:ITEMP+8)=' '
          WRITE_RRF=1
        END IF
        ITEMP=INDEX(CLINE,' norrfsave ')
        IF(ITEMP.NE.0)THEN
          IF(WRITE_RRF.EQ.1) THEN
            WRITE(ERRMSG,46508) AALINE(1:MMLINE)
46508       FORMAT(A,'contradictory RRFSAVE settings.')
            CALL WRTERR
          END IF
          CLINE(ITEMP+1:ITEMP+10)=' '
          WRITE_RRF=0
        END IF

        IF(PARSAVERUN.EQ.-9999)PARSAVERUN=0
        IF((INDEX(CLINE,'parsaverun').NE.0).OR.
     +     (INDEX(CLINE,'parsaveitn').NE.0).OR.
     +     (INDEX(CLINE,'verboserec').NE.0).OR.
     +     (INDEX(CLINE,'jcosave').NE.0).OR.
     +     (INDEX(CLINE,'reisaveitn').NE.0).OR.
     +     (INDEX(CLINE,'jcosaveitn').NE.0).OR.
     +     (INDEX(CLINE,'rrfsave').NE.0))THEN
            WRITE(ERRMSG,46512) AALINE(1:MMLINE)
46512       FORMAT(A,'at least one variable is duplicated.')
            CALL WRTERR
        END IF

        CALL LINSPL(IFAIL,3,LW,RW,CLINE)
        IF(IFAIL.NE.0) GO TO 9500
        CALL NUMRD(IFAIL,0,LW(1),RW(1),ICOV,RTEMP,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(0,'ICOV')
        ELSE
          IF((ICOV.NE.0).AND.(ICOV.NE.1))CALL ZROONE('ICOV')
        END IF
        CALL NUMRD(IFAIL,0,LW(2),RW(2),ICOR,RTEMP,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(0,'ICOR')
        ELSE
          IF((ICOR.NE.0).AND.(ICOR.NE.1))CALL ZROONE('ICOR')
        END IF
        CALL NUMRD(IFAIL,0,LW(3),RW(3),IEIG,RTEMP,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(0,'IEIG')
        ELSE
          IF((IEIG.NE.0).AND.(IEIG.NE.1))CALL ZROONE('IEIG')
        END IF
        IF(IERR.NE.0) GO TO 9800
        CALL LINSPL(IFAIL,4,LW,RW,CLINE)
        IF(IFAIL.EQ.0)THEN
          CALL NUMRD(IFAIL,0,LW(4),RW(4),IRES,RTEMP,CLINE)
          IF(IFAIL.NE.0) THEN
            WRITE(ERRMSG,3139) AALINE(1:MMLINE),CLINE(LW(4):RW(4))
            CALL WRTERR
          ELSE
            IF((IRES.NE.0).AND.(IRES.NE.1))CALL ZROONE('IRES')
          END IF
          CALL LINSPL(IFAIL,5,LW,RW,CLINE)
          IF(IFAIL.EQ.0)THEN
            WRITE(ERRMSG,3139) AALINE(1:MMLINE),CLINE(LW(5):RW(5))
3139        FORMAT(A,'unrecognised character string "',a,'".')
            CALL WRTERR
            DO I=1,8
              CALL LINSPL(IFAIL,5+I,LW,RW,CLINE)
              IF(IFAIL.NE.0) GO TO 3138
              WRITE(ERRMSG,3139) AALINE(1:MMLINE),CLINE(LW(5+I):RW(5+I))
              CALL WRTERR
            END DO
3138        CONTINUE
          END IF
        END IF
        IF((NOPTMX.EQ.-1).OR.(NOPTMX.EQ.-2))THEN
          IF(JCOSAVE.EQ.0)THEN
            WRITE(ERRMSG,3137)
3137        FORMAT(' If NOPTMAX is set to -1 or -2, JCOSAVE must not ',
     +      'be set to "nojcosave".')
            CALL WRTERR
          END IF
        END IF
        IF((IJCO.EQ.1).AND.(JCOSAVE.EQ.0))THEN
          WRITE(ERRMSG,3136) AALINE(1:MMLINE)
3136      FORMAT(A,'if JCOSAVEITN is set to "jcosaveitn" then ',
     +    'JCOSAVE must NOT be set to "nojcosave".')
          CALL WRTERR
        END IF

C -- IF AN AUTOMATIC USER INTERVENTION SECTION IS PRESENT, IT IS READ
C -- AND CHECKED.

3142    CONTINUE
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 3142
        CALL LOWCAS(CLINE)
        CALL SHIFTL(CLINE)
3143    IF(CLINE(1:1).NE.'*')THEN
          WRITE(ERRMSG,632) AALINE(1:MMLINE)
632       FORMAT(A,'section header expected.')
          CALL WRTERR
          GO TO 9800
        END IF
        SVDMODE=0
        LSQRMODE=0
        ISVD=0
        ILSQR=0
        IF(INDEX(CLINE,'group').NE.0)GO TO 649
        IF(INDEX(CLINE,'svd assist').NE.0) THEN
          CALL SVDACHEK(IFAIL,ILINE,IASECT,DOAUI,NUMCOM,JACFILE,CLINE,
     +    SVDA_MULBPA,SVDA_NLSCALADJ,SVDA_EXTSUPER,SVDA_SUPDERCALC,
     +    SVDA_PAR_EXCL,PESTMODE,IPPCOUNT)
          IF(IFAIL.EQ.1)THEN
            GO TO 9600
          ELSE IF(IFAIL.EQ.2) THEN
            GO TO 9700
          ELSE IF(IFAIL.EQ.3)THEN
            GO TO 9800
          END IF
          IF(IERR.NE.0) GO TO 9800
          ISVDA=1
          IF(SVDA_MULBPA.EQ.-999)THEN
            SVDA_MULBPA=0
            SVDA_NLSCALADJ=0
            GO TO 3143
          ELSE
            GO TO 3142
          END IF
        END IF
31458   CONTINUE
        IF(INDEX(CLINE,'singular value').NE.0)THEN
          IF(ISVDA.EQ.1)THEN
            WRITE(ERRMSG,31452)
31452       FORMAT(' "Singular value decomposition" section must ',
     +      'precede "svd assist" section in PEST control file.')
            CALL WRTERR
          END IF
          IF(ISVD.EQ.1)THEN
            WRITE(ERRMSG,31453) 'singular value decomposition'
31453       FORMAT(' There is more than one "',A,
     +      '" section in PEST control file.')
            CALL WRTERR
          END IF
7608      CONTINUE
          ILINE=ILINE+1
          CALL WRTINT(ATEMP,ILINE)
          ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
          AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
          MMLINE=NBLNK(AALINE)+1
          READ(30,'(A)',ERR=9600,END=9700) CLINE
          CALL PPROC(CLINE,IPPCOUNT)
          IF(CLINE.EQ.' ') GO TO 7608
          CALL LINSPL(IFAIL,1,LW,RW,CLINE)
          IF(IFAIL.NE.0) GO TO 9500
          ISVD=1
          CALL NUMRD(IFAIL,0,LW(1),RW(1),SVDMODE,RTEMP,CLINE)
          IF(IFAIL.NE.0)THEN
            CALL RDERR(0,'SVDMODE')
          ELSE
            IF((SVDMODE.NE.0).AND.(SVDMODE.NE.1).AND.
     +         (SVDMODE.NE.2))THEN
              WRITE(ERRMSG,3120) AALINE(1:MMLINE)
3120          FORMAT(A,'SVDMODE must be 0, 1 or 2.')
              CALL WRTERR
            END IF
          END IF
          IF((SVDMODE.NE.0).AND.(DOAUI(1:3).EQ.'aui'))THEN
            WRITE(ERRMSG,3110) AALINE(1:MMLINE)
3110        FORMAT(A,'singular value decomposition must not be ',
     +      'activated ',
     +      'if automatic user intervention is also activated.')
            CALL WRTERR
          END IF
          IF((SVDMODE.NE.0).AND.(IPRED.EQ.1))THEN
            WRITE(ERRMSG,3111) AALINE(1:MMLINE)
3111        FORMAT(A,'singular value decomposition must not be ',
     +      'activated if PEST is run in predictive analysis ',
     +      'mode.')
            CALL WRTERR
          END IF
C          IF((SVDMODE.EQ.2).AND.(IREG.EQ.1))THEN
C            WRITE(ERRMSG,3112) AALINE(1:MMLINE)
C3112        FORMAT(A,'SVDMODE must not be set to 2 if PEST is ',
C     +      'run in regularisation mode.')
C            CALL WRTERR
C          END IF
          IF((SVDMODE.NE.0).AND.(LSQRMODE.NE.0))THEN
            WRITE(ERRMSG,31121) AALINE(1:MMLINE)
31121       FORMAT(A,'SVD must not be activated if LSQR is ',
     +      'also activated.')
            CALL WRTERR
          END IF
7609      CONTINUE
          ILINE=ILINE+1
          CALL WRTINT(ATEMP,ILINE)
          ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
          AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
          MMLINE=NBLNK(AALINE)+1
          READ(30,'(A)',ERR=9600,END=9700) CLINE
          CALL PPROC(CLINE,IPPCOUNT)
          IF(CLINE.EQ.' ') GO TO 7609
          CALL LINSPL(IFAIL,2,LW,RW,CLINE)
          IF(IFAIL.NE.0) GO TO 9500
          CALL NUMRD(IFAIL,0,LW(1),RW(1),MAXSING,RTEMP,CLINE)
          IF(IFAIL.NE.0)THEN
            CALL RDERR(0,'MAXSING')
          ELSE
            IF(MAXSING.LE.0)THEN
              WRITE(ERRMSG,3130) AALINE(1:MMLINE)
3130          FORMAT(A,'MAXSING must be greater than zero.')
              CALL WRTERR
            END IF
          END IF
          CALL NUMRD(IFAIL,1,LW(2),RW(2),ITEMP,EIGTHRESH,CLINE)
          IF(IFAIL.NE.0)THEN
            CALL RDERR(1,'EIGTHRESH')
          ELSE
            IF((EIGTHRESH.GE.1.0).OR.(EIGTHRESH.LT.0.0D0))THEN
              WRITE(ERRMSG,3140) AALINE(1:MMLINE)
3140          FORMAT(A,'EIGTHRESH must be between zero and one.')
              CALL WRTERR
            END IF
          END IF
7611      CONTINUE
          ILINE=ILINE+1
          CALL WRTINT(ATEMP,ILINE)
          ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
          AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
          MMLINE=NBLNK(AALINE)+1
          READ(30,'(A)',ERR=9600,END=9700) CLINE
          CALL PPROC(CLINE,IPPCOUNT)
          IF(CLINE.EQ.' ') GO TO 7611
          CALL LINSPL(IFAIL,1,LW,RW,CLINE)
          IF(IFAIL.NE.0) GO TO 9500
          CALL NUMRD(IFAIL,0,LW(1),RW(1),EIGWRITE,RTEMP,CLINE)
          IF(IFAIL.NE.0)THEN
            CALL RDERR(0,'EIGWRITE')
          END IF
3141      ILINE=ILINE+1
          CALL WRTINT(ATEMP,ILINE)
          ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
          AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
          MMLINE=NBLNK(AALINE)+1
          READ(30,'(A)',ERR=9600,END=9700) CLINE
          CALL PPROC(CLINE,IPPCOUNT)
          IF(CLINE.EQ.' ') GO TO 3141
          CALL LOWCAS(CLINE)
          CALL SHIFTL(CLINE)
31411     IF(CLINE(1:1).NE.'*')THEN
            WRITE(ERRMSG,632) AALINE(1:MMLINE)
            CALL WRTERR
            GO TO 9800
          END IF
          IF(INDEX(CLINE,'group').NE.0)GO TO 649
          IF(INDEX(CLINE,'svd assist').NE.0) THEN
            CALL SVDACHEK(IFAIL,ILINE,IASECT,DOAUI,NUMCOM,JACFILE,CLINE,
     +      SVDA_MULBPA,SVDA_NLSCALADJ,SVDA_EXTSUPER,SVDA_SUPDERCALC,
     +      SVDA_PAR_EXCL,PESTMODE,IPPCOUNT)
            IF(IFAIL.EQ.1)THEN
              GO TO 9600
            ELSE IF(IFAIL.EQ.2) THEN
              GO TO 9700
            ELSE IF(IFAIL.EQ.3)THEN
              GO TO 9800
            END IF
            IF(IERR.NE.0) GO TO 9800
            ISVDA=1
            IF(SVDA_MULBPA.EQ.-999)THEN
              SVDA_MULBPA=0
              SVDA_NLSCALADJ=0
              GO TO 31411
            ELSE
              GO TO 3141
            END IF
          END IF
        END IF

31449   CONTINUE
        IF(INDEX(CLINE,'lsqr').NE.0)THEN
          IF(ISVDA.EQ.1)THEN
            WRITE(ERRMSG,31451)
31451       FORMAT(' "LSQR" section must ',
     +      'precede "svd assist" section in PEST control file.')
            CALL WRTERR
          END IF
          IF(ILSQR.EQ.1)THEN
            WRITE(ERRMSG,31453) 'lsqr'
            CALL WRTERR
          END IF
7612      CONTINUE
          ILINE=ILINE+1
          CALL WRTINT(ATEMP,ILINE)
          ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
          AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
          MMLINE=NBLNK(AALINE)+1
          READ(30,'(A)',ERR=9600,END=9700) CLINE
          CALL PPROC(CLINE,IPPCOUNT)
          IF(CLINE.EQ.' ') GO TO 7612
          CALL LINSPL(IFAIL,1,LW,RW,CLINE)
          IF(IFAIL.NE.0) GO TO 9500
          ILSQR=1
          CALL NUMRD(IFAIL,0,LW(1),RW(1),LSQRMODE,RTEMP,CLINE)
          IF(IFAIL.NE.0)THEN
            CALL RDERR(0,'LSQRMODE')
          ELSE
            IF((LSQRMODE.NE.0).AND.(LSQRMODE.NE.1))THEN
              WRITE(ERRMSG,31201) AALINE(1:MMLINE)
31201         FORMAT(A,'LSQRMODE must be 0 or 1.')
              CALL WRTERR
            END IF
          END IF
          IF((LSQRMODE.NE.0).AND.(DOAUI(1:3).EQ.'aui'))THEN
            WRITE(ERRMSG,31101) AALINE(1:MMLINE)
31101       FORMAT(A,'LSQR must not be activated ',
     +      'if automatic user intervention is also activated.')
            CALL WRTERR
          END IF
          IF((LSQRMODE.NE.0).AND.(IPRED.EQ.1))THEN
            WRITE(ERRMSG,31111) AALINE(1:MMLINE)
31111       FORMAT(A,'LSQR must not be ',
     +      'activated if PEST is run in predictive analysis ',
     +      'mode.')
            CALL WRTERR
          END IF
          IF((LSQRMODE.NE.0).AND.(SVDMODE.NE.0))THEN
            WRITE(ERRMSG,31112) AALINE(1:MMLINE)
31112       FORMAT(A,'LSQR must not be activated if SVD is also ',
     +      'activated.')
            CALL WRTERR
          END IF
7613      CONTINUE
          ILINE=ILINE+1
          CALL WRTINT(ATEMP,ILINE)
          ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
          AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
          MMLINE=NBLNK(AALINE)+1
          READ(30,'(A)',ERR=9600,END=9700) CLINE
          CALL PPROC(CLINE,IPPCOUNT)
          IF(CLINE.EQ.' ') GO TO 7613
          CALL LINSPL(IFAIL,4,LW,RW,CLINE)
          IF(IFAIL.NE.0) GO TO 9500
          CALL NUMRD(IFAIL,1,LW(1),RW(1),ITEMP,LSQR_ATOL,CLINE)
          IF(IFAIL.NE.0)THEN
            CALL RDERR(1,'LSQR_ATOL')
          ELSE
            IF(LSQR_ATOL.LT.0.0D0)THEN
              WRITE(ERRMSG,31301) AALINE(1:MMLINE)
31301         FORMAT(A,'LSQR_ATOL must not be less than zero.')
              CALL WRTERR
            END IF
          END IF
          CALL NUMRD(IFAIL,1,LW(2),RW(2),ITEMP,LSQR_BTOL,CLINE)
          IF(IFAIL.NE.0)THEN
            CALL RDERR(1,'LSQR_BTOL')
          ELSE
            IF(LSQR_BTOL.LT.0.0D0)THEN
              WRITE(ERRMSG,31302) AALINE(1:MMLINE)
31302         FORMAT(A,'LSQR_BTOL must not be less than zero.')
              CALL WRTERR
            END IF
          END IF
          CALL NUMRD(IFAIL,1,LW(3),RW(3),ITEMP,LSQR_CONLIM,CLINE)
          IF(IFAIL.NE.0)THEN
            CALL RDERR(1,'LSQR_CONLIM')
          ELSE
            IF(LSQR_CONLIM.LT.0.0D0)THEN
              WRITE(ERRMSG,31303) AALINE(1:MMLINE)
31303         FORMAT(A,'LSQR_CONLIM must not be less than zero.')
              CALL WRTERR
            END IF
          END IF
          CALL NUMRD(IFAIL,0,LW(4),RW(4),LSQR_ITNLIM,RTEMP,CLINE)
          IF(IFAIL.NE.0)THEN
            CALL RDERR(0,'LSQR_ITNLIM')
          ELSE
            IF(LSQR_ITNLIM.LE.0)THEN
              WRITE(ERRMSG,31304) AALINE(1:MMLINE)
31304         FORMAT(A,'LSQR_ITNLIM must be greater than zero.')
              CALL WRTERR
            END IF
          END IF

          RSQR_ITNLIM=-999
          CALL LINSPL(IFAIL,5,LW,RW,CLINE)
          ATEMP=CLINE(LW(5):RW(5))
          CALL LOWCAS(ATEMP)
          IF(ATEMP.EQ.'alt_regwt')THEN
            CALL LINSPL(IFAIL,10,LW,RW,CLINE)
            IF(IFAIL.NE.0)THEN
              WRITE(ERRMSG,31306) AALINE(1:MMLINE)
31306         FORMAT(A,'if alternative LSQR control variables are ',
     +        'supplied, then 10 items must appear on this line.')
              CALL WRTERR
            ELSE
              CLINE=CLINE(RW(5)+1:)
              CALL LINSPL(IFAIL,4,LW,RW,CLINE)
              CALL NUMRD(IFAIL,1,LW(1),RW(1),ITEMP,RSQR_ATOL,CLINE)
              IF(IFAIL.NE.0)THEN
                CALL RDERR(1,'ALT_LSQR_ATOL')
              ELSE
                IF(RSQR_ATOL.LT.0.0D0)THEN
                  WRITE(ERRMSG,31307) AALINE(1:MMLINE)
31307             FORMAT(A,'alternative LSQR_ATOL must not be less ',
     +            'than zero.')
                  CALL WRTERR
                END IF
              END IF
              CALL NUMRD(IFAIL,1,LW(2),RW(2),ITEMP,RSQR_BTOL,CLINE)
              IF(IFAIL.NE.0)THEN
                CALL RDERR(1,'ALT_LSQR_BTOL')
              ELSE
                IF(RSQR_BTOL.LT.0.0D0)THEN
                  WRITE(ERRMSG,31308) AALINE(1:MMLINE)
31308             FORMAT(A,'alternative LSQR_BTOL must not be less ',
     +            'than zero.')
                  CALL WRTERR
                END IF
              END IF
              CALL NUMRD(IFAIL,1,LW(3),RW(3),ITEMP,RSQR_CONLIM,CLINE)
              IF(IFAIL.NE.0)THEN
                CALL RDERR(1,'ALT_LSQR_CONLIM')
              ELSE
                IF(RSQR_CONLIM.LT.0.0D0)THEN
                  WRITE(ERRMSG,31309) AALINE(1:MMLINE)
31309             FORMAT(A,'alternative LSQR_CONLIM must not be ',
     +            'less than zero.')
                  CALL WRTERR
                END IF
              END IF
              CALL NUMRD(IFAIL,0,LW(4),RW(4),RSQR_ITNLIM,RTEMP,CLINE)
              IF(IFAIL.NE.0)THEN
                CALL RDERR(0,'ALT_LSQR_ITNLIM')
              ELSE
                IF(RSQR_ITNLIM.LE.0)THEN
                  WRITE(ERRMSG,31310) AALINE(1:MMLINE)
31310             FORMAT(A,'alternative LSQR_ITNLIM must be greater ',
     +            'than zero.')
                  CALL WRTERR
                END IF
              END IF
              CALL NUMRD(IFAIL,0,LW(5),RW(5),LSQR_STOPITER,RTEMP,CLINE)
              IF(IFAIL.NE.0)THEN
                CALL RDERR(0,'LSQR_STOPITER')
              END IF
            END IF
          END IF

7614      CONTINUE
          ILINE=ILINE+1
          CALL WRTINT(ATEMP,ILINE)
          ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
          AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
          MMLINE=NBLNK(AALINE)+1
          READ(30,'(A)',ERR=9600,END=9700) CLINE
          CALL PPROC(CLINE,IPPCOUNT)
          IF(CLINE.EQ.' ') GO TO 7614
          CALL LINSPL(IFAIL,1,LW,RW,CLINE)
          IF(IFAIL.NE.0) GO TO 9500
          CALL NUMRD(IFAIL,0,LW(1),RW(1),LSQRWRITE,RTEMP,CLINE)
          IF(IFAIL.NE.0)THEN
            CALL RDERR(0,'LSQRWRITE')
          ELSE
            IF((LSQRWRITE.NE.0).AND.(LSQRWRITE.NE.1))THEN
              WRITE(ERRMSG,31305) AALINE(1:MMLINE)
31305         FORMAT(A,'LSQRWRITE must be zero or one.')
              CALL WRTERR
            END IF
          END IF

31419     ILINE=ILINE+1
          CALL WRTINT(ATEMP,ILINE)
          ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
          AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
          MMLINE=NBLNK(AALINE)+1
          READ(30,'(A)',ERR=9600,END=9700) CLINE
          CALL PPROC(CLINE,IPPCOUNT)
          IF(CLINE.EQ.' ') GO TO 31419
          CALL LOWCAS(CLINE)
          CALL SHIFTL(CLINE)
31418     IF(CLINE(1:1).NE.'*')THEN
            WRITE(ERRMSG,632) AALINE(1:MMLINE)
            CALL WRTERR
            GO TO 9800
          END IF
          IF(INDEX(CLINE,'group').NE.0)GO TO 649
          IF(INDEX(CLINE,'svd assist').NE.0) THEN
            CALL SVDACHEK(IFAIL,ILINE,IASECT,DOAUI,NUMCOM,JACFILE,CLINE,
     +      SVDA_MULBPA,SVDA_NLSCALADJ,SVDA_EXTSUPER,SVDA_SUPDERCALC,
     +      SVDA_PAR_EXCL,PESTMODE,IPPCOUNT)
            IF(IFAIL.EQ.1)THEN
              GO TO 9600
            ELSE IF(IFAIL.EQ.2) THEN
              GO TO 9700
            ELSE IF(IFAIL.EQ.3)THEN
              GO TO 9800
            END IF
            IF(IERR.NE.0) GO TO 9800
            ISVDA=1
            IF(SVDA_MULBPA.EQ.-999)THEN
              SVDA_MULBPA=0
              SVDA_NLSCALADJ=0
              GO TO 31418
            ELSE
              GO TO 31419
            END IF
          END IF
        END IF

32000   CONTINUE
        IF(INDEX(CLINE,'sensitivity reuse').NE.0)THEN
          IF(ISVDA.EQ.1)THEN
            WRITE(ERRMSG,32010)
32010       FORMAT(' "sensitivity reuse" section must ',
     +      'precede "svd assist" section in PEST control file.')
            CALL WRTERR
          END IF
          IF(ISENREUSE.EQ.1)THEN
            WRITE(ERRMSG,31453) 'sensitivity reuse'
            CALL WRTERR
          END IF
7616      CONTINUE
          ILINE=ILINE+1
          CALL WRTINT(ATEMP,ILINE)
          ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
          AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
          MMLINE=NBLNK(AALINE)+1
          READ(30,'(A)',ERR=9600,END=9700) CLINE
          CALL PPROC(CLINE,IPPCOUNT)
          IF(CLINE.EQ.' ') GO TO 7616
          CALL LINSPL(IFAIL,2,LW,RW,CLINE)
          IF(IFAIL.NE.0) GO TO 9500
          ISENREUSE=1
          CALL NUMRD(IFAIL,1,LW(1),RW(1),ITEMP,SENRELTHRESH,CLINE)
          IF(IFAIL.NE.0)THEN
            CALL RDERR(1,'SENRELTHRESH')
          ELSE
            IF(SENRELTHRESH.LT.0.0D0)THEN
              WRITE(ERRMSG,32030) AALINE(1:MMLINE)
32030         FORMAT(A,'SENRELTHRESH must not be less than zero.')
              CALL WRTERR
            END IF
          END IF
          CALL NUMRD(IFAIL,0,LW(2),RW(2),SENMAXREUSE,RTEMP,CLINE)
          IF(IFAIL.NE.0)THEN
            CALL RDERR(0,'SENMAXREUSE')
          ELSE
            IF(SENMAXREUSE.EQ.0)THEN
              WRITE(ERRMSG,32040) AALINE(1:MMLINE)
32040         FORMAT(A,'SENMAXREUSE must not zero.')
              CALL WRTERR
            END IF
          END IF
7617      CONTINUE
          ILINE=ILINE+1
          CALL WRTINT(ATEMP,ILINE)
          ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
          AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
          MMLINE=NBLNK(AALINE)+1
          READ(30,'(A)',ERR=9600,END=9700) CLINE
          CALL PPROC(CLINE,IPPCOUNT)
          IF(CLINE.EQ.' ') GO TO 7617
          CALL LINSPL(IFAIL,3,LW,RW,CLINE)
          IF(IFAIL.NE.0) GO TO 9500
          CALL NUMRD(IFAIL,0,LW(1),RW(1),SENALLCALCINT,RTEMP,CLINE)
          IF(IFAIL.NE.0)THEN
            CALL RDERR(0,'SENALLCALCINT')
          END IF
          IF(SENALLCALCINT.LE.1)THEN
            WRITE(ERRMSG,32050) AALINE(1:MMLINE)
32050       FORMAT(A,'SENALLCALCINT must be greater than one.')
            CALL WRTERR
          END IF
          CALL NUMRD(IFAIL,1,LW(2),RW(2),ITEMP,SENPREDWEIGHT,CLINE)
          IF(IFAIL.NE.0)THEN
            CALL RDERR(1,'SENPREDWEIGHT')
          END IF
          SENPIEXCLUDE=CLINE(LW(3):RW(3))
          SENPIEXCLUDE=ADJUSTL(SENPIEXCLUDE)
          CALL LOWCAS(SENPIEXCLUDE)
          IF((SENPIEXCLUDE.NE.'yes').AND.(SENPIEXCLUDE.NE.'no'))THEN
            WRITE(ERRMSG,32060) AALINE(1:MMLINE)
32060       FORMAT(A,'SENPIEXCLUDE must be yes or no (no quotes).')
            CALL WRTERR
          END IF

31070     ILINE=ILINE+1
          CALL WRTINT(ATEMP,ILINE)
          ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
          AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
          MMLINE=NBLNK(AALINE)+1
          READ(30,'(A)',ERR=9600,END=9700) CLINE
          CALL PPROC(CLINE,IPPCOUNT)
          IF(CLINE.EQ.' ') GO TO 31070
          CALL LOWCAS(CLINE)
          CALL SHIFTL(CLINE)
31080     IF(CLINE(1:1).NE.'*')THEN
            WRITE(ERRMSG,632) AALINE(1:MMLINE)
            CALL WRTERR
            GO TO 9800
          END IF
          IF(INDEX(CLINE,'group').NE.0)GO TO 649
          IF(INDEX(CLINE,'svd assist').NE.0) THEN
            CALL SVDACHEK(IFAIL,ILINE,IASECT,DOAUI,NUMCOM,JACFILE,CLINE,
     +      SVDA_MULBPA,SVDA_NLSCALADJ,SVDA_EXTSUPER,SVDA_SUPDERCALC,
     +      SVDA_PAR_EXCL,PESTMODE,IPPCOUNT)
            IF(IFAIL.EQ.1)THEN
              GO TO 9600
            ELSE IF(IFAIL.EQ.2) THEN
              GO TO 9700
            ELSE IF(IFAIL.EQ.3)THEN
              GO TO 9800
            END IF
            IF(IERR.NE.0) GO TO 9800
            ISVDA=1
            IF(SVDA_MULBPA.EQ.-999)THEN
              SVDA_MULBPA=0
              SVDA_NLSCALADJ=0
              GO TO 31080
            ELSE
              GO TO 31070
            END IF
          END IF
        END IF

33000   CONTINUE
        IF((INDEX(CLINE,'simultaneous').NE.0).OR.
     +     (INDEX(CLINE,'random').NE.0).OR.
     +     (INDEX(CLINE,'rsi ').NE.0))THEN
          IF(INDEX(CLINE,'simul').NE.0)THEN
            IF(SIMINCFLAG.EQ.1)THEN
              WRITE(ERRMSG,31453)
     +        'simultaneous parameter increments'
              CALL WRTERR
            END IF
            SIMINCFLAG=1
          ELSE IF(INDEX(CLINE,'random').NE.0)THEN
            IF(RANDOMFLAG.EQ.1)THEN
              WRITE(ERRMSG,31453) 'randomized jacobian'
              CALL WRTERR
            END IF
            RANDOMFLAG=1
          ELSE IF(INDEX(CLINE,'rsi ').NE.0)THEN
            IF(RSIFLAG.EQ.1)THEN
              WRITE(ERRMSG,31453) 'rsi'
              CALL WRTERR
            END IF
            RSIFLAG=1
          END IF
          NNLINE=5
          IF(INDEX(CLINE,'rsi ').NE.0) NNLINE=12
!          DO I=1,NNLINE
          DO
33001       CONTINUE
            ILINE=ILINE+1
            CALL WRTINT(ATEMP,ILINE)
            ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
            AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
            MMLINE=NBLNK(AALINE)+1
            READ(30,'(A)',ERR=9600,END=9700) CLINE
            CALL PPROC(CLINE,IPPCOUNT)
            IF(CLINE.EQ.' ') GO TO 33001
            IF(CLINE(1:1).EQ.'*') GO TO 33004
          END DO
33002     CONTINUE
          ILINE=ILINE+1
          CALL WRTINT(ATEMP,ILINE)
          ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
          AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
          MMLINE=NBLNK(AALINE)+1
          READ(30,'(A)',ERR=9600,END=9700) CLINE
          CALL PPROC(CLINE,IPPCOUNT)
          IF(CLINE.EQ.' ') GO TO 33002
          CALL LOWCAS(CLINE)
          CALL SHIFTL(CLINE)
33003     IF(CLINE(1:1).NE.'*')THEN
            WRITE(ERRMSG,632) AALINE(1:MMLINE)
            CALL WRTERR
            GO TO 9800
          END IF
33004     CONTINUE
          IF(INDEX(CLINE,'group').NE.0)GO TO 649
          IF(INDEX(CLINE,'svd assist').NE.0) THEN
            CALL SVDACHEK(IFAIL,ILINE,IASECT,DOAUI,NUMCOM,JACFILE,CLINE,
     +      SVDA_MULBPA,SVDA_NLSCALADJ,SVDA_EXTSUPER,SVDA_SUPDERCALC,
     +      SVDA_PAR_EXCL,PESTMODE,IPPCOUNT)
            IF(IFAIL.EQ.1)THEN
              GO TO 9600
            ELSE IF(IFAIL.EQ.2) THEN
              GO TO 9700
            ELSE IF(IFAIL.EQ.3)THEN
              GO TO 9800
            END IF
            IF(IERR.NE.0) GO TO 9800
            ISVDA=1
            IF(SVDA_MULBPA.EQ.-999)THEN
              SVDA_MULBPA=0
              SVDA_NLSCALADJ=0
              GO TO 33003
            ELSE
              GO TO 33002
            END IF
          END IF
        END IF

        IF(INDEX(CLINE,'singular value').NE.0) GO TO 31458
        IF(INDEX(CLINE,'lsqr').NE.0) GO TO 31449
        IF(INDEX(CLINE,'sensitivity reuse').NE.0) go to 32000
        IF(INDEX(CLINE,'randomized jacobian').NE.0) GO TO 33000
        IF(INDEX(CLINE,'randomised jacobian').NE.0) GO TO 33000
        IF(INDEX(CLINE,'simultaneous par').NE.0) GO TO 33000
        IF(INDEX(CLINE,'rsi ').NE.0) GO TO 33000
        IF(INDEX(CLINE,'automatic user intervention').EQ.0)THEN
          WRITE(ERRMSG,633) AALINE(1:MMLINE)
633       FORMAT(A,'"parameter groups", "automatic user ',
     +    'intervention", "singular value decomposition", ',
     +    '"lsqr", "sensitivity reuse", "simultaneous parameter ',
     +    'increments", "randomized jacobian", "rsi" or "svd assist" ',
     +    'section header expected.')
          CALL WRTERR
          GO TO 9800
        END IF
        IF(DOAUI.EQ.'AAAA')THEN
          WRITE(ERRMSG,634) AALINE(1:MMLINE)
634       FORMAT(A,'an "automatic user intervention" section is ',
     +    'present in this file - however no value is supplied for ',
     +    'variable DOAUI in the "control data" section.')
          CALL WRTERR
        END IF
        IF(ISVDA.NE.0)THEN
          WRITE(ERRMSG,635)
635       FORMAT(' "svd assist" section must not precede "automatic ',
     +    'user intervention" section in PEST control file.')
          CALL WRTERR
        END IF

        IASECT=1
7618    CONTINUE
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7618
        CALL LINSPL(IFAIL,4,LW,RW,CLINE)
        IF(IFAIL.NE.0) GO TO 9500
        CALL NUMRD(IFAIL,0,LW(1),RW(1),MAXAUI,RTEMP,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(0,'MAXAUI')
        ELSE
          IF(MAXAUI.LT.0)THEN
            CALL NOTNEG(0,'MAXAUI')
          ELSE
            IF(MAXAUI.GE.NPAR)THEN
              WRITE(ERRMSG,7011) AALINE(1:MMLINE)
7011          FORMAT(A,'MAXAUI must be less than the number of ',
     +        'parameters - try using half the number of non-tied ',
     +        'plus non-fixed parameters.')
              CALL WRTERR
            END IF
          END IF
        END IF
        CALL NUMRD(IFAIL,0,LW(2),RW(2),AUIOPT,RTEMP,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(0,'AUISTARTOPT')
        ELSE
          IF(AUIOPT.LE.0)THEN
            CALL ZRONEG(0,'AUISTARTOPT')
          END IF
        END IF
        CALL NUMRD(IFAIL,1,LW(3),RW(3),ITEMP,NOAUIPHIRAT,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(1,'NOAUIPHIRAT')
        ELSE
          IF((NOAUIPHIRAT.GE.1.0).OR.(NOAUIPHIRAT.LE.0.0))THEN
            WRITE(ERRMSG,7021) AALINE(1:MMLINE)
7021        FORMAT(A,'NOAUIPHIRAT must be less than one and ',
     +      'greater than zero - try 0.9.')
            CALL WRTERR
          END IF
        END IF
        CALL NUMRD(IFAIL,0,LW(4),RW(4),AUIREST,RTEMP,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(0,'AUIRESTITN')
        ELSE
          IF(AUIREST.LT.0)THEN
            WRITE(ERRMSG,7030) AALINE(1:MMLINE)
7030        FORMAT(A,'integer variable AUIRESTITN must not be ',
     +      'less than zero - try using zero.')
            CALL WRTERR
          ELSE IF(AUIREST.EQ.1)THEN
            WRITE(ERRMSG,7040) AALINE(1:MMLINE)
7040        FORMAT(A,'AUIRESTITN can be zero or greater than one - ',
     +      'however it must not be one - try using zero.')
            CALL WRTERR
          END IF
        END IF

7619    CONTINUE
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7619
        CALL LINSPL(IFAIL,3,LW,RW,CLINE)
        IF(IFAIL.NE.0) GO TO 9500
        CALL NUMRD(IFAIL,1,LW(1),RW(1),ITEMP,AUISENSRAT,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(1,'AUISENSRAT')
        ELSE
          IF(DOAUI.EQ.'aui')THEN
            IF(AUISENSRAT.LE.1.0)THEN
              WRITE(ERRMSG,7050) AALINE(1:MMLINE)
7050          FORMAT(A,'real variable AUISENSRAT must be greater than ',
     +        'one - try 50.0.')
              CALL WRTERR
            END IF
          ELSE IF(DOAUI.EQ.'auid')THEN
            IF(AUISENSRAT.LT.1.0)THEN
              WRITE(ERRMSG,7051) AALINE(1:MMLINE)
7051          FORMAT(A,'real variable AUISENSRAT must be one or ',
     +        'greater.')
              CALL WRTERR
            END IF
          END IF
        END IF
        CALL NUMRD(IFAIL,0,LW(2),RW(2),AUIHOLDMAXCHG,RTEMP,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(0,'AUIHOLDMAXCHG')
        ELSE
          IF((AUIHOLDMAXCHG.NE.0).AND.(AUIHOLDMAXCHG.NE.1))THEN
            WRITE(ERRMSG,7055) AALINE(1:MMLINE)
7055        FORMAT(A,'integer variable AUIHOLDMAXCHG must be one ',
     +      'or zero - try zero.')
            CALL WRTERR
          END IF
        END IF
        CALL NUMRD(IFAIL,0,LW(3),RW(3),AUINUMFREE,RTEMP,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(0,'AUINUMFREE')
        ELSE
          IF((AUINUMFREE.LE.0).OR.(AUINUMFREE.GE.NPAR))THEN
            IF(DOAUI.EQ.'aui')THEN
              WRITE(ERRMSG,7060) AALINE(1:MMLINE)
7060          FORMAT(A,'integer variable AUINUMFREE must be greater ',
     +        'than zero and less than the number of adjustable ',
     +        'parameters - try 3 ',
     +        'unless this is greater than or equal to the number of ',
     +        'adjustable parameters.')
              CALL WRTERR
            ELSE IF(DOAUI.EQ.'auid')THEN
              WRITE(ERRMSG,7061) AALINE(1:MMLINE)
7061          FORMAT(A,'integer variable AUINUMFREE must be greater ',
     +        'than zero and less than the number of adjustable ',
     +        'parameters.')
              CALL WRTERR
            END IF
          END IF
        END IF

7621    CONTINUE
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7621
        CALL LINSPL(IFAIL,3,LW,RW,CLINE)
        IF(IFAIL.NE.0) GO TO 9500
        CALL NUMRD(IFAIL,1,LW(1),RW(1),ITEMP,ALAMPSI,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(1,'AUIPHIRATSUF')
        ELSE
          IF((ALAMPSI.GE.1.0).OR.(ALAMPSI.LE.0.0))THEN
            WRITE(ERRMSG,7070) AALINE(1:MMLINE)
7070        FORMAT(A,'real variable AUIPHIRATSUF must be less than ',
     +      'one and greater than zero - try 0.8')
            CALL WRTERR
          END IF
        END IF
        CALL NUMRD(IFAIL,1,LW(2),RW(2),ITEMP,APHIRATACC,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(1,'AUIPHIRATACCEPT')
        ELSE
          IF(DOAUI.EQ.'aui')THEN
          IF((APHIRATACC.GE.1.0).OR.(APHIRATACC.LE.0.0))THEN
            WRITE(ERRMSG,7080) AALINE(1:MMLINE)
7080        FORMAT(A,'real variable AUIPHIRATACCEPT must be less ',
     +      'than one and greater than zero - try 0.95')
            CALL WRTERR
          ELSE IF(APHIRATACC.LE.ALAMPSI)THEN
            WRITE(ERRMSG,7090) AALINE(1:MMLINE)
7090        FORMAT(A,'AUIPHIRATACCEPT must be larger than ',
     +      'AUIPHIRATSUF - try 0.8 for AUIPHIRATSUF and ',
     +      '0.975 for AUIPHIRATACCEPT.')
            CALL WRTERR
          END IF
          ELSE IF(DOAUI.EQ.'auid')THEN
          IF((APHIRATACC.GE.1.0).OR.(APHIRATACC.LE.0.0))THEN
            WRITE(ERRMSG,7081) AALINE(1:MMLINE)
7081        FORMAT(A,'real variable AUIPHIRATACCEPT must be less ',
     +      'than one and greater than zero.')
            CALL WRTERR
          ELSE IF(APHIRATACC.LE.ALAMPSI)THEN
            WRITE(ERRMSG,7091) AALINE(1:MMLINE)
7091        FORMAT(A,'AUIPHIRATACCEPT must be larger than ',
     +      'AUIPHIRATSUF - try 0.8 for AUIPHIRATSUF and ',
     +      '0.99 for AUIPHIRATACCEPT.')
            CALL WRTERR
          END IF
          END IF
        END IF
        CALL NUMRD(IFAIL,0,LW(3),RW(3),NAUINOACC,RTEMP,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(0,'NAUINOACCEPT')
        ELSE
          IF(NAUINOACC.LT.1)THEN
            WRITE(ERRMSG,7100) AALINE(1:MMLINE)
7100        FORMAT(A,'integer variable NAUINOACCEPT must be greater ',
     +      'than zero - try a third of the number of adjustable ',
     +      'parameters.')
            CALL WRTERR
          ELSE IF(NAUINOACC.GE.NPAR)THEN
            WRITE(ERRMSG,7101) AALINE(1:MMLINE)
7101        FORMAT(A,'integer variable NAUINOACCEPT must be less',
     +      ' than the number of parameters - try one third of the ',
     +      'number of non-fixed plus non-tied parameters.')
            CALL WRTERR
          END IF
        END IF

C -- THE PARAMETER GROUPS SECTION OF THE PEST CONTROL FILE IS READ AND
C -- CHECKED

647     CONTINUE
        ILINE=ILINE+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 647
        CALL LOWCAS(CLINE)
        CALL SHIFTL(CLINE)
649     IF((CLINE.NE.'* group definitions and derivative data').AND.
     +     (CLINE.NE.'* parameter groups')) THEN
          IF(CLINE(1:13).EQ.'* singular va')THEN
            WRITE(ERRMSG,648)
648         FORMAT(' "singular value decomposition" section must not ',
     +      'follow "automatic user intervention" section.')
            CALL WRTERR
            GO TO 9800
          END IF
          IF(CLINE(1:16).EQ.'* sensitivity re')THEN
            WRITE(ERRMSG,6481)
6481        FORMAT(' "sensitivity reuse" section must not ',
     +      'follow "automatic user intervention" section.')
            CALL WRTERR
            GO TO 9800
          END IF
          IF(INDEX(CLINE,'svd assist').NE.0) THEN
            CALL SVDACHEK(IFAIL,ILINE,IASECT,DOAUI,NUMCOM,JACFILE,
     +      CLINE,SVDA_MULBPA,SVDA_NLSCALADJ,SVDA_EXTSUPER,
     +      SVDA_SUPDERCALC,SVDA_PAR_EXCL,PESTMODE,IPPCOUNT)
            IF(IFAIL.EQ.1)THEN
              GO TO 9600
            ELSE IF(IFAIL.EQ.2) THEN
              GO TO 9700
            ELSE IF(IFAIL.EQ.3)THEN
              GO TO 9800
            END IF
            IF(IERR.NE.0) GO TO 9800
            ISVDA=1
            IF(SVDA_MULBPA.EQ.-999)THEN
              SVDA_MULBPA=0
              SVDA_NLSCALADJ=0
              GO TO 649
            ELSE
              GO TO 647
            END IF
          END IF
c          IF(IASECT.EQ.1)THEN
c            IF(INDEX(CLINE,'svd assist').NE.0)THEN
c              WRITE(ERRMSG,647)
c647           FORMAT(' "svd assist" section must not be present in ',
c     +        'same control file as "automatic user intervention" ',
c     +        'section.')
c              CALL WRTERR
c              GO TO 9800
c            END IF
c          END IF
          CALL WRTINT(ATEMP,ILINE)
          WRITE(6,650,ERR=9900) ATEMP(1:NBLNK(ATEMP)),CASFL(1:MCASE)
650       FORMAT(' Line ',A,' of file ',A,' should read:',
     +    /,' * parameter groups')
          GO TO 9800
        END IF

        IF(ISVDA.EQ.0)THEN
          IF(IBOUNDSCALE.NE.0)THEN
            IF((SVDMODE.EQ.0).AND.(LSQRMODE.EQ.0))THEN
              WRITE(ERRMSG,6471)
6471          FORMAT(' Parameter bounds scaling must not be activated ',
     +        'unless either singular value decomposition or LSQR is ',
     +        'also activated.')
              CALL WRTERR
            END IF
          END IF
        END IF

        DO 750 I=1,NPARGP
7622    CONTINUE
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7622
        CALL LOWCAS(CLINE)
        CALL SHIFTL(CLINE)
        IF(CLINE.EQ.'* parameter data')THEN
          CALL WRTINT(ATEMP,NPARGP)
          WRITE(ERRMSG,655)AALINE(1:MMLINE),ATEMP(1:INDEX(ATEMP,' ')-1)
655       FORMAT(A,'too few "parameter group" ',
     +    'lines - ',A,' expected.')
          CALL WRTERR
          GO TO 9800
        END IF
c        IF(((ACODE.EQ.'d').AND.(ACODEP.EQ.'d'))
c     +     .OR.((ACODE.EQ.'d').AND.(MODPRED.EQ.'t'))
c     +     .OR.((ACODE.EQ.'d').AND.(IPRED.EQ.0))) GO TO 750
        CALL LINSPL(IFAIL,7,LW,RW,CLINE)
        IF(IFAIL.NE.0) GO TO 9500
        IF(RW(1)-LW(1)+1.GT.12)THEN
          WRITE(ERRMSG,660) AALINE(1:MMLINE),
     +    CLINE(LW(1):RW(1))
660       FORMAT(A,'parameter group name "',a,'" greater ',
     +    'than 12 characters long.')
          CALL WRTERR
        END IF
        AGROUP(I)=CLINE(LW(1):RW(1))
        IF(AGROUP(I).EQ.'none') THEN
          WRITE(ERRMSG,665) AALINE(1:MMLINE)
665       FORMAT(A,'"none" is a reserved group name, used for some ',
     +    'fixed and tied parameters; no information can be provided ',
     +    'for this group.')
          CALL WRTERR
        END IF
        IF(CLINE(LW(2):RW(2)).EQ.'relative')THEN
          IDERIV(I)=1
        ELSE IF(CLINE(LW(2):RW(2)).EQ.'absolute')THEN
          IDERIV(I)=2
        ELSE IF(CLINE(LW(2):RW(2)).EQ.'rel_to_max')THEN
          IDERIV(I)=3
        ELSE
           WRITE(ERRMSG,670) AALINE(1:MMLINE)
670        FORMAT(A,'character variable INCTYP must be ',
     +     '"relative", "absolute" or "rel_to_max".')
           CALL WRTERR
        ENDIF
        CALL NUMRD(IFAIL,1,LW(3),RW(3),ITEMP,DERVAL(I),CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(1,'DERINC')
        ELSE
          IF(DERVAL(I).LE.0.0D0)THEN
            CALL ZRONEG(1,'DERINC')
          END IF
        END IF
        CALL NUMRD(IFAIL,1,LW(4),RW(4),ITEMP,DERBND(I),CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(1,'DERINCLB')
        ELSE
          IF(DERBND(I).LT.0.0D0)THEN
            WRITE(ERRMSG,680) AALINE(1:MMLINE)
680         FORMAT(A,'real variable DERINCLB negative.')
            CALL WRTERR
          END IF
        END IF
        IF(DERBND(I).LE.0.0D0)THEN
          IDBND(I)=0
        ELSE
          IDBND(I)=1
        END IF
        IF(CLINE(LW(5):RW(5)).EQ.'switch')THEN
          J2OR3(I)=1
          IF(PESTMODE.EQ.4)THEN
            WRITE(ERRMSG,682) AALINE(1:MMLINE)
682         FORMAT(A,'if PEST is run in pareto mode, character ',
     +      'variable FORCEN must be set to "always_2", "always_3" ',
     +      'or "always_5" for all parameter groups.')
            CALL WRTERR
          END IF
        ELSE IF(CLINE(LW(5):RW(5)).EQ.'always_2')THEN
          J2OR3(I)=2
        ELSE IF(CLINE(LW(5):RW(5)).EQ.'always_3')THEN
          J2OR3(I)=3
        ELSE IF(CLINE(LW(5):RW(5)).EQ.'switch_5')THEN
          J2OR3(I)=4
          IF(PESTMODE.EQ.4)THEN
            WRITE(ERRMSG,682) AALINE(1:MMLINE)
            CALL WRTERR
          END IF
        ELSE IF(CLINE(LW(5):RW(5)).EQ.'always_5')THEN
          J2OR3(I)=5
        ELSE
          WRITE(ERRMSG,690) AALINE(1:MMLINE)
690       FORMAT(A,'character variable FORCEN must be "switch",',
     +    ' "always_2", "always_3", "switch_5" or "always_5".')
          CALL WRTERR
          J2OR3(I)=0
        END IF
        CALL NUMRD(IFAIL,1,LW(6),RW(6),ITEMP,DERMUL(I),CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(1,'DERINCMUL')
        ELSE
          IF(DERMUL(I).LE.0.0D0) CALL ZRONEG(1,'DERINCMUL')
        END IF
        IF(CLINE(LW(7):RW(7)).EQ.'parabolic') THEN
          I3CALC(I)=1
        ELSE IF(CLINE(LW(7):RW(7)).EQ.'best_fit') THEN
          I3CALC(I)=2
        ELSE IF(CLINE(LW(7):RW(7)).EQ.'outside_pts') THEN
          I3CALC(I)=3
        ELSE IF(CLINE(LW(7):RW(7)).EQ.'minvar') THEN
          I3CALC(I)=4
        ELSE IF(CLINE(LW(7):RW(7)).EQ.'maxprec') THEN
          I3CALC(I)=5
        ELSE
          WRITE(ERRMSG,700) AALINE(1:MMLINE)
700       FORMAT(A,'character variable DERMTHD must be ',
     +    '"parabolic", "best_fit", "outside_pts", ',
     +    '"minvar" or "maxprec".')
          CALL WRTERR
          I3CALC(I)=0
        END IF
        IF((J2OR3(I).LE.3).AND.(J2OR3(I).GT.0))THEN
          IF(I3CALC(I).GE.4)THEN
            WRITE(ERRMSG,707) AALINE(1:MMLINE)
707         FORMAT(A,'if FORCEN is set to "switch", "always_2" ',
     +      'or "always_3", then DERTHMD must be set to "parabolic" ',
     +      '"best_fit" or "outside_pts".')
            CALL WRTERR
          END IF
        ELSE IF((J2OR3(I).EQ.4).OR.(J2OR3(I).EQ.5))THEN
          IF((I3CALC(I).EQ.1).OR.(I3CALC(I).EQ.2).OR.(I3CALC(I).EQ.3))
     +    THEN
            WRITE(ERRMSG,709) AALINE(1:MMLINE)
709         FORMAT(A,'if FORCEN is set to "switch_5" or "always_5", ',
     +      'then DERMTHD must be set to "minvar" or "maxprec".')
            CALL WRTERR
          END IF
        END IF

        CALL LINSPL(IFAIL,8,LW,RW,CLINE)
        IF(IFAIL.EQ.0) THEN
          CALL LINSPL(IFAIL,9,LW,RW,CLINE)
          IF(IFAIL.NE.0)THEN
            WRITE(ERRMSG,701) AALINE(1:MMLINE)
701         FORMAT(A,'each line in the "parameter groups" section of a ',
     +      'PEST control file must have either 7 or 10 entries.')
            CALL WRTERR
            GO TO 750
          END IF
          CALL LINSPL(IFAIL,10,LW,RW,CLINE)
          IF(IFAIL.NE.0)THEN
            WRITE(ERRMSG,701) AALINE(1:MMLINE)
            CALL WRTERR
            GO TO 750
          END IF
          CALL NUMRD(IFAIL,1,LW(8),RW(8),ITEMP,SPLITTHRESH(I),CLINE)
          IF(IFAIL.NE.0) THEN
            CALL RDERR(1,'SPLITTHRESH')
          ELSE
            IF(SPLITTHRESH(I).LT.0.0D0) THEN
              WRITE(ERRMSG,702) AALINE(1:MMLINE)
702           FORMAT(A,'SPLITTHRESH must be zero or greater.')
              CALL WRTERR
            ELSE IF(SPLITTHRESH(I).GT.0.0D0)THEN
              IF(J2OR3(I).GE.4)THEN
                WRITE(ERRMSG,708) AALINE(1:MMLINE)
708             FORMAT(A,'if FORCEN is set to "switch_5" ',
     +          'or "always_5", SPLITTHRESH, if present, must be ',
     +          'set to zero.')
                CALL WRTERR
              END IF
            END IF
          END IF
          CALL NUMRD(IFAIL,1,LW(9),RW(9),ITEMP,SPLITRELDIFF(I),CLINE)
          IF(IFAIL.NE.0)THEN
            CALL RDERR(1,'SPLITRELDIFF')
          ELSE
            IF(SPLITRELDIFF(I).LE.0.0D0)THEN
              WRITE(ERRMSG,703) AALINE(1:MMLINE)
703           FORMAT(A,'SPLITRELDIFF must not be zero or negative.')
              CALL WRTERR
            END IF
          END IF
          ATEMP=CLINE(LW(10):RW(10))
          IF(ATEMP(1:5).EQ.'small')THEN
            SPLITACTION(I)=1
          ELSE IF(ATEMP.EQ.'zero')THEN
            SPLITACTION(I)=2
          ELSE IF(ATEMP.EQ.'previous')THEN
            SPLITACTION(I)=3
          ELSE
            WRITE(ERRMSG,704) AALINE(1:MMLINE)
704         FORMAT(A,'character variable SPLITACTION must be ',
     +      '"smaller", "zero" or "previous".')
            CALL WRTERR
          END IF
        ELSE
          SPLITTHRESH(I)=-1.1D35
          SPLITRELDIFF(I)=-1.1D35
          SPLITACTION(I)=0
        END IF
750     CONTINUE
c        IF(((ACODE.EQ.'d').AND.(ACODEP.EQ.'d')).OR.
c     +     ((ACODE.EQ.'d').AND.(MODPRED.EQ.'t')).OR.
c     +     ((ACODE.EQ.'d').AND.(IPRED.EQ.0))) GO TO 761
        IF(NPARGP.EQ.1) GO TO 761
        DO 760 I=1,NPARGP-1
        DO 760 J=I+1,NPARGP
        IF(AGROUP(I).EQ.AGROUP(J))THEN
          WRITE(ERRMSG,755) AGROUP(I)(:NBLNK(AGROUP(I)))
755       FORMAT(' Multiple parameter groups with the same name ',
     +    '("',A,'").')
          CALL WRTERR
          GO TO 761
        END IF
760     CONTINUE
761     CONTINUE
        IF(IERR.NE.0) GO TO 9800

C -- THE PARAMETER DATA SECTION OF THE PEST CONTROL FILE IS READ AND
C -- CHECKED

        IF((ISVDA.EQ.1).AND.(WRITE_RRF.EQ.1))THEN
          WRITE(ERRMSG,6321)
6321      FORMAT(' PEST cannot write a run results file recording ',
     +    'base parameters when it is estimating super parameters ',
     +    'using SVD-assist.')
          CALL WRTERR
        END IF

7623    CONTINUE
        ILINE=ILINE+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7623
        CALL LOWCAS(CLINE)
        CALL SHIFTL(CLINE)
        IF(CLINE.NE.'* parameter data') THEN
          CALL WRTINT(ATEMP,ILINE)
          WRITE(6,800,ERR=9900) ATEMP(1:INDEX(ATEMP,' ')-1),
     +    CASFL(1:MCASE)
800       FORMAT(' Line ',A,' of file ',A,' should read:',
     +    /,' * parameter data')
          GO TO 9800
        END IF

        CALL PRMCHK(ACODE,ACODEP,MODPRED,IFAIL,NPAR,NPARGP,ILINE,
     +  DMAX0,LBOUND,UBOUND,IPARGP,IDERIV,IDBND,DERVAL,DERBND,
     +  ITRANS,CHGTYP,LW,RW,PVAL,AGROUP,APAR,CLINE,ATEMP,PRECIS,
     +  JACFILE,NUMCOM,JCWARN,DERCOM,ISVDA,MAXABSPARMAX,ABSPARMAX,
     +  IREFOBS,IPPCOUNT,NPARSEC,NPARFILE,NEQUATION)
        IF(IFAIL.EQ.1) GO TO 9800
        IF(IFAIL.EQ.2) GO TO 890
        IF(IFAIL.EQ.3) GO TO 9500
        IF(IFAIL.EQ.4) GO TO 9600
        IF(IFAIL.EQ.5) GO TO 9700

C -- IF NECESSARY, SOME CHECKING OF ADAPTIVE REGULARISATION FUNCTIONALITY
C    IS UNDERTAKEN.

        DO I=1,NPAR
          IF(APAR(I)(1:3).EQ.'iw_')THEN
            IF(IREG.NE.1)THEN
              WRITE(ERRMSG,3320) CASFL(1:MCASE),CASFL(1:MCASE)
3320          FORMAT(' File ',A,' has at least one parameter ',
     +        'whose name begins with "iw_"; these parameter ',
     +        'names are reserved for PEST''s adaptive ',
     +        'regularisation functionality. This is incompatible ',
     +        'with the fact that file ',A,' does not request ',
     +        'PEST to run in regularisation mode.')
              CALL WRTERR
              GO TO 3390
            END IF
            GO TO 3400
          END IF
        END DO
3390    NOMOREIW_CHECK=1
3400    CONTINUE

C -- THE OBSERVATION GROUPS SECTION OF THE PEST CONTROL FILE IS READ AND
C -- CHECKED

7624    CONTINUE
        ILINE=ILINE+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7624
890     CALL LOWCAS(CLINE)
        CALL SHIFTL(CLINE)
        IF(NOBSGP1.NE.-999)THEN
          IF(CLINE.NE.'* observation groups') THEN
            CALL WRTINT(ATEMP,ILINE)
            WRITE(6,900,ERR=9900) ATEMP(1:INDEX(ATEMP,' ')-1),
     +      CASFL(1:MCASE)
900         FORMAT(' Line ',A,' of file ',A,' should read:',/,
     +      ' * observation groups')
            GO TO 9800
          END IF
        ELSE
          IF(INDEX(CLINE,'groups').NE.0)THEN
            WRITE(ERRMSG,920) CASFL(1:MCASE),CASFL(1:MCASE)
920         FORMAT(' File ',A,' appears to have an " * observation ',
     +      'groups" section. However no value was supplied for ',
     +      'NOBSGP on line 4 of file ',A,'.')
            CALL WRTERR
            GO TO 9800
          END IF
          GO TO 1240
        END IF

1000    DO 1100 I=1,NOBSGP
          AOBSGP(I)='{{{'
7626      CONTINUE
          ILINE=ILINE+1
          CALL WRTINT(ATEMP,ILINE)
          ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
          AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
          MMLINE=NBLNK(AALINE)+1
          READ(30,'(A)',ERR=9600,END=9700) CLINE
          CALL PPROC(CLINE,IPPCOUNT)
          IF(CLINE.EQ.' ') GO TO 7626
          CALL SHIFTL(CLINE)
          DLINE=CLINE
          CALL LOWCAS(CLINE)
          IF(CLINE.EQ.'* observation data')THEN
            CALL WRTINT(ATEMP,NOBSGP)
            WRITE(ERRMSG,1110) AALINE(1:MMLINE),ATEMP(1:NBLNK(ATEMP))
1110        FORMAT(A,'too few "observation group" lines - ',A,
     +      ' expected.')
            CALL WRTERR
            GO TO 1300
          END IF
          CALL LINSPL(IFAIL,1,LW,RW,CLINE)
          IF(IFAIL.NE.0) GO TO 9500
          IF(RW(1)-LW(1)+1.GT.12) THEN
            WRITE(ERRMSG,1130) AALINE(1:MMLINE),CLINE(LW(1):RW(1))
1130        FORMAT(A,'observation group name "',A,'" greater than 12 ',
     +      'characters long.')
            CALL WRTERR
            GO TO 1100
          END IF
          AOBSGP(I)=CLINE(LW(1):RW(1))
          IF(I.GE.2)THEN
            DO 1150 J=1,I-1
              IF(AOBSGP(I).EQ.AOBSGP(J))THEN
                WRITE(ERRMSG,1140) AALINE(1:MMLINE),
     +          AOBSGP(I)(:NBLNK(AOBSGP(I)))
1140            FORMAT(A,'observation group name "',A,'" used more ',
     +          'than once.')
                CALL WRTERR
                GO TO 1151
              END IF
1150        CONTINUE
          END IF
1151      CONTINUE
          CALL SPACESUB(DLINE)
          CALL LINSPL(IFAIL,2,LW,RW,DLINE)
          IF(IFAIL.NE.0)THEN
            COVFLE(I)=' '
          ELSE
            COVFLE(I)=DLINE(LW(2):RW(2))
            CALL NUMRD(IFAIL,1,LW(2),RW(2),ITEMP,GTARG(I),DLINE)
            IF(IFAIL.NE.0)THEN
              GTARG(I)=-1.1D35
              CALL REMCHAR(COVFLE(I),CHAR(211))
              NUMCOV=NUMCOV+1
#ifdef CAPFILE
              CALL UPCAS(COVFLE(I))
#endif
            ELSE
              CALL LINSPL(IFAIL,3,LW,RW,DLINE)
              IF(IFAIL.NE.0)THEN
                COVFLE(I)=' '
              ELSE
                COVFLE(I)=DLINE(LW(3):RW(3))
                CALL REMCHAR(COVFLE(I),CHAR(211))
                NUMCOV=NUMCOV+1
#ifdef CAPFILE
                CALL UPCAS(COVFLE(I))
#endif
              END IF
            END IF
          END IF
          IF(GTARG(I).GT.-1.0D35)THEN
            IF(IREG.NE.1)THEN
              WRITE(ERRMSG,1152) AALINE(1:MMLINE)
1152          FORMAT(A,'a group-specific target measurement ',
     +        'objective function cannot be supplied to an ',
     +        'observation group unless PEST is run in ',
     +        'regularisation mode.')
              CALL WRTERR
            ELSE
              IF(AOBSGP(I)(1:5).EQ.'regul')THEN
                WRITE(ERRMSG,1153) AALINE(1:MMLINE)
1153            FORMAT(A,'a group-specific target measurement ',
     +          'objective function must not be supplied for a ',
     +          'regularisation group.')
                CALL WRTERR
              ELSE
                IF(GTARG(I).LE.0.0D0)THEN
                  WRITE(ERRMSG,1154) AALINE(1:MMLINE)
1154              FORMAT(A,'a group-specific target measurement ',
     +            'objective function must not be zero or negative.')
                  CALL WRTERR
                ELSE
                  IGTARG=IGTARG+1
                END IF
              END IF
            END IF
          END IF
1100    CONTINUE
        IF((MAXCOMPDIM.GT.1).AND.(NUMCOV.GT.0))THEN
          WRITE(ERRMSG,1101)
1101      FORMAT(' If MAXCOMPDIM is greater than 1 (and hence ',
     +    'Jacobian matrix compression is activated), the use ',
     +    'of observation covariance matrices is disallowed.')
          CALL WRTERR
        END IF
        IF(IPRED.NE.0)THEN
          DO 1105  I=1,NOBSGP
            IF(AOBSGP(I).EQ.'predict') THEN
              JPREDICT=I
              GO TO 1170
            END IF
1105      CONTINUE
          WRITE(ERRMSG,1120)
1120      FORMAT(' If PEST is run in predictive analysis mode then ',
     +    'one obervation group must be named "predict".')
          CALL WRTERR
1170      CONTINUE
        END IF
        DO 1171 I=1,NOBSGP
          IRGP(I)=0
1171    CONTINUE
        IF(IREG.NE.0)THEN
          DO 1180 I=1,NOBSGP
            IF(AOBSGP(I)(1:5).EQ.'regul')THEN
              IRGP(I)=1
              JREGOBS=I
            END IF
1180      CONTINUE
          IF(JREGOBS.EQ.0)THEN
          WRITE(ERRMSG,1185)
1185      FORMAT(' If PEST is run in regularisation mode then at ',
     +    'least one observation group must be a regularisation group;',
     +    ' that is, the first five letters of the name of at least ',
     +    'one group must be "regul".')
          CALL WRTERR
          END IF
1190      CONTINUE
        END IF

        IF(IGTARG.NE.0)THEN
          DO I=1,NOBSGP
            IF(IRGP(I).NE.1)THEN
              IF(GTARG(I).LT.-1.0D35)THEN
                WRITE(ERRMSG,1191)
1191            FORMAT(' A group-specific target measurement ',
     +          'objective function must be supplied for all ',
     +          'non-regularisation observation groups or for none ',
     +          'of them.')
                CALL WRTERR
                GO TO 1193
              END IF
            END IF
          END DO
1193      CONTINUE
          ICOUNT=0
          DO I=1,NOBSGP
            IF(IRGP(I).NE.1)ICOUNT=ICOUNT+1
          END DO
          IF((IGTARG.NE.0).AND.(ICOUNT.EQ.1))THEN
            WRITE(ERRMSG,1192)
1192        FORMAT(' Group-specific target measurement objective ',
     +      'functions can only be supplied if there is more than one ',
     +      'non-regularisation observation group.')
            CALL WRTERR
          END IF
        END IF

C -- SOME CHECKING IS CARRIED OUT ON ADAPTIVE PARAMETERS.

        IF(NOMOREIW_CHECK.EQ.0)THEN
          DO I=1,NPAR
            IF(APAR(I)(1:3).EQ.'iw_')THEN
              ASUFFIX=APAR(I)(4:)
              IF(ASUFFIX(1:5).NE.'regul')THEN
                WRITE(ERRMSG,3410) TRIM(APAR(I))
3410            FORMAT(' The name of parameter "',A,'" begins with ',
     +          '"iw_"; hence it is an adaptive regularisation ',
     +          'parameter. The next 5 letters of its name should ',
     +          'thus be "regul".')
                CALL WRTERR
              ELSE
                DO J=1,NOBSGP
                  IF(ASUFFIX.EQ.AOBSGP(J)) GO TO 3420
                END DO
                WRITE(ERRMSG,3425) TRIM(APAR(I))
3425            FORMAT(' The name of parameter "',A,'" begins with ',
     +          '"iw_"; hence it is an adaptive regularisation ',
     +          'parameter. The rest of its name should cite the ',
     +          'name of a regularisation observation group.')
                CALL WRTERR
3420            CONTINUE
                IF(ITRANS(I).EQ.0)THEN
                  WRITE(ERRMSG,3426) TRIM(APAR(I))
3426              FORMAT(' Adaptive regularisation ("iw_") ',
     +            'parameters must be log-transformed. Parameter "',
     +            A,'" is not log-transformed.')
                  CALL WRTERR
                END IF
              END IF
            END IF
          END DO
        END IF

        IF(IERR.NE.0) GO TO 9800

C -- THE OBSERVATION DATA SECTION OF THE PEST CONTROL FILE IS READ AND
C -- CHECKED

7627    CONTINUE
        ILINE=ILINE+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7627
        CALL LOWCAS(CLINE)
        CALL SHIFTL(CLINE)
1240    IF(CLINE.NE.'* observation data') THEN
          CALL WRTINT(ATEMP,ILINE)
          WRITE(6,1250,ERR=9900) ATEMP(1:INDEX(ATEMP,' ')-1),
     +    CASFL(1:MCASE)
1250      FORMAT(' Line ',A,' of file ',A,' should read:',
     +    /,' * observation data')
          GO TO 9800
        END IF
1300    CONTINUE
        IPCOUNT=0
        DO 1700 I=1,NOBS
7628    CONTINUE
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7628
        CALL LOWCAS(CLINE)
        CALL SHIFTL(CLINE)
        IF(CLINE.EQ.'* model command line')THEN
          CALL WRTINT(ATEMP,NOBS)
          WRITE(ERRMSG,1310) AALINE(1:MMLINE),ATEMP(:NBLNK(ATEMP))
1310      FORMAT(A,'too few "observation data" lines ',
     +    '- ',A,' expected.')
          CALL WRTERR
          GO TO 1755
        END IF
        IF(NOBSGP1.EQ.-999)THEN
          CALL LINSPL(IFAIL,3,LW,RW,CLINE)
        ELSE
          CALL LINSPL(IFAIL,4,LW,RW,CLINE)
        END IF
        IF(IFAIL.NE.0) GO TO 9500
        IF(RW(1)-LW(1)+1.GT.20)THEN
          WRITE(ERRMSG,1320) AALINE(1:MMLINE),
     +    CLINE(LW(1):RW(1))
1320       FORMAT(A,'observation name "',A,'" greater ',
     +    'than 20 characters long.')
          CALL WRTERR
        END IF
        AOBS(I)=CLINE(LW(1):RW(1))
        OBSNAM=' (observation "'//AOBS(I)(:NBLNK(AOBS(I)))//'").'
        IF(AOBS(I).EQ.'dum') THEN
          WRITE(ERRMSG,1323) AALINE(1:MMLINE)
1323      FORMAT(A,'illegal observation name - "dum".')
          CALL WRTERR
          GO TO 1331
        END IF
1331    CONTINUE
        CALL NUMRD(IFAIL,1,LW(2),RW(2),ITEMP,OVAL,CLINE)
        IF(IFAIL.NE.0) CALL RDERR(1,'OBSVAL')
        CALL NUMRD(IFAIL,1,LW(3),RW(3),ITEMP,WEIGHT,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(1,'WEIGHT')
          IWORK(I)=-1
        ELSE
          IF(WEIGHT.LT.0.0D0)THEN
            WRITE(ERRMSG,1350) AALINE(1:MMLINE),
     +      OBSNAM(:NBLNK(OBSNAM))
1350        FORMAT(A,'observation weight must not be less than zero',A)
            CALL WRTERR
            IWORK(I)=-1
          ELSE IF(WEIGHT.GT.0.0D0)THEN
            IWORK(I)=1
          ELSE IF(WEIGHT.EQ.0.0D0) THEN
            IWORK(I)=0
          END IF
        END IF
        OWGHT(I)=WEIGHT
        IF(NOBSGP1.NE.-999)THEN
          IF(RW(4)-LW(4)+1.GT.12)THEN
            WRITE(ERRMSG,1380) AALINE(1:MMLINE),CLINE(LW(4):RW(4))
1380        FORMAT(A,'observation group name "',A,
     +      '" greater than 12 characters long.')
            CALL WRTERR
            GO TO 1400
          END IF
          DO 1390 J=1,NOBSGP
            IF(CLINE(LW(4):RW(4)).EQ.AOBSGP(J))THEN
              IOBGNM(I)=J
              IF(J.EQ.JPREDICT)THEN
                IPCOUNT=IPCOUNT+1
                PWTKP=WEIGHT
              END IF
              NNUMOBS(J)=NNUMOBS(J)+1
              GO TO 1400
            END IF
1390      CONTINUE
          WRITE(ERRMSG,1395) AALINE(1:MMLINE),CLINE(LW(4):RW(4))
1395      FORMAT(A,'observation belongs to unlisted ',
     +    'observation group ("',A,'").')
          CALL WRTERR
1400      CONTINUE
        END IF
1700    CONTINUE

C -- Duplicates are checked for.

        CALL DUP_CHECK(NOBS,AOBS,TEMPOBS)

        DO I=1,NOBS
          IF(IWORK(I).NE.0) GO TO 1721
        END DO
        OZROWT=1
        IF(NPRIOR.EQ.0)THEN
          IF(NUMCOV.EQ.0)THEN
          FLAG_ZERO_WEIGHT=1
C          WRITE(ERRMSG,1710)
C1710      FORMAT(' There are no observations with a non-zero weight.')
C          CALL WRTERR
          END IF
        END IF
1721    CONTINUE
        IF(IPRED.NE.0)THEN
          IF(IPCOUNT.NE.1)THEN
            WRITE(ERRMSG,1722)
1722        FORMAT(' When PEST is run in predictive analysis mode, ',
     +      'one (and only one) observation must belong to ',
     +      'observation group "predict".')
            CALL WRTERR
          END IF
        END IF
        IF(IPRED.NE.0)THEN
          IF(IPCOUNT.EQ.1)THEN
            IF(IOBGNM(NOBS).NE.JPREDICT)THEN
              IF(ABS(JACFILE).EQ.1)THEN
              WRITE(ERRMSG,1724)
1724          FORMAT(' When PEST is run in predictive analysis ',
     +        'mode and JACFILE is supplied as 1 or -1, the ',
     +        'prediction must be the last observation.')
              CALL WRTERR
              END IF
            END IF
          END IF
        END IF
        IF(IGTARG.NE.0)THEN
          IF(NPRIOR.EQ.0)THEN
            DO I=1,NOBSGP
              IF(IRGP(I).EQ.0)THEN
                IF(NNUMOBS(I).EQ.0)THEN
                  WRITE(ERRMSG,1725)
1725              FORMAT(' If group-specific target measurement ',
     +            'objective function values ',
     +            'are supplied, then every non-regularisation ',
     +            'observation group must possess at least one member.')
                  CALL WRTERR
                  GO TO 1726
                END IF
              END IF
            END DO
1726        CONTINUE
          END IF
        END IF

C -- THE DERIVATIVES COMMAND LINE SECTION OF THE PEST CONTROL FILE IS
C    READ AND CHECKED.

7629   CONTINUE
       ILINE=ILINE+1
       READ(30,'(A)',ERR=9600,END=9700) CLINE
       CALL PPROC(CLINE,IPPCOUNT)
       IF(CLINE.EQ.' ') GO TO 7629
       CALL LOWCAS(CLINE)
       CALL SHIFTL(CLINE)
       IF(INDEX(CLINE,'* model comm').NE.0)THEN
         IF(JACFILE.NE.0)THEN
           CALL WRTINT(ATEMP,ILINE)
           WRITE(6,1750,ERR=9900) ATEMP(1:INDEX(ATEMP,' ')-1),
     +     CASFL(1:MCASE)
1750       FORMAT(' Line ',A,' of file ',A,' should read:',
     +     /,' " * derivatives command line"')
           GO TO 9800
         END IF
         GO TO 1745
       END IF
       IF(INDEX(CLINE,'* derivativ').EQ.0)THEN
          CALL WRTINT(ATEMP,ILINE)
          WRITE(6,1742,ERR=9900) ATEMP(1:INDEX(ATEMP,' ')-1),
     +    CASFL(1:MCASE)
1742      FORMAT(' Line ',A,' of file ',A,' should read:',
     +    /,' "* derivatives command line" or ',
     +    '" * model command line".')
          GO TO 9800
        END IF
7631    CONTINUE
        ILINE=ILINE+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7631
        CALL LOWCAS(CLINE)
        CALL SHIFTL(CLINE)
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        IF(CLINE.EQ.' ')THEN
          WRITE(ERRMSG,1744)AALINE(1:MMLINE)
1744      FORMAT(A,'blank derivatives command line.')
          CALL WRTERR
        ELSE IF(INDEX(CLINE,'* model comm').NE.0)THEN
          WRITE(ERRMSG,1746) AALINE(1:MMLINE)
1746      FORMAT(A,'missing derivatives command line.')
          CALL WRTERR
          GO TO 1745
        ELSE
          DCOMLIN=CLINE
        END IF
7633    CONTINUE
        ILINE=ILINE+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7633
        CALL LOWCAS(CLINE)
        CALL SHIFTL(CLINE)
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        IF(CLINE.EQ.' ')THEN
          IF(ABS(JACFILE).EQ.1)THEN
            WRITE(ERRMSG,1747)AALINE(1:MMLINE)
1747        FORMAT(A,'blank external derivatives return filename.')
          ELSE IF(JACFILE.EQ.2)THEN
            WRITE(ERRMSG,17471) AALINE(1:MMLINE)
17471       FORMAT(A,'blank JUPITER derivatives interface filename.')
          END IF
          CALL WRTERR
        ELSE IF(INDEX(CLINE,'* model comm').NE.0)THEN
          IF(ABS(JACFILE).EQ.1)THEN
            WRITE(ERRMSG,4295) AALINE(1:MMLINE)
4295        FORMAT(A,'missing derivatives return filename.')
          ELSE IF(JACFILE.EQ.2)THEN
            WRITE(ERRMSG,42951) AALINE(1:MMLINE)
42951       FORMAT(A,'missing JUPITER derivatives interface ',
     +      'filename.')
          END IF
          CALL WRTERR
          GO TO 1745
        ELSE
          CALL SPACESUB(CLINE)
          CALL LINSPL(IFAIL,1,LW,RW,CLINE)
          AJACFLE=CLINE(LW(1):RW(1))
          CALL REMCHAR(AJACFLE,CHAR(211))
        END IF

C -- THE MODEL COMMAND LINE SECTION OF THE PEST CONTROL FILE IS READ AND
C -- CHECKED

7634    CONTINUE
        ILINE=ILINE+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7634
        CALL LOWCAS(CLINE)
        CALL SHIFTL(CLINE)
1745    IF(CLINE.NE.'* model command line') THEN
          CALL WRTINT(ATEMP,ILINE)
          WRITE(6,1759,ERR=9900) ATEMP(1:INDEX(ATEMP,' ')-1),
     +    CASFL(1:MCASE)
1759      FORMAT(' Line ',A,' of file ',A,' should read:',
     +    /,' * model command line')
          GO TO 9800
        END IF
1755    DO 1769 ICOM=1,NUMCOM
7635      CONTINUE
          ILINE=ILINE+1
          READ(30,'(A)',ERR=9600,END=9700) CLINE
          CALL PPROC(CLINE,IPPCOUNT)
          IF(CLINE.EQ.' ') GO TO 7635
          CALL LOWCAS(CLINE)
          CALL SHIFTL(CLINE)
          CALL WRTINT(ATEMP,ILINE)
          ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
          AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
          MMLINE=NBLNK(AALINE)+1
          IF(CLINE.EQ.' ')THEN
            WRITE(ERRMSG,1760)AALINE(1:MMLINE)
1760        FORMAT(A,'blank model command line.')
            CALL WRTERR
          ELSE IF(CLINE(1:13).EQ.'* model input')THEN
            CALL WRTINT(ATEMP,NUMCOM)
            WRITE(ERRMSG,1765) AALINE(1:MMLINE),ATEMP(1:NBLNK(ATEMP))
1765        FORMAT(A,'missing model command line (',A,' expected).')
            CALL WRTERR
            GO TO 1800
          ELSE
            COMLIN(ICOM)=CLINE
          END IF
1769    CONTINUE

C -- THE MODEL INPUT/OUTPUT SECTION OF THE PEST CONTROL FILE IS READ AND
C -- CHECKED

7636    CONTINUE
        ILINE=ILINE+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7636
        CALL LOWCAS(CLINE)
        CALL SHIFTL(CLINE)
        IF((CLINE.NE.'* model input/output').AND.
     +     (CLINE.NE.'* model input'))THEN
          CALL WRTINT(ATEMP,ILINE)
          WRITE(6,1770,ERR=9900) ATEMP(1:INDEX(ATEMP,' ')-1),
     +    CASFL(1:MCASE)
1770       FORMAT(' Line ',A,' of file ',A,' should read:',
     +    /,' "* model input/output" or "* model input"')
          GO TO 9800
        END IF
1800    CONTINUE
        IF(IERR.NE.0) GO TO 9800
        JJFLAG=0
        IF(CLINE.EQ.'* model input') JJFLAG=1

        DO 1805 I=1,NPARTOT
1805    IWORK(I)=1000
        NUMERR=IERR
        DO 2500 I=1,NINFLE
        NUMPAR(I)=0
7637    CONTINUE
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7637
        CALL SHIFTL(CLINE)
#ifdef CAPFILE
        CALL UPCAS(CLINE)
#endif
        TCLINE=CLINE(1:50)
        CALL LOWCAS(TCLINE)
        IF(TCLINE.EQ.'* model output')THEN
          IF(JJFLAG.EQ.0)THEN
            WRITE(ERRMSG,1809)AALINE(1:MMLINE)
1809        FORMAT(A,'a "model output" section header must ',
     +      'not be present unless it follows a ',
     +      '"model input" section header.')
          ELSE
          CALL WRTINT(ATEMP,NINFLE)
          WRITE(ERRMSG,1811)AALINE(1:MMLINE),ATEMP(1:INDEX(ATEMP,' ')-1)
          END IF
          CALL WRTERR
          GO TO 9800
        END IF
        IF(TCLINE.EQ.'* prior information')THEN
          IF(JJFLAG.EQ.0)THEN
          CALL WRTINT(ATEMP,NINFLE+NOUTFL)
          WRITE(ERRMSG,1810)AALINE(1:MMLINE),ATEMP(1:INDEX(ATEMP,' ')-1)
1810      FORMAT(A,'too few "model input/output" lines - ',A,
     +    ' expected.')
          ELSE
          CALL WRTINT(ATEMP,NINFLE)
          WRITE(ERRMSG,1811)AALINE(1:MMLINE),ATEMP(1:INDEX(ATEMP,' ')-1)
1811      FORMAT(A,'too few "model input" lines - ',A,
     +    ' expected.')
          END IF
          CALL WRTERR
          IF(NPRIOR.NE.0) GO TO 3050
          GO TO 9800
        END IF
        IF((TCLINE.EQ.'* predictive analysis').OR.
     +     (TCLINE.EQ.'* pareto'))THEN
          IF(JJFLAG.EQ.0)THEN
          CALL WRTINT(ATEMP,NINFLE+NOUTFL)
          WRITE(ERRMSG,1810)AALINE(1:MMLINE),ATEMP(1:INDEX(ATEMP,' ')-1)
          ELSE
          CALL WRTINT(ATEMP,NINFLE)
          WRITE(ERRMSG,1811)AALINE(1:MMLINE),ATEMP(1:INDEX(ATEMP,' ')-1)
          END IF
          CALL WRTERR
          GO TO 9800
        END IF

        CALL SPACESUB(CLINE)
        CALL LINSPL(IFAIL,2,LW,RW,CLINE)
        IF(IFAIL.NE.0) GO TO 9500
        TMPFLE(I)=CLINE(LW(1):RW(1))
        CALL REMCHAR(TMPFLE(I),CHAR(211))
        WRKLEN=NBLNK(TMPFLE(I))
        MODFLE(I)=CLINE(LW(2):RW(2))
        CALL REMCHAR(MODFLE(I),CHAR(211))
        IF(I.GT.1) THEN
          DO 1814 J=1,I-1
          IF(MODFLE(J).EQ.MODFLE(I))THEN
          WRITE(ERRMSG,1813)AALINE(1:MMLINE),
     +    MODFLE(I)(:NBLNK(MODFLE(I)))
1813        FORMAT(A,'model input file ',A,' cited more than once.')
            CALL WRTERR
            GO TO 1812
          END IF
1814      CONTINUE
1812      CONTINUE
          DO 1815 J=1,I-1
          IF(TMPFLE(I).EQ.TMPFLE(J))THEN
            NUMPAR(I)=NUMPAR(J)
            GO TO 2500
          END IF
1815      CONTINUE
        END IF
        DLINE=TMPFLE(I)
        CALL LOWCAS(DLINE)
        IF((ISVDA.EQ.1).AND.(DLINE.EQ.'parcalc.tpl'))THEN
          DO J=1,NPAR
            IF(APAR(J)(1:3).EQ.'par')IWORK(J)=0
          END DO
          NUMPAR(I)=NPAR
          GO TO 2500
        END IF

        IF(CHECKFLAG.EQ.1) GO TO 2500

C -- A TEMPLATE FILE IS READ AND CHECKED

#ifdef UNICOS
        OPEN(UNIT=40,FILE=TMPFLE(I)(1:WRKLEN),STATUS='OLD',
     +  RECL=2000,ERR=1820)
#else
        OPEN(UNIT=40,FILE=TMPFLE(I)(1:WRKLEN),STATUS='OLD',ERR=1820)
#endif
        GO TO 1840
1820    WRITE(ERRMSG,1830) AALINE(1:MMLINE),TMPFLE(I)(1:WRKLEN)
1830    FORMAT(A,'cannot open model template file ',A,'.')
        CALL WRTERR
        GO TO 2500
1840    JLINE=1
        CALL WRTINT(ATEMP,JLINE)
        AALINE=' Line '//ATEMP(1:INDEX(ATEMP,' ')-1)//' of template '//
     +  'file '//TMPFLE(I)(1:WRKLEN)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(40,'(A)',ERR=2300,END=2400) CLINE
        CALL LOWCAS(CLINE)
        IF((CLINE(1:3).EQ.'pif').OR.(CLINE(1:3).EQ.'jif'))THEN
          WRITE(ERRMSG,1845) TMPFLE(I)(1:WRKLEN)
1845      FORMAT(' File ',A,' is an instruction file: a template ',
     +    'file is expected.')
          CALL WRTERR
          GO TO 2490
        END IF
        IF(((CLINE(1:4).NE.'ptf ').AND.
     +      (CLINE(1:4).NE.'jtf ')).OR.(NBLNK(CLINE).LT.5))THEN
          WRITE(ERRMSG,1850) AALINE(1:MMLINE)
1850      FORMAT(A,'first line must be "ptf" or "jtf" + ',
     +    'parameter delimiter.')
          CALL WRTERR
          GO TO 2490
        END IF
        PARDEL=CLINE(5:5)
        IF(PARDEL.EQ.' ')THEN
          WRITE(ERRMSG,1860) AALINE(1:MMLINE)
1860      FORMAT(A,'parameter delimiter not found in fifth character ',
     +    'position.')
          CALL WRTERR
          GO TO 2490
        END IF
        II=ICHAR(PARDEL)
        IF(((II.GE.48).AND.(II.LE.57)).OR.
     +     ((II.GE.97).AND.(II.LE.122)))THEN
          WRITE(ERRMSG,1870) AALINE(1:MMLINE)
1870      FORMAT(A,'illegal parameter delimiter.')
          CALL WRTERR
          GO TO 2490
        END IF
1880    JLINE=JLINE+1
        CALL WRTINT(ATEMP,JLINE)
        AALINE=' Line '//ATEMP(1:INDEX(ATEMP,' ')-1)//' of template '//
     +  'file '//TMPFLE(I)(1:WRKLEN)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(40,'(A)',ERR=2300,END=2400) CLINE
1900    NBLC=NBLNK(CLINE)
        IF(NBLC.GT.MAXWID)THEN
          WRITE(ERRMSG,1915) AALINE(1:MMLINE),
     +    AMXWID(:NBLNK(AMXWID))
1915      FORMAT(A,'line exceeds ',A,' characters.')
          CALL WRTERR
          GO TO 1880
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
1931      FORMAT(A,'tab character embedded in parameter space ',
     +    '("',A,'"); use space characters.')
          CALL WRTERR
          GO TO 1920
        END IF
        CALL PARCHK(IFAIL,J1,J2,TPAR,CLINE)
        IF(IFAIL.NE.0) GO TO 1920
        CALL WHICH1(IFAIL4,NPARTOT,II,APAR,TPAR)
        IF(IFAIL4.NE.0)THEN
          WRITE(ERRMSG,1940) AALINE(1:MMLINE),TPAR(:NBLNK(TPAR)),
     +    CASFL(1:MCASE)
1940      FORMAT(A,'parameter "',A,'" not cited in file ',A,'.')
          CALL WRTERR
          GO TO 1920
        END IF
        IF(APAR(II)(1:3).EQ.'iw_')THEN
          WRITE(ERRMSG,1932) AALINE(1:MMLINE),TRIM(TPAR)
1932      FORMAT(A,'parameter "',A,'" is an adaptive regularisation ',
     +    '("iw_") parameter, and hence must not be featured in a ',
     +    'template file.')
          CALL WRTERR
        END IF
        IWORK(II)=0
        NUMPAR(I)=NUMPAR(I)+1
        GO TO 1920

2300    WRITE(ERRMSG,2310) AALINE(1:MMLINE)
2310    FORMAT(A,'cannot read line.')
        CALL WRTERR
        GO TO 2490
2400    IF(JLINE.EQ.1)THEN
          WRITE(ERRMSG,2410) AALINE(1:MMLINE)
2410      FORMAT(A,'unexpected end to file.')
          CALL WRTERR
        END IF
2490    CLOSE(UNIT=40)
2500    CONTINUE

        IF(CHECKFLAG.EQ.0)THEN
          IF(IERR.EQ.NUMERR)THEN
            DO 2520 I=1,NPAR
            IF(IWORK(I).EQ.1000) THEN
              A3=APAR(I)(1:3)
              IF(A3.NE.'iw_')THEN
                IF(NPARSEC.EQ.0)THEN
                  WRITE(ERRMSG,2510) APAR(I)(:NBLNK(APAR(I))),
     +            CASFL(1:MCASE)
2510              FORMAT(' Parameter "',A,'" from file ',A,
     +            ' is not cited in a template file.')
                  CALL WRTERR
                ELSE
                  IPRIMWARN=1
                END IF
              END IF
            ENDIF
2520        CONTINUE
            IF(NPARFILE.NE.0)THEN
              DO I=NPAR+1,NPAR+NPARFILE
                IF(IWORK(I).EQ.1000) THEN
                  WRITE(ERRMSG,2511) APAR(I)(:NBLNK(APAR(I))),
     +            CASFL(1:MCASE)
2511              FORMAT(' File-parameter "',A,'" from file ',A,
     +            ' is not cited in a template file.')
                  CALL WRTERR
                END IF
              END DO
            ENDIF
            IF(NPARSEC.NE.0)THEN
              DO I=NPAR+NPARFILE+1,NPAR+NPARFILE+NPARSEC
                IF(IWORK(I).NE.1000) GO TO 2512
              END DO
              ISECWARN=1
2512          CONTINUE
            END IF
          END IF
        END IF

        IF(JJFLAG.EQ.1)THEN
          IPPCOUNT=IPPCOUNT+1
2513      ILINE=ILINE+1
          READ(30,'(A)',ERR=9600,END=9700) CLINE
          CALL PPROC(CLINE,IPPCOUNT)
          IF(CLINE.EQ.' ') GO TO 2513
          IF(CLINE.NE.'* model output')THEN
            CALL WRTINT(ATEMP,ILINE)
            ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
            AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
            MMLINE=NBLNK(AALINE)+1
            WRITE(ERRMSG,2514)AALINE(1:MMLINE)
2514        FORMAT(A,'this line should be "* model output".')
            CALL WRTERR
            GO TO 9800
          END IF
        END IF

        DO 2980 I=1,NOBS
2980    IWORK(I)=0
        NUMERR=IERR
        DO 3000 I=1,NOUTFL
        NUMOBS(I)=0
7638    CONTINUE
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7638
        CALL SHIFTL(CLINE)
#ifdef CAPFILE
        CALL UPCAS(CLINE)
#endif
        TCLINE=CLINE(1:50)
        CALL LOWCAS(TCLINE)
        IF(TCLINE.EQ.'* model output')THEN
          IF(JJFLAG.EQ.0)THEN
            WRITE(ERRMSG,1809)AALINE(1:MMLINE)
          ELSE
            WRITE(ERRMSG,18091)AALINE(1:MMLINE)
18091       FORMAT(A,'unexpected "model output" section ',
     +      'header.')
          END IF
          CALL WRTERR
          GO TO 9800
        END IF
        IF(TCLINE.EQ.'* prior information')THEN
          IF(JJFLAG.EQ.0)THEN
          CALL WRTINT(ATEMP,NINFLE+NOUTFL)
          WRITE(ERRMSG,1810)AALINE(1:MMLINE),ATEMP(1:INDEX(ATEMP,' ')-1)
          ELSE
          CALL WRTINT(ATEMP,NOUTFL)
          WRITE(ERRMSG,18131)
     +    AALINE(1:MMLINE),ATEMP(1:INDEX(ATEMP,' ')-1)
18131     FORMAT(A,'too few "model output" lines - ',A,
     +    ' expected.')
          END IF
          CALL WRTERR
          IF(NPRIOR.NE.0) GO TO 3050
          GO TO 9800
        END IF
        IF(TCLINE.EQ.'* pareto')THEN
          IF(JJFLAG.EQ.0)THEN
          CALL WRTINT(ATEMP,NINFLE+NOUTFL)
          WRITE(ERRMSG,1810)AALINE(1:MMLINE),ATEMP(1:INDEX(ATEMP,' ')-1)
          ELSE
          CALL WRTINT(ATEMP,NOUTFL)
          WRITE(ERRMSG,18131)
     +    AALINE(1:MMLINE),ATEMP(1:INDEX(ATEMP,' ')-1)
          END IF
          CALL WRTERR
          GO TO 9800
        END IF
        CALL SPACESUB(CLINE)
        CALL LINSPL(IFAIL,2,LW,RW,CLINE)
        IF(IFAIL.NE.0) GO TO 9500
        INSFLE(I)=CLINE(LW(1):RW(1))
        CALL REMCHAR(INSFLE(I),CHAR(211))
        WRKLEN=NBLNK(INSFLE(I))
        MODFLE(I)=CLINE(LW(2):RW(2))
        CALL REMCHAR(MODFLE(I),CHAR(211))
        IF(I.GT.1) THEN
          DO 2600 J=1,I-1
          IF(INSFLE(J).EQ.INSFLE(I))THEN
            WRITE(ERRMSG,2610)AALINE(1:MMLINE),
     +      INSFLE(I)(:NBLNK(INSFLE(I)))
2610        FORMAT(A,'instruction file ',A,' cited more than once.')
            CALL WRTERR
            GO TO 3000
          END IF
2600      CONTINUE
        END IF

C -- AN INSTRUCTION FILE IS READ AND CHECKED

        IF(CHECKFLAG.EQ.1) GO TO 3000

#ifdef UNICOS
        OPEN(UNIT=40,FILE=INSFLE(I)(1:WRKLEN),STATUS='OLD',
     +  RECL=2000,ERR=2620)
#else
        OPEN(UNIT=40,FILE=INSFLE(I)(1:WRKLEN),STATUS='OLD',ERR=2620)
#endif
        GO TO 2640
2620    WRITE(ERRMSG,2630) AALINE(1:MMLINE),INSFLE(I)(1:WRKLEN)
2630    FORMAT(A,'cannot open instruction file ',A,'.')
        CALL WRTERR
        GO TO 3000
2640    JLINE=1
        CALL WRTINT(ATEMP,JLINE)
        AALINE=' Line '//ATEMP(1:INDEX(ATEMP,' ')-1)//' of instruction '
     +  //'file '//INSFLE(I)(1:WRKLEN)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(40,'(A)',ERR=2900,END=2950) CLINE
        CALL TABREM(CLINE)
        CALL LOWCAS(CLINE)
        IF((CLINE(1:3).EQ.'ptf').OR.(CLINE(1:3).EQ.'jtf'))THEN
          WRITE(ERRMSG,2645) INSFLE(I)(1:WRKLEN)
2645      FORMAT(' File ',A,' is a template file: an instruction ',
     +    'file is expected.')
          CALL WRTERR
          GO TO 2990
        END IF
        IF(((CLINE(1:4).NE.'pif ').AND.
     +      (CLINE(1:4).NE.'jif ')).OR.(NBLNK(CLINE).LT.5))THEN
          WRITE(ERRMSG,2650) AALINE(1:MMLINE)
2650      FORMAT(A,'first line must be "pif" or "jif" + ',
     +    'marker delimiter.')
          CALL WRTERR
          GO TO 2990
        END IF
        PARDEL=CLINE(5:5)
        II=ICHAR(PARDEL)
        IF(PARDEL.EQ.' ')THEN
          WRITE(ERRMSG,2660) AALINE(1:MMLINE)
2660      FORMAT(A,'marker delimiter not found in fifth character ',
     +    'position.')
          CALL WRTERR
          GO TO 2990
        ELSE IF((PARDEL.EQ.'(').OR.(PARDEL.EQ.')').OR.(PARDEL.EQ.'[')
     +    .OR.(PARDEL.EQ.']').OR.(PARDEL.EQ.'!').OR.
     +    ((II.GE.48).AND.(II.LE.57)).OR.
     +    ((II.GE.97).AND.(II.LE.122)).OR.
     +    (PARDEL.EQ.'&').OR.(PARDEL.EQ.':'))THEN
          WRITE(ERRMSG,2670) AALINE(1:MMLINE)
2670      FORMAT(A,'illegal marker delimiter.')
          CALL WRTERR
          PARDEL=' '
          GO TO 2990
        END IF
        IS=0
2680    JLINE=JLINE+1
        CALL WRTINT(ATEMP,JLINE)
        AALINE=' Line '//ATEMP(1:INDEX(ATEMP,' ')-1)//
     +  ' of instruction file '//INSFLE(I)(1:WRKLEN)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(40,'(A)',ERR=2900,END=2950) CLINE
        IF(INDEX(CLINE,CHAR(9)).NE.0)THEN
          IF(INDEX(CLINE,PARDEL).EQ.0) CALL TABREM(CLINE)
        ENDIF
        IF(CLINE.EQ.' ') GO TO 2680
        NBLC=NBLNK(CLINE)
        IF(NBLC.GT.MAXWID)THEN
          WRITE(ERRMSG,2705) AALINE(1:MMLINE),
     +    AMXWID(:NBLNK(AMXWID))
2705      FORMAT(A,'line exceeds ',A,' characters.')
          CALL WRTERR
          GO TO 2680
        END IF
        CALL SHIFTL(CLINE)
        IF((IS.EQ.0).AND.(CLINE(1:1).EQ.'&'))THEN
          WRITE(ERRMSG,2706) AALINE(1:MMLINE)
2706      FORMAT(A,'first instruction line in file cannot begin ',
     +    'with continuation character.')
          CALL WRTERR
        END IF
        IS=1
        N2=0
        IF((CLINE(1:1).NE.'l').AND.(CLINE(1:1).NE.'L').AND.
     +  (CLINE(1:1).NE.'&').AND.(CLINE(1:1).NE.PARDEL))THEN
          WRITE(ERRMSG,2685) AALINE(1:MMLINE)
2685      FORMAT(A,'instruction line must begin with "l", a ',
     +    'marker delimiter or "&" (for continuation).')
          CALL WRTERR
        END IF
        IF(CLINE(1:1).NE.'&') J1=0
2690    NBLC=NBLNK(CLINE)
2700    CALL GETINT(IFAIL4,CLINE,N1,N2,NBLC,PARDEL)
        IF(IFAIL4.NE.0)THEN
          WRITE(ERRMSG,2710) AALINE(1:MMLINE)
2710      FORMAT(A,'missing marker delimiter.')
          CALL WRTERR
          GO TO 2680
        END IF
        IF(N1.EQ.0) GO TO 2680
        IF((CLINE(N1:N1).EQ.'l').OR.(CLINE(N1:N1).EQ.'L'))THEN
          IF(N1.EQ.N2)THEN
            CALL ILLINS(CLINE(N1:N2))
            GO TO 2700
          END IF
          WRITE(FMT,2730) N2-N1
2730      FORMAT('(I',I4,')')
          READ(CLINE(N1+1:N2),FMT,ERR=2740) ITEMP
          GO TO 2750
2740      CALL ILLINS(CLINE(N1:N2))
          GO TO 2700
2750      IF(N1.NE.1)THEN
            WRITE(ERRMSG,2720) AALINE(1:MMLINE)
2720        FORMAT(A,'line advance instruction can only occur at the ',
     +      'beginning of an instruction line.')
            CALL WRTERR
          END IF
          IF(ITEMP.LE.0) THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2755)
2755        FORMAT('  (Integer following "l" must be positive.)')
            CALL WRTERR
          END IF
        ELSE IF(CLINE(N1:N1).EQ.PARDEL)THEN
          IF(INDEX(CLINE(N1:N2),CHAR(9)).NE.0)THEN
            WRITE(ERRMSG,2686) AALINE(1:MMLINE)
2686        FORMAT(A,'tab character embedded in marker; space ',
     +      'characters must be used.')
            CALL WRTERR
          END IF
          IF(N2-N1.EQ.1)THEN
            WRITE(ERRMSG,2760)AALINE(1:MMLINE)
2760        FORMAT(A,'marker has zero length.')
            CALL WRTERR
          ELSE
            J1=J1+N2-N1-1
            IF(J1.GT.MAXWID)THEN
              WRITE(ERRMSG,2765) AALINE(1:MMLINE),
     +        AMXWID(:NBLNK(AMXWID))
2765          FORMAT(A,'model output file width cannot exceed ',A,
     +        ' characters.')
              CALL WRTERR
              GO TO 2680
            END IF
          ENDIF
        ELSE IF(CLINE(N1:N1).EQ.'&')THEN
          IF(N1.NE.N2)THEN
            CALL ILLINS(CLINE(N1:N2))
          ELSE
            IF(N1.GT.1)THEN
              WRITE(ERRMSG,2770) AALINE(1:MMLINE)
2770          FORMAT(A,'"&" is a continuation character and can only ',
     +        'occur at the beginning of an instruction line.')
              CALL WRTERR
            END IF
          END IF
        ELSE IF((CLINE(N1:N1).EQ.'w').OR.(CLINE(N1:N1).EQ.'W'))THEN
          IF(N1.NE.N2)THEN
            CALL ILLINS(CLINE(N1:N2))
          ELSE
            J1=J1+1
            IF(J1.GT.MAXWID) THEN
          WRITE(ERRMSG,2765) AALINE(1:MMLINE),
     +    AMXWID(:NBLNK(AMXWID))
              CALL WRTERR
              GO TO 2680
            END IF
          ENDIF
        ELSE IF((CLINE(N1:N1).EQ.'t').OR.(CLINE(N1:N1).EQ.'T'))THEN
          IF(N1.EQ.N2)THEN
            CALL ILLINS(CLINE(N1:N2))
            GO TO 2700
          END IF
          WRITE(FMT,2730)N2-N1
          READ(CLINE(N1+1:N2),FMT,ERR=2780) ITEMP
          GO TO 2790
2780      CALL ILLINS(CLINE(N1:N2))
          GO TO 2700
2790      IF(ITEMP.LE.0)THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2848)
2848        FORMAT('  (Number following "t" must be greater than ',
     +      'zero.)')
            CALL WRTERR
          ELSE IF(ITEMP.LT.J1)THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2800)
2800        FORMAT('  (Tab position results in backward move - model ',
     +      'output file must be read from left to right.)')
            CALL WRTERR
          ELSE IF(ITEMP.GT.MAXWID)THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2847) AMXWID(:NBLNK(AMXWID))
2847        FORMAT('  (Model output file width cannot exceed ',A,
     +      ' characters.)')
            CALL WRTERR
            GO TO 2680
          ELSE
            J1=ITEMP
          END IF
        ELSE IF((CLINE(N1:N1).EQ.'[').OR.(CLINE(N1:N1).EQ.'('))THEN
          AA=CLINE(N1:N1)
          IF(AA.EQ.'[')THEN
            N3=INDEX(CLINE(N1:N2),']')
          ELSE
            N3=INDEX(CLINE(N1:N2),')')
          END IF
          IF(N3.EQ.0)THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2810)
2810        FORMAT('  (Instruction contains unbalanced parentheses.)')
            CALL WRTERR
            GO TO 2700
          END IF
          N3=N3+N1-1
          IF((N3-1.GT.N1).AND.(N3-1-(N1+1)+1.LE.20))
     +    OTPAR=CLINE(N1+1:N3-1)
          CALL LOWCAS(OTPAR)
          IF(N3-1.EQ.N1)THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2815)
2815        FORMAT('  (Instruction missing observation name.)')
            CALL WRTERR
          ELSE IF(N3-1-(N1+1)+1.GT.20)THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2816)
2816        FORMAT('  (Observation name greater than 20 characters ',
     +      'long.)')
            CALL WRTERR
          ELSE IF((N3-1-(N1+1)+1.EQ.3).AND.(OTPAR.EQ.'dum'))THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2817)
2817        FORMAT('  (Dummy observation name "dum" only allowed for ',
     +      'non-fixed observations.)')
            CALL WRTERR
          ELSE
            OTPAR=CLINE(N1+1:N3-1)
            CALL WHICH1(IFAIL4,NOBS,IOBS,AOBS,OTPAR)
            IF(IFAIL4.NE.0)THEN
              WRITE(ERRMSG,2820) AALINE(1:MMLINE),OTPAR(:NBLNK(OTPAR)),
     +        CASFL(1:MCASE)
2820          FORMAT(A,'observation "',A,'" not cited in file ',A,'.')
              CALL WRTERR
            ELSE
              IF(IWORK(IOBS).EQ.1)THEN
                WRITE(ERRMSG,2830)AALINE(1:MMLINE),OTPAR(:NBLNK(OTPAR))
2830            FORMAT(A,'observation "',A,'" already cited in an ',
     +          'instruction file.')
                CALL WRTERR
              ELSE
                IWORK(IOBS)=1
                NUMOBS(I)=NUMOBS(I)+1
              ENDIF
            END IF
          END IF
          IF(N2.EQ.N3)THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2840)
            CALL WRTERR
            GO TO 2700
          END IF
          CALL GETNUM(IFAIL4,CLINE,N3,N2,NUM1,NUM2,FMT)
          IF(IFAIL4.NE.0)THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2840)
2840        FORMAT('  (Cannot read numbers in format "N1:N2" after ',
     +      'observation name.)')
            CALL WRTERR
          ELSE IF(NUM1.EQ.0)THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2841)
2841        FORMAT('  (First Number in format "N1:N2" following ',
     +      'observation name must not be zero.)')
            CALL WRTERR
          ELSE IF(NUM2.LE.NUM1)THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2845)
2845        FORMAT(
     +      '  (Numbers in format "N1:N2" following observation ',
     +      'name must be in increasing order.)')
            CALL WRTERR
          ELSE IF (NUM2.GT.MAXWID) THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2847) AMXWID(:NBLNK(AMXWID))
            CALL WRTERR
            GO TO 2680
          ELSE
            IF(NUM1.LT.J1)THEN
              WRITE(ERRMSG,2850) AALINE(1:MMLINE)
2850          FORMAT(A,'model output file must be read from left to ',
     +        'right.')
              CALL WRTERR
            END IF
            J1=NUM2
          END IF
        ELSE IF(CLINE(N1:N1).EQ.'!')THEN
          IF(N1.EQ.N2)THEN
            CALL ILLINS(CLINE(N1:N2))
            GO TO 2700
          END IF
          ITEMP=INDEX(CLINE(N1+1:N2),'!')
          IF(ITEMP.EQ.0)THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2860)
2860        FORMAT('  ("!"s not balanced.)')
            CALL WRTERR
            GO TO 2700
          END IF
          IF(CLINE(N2:N2).NE.'!')THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2870)
2870        FORMAT('  (Instruction must end in "!".)')
            CALL WRTERR
            GO TO 2700
          END IF
          IF(N1+1.EQ.N2)THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2815)
            CALL WRTERR
          ELSE IF(N2-1-(N1+1)+1.GT.20)THEN
            CALL ILLINS(CLINE(N1:N2))
            WRITE(ERRMSG,2816)
            CALL WRTERR
          ELSE
            OTPAR=CLINE(N1+1:N2-1)
            CALL LOWCAS(OTPAR)
            IF(OTPAR.EQ.'dum') GO TO 2700
            CALL WHICH1(IFAIL4,NOBS,IOBS,AOBS,OTPAR)
            IF(IFAIL4.NE.0)THEN
              WRITE(ERRMSG,2820) AALINE(1:MMLINE),OTPAR(:NBLNK(OTPAR)),
     +        CASFL(1:MCASE)
              CALL WRTERR
            ELSE
              IF(IWORK(IOBS).EQ.1)THEN
                WRITE(ERRMSG,2830) AALINE(1:MMLINE),OTPAR(:NBLNK(OTPAR))
                CALL WRTERR
              ELSE
                IWORK(IOBS)=1
                NUMOBS(I)=NUMOBS(I)+1
              END IF
            END IF
          END IF
        ELSE
          CALL ILLINS(CLINE(N1:N2))
        END IF
        GO TO 2700

2900    WRITE(ERRMSG,2310) AALINE(1:MMLINE)
        CALL WRTERR
        GO TO 2990
2950    IF(JLINE.EQ.1)THEN
          WRITE(ERRMSG,2410) AALINE(1:MMLINE)
          CALL WRTERR
        END IF
2990    CLOSE(UNIT=40)
3000    CONTINUE

        IF(CHECKFLAG.EQ.0)THEN
          IF(IERR.EQ.NUMERR)THEN
            DO 3020 I=1,NOBS
            IF(IWORK(I).EQ.0)THEN
              WRITE(ERRMSG,3010) AOBS(I)(:NBLNK(AOBS(I))),
     +        CASFL(1:MCASE)
3010          FORMAT(' Observation "',A,'" from file ',A,
     +        ' not cited in an instruction file.')
              CALL WRTERR
            END IF
3020        CONTINUE
          END IF
        END IF

C -- AN ADAPTIVE REGULARISATION CHECK IS MADE.

        IF(NPRIOR.EQ.0)THEN
          DO I=1,NPAR
            IF(APAR(I)(1:3).EQ.'iw_')THEN
              IF(ITRANS(I).GE.0)THEN
              WRITE(ERRMSG,3011)
3011          FORMAT(' If a PEST control file cites ',
     +        'adaptive regularisation ("iw_") parameters, then ',
     +        'it must contain prior information which references ',
     +        'those parameters.')
              CALL WRTERR
              GO TO 3012
              END IF
            END IF
          END DO
        END IF
3012    CONTINUE

C -- THE PRIOR INFORMATION SECTION OF THE PEST CONTROL FILE IS READ AND CHECKED

        IF(NPRIOR.EQ.0) THEN
3022      ILINE=ILINE+1
          IF((IPRED.EQ.0).AND.(IREG.EQ.0).AND.(PESTMODE.NE.4))THEN
            READ(30,'(A)',ERR=9600,END=7999) CLINE
          ELSE
            READ(30,'(A)',ERR=9600,END=9700) CLINE
          END IF
          CALL PPROC(CLINE,IPPCOUNT)
          IF(CLINE.EQ.' ') GO TO 3022
          CALL LOWCAS(CLINE)
          CALL SHIFTL(CLINE)
          IF(CLINE.EQ.'* prior information') THEN
            IF(IPRED.NE.0) THEN
              GO TO 4000
            ELSE IF(IREG.NE.0)THEN
              GO TO 5000
            ELSE IF(PESTMODE.EQ.4) THEN
              GO TO 7000
            ELSE
3025          ILINE=ILINE+1
              READ(30,'(A)',ERR=9600,END=7999) CLINE
              CALL LOWCAS(CLINE)
              CALL SHIFTL(CLINE)
              CALL PPROC(CLINE,IPPCOUNT)
              IF(CLINE.EQ.' ') GO TO 3025
              IF((CLINE.NE.'* predictive analysis').AND.
     +           (INDEX(CLINE,'* regul').EQ.0).AND.
     +           (CLINE(1:8).NE.'* pareto'))THEN
                IFINFLAG=1
                GO TO 7999
              ELSE
                IF(CLINE.EQ.'* predictive analysis')THEN
3024              ILINE=ILINE+1
                  READ(30,'(A)',ERR=9600,END=7999) CLINE
                  CALL PPROC(CLINE,IPPCOUNT)
                  IF(CLINE.NE.' ') THEN
                    IF(INDEX(CLINE,'* regul').NE.0)THEN
3027                  ILINE=ILINE+1
                      READ(30,'(A)',ERR=9600,END=7999) CLINE
                      CALL PPROC(CLINE,IPPCOUNT)
                      IF(CLINE.NE.' ')THEN
                        IFINFLAG=3
                        GO TO 7999
                      ELSE
                        GO TO 3027
                      END IF
                    END IF
                    IF(INDEX(CLINE,'* pareto').NE.0)THEN
30271                 ILINE=ILINE+1
                      READ(30,'(A)',ERR=9600,END=7999) CLINE
                      CALL PPROC(CLINE,IPPCOUNT)
                      IF(CLINE.NE.' ')THEN
                        IFINFLAG=4
                        GO TO 7999
                      ELSE
                        GO TO 30271
                      END IF
                    ELSE
                      IFINFLAG=2
                      GO TO 7999
                    END IF
                  ELSE
                    GO TO 3024
                  END IF
                ELSE
3028              ILINE=ILINE+1
                  READ(30,'(A)',ERR=9600,END=7999) CLINE
                  CALL PPROC(CLINE,IPPCOUNT)
                  IF(CLINE.NE.' ') THEN
                    IFINFLAG=3
                    GO TO 7999
                  ELSE
                    GO TO 3028
                  END IF
                END IF
              END IF
            END IF
          END IF
          IF(IPRED.NE.0) THEN
            GO TO 4015
          ELSE IF(IREG.NE.0)THEN
            GO TO 5015
          ELSE IF(PESTMODE.EQ.4)THEN
            GO TO 7015
          ELSE
            IF(CLINE.EQ.'* predictive analysis') THEN
3023          ILINE=ILINE+1
              READ(30,'(A)',ERR=9600,END=7999) CLINE
              CALL PPROC(CLINE,IPPCOUNT)
              IF(CLINE.NE.' ') THEN
                IFINFLAG=2
                GO TO 7999
              ELSE
                GO TO 3023
              END IF
            ELSE IF(INDEX(CLINE,'* regul').NE.0)THEN
3033          ILINE=ILINE+1
              READ(30,'(A)',ERR=9600,END=7999) CLINE
              CALL PPROC(CLINE,IPPCOUNT)
              IF(CLINE.NE.' ') THEN
                IFINFLAG=3
                GO TO 7999
              ELSE
                GO TO 3033
              END IF
            ELSE IF(INDEX(CLINE,'* pareto').NE.0)THEN
30331         ILINE=ILINE+1
              READ(30,'(A)',ERR=9600,END=7999) CLINE
              CALL PPROC(CLINE,IPPCOUNT)
              IF(CLINE.NE.' ') THEN
                IFINFLAG=4
                GO TO 7999
              ELSE
                GO TO 30331
              END IF
            ELSE IF(INDEX(CLINE,'* distribution files').NE.0)THEN
              GO TO 7600
            ELSE
              CALL WRTINT(ATEMP,ILINE)
              WRITE(ERRMSG,3062,ERR=9900) ATEMP(:NBLNK(ATEMP)),
     +        CASFL(1:MCASE)
3062          FORMAT(' Line ',A,' of file ',A,': unknown data.')
              CALL WRTERR
              GO TO 9800
            END IF
          END IF
        END IF

C          IF(CLINE.NE.'* prior information') THEN
C            CALL WRTINT(ATEMP,ILINE)
C            WRITE(6,3062,ERR=9900) ATEMP(:NBLNK(ATEMP)),
C     +      CASFL(1:MCASE)
C3062        FORMAT(' Line ',A,' of file ',A,' should read:',
C     +      /,' * prior information')
C            GO TO 9800
C          END IF
C3061      GO TO 4000
C        END IF

        ALREDY=0
        GO TO 3051
3050    ALREDY=1
3051    NESPAR=0
        DO 3009 I=1,NPAR
        IF(ITRANS(I).GE.0) NESPAR=NESPAR+1
3009    CONTINUE
        ITEMP=NESPAR+1
        NESDIM=MAX(1,ITEMP)

        ALLOCATE(X(PRRDIM,NESDIM),Y(NESDIM),IW_PR(PRRDIM),
     +  STAT=IALERR)
        IF(IALERR.NE.0) THEN
          WRITE(6,3008,ERR=9900)
3008      FORMAT(/,' *** Cannot allocate memory to check prior ',
     +    'information ***',/)
          GO TO 7999
        END IF

        CALL PRRCHK(IFAIL,NPRWRN,NESPAR,ITEMP,NPRIOR,NPAR,OBGDIM,
     +  OBSDIM,OBNDIM,NOBSGP,NOBSGP1,NOBS,ILINE,ALREDY,MAXWID,ITRANS,
     +  X,Y,APRIOR,AOBSGP,AOBS,APAR,CLINE,AMXWID,ATEMP,FMT,PZROWT,
     +  IOBGNM,IW_PR,NOMOREIW_CHECK,NXROW,OWGHT,QUICKPRIOR,IPPCOUNT,
     +  NPARSEC,NPARFILE)
        IF(IFAIL.EQ.1)THEN
          GO TO 9600
        ELSE IF(IFAIL.EQ.2) THEN
          GO TO 9700
        ELSE IF(IFAIL.EQ.3) THEN
          GO TO 9800
        ELSE IF(IFAIL.EQ.4) THEN
          GO TO 9900
        ELSE IF(IFAIL.EQ.5)THEN
          IF(IPRED.NE.0) GO TO 4100
          GO TO 9800
        END IF
        IF((OZROWT.EQ.1).AND.(PZROWT.EQ.1))THEN
          IF(NPRIOR.NE.0)THEN
            IF(NUMCOV.EQ.0)THEN
            WRITE(ERRMSG,3060)
3060        FORMAT(' No observations or prior information articles ',
     +      'have a non-zero weight.')
            CALL WRTERR
            END IF
          END IF
        END IF
        DO I=1,NPAR
          IF(APAR(I)(1:3).EQ.'iw_')THEN
            IF(ITRANS(I).GE.0)THEN
            DO J=1,PRRDIM
              IF(IW_PR(J).EQ.I) GO TO 3063
            END DO
            WRITE(ERRMSG,3061) TRIM(APAR(I))
3061        FORMAT(' Adaptive regularisation parameter "',a,
     +      '" is not cited in any prior information equation.')
            CALL WRTERR
            END IF
          END IF
3063      CONTINUE
        END DO

        IF(NPRIOR.NE.0)THEN
          IF(IREG.NE.0)THEN
            IF(IGTARG.NE.0)THEN
              DO I=NOBS+1,NPRIOR
                J=IOBGNM(I)
                IF((J.GT.0).AND.(J.LE.NOBSGP))THEN
                  NNUMOBS(J)=NNUMOBS(J)+1
                END IF
              END DO
              DO I=1,NOBSGP
                IF(IRGP(I).EQ.0)THEN
                  IF(NNUMOBS(I).EQ.0)THEN
                    WRITE(ERRMSG,1725)
                    CALL WRTERR
                    GO TO 3064
                  END IF
                END IF
              END DO
3064          CONTINUE
            END IF
          END IF
        END IF

        IF(IREG.NE.0) GO TO 5000
        IF(PESTMODE.EQ.4) GO TO 7000

C -- NOW THE PREDICTIVE ANALYSIS SECTION OF THE PEST CONTROL FILE IS CHECKED

4000    CONTINUE
        IF(IPRED.EQ.0) THEN
4001      ILINE=ILINE+1
          READ(30,'(A)',ERR=9600,END=7999) CLINE
          CALL PPROC(CLINE,IPPCOUNT)
          IF(CLINE.EQ.' ') GO TO 4001
          CALL LOWCAS(CLINE)
          CALL SHIFTL(CLINE)
          IF(CLINE.EQ.'* predictive analysis') THEN
4002        ILINE=ILINE+1
            READ(30,'(A)',ERR=9600,END=7999) CLINE
            CALL PPROC(CLINE,IPPCOUNT)
            IF(CLINE.NE.' ')THEN
              IFINFLAG=2
              GO TO 7999
            ELSE
              GO TO 4002
            END IF
          ELSE IF(INDEX(CLINE,'* regul').NE.0) THEN
4003        ILINE=ILINE+1
            READ(30,'(A)',ERR=9600,END=7999) CLINE
            CALL PPROC(CLINE,IPPCOUNT)
            IF(CLINE.NE.' ')THEN
              IFINFLAG=3
              GO TO 7999
            ELSE
              GO TO 4003
            END IF
          ELSE IF(INDEX(CLINE,'* pareto').NE.0) THEN
40031       ILINE=ILINE+1
            READ(30,'(A)',ERR=9600,END=7999) CLINE
            CALL PPROC(CLINE,IPPCOUNT)
            IF(CLINE.NE.' ')THEN
              IFINFLAG=4
              GO TO 7999
            ELSE
              GO TO 40031
            END IF
          ELSE IF(INDEX(CLINE,'* distribution files').NE.0)THEN
            GO TO 7600
          ELSE
            CALL WRTINT(ATEMP,ILINE)
            WRITE(ERRMSG,3062,ERR=9900) ATEMP(:NBLNK(ATEMP)),
     +      CASFL(1:MCASE)
            CALL WRTERR
            GO TO 9800
          END IF
        END IF
4010    ILINE=ILINE+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 4010
        CALL LOWCAS(CLINE)
        CALL SHIFTL(CLINE)
4015    IF(CLINE.NE.'* predictive analysis') THEN
          CALL WRTINT(ATEMP,ILINE)
          WRITE(6,4020,ERR=9900) ATEMP(:NBLNK(ATEMP)),
     +    CASFL(1:MCASE)
4020      FORMAT(' Line ',A,' of file ',A,' should read:',
     +    /,' * predictive analysis')
          GO TO 9800
        END IF

4100    CONTINUE
4110    ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 4100
        CALL LINSPL(IFAIL,1,LW,RW,CLINE)
        IF(IFAIL.NE.0) GO TO 4110
        CALL NUMRD(IFAIL,0,LW(1),RW(1),NPMXMN,RTEMP,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(0,'NPREDMAXMIN')
        ELSE
          IF((NPMXMN.NE.1).AND.(NPMXMN.NE.-1))THEN
            WRITE(ERRMSG,4130) AALINE(1:MMLINE)
4130        FORMAT(A,'NPREDMAXMIN must be 1 or -1.')
            CALL WRTERR
          END IF
        END IF
        PREDNOISE=-9999
        CALL LINSPL(IFAIL,2,LW,RW,CLINE)
        IF(IFAIL.EQ.0)THEN
          CALL NUMRD(IFAIL,0,LW(2),RW(2),PREDNOISE,RTEMP,CLINE)
          IF(IFAIL.NE.0) THEN
            CALL RDERR(0,'PREDNOISE')
          ELSE
            IF((PREDNOISE.NE.1).AND.(PREDNOISE.NE.0))THEN
              WRITE(ERRMSG,4131) AALINE(1:MMLINE)
4131          FORMAT(A,'PREDNOISE must be 0 or 1.')
              CALL WRTERR
            END IF
          END IF
        END IF
        IF(IPCOUNT.GT.0)THEN
          IF((PREDNOISE.EQ.1).AND.(PWTKP.EQ.0.0D0))THEN
            WRITE(ERRMSG,4132)
4132        FORMAT(' If PREDNOISE is set to 1, the weight assigned ',
     +      'to the observation comprising the sole member of ',
     +      'observation group "predict" must not be zero.')
            CALL WRTERR
          END IF
        END IF

7641    CONTINUE
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7641
        CALL LINSPL(IFAIL,3,LW,RW,CLINE)
        IF(IFAIL.NE.0)THEN
          WRITE(ERRMSG,4140)AALINE(1:MMLINE)
4140      FORMAT(A,'too few items on this line - 3 expected.')
          CALL WRTERR
          GO TO 9800
        END IF
        IPFLAG1=0
        CALL NUMRD(IFAIL,1,LW(1),RW(1),ITEMP,PD0,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(1,'PD0')
          IPFLAG1=1
        ELSE
          IF(PD0.LE.0.0)THEN
            CALL ZRONEG(1,'PD0')
            IPFLAG1=1
          END IF
        END IF
        CALL NUMRD(IFAIL,1,LW(2),RW(2),ITEMP,PD1,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(1,'PD1')
          IPFLAG1=1
        ELSE
          IF(PD1.LE.0.0)THEN
            CALL ZRONEG(1,'PD1')
            IPFLAG1=1
          END IF
        END IF
        CALL NUMRD(IFAIL,1,LW(3),RW(3),ITEMP,PD2,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(1,'PD2')
          IPFLAG1=1
        ELSE
          IF(PD2.LE.0.0)THEN
            CALL ZRONEG(1,'PD2')
            IPFLAG1=1
          END IF
        END IF
        IF((PD1.LE.PD0).AND.(IPFLAG1.EQ.0))THEN
          WRITE(ERRMSG,4150) AALINE(1:MMLINE)
4150      FORMAT(A,'PD1 must be greater than PD0')
          CALL WRTERR
        END IF
        IF((PD2.LE.PD1).AND.(IPFLAG1.EQ.0))THEN
          WRITE(ERRMSG,4160) AALINE(1:MMLINE)
4160      FORMAT(A,'PD2 must be greater than PD1')
          CALL WRTERR
        END IF

7642    CONTINUE
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7642
        CALL LINSPL(IFAIL,2,LW,RW,CLINE)
        IF(IFAIL.NE.0)THEN
          WRITE(ERRMSG,4170) AALINE(1:MMLINE)
4170      FORMAT(A,'too few items on this line.')
          CALL WRTERR
          GO TO 9800
        END IF
        CALL NUMRD(IFAIL,1,LW(1),RW(1),ITEMP,ABPRED,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(1,'ABSPREDLAM')
        ELSE
          IF(ABPRED.LT.0.0)THEN
            WRITE(ERRMSG,4180) AALINE(1:MMLINE)
4180        FORMAT(A,'real variable ABSPREDLAM must be zero or larger.')
            CALL WRTERR
          END IF
        END IF
        CALL NUMRD(IFAIL,1,LW(2),RW(2),ITEMP,REPRED,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(1,'RELPREDLAM')
        ELSE
          IF(REPRED.LT.0.0)THEN
            WRITE(ERRMSG,4190) AALINE(1:MMLINE)
4190        FORMAT(A,'real variable RELPREDLAM must be zero or larger.')
            CALL WRTERR
          ELSE IF(REPRED.GT.1.0)THEN
            WRITE(ERRMSG,4191) AALINE(1:MMLINE)
4191        FORMAT(A,'real variable RELPREDLAM must not be greater ',
     +      'than one.')
            CALL WRTERR
          END IF
        END IF
        NSEARCH=-999
        LINITFAC=1.0
        LFACFAC=2.0
        CALL LINSPL(IFAIL,5,LW,RW,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL LINSPL(IFAIL,4,LW,RW,CLINE)
          IF(IFAIL.EQ.0)THEN
            WRITE(ERRMSG,4192) AALINE(1:MMLINE)
4192        FORMAT(A,'either 5 or 2 variable expected on this line.')
            CALL WRTERR
          ELSE
            CALL LINSPL(IFAIL,3,LW,RW,CLINE)
            IF(IFAIL.EQ.0)THEN
              WRITE(ERRMSG,4192) AALINE(1:MMLINE)
              CALL WRTERR
            END IF
          END IF
        ELSE
          GO TO 4193
        END IF
        GO TO 4199
4193    CALL NUMRD(IFAIL,1,LW(3),RW(3),ITEMP,LINITFAC,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(1,'INITSCHFAC')
        ELSE
          IF(LINITFAC.LE.0.0D0)THEN
            WRITE(ERRMSG,4194) AALINE(1:MMLINE)
4194        FORMAT(A,'real variable INITSCHFAC must be greater than ',
     +      'zero.')
            CALL WRTERR
          END IF
        END IF
        CALL NUMRD(IFAIL,1,LW(4),RW(4),ITEMP,LFACFAC,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(1,'MULSCHFAC')
        ELSE
          IF(LFACFAC.LE.1.0D0)THEN
            WRITE(ERRMSG,4195) AALINE(1:MMLINE)
4195        FORMAT(A,'real variable MULSCHFAC must be larger than ',
     +      'unity.')
            CALL WRTERR
          END IF
        END IF
        CALL NUMRD(IFAIL,0,LW(5),RW(5),NSEARCH,RTEMP,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(0,'NSEARCH')
        ELSE
          IF(NSEARCH.LE.0)THEN
            WRITE(ERRMSG,4196) AALINE(1:MMLINE)
4196        FORMAT(A,'integer variable NSEARCH must be greater than ',
     +      'zero.')
            CALL WRTERR
          END IF
        END IF

4199    CONTINUE
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 4199
        CALL LINSPL(IFAIL,2,LW,RW,CLINE)
        IF(IFAIL.NE.0)THEN
          WRITE(ERRMSG,4170) AALINE(1:MMLINE)
          CALL WRTERR
          GO TO 9800
        END IF
        CALL NUMRD(IFAIL,1,LW(1),RW(1),ITEMP,ASWPRD,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(1,'ABSPREDSWH')
        ELSE
          IF(ASWPRD.LT.0.0)THEN
            WRITE(ERRMSG,4200) AALINE(1:MMLINE)
4200        FORMAT(A,'real variable ABSPREDSWH must be zero or larger.')
            CALL WRTERR
          END IF
        END IF
        CALL NUMRD(IFAIL,1,LW(2),RW(2),ITEMP,RSWPRD,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(1,'RELPREDSWH')
        ELSE
          IF(RSWPRD.LT.0.0)THEN
            WRITE(ERRMSG,4205) AALINE(1:MMLINE)
4205        FORMAT(A,'real variable RELPREDSWH must be zero or larger.')
            CALL WRTERR
          ELSE IF(RSWPRD.GT.1.0)THEN
            WRITE(ERRMSG,4211) AALINE(1:MMLINE)
4211        FORMAT(A,'real variable RELPREDSWH must be not be ',
     +      'greater than one.')
            CALL WRTERR
          END IF
        END IF

7643    CONTINUE
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7643
        CALL LINSPL(IFAIL,4,LW,RW,CLINE)
        IF(IFAIL.NE.0)THEN
          WRITE(ERRMSG,4210) AALINE(1:MMLINE)
4210       FORMAT(A,'too few items on this line - 4 expected.')
          CALL WRTERR
          GO TO 9800
        END IF
        CALL NUMRD(IFAIL,0,LW(1),RW(1),NPRNRD,RTEMP,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(0,'NPREDNORED')
        ELSE
          IF(NPRNRD.LE.0.0)THEN
            CALL ZRONEG(0,'NPREDNORED')
          ENDIF
        END IF
        CALL NUMRD(IFAIL,1,LW(2),RW(2),ITEMP,PRSTPA,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(1,'ABSPREDSTP')
        ELSE
          IF(PRSTPA.LT.0.0) THEN
            WRITE(ERRMSG,4230) AALINE(1:MMLINE)
4230        FORMAT(A,': real variable ABSPREDSTP must not be less ',
     +      'than zero.')
            CALL WRTERR
          END IF
        END IF
        CALL NUMRD(IFAIL,1,LW(3),RW(3),ITEMP,PRSTPR,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(1,'RELPREDSTP')
        ELSE
          IF(PRSTPR.LT.0.0) THEN
            WRITE(ERRMSG,4240) AALINE(1:MMLINE)
4240        FORMAT(A,': real variable RELPREDSTP must not be less ',
     +      'than zero.')
            CALL WRTERR
          ELSE IF(PRSTPR.GT.1.0)THEN
            WRITE(ERRMSG,4241) AALINE(1:MMLINE)
4241        FORMAT(A,'real variable RELPREDSTP must be not be ',
     +      'greater than one.')
            CALL WRTERR
          END IF
        END IF
        CALL NUMRD(IFAIL,0,LW(4),RW(4),NPRDLO,RTEMP,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(0,'NPREDSTP')
        ELSE
          IF(NPRDLO.LE.1)THEN
            WRITE(ERRMSG,4220) AALINE(1:MMLINE)
4220        FORMAT(A,': integer variable NPREDSTP must be 2 or ',
     +      'greater.')
            CALL WRTERR
          ENDIF
        END IF
4030    ILINE=ILINE+1
        READ(30,'(A)',ERR=9600,END=7999) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ')THEN
          GO TO 4030
        ELSE IF(INDEX(CLINE,'* regul').NE.0)THEN
4040      ILINE=ILINE+1
          READ(30,'(A)',ERR=9600,END=7999) CLINE
          CALL PPROC(CLINE,IPPCOUNT)
          IF(CLINE.EQ.' ') THEN
            GO TO 4040
          ELSE
            IFINFLAG=3
            GO TO 7999
          END IF
        ELSE IF(INDEX(CLINE,'* pareto').NE.0)THEN
40401     ILINE=ILINE+1
          READ(30,'(A)',ERR=9600,END=7999) CLINE
          CALL PPROC(CLINE,IPPCOUNT)
          IF(CLINE.EQ.' ') THEN
            GO TO 40401
          ELSE
            IFINFLAG=4
            GO TO 7999
          END IF
        ELSE IF(INDEX(CLINE,'* distribution files').NE.0)THEN
          GO TO 7600
        ELSE
          CALL WRTINT(ATEMP,ILINE)
          WRITE(ERRMSG,3062,ERR=9900) ATEMP(:NBLNK(ATEMP)),
     +    CASFL(1:MCASE)
          CALL WRTERR
          GO TO 9800
        END IF

C -- THE REGULARISATION SECTION OF THE PEST CONTROL FILE IS NOW READ

5000    CONTINUE
5010    ILINE=ILINE+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 5000
        CALL LOWCAS(CLINE)
        CALL SHIFTL(CLINE)
5015    IF(INDEX(CLINE,'* regul').EQ.0)THEN
          CALL WRTINT(ATEMP,ILINE)
          WRITE(6,5020,ERR=9900) ATEMP(:NBLNK(ATEMP)),CASFL(1:MCASE)
5020      FORMAT(' Line ',A,' of file ',A,' should read: ',/,
     +    ' * regularisation')
          GO TO 9800
        END IF
5100    CONTINUE
5110    ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 5100
        VARTEXT='reg2measrat'
        UPVARTEXT='REG2MEASRAT'
        REG2MEASRAT=0.0
        CALL READ_MRUN_VAR_PC(IFAIL,REG2MEASRAT,VARTEXT,UPVARTEXT,
     +  CLINE,1,DUMFILE,NALINE)
        IF(IFAIL.NE.0) GO TO 9998
        CALL LINSPL(IFAIL,2,LW,RW,CLINE)
        IF(IFAIL.NE.0) THEN
          WRITE(ERRMSG,5120) AALINE(1:MMLINE)
5120      FORMAT(A,' too few items on this line - at least 2 expected.')
          CALL WRTERR
          GO TO 9800
        END IF
        IREGFLAG=0
        CALL NUMRD(IFAIL,1,LW(1),RW(1),ITEMP,PHIMLIM,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(1,'PHIMLIM')
          IREGFLAG=1
        ELSE
          IF(PHIMLIM.LE.0.0)THEN
            CALL ZRONEG(1,'PHIMLIM')
            IREGFLAG=1
          END IF
        END IF
        CALL NUMRD(IFAIL,1,LW(2),RW(2),ITEMP,PD1R,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(1,'PHIMACCEPT')
          IREGFLAG=1
        ELSE
          IF(PD1R.LE.0.0) THEN
            CALL ZRONEG(1,'PHIMACCEPT')
            IREGFLAG=1
          END IF
        END IF
        IF(IREGFLAG.EQ.0)THEN
          IF(PD1R.LE.PHIMLIM)THEN
              WRITE(ERRMSG,5150) AALINE(1:MMLINE)
5150          FORMAT(A,'PHIMACCEPT must be greater than PHIMLIM.')
              CALL WRTERR
          END IF
          IF(PD1R.GT.PHIMLIM*1.2)THEN
            WRITE(ERRMSG,51501) AALINE(1:MMLINE)
51501        FORMAT(A,'PHIMACCEPT must be less than 1.2 times ',
     +      'PHIMLIM.')
            CALL WRTERR
          END IF
        END IF
        FRACPHIM=-2.0E30
        FPOS=-1
        LPOS=-1
        MEMSAVE=-1
        IPCG=-1
        CALL LINSPL(IFAIL,3,LW,RW,CLINE)
        IF(IFAIL.EQ.0) THEN
          CALL NUMRD(IFAIL,1,LW(3),RW(3),ITEMP,FRACPHIM,CLINE)
          IF(IFAIL.NE.0) THEN
            AMEMSAVE=CLINE(LW(3):RW(3))
            CALL LOWCAS(AMEMSAVE)
            IF(AMEMSAVE.EQ.'nomemsave')THEN
              MEMSAVE=0
              LPOS=3
            ELSE IF(AMEMSAVE.EQ.'memsave')THEN
              MEMSAVE=1
              LPOS=3
            ELSE
              WRITE(ERRMSG,5151) AALINE(1:MMLINE)
5151          FORMAT(A,'third variable must be FRACPHIM (a number) ',
     +        'or MEMSAVE (a character variable either "memsave" ',
     +        'or "nomemsave").')
              CALL WRTERR
            END IF
          ELSE
            FPOS=1
            IF(FRACPHIM.LE.0.0) THEN
              FRACPHIM=-1.0E30
            ELSE
              IF(FRACPHIM.GE.1.0)THEN
                WRITE(ERRMSG,5148) AALINE(1:MMLINE)
5148            FORMAT(A,'FRACPHIM must be less than 1.')
                CALL WRTERR
              END IF
            END IF
          END IF
        END IF

        IF(FPOS.NE.-1)THEN
          CALL LINSPL(IFAIL,4,LW,RW,CLINE)
          IF(IFAIL.EQ.0)THEN
            AMEMSAVE=CLINE(LW(4):RW(4))
            CALL LOWCAS(AMEMSAVE)
            IF(AMEMSAVE.EQ.'nomemsave')THEN
              MEMSAVE=0
              LPOS=4
            ELSE IF(AMEMSAVE.EQ.'memsave')THEN
              MEMSAVE=1
              LPOS=4
            ELSE
              WRITE(ERRMSG,5149) AALINE(1:MMLINE)
5149          FORMAT(A,'MEMSAVE must be "memsave" or "nomemsave".')
              CALL WRTERR
            END IF
          END IF
        END IF

        IF(LPOS.NE.-1)THEN
          CALL LINSPL(IFAIL,LPOS+1,LW,RW,CLINE)
          IF(IFAIL.NE.0) GO TO 5158
          APCG=CLINE(LW(LPOS+1):RW(LPOS+1))
          CALL LOWCAS(APCG)
          IF(APCG.EQ.'cg')THEN
            WRITE(ERRMSG,51591) AALINE(1:MMLINE)
51591       FORMAT(A,'use of conjugate gradient solver is no ',
     +      'longer supported by PEST. Alter "cg" to "nocg" or ',
     +      'omit it and following variables.')
            CALL WRTERR
          ELSE IF(APCG.EQ.'nocg')THEN
            IPCG=0
          ELSE
            WRITE(ERRMSG,5152) AALINE(1:MMLINE)
5152        FORMAT(A,'CONJGRAD must be "cg" or "nocg".')
            CALL WRTERR
          END IF
        END IF
5158    CONTINUE

7644    CONTINUE
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7644
        IREGFLAG=0
        CALL LINSPL(IFAIL,3,LW,RW,CLINE)
        IF(IFAIL.NE.0)THEN
          WRITE(ERRMSG,5125) AALINE(1:MMLINE)
5125      FORMAT(A,'too few items on this line - 3 expected.')
          CALL WRTERR
          GO TO 9800
        END IF
        CALL NUMRD(IFAIL,1,LW(1),RW(1),ITEMP,WFINIT,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(1,'WFINIT')
          IREGFLAG=1
        ELSE
          IF(WFINIT.LE.0.0)THEN
            CALL ZRONEG(1,'WFINIT')
            IREGFLAG=1
          END IF
        END IF
        CALL NUMRD(IFAIL,1,LW(2),RW(2),ITEMP,WFMIN,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(1,'WFMIN')
          IREGFLAG=1
        ELSE
          IF(WFMIN.LE.0.0)THEN
            CALL ZRONEG(1,'WFMIN')
            IREGFLAG=1
          END IF
        END IF
        CALL NUMRD(IFAIL,1,LW(3),RW(3),ITEMP,WFMAX,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(1,'WFMAX')
          IREGFLAG=1
        ELSE
          IF(WFMAX.LE.0.0)THEN
            CALL ZRONEG(1,'WFMAX')
            IREGFLAG=1
          END IF
        END IF
        IF(IREGFLAG.EQ.0)THEN
          IF(WFMIN.GT.WFINIT)THEN
            WRITE(ERRMSG,5170) AALINE(1:MMLINE)
5170        FORMAT(A,'WFINIT must be greater than or equal to WFMIN.')
            CALL WRTERR
          END IF
          IF(WFMAX.LT.WFINIT)THEN
            WRITE(ERRMSG,5180) AALINE(1:MMLINE)
5180        FORMAT(A,'WFINIT must be less than or equal to WFMAX.')
            CALL WRTERR
          END IF
          IF(WFMAX.LE.WFMIN)THEN
            WRITE(ERRMSG,5190) AALINE(1:MMLINE)
5190        FORMAT(A,'WFMIN must be less than WFMAX.')
            CALL WRTERR
          END IF
        END IF
        ILINREG=0
        IREGCONTINUE=0
        CALL LINSPL(IFAIL,4,LW,RW,CLINE)
        IF(IFAIL.EQ.0)THEN
          ATEMP=CLINE(LW(4):RW(4))
          CALL LOWCAS(ATEMP)
          IF((ATEMP.NE.'linreg').AND.(ATEMP.NE.'nonlinreg').AND.
     +       (ATEMP.NE.'continue').AND.(ATEMP.NE.'nocontinue'))THEN
            WRITE(ERRMSG,5191) AALINE(1:MMLINE)
5191        FORMAT(A,'fourth variable must be LINREG ',
     +      '(values of "linreg" or "nonlinreg") or REGCONTINUE ',
     +      '(values of "continue" or "nocontinue").')
            CALL WRTERR
          ELSE
            IF((ATEMP.EQ.'linreg').OR.(ATEMP.EQ.'nonlinreg'))THEN
              ILINREG=1
            ELSE IF((ATEMP.EQ.'continue').OR.
     +              (ATEMP.EQ.'nocontinue'))THEN
              IREGCONTINUE=1
              IF(ATEMP.EQ.'continue')REGCONTINUE=1
            END IF
          END IF
          CALL LINSPL(IFAIL,5,LW,RW,CLINE)
          IF(IFAIL.EQ.0)THEN
            ATEMP=CLINE(LW(5):RW(5))
            CALL LOWCAS(ATEMP)
            IF((ATEMP.NE.'linreg').AND.(ATEMP.NE.'nonlinreg').AND.
     +         (ATEMP.NE.'continue').AND.(ATEMP.NE.'nocontinue'))THEN
              WRITE(ERRMSG,5192) AALINE(1:MMLINE)
5192          FORMAT(A,'fifth variable must be LINREG ',
     +        '(values of "linreg" or "nonlinreg") or REGCONTINUE ',
     +        '(values of "continue" or "nocontinue").')
              CALL WRTERR
            ELSE
              IF((ATEMP.EQ.'linreg').OR.(ATEMP.EQ.'nonlinreg'))then
                IF(ILINREG.EQ.1)THEN
                  WRITE(ERRMSG,5913) AALINE(1:MMLINE)
5913              FORMAT(A,'value supplied twice for LINREG variable.')
                  CALL WRTERR
                END IF
              ELSE IF((ATEMP.EQ.'continue').OR.
     +                (ATEMP.EQ.'nocontinue'))THEN
                IF(IREGCONTINUE.EQ.1)THEN
                  WRITE(ERRMSG,5914) AALINE(1:MMLINE)
5914              FORMAT(A,'value supplied twice for REGCONTINUE ',
     +            'variable.')
                  CALL WRTERR
                ELSE
                  IF(ATEMP.EQ.'continue')REGCONTINUE=1
                END IF
              END IF
            END IF
          END IF
        END IF

7645    CONTINUE
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7645
        CALL LINSPL(IFAIL,2,LW,RW,CLINE)
        IF(IFAIL.NE.0)THEN
          WRITE(ERRMSG,5120) AALINE(1:MMLINE)
          CALL WRTERR
          GO TO 9800
        END IF
        CALL NUMRD(IFAIL,1,LW(1),RW(1),ITEMP,WFFAC,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(1,'WFFAC')
        ELSE
          IF(WFFAC.LE.1.0)THEN
            WRITE(ERRMSG,5195) AALINE(1:MMLINE)
5195        FORMAT(A,'WFFAC must be greater than 1.')
            CALL WRTERR
          END IF
        END IF
        CALL NUMRD(IFAIL,1,LW(2),RW(2),ITEMP,REGTOL,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(1,'WFTOL')
        ELSE
          IF(REGTOL.LE.0.0)THEN
            CALL ZRONEG(1,'WFTOL')
          END IF
        END IF
        IREGADJ=0
        CALL LINSPL(IFAIL,3,LW,RW,CLINE)
        IF(IFAIL.EQ.0)THEN
          CALL NUMRD(IFAIL,0,LW(3),RW(3),IREGADJ,RTEMP,CLINE)
          IF(IFAIL.NE.0)THEN
            CALL RDERR(1,'IREGADJ')
          ELSE
          IF((IREGADJ.NE.0).AND.(IREGADJ.NE.1).AND.(IREGADJ.NE.2).
     +       AND.(IREGADJ.NE.3).AND.(IREGADJ.NE.4).AND.
     +       (IREGADJ.NE.5))THEN
            WRITE(ERRMSG,5196) AALINE(1:MMLINE)
5196        FORMAT(A,'IREGADJ must be 0, 1, 2, 3, 4 or 5.')
            CALL WRTERR
          END IF
          END IF
        END IF
        IF((IREGADJ.EQ.4).OR.(IREGADJ.EQ.5))THEN
          CALL LINSPL(IFAIL,5,LW,RW,CLINE)
          IF(IFAIL.NE.0)THEN
            IF(IREGADJ.EQ.4)THEN
              WRITE(ERRMSG,5310) AALINE(1:MMLINE)
5310          FORMAT(A,'if IREGADJ is set to 4, two entries must ',
     +        'follow it.')
            ELSE
              WRITE(ERRMSG,5309) AALINE(1:MMLINE)
5309          FORMAT(A,'if IREGADJ is set to 5, three entries must ',
     +        'follow it.')
            END IF
            CALL WRTERR
          ELSE
            CALL NUMRD(IFAIL,0,LW(4),RW(4),NOPTREGADJ,RTEMP,CLINE)
            IF(IFAIL.NE.0)THEN
              CALL RDERR(0,'NOPTREGADJ')
            ELSE
              IF(NOPTREGADJ.LE.0)THEN
                WRITE(ERRMSG,5311) AALINE(1:MMLINE)
5311            FORMAT(A,'integer variable NOPTREGADJ must be ',
     +          'greater than zero.')
                CALL WRTERR
              END IF
            END IF
            CALL NUMRD(IFAIL,1,LW(5),RW(5),ITEMP,REGWEIGHTRAT,CLINE)
            IF(IFAIL.NE.0)THEN
              CALL RDERR(1,'REGWEIGHTRAT')
            ELSE
              IF(ABS(REGWEIGHTRAT).LT.1.0D0)THEN
                WRITE(ERRMSG,5312) AALINE(1:MMLINE)
5312            FORMAT(A,'absolute value of real variable ',
     +          'REGWEIGHTRAT must be greater than 1.0.')
                CALL WRTERR
              END IF
            END IF
            IF(IREGADJ.EQ.5)THEN
              CALL LINSPL(IFAIL,6,LW,RW,CLINE)
              IF(IFAIL.NE.0)THEN
                WRITE(ERRMSG,5309) AALINE(1:MMLINE)
                CALL WRTERR
              ELSE
                CALL NUMRD(IFAIL,1,LW(6),RW(6),ITEMP,REGSINGTHRESH,
     +          CLINE)
                IF(IFAIL.NE.0)THEN
                  CALL RDERR(1,'REGSINGTHRESH')
                ELSE
                  IF((REGSINGTHRESH.GE.1.0D0).OR.
     +               (REGSINGTHRESH.LE.0.0D0))THEN
                    WRITE(ERRMSG,5314) AALINE(1:MMLINE)
5314                FORMAT(A,'real variable REGSINGTHRESH must be ',
     +              'greater then zero and less than one.')
                    CALL WRTERR
                  END IF
                END IF
              END IF
            END IF
          END IF
        END IF
        IF(IREGADJ.GT.0)THEN
          IF(WFMAX/WFMIN.LT.1.0D10)THEN
            WRITE(ERRMSG,5315)
5315        FORMAT(' If the IREGADJ regularisation control variable ',
     +      'is set greater than zero, then upper ',
     +      'and lower bounds for the regularization weight factor ',
     +      '(i.e. WFMAX and WFMIN) must be set far apart.')
            CALL WRTERR
          END IF
C          IF(REGCONTINUE.NE.0)THEN
C            WRITE(ERRMSG,53151)
C53151       FORMAT(' If the IREGADJ regularisation control variable ',
C     +      'is set greater than zero, the REGCONTINUE variable must ',
C     +      'not be set to "continue".')
C            CALL WRTERR
C          END IF
        END IF

        DO 5210 I=1,OBNDIM
          IF(IRGP(IOBGNM(I)).EQ.1) GO TO 5126
5210    CONTINUE
        WRITE(ERRMSG,5211)
5211    FORMAT(' PEST cannot run in regularisation mode as no ',
     +  'observations or prior information items belong to ',
     +  'a regularisation group.')
        CALL WRTERR
5126    CONTINUE
5200    ILINE=ILINE+1
        READ(30,'(A)',ERR=9600,END=7999)CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') THEN
          GO TO 5126
        ELSE IF(INDEX(CLINE,'* distribution files').NE.0)THEN
          GO TO 7600
        ELSE
          CALL WRTINT(ATEMP,ILINE)
          WRITE(ERRMSG,3062,ERR=9900) ATEMP(:NBLNK(ATEMP)),
     +    CASFL(1:MCASE)
          CALL WRTERR
          GO TO 9800
        END IF

5201    ILINE=ILINE+1
        READ(30,'(A)',ERR=9600,END=7999) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ')THEN
          GO TO 5201
        ELSE IF(INDEX(CLINE,'* predict').NE.0)THEN
5202      ILINE=ILINE+1
          READ(30,'(A)',ERR=9600,END=7999) CLINE
          CALL PPROC(CLINE,IPPCOUNT)
          IF(CLINE.EQ.' ') THEN
            GO TO 5202
          ELSE
            IFINFLAG=2
            GO TO 7999
          END IF
        ELSE IF(INDEX(CLINE,'* pareto').NE.0)THEN
40402     ILINE=ILINE+1
          READ(30,'(A)',ERR=9600,END=7999) CLINE
          CALL PPROC(CLINE,IPPCOUNT)
          IF(CLINE.EQ.' ') THEN
            GO TO 40402
          ELSE
            IFINFLAG=4
            GO TO 7999
          END IF
        ELSE IF(INDEX(CLINE,'* distribution files').NE.0)THEN
          GO TO 7600
        ELSE
          CALL WRTINT(ATEMP,ILINE)
          WRITE(ERRMSG,3062,ERR=9900) ATEMP(:NBLNK(ATEMP)),
     +    CASFL(1:MCASE)
          CALL WRTERR
          GO TO 9800
        END IF

C -- THE PARETO SECTION OF THE PEST CONTROL FILE IS READ.

7000    CONTINUE
7010    ILINE=ILINE+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7010
        CALL LOWCAS(CLINE)
        CALL SHIFTL(CLINE)
7015    IF(INDEX(CLINE,'* pareto').EQ.0)THEN
          CALL WRTINT(ATEMP,ILINE)
          WRITE(6,7020,ERR=9900) TRIM(ATEMP),CASFL(1:MCASE)
7020      FORMAT(' Line ',A,' of file ',A,' should read: ',/,
     +    ' * pareto')
          GO TO 9800
        END IF
        CONTINUE
7110    ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//TRIM(ATEMP)//' of file '
        AALINE=TRIM(ALINE)//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7110
        IF(CLINE(1:1).EQ.'#') GO TO 7110
        CALL LOWCAS(CLINE)
        CALL SPACESUB(CLINE)
        CALL LINSPL(IFAIL,1,LW,RW,CLINE)
        ATEMP25=CLINE(LW(1):RW(1))
        CALL REMCHAR(ATEMP25,CHAR(211))
        ATEMP12=ADJUSTL(ATEMP25)
        DO I=1,NOBSGP
          IF(ATEMP12.EQ.AOBSGP(I)) GO TO 7115
        END DO
        WRITE(ERRMSG,7111) AALINE(1:MMLINE),TRIM(ATEMP12)
7111    FORMAT(A,' "',A,'" is not an observation group ',
     +  'named in "observation groups" section of the PEST control ',
     +  'file.')
        CALL WRTERR
        PAR_OBSGROUP=0
        GO TO 7120
7115    PAR_OBSGROUP=I
        J=0
        K=0
        DO I=1,NOBS+NPRIOR
          IF(IOBGNM(I).EQ.PAR_OBSGROUP)THEN
            J=J+1
            IF(OWGHT(I).GT.0.0D0)THEN
              K=K+1
              GO TO 7120
            END IF
          END IF
        END DO
        IF(J.EQ.0)THEN
          WRITE(ERRMSG,7118) AALINE(1:MMLINE),TRIM(AOBSGP(PAR_OBSGROUP))
7118      FORMAT(A,' no observations belong to observation group "',A,
     +    '".')
          CALL WRTERR
        ELSE IF(K.EQ.0)THEN
          IF(COVFLE(PAR_OBSGROUP).EQ.' ')THEN
            WRITE(ERRMSG,7119) AALINE(1:MMLINE),
     +      TRIM(AOBSGP(PAR_OBSGROUP))
7119        FORMAT(A,' all observations belonging to observation ',
     +      'group "',A,'" have a weight of zero.')
            CALL WRTERR
          END IF
        END IF

7120    CONTINUE
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=TRIM(ALINE)//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7120
        IF(CLINE(1:1).EQ.'#') GO TO 7120
        CALL LINSPL(IFAIL,3,LW,RW,CLINE)
        IF(IFAIL.NE.0)THEN
          WRITE(ERRMSG,7121) AALINE(1:MMLINE)
7121      FORMAT(A,'three items are expected on this line.')
          CALL WRTERR
        ELSE
          CALL NUMRD(IFAIL,1,LW(1),RW(1),ITEMP,PAR_WTFAC_START,CLINE)
          IF(IFAIL.NE.0)THEN
            CALL RDERR(1,'PARETO_WTFAC_START')
            PAR_WTFAC_START=-1.1E35
          ELSE
            IF(PAR_WTFAC_START.LT.0.0)THEN
              WRITE(ERRMSG,7122) AALINE(1:MMLINE)
7122          FORMAT(A,'PARETO_WTFAC_START must be zero or greater.')
              CALL WRTERR
            END IF
          END IF
          CALL NUMRD(IFAIL,1,LW(2),RW(2),ITEMP,PAR_WTFAC_FIN,CLINE)
          IF(IFAIL.NE.0)THEN
            CALL RDERR(1,'PARETO_WTFAC_FIN')
          ELSE
            IF(PAR_WTFAC_START.GE.-1.0E35)THEN
              IF(PAR_WTFAC_FIN.LE.PAR_WTFAC_START)THEN
                WRITE(ERRMSG,7123) AALINE(1:MMLINE)
7123            FORMAT(A,'PARETO_WTFAC_FIN must be greater than ',
     +          'PARETO_WTFAC_START.')
                CALL WRTERR
              END IF
            END IF
          END IF
          CALL NUMRD(IFAIL,0,LW(3),RW(3),NUM_WTFAC_INC,RTEMP,CLINE)
          IF(IFAIL.NE.0)THEN
            CALL RDERR(0,'NUM_WTFAC_INC')
          ELSE
            IF(NUM_WTFAC_INC.LE.0)THEN
              WRITE(ERRMSG,7125) AALINE(1:MMLINE)
7125          FORMAT(A,'NUM_WTFAC_INC must be greater than zero.')
              CALL WRTERR
            END IF
          END IF
        END IF

7130    CONTINUE
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=TRIM(ALINE)//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7130
        IF(CLINE(1:1).EQ.'#') GO TO 7130
        CALL LINSPL(IFAIL,3,LW,RW,CLINE)
        IF(IFAIL.NE.0)THEN
          WRITE(ERRMSG,7121) AALINE(1:MMLINE)
          CALL WRTERR
        ELSE
          CALL NUMRD(IFAIL,0,LW(1),RW(1),NUM_ITER_START,RTEMP,CLINE)
          IF(IFAIL.NE.0)THEN
            CALL RDERR(0,'NUM_ITER_START')
          ELSE
            IF(NUM_ITER_START.LT.0)THEN
              WRITE(ERRMSG,7132) AALINE(1:MMLINE)
7132          FORMAT(A,'NUM_ITER_START must be zero or greater.')
              CALL WRTERR
            END IF
          END IF
          CALL NUMRD(IFAIL,0,LW(2),RW(2),NUM_ITER_GEN,RTEMP,CLINE)
          IF(IFAIL.NE.0)THEN
            CALL RDERR(0,'NUM_ITER_GEN')
          ELSE
            IF(NUM_ITER_GEN.LT.1)THEN
              WRITE(ERRMSG,7133) AALINE(1:MMLINE)
7133          FORMAT(A,'NUM_ITER_GEN must be greater than ',
     +        'zero.')
              CALL WRTERR
            END IF
          END IF
          CALL NUMRD(IFAIL,0,LW(3),RW(3),NUM_ITER_FIN,RTEMP,CLINE)
          IF(IFAIL.NE.0)THEN
            CALL RDERR(0,'NUM_ITER_FIN')
          ELSE
            IF(NUM_ITER_FIN.LT.0)THEN
              WRITE(ERRMSG,7135) AALINE(1:MMLINE)
7135          FORMAT(A,'NUM_ITER_FIN must be zero or greater.')
              CALL WRTERR
            END IF
          END IF
        END IF

7140    CONTINUE
        ALT_TERM=-999
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=TRIM(ALINE)//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7140
        IF(CLINE(1:1).EQ.'#') GO TO 7140
        CALL LINSPL(IFAIL,1,LW,RW,CLINE)
        CALL NUMRD(IFAIL,0,LW(1),RW(1),ALT_TERM,RTEMP,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(0,'ALT_TERM')
        ELSE
          IF((ALT_TERM.NE.0).AND.(ALT_TERM.NE.1))THEN
            WRITE(ERRMSG,7142) AALINE(1:MMLINE)
7142        FORMAT(A,'ALT_TERM must be zero or one.')
            CALL WRTERR
          END IF
        END IF
        IF((ALT_TERM.NE.0).AND.(ALT_TERM.NE.1)) GO TO 7999
        IF(ALT_TERM.EQ.0) GO TO 7160

7150    CONTINUE
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=TRIM(ALINE)//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7150
        IF(CLINE(1:1).EQ.'#') GO TO 7150
        CALL LOWCAS(CLINE)
        CALL SPACESUB(CLINE)
        CALL LINSPL(IFAIL,4,LW,RW,CLINE)
        IF(IFAIL.NE.0)THEN
          WRITE(ERRMSG,7151) AALINE(1:MMLINE)
7151      FORMAT(A,'four items are expected on this line.')
          CALL WRTERR
        ELSE
          ATEMP25=CLINE(LW(1):RW(1))
          CALL REMCHAR(ATEMP25,CHAR(211))
          OBS_TERM=ADJUSTL(ATEMP25)
          DO I=1,NOBS
            IF(OBS_TERM.EQ.AOBS(I)) GO TO 7153
          END DO
          IF(NPRIOR.NE.0)THEN
            DO I=1,NPRIOR
              IF(OBS_TERM.EQ.APRIOR(I)) GO TO 7153
            END DO
          END IF
          WRITE(ERRMSG,7152) AALINE(1:MMLINE),TRIM(OBS_TERM)
7152      FORMAT(A,'"',A,'" is not the name of an observation or ',
     +    'prior information equation.')
          CALL WRTERR
7153      CONTINUE
          ATEMP20=CLINE(LW(2):RW(2))
          IF((ATEMP20.NE.'above').AND.(ATEMP20.NE.'below'))THEN
            WRITE(ERRMSG,7154) AALINE(1:MMLINE)
7154        FORMAT(A,'second item must be "above" or "below".')
            CALL WRTERR
          END IF
          CALL NUMRD(IFAIL,1,LW(3),RW(3),ITEMP,OBS_THRESH,CLINE)
          IF(IFAIL.NE.0)THEN
            CALL RDERR(1,'OBS_THRESH')
          END IF
          CALL NUMRD(IFAIL,0,LW(4),RW(4),NUM_ITER_THRESH,RTEMP,CLINE)
          IF(IFAIL.NE.0)THEN
            CALL RDERR(0,'NUM_ITER_THRESH')
          ELSE
            IF(NUM_ITER_THRESH.LT.1)THEN
              WRITE(ERRMSG,7157) AALINE(1:MMLINE)
7157          FORMAT(A,'NUM_ITER_THRESH must be greater than ',
     +        'zero.')
              CALL WRTERR
            END IF
          END IF
        END IF

7160    CONTINUE
        NOBS_REPORT=-999
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=TRIM(ALINE)//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7160
        IF(CLINE(1:1).EQ.'#') GO TO 7160
        CALL LINSPL(IFAIL,1,LW,RW,CLINE)
        CALL NUMRD(IFAIL,0,LW(1),RW(1),NOBS_REPORT,RTEMP,CLINE)
        IF(IFAIL.NE.0)THEN
          CALL RDERR(0,'NOBS_REPORT')
        ELSE
          IF(NOBS_REPORT.LT.0)THEN
            WRITE(ERRMSG,7162) AALINE(1:MMLINE)
7162        FORMAT(A,'NOBS_REPORT must be zero or greater.')
            CALL WRTERR
          END IF
        END IF
        IF(NOBS_REPORT.LE.0) GO TO 7999

        ALLOCATE(OBS_REPORT(NOBS_REPORT),STAT=IALERR)
        IF(IALERR.NE.0) GO TO 9400
7170    CONTINUE
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=TRIM(ALINE)//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7170
        IF(CLINE(1:1).EQ.'#') GO TO 7170
        CALL SPACESUB(CLINE)
        CALL LOWCAS(CLINE)
        DO I=1,NOBS_REPORT
          CALL LINSPL(IFAIL,1,LW,RW,CLINE)
          IF(IFAIL.NE.0)THEN
            WRITE(ATEMP12,'(I12)') NOBS_REPORT
            ATEMP12=ADJUSTL(ATEMP12)
            WRITE(ERRMSG,7172) AALINE(1:MMLINE),TRIM(ATEMP12)
7172        FORMAT(A,A,' (i.e. NOBS_REPORT) entries are expected ',
     +      'on this line.')
            CALL WRTERR
            EXIT
          END IF
          ATEMP25=CLINE(LW(1):RW(1))
          CALL REMCHAR(ATEMP25,CHAR(211))
          ATEMP20=ADJUSTL(ATEMP25)
          DO J=1,NOBS
            IF(ATEMP20.EQ.AOBS(J)) GO TO 7175
          END DO
          IF(NPRIOR.NE.0)THEN
            DO J=1,NPRIOR
              IF(ATEMP20.EQ.APRIOR(J)) GO TO 7175
            END DO
          END IF
          WRITE(ERRMSG,7152) AALINE(1:MMLINE),TRIM(ATEMP20)
          CALL WRTERR
7175      CONTINUE
          IF(I.GT.1)THEN
            DO J=1,I-1
              IF(ATEMP20.EQ.OBS_REPORT(J))THEN
                WRITE(ERRMSG,7176) AALINE(1:MMLINE),TRIM(ATEMP20)
7176            FORMAT(A,'observation name "',A,'" duplicated.')
                CALL WRTERR
                GO TO 7177
              END IF
            END DO
7177        CONTINUE
          END IF
          OBS_REPORT(I)=ATEMP20
          CLINE=CLINE(RW(1)+1:)
        END DO

7201    ILINE=ILINE+1
        READ(30,'(A)',ERR=9600,END=7999) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        CALL LOWCAS(CLINE)
        IF(CLINE.EQ.' ')THEN
          GO TO 7201
        ELSE IF(INDEX(CLINE,'* predict').NE.0)THEN
7202      ILINE=ILINE+1
          READ(30,'(A)',ERR=9600,END=7999) CLINE
          CALL PPROC(CLINE,IPPCOUNT)
          IF(CLINE.EQ.' ') THEN
            GO TO 7202
          ELSE
            IFINFLAG=2
            GO TO 7999
          END IF
        ELSE IF(INDEX(CLINE,'* regul').NE.0)THEN
40403     ILINE=ILINE+1
          READ(30,'(A)',ERR=9600,END=7999) CLINE
          CALL PPROC(CLINE,IPPCOUNT)
          IF(CLINE.EQ.' ') THEN
            GO TO 40403
          ELSE
            IFINFLAG=3
            GO TO 7999
          END IF
        ELSE IF(INDEX(CLINE,'* distribution files').NE.0)THEN
          GO TO 7600
        ELSE
          CALL WRTINT(ATEMP,ILINE)
          WRITE(ERRMSG,3062,ERR=9900) ATEMP(:NBLNK(ATEMP)),
     +    CASFL(1:MCASE)
          CALL WRTERR
          GO TO 9800
        END IF

7600    CONTINUE

        DO
          ILINE=ILINE+1
          CALL WRTINT(ATEMP,ILINE)
          ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
          AALINE=TRIM(ALINE)//' '//CASFL(1:MCASE)//': '
          MMLINE=NBLNK(AALINE)+1
          READ(30,'(A)',ERR=9600,END=7650) CLINE
          IF(CLINE.EQ.' ') GO TO 7650
          IF(CLINE(1:1).EQ.'#') THEN
            IPPCOUNT=IPPCOUNT+1
            CYCLE
          END IF
          IF(CLINE(1:2).EQ.'++') THEN
            IPPCOUNT=IPPCOUNT+1
            CYCLE
          END IF
          CLINE=ADJUSTL(CLINE)
          ATEMP20=CLINE(1:20)
          CALL LOWCAS(ATEMP20)
          IF(ATEMP20(1:7).EQ.'command')THEN
            IF(DISTRIB_COMLIN.NE.' ')THEN
              WRITE(ERRMSG,76062) AALINE(1:MMLINE)
76062         FORMAT(A,'duplicate command supplied in the ',
     +        '"distribution files" section.')
              CALL WRTERR
              CYCLE
            END IF
            ATEMP20=ADJUSTL(CLINE(8:))
            IF(ATEMP20(1:1).EQ.'=')THEN
              CLINE=ADJUSTL(CLINE(8:))
              CLINE=ADJUSTL(CLINE(2:))
              DISTRIB_COMLIN=CLINE
              IF(DISTRIB_COMLIN(1:1).EQ.'"')THEN
                DISTRIB_COMLIN=DISTRIB_COMLIN(2:)
                J=INDEX(DISTRIB_COMLIN,'"')
                IF(J.EQ.0)THEN
                  WRITE(ERRMSG,76091) AALINE(1:MMLINE)
76091             FORMAT(A,'unbalanced quotes supplied with ',
     +            '"distribution files" command line.')
                  CALL WRTERR
                  DISTRIB_COMLIN='####'
                  CYCLE
                ELSE IF(J.EQ.1)THEN
                  DISTRIB_COMLIN='####'
                  WRITE(ERRMSG,7610) AALINE(1:MMLINE)
7610              FORMAT(A,'blank command line.')
                  DISTRIB_COMLIN='####'
                  CALL WRTERR
                  CYCLE
                ELSE
                  DISTRIB_COMLIN=DISTRIB_COMLIN(1:J-1)
                END IF
              ELSE
                IF(DISTRIB_COMLIN.EQ.' ')THEN
                  WRITE(ERRMSG,7610) AALINE(1:MMLINE)
                  DISTRIB_COMLIN='####'
                  CALL WRTERR
                  CYCLE
                END IF
              END IF
            ELSE
              WRITE(ERRMSG,76131) AALINE(1:MMLINE)
76131         FORMAT(A,'an "=" character must follow the "command" ',
     +        'string.')
              CALL WRTERR
              DISTRIB_COMLIN='####'
            END IF
            CYCLE
          END IF
          NUM_DISTRIB_FILES=NUM_DISTRIB_FILES+1
          IF(NUM_DISTRIB_FILES.GT.MAX_DISTRIB_FILES)THEN
            CALL WRTINT(ATEMP20,MAX_DISTRIB_FILES)
            WRITE(ERRMSG,76141) TRIM(ATEMP20)
76141       FORMAT(' Too many files cited in "distribution files" ',
     +      'section of PEST control file. As presently ',
     +      'programmed only ',A,' are allowed.')
            CALL WRTERR
          END IF
          JJ=0
          DO I=1,LEN_TRIM(CLINE)
            IF(CLINE(I:I).EQ.'"')JJ=JJ+1
          END DO
          IF((JJ/2)*2.NE.JJ)THEN
            WRITE(ERRMSG,7615) AALINE(1:MMLINE)
7615        FORMAT(A,'unbalanced quotes.')
            CALL WRTERR
            CYCLE
          END IF
          CALL SPACESUB(CLINE)
          CALL LINSPL(IFAIL,3,LW,RW,CLINE)
          IF(IFAIL.NE.0)THEN
            WRITE(ERRMSG,76161) AALINE(1:MMLINE)
76161       FORMAT(A,'insufficient entries on this line.')
            CALL WRTERR
            CYCLE
          END IF
          CALL INTREAD(IFAIL,CLINE(LW(1):RW(1)),ITEMP)
          IF(IFAIL.NE.0)THEN
            WRITE(ERRMSG,76181) AALINE(1:MMLINE)
76181       FORMAT(A,'cannot read file distribution type.')
            CALL WRTERR
            CYCLE
          END IF
          IF(NUM_DISTRIB_FILES.LE.MAX_DISTRIB_FILES) THEN
            DISTRIB_TYPE(NUM_DISTRIB_FILES)=ITEMP
            IF((DISTRIB_TYPE(NUM_DISTRIB_FILES).NE.1).AND.
     +         (DISTRIB_TYPE(NUM_DISTRIB_FILES).NE.2))THEN
               WRITE(ERRMSG,7620) AALINE(1:MMLINE)
7620           FORMAT(A,'file distribution type must be 1 or 2.')
               CALL WRTERR
               CYCLE
            END IF
            SOURCEFILE_AGENT(NUM_DISTRIB_FILES)=CLINE(LW(2):RW(2))
            CALL REMCHAR(SOURCEFILE_AGENT(NUM_DISTRIB_FILES),CHAR(211))
            TARGETFILE_MASTER(NUM_DISTRIB_FILES)=CLINE(LW(3):RW(3))
            CALL REMCHAR(TARGETFILE_MASTER(NUM_DISTRIB_FILES),
     +      CHAR(211))
            IF(DISTRIB_TYPE(NUM_DISTRIB_FILES).EQ.1) CYCLE
            CALL LINSPL(IFAIL,5,LW,RW,CLINE)
            IF(IFAIL.NE.0)THEN
              WRITE(ERRMSG,76221) AALINE(1:MMLINE)
76221         FORMAT(A,'four filenames must be supplied if ',
     +        'DISTRIB_TYPE is 2.')
              CALL WRTERR
              CYCLE
            END IF
            SOURCEFILE_MASTER(NUM_DISTRIB_FILES)=CLINE(LW(4):RW(4))
            CALL REMCHAR(SOURCEFILE_MASTER(NUM_DISTRIB_FILES),
     +      CHAR(211))
            TARGETFILE_AGENT(NUM_DISTRIB_FILES)=CLINE(LW(5):RW(5))
            CALL REMCHAR(TARGETFILE_AGENT(NUM_DISTRIB_FILES),
     +      CHAR(211))
          END IF
        END DO
7650    CONTINUE
C        IF((NUM_DISTRIB_FILES.NE.0).AND.(JACUPDATE.NE.0))THEN
C          WRITE(ERRMSG,7660)
C7660      FORMAT(' If JACUPDATE is greater than zero, then a ',
C     +    '"* distribution files" section is not allowed in the PEST ',
C     +    'control file, or must contain only a system command.')
C          CALL WRTERR
C        END IF
        IF(NUM_DISTRIB_FILES.GT.MAX_DISTRIB_FILES)
     +     NUM_DISTRIB_FILES=MAX_DISTRIB_FILES
        IF(NUM_DISTRIB_FILES.GT.1)THEN
          DO I=1,NUM_DISTRIB_FILES-1
            AFILE=TARGETFILE_MASTER(I)
            IF(AFILE.NE.' ')THEN
              ATEMP50='TARGETFILE_MANAGER'
              DO J=I+1,NUM_DISTRIB_FILES
                IF(TARGETFILE_MASTER(J).EQ.AFILE) THEN
                  WRITE(ERRMSG,7670) TRIM(ATEMP50),TRIM(AFILE)
7670              FORMAT(' Duplicate ',a,' filename "',A,
     +            '" in "distribution files" section of PEST ',
     +            'control file.')
                  CALL WRTERR
                  GO TO 7680
                END IF
              END DO
            END IF
7680        CONTINUE
            AFILE=TARGETFILE_AGENT(I)
            IF(AFILE.NE.' ')THEN
              ATEMP50='TARGETFILE_AGENT'
              DO J=I+1,NUM_DISTRIB_FILES
                IF(TARGETFILE_AGENT(J).EQ.AFILE) THEN
                  WRITE(ERRMSG,7670) TRIM(ATEMP50),TRIM(AFILE)
                  CALL WRTERR
                  GO TO 7690
                END IF
              END DO
            END IF
          END DO
        END IF
7690    CONTINUE
        DO
          ILINE=ILINE+1
          READ(30,'(A)',ERR=7999,END=7999)CLINE
          CALL PPROC(CLINE,IPPCOUNT)
          IF(CLINE.NE.' ')THEN
            WRITE(ERRMSG,7700)
7700        FORMAT(' If present, the "distribution files" section ',
     +      'should be the last data present in the PEST control ',
     +      'file.')
            CALL WRTERR
            GO TO 7999
          END IF
        END DO

7999    CONTINUE

C -- We see if there is a "distribution files" section.

        IF((NUM_DISTRIB_FILES.EQ.0).AND.(DISTRIB_COMLIN.EQ.' '))THEN
          DO
            ILINE=ILINE+1
            READ(30,'(A)',ERR=79999,END=79999) CLINE
            CALL LOWCAS(CLINE)
            IF(INDEX(CLINE,'* distribution files').NE.0) GO TO 7600
          END DO
        END IF
79999   CONTINUE

        IF(IERR.NE.0) GO TO 9800

C -- IF A JUPITER DERIVATIVES INTERFACE FILE IS SUPPLIED, THIS IS NOW CHECKED.

        IF(JACFILE.EQ.2)THEN
          IF(AJACFLE.EQ.' ') GO TO 9800
          OPEN(UNIT=36,FILE=AJACFLE,STATUS='OLD',IOSTAT=IFAIL)
          IF(IFAIL.NE.0)THEN
            WRITE(ERRMSG,7210) TRIM(AJACFLE)
7210        FORMAT(' Cannot open JUPITER derivatives ',
     +      'interface file ',A)
            CALL WRTERR
            GO TO 7950
          END IF

          KLINE=0
          DO
            KLINE=KLINE+1
            CALL WRTINT(ATEMP,KLINE)
            AALINE=' Line '//TRIM(ATEMP)//
     +      ' of JUPITER derivatives interface file '//
     +      TRIM(AJACFLE)//': '
            MMLINE=NBLNK(AALINE)+1
            READ(36,'(A)',ERR=7900,END=7920) CLINE
            IF(CLINE.EQ.' ') CYCLE
            CLINE=ADJUSTL(CLINE)
            IF(CLINE(1:1).EQ.'#') CYCLE
            CALL SPACESUB(CLINE)
            CALL LINSPL(IFAIL,1,LW,RW,CLINE)
            MODDERFILE=CLINE(LW(1):RW(1))
            CALL REMCHAR(MODDERFILE,CHAR(211))
            ATEMP1=AJACFLE
            ATEMP2=MODDERFILE
#ifndef UNIX
            CALL LOWCAS(ATEMP1)
            CALL LOWCAS(ATEMP2)
#endif
            IF(ATEMP1.EQ.ATEMP2)THEN
              WRITE(ERRMSG,7230)AALINE(1:MMLINE)
7230          FORMAT(A,'model-generated derivatives file DERFILE ',
     +        'must not have same name as JUPITER derivatives ',
     +        'interface file.')
               CALL WRTERR
            END IF
            EXIT
          END DO

          KLINE=KLINE+1
          CALL WRTINT(ATEMP,KLINE)
          AALINE=' Line '//TRIM(ATEMP)//
     +    ' of JUPITER derivatives interface file '//
     +    TRIM(AJACFLE)//': '
          MMLINE=NBLNK(AALINE)+1
          READ(36,'(A)',ERR=7900,END=7920) CLINE
          CALL LINSPL(IFAIL,1,LW,RW,CLINE)
          IF(IFAIL.NE.0)THEN
            WRITE(ERRMSG,7233) AALINE(1:MMLINE)
7233        FORMAT(A,'cannot read value for NSKIP variable.')
            CALL WRTERR
          ELSE
            CALL NUMRD(IFAIL,0,LW(1),RW(1),DI_NSKIP,RTEMP,CLINE)
            IF(IFAIL.NE.0)THEN
              WRITE(ERRMSG,7233) AALINE(1:MMLINE)
              CALL WRTERR
            ELSE
              IF(DI_NSKIP.LT.0)THEN
                WRITE(ERRMSG,7250) AALINE(1:MMLINE)
7250            FORMAT(A,'NSKIP must not be negative.')
                CALL WRTERR
              END IF
            END IF
          END IF

          KLINE=KLINE+1
          CALL WRTINT(ATEMP,KLINE)
          AALINE=' Line '//TRIM(ATEMP)//
     +    ' of JUPITER derivatives interface file '//
     +    TRIM(AJACFLE)//': '
          MMLINE=NBLNK(AALINE)+1
          READ(36,'(A)',ERR=7900,END=7920) CLINE
          CALL LINSPL(IFAIL,2,LW,RW,CLINE)
          IF(IFAIL.NE.0)THEN
            WRITE(ERRMSG,7260) AALINE(1:MMLINE)
7260        FORMAT(A,'two entries expected on this line ',
     +      '- NDEP and NPAR.')
            CALL WRTERR
          ELSE
            CALL NUMRD(IFAIL,0,LW(1),RW(1),DI_NDEP,RTEMP,CLINE)
            IF(IFAIL.NE.0)THEN
              WRITE(ERRMSG,7270) AALINE(1:MMLINE)
7270          FORMAT(A,'cannot read value for NDEP variable.')
              CALL WRTERR
            ELSE
              IF(DI_NDEP.LE.0)THEN
                WRITE(ERRMSG,7280) AALINE(1:MMLINE)
7280            FORMAT(A,'NDEP must not be zero or negative.')
                CALL WRTERR
              ELSE IF(DI_NDEP.GT.NOBS)THEN
                WRITE(ERRMSG,7290) AALINE(1:MMLINE)
7290            FORMAT(A,'NDEP must not be greater than ',
     +          'number of observations.')
                CALL WRTERR
              END IF
            END IF
            CALL NUMRD(IFAIL,0,LW(2),RW(2),DI_NPAR,RTEMP,CLINE)
            IF(IFAIL.NE.0)THEN
              WRITE(ERRMSG,7300) AALINE(1:MMLINE)
7300          FORMAT(A,'cannot read value for NPAR variable.')
              CALL WRTERR
            ELSE
              IF(DI_NPAR.LE.0)THEN
                WRITE(ERRMSG,7310) AALINE(1:MMLINE)
7310            FORMAT(A,'NPAR must not be zero or negative.')
                CALL WRTERR
              ELSE IF(DI_NPAR.GT.NPAR)THEN
                WRITE(ERRMSG,7320) AALINE(1:MMLINE)
7320            FORMAT(A,'NPAR must not be greater than ',
     +          'number of parameters.')
                CALL WRTERR
              END IF
            END IF
          END IF
          IF(IERR.NE.0) GO TO 7950

          ALLOCATE(DI_PAR(DI_NPAR),DI_PARENT(DI_NPAR),STAT=IALERR)
          IF(IALERR.NE.0) GO TO 9400
          ITEMP=MAX(NPAR,DI_NDEP)
          ALLOCATE(DI_TEMP(ITEMP),STAT=IALERR)
          IF(IALERR.NE.0) GO TO 9400

          KLINE=KLINE+1
          CALL WRTINT(ATEMP,KLINE)
          AALINE=' Line '//TRIM(ATEMP)//
     +    ' of JUPITER derivatives interface file '//
     +    TRIM(AJACFLE)//': '
          MMLINE=NBLNK(AALINE)+1
          READ(36,'(A)',ERR=7900,END=7920) CLINE
          CALL SPACESUB(CLINE)
          CALL LINSPL(IFAIL,1,LW,RW,CLINE)
          IF(IFAIL.NE.0)THEN
            WRITE(ERRMSG,7335) AALINE(1:MMLINE)
7335        FORMAT(A,'cannot read value for ORIENTATION variable.')
            CALL WRTERR
          ELSE
            DI_ORIENTATION=CLINE(LW(1):RW(1))
            CALL REMCHAR(DI_ORIENTATION,CHAR(211))
            CALL LOWCAS(DI_ORIENTATION)
            IF((INDEX(DI_ORIENTATION,'row/dep').EQ.0).AND.
     +         (INDEX(DI_ORIENTATION,'row/par').EQ.0))THEN
               WRITE(ERRMSG,7330) AALINE(1:MMLINE)
7330           FORMAT(A,'ORIENTATION must be "ROW/DEP or "ROW/PAR".')
               CALL WRTERR
            END IF
          END IF

          KLINE=KLINE+1
          CALL WRTINT(ATEMP,KLINE)
          AALINE=' Line '//TRIM(ATEMP)//
     +    ' of JUPITER derivatives interface file '//
     +    TRIM(AJACFLE)//': '
          MMLINE=NBLNK(AALINE)+1
          READ(36,'(A)',ERR=7900,END=7920) CLINE
          CALL SPACESUB(CLINE)
          CALL LINSPL(IFAIL,1,LW,RW,CLINE)
          IF(IFAIL.NE.0)THEN
            WRITE(ERRMSG,7334) AALINE(1:MMLINE)
7334        FORMAT(A,'cannot read DERFORMAT variable.')
            CALL WRTERR
          ELSE
            DI_DERFORMAT=CLINE(LW(1):RW(1))
            CALL REMCHAR(DI_DERFORMAT,CHAR(211))
          END IF

          KLINE=KLINE+1
          CALL WRTINT(ATEMP,KLINE)
          AALINE=' Line '//TRIM(ATEMP)//
     +    ' of JUPITER derivatives interface file '//
     +    TRIM(AJACFLE)//': '
          MMLINE=NBLNK(AALINE)+1
          READ(36,'(A)',ERR=7900,END=7920) CLINE
          CALL SPACESUB(CLINE)
          CALL LINSPL(IFAIL,1,LW,RW,CLINE)
          IF(IFAIL.NE.0)THEN
            WRITE(ERRMSG,7340) AALINE(1:MMLINE)
7340        FORMAT(A,'this line should contain only the word ',
     +      '"PARAMETERS".')
            CALL WRTERR
          ELSE
            ATEMP=CLINE(LW(1):RW(1))
            CALL REMCHAR(ATEMP,CHAR(211))
            CALL LOWCAS(ATEMP)
            IF(INDEX(ATEMP,'parameter').EQ.0)THEN
              WRITE(ERRMSG,7340) AALINE(1:MMLINE)
              CALL WRTERR
            END IF
          END IF

          ICOUNT=0
          READ(36,*,ERR=7935,END=7937) (DI_TEMP(I),I=1,DI_NPAR)
          DO IPAR=1,DI_NPAR
            ATEMP12=DI_TEMP(IPAR)
            CALL LOWCAS(ATEMP12)
            CALL WHICH1(IFAIL,NPAR,J,APAR,ATEMP12)
            IF(IFAIL.NE.0)THEN
              ICOUNT=ICOUNT+1
              IF(ICOUNT.GT.10) GO TO 7950
              WRITE(ERRMSG,7350) TRIM(ATEMP12),TRIM(AJACFLE)
7350          FORMAT(' Unrecognized parameter "',A,'" cited in ',
     +        'JUPITER derivatives interface file ',A,'.')
              CALL WRTERR
              CYCLE
            END IF
            DI_PAR(IPAR)=J
            DI_PARENT(IPAR)=0
            DO K=1,NPAR
              IF(-ITRANS(K).EQ.J)THEN
                DI_PARENT(IPAR)=DI_PARENT(IPAR)+1
              END IF
            END DO
          END DO

          READ(36,'(A)',ERR=7941,END=7943) CLINE
          CALL SPACESUB(CLINE)
          CALL LINSPL(IFAIL,1,LW,RW,CLINE)
          IF(IFAIL.NE.0)THEN
            WRITE(ERRMSG,7360) TRIM(AJACFLE)
7360        FORMAT(' A line containing the single entry "DEPENDENTS" ',
     +      'should follow the parameter list in the JUPITER ',
     +      'derivatives interface file ',A)
             CALL WRTERR
          ELSE
            ATEMP=CLINE(LW(1):RW(1))
            CALL REMCHAR(ATEMP,CHAR(211))
            CALL LOWCAS(ATEMP)
            IF(INDEX(ATEMP,'dependent').EQ.0)THEN
              WRITE(ERRMSG,7360) TRIM(AJACFLE)
              CALL WRTERR
            END IF
          END IF

          JCOUNT=0
          READ(36,*,ERR=7941,END=7943) (DI_TEMP(I),I=1,DI_NDEP)
          DO IDEP=1,DI_NDEP
            ATEMP20=DI_TEMP(IDEP)
            CALL LOWCAS(ATEMP20)
            CALL WHICH1(IFAIL,NOBS,J,AOBS,ATEMP20)
            IF(IFAIL.NE.0)THEN
              JCOUNT=JCOUNT+1
              IF(JCOUNT.GT.10) GO TO 7950
              WRITE(ERRMSG,7370) TRIM(ATEMP20),TRIM(AJACFLE)
7370          FORMAT(' Unrecognized dependent (observation) "',A,
     +        '" cited in JUPITER derivatives interface file ',A)
              CALL WRTERR
            END IF
          END DO

          IF((ICOUNT.GT.0).OR.(JCOUNT.GT.0)) GO TO 7950
          IF(IERR.NE.0) GO TO 7950

C -- It is now established that tied and parent parameters are both in,
C    or both out of the JUPITER derivatives file.

          DO I=1,DI_NPAR
            IF(DI_PARENT(I).EQ.0) CYCLE
            II=DI_PAR(I)
            DO J=1,NPAR
              IF(-ITRANS(J).EQ.II)THEN
                DO K=1,DI_NPAR
                  IF(DI_PAR(K).EQ.J) GO TO 7390
                END DO
                WRITE(ERRMSG,7380) TRIM(APAR(II))
7380            FORMAT(' Parameter "',A,'" is cited in the JUPITER ',
     +          'derivatives interface file. Parameters which are ',
     +          'tied to this parameter must also be cited in this ',
     +          'file. Alternatively, they must not be tied to this ',
     +          'parameter.')
                CALL WRTERR
                GO TO 7400
7390            CONTINUE
              END IF
            END DO
7400        CONTINUE
          END DO

          DO I=1,DI_NPAR
            II=DI_PAR(I)
            JJ=ITRANS(II)
            IF((JJ.LT.0).AND.(JJ.GT.-10000))THEN
              JJ=-JJ
              DO J=1,DI_NPAR
                IF(DI_PAR(J).EQ.JJ)GO TO 7410
              END DO
              WRITE(ERRMSG,7405) TRIM(APAR(II)),TRIM(AJACFLE)
7405          FORMAT(' Parameter "',A,'" is a tied parameter which ',
     +        'is cited in the JUPITER derivatives interface file ',
     +        A,'. However the parameter to which it is tied is not ',
     +        'also cited within this file.')
              CALL WRTERR
7410          CONTINUE
            END IF
          END DO

C -- Other tests are made to ensure integrity of the JUPITER derivatives
C    interface file.

          IF(DI_NDEP.LT.NOBS)THEN
            DO I=1,NPAR
              IF(ITRANS(I).GE.0)THEN
                IF(DERCOM(I).EQ.0)THEN
                  WRITE(ERRMSG,7420)
7420              FORMAT(' If a JUPITER-protocol external ',
     +            'derivatives ',
     +            'file is supplied in which some dependents ',
     +            '(ie. observations) are ommitted, then DERCOM ',
     +            'must be non-zero for all adjustable parameters.')
                  CALL WRTERR
                  GO TO 7500
                END IF
              END IF
            END DO
          ELSE
            IF(DI_NPAR.LT.NPAR)THEN
              DO I=1,NPAR
                DI_TEMP(I)=' '
              END DO
              DO I=1,DI_NPAR
                II=DI_PAR(I)
                DI_TEMP(II)='x'
              END DO
              DO I=1,NPAR
                IF(ITRANS(I).LT.0) CYCLE
                IF(DI_TEMP(I).EQ.' ')THEN
                  IF(DERCOM(I).EQ.0)THEN
                    WRITE(ERRMSG,7430) TRIM(APAR(I)),TRIM(AJACFLE)
7430                 FORMAT(' Parameter "',A,'" is not cited in ',
     +              'JUPITER derivatives interface file ',A,
     +              '; therefore it must have a non-zero DERCOM ',
     +              'value.')
                    CALL WRTERR
                  END IF
                ELSE
                  IF(DERCOM(I).NE.0)THEN
                    WRITE(ERRMSG,7440) TRIM(AJACFLE),TRIM(APAR(I))
7440                 FORMAT(' All dependents (observations) in ',
     +              'PEST control file are cited in JUPITER ',
     +              'derivatives interface file ',A,'. Adjustable ',
     +              'parameter "',A,
     +              '" cited in this file must have DERCOM value ',
     +              'of zero.')
                    CALL WRTERR
                  END IF
                END IF
              END DO
            ELSE
              DO I=1,NPAR
                IF(ITRANS(I).GT.-1000000)THEN
                  IF(DERCOM(I).NE.0)THEN
                    WRITE(ERRMSG,7450)
7450                FORMAT(' All dependents (observations) and ',
     +              'adjustable parameters in PEST control file ',
     +              'are cited in JUPITER derivatives interface ',
     +              'file. All parameters (except for fixed ones) '
     +              'must therefore have a DERCOM value of zero.')
                    CALL WRTERR
                    GO TO 7500
                  END IF
                END IF
              END DO
            END IF
          END IF

7500      CONTINUE
          GO TO 7950

7900      WRITE(ERRMSG,7910) AALINE(1:MMLINE)
7910      FORMAT(A,'cannot read line.')
          CALL WRTERR
          GO TO 7950
7920      WRITE(ERRMSG,7930) AALINE(1:MMLINE)
7930      FORMAT(A,'unexpected end to file.')
          CALL WRTERR
          GO TO 7950
7935      WRITE(ERRMSG,7936) TRIM(AJACFLE)
7936      FORMAT(' Error encountered while reading parameter ',
     +    'list from JUPITER derivatives interface file ',A)
          CALL WRTERR
          GO TO 7950
7937      WRITE(ERRMSG,7938) TRIM(AJACFLE)
7938      FORMAT(' Unexpected end encountered to JUPITER ',
     +    'derivatives interface file ',A,' while reading parameter ',
     +    'list.')
          CALL WRTERR
          GO TO 7950
7941      WRITE(ERRMSG,7942) TRIM(AJACFLE)
7942      FORMAT(' Error encountered while reading dependents ',
     +    '(ie. observations) from JUPITER derivatives ',
     +    'interface file ',A)
          CALL WRTERR
          GO TO 7950
7943      WRITE(ERRMSG,7944) TRIM(AJACFLE)
7944      FORMAT(' Unexpected end encountered to JUPITER ',
     +    'derivatives interface file ',A,' while reading ',
     +    'dependents (ie. observations).')
          CALL WRTERR
          GO TO 7950

7950      CLOSE(UNIT=36,IOSTAT=IFAIL)
        END IF

C -- COVARIANCE MATRIX FILES ARE NOW CHECKED

7951    CONTINUE
        IF(NOBSGP1.EQ.-999) GO TO 7998
        IF(NUMCOV.EQ.0) GO TO 7998
        IF(CHECKFLAG.EQ.1) GO TO 7998

        MXOBSCOV=0
        DO 6100 IGROUP=1,NOBSGP
          IF(COVFLE(IGROUP).EQ.' ') GO TO 6100
          IF(IREG.NE.0)THEN
            IF(AOBSGP(IGROUP)(1:5).NE.'regul') IMCOVFLAG=1
          ELSE
            IMCOVFLAG=1
          END IF
          ICOUNT=0
          DO 6110 I=1,OBNDIM
            IF(IOBGNM(I).EQ.IGROUP) THEN
              ICOUNT=ICOUNT+1
            END IF
6110      CONTINUE
          IF(ICOUNT.GT.MXOBSCOV)MXOBSCOV=ICOUNT
6100    CONTINUE
        IF(MXOBSCOV.EQ.0) GO TO 7998

        ALLOCATE(COVAR(MXOBSCOV,MXOBSCOV),STAT=IALERR)
        IF(IALERR.NE.0) THEN
          WRITE(ERRMSG,6103)
6103      FORMAT(' Cannot allocate sufficient memory for ',
     +    'processing of covariance matrices to continue ',
     +    'execution.')
          CALL WRTERR
          GO TO 9800
        END IF

        DO 6300 IGROUP=1,NOBSGP
          IF(COVFLE(IGROUP).EQ.' ') GO TO 6300
          IF(NPRIOR.GT.0)THEN
            DO I=1,NPRIOR
              IF(IW_PR(I).GT.0)THEN
                IF(IOBGNM(NOBS+I).EQ.IGROUP)THEN
                  WRITE(ERRMSG,6104) TRIM(AOBSGP(IGROUP))
6104              FORMAT(' A covariance matrix must not be assigned ',
     +            'to observation group "',A,'" because this group ',
     +            'holds prior information which cites an ',
     +            'adaptive regularisation ("iw_") parameter.')
                  CALL WRTERR
                  GO TO 6300
                END IF
              END IF
            END DO
          END IF
          NNCOV=0
          DO 6150 I=1,OBNDIM
            IF(IOBGNM(I).EQ.IGROUP)THEN
              NNCOV=NNCOV+1
            END IF
6150      CONTINUE
          IF(NNCOV.EQ.0) GO TO 6300
          OPEN(UNIT=35,FILE=COVFLE(IGROUP),STATUS='OLD',IOSTAT=JERR)
          IF(JERR.NE.0)THEN
            WRITE(ERRMSG,6160)
     +      COVFLE(IGROUP)(1:NBLNK(COVFLE(IGROUP))),
     +      AOBSGP(IGROUP)(1:NBLNK(AOBSGP(IGROUP)))
6160        FORMAT(' Cannot open covariance matrix file ',A,
     +      ' for observation group "',A,'".')
            CALL WRTERR
            GO TO 6300
          END IF

C -- Is the file a "matrix file"? If so it should be read as such.

          MAYBE_PLPROC=.FALSE.
          READ(35,'(A)',ERR=6180,END=6196) CLINE
          IF(CLINE.EQ.' ') GO TO 6169
          CALL LINSPL(IFAIL,4,LW,RW,CLINE)
          IF(IFAIL.EQ.0)THEN
            CALL NUMRD(IFAIL,1,LW(4),RW(4),ITEMP,RTEMP,CLINE)
            IF(IFAIL.EQ.0) GO TO 6169
          END IF
          CALL LINSPL(IFAIL,3,LW,RW,CLINE)
          IF(IFAIL.NE.0) GO TO 6169
          CALL NUMRD(IFAIL,0,LW(1),RW(1),NNDIM1,RTEMP,CLINE)
          IF(IFAIL.NE.0) GO TO 6169
          IF(NNDIM1.LE.0) GO TO 6169
          CALL NUMRD(IFAIL,0,LW(2),RW(2),NNDIM2,RTEMP,CLINE)
          IF(IFAIL.NE.0) GO TO 6169
          IF(NNDIM2.LE.0) GO TO 6169
          CALL NUMRD(IFAIL,0,LW(3),RW(3),NNCODE,RTEMP,CLINE)
          IF(IFAIL.NE.0) GO TO 6169
          IF((NNCODE.NE.-1).AND.(NNCODE.NE.1).AND.(NNCODE.NE.2))
     +    GO TO 6169
          DO
            READ(35,'(A)',ERR=6180,END=814) CLINE
            CLINE=ADJUSTL(CLINE)
            IF(CLINE(1:1).EQ.'*') GO TO 812
          END DO
814       CONTINUE
          IF(NNCODE.EQ.2) GO TO 6169
          MAYBE_PLPROC=.TRUE.
          GO TO 8121
812       CALL LOWCAS(CLINE)
          IF((INDEX(CLINE,'row').EQ.0).AND.
     +       (INDEX(CLINE,'name').EQ.0)) GO TO 6169
8121      CONTINUE
          REWIND(UNIT=35)
          READ(35,'(A)',ERR=6180,END=6196) CLINE
          IF(NNDIM1.NE.NNDIM2)THEN
            WRITE(ERRMSG,815) TRIM(COVFLE(IGROUP))
815         FORMAT(' File ',A,' appears to be in PEST or PLPROC ',
     +      'matrix file format. The matrix contained in it is ',
     +      'not square.')
            CALL WRTERR
            GO TO 6290
          END IF
          IF(NNDIM1.NE.NNCOV)THEN
            CALL WRTINT(ATEMP,NNCOV)
            WRITE(ERRMSG,816) TRIM(COVFLE(IGROUP)),TRIM(ATEMP)
816         FORMAT(' File ',A,' appears to be in PEST or PLPROC ',
     +      'matrix file format. The dimensions of the matrix ',
     +      'contained in it are not the same as the number of ',
     +      'observations, ',A,', in the observation group to which ',
     +      'it is assigned.')
            CALL WRTERR
            GO TO 6290
          END IF
          IF(NNCODE.GT.0)THEN
            DO I=1,NNCOV
              READ(35,*,ERR=6161,END=6163) (COVAR(I,J),J=1,NNCOV)
            END DO
          ELSE
            DO I=1,NNCOV
              DO J=1,NNCOV
                COVAR(J,I)=0.0D0
              END DO
            END DO
            DO I=1,NNCOV
              READ(35,*,ERR=6161,END=6163) COVAR(I,I)
            END DO
          END IF
          GO TO 6119
6161      WRITE(ERRMSG,6162) TRIM(COVFLE(IGROUP))
6162      FORMAT(' File ',A,' appears to be in PEST matrix file ',
     +    'format. Error reading covariance matrix from this file.')
          CALL WRTERR
          GO TO 6290
6163      WRITE(ERRMSG,6164) TRIM(COVFLE(IGROUP))
6164      FORMAT(' File ',A,' appears to be in PEST matrix file ',
     +    'format. Unexpected end encountered to this file. ')
          CALL WRTERR
          GO TO 6290
6169      CONTINUE
          REWIND(UNIT=35)
          DO 6170 I=1,NNCOV
            READ(35,*,ERR=6180,END=6190) (COVAR(I,J),J=1,NNCOV)
6170      CONTINUE
          GO TO 6200
6180      WRITE(ERRMSG,6185) COVFLE(IGROUP)(1:NBLNK(COVFLE(IGROUP)))
6185      FORMAT(' Error encountered when reading covariance matrix ',
     +    'from file ',A,'.')
          CALL WRTERR
          GO TO 6290
6190      CALL WRTINT(ATEMP,NNCOV)
          WRITE(ERRMSG,6195) COVFLE(IGROUP)(1:NBLNK(COVFLE(IGROUP))),
     +    ATEMP(1:NBLNK(ATEMP)),ATEMP(1:NBLNK(ATEMP)),
     +    ATEMP(1:NBLNK(ATEMP)),AOBSGP(IGROUP)(1:NBLNK(AOBSGP(IGROUP)))
6195      FORMAT(' Insufficient elements in covariance matix file ',
     +    A,': matrix must be ',A,'x',A,' because there are ',A,
     +    ' observations in obervation group "',A,'".')
          CALL WRTERR
          GO TO 6290
6196      WRITE(ERRMSG,6197) COVFLE(IGROUP)(1:NBLNK(COVFLE(IGROUP)))
6197      FORMAT(' Unexpected end encountered to covariance matrix ',
     +    'file ',a,'.')
          CALL WRTERR
          GO TO 6290
6200      CONTINUE
          DO
            READ(35,'(A)',ERR=6119,END=6119) CLINE
            CLINE=ADJUSTL(CLINE)
            IF(CLINE(1:1).EQ.'*')THEN
              CALL LOWCAS(CLINE)
              IF((INDEX(CLINE,'row').NE.0).OR.
     +           (INDEX(CLINE,'name').NE.0)) GO TO 6180
            END IF
          END DO
6119      CONTINUE
          IF(NNCOV.GT.1)THEN
            DO 6220 I=2,NNCOV
              DO 6210 J=1,I-1
                RTEMP1=COVAR(J,I)
                RTEMP2=COVAR(I,J)
                IF(RTEMP1.EQ.0.0D0)THEN
                  IF(ABS(RTEMP2).GT.1.0D-20) GO TO 6250
                ELSE IF(RTEMP2.EQ.0.0D0)THEN
                  IF(ABS(RTEMP1).GT.1.0D-20) GO TO 6250
                ELSE
                  IF(ABS(RTEMP1-RTEMP2).GT.
     +            1.0E-5*0.5*(ABS(RTEMP1)+ABS(RTEMP2)))GO TO 6250
                END IF
6210          CONTINUE
6220        CONTINUE
            GO TO 6260
6250        WRITE(ERRMSG,6255) COVFLE(IGROUP)(1:NBLNK(COVFLE(IGROUP)))
6255        FORMAT(' The covariance matrix provided in file ',A,
     +      ' is not symmetrical.')
            CALL WRTERR
          END IF
6260      DO 6280 I=1,NNCOV
            IF(COVAR(I,I).LE.0.0D0)THEN
              WRITE(ERRMSG,6270) COVFLE(IGROUP)
     +        (1:NBLNK(COVFLE(IGROUP)))
6270          FORMAT(' The diagonal elements of the covariance matrix ',
     +        'provided in file ',A,' are not all positive.')
              CALL WRTERR
              GO TO 6285
            END IF
6280      CONTINUE
6285      CONTINUE
          IF(IPRED.NE.0)THEN
            IF(IGROUP.EQ.JPREDICT)THEN
              WRITE(ERRMSG,6827)
6827          FORMAT(' A covariance matrix must not be supplied ',
     +        'for observation group "predict" when PEST is run in ',
     +        'predictive analysis mode.')
              CALL WRTERR
            END IF
          END IF
6290      CLOSE(UNIT=35,IOSTAT=JERR)
6300    CONTINUE

        IF(NPRIOR.GT.0)THEN
          DO 6350 IGROUP=1,NOBSGP
            IF(COVFLE(IGROUP).EQ.' ') GO TO 6350
            DO 6340 I=NOBS+1,OBNDIM
              IF(IOBGNM(I).EQ.IGROUP) GO TO 6360
6340        CONTINUE
            GO TO 6350
6360        DO 6366 I=1,NOBS
              IF(IOBGNM(I).EQ.IGROUP) THEN
                WRITE(ERRMSG,6365)
     +          AOBSGP(IGROUP)(1:NBLNK(AOBSGP(IGROUP)))
6365            FORMAT(' If a covariance matrix is supplied for an ',
     +          'observation group, members of that group can be ',
     +          'observations or prior information, but not both. ',
     +          'This is violated for observation group "',A,'".')
                CALL WRTERR
                GO TO 6350
              END IF
6366        CONTINUE
6350      CONTINUE
        END IF

        IF(IDOSENREUSE.EQ.1)THEN
          IF(NPRIOR.EQ.0)THEN
            WRITE(ERRMSG,63501)
63501       FORMAT(' Sensitivity re-use functionality must not be ',
     +      'activated if observation covariance matrices are ',
     +      'assigned in PEST control file to observation groups ',
     +      'which are not exclusively prior information.')
            CALL WRTERR
            GO TO 6370
          ELSE
            DO IGROUP=1,NOBSGP
              IF(COVFLE(IGROUP).NE.' ') THEN
                DO J=1,NOBS
                  IF(IOBGNM(J).EQ.IGROUP) THEN
                    WRITE(ERRMSG,63501)
                    CALL WRTERR
                    GO TO 6370
                  END IF
                END DO
              END IF
            END DO
          END IF
        END IF
6370    CONTINUE

C -- NOW THAT ALL OF THE PEST CONTROL FILE HAS BEEN READ, FINAL CHECKS ARE
C -- MADE FOR NON-FATAL ERRORS

7998    CONTINUE
        IF(IERR.NE.0) THEN
          GO TO 9800
        ELSE
          IF(CHECKFLAG.EQ.1) THEN
            WRITE(6,6351,ERR=9900)
6351        FORMAT(' No errors encountered.')
            GO TO 9998
          ELSE
          WRITE(6,8000,ERR=9900)
8000      FORMAT(' No errors encountered.',/,/,' Warnings ----->')
          END IF
        END IF

        IF(SIMINCFLAG.NE.0)THEN
          WRITE(ERRMSG,80001) 'simultaneous parameter increments'
80001     FORMAT(' It appears that the PEST control file contains a "',
     +    A,'" section. PESTCHEK did not check this. It can only be ',
     +    'used by PEST_HP.')
          CALL WRTERR
        END IF
        IF(RANDOMFLAG.NE.0)THEN
          WRITE(ERRMSG,80001) 'randomized jacobian'
          CALL WRTERR
        END IF
        IF(RSIFLAG.NE.0)THEN
          WRITE(ERRMSG,80002) 'rsi'
80002     FORMAT(' It appears that the PEST control file contains a "',
     +    A,'" section. PESTCHEK did not check this. It can only be ',
     +    'used by RSI_HP.')
          CALL WRTERR
        END IF
        IF(RUN_ABANDON_FAC.GT.0.0)THEN
          ATEMP25='RUN_ABANDON_FAC'
          WRITE(ERRMSG,17089) TRIM(ATEMP25)
17089     FORMAT(' Only PEST_HP, or PEST when run with the ',
     +    '"/hpstart" command line switch, can accept a value for ',
     +    'the ',A,' control variable.')
          CALL WRTERR
        END IF
        IF(WIN_MRUN_HOURS.GT.0.0)THEN
          ATEMP25='WIN_MRUN_HOURS'
          WRITE(ERRMSG,17089) TRIM(ATEMP25)
          CALL WRTERR
        END IF
        IF(SOFTSTOPHOURS.GT.0.0)THEN
          ATEMP25='SOFTSTOPHOURS'
          WRITE(ERRMSG,17089) TRIM(ATEMP25)
          CALL WRTERR
        END IF
        IF(HARDSTOPHOURS.GT.0.0)THEN
          ATEMP25='HARDSTOPHOURS'
          WRITE(ERRMSG,17089) TRIM(ATEMP25)
          CALL WRTERR
        END IF
        IF(WRITE_RRF.NE.-9999)THEN
          ATEMP25='RRFSAVE'
          WRITE(ERRMSG,17089) TRIM(ATEMP25)
          CALL WRTERR
        END IF
        IF(IZEROSENVAL.GT.0)THEN
          ATEMP25='ZEROSENVAL'
          WRITE(ERRMSG,17089) TRIM(ATEMP25)
          CALL WRTERR
        END IF
        IF(ORR_NOT_FIRST.NE.0)THEN
          ATEMP25='ORR_NOT_FIRST'
          WRITE(ERRMSG,17089) TRIM(ATEMP25)
          CALL WRTERR
        END IF
        IF(UPTESTLIM.GT.0)THEN
          ATEMP25='UPTESTLIM'
          WRITE(ERRMSG,17089) TRIM(ATEMP25)
          CALL WRTERR
        END IF
        IF(UPTESTMIN.GT.0)THEN
          ATEMP25='UPTESTMIN'
          WRITE(ERRMSG,17089) TRIM(ATEMP25)
          CALL WRTERR
        END IF
        IF(REG2MEASRAT.GT.0.0D0)THEN
          ATEMP25='REG2MEASRAT'
          WRITE(ERRMSG,17089) TRIM(ATEMP25)
          CALL WRTERR
        END IF
        IF(JCOWARNTHRESH.GT.0.0D0)THEN
          ATEMP25='JCOWARNTHRESH'
          WRITE(ERRMSG,17089) TRIM(ATEMP25)
          CALL WRTERR
        END IF
        IF(JCOZEROTHRESH.GT.0.0D0)THEN
          ATEMP25='JCOZEROTHRESH'
          WRITE(ERRMSG,17089) TRIM(ATEMP25)
          CALL WRTERR
        END IF
        IF(RSQR_ITNLIM.NE.-999)THEN
          WRITE(ERRMSG,17094)
17094     FORMAT(' Only PEST_HP, or PEST when run with the ',
     +    '"/hpstart" command line switch, can accept values for ',
     +    'alternative LSQR control variables.')
          CALL WRTERR
        END IF
        IF(NPARSEC.GT.0)THEN
          ATEMP25='secondary parameters'
          WRITE(ERRMSG,17095) TRIM(ATEMP25)
17095     FORMAT(' Only programs of the HP suite can accept ',A,'.')
          CALL WRTERR
        END IF
        IF(NPARFILE.GT.0)THEN
          ATEMP25='file parameters'
          WRITE(ERRMSG,17095) TRIM(ATEMP25)
          CALL WRTERR
        END IF
C        IF(RUN_SLOW_FAC.GT.0.0)THEN
C          WRITE(ERRMSG,17089)
C17089     FORMAT(' A value is supplied for RUN_SLOW_FAC. Only ',
C     +    'PEST_HP can read this from the PEST control file. BEOPEST',
C     +    ' and Parallel PEST must read this from the parallel ',
C     +    'run management file.')
C          CALL WRTERR
C        END IF
        IF(MEMSAVE.EQ.1)THEN
          WRITE(ERRMSG,17085)
17085     FORMAT(' If running PEST_HP, be aware that it does not ',
     +    'accept a setting of "memsave".')
          CALL WRTERR
        END IF
        IF(ISECWARN.NE.0)THEN
          WRITE(ERRMSG,17099)
17099     FORMAT(' The PEST control file cites secondary ',
     +    'parameters. However none of these parameters are ',
     +    'cited in a template file.')
          CALL WRTERR
        END IF
        IF(IPRIMWARN.NE.0)THEN
          WRITE(ERRMSG,17098)
17098     FORMAT(' At least one primary parameter that is cited ',
     +    'in the "* parameter data" section of the PEST control ',
     +    'file does not appear in a template file.')
          CALL WRTERR
        END IF
        IF(IBOUNDSCALE.NE.0)THEN
          IF(ISVDA.NE.0)THEN
            WRITE(ERRMSG,17091)
17091       FORMAT(' Parameter bounds scaling cannot be activated ',
     +      'through the BOUNDSCALE variable if PEST implements ',
     +      'SVDA. However base parameter scaling will be employed ',
     +      'in calculating super parameters from base parameters. ',
     +      'Ensure that their bounds in base parameter control ',
     +      'file represent their pre-calibration uncertainties.')
            CALL WRTERR
          ELSE
            WRITE(ERRMSG,17092)
17092       FORMAT(' Parameter bounds scaling has been activated. ',
     +      'Ensure that parameter bounds represent pre-calibration ',
     +      'parameter uncertainties.')
            CALL WRTERR
            IF(JACUPDATE.NE.0)THEN
              WRITE(ERRMSG,17093)
17093         FORMAT(' Unless PEST_HP is used, or PEST is run with ',
     +        'the "/hpstart" switch, Broyden Jacobian updating ',
     +        'must be de-activated if parameter bounds scaling is ',
     +        'activated.')
              CALL WRTERR
            END IF
          END IF
        END IF

        IF(QUICKPRIOR.EQ.1)THEN
          WRITE(ERRMSG,1709)
1709      FORMAT(' Indexed prior information is supplied. Many PEST ',
     +    'utilities will report an error when they read a negative ',
     +    'NPRIOR value and encounter indexed prior information, as ',
     +    'they have not yet been programmed to recognise this.')
          CALL WRTERR
        END IF

        IF(FLAG_ZERO_WEIGHT.EQ.1)THEN
          WRITE(ERRMSG,1710)
1710      FORMAT(' There are no observations with a non-zero weight.')
          CALL WRTERR
        END IF

        NESPAR=0
        DO I=1,NPAR
          IF(ITRANS(I).GE.0)NESPAR=NESPAR+1
        END DO
        IF((IPRED.NE.0).OR.(IREG.NE.0)) GO TO 8003

        IF(IFINFLAG.EQ.1)THEN
          WRITE(ERRMSG,8002) CASFL(:NBLNK(CASFL))
8002      FORMAT(' NPRIOR provided as zero yet file ',A,
     +    ' appears to contain prior information (not checked by ',
     +    'PESTCHEK).')
          CALL WRTERR
          GO TO 8003
        ELSE
          IF((NPRIOR.NE.0).AND.(IFINFLAG.NE.2).AND.(IFINFLAG.NE.3).AND.
     +       (IFINFLAG.NE.4))THEN
8001        ILINE=ILINE+1
            READ(30,'(A)',ERR=9600,END=8003) CLINE
            CALL PPROC(CLINE,IPPCOUNT)
            IF(CLINE.NE.' ')THEN
              CALL WRTINT(ATEMP,NPRIOR)
              WRITE(ERRMSG,8004) ATEMP(:NBLNK(ATEMP)),
     +        CASFL(:NBLNK(CASFL))
8004          FORMAT(' NPRIOR provided as ',A,' yet file ',A,
     +       ' appears to contain more articles of prior information ',
     +       'than this; only the first NPRIOR have been checked by ',
     +       'PESTCHEK and will be used by PEST.')
              CALL WRTERR
              GO TO 8003
            ELSE
              GO TO 8001
            END IF
          END IF
        END IF

8003    CONTINUE

        IF((IPRED.EQ.0).AND.(IFINFLAG.EQ.2))THEN
          WRITE(ERRMSG,8014)
8014      FORMAT(' Variable PESTMODE was not set to "predictive ',
     +    'analysis"; however there appears to be predictive ',
     +    'analysis information contained in PEST control file.')
          CALL WRTERR
        END IF
        IF((IREG.EQ.0).AND.(IFINFLAG.EQ.3))THEN
          WRITE(ERRMSG,80121)
80121     FORMAT(' Variable PESTMODE was not set to ',
     +    '"regularisation"; however there appears to be ',
     +    'regularisation information in PEST control file.')
          CALL WRTERR
        END IF
        IF((PESTMODE.NE.4).AND.(IFINFLAG.EQ.4))THEN
          WRITE(ERRMSG,8012)
8012      FORMAT(' Variable PESTMODE was not set to ',
     +    '"pareto"; however there appears to be ',
     +    'pareto control information in PEST control file.')
          CALL WRTERR
        END IF

        IF(DERZEROLIM.GT.1.0D0)THEN
          WRITE(ERRMSG,8019)
8019      FORMAT(' Real variable DERZEROLIM is set unusually high.')
          CALL WRTERR
        END IF

        IF(NUMRUN.LT.-1)THEN
          WRITE(ERRMSG,80197)
80197     FORMAT(' NUMLAM is supplied as negative. This will be ',
     +    'reset to positive. If Parallel PEST or BEOPEST is used ',
     +    'PARLAM will automatically be set to -9999.')
          CALL WRTERR
        END IF
        IF((NUMRUN.GT.1).AND.(JACUPDATE.NE.0).AND.
     +     (JACUPDATE.LT.NUMRUN))THEN
          IF(IBOUNDSCALE.EQ.0)THEN
          WRITE(ERRMSG,80191)
80191     FORMAT(' Consider setting JACUPDATE to 999 so that a ',
     +    'Broyden''s update can be undertaken on every occasion ',
     +    'that a parameter upgrade is tested (normal PEST) ',
     +    'or for every parameter upgrade direction (PEST_HP).')
          CALL WRTERR
          END IF
        END IF
!        IF((SVDMODE.EQ.0).AND.(LSQRMODE.EQ.0)
!     +     .AND.(IPCG.LE.0).AND.(JACUPDATE.EQ.0))THEN
!          WRITE(ERRMSG,80192)
!80192     FORMAT(' Consider setting JACUPDATE to 999 to implement ',
!     +    'Broyden''s Jacobian updating.')
!          CALL WRTERR
!        END IF

        IF((NUM_DISTRIB_FILES.GT.0).OR.(DISTRIB_COMLIN.NE.' '))THEN
          WRITE(ERRMSG,80172)
80172     FORMAT(' Only PEST_HP (and not PEST or BEOPEST) supports ',
     +    '"distribution files" functionality.')
          CALL WRTERR
        END IF

        IF(SPLITSWH.GT.-1.0D35)THEN
          ISPLIT=0
          DO I=1,NPARGP
            IF(SPLITTHRESH(I).GT.-1.0D35)THEN
              ISPLIT=1
            END IF
          END DO
          IF(ISPLIT.EQ.0)THEN
            WRITE(ERRMSG,80193)
80193       FORMAT(' A value was supplied for the SPLITSWH variable. ',
     +      'However no value was supplied for SPLITTHRESH for ',
     +      'any parameter group.')
            CALL WRTERR
          ELSE
            ISPLIT=0
            DO I=1,NPARGP
              IF(SPLITTHRESH(I).GT.0.0D0)THEN
                ISPLIT=1
              END IF
            END DO
            IF(ISPLIT.EQ.0)THEN
              WRITE(ERRMSG,80194)
80194         FORMAT(' A value was supplied for the SPLITSWH variable. ',
     +        'However SPLITTHRESH for no parameter group is greater ',
     +        'than zero.')
              CALL WRTERR
            END IF
          END IF
        END IF
        DO I=1,NPARGP
          IF(SPLITTHRESH(I).GT.0.0D0)THEN
            IF(J2OR3(I).EQ.2)THEN
              WRITE(ERRMSG,80195) TRIM(AGROUP(I))
80195         FORMAT(' A non-zero SPLITTHRESH variable is supplied ',
     +        'for parameter group "',A,'". However FORCEN is set to ',
     +        '"always_2" for that group.')
              CALL WRTERR
            END IF
          END IF
        END DO
        IF(SPLITSWH.GT.0.0D0)THEN
          IF((SPLITSWH.LT.0.5).OR.(SPLITSWH.GT.5.0))THEN
            WRITE(ERRMSG,80196)
80196       FORMAT(' Real variable SPLITSWH is set unusually high.')
            CALL WRTERR
          END IF
        END IF

        IF(PESTMODE.EQ.4)THEN
          IF((NOPTMX.EQ.0).OR.(NOPTMX.EQ.-1).OR.(NOPTMX.EQ.-2))THEN
            WRITE(ERRMSG,80151)
80151       FORMAT(' When PESTMODE is set to "pareto" and NOPTMAX ',
     +      'is provided as 0, -1, or -2, PEST re-sets its mode to ',
     +      '"estimation".')
            CALL WRTERR
          END IF
        END IF

        IF(NOPTMX.EQ.0)THEN
          WRITE(ERRMSG,8015)
8015      FORMAT(' NOPTMAX provided as zero. No optimisation ',
     +    'iterations will be carried out; objective function and ',
     +    'residuals will be recorded for initial parameter ',
     +    'estimates only.')
          CALL WRTERR
        END IF
        IF(NOPTMX.EQ.-1)THEN
          WRITE(ERRMSG,8016)
8016      FORMAT(' NOPTMAX provided as -1. No optimisation ',
     +    'will be carried out; objective function, ',
     +    'residuals and statistics will be recorded for ',
     +    'initial parameter estimates only.')
          CALL WRTERR
        END IF
        IF(NOPTMX.EQ.-2)THEN
          WRITE(ERRMSG,80161)
80161     FORMAT(' NOPTMAX provided as -2. No optimisation ',
     +    'will be carried out; nor will statistics be ',
     +    'calculated. The Jacobian matrix will ',
     +    'be computed, stored in the "JCO file", and then ',
     +    'PEST will cease execution immediately.')
          CALL WRTERR
        END IF

        IF(NOPTMX.GT.0)THEN
          IF(PHISTOPTHRESH.GT.0.0D0)THEN
            IF(SCHEDULEFILE.EQ.' ')THEN
              IF(PHIAB.GT.0.0D0)THEN
                IF(PHIAB.LE.PHISTOPTHRESH)THEN
                  WRITE(ERRMSG,80162)
80162             FORMAT(' Value provided for run abandonmnent ',
     +            '(PHIABANDON) does not exceed value supplied for ',
     +            'run termination (PHISTOPTHRESH).')
                  CALL WRTERR
                END IF
              END IF
            ELSE
              DO K=1,NOPTMX
                IF(PHIABANDON(K).GT.0.0D0)THEN
                  IF(PHIABANDON(K).LE.PHISTOPTHRESH)THEN
                    WRITE(ERRMSG,80163) TRIM(SCHEDULEFILE)
80163               FORMAT(' At least one PHIABANDON value supplied ',
     +              'in file ',a,' does not exceed run termination ',
     +              'criterion PHISTOPTHRESH.')
                    CALL WRTERR
                    GO TO 80164
                  END IF
                END IF
              END DO
80164         CONTINUE
            ENDIF
          END IF
        END IF

        IF(ISVDA.NE.0)THEN
          IF((SVDA_EXTSUPER.EQ.1).AND.(SVDA_NLSCALADJ.NE.0))THEN
            WRITE(ERRMSG,8017)
8017        FORMAT(' Because SVDA_EXTSUPER has been set to 1, ',
     +      'the non-zero value supplied for SVDA_NLSCALADJ will be ',
     +      'ignored.')
            CALL WRTERR
          END IF
          IF((SVDA_SUPDERCALC.EQ.0).AND.(JACFILE.NE.0))THEN
            WRITE(ERRMSG,80171)
80171       FORMAT(' Because JACFILE has been set to 1, ',
     +      'the value of zero assigned to SVDA_SUPEDERCALC will be ',
     +      'altered internally by PEST to 1; i.e. super parameter ',
     +      'derivatives will be calculated on the basis of ',
     +      'externally supplied base parameter derivatives.')
            CALL WRTERR
          END IF
        END IF
C        IF(JCWARN.EQ.1)THEN
C          WRITE(ERRMSG,4350)
C4350      FORMAT(' PEST control file uses old format. No values are ',
C     +    'supplied for NUMCOM, JACFILE and MESSFILE on line 5 of ',
C     +    'this file.')
C          CALL WRTERR
C        END IF
C        IF((IVECBND.EQ.-999).OR.(IBNDSTK.EQ.-999))THEN
C          WRITE(ERRMSG,4351)
C4351      FORMAT(' PEST control file uses old format. No values ',
C     +    'supplied for IBOUNDSTICK and/or UPVECBEND on line 7 of ',
C     +    'this file.')
C          CALL WRTERR
C        END IF

#ifndef UNIX
        DO 4370 I=1,NUMCOM
          CALL LOWCAS(COMLIN(I))
4370    CONTINUE
#endif

        IF(NUMCOM.GT.1)THEN
          DO 4390 I=2,NUMCOM
            DO 4385 J=1,I-1
              IF(COMLIN(I).EQ.COMLIN(J))THEN
                WRITE(ERRMSG,4380)
4380            FORMAT(' At least two command lines supplied in the ',
     +          '"* model command line" section of the PEST control ',
     +          'file are the same.')
                CALL WRTERR
                GO TO 4450
              END IF
4385        CONTINUE
4390      CONTINUE
        END IF

4450    CONTINUE
        IF(JACFILE.NE.0)THEN
#ifdef UNIX
          CALL LOWCAS(DCOMLIN)
#endif

          DO 4420 I=1,NUMCOM
            IF(DCOMLIN.EQ.COMLIN(I))THEN
              WRITE(ERRMSG,4410)
4410          FORMAT(' Derivatives command line is the same as ',
     +        'model command line.')
              CALL WRTERR
              GO TO 4430
            END IF
4420      CONTINUE
4430      CONTINUE

        END IF

        IF(LMBDA1.GT.50.0D0) CALL LOHI(1,'RLAMBDA1',1,ITEMP,LMBDA1,4)
        IF(ABS(LAMFAC).LT.1.2D0) THEN
          WRITE(ATEMP,'(1PG12.5)') ABS(LAMFAC)
          ATEMP=ADJUSTL(ATEMP)
          WRITE(ERRMSG,4431) TRIM(ATEMP)
4431      FORMAT(' Absolute value of RLAMFAC is unusually low (',A,
     +    ').')
          CALL WRTERR
        ELSE IF(ABS(LAMFAC).GT.5.0D0) THEN
          WRITE(ATEMP,'(1PG12.5)') ABS(LAMFAC)
          ATEMP=ADJUSTL(ATEMP)
          WRITE(ERRMSG,4432) TRIM(ATEMP)
4432      FORMAT(' Absolute value of RLAMFAC is unusually high (',A,
     +    ').')
          CALL WRTERR
        END IF
        IF(LAMPSI.LT.0.1D0)THEN
          CALL LOHI(1,'PHIRATSUF',0,ITEMP,LAMPSI,4)
        ELSE IF(LAMPSI.GT.0.7D0)THEN
          CALL LOHI(1,'PHIRATSUF',1,ITEMP,LAMPSI,4)
        END IF
        IF(FRCLAM.LT.0.001D0)THEN
          CALL LOHI(1,'PHIREDLAM',0,ITEMP,FRCLAM,4)
        ELSE IF(FRCLAM.GT.0.1D0)THEN
          CALL LOHI(1,'PHIREDLAM',1,ITEMP,FRCLAM,4)
        END IF
        IF(NUMRUN.GT.20) CALL LOHI(0,'NUMLAM',1,NUMRUN,RTEMP,4)
        DO 8005 I=1,NPAR
        IF(CHGTYP(I).EQ.0) GO TO 8006
8005    CONTINUE
        GO TO 8007
8006    IF(DMAX0.LT.0.1D0) THEN
          CALL LOHI(1,'RELPARMAX',0,ITEMP,DMAX0,4)
        ELSE IF(DMAX0.GT.50.0D0) THEN
          IF(RSIFLAG.EQ.0)THEN
            CALL LOHI(1,'RELPARMAX',1,ITEMP,DMAX0,4)
          END IF
        END IF
8007    DO 8008 I=1,NPAR
        IF(CHGTYP(I).EQ.1) GO TO 8011
8008    CONTINUE
        GO TO 8009
8011    IF(DMAX1.LT.1.1D0) THEN
          CALL LOHI(1,'FACPARMAX',0,ITEMP,DMAX1,4)
        ELSE IF(DMAX1.GT.50.0D0) THEN
          IF(RSIFLAG.EQ.0)THEN
            CALL LOHI(1,'FACPARMAX',1,ITEMP,DMAX1,4)
          END IF
        END IF
8009    CONTINUE
        IF(IBNDSTK.GT.4)THEN
          CALL LOHI(0,'IBOUNDSTICK',1,IBNDSTK,RTEMP,3)
        END IF
        IF((ACODE.EQ.'m').OR.(ACODEP.EQ.'m')) THEN
        DO 8040 I=1,NPARGP
        IF(J2OR3(I).EQ.1) GO TO 8045
8040    CONTINUE
        GO TO 8050
8045    IF(PSI3.LT.0.05D0) THEN
          CALL LOHI(1,'PHIREDSWH',0,ITEMP,PSI3,4)
        ELSE IF(PSI3.GT.0.5D0)THEN
          CALL LOHI(1,'PHIREDSWH',1,ITEMP,PSI3,4)
        END IF
        END IF
8050    IF(PSISTP.GT.0.3) CALL LOHI(1,'PHIREDSTP',1,ITEMP,PSISTP,4)
        IF(NPSILO.GT.5) CALL LOHI(0,'NPHISTP',1,NPSILO,RTEMP,4)
        IF(NLOPT.GE.5) CALL LOHI(0,'NPHINORED',1,NLOPT,RTEMP,4)
        IF(ENDCHG.LT.1.0E-6)THEN
          CALL LOHI(1,'RELPARSTP',0,ITEMP,ENDCHG,4)
        ELSE IF(ENDCHG.GT.0.2D0)THEN
          CALL LOHI(1,'RELPARSTP',1,ITEMP,ENDCHG,4)
        END IF
        IF(NUMCHG.GT.5) CALL LOHI(0,'NRELPAR',1,NUMCHG,RTEMP,4)
        IF((ACODE.EQ.'m').OR.(ACODEP.EQ.'m')) THEN
        DO 8100 I=1,NPARGP
        IF(((IDERIV(I).EQ.1).OR.(IDERIV(I).EQ.3)).AND.
     +    (DERVAL(I).GT.0.5D0))THEN
          CALL WRTRL(ATEMP,DERVAL(I),4)
          WRITE(ERRMSG,8010) AGROUP(I)(:NBLNK(AGROUP(I))),
     +    ATEMP(:NBLNK(ATEMP))
8010      FORMAT(' Relative derivative increment (DERINC) for ',
     +    'parameter group "',A,'" unusually high (',A,').')
          CALL WRTERR
        END IF
        IF((J2OR3(I).EQ.1).OR.(J2OR3(I).EQ.3))THEN
          IF(DERMUL(I).GT.10.0D0)THEN
            CALL WRTRL(ATEMP,DERMUL(I),4)
            WRITE(ERRMSG,8020) AGROUP(I)(:NBLNK(AGROUP(I))),
     +      ATEMP(:NBLNK(ATEMP))
8020        FORMAT(' Derivative increment multiplier (DERINCMUL) for ',
     +      'parameter group "',A,'" unusually high (',A,').')
            CALL WRTERR
          ELSE IF(DERMUL(I).LT.0.2)THEN
            CALL WRTRL(ATEMP,DERMUL(I),4)
            WRITE(ERRMSG,8021) AGROUP(I)(:NBLNK(AGROUP(I))),
     +      ATEMP(:NBLNK(ATEMP))
8021        FORMAT(' Derivative increment multiplier (DERINCMUL) for ',
     +      'parameter group "',A,'" unusually low (',A,').')
            CALL WRTERR
          END IF
        END IF
        IF((IDERIV(I).EQ.2).AND.(DERBND(I).GT.0.0D0))THEN
          WRITE(ERRMSG,8030) AGROUP(I)(:NBLNK(AGROUP(I)))
8030      FORMAT(' Derivative increment type for parameter group "',A,
     +    '" is "absolute": non-zero increment lower bound ignored.')
          CALL WRTERR
        END IF
        IF(SPLITTHRESH(I).GT.-1.0D35)THEN
          IF(SPLITRELDIFF(I).GT.10.0)THEN
            CALL WRTRL(ATEMP,SPLITRELDIFF(I),4)
            WRITE(ERRMSG,8031) TRIM(AGROUP(I)),TRIM(ATEMP)
8031        FORMAT(' Value of SPLITRELDIFF for parameter group "',A,
     +      '" is unusually large (',A,').')
            CALL WRTERR
          END IF
        END IF
8100    CONTINUE
        DO I=1,NPARGP
          IF((J2OR3(I).EQ.4).OR.(J2OR3(I).EQ.5))THEN
            IF(I3CALC(I).EQ.5)THEN
            DO J=1,NPAR
              IF(IPARGP(J).EQ.I)THEN
                IF(ITRANS(J).GE.0)THEN
                  WRITE(ERRMSG,8101)
8101              FORMAT(' The DERMTHD variable for at least one ',
     +            'parameter group is set to "maxprec". Consider ',
     +            'using the "minvar" setting instead.')
                  CALL WRTERR
                  GO TO 8109
                END IF
              END IF
            END DO
            END IF
          END IF
        END DO
8109    CONTINUE

        IF(DOSENREUSE.EQ.'senreuse')THEN
          IF(SENALLCALCINT.GT.5)THEN
            CALL WRTINT(ATEMP,SENALLCALCINT)
            WRITE(ERRMSG,8102) TRIM(ATEMP)
8102        FORMAT(' Iteration interval for complete sensitivity ',
     +       'recalculation (SENALLCALCINT) is unusually high (',A,').')
            CALL WRTERR
          END IF
          IF(SENRELTHRESH.GT.1.0)THEN
            WRITE(ERRMSG,8103)
8103        FORMAT(' Relative sensitivity threshold for ',
     +      'sensitivity reuse (SENRELTHRESH) will be adjusted ',
     +      'down to 1.0.')
            CALL WRTERR
          END IF
        END IF
        IF((ISENREUSE.EQ.1).AND.(DOSENREUSE.NE.'senreuse'))THEN
          WRITE(ERRMSG,8104)
8104      FORMAT(' A "sensitivity reuse" section is present in the ',
     +    'PEST control file. However sensitivity re-use is not ',
     +    'activated through use of the DOSENREUSE variable.')
          CALL WRTERR
        END IF

        DO 8200 I=1,NPARGP
        ITEMP=0
        DO 8180 J=1,NPAR
        IF(IPARGP(J).EQ.I) THEN
          IF(ITRANS(J).LT.0) THEN
            ITEMP=ITEMP+1
          ELSE
            GO TO 8200
          END IF
        END IF
8180    CONTINUE
        IF(ITEMP.EQ.0)THEN
          WRITE(ERRMSG,8150) AGROUP(I)(:NBLNK(AGROUP(I)))
8150      FORMAT(' No parameters belong to parameter group "',A,'".')
        ELSE
          WRITE(ERRMSG,8140) AGROUP(I)(:NBLNK(AGROUP(I)))
8140      FORMAT(' All parameters belonging to parameter group "',
     +    A,'" are either fixed or tied.')
        END IF
        CALL WRTERR
8200    CONTINUE
        DO 8230 I=1,NPARGP
        IF(IDERIV(I).EQ.1) GO TO 8230
        DO 8220 J=1,NPAR
        IF(IPARGP(J).NE.I) GO TO 8220
        IF(APAR(J)(1:3).NE.'iw_')THEN
        IF(ITRANS(J).EQ.1) THEN
          WRITE(ERRMSG,8210) AGROUP(I)(:NBLNK(AGROUP(I)))
8210      FORMAT(' Parameter group "',A,'" contains log-transformed ',
     +    'parameters; however increment type is not "relative".')
          CALL WRTERR
          GO TO 8230
        END IF
        END IF
8220    CONTINUE
8230    CONTINUE
        END IF

        DO 8250 I=1,NPAR
        IF((PVAL(I).EQ.0.0D0).AND.(ITRANS(I).EQ.0))THEN
          WRITE(ERRMSG,8240)
8240      FORMAT(' At least one adjustable parameter value is zero: ',
     +    'that parameter will not be subject to relative or factor ',
     +    'change limits in the 1st optimisation iteration and will ',
     +    'not have the benefits of FACORIG in subsequent iterations.')
          CALL WRTERR
          GO TO 8251
        END IF
8250    CONTINUE
8251    CONTINUE
        IF(NOBSGP1.EQ.-999)THEN
          WRITE(ERRMSG,8270)
8270      FORMAT(' PEST control file uses old format - ',
     +    'no value for NOBSGP and/or observation groups provided.')
          CALL WRTERR
        ELSE
          IF(NOBS+NPRIOR.NE.0)THEN
            DO 8290 I=1,NOBSGP
              DO 8280 J=1,NOBS+NPRIOR
                IF(IOBGNM(J).EQ.I) GO TO 8290
8280          CONTINUE
8285          WRITE(ERRMSG,8287) AOBSGP(I)(1:NBLNK(AOBSGP(I)))
8287          FORMAT(' No observations or prior information articles ',
     +        'belong to observation group "',A,'".')
              CALL WRTERR
8290        CONTINUE
          END IF
        END IF

        IF(NPRWRN.NE.0)THEN
          WRITE(ERRMSG,8295)
8295      FORMAT(' There is at least one item of prior information ',
     +    'that has not been assigned to an observation group. (Note ',
     +    'that earlier versions of PEST did not require this.)')
          CALL WRTERR
        END IF

        DO 8310 I=1,NINFLE
        IF(NUMPAR(I).EQ.0) THEN
          IF(I.NE.1)THEN
            DO 8300 J=1,I-1
            IF(TMPFLE(I).EQ.TMPFLE(J)) GO TO 8310
8300        CONTINUE
          END IF
          WRITE(ERRMSG,8320) TMPFLE(I)(:NBLNK(TMPFLE(I)))
8320      FORMAT(' Template file ',A,' references no parameters.')
          CALL WRTERR
        END IF
8310    CONTINUE

        IF(NINFLE.GT.1) THEN
          DO 8350 I=1,NINFLE-1
          DO 8340 J=I+1,NINFLE
          IF(TMPFLE(I).EQ.TMPFLE(J))THEN
            IF(I.NE.1)THEN
              DO 8335 K=1,I-1
              IF(TMPFLE(K).EQ.TMPFLE(I)) GO TO 8350
8335          CONTINUE
            END IF
            WRITE(ERRMSG,8330) TMPFLE(I)(:NBLNK(TMPFLE(I)))
8330        FORMAT(' Template file ',A,' used for more than one model ',
     +      'input file.')
            CALL WRTERR
            GO TO 8350
          END IF
8340      CONTINUE
8350      CONTINUE
        END IF

        DO 8400 I=1,NOUTFL
        IF(NUMOBS(I).EQ.0) THEN
          WRITE(ERRMSG,8410) INSFLE(I)(:NBLNK(INSFLE(I)))
8410      FORMAT(' Instruction file ',A,' references no observations.')
          CALL WRTERR
        END IF
8400    CONTINUE

        IF(NOUTFL.GT.1) THEN
          DO 8450 I=1,NOUTFL-1
          DO 8440 J=I+1,NOUTFL
          IF(MODFLE(I).EQ.MODFLE(J))THEN
            IF(I.NE.1)THEN
              DO 8435 K=1,I-1
              IF(MODFLE(K).EQ.MODFLE(I)) GO TO 8450
8435          CONTINUE
            END IF
            WRITE(ERRMSG,8430) MODFLE(I)(:NBLNK(MODFLE(I)))
8430        FORMAT(' Model output file ',A,' read using more than one ',
     +      'instruction file.')
            CALL WRTERR
            GO TO 8450
          END IF
8440      CONTINUE
8450      CONTINUE
        END IF


C -- WARNINGS PERTAINING TO PREDICTIVE ANALYSIS DATA ARE NOW MADE.

        IF(IPRED.EQ.0) GO TO 8600
        IF(PD1.GT.PD0*1.1)THEN
          WRITE(ERRMSG,8510)
8510      FORMAT(' Real variable PD1 is unusually high with respect ',
     +    'to PD0')
          CALL WRTERR
        END IF
        IF(PD2.GT.PD0*3.0)THEN
          WRITE(ERRMSG,8520)
8520      FORMAT(' Real variable PD2 is unusually high with respect ',
     +    'to PD0')
          CALL WRTERR
        END IF
        IF(REPRED.GT.0.1)THEN
          CALL LOHI(1,'RELPREDLAM',1,ITEMP,REPRED,4)
        ELSE IF((REPRED.NE.0.0D0).AND.(REPRED.LE.0.00005))THEN
          CALL LOHI(1,'RELPREDLAM',0,ITEMP,REPRED,4)
        END IF
        IF((ABPRED.EQ.0.0).AND.(REPRED.EQ.0.0))THEN
          WRITE(ERRMSG,8530)
8530      FORMAT(' Real variables ABSPREDLAM and RELPREDLAM are ',
     +    'both zero.')
          CALL WRTERR
        END IF
        DO 8540 I=1,NPARGP
        IF(J2OR3(I).EQ.1) GO TO 8545
8540    CONTINUE
        GO TO 8550
8545    IF(RSWPRD.GT.0.1D0)THEN
          CALL LOHI(1,'RELPREDSWH',1,ITEMP,RSWPRD,4)
        END IF
        IF((ASWPRD.EQ.0.0).AND.(RSWPRD.EQ.0.0))THEN
          WRITE(ERRMSG,8547)
8547      FORMAT(' Real variables ABSPREDSWH and RELPREDSWH are ',
     +    'both zero.')
          CALL WRTERR
        END IF
8550    CONTINUE
        IF(NPRDLO.GT.8) CALL LOHI(0,'NPREDSTP',1,NPRDLO,RTEMP,4)
        IF(NPRNRD.GT.8) CALL LOHI(0,'NPREDNORED',1,NPRNRD,RTEMP,4)
        IF(PRSTPR.GT.0.1D0)THEN
          CALL LOHI(1,'RELPREDSTP',1,ITEMP,PRSTPR,4)
        END IF
        IF(LINITFAC.GT.2.0) CALL LOHI(1,'INITSCHFAC',1,ITEMP,
     +  LINITFAC,4)
        IF(LFACFAC.GT.5.0) CALL LOHI(1,'MULSCHFAC',1,ITEMP,LFACFAC,4)
        IF(NSEARCH.GT.15) CALL LOHI(0,'NSEARCH',1,NSEARCH,RTEMP,4)
        IF(LINITFAC.LT.0.05) CALL LOHI(1,'INITSCHFAC',0,ITEMP,
     +  LINITFAC,4)
        IF(LFACFAC.LT.1.2) CALL LOHI(1,'MULSCHFAC',0,ITEMP,
     +  LFACFAC,4)
        IF(NSEARCH.LT.2)THEN
          WRITE(ERRMSG,8551)
8551      FORMAT(' PEST''s predictive analyser sometimes works ',
     +    'better when the line search option is activated.')
          CALL WRTERR
        END IF

C -- WARNINGS PERTAINING TO REGULARISATION ARE NOW MADE.

8600    CONTINUE
        IF(IREG.EQ.0) GO TO 8990
        IF(WFFAC.GT.2.0)CALL LOHI(1,'WFFAC',1,ITEMP,WFFAC,4)
        IF(WFFAC.LT.1.1)CALL LOHI(1,'WFFAC',0,ITEMP,WFFAC,4)
        IF(PD1R.GT.PHIMLIM*1.1)THEN
          WRITE(ERRMSG,8610)
8610      FORMAT(' It is unusual for PHIMACCEPT to be larger than ',
     +    '1.1 times PHIMLIM.')
          CALL WRTERR
        END IF
        IF(PD1R.LT.PHIMLIM*1.01)THEN
          WRITE(ERRMSG,8611)
8611      FORMAT(' It is unusual for PHIMACCEPT to be less than ',
     +    '1.01 times PHIMLIM.')
          CALL WRTERR
        END IF
        IF(REGTOL.GT.0.1) CALL LOHI(1,'WFTOL',1,ITEMP,REGTOL,4)
        IF(REGTOL.LT.1.0E-4)CALL LOHI(1,'WFTOL',0,ITEMP,REGTOL,4)
        IF(FRACPHIM.GT.0.3)THEN
          WRITE(ERRMSG,8612)
8612      FORMAT(' It is unusual for FRACPHIM to be larger than ',
     +    'about 0.3.')
          CALL WRTERR
        END IF
        IF((IREGADJ.EQ.4).OR.(IREGADJ.EQ.5))THEN
          IF(NOPTREGADJ.GT.5)THEN
            WRITE(ERRMSG,8615)
8615        FORMAT(' Value supplied for NOPTREGADJ is unusually high.')
            CALL WRTERR
          END IF
          IF(REGWEIGHTRAT.GT.1.0E4)THEN
            WRITE(ERRMSG,8616)
8616        FORMAT(' Value supplied for REGWEIGHTRAT is ',
     +      'unusually high.')
            CALL WRTERR
          END IF
          IF(IREGADJ.EQ.5)THEN
            IF(REGSINGTHRESH.LT.1.0D-7)THEN
              WRITE(ERRMSG,8617)
8617          FORMAT(' Value supplied for REGSINGTHRESH is unusually ',
     +        'low.')
              CALL WRTERR
            END IF
          END IF
        END IF

C -- WARNINGS PERTAINING TO SINGULAR VALUE DECOMPOSITION ARE NOW MADE.

8990    CONTINUE
        IF(SVDMODE.NE.0)THEN
          IF(MAXSING.GT.NESPAR)THEN
            WRITE(ERRMSG,3200)
3200        FORMAT(' MAXSING in the singular value decomposition ',
     +      'section is greater than the number of adjustable ',
     +      'parameters.')
            CALL WRTERR
          END IF
          IF(EIGTHRESH.GT.1.0E-1)THEN
            WRITE(ERRMSG,3210)
3210        FORMAT(' Value for EIGTHRESH in singular value ',
     +      'decomposition section is unusually high.')
            CALL WRTERR
          END IF
          IF(LMBDA1.NE.0.0D0)THEN
C            WRITE(ERRMSG,3220)
C3220        FORMAT(' When singular value decomposition is activated, ',
C     +      'it is sometimes best to set RLAMBDA1 to zero (and ',
C     +      'NUMLAM to one).')
C             CALL WRTERR
          END IF
        ELSE
          IF(LMBDA1.EQ.0.0D0)THEN
            WRITE(ERRMSG,3230)
3230        FORMAT(' Normally, unless singular value decomposition ',
     +      'or predictive analysis is activated, RLAMBDA1 should ',
     +      'be set greater than zero.')
            CALL WRTERR
          END IF
        END IF

        IF(IRES.EQ.1)THEN
          IF(IPRED.EQ.1)THEN
            WRITE(ERRMSG,3233)
3233        FORMAT(' The IRES variable is set to 1 in the "control ',
     +      'data" section of the PEST control file, requesting that ',
     +      'a resolution data file be written by PEST. As this ',
     +      'cannot happen when PEST is run in predictive analysis ',
     +      'mode, PEST will reset IRES to 0.')
            CALL WRTERR
          ELSE
            IF((ISVDA.EQ.0).AND.(SVDMODE.EQ.0).AND.(IREG.EQ.0).AND.
     +         (LSQRMODE.EQ.0))THEN
              WRITE(ERRMSG,3231)
3231          FORMAT(' The IRES variable is set to 1 in the ',
     +        '"control data" section of the PEST control file, ',
     +        'thus requesting that a resolution data file be ',
     +        'written by PEST. This is unusual unless ',
     +        'PEST is run in "regularisation" mode, ',
     +        'singular value decomposition or LSQR is activated, or ',
     +        'PEST''s SVD-assist functionality is activated.')
              CALL WRTERR
            END IF
            IF(IMCOVFLAG.EQ.1)THEN
              WRITE(ERRMSG,3234)
3234          FORMAT(' The IRES variable has been set to 1 in the ',
     +        '"control data" section of the PEST control file, ',
     +        'requesting that a resolution data file be generated. ',
     +        'However a covariance matrix has been supplied for a ',
     +        'non-regularisation observation group. As presently ',
     +        'programmed, RESPROC cannot process resolution ',
     +        'data under these conditions.')
              CALL WRTERR
            END IF
          END IF
        ELSE IF(IRES.EQ.-9999)THEN
          IF((ISVDA.NE.0).OR.(SVDMODE.NE.0).OR.(IREG.NE.0).OR.
     +       (LSQRMODE.NE.0))THEN
            IF(NOPTMX.GT.0)THEN
            IF(IMCOVFLAG.EQ.0)THEN
c              WRITE(ERRMSG,3232)
c3232          FORMAT(' No value has been supplied for the IRES ',
c     +        'variable in the "control data" section of the PEST ',
c     +        'control file. As the ',
c     +        'PEST run involves LSQR and/or regularisation and/or ',
c     +        'SVD and/or SVD-assist, IRES '
c     +        'will be set to "1" by PEST in order that resolution ',
c     +        'data is recorded for the use of RESPROC.')
c              CALL WRTERR
            END IF
          END IF
          END IF
        END IF

C -- WARNINGS PERTAINING TO LSQR ARE NOW MADE.

        IF(LSQRMODE.NE.0)THEN
          IF(LSQR_ATOL.GT.1.0D-2)THEN
            WRITE(ATEMP,'(1PG12.5)') LSQR_ATOL
            ATEMP=ADJUSTL(ATEMP)
            WRITE(ERRMSG,8622) TRIM(ATEMP)
8622        FORMAT(' Value for LSQR_ATOL set unusually high (',A,').')
            CALL WRTERR
          END IF
        END IF

C -- WARNINGS PERTAINING TO AUTOMATIC USER INTERVENTION ARE NOW MADE.

        IF((IREG.NE.0).OR.(IPRED.NE.0)) GO TO 8991
        IF(DOAUI.EQ.'AAAA')THEN
          IF((ISVD.EQ.0).AND.(ISVDA.EQ.0).AND.(ILSQR.EQ.0))THEN
            WRITE(ERRMSG,8620) CASFL(1:NBLNK(CASFL))
8620        FORMAT(' No SVDMODE or LSQRMODE variables appear in ',
     +      'PEST control ',A,'. '
     +      'The user should be aware of how powerful these ',
     +      'can be. Try one of these (especially SVD) if PEST ',
     +      'does not perform as well as you think it should.')
            CALL WRTERR
          END IF
        ELSE IF(DOAUI.EQ.'noaui')THEN
          IF(IASECT.EQ.1)THEN
            WRITE(ERRMSG,8621) CASFL(1:NBLNK(CASFL))
8621        FORMAT(' The DOAUI variable in the "control data" ',
     +      'section of file ',A,' is set to "noaui"; ',
     +      'however there is an "automatic user intervention" ',
     +      'section in this file.')
            CALL WRTERR
          END IF
        ELSE IF(DOAUI(1:3).EQ.'aui')THEN
          IF(IASECT.EQ.1)THEN
            IF(MAXAUI.EQ.0)THEN
              WRITE(ERRMSG,8623) CASFL(1:NBLNK(CASFL))
8623          FORMAT(' The DOAUI variable in the "control data" ',
     +        'section of file ',A,' is set to "aui" or "auid"; ',
     +        'however ',
     +        'automatic user intervention will not be carried out ',
     +        'as the MAXAUI variable in the "automatic user ',
     +        'intervention" section is set to 0.')
              CALL WRTERR
            END IF
          END IF
        END IF
        IF((DOAUI.EQ.'AAAA').OR.(DOAUI.EQ.'noaui').OR.
     +  (MAXAUI.EQ.0)) GO TO 8991

        NESPAR=0
        DO 8624 I=1,NPAR
        IF(ITRANS(I).GE.0) NESPAR=NESPAR+1
8624    CONTINUE
        IF(MAXAUI.GE.NESPAR)THEN
          WRITE(ERRMSG,8630)
8630      FORMAT(' Integer variable MAXAUI is as high or higher than ',
     +    'the number of non-tied and non-fixed parameters. Try half ',
     +    'of the latter number.')
          CALL WRTERR
        END IF
        IF(AUIOPT.GT.5)THEN
          WRITE(ERRMSG,8640)
8640      FORMAT(' Integer variable AUISTARTOPT is unusually high; ',
     +    'zero is often fine, no higher than 5 is suggested.')
          CALL WRTERR
        END IF
        IF((NOAUIPHIRAT.LT.0.8).OR.(NOAUIPHIRAT.GT.0.95))THEN
          WRITE(ERRMSG,8650)
8650      FORMAT(' Real variable NOAUIPHIRAT is outside its normal ',
     +    'range of effectiveness; 0.9 is suggested.')
          CALL WRTERR
        END IF
        IF(DOAUI.EQ.'aui')THEN
        IF(NOAUIPHIRAT.GT.1.0-PSI3+0.02)THEN
          WRITE(ERRMSG,8655)
8655      FORMAT(' The value supplied for NOAUIPHIRAT should be ',
     +    'equal to or less than (1.0 - PHIREDSWH) so that PEST will ',
     +    'attempt automatic user intervention before switching to ',
     +    'three-point derivatives calculation; 0.1 is suggested for ',
     +    'PHIREDSWH and 0.9 is suggested for NAUIPHIRAT.')
          CALL WRTERR
        END IF
        END IF
        IF(DOAUI.EQ.'aui')THEN
          IF((AUISENSRAT.LT.8.0).OR.(AUISENSRAT.GT.200.0))THEN
            WRITE(ERRMSG,8660)
8660        FORMAT(' Real variable AUISENSRAT is outside its normal ',
     +      'range of effectiveness; 80.0 is suggested.')
            CALL WRTERR
          END IF
        ELSE IF(DOAUI.EQ.'auid')THEN
          IF(AUISENSRAT.GT.5.0)THEN
            WRITE(ERRMSG,8661)
8661        FORMAT(' For a DOAUI setting of "auid" consider setting ',
     +      'AUISENSRAT less than 5.0.')
            CALL WRTERR
          END IF
        END IF
        IF(AUINUMFREE.GT.NESPAR*3/4)THEN
          WRITE(ERRMSG,8670)
8670      FORMAT(' Integer variable AUINUMFREE is unusually high;',
     +    ' suggest no higher than three quarters of the number of ',
     +    'adjustable parameters.')
          CALL WRTERR
        END IF
        IF((ALAMPSI.LT.0.6).OR.(ALAMPSI.GT.0.95))THEN
          WRITE(ERRMSG,8680)
8680      FORMAT(' Real variable AUIPHIRATSUF is outside its normal ',
     +    'range of effectiveness; 0.8 is suggested.')
          CALL WRTERR
        END IF
        IF(DOAUI.EQ.'aui')THEN
        IF((APHIRATACC.LT.0.9).OR.(APHIRATACC.GT.0.99))THEN
          WRITE(ERRMSG,8690)
8690      FORMAT(' Real variable AUIPHIRATACC is outside its normal ',
     +    'range of effectiveness; 0.95 is suggested.')
          CALL WRTERR
        END IF
        ELSE IF(DOAUI.EQ.'auid')THEN
          IF(APHIRATACC.LT.0.98)THEN
            WRITE(ERRMSG,8691)
8691        FORMAT(' For a DOAUI setting of "auid" consider setting ',
     +      'AUIPHIRATACC to 0.99.')
            CALL WRTERR
          END IF
        END IF
        IF((NAUINOACC.LT.NESPAR/5).OR.(NAUINOACC.GT.NESPAR/2))THEN
          WRITE(ERRMSG,8700)
8700      FORMAT(' Integer variable NAUINOACCEPT should be between ',
     +    'one fifth and one half of the number of adjustable ',
     +    'parameters (ie. the number of non-fixed and non-tied ',
     +    'parameters).')
          CALL WRTERR
        END IF

C -- A warning pertaining to memory conservation is now made.

8991    CONTINUE

C -- Warnings pertaining to Pareto mode are now made.

        IF(PESTMODE.EQ.4)THEN
          IF(NUM_WTFAC_INC.LT.4)THEN
            WRITE(ERRMSG,8704)
8704        FORMAT(' Value supplied for NUM_WTFAC_INC in "pareto "',
     +      ' section of PEST control file is unusually small.')
            CALL WRTERR
          END IF
          IF(PAR_WTFAC_FIN.GT.10.0)THEN
            WRITE(ERRMSG,8703)
8703        FORMAT(' Value supplied for PARETO_WTFAC_FIN in ',
     +      '"pareto" section of PEST control file is unusually large.')
            CALL WRTERR
          END IF
          IF(ALT_TERM.EQ.1)THEN
            IF(NUM_ITER_THRESH.GT.6)THEN
              WRITE(ERRMSG,8706)
8706          FORMAT(' Value supplied for NUM_ITER_THRESH in ',
     +        '" pareto" section of PEST control file is unusually ',
     +        'large.')
              CALL WRTERR
            END IF
          END IF
          IF((NUM_ITER_START.GT.5).OR.
     +       (NUM_ITER_GEN.GT.5).OR.
     +       (NUM_ITER_FIN.GT.5))THEN
             WRITE(ERRMSG,8702)
8702         FORMAT(' Value supplied for NUM_ITER_START, ',
     +       'NUM_ITER_GEN or NUM_ITER_FIN in "pareto" section ',
     +       'of PEST control file is unusually large.')
             CALL WRTERR
          END IF
          IF(ISVDA.EQ.1)THEN
            IF(SVDA_PAR_EXCL.EQ.1)THEN
              ICOUNT=0
              DO I=1,NXROW
                IF(IOBGNM(I).EQ.PAR_OBSGROUP) ICOUNT=ICOUNT+1
              END DO
              IF(ICOUNT.EQ.1)THEN
                WRITE(ERRMSG,8709) TRIM(AOBSGP(PAR_OBSGROUP))
8709            FORMAT(' SVDA_PAR_EXCL variable in "SVD assist" ',
     +          'section is set to 1. However pareto weight-',
     +          'adjusted observation group "',A,
     +          '" has only one member.')
                CALL WRTERR
              END IF
            ELSE IF(SVDA_PAR_EXCL.EQ.-1)THEN
              ICOUNT=0
              DO I=1,NXROW
                IF(IOBGNM(I).NE.PAR_OBSGROUP) ICOUNT=ICOUNT+1
              END DO
              IF(ICOUNT.EQ.1)THEN
                WRITE(ERRMSG,8708) TRIM(AOBSGP(PAR_OBSGROUP))
8708            FORMAT(' SVDA_PAR_EXCL variable in "SVD assist" ',
     +          'section is set to -1. However groups other ',
     +          'than the pareto weight-adjusted observation group "',A,
     +          '" have only one member.')
                CALL WRTERR
              END IF
            END IF
          END IF
        END IF
        IF(NESPAR.GT.300)THEN
          IF((LSQRMODE.EQ.0).OR.(SVDMODE.EQ.0).OR.(SVDMODE.EQ.1))THEN
            IF((ICOV.NE.0).OR.(ICOR.NE.0).OR.(IEIG.NE.0))THEN
              WRITE(ERRMSG,8710)
8710          FORMAT(' Where many parameters are being estimated as ',
     +        'in the present case, significant memory can be saved ',
     +        'by setting all of ICOV, ICOR and IEIG to zero in the ',
     +        '"control data" section of the PEST control file.')
              CALL WRTERR
            END IF
          END IF
        END IF

C -- Now we do a test for ascii files written by some weird editors.

        IF((QUICKPRIOR.EQ.0).AND.(IPPCOUNT.EQ.0))THEN
          IF(IERR.NE.0)THEN
            REWIND(UNIT=30)
            DO I=1,3
              READ(30,*,ERR=303,END=303)
            END DO
            READ(30,*,ERR=303,END=303) MM1,MM2,MM3,MM4,MM5
            IF((MM1.NE.NPAR).OR.(MM2.NE.NOBS).OR.(MM3.NE.NPARGP).OR.
     +         (MM4.NE.NPRIOR).OR.(MM5.NE.NOBSGP)) GO TO 303
            DO I=1,NPAR+NOBSGP+NOBS+NPRIOR
              READ(30,*,ERR=303,END=303)
            END DO
            GO TO 304
303         WRITE(ERRMSG,305) CASFL(1:MCASE)
305         FORMAT(' File ',A,' appears to be missing carriage ',
     +      'returns and may not be read properly by PEST or some of ',
     +      'its utilities. Has it been ',
     +      'imported from a UNIX machine? Try running a UNIX2DOS ',
     +      'converter, or reading it into EDIT, making a change ',
     +      'and re-writing it.')
            CALL WRTERR
304         CONTINUE
          END IF
        END IF

        IF((IPPCOUNT.NE.0).OR.(NPARFILE.NE.0).OR.
     +     (NPARSEC.NE.0).OR.(JCOZEROTHRESH.GT.0.0D0).OR.
     +     (JCOWARNTHRESH.GT.0.0D0).OR.(REG2MEASRAT.GT.0.0D0).OR.
     +     (UPTESTMIN.GT.0).OR.(UPTESTLIM.GT.0).OR.
     +     (ORR_NOT_FIRST.NE.0).OR.(IZEROSENVAL.GT.0).OR.
     +     (WRITE_RRF.NE.-9999).OR.(HARDSTOPHOURS.GT.0.0).OR.
     +     (SOFTSTOPHOURS.GT.0.0).OR.(WIN_MRUN_HOURS.GT.0.0).OR.
     +     (RUN_ABANDON_FAC.GT.0.0))THEN
          WRITE(6,306)
306       FORMAT(/,' Special warning ----->')
          WRITE(ERRMSG,307)
307       FORMAT(' The PEST control file appears to contain comments ',
     +    'and/or lines that employ PEST++ or PEST_HP input protocol. ',
     +    'If a particular PEST utility ',
     +    'program objects to this file, strip the file of these ',
     +    'features using the PSTCLEAN utility.')
          CALL WRTERR
        END IF

        IF(IERR.EQ.0) THEN
          WRITE(6,8999,ERR=9900)
8999      FORMAT(' No warnings.')
          GO TO 9998
        ELSE
          GO TO 9998
        END IF

9400    WRITE(6,9410,ERR=9900)
9410    FORMAT(' Insufficient memory to run PESTCHEK.')
        ISTOP=1
        GO TO 9998
9500    WRITE(AFIG,'(I10)') ILINE
        CALL SHIFTL(AFIG)
        WRITE(ERRMSG,9510) AFIG(:NBLNK(AFIG)),CASFL(1:MCASE)
9510    FORMAT(' Line ',A,' of file ',A,': insufficient data items.')
        CALL WRTERR
        ISTOP=1
        GO TO 9998
9600    WRITE(AFIG,'(I10)') ILINE
        CALL SHIFTL(AFIG)
        WRITE(ERRMSG,9610) AFIG(:NBLNK(AFIG)),CASFL(1:MCASE)
9610    FORMAT(' Line ',A,' of file ',A,': cannot read line.')
        CALL WRTERR
        ISTOP=1
        GO TO 9998
9700    WRITE(AFIG,'(I10)') ILINE
        CALL SHIFTL(AFIG)
        WRITE(ERRMSG,9710) AFIG(:NBLNK(AFIG)),CASFL(1:MCASE)
9710    FORMAT(' Line ',A,' of file ',A,': unexpected end to file.')
        CALL WRTERR
        ISTOP=1
        GO TO 9998
9800    ISTOP=1
        GO TO 9998
9900    ISTOP=1
        GO TO 9998

9998    CONTINUE
        DEALLOCATE(AGROUP,IDERIV,DERVAL,DERBND,J2OR3,DERMUL,I3CALC,
     +  IDBND,APAR,ITRANS,CHGTYP,PVAL,LBOUND,UBOUND,IPARGP,AOBS,IWORK,
     +  TMPFLE,NUMPAR,NUMOBS,MODFLE,INSFLE,APRIOR,X,Y,AOBSGP,IOBGNM,
     +  COMLIN,COVFLE,COVAR,IRGP,IW_PR,DERCOM,STAT=IALERR)
        DEALLOCATE(GTARG,PHIABANDON,NNUMOBS,TEMPOBS,STAT=IALERR)
        DEALLOCATE(SPLITTHRESH,SPLITRELDIFF,SPLITACTION,STAT=IALERR)
        DEALLOCATE(OWGHT,STAT=IALERR)

        IF(PESTMODE.EQ.4)THEN
          DEALLOCATE(OBS_REPORT,STAT=IALERR)
        END IF

        DEALLOCATE(DI_TEMP,DI_PAR,DI_PARENT,STAT=IALERR)

        IF(ISTOP.NE.0)THEN
          CALL EXIT(100)
        ELSE
          CALL EXIT(0)
        END IF

        END
