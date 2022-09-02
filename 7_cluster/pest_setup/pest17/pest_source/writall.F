        SUBROUTINE WRTALL(JFAIL,IUNIT,NPAR,NESPAR,
     +  NPARGP,NOBS,NPRIOR,NUMCOM,JACFILE,NXROW,NINFLE,NOUTFL,ITRANS,
     +  SCALE,OFFSET,CHGTYP,IPARGP,PVAL,LBOUND,UBOUND,APAR,OVAL,OWGHT,
     +  X,AOBS,IDBND,IDERIV,J2OR3,I3CALC,DERVAL,DERMUL,DERBND,AGROUP,
     +  LMBDA1,LAMFAC,LAMPSI,FRCLAM,NUMRUN,DMAX0,DMAX2,FACORG,PSI3,
     +  PSISTP,NLOPT,NPSILO,NOPTMX,ENDCHG,NUMCHG,NUMREL,NUMFAC,TMPFL,
     +  INFLE,INSFLE,OUTFLE,CLINE,PRECIS,NOPNT,NOBSGP,NOBGNM,AOBSGP,
     +  JPREDICT,NPMXMN,PD0,PD1,PD2,ABPRED,REPRED,ASWPRD,RSWPRD,NPRNRD,
     +  PRSTPA,PRSTPR,NPRDLO,MAXLITN,LTINITFAC,LTFACFAC,IREG,PHIMLIM,
     +  PD1R,FRACPHIM,WFINIT,WFMIN,WFMAX,WFFAC,REGTOL,REGITN,PARJAC,
     +  COMLIN,COMJAC,AJACFLE,IMSFLE,NCOVDIM1,NUMCOV,COVGP,
     +  NOBSCOV,COVAR,MAXAUI,AUIOPT,NOAUIPHIRAT,AUIREST,AUISENSRAT,
     +  AUIHOLDMAXCHG,AUINUMFREE,ALAMPSI,APHIRATACC,NAUINOACC,IVECBND,
     +  IBNDSTK,ICOVOBS,MEMSAV,LINREG,
     +  SVDMODE,MAXSING,EIGTHRESH,EIGWRITE,DI_DERFILE,NOPTSWITCH,
     +  BNPARGP,BPARGPNME,SENFACTOR,PWTKP,IGTARG)

        USE PESTDATA,  ONLY: SQUARE_DMATRIX,LAMFORGIVE,PARSAVEITN,
     +  SVDA,BASEJACFILE,BASEPESTFILE,PESTMODE,XDIM1,XDIM2,DERZEROLIM,
     +  MAXCOMPDIM,NCOMPDIM,XC,IXC,ISTART_C,IFOUND_C,SVDA_MULBPA,
     +  SVDA_SCALADJ,PREDNOISE,SVDA_EXTSUPER,SVDA_SUPDERCALC,LSQRMODE,
     +  LSQR_ATOL,LSQR_BTOL,LSQR_CONLIM,LSQR_ITNLIM,LSQRWRITE,JACUPDATE,
     +  DOSENREUSE,SENRELTHRESH,SENPREDWEIGHT,SENMAXREUSE,SENALLCALCINT,
     +  SENPIEXCLUDE,REGCONTINUE,PHISTOPTHRESH,PHIABANDON,IREI,IJCO,
     +  GTARG,SPLITTHRESH,SPLITRELDIFF,SPLITACTION,SPLITSWH,DOAUI,
     +  IREGADJ,NOPTREGADJ,REGWEIGHTRAT,REGSINGTHRESH,PAR_NOBGNM,
     +  PAR_WTFAC_START,PAR_WTFAC_FIN,NUM_WTFAC_INC,NUM_ITER_START,
     +  NUM_ITER_GEN,NUM_ITER_FIN,ALT_TERM,NOBS_TERM,N_ABOVE_BELOW,
     +  OBS_THRESH,NUM_ITER_THRESH,NOBS_REPORT,IOBS_REPORT,
     +  SVDA_PAR_EXCL,IFINSTOPFLAG,JCOSAVE,VERBOSEREC,BINJACFILE,
     +  MAXABSPARMAX,ABSPARMAX,IREFOBS,DERFORGIVE,IBOUNDSCALE,
     +  IREFOBS_PAUSE,PARFILE_START,PARFILE_END,RUN_RESULTS_FILE,
     +  PARFILEBASE


#ifdef INTEL
        USE COMMON_MOD, ONLY: FLENME,CASEFL
#endif
        IMPLICIT NONE

        INTEGER NPAR,NESPAR,NPARGP,NOBS,NPRIOR,NXROW,IPP,I,J,IES,
     +  NUMRUN,NLOPT,NOPTMX,NPSILO,K,NUMCHG,NUMREL,NUMFAC,NINFLE,
     +  NOUTFL,PRECIS,NOPNT,NOBSGP,JPREDICT,NPMXMN,NPRNRD,NPRDLO,
     +  MAXLITN,IREG,REGITN,NUMCOM,JACFILE,IMSFLE,JFAIL
        INTEGER NCOVDIM1,NUMCOV,JCOV,IR,IC,NCV
        INTEGER MAXAUI,AUIOPT,AUIREST,AUIHOLDMAXCHG,AUINUMFREE,
     +  NAUINOACC
        INTEGER IVECBND,IBNDSTK,ICOVOBS,SVDMODE,MAXSING,
     +  EIGWRITE,NOPTSWITCH,IITEMP
        INTEGER BNPARGP,ISCOUNT,IGTARG,ISPLIT
        INTEGER NTEMP
        INTEGER ABSIND
        DOUBLE PRECISION NOAUIPHIRAT,AUISENSRAT,ALAMPSI,
     +  APHIRATACC,EIGTHRESH
        CHARACTER*10 AAUI,ANUM1*15,AJAC
        INTEGER IDBND(NPARGP),IDERIV(NPARGP),
     +  J2OR3(NPARGP),I3CALC(NPARGP),CHGTYP(NPAR)
        INTEGER IUNIT,MEMSAV,LINREG
        INTEGER ITRANS(NPAR),IPARGP(NPAR),NOBGNM(NXROW),PARJAC(NPAR),
     +  COVGP(NCOVDIM1),NOBSCOV(NCOVDIM1)
        DOUBLE PRECISION LMBDA1,LAMPSI,FRCLAM,LAMFAC,DMAX0,DMAX2,
     +  PSI3,PSISTP,ENDCHG,FACORG,PD0,PD1,PD2,ABPRED,REPRED,
     +  ASWPRD,RSWPRD,PRSTPA,PRSTPR,LTINITFAC,LTFACFAC,PHIMLIM,PD1R,
     +  WFINIT,WFMIN,WFMAX,WFFAC,REGTOL,FRACPHIM,RXFAC
        DOUBLE PRECISION RRTEMP,PWTKP
        TYPE(SQUARE_DMATRIX) COVAR(NCOVDIM1)
        DOUBLE PRECISION PVAL(NPAR),LBOUND(NPAR),UBOUND(NPAR),
     +  OVAL(NXROW),OWGHT(NXROW),X(XDIM1,XDIM2),DERVAL(NPARGP),
     +  DERMUL(NPARGP),DERBND(NPARGP),SCALE(NPAR),OFFSET(NPAR)
        DOUBLE PRECISION SENFACTOR(BNPARGP)
        CHARACTER*3 AVEC,AYESNO,ATEMP2,AREG,AIBS
        CHARACTER*3 ASENPI,ALAMFORGIVE,ADERFORGIVE
        CHARACTER*3 AREFOBS
        CHARACTER*11 ASENWT
        CHARACTER*12 AATEMP
        CHARACTER*20 ATEMP(5),ATEMP1*22,ATEMP4*50
#ifndef INTEL
        CHARACTER*100 CASEFL
        CHARACTER*200 FLENME
#endif
        CHARACTER*12 APAR(NPAR),AGROUP(NPARGP),
     +  AOBSGP(NOBSGP)
        CHARACTER*12 BPARGPNME(BNPARGP)
        CHARACTER*(*) AOBS(NXROW)
        CHARACTER*(*) INFLE(NINFLE),TMPFL(NINFLE),INSFLE(NOUTFL),
     +  OUTFLE(NOUTFL)
        CHARACTER*(*) CLINE,AJACFLE,COMJAC
        CHARACTER*128 TMPSTR
        CHARACTER*(*) COMLIN(NUMCOM)
        CHARACTER*(*) DI_DERFILE
#ifdef INTEL
        INTEGER (8) :: NTEMP8
#else
        INTEGER        NTEMP8
#endif

#ifndef INTEL
        COMMON /FLENME/FLENME
        COMMON /CASE/ CASEFL
#endif
c        EQUIVALENCE (FLENME,ATEMP)

        JFAIL=0
5       WRITE(IUNIT,6,ERR=999)
6       FORMAT(//,' PEST run mode:-',/)
        IF(PESTMODE.EQ.4)THEN
          WRITE(IUNIT,5009)
5009      FORMAT(4X,'Pareto mode')
        ELSE
          IF(IREG.EQ.1)THEN
            WRITE(IUNIT,5010)
5010        FORMAT(4X,'Regularisation mode')
          ELSE
            IF(JPREDICT.NE.0)THEN
7             WRITE(IUNIT,8,ERR=999)
8             FORMAT(4X,'Predictive analyzer mode')
            ELSE
              WRITE(IUNIT,9,ERR=999)
9             FORMAT(4X,'Parameter estimation mode')
            END IF
          END IF
        END IF

        WRITE(IUNIT,10,ERR=999)
10      FORMAT(//,' Case dimensions:-',/)
        WRITE(IUNIT,20,ERR=999) NPAR
20      FORMAT(4X,'Number of parameters',T52,':',I6)
        WRITE(IUNIT,30,ERR=999) NESPAR
30      FORMAT(4X,'Number of adjustable parameters',T52,':',I6)
        WRITE(IUNIT,35,ERR=999) NPARGP
35      FORMAT(4X,'Number of parameter groups',T52,':',I6)
        WRITE(IUNIT,40) NOBS
40      FORMAT(4X,'Number of observations',T52,':',I6)
        WRITE(IUNIT,50,ERR=999) NPRIOR
50      FORMAT(4X,'Number of prior estimates',T52,':',I6)
        IF(MAXCOMPDIM.GT.1)THEN
          WRITE(IUNIT,48,ERR=999)
48        FORMAT(//,' Jacobian compression:-',/)
          CALL WRITINT(ATEMP(1),MAXCOMPDIM)
          WRITE(IUNIT,51,ERR=999) TRIM(ATEMP(1))
51        FORMAT(4X,'Maximum compressed Jacobian elements',T52,
     +    ': ',A)
          WRITE(ATEMP(1),'(1PG10.4)') DERZEROLIM
          ATEMP(1)=ADJUSTL(ATEMP(1))
          WRITE(IUNIT,49,ERR=999) TRIM(ATEMP(1))
49        FORMAT(4X,'Non-zero finite-diff derivatives threshold',
     +    T52,': ',A)
          IF(ABS(JACFILE).EQ.1)THEN
            NTEMP8=NESPAR*NOBS+NCOMPDIM
            IF(MAXCOMPDIM.GE.NTEMP8)THEN
              ATEMP(1)='yes'
            ELSE
              ATEMP(1)='no'
            END IF
            WRITE(IUNIT,56,ERR=999) TRIM(ATEMP(1))
56          FORMAT(4X,'Enough elements for easy-access compression',
     +      T52,': ',A)
          END IF
        END IF

        WRITE(IUNIT,52,ERR=999)
52      FORMAT(/,/,' Model command line(s):-')
        WRITE(IUNIT,*,ERR=999)
        DO 109 I=1,NUMCOM
          WRITE(IUNIT,53,ERR=999) TRIM(COMLIN(I))
53        FORMAT(4X,A)
109     CONTINUE
        WRITE(IUNIT,112,ERR=999)
112     FORMAT(/,/,' Jacobian command line:-',/)
        IF(JACFILE.EQ.0)THEN
          WRITE(IUNIT,53,ERR=999) 'na'
        ELSE
          WRITE(IUNIT,53) TRIM(COMJAC)
          IF(ABS(JACFILE).EQ.1)THEN
            WRITE(IUNIT,115) TRIM(AJACFLE)
115         FORMAT(4X,'Jacobian read from file ',A)
            IF(BINJACFILE.EQ.0)THEN
              WRITE(IUNIT,116)
116           FORMAT(4X,'This is an ASCII file.')
            ELSE
              WRITE(IUNIT,117)
117           FORMAT(4X,'This is a binary file.')
            END IF
          ELSE IF(JACFILE.EQ.2)THEN
            WRITE(IUNIT,114) TRIM(DI_DERFILE)
114         FORMAT(4X,'(JUPITER derivatives interface file is ',A,')')
            WRITE(IUNIT,113) TRIM(AJACFLE)
113         FORMAT(4X,'(Derivatives read from file ',A,')')
          END IF
        END IF

        WRITE(IUNIT,60,ERR=999)
60      FORMAT(/,/,1X,'Model interface files:-')
        WRITE(IUNIT,65,ERR=999)
65      FORMAT(/,4X,'Templates:')
        DO 70 I=1,NINFLE
70      WRITE(IUNIT,72,ERR=999) TRIM(TMPFL(I))
72      FORMAT(7X,A)
        WRITE(IUNIT,75,ERR=999)
75      FORMAT(4X,'for model input files:')
        DO 80 I=1,NINFLE
80      WRITE(IUNIT,72,ERR=999) TRIM(INFLE(I))
        IF(PRECIS.EQ.0)THEN
          ATEMP(1)='single'
        ELSE
          ATEMP(1)='double'
        END IF
        WRITE(IUNIT,88,ERR=999) TRIM(ATEMP(1))
88      FORMAT(/,4X,'(Parameter values written ',
     +  'using ',A,' precision protocol.)')
        IF(NOPNT.EQ.0)THEN
          WRITE(IUNIT,87,ERR=999)
87        FORMAT(4X,'(Decimal point always included.)')
        ELSE
          WRITE(IUNIT,89,ERR=999)
89        FORMAT(4X,'(Decimal point omitted where possible to save ',
     +    'space.)')
        END IF
        WRITE(IUNIT,85,ERR=999)
85      FORMAT(/,4X,'Instruction files:')
        DO 90 I=1,NOUTFL
90      WRITE(IUNIT,72,ERR=999) TRIM(INSFLE(I))
        WRITE(IUNIT,95,ERR=999)
95      FORMAT(4X,'for reading model output files:')
        DO 97 I=1,NOUTFL
97      WRITE(IUNIT,72,ERR=999) TRIM(OUTFLE(I))

        WRITE(IUNIT,502,ERR=999)
502     FORMAT(//,' PEST-to-model message file:-',/)
        IF(IMSFLE.EQ.0)THEN
          WRITE(IUNIT,504,ERR=999)
504       FORMAT(4X,'na')
        ELSE
          WRITE(IUNIT,505,ERR=999)
505       FORMAT(4X,'pest.mmf')
        END IF

        IF(SVDMODE.NE.0)THEN
          WRITE(IUNIT,506,ERR=999)
506       FORMAT(//,' Singular value decomposition:-')
          IF(SVDMODE.EQ.1)THEN
            ANUM1='XtQX'
          ELSE IF(SVDMODE.EQ.2)THEN
            ANUM1='Q^(1/2)X'
          END IF
          WRITE(IUNIT,5061,ERR=999) TRIM(ANUM1)
5061      FORMAT(/,4X,'Perform SVD on XtQX or Q^(1/2)X         ',
     +    '      : ',A)
          CALL WRITINT(ANUM1,MAXSING)
          WRITE(IUNIT,507,ERR=999) TRIM(ANUM1)
507       FORMAT(  4X,'Max. number of singular values to employ',
     +    '      : ',A)
          WRITE(ANUM1,'(1PG13.6)') EIGTHRESH
          WRITE(IUNIT,508,ERR=999) TRIM(ANUM1)
508       FORMAT(  4X,'Ratio of lowest/highest singular value',
     +    '        : ',A)
          IF(EIGWRITE.EQ.0)THEN
            ANUM1='no'
          ELSE
            ANUM1='yes'
          END IF
          WRITE(IUNIT,509,ERR=999) TRIM(ANUM1)
509       FORMAT(   4X,'Record eigenvectors in SVD file',
     +    '               : ',A)
        END IF

        IF(LSQRMODE.NE.0)THEN
          WRITE(IUNIT,1500,ERR=999)
1500      FORMAT(//,' LSQR solution control data:-')
          WRITE(ANUM1,'(1PG13.6)') LSQR_ATOL
          ANUM1=ADJUSTL(ANUM1)
          WRITE(IUNIT,1510,ERR=999) TRIM(ANUM1)
1510      FORMAT(/,4X,'Relative error in Jacobian data (ATOL)       ',
     +    ' : ',A)
          WRITE(ANUM1,'(1PG13.6)') LSQR_BTOL
          ANUM1=ADJUSTL(ANUM1)
          WRITE(IUNIT,1520,ERR=999) TRIM(ANUM1)
1520      FORMAT(  4X,'Relative error in measurement data (BTOL)    ',
     +    ' : ',A)
          WRITE(ANUM1,'(1PG13.6)') LSQR_CONLIM
          ANUM1=ADJUSTL(ANUM1)
          WRITE(IUNIT,1530,ERR=999) TRIM(ANUM1)
1530      FORMAT(  4X,'Upper limit of problem condition no. (CONLIM)',
     +    ' : ',A)
          CALL WRITINT(ANUM1,LSQR_ITNLIM)
          ANUM1=ADJUSTL(ANUM1)
          WRITE(IUNIT,1540,ERR=999) TRIM(ANUM1)
1540      FORMAT(  4X,'Upper iteration limit (ITNLIM)               ',
     +    ' : ',A)
        END IF

        IF(SVDA.NE.0)THEN
          WRITE(IUNIT,512,ERR=999)
512       FORMAT(//,' SVD Assist data:-'/)
          WRITE(IUNIT,513) TRIM(BASEPESTFILE)
513       FORMAT(4X,'PEST control file holding base parameters',
     +    T60,': ',A)
          AYESNO='no'
          IF(SVDA_EXTSUPER.EQ.1)AYESNO='yes'
          WRITE(IUNIT,527) TRIM(AYESNO)
527       FORMAT(4X,'Use external super parameter definition',
     +    T60,': ',A)
          IF(SVDA_EXTSUPER.NE.1)THEN
          WRITE(IUNIT,514) TRIM(BASEJACFILE)
514       FORMAT(4X,'Jacobian matrix file',T60,': ',A)
          IF(SVDA_EXTSUPER.EQ.0)THEN
            WRITE(IUNIT,1515)
1515        FORMAT(4x,'Base parameter definition method',T60,
     +      ': SVD on Q^(1/2)X')
          ELSE IF(SVDA_EXTSUPER.EQ.3)THEN
            WRITE(IUNIT,1514)
1514        FORMAT(4x,'Base parameter definition method',T60,
     +      ': SVD on XtQX')
          ELSE
            WRITE(IUNIT,1516)
1516        FORMAT(4x,'Base parameter definition method',T60,': LSQR')
            AYESNO='no'
            IF(SVDA_EXTSUPER.EQ.-2) AYESNO='yes'
            WRITE(IUNIT,1517) TRIM(AYESNO)
1517        FORMAT(4X,'Orthogonalise base parameters',T60,': ',A)
          END IF
          AYESNO='no'
          IF(SVDA_SUPDERCALC.EQ.1)AYESNO='yes'
          IF(JACFILE.EQ.0)THEN
            WRITE(IUNIT,1518) TRIM(AYESNO)
1518        FORMAT(4X,'Calculate initial Jacobian from base ',
     +      'sensitivities',T60,': ',A)
          ELSE
            WRITE(IUNIT,15181) TRIM(AYESNO)
15181       FORMAT(4X,'Calculate Jacobian from base ',
     +      'sensitivities',T60,': ',A)
          END IF
          ELSE
          WRITE(IUNIT,536) TRIM(BASEJACFILE)
536       FORMAT(4X,'Super parameter definition file',T60,': ',A)
          END IF
          AYESNO='no'
          IF(SVDA_MULBPA.NE.0)AYESNO='yes'
          WRITE(IUNIT,517) TRIM(AYESNO)
517       FORMAT(4X,'Record multiple bpa files',T60,': ',A)
          IF(PESTMODE.EQ.4)THEN
            AYESNO='no'
            IF(SVDA_PAR_EXCL.EQ.1)THEN
              AYESNO='yes'
              WRITE(IUNIT,5171) TRIM(AYESNO)
5171          FORMAT(4X,'Exclude non-Pareto groups from super ',
     +        'param defn',T60,': ',A)
            ELSE IF(SVDA_PAR_EXCL.EQ.-1)THEN
              AYESNO='yes'
              WRITE(IUNIT,5172) TRIM(AYESNO)
5172          FORMAT(4X,'Exclude Pareto groups from super ',
     +        'param defn',T60,': ',A)
            ELSE
              WRITE(IUNIT,5173) TRIM(AYESNO)
5173          FORMAT(4X,'Exclude any groups from super ',
     +        'param defn',T60,': ',A)
            END IF
          END IF
          AYESNO='no'
          IF(SVDA_SCALADJ.NE.0)AYESNO='yes'
          WRITE(IUNIT,518) TRIM(AYESNO)
518       FORMAT(4X,'Perform base parameter scale adjustment',
     +    T60,': ',A)
          IF(SVDA_SCALADJ.NE.0)THEN
            CALL WRITINT(ATEMP2,SVDA_SCALADJ)
            WRITE(IUNIT,519) TRIM(ATEMP2)
519         FORMAT(4X,'SVDA_SCALADJ adjustment control variable',T60,
     +     ': ',A)
            IF(ABS(SVDA_SCALADJ).LE.2)THEN
              WRITE(IUNIT,521)
521           FORMAT(/,4X,'Calculated parameter group base ',
     +        'sensitivity adjustment factors:-')
              WRITE(IUNIT,522)
522           FORMAT(10X,'Group name',T25,'Adjustment factor')
              WRITE(IUNIT,523)
523           FORMAT(10X,'----------',T25,'-----------------')
              ISCOUNT=0
              DO I=1,BNPARGP
                RRTEMP=SENFACTOR(I)
                IF(RRTEMP.GT.0.0D0) THEN
                  WRITE(IUNIT,524) TRIM(BPARGPNME(I)),RRTEMP
524               FORMAT(11X,A,T26,1PG12.5)
                  ISCOUNT=ISCOUNT+1
                END IF
              END DO
              IF(ISCOUNT.EQ.0)THEN
                WRITE(IUNIT,526)
526             FORMAT(10X,'No non-log-transformed parameter groups.')
              END IF
            END IF
          END IF
        END IF

        WRITE(IUNIT,510,ERR=999)
510     FORMAT(//,' Derivatives calculation:-',/)
        WRITE(IUNIT,520,ERR=999)
520     FORMAT(' Param',T15,'Increment',T27,'Increment',
     +  T39,'Increment',T51,'Forward or',T64,'Multiplier',T76,
     +  'Method')
        WRITE(IUNIT,530,ERR=999)
530     FORMAT(' group',T15,'type',T39,'low bound',T51,
     +  'central',T64,'(central)',T76,'(central)')
        DO I=1,NPARGP
          IF(IDERIV(I).EQ.1) THEN
            ATEMP(1)='relative'
          ELSE IF(IDERIV(I).EQ.2) THEN
            ATEMP(1)='absolute'
          ELSE
            ATEMP(1)='rel_to_max'
          END IF
          IF(IDERIV(I).EQ.2)THEN
            ATEMP(2)='  na'
          ELSE IF(IDBND(I).EQ.0) THEN
            ATEMP(2)=' none'
          ELSE
            WRITE(ATEMP(2),540) DERBND(I)
540         FORMAT(1PG10.4)
          END IF
          IF(J2OR3(I).EQ.1) THEN
            ATEMP(3)='  switch'
            WRITE(ATEMP(4),540) DERMUL(I)
          ELSE IF(J2OR3(I).EQ.2) THEN
            ATEMP(3)='always_2'
            ATEMP(4)='  na'
          ELSE IF(J2OR3(I).EQ.3) THEN
            ATEMP(3)='always_3'
            WRITE(ATEMP(4),540) DERMUL(I)
          ELSE IF(J2OR3(I).EQ.4) THEN
            ATEMP(3)='switch_5'
            WRITE(ATEMP(4),540) DERMUL(I)
          ELSE IF(J2OR3(I).EQ.5) THEN
            ATEMP(3)='always_5'
            WRITE(ATEMP(4),540) DERMUL(I)
          END IF
          IF(J2OR3(I).EQ.2) THEN
            ATEMP(5)='  na'
          ELSE IF(I3CALC(I).EQ.1) THEN
            ATEMP(5)='parabolic'
          ELSE IF(I3CALC(I).EQ.2) THEN
            ATEMP(5)='best_fit'
          ELSE IF(I3CALC(I).EQ.3) THEN
            ATEMP(5)='outside_pts'
          ELSE IF(I3CALC(I).EQ.4) THEN
            ATEMP(5)='minimum_variance'
          ELSE IF(I3CALC(I).EQ.5) THEN
            ATEMP(5)='maximum_precision'
          END IF
          WRITE(IUNIT,560,ERR=999) TRIM(AGROUP(I)),
     +    TRIM(ATEMP(1)),DERVAL(I),
     +    TRIM(ATEMP(2)),TRIM(ATEMP(3)),
     +    TRIM(ATEMP(4)),TRIM(ATEMP(5))
560       FORMAT(1X,A,T15,A,T27,1PG10.4,T39,A,T51,A,T65,A,T76,A)
        END DO

        ISPLIT=0
        DO I=1,NPARGP
          IF(SPLITTHRESH(I).GT.0.0D0) ISPLIT=1
        END DO
        IF(ISPLIT.EQ.1)THEN
          WRITE(IUNIT,561,ERR=999)
561       FORMAT(/,' Derivatives problem rectification through split ',
     +    'slope analysis:-',/)
          WRITE(IUNIT,562,ERR=999)
562       FORMAT(' Param',T15,'Slope inaction',T35,'Rel. slope diff',
     +    T56,'Action')
          WRITE(IUNIT,563,ERR=999)
563       FORMAT(' group',T15,'threshold',T35,'threshold')
          DO I=1,NPARGP
            IF(SPLITTHRESH(I).EQ.0.0D0)THEN
              ATEMP(1)='   na'
              ATEMP1=  '   na'
            ELSE
              WRITE(ATEMP(1),540) SPLITRELDIFF(I)
              IF(SPLITACTION(I).EQ.1)THEN
                ATEMP1='use_smaller'
              ELSE IF(SPLITACTION(I).EQ.2)THEN
                ATEMP1='use_zero'
              ELSE IF(SPLITACTION(I).EQ.3)THEN
                ATEMP1='use_previous'
              END IF
            END IF
            WRITE(IUNIT,564) TRIM(AGROUP(I)),SPLITTHRESH(I),
     +      TRIM(ATEMP(1)),TRIM(ATEMP1)
564         FORMAT(1X,A,T15,1PG10.4,T37,A,T55,A)
          END DO
        END IF

        WRITE(IUNIT,100,ERR=999)
100     FORMAT(//,' Parameter definitions:-',/)
        IF(VERBOSEREC.EQ.1)THEN
          WRITE(IUNIT,110,ERR=999)
110       FORMAT(T2,'Name',T15,'Trans-',T33,'Change',T46,'Initial',T61,
     +    'Lower',T76,'Upper')
          WRITE(IUNIT,120,ERR=999)
120       FORMAT(T15,'formation',T33,'limit',T46,'value',T61,'bound',
     +    T76,'bound')
          DO 160 IPP=1,NPAR
          IF(ITRANS(IPP).EQ.0) THEN
            ATEMP1='none'
          ELSE IF(ITRANS(IPP).EQ.1) THEN
            ATEMP1='log'
          ELSE IF(ITRANS(IPP).EQ.-1000000)THEN
            ATEMP1='fixed'
          ELSE
            WRITE(ATEMP1,140)
     +      TRIM(APAR(-ITRANS(IPP)))
140         FORMAT('tied ',A)
          END IF
          IF(CHGTYP(IPP).EQ.0)THEN
            ATEMP(2)='relative'
          ELSE IF(CHGTYP(IPP).EQ.1)THEN
            ATEMP(2)='factor'
          ELSE
            ABSIND=-CHGTYP(IPP)
            CALL WRITINT(AATEMP,ABSIND)
            ATEMP(2)='abs('//TRIM(AATEMP)//')'
          END IF
          IF(ITRANS(IPP).GE.0) THEN
            WRITE(IUNIT,150,ERR=999) TRIM(APAR(IPP)),
     +      TRIM(ATEMP1),TRIM(ATEMP(2)),
     +      PVAL(IPP),LBOUND(IPP),UBOUND(IPP)
150         FORMAT(1X,A,T15,A,T33,A,T43,1PG13.6,T58,1PG13.6,T73,1PG13.6)
          ELSE
            WRITE(IUNIT,155,ERR=999) TRIM(APAR(IPP)),
     +      TRIM(ATEMP1),PVAL(IPP)
155         FORMAT(1X,A,T15,A,T35,'na',T43,1PG13.6,T62,'na',T77,'na')
          END IF
160       CONTINUE
          WRITE(IUNIT,165,ERR=999)
165       FORMAT(/,T2,'Name',T15,'Group',T30,'Scale',T44,'Offset',
     +    T58,'Model command number')
          DO 180 IPP=1,NPAR
            IF(IPARGP(IPP).EQ.0)THEN
              ATEMP(3)='none'
            ELSE
              ATEMP(3)=AGROUP(IPARGP(IPP))
            END IF
            WRITE(IUNIT,170,ERR=999) TRIM(APAR(IPP)),
     +      TRIM(ATEMP(3)),SCALE(IPP),OFFSET(IPP),
     +      PARJAC(IPP)
170         FORMAT(T2,A,T15,A,T27,1PG13.6,T42,1PG13.6,T60,I4)
180       CONTINUE
        ELSE
          WRITE(IUNIT,181)
181       FORMAT(' This information not provided as non-verbose ',
     +    'run record file requested.')
        END IF


        WRITE(IUNIT,200,ERR=999)
200     FORMAT(//,' Prior information:-',/)
        IF(NPRIOR.EQ.0) THEN
          WRITE(IUNIT,205,ERR=999)
205       FORMAT(4X,'No prior information supplied')
          GO TO 401
        END IF
        IF(VERBOSEREC.EQ.0)THEN
          WRITE(IUNIT,181)
          GO TO 401
        END IF
        WRITE(IUNIT,210,ERR=999)
210     FORMAT(' Prior info',T27,'Factor',T46,'Parameter',T69,
     +  'Prior',T83,'Weight')
        WRITE(IUNIT,220,ERR=999)
220     FORMAT(' name',T69,'information')
        ISTART_C=1
        DO 400 I=1,NPRIOR

        CLINE=' '
        WRITE(CLINE,230,ERR=999) TRIM(AOBS(NOBS+I))
230     FORMAT(1X,A20)
        J=NOBS+I
        IES=0
        K=0
        DO 380 IPP=1,NPAR
        IF(ITRANS(IPP).LT.0) GO TO 380
        IES=IES+1
        IF(MAXCOMPDIM.LE.1)THEN
          RXFAC=X(J,IES)
        ELSE
          CALL GET_VALUE(NCOMPDIM,XC,IXC,RXFAC,J,IES)
          ISTART_C=IFOUND_C
        END IF
        IF(RXFAC.EQ.0.0D0) GO TO 380
        K=K+1
        IF(ITRANS(IPP).EQ.1) THEN
          ATEMP(1)='log['
          ATEMP(2)=']'
        ELSE
          ATEMP(1)='    '
          ATEMP(2)=' '
        END IF

        IF(K.EQ.1) THEN
          WRITE(TMPSTR,240,ERR=999) RXFAC,ATEMP(1)(1:4),
     +    TRIM(APAR(IPP)),ATEMP(2)(1:1)
240       FORMAT('    ',1PG13.6,'   ','*',T25,A,A,A)
          CLINE = TRIM(CLINE)// TMPSTR
        ELSE
          CLINE(64:64) = '+'
          WRITE(IUNIT,251) TRIM(CLINE)
          WRITE(CLINE,250,ERR=999) RXFAC,ATEMP(1)(1:4),
     +    TRIM(APAR(IPP)),ATEMP(2)(1:1)
250       FORMAT('                         ',1PG13.6,'   ','*',
     +    T46,A,A,A)
251       FORMAT(A)
        END IF
380     CONTINUE

        IF(NUMCOV.GT.0)THEN
          DO 275 JCOV=1,NUMCOV
            IF(NOBGNM(J).EQ.COVGP(JCOV))THEN
              WRITE(TMPSTR,279,ERR=999) OVAL(J)
279           FORMAT(' =   ',1PG13.6,'  Cov. Mat.')
              GO TO 281
            END IF
275       CONTINUE
        END IF
        WRITE(TMPSTR,280,ERR=999) OVAL(J),SQRT(OWGHT(J))
280     FORMAT(' =   ',1PG13.6,' ',1PG11.4)
281     CONTINUE
        CLINE = CLINE(:62) // TMPSTR
        WRITE(IUNIT,251) TRIM(CLINE)

400     CONTINUE

        WRITE(IUNIT,320)
320     FORMAT(/,' Prior Info Name',T28,'Observation Group')
        DO 350 I=1,NPRIOR
          IF(NOBGNM(NOBS+I).EQ.-1)THEN
            ATEMP1='pr_info'
          ELSE
            ATEMP1=AOBSGP(NOBGNM(NOBS+I))
          END IF
          WRITE(IUNIT,360) TRIM(AOBS(NOBS+I)),
     +    TRIM(ATEMP1)
360       FORMAT(1X,A,T31,A)
350     CONTINUE

401     WRITE(IUNIT,410,ERR=999)
410     FORMAT(//,' Observations:-',/)
        IF(VERBOSEREC.EQ.1)THEN
          WRITE(IUNIT,420,ERR=999)
420       FORMAT(' Observation name',T26,'Observation',T44,'Weight',
     +    T57,'Group')
          DO 500 I=1,NOBS-1
          IF(NUMCOV.GT.0)THEN
            DO 480 JCOV=1,NUMCOV
              IF(NOBGNM(I).EQ.COVGP(JCOV))THEN
                WRITE(IUNIT,459,ERR=999) TRIM(AOBS(I)),
     +          OVAL(I),AOBSGP(NOBGNM(I))
459             FORMAT(T3,A,T25,1PG13.6,T42,' Cov. Mat.',T56,A)
                GO TO 500
              END IF
480         CONTINUE
          END IF
          WRITE(IUNIT,460,ERR=999) TRIM(AOBS(I)),
     +    OVAL(I),SQRT(OWGHT(I)),AOBSGP(NOBGNM(I))
460       FORMAT(T3,A,T25,1PG13.6,T42,1PG11.4,T56,A)
500       CONTINUE
          IF(JPREDICT.EQ.0)THEN
            IF(NUMCOV.GT.0)THEN
              DO 516 JCOV=1,NUMCOV
                IF(NOBGNM(I).EQ.COVGP(JCOV))THEN
                  WRITE(IUNIT,459,ERR=999) TRIM(AOBS(I)),
     +            OVAL(I),AOBSGP(NOBGNM(I))
                  GO TO 503
                END IF
516           CONTINUE
            END IF
            WRITE(IUNIT,460,ERR=999) TRIM(AOBS(NOBS)),
     +      OVAL(NOBS),SQRT(OWGHT(NOBS)),AOBSGP(NOBGNM(NOBS))
503         CONTINUE
          END IF

          IF(JPREDICT.NE.0)THEN
            WRITE(IUNIT,515,ERR=999)
515         FORMAT(//,' Prediction:-',/)
            WRITE(IUNIT,525,ERR=999)
525         FORMAT(' Prediction name',T26,'Prediction',T44,'Weight',
     +      T57,'Group')
            WRITE(IUNIT,535,ERR=999) TRIM(AOBS(NOBS)),
     +      AOBSGP(JPREDICT)
535         FORMAT(T3,A,T32,'na',T47,'na',T56,A)
          END IF
        ELSE
          WRITE(IUNIT,181)
        END IF

        IF(NUMCOV.GT.0)THEN
          WRITE(IUNIT,545,ERR=999)
545       FORMAT(//,' Covariance matrices:-')
          IF(VERBOSEREC.EQ.1)THEN
            DO 605 JCOV=1,NUMCOV
              IF(NOBSCOV(JCOV).LE.0) GO TO 605
              WRITE(IUNIT,546,ERR=999)
     +        TRIM(AOBSGP(COVGP(JCOV)))
546           FORMAT(/,' Covariance matrix for observation group "',A,
     +        '" ---->')
              NCV=NOBSCOV(JCOV)
              DO 550 IR=1,NOBSCOV(JCOV)
                IF((NCV.GT.8).AND.((NCV/8)*8.EQ.NCV))THEN
                  WRITE(IUNIT,566,ERR=999)
     +            (COVAR(JCOV)%DVAL(IC,IR),IC=1,NOBSCOV(JCOV))
566               FORMAT(9(1X,1PG11.4,1X))
                ELSE
                  WRITE(IUNIT,565,ERR=999)
     +            (COVAR(JCOV)%DVAL(IC,IR),IC=1,NOBSCOV(JCOV))
565               FORMAT(8(1X,1PG11.4,1X))
                END IF
550           CONTINUE
605         CONTINUE
          ELSE
            WRITE(IUNIT,*)
            WRITE(IUNIT,181)
          END IF
        END IF

        WRITE(IUNIT,610,ERR=999)
610     FORMAT(//,' Control settings:-',/)
        WRITE(IUNIT,620,ERR=999) LMBDA1
620     FORMAT(4X,'Initial lambda',T66,': ',1PG11.5)
        IF(LAMFAC.GT.0.0D0)THEN
          WRITE(IUNIT,630,ERR=999) LAMFAC
630       FORMAT(4X,'Lambda adjustment factor',T66,': ',1PG11.5)
        ELSE
          WRITE(IUNIT,631,ERR=999)
631       FORMAT(4X,'Lambda adjustment factor',T66,
     +    ': iteration-dependent')
          WRITE(IUNIT,632,ERR=999) LAMFAC
632       FORMAT(4X,'Value of variable governing adjustment',
     +    T66,': ',1PG11.5)
        END IF
        WRITE(IUNIT,640,ERR=999) LAMPSI
640     FORMAT(4X,'Sufficient new/old phi ratio per optimisation ',
     +  'iteration ',T66,': ',1PG11.5)
        WRITE(IUNIT,650,ERR=999) FRCLAM
650     FORMAT(4X,'Limiting relative phi reduction between ',
     +  'lambdas',T66,': ',1PG11.5)
        WRITE(IUNIT,660,ERR=999) NUMRUN
660     FORMAT(4X,'Maximum trial lambdas per iteration',T66,': ',I3)
        IF(LAMFORGIVE.EQ.1)THEN
          ALAMFORGIVE='yes'
        ELSE
          ALAMFORGIVE='no'
        END IF
        WRITE(IUNIT,663,ERR=999) TRIM(ALAMFORGIVE)
663     FORMAT(4X,'Forgive model run failure during lamda testing',
     +  T66,': ',A)
        IF(DERFORGIVE.EQ.1)THEN
          ADERFORGIVE='yes'
        ELSE
          ADERFORGIVE='no'
        END IF
        WRITE(IUNIT,6631,ERR=999) TRIM(ADERFORGIVE)
6631    FORMAT(4X,'Forgive model run failure during Jacobian runs',
     +  T66,': ',A)


        IF(JACUPDATE.GT.0)THEN
          AJAC='yes'
        ELSE
          AJAC='no'
        END IF
        WRITE(IUNIT,661,ERR=999) TRIM(AJAC)
661     FORMAT(/,4X,'Perform Broyden''s update of Jacobian matrix',
     +  T66,': ',A)
        IF(JACUPDATE.GT.0)THEN
          CALL WRITINT(AJAC,JACUPDATE)
          WRITE(IUNIT,662) TRIM(AJAC)
662       FORMAT(4X,'Maximum number of updates per iteration',
     +    T66,': ',A)
        END IF
        IF(IREFOBS.EQ.0)THEN
          AREFOBS='no'
        ELSE
          AREFOBS='yes'
        END IF
        WRITE(IUNIT,664,ERR=999) TRIM(AREFOBS)
664     FORMAT(4X,'Undertake observation re-referencing',T66,': ',A)
        IF(IREFOBS.NE.0)THEN
          IF(IREFOBS_PAUSE.EQ.0)THEN
            AREFOBS='no'
          ELSE
            AREFOBS='yes'
          END IF
          WRITE(IUNIT,665,ERR=999) TRIM(AREFOBS)
665       FORMAT(4X,'Pause after undertaking re-referencing runs ',
     +    '(BEOPEST only)',T66,': ',A)
          IF(IREFOBS_PAUSE.GT.0)THEN
            CALL WRITINT(AATEMP,IREFOBS_PAUSE)
            WRITE(IUNIT,666,ERR=999) TRIM(AATEMP)
666         FORMAT(4X,'Pause time in seconds',T66,': ',A)
          END IF
        END IF

        IF(NUMFAC.EQ.0) THEN
          ATEMP(2)='na'
        ELSE
          WRITE(ATEMP(2),710) DMAX2
        END IF
        WRITE(IUNIT,730,ERR=999) TRIM(ATEMP(2))
730     FORMAT(/,4X,'Maximum  factor  parameter change ',
     +  '(factor-limited changes)',T66,': ',A)
        IF(NUMREL.EQ.0) THEN
          ATEMP(2)='na'
        ELSE
          WRITE(ATEMP(2),710) DMAX0
710       FORMAT(1PG11.5)
        END IF
        WRITE(IUNIT,720,ERR=999) TRIM(ATEMP(2))
720     FORMAT(  4X,'Maximum relative parameter change ',
     +  '(relative-limited changes)',T66,': ',A)
        WRITE(IUNIT,740,ERR=999) FACORG
740     FORMAT(4X,'Fraction of initial parameter values used in ',
     +  'computing',/,4X,'change limit for near-zero parameters',
     +  T66,': ',1PG11.5)
        DO I=1,MAXABSPARMAX
          IF(ABSPARMAX(I).GT.-1.0E35)THEN
            WRITE(ATEMP(2),710) ABSPARMAX(I)
            CALL WRITINT(ATEMP(1),I)
            WRITE(IUNIT,7401) TRIM(ATEMP(1)),TRIM(ATEMP(2))
7401        FORMAT(4X,'Absolute parameter change limit (',A,')',
     +      T66,': ',A)
          END IF
        END DO
        AVEC='no'
        IF(IVECBND.EQ.1)AVEC='yes'
        WRITE(IUNIT,741,ERR=999) TRIM(AVEC)
741     FORMAT(4X,'Allow bending of parameter upgrade vector',
     +  T66,': ',A)
        IF(IBNDSTK.GT.0)THEN
          AVEC='yes'
        ELSE
          AVEC='no'
        END IF
        WRITE(IUNIT,742,ERR=999) TRIM(AVEC)
742     FORMAT(4X,'Allow parameters to stick to their bounds',
     +  T66,': ',A)
        IF(IBNDSTK.GT.0)THEN
          IF(ICOVOBS.NE.0)THEN
            WRITE(IUNIT,744)
744         FORMAT(4X,'However this is over-ridden because a ',
     +      'covariance matrix has been assigned',/,4X,'to at ',
     +      'least some observations.')
          ELSE
            WRITE(IUNIT,743,ERR=999) IBNDSTK
743         FORMAT(4X,'Number of iterations at bound before glue ',
     +      'applied',T66,': ',I2)
          END IF
        END IF

        J=0
        DO 900 I=1,NPARGP
        IF((J2OR3(I).EQ.1).OR.(J2OR3(I).EQ.4)) J=J+1
900     CONTINUE
        IF(J.EQ.0) THEN
          ATEMP(2)='na'
        ELSE
         WRITE(ATEMP(2),710) PSI3
        END IF
        WRITE(IUNIT,920,ERR=999) ATEMP(2)
920     FORMAT(/,4X,'Relative phi reduction below which to begin ',
     +  'use of',/,4X,'central derivatives',T66,': ',A)
        IITEMP=NOPTSWITCH
        IF(IITEMP.LT.1)IITEMP=1
        IF(J.EQ.0)THEN
        WRITE(IUNIT,9211,ERR=999)
9211    FORMAT(4X,'Iteration at which to first consider derivatives ',
     +  'switch ',T66,': na')
        ELSE
        WRITE(IUNIT,921,ERR=999) IITEMP
921     FORMAT(4X,'Iteration at which to first consider derivatives ',
     +  'switch ',T66,':',I4)
        END IF
        IF(ISPLIT.EQ.1)THEN
          IF(SPLITSWH.LE.0.0)THEN
            ATEMP(1)= 'na'
          ELSE
            WRITE(ATEMP(1),710) SPLITSWH
          END IF
          WRITE(IUNIT,922) TRIM(ATEMP(1))
922       FORMAT(4X,'New-to-old phi to commence split slope deriv ',
     +    'analysis ',T66,':',A)
        END IF

        IF(PESTMODE.EQ.4) THEN
        WRITE(IUNIT,10001,ERR=999)
10001   FORMAT(/,4X,'Relative phi reduction indicating ',
     +  'convergence',T66,': na')
        WRITE(IUNIT,10101,ERR=999)
10101   FORMAT(4X,'Number of phi values required within ',
     +  'this range',T66,': na')
        WRITE(IUNIT,10201,ERR=999)
10201   FORMAT(4X,'Maximum number of consecutive failures to lower ',
     +  'phi',T66,': na')
        WRITE(IUNIT,10401,ERR=999)
10401   FORMAT(4X,'Minimal relative parameter change indicating ',
     +  'convergence',T66,': na')
        WRITE(IUNIT,10501,ERR=999)
10501   FORMAT(4X,'Number of consecutive iterations with minimal ',
     +  'param change',T66,': na')
        IF((NOPTMX.EQ.0).OR.(IFINSTOPFLAG.EQ.1))THEN
          WRITE(IUNIT,1030,ERR=999) 0
        ELSE IF(NOPTMX.GT.0)THEN
          WRITE(IUNIT,10301)
10301     FORMAT(4X,'Maximum number of optimisation iterations',
     +    T66,': na')
        ELSE
          ATEMP4='(only compute Jacobian)'
          WRITE(IUNIT,1029,ERR=999) NOPTMX,TRIM(ATEMP4)
        END IF
        WRITE(IUNIT,10281,ERR=999)
10281   FORMAT(4X,'Objective function termination limit',
     +    T66,': na')
        IF(ANY(PHIABANDON.GT.0.0D0))THEN
          WRITE(IUNIT,10231,ERR=999)
10231     FORMAT(4X,'Objective function abandonment active ',
     +    T66,': na')
        END IF
        ELSE
        WRITE(IUNIT,1000,ERR=999) PSISTP
1000    FORMAT(/,4X,'Relative phi reduction indicating ',
     +  'convergence',T66,': ',G11.5)
        WRITE(IUNIT,1010,ERR=999) NPSILO+1
1010    FORMAT(4X,'Number of phi values required within ',
     +  'this range',T66,': ',I3)
        WRITE(IUNIT,1020,ERR=999) NLOPT
1020    FORMAT(4X,'Maximum number of consecutive failures to lower ',
     +  'phi',T66,': ',I3)
        WRITE(IUNIT,1040,ERR=999) ENDCHG
1040    FORMAT(4X,'Minimal relative parameter change indicating ',
     +  'convergence',T66,': ',G11.5)
        WRITE(IUNIT,1050,ERR=999) NUMCHG
1050    FORMAT(4X,'Number of consecutive iterations with minimal ',
     +  'param change',T66,': ',I3)
        IF((NOPTMX.GE.0).OR.(IFINSTOPFLAG.EQ.1))THEN
          IITEMP=NOPTMX
          IF(IFINSTOPFLAG.EQ.1)IITEMP=0
          WRITE(IUNIT,1030,ERR=999) IITEMP
1030      FORMAT(4X,'Maximum number of optimisation iterations',
     +    T66,': ',I3)
        ELSE
          ATEMP4='(only compute Jacobian)'
          WRITE(IUNIT,1029,ERR=999) NOPTMX,TRIM(ATEMP4)
1029      FORMAT(4X,'Maximum number of optimisation iterations',
     +    T66,': ',I3,1X,A)
        END IF
        IF(PHISTOPTHRESH.GT.0.0D0)THEN
          WRITE(IUNIT,1028,ERR=999) PHISTOPTHRESH
1028      FORMAT(4X,'Objective function termination limit',
     +    T66,': ',G11.5)
        END IF
        IF(ANY(PHIABANDON.GT.0.0D0))THEN
          WRITE(IUNIT,1023,ERR=999)
1023      FORMAT(4X,'Objective function abandonment active ',
     +    T66,': yes')
        END IF
        END IF

        AAUI='no'
        IF(MAXAUI.EQ.0)THEN
          WRITE(IUNIT,1031) TRIM(AAUI)
1031      FORMAT(/,4X,'Attempt automatic user intervention',T66,': ',A)
        ELSE
          AAUI='yes'
          IF(DOAUI.EQ.'aui')THEN
            WRITE(IUNIT,1031) TRIM(AAUI)
          ELSE
            WRITE(IUNIT,10311) TRIM(AAUI)
10311       FORMAT(/,4X,'Attempt automatic user intervention',T66,': ',
     +      A,' (for bad derivatives mitigation)')
          END IF
          WRITE(IUNIT,1032) MAXAUI
1032      FORMAT(4X,'Maximum number of AUI iterations',T66,': ',I3)
          WRITE(IUNIT,1033) AUIOPT
1033      FORMAT(4X,'Optimisation iteration to begin AUI',T66,': ',I3)
          WRITE(IUNIT,1034) NOAUIPHIRAT
1034      FORMAT(4X,'Threshold objective function ratio to allow AUI',
     +    T66,': ',G11.5)
          WRITE(IUNIT,1035) AUIREST
1035      FORMAT(4X,'Optimisation iteration rest interval for AUI',
     +    T66,': ',I3)
          WRITE(IUNIT,1036) AUISENSRAT
1036      FORMAT(4X,'High/low parameter sensitivity ratio to allow ',
     +    'AUI',T66,': ',G11.5)
          IF(AUIHOLDMAXCHG.EQ.1)THEN
            AAUI='yes'
          ELSE
            AAUI='no'
          END IF
          WRITE(IUNIT,1037) TRIM(AAUI)
1037      FORMAT(4X,'Preferentially hold parameters which move ',
     +    'maximum amount',T66,': ',A)
          WRITE(IUNIT,1038) AUINUMFREE
1038      FORMAT(4X,'Number of parameters to keep adjustable',T66,
     +    ': ',I3)
          IF(DOAUI.EQ.'aui')THEN
            WRITE(IUNIT,1039) ALAMPSI
1039        FORMAT(4X,'AUI/non-AUI objective function ratio to ',
     +      'terminate AUI',T66,': ',G11.5)
          ELSE
            WRITE(IUNIT,10391) ALAMPSI
10391       FORMAT(4X,'AUI/starting objective function ratio to ',
     +      'terminate AUI',T66,': ',G11.5)
          END IF
          WRITE(IUNIT,1041) APHIRATACC
1041      FORMAT(4X,'New/current objective function ratio for AUI ',
     +    'acceptance',T66,': ',G11.5)
          WRITE(IUNIT,1042) NAUINOACC
1042      FORMAT(4X,'No. of AUI iterations with no acceptable phi ',
     +    'improvement',T66,': ',I3)
        END IF

        AAUI='no'
        IF(DOSENREUSE.EQ.'nosenreuse')THEN
          WRITE(IUNIT,5200) TRIM(AAUI)
5200      FORMAT(/,4X,'Attempt reuse of parameter sensitivities',T66,
     +    ': ',A)
        ELSE
          AAUI='yes'
          WRITE(IUNIT,5200) TRIM(AAUI)
          WRITE(IUNIT,5210) SENALLCALCINT
5210      FORMAT(4X,'Full sensitivity optimisation iteration ',
     +    'interval',T66,': ',i3)
          WRITE(IUNIT,5220) SENRELTHRESH
5220      FORMAT(4X,'Relative sensitivity threshold for re-use',
     +    T66,': ',1PG11.5)
          WRITE(IUNIT,5230) SENMAXREUSE
5230      FORMAT(4X,'Maximum number of re-used sensitivities per ',
     +    'iteration',T66,': ',I3)
          IF(NPRIOR.EQ.0)THEN
            ASENPI='na'
          ELSE
            ASENPI=SENPIEXCLUDE
          END IF
          WRITE(IUNIT,5240) TRIM(ASENPI)
5240      FORMAT(4X,'Exclude prior information from sensitivity ',
     +    'calculations',T66,': ',A)
          IF(JPREDICT.EQ.0)THEN
            ASENWT='na'
          ELSE
            WRITE(ASENWT,'(1PG11.5)') SQRT(MAX(SENPREDWEIGHT,0.0D0))
          END IF
          WRITE(IUNIT,5250) TRIM(ASENWT)
5250      FORMAT(4X,'Weight to apply to predictive sensitivities: '
     +    T66,': ',A)
        END IF

        AIBS='no'
        IF(IBOUNDSCALE.EQ.0)THEN
          AIBS='no'
        ELSE
          AIBS='yes'
        END IF
        WRITE(IUNIT,5251) TRIM(AIBS)
5251    FORMAT(/,4X,'Scale parameters by their bounds ',T66,
     +    ': ',A)
        IF(SVDA.NE.0)THEN
          IF(SVDA_SCALADJ.EQ.4)THEN
            WRITE(IUNIT,5259)
5259        FORMAT(4X,'However super parameter definition uses ',
     +      'a Jacobian scaled by ',/,4X,'native parameter bounds.')
          END IF
        END IF

        WRITE(IUNIT,5260)
5260    FORMAT(//,' File saving options: -',/)
        IF(JCOSAVE.NE.0)THEN
          ATEMP(1)='yes'
        ELSE
          ATEMP(1)='no'
        ENDIF
        WRITE(IUNIT,5261) TRIM(ATEMP(1))
5261    FORMAT(4X,'Save best JCO file',T66,': ',A)
        IF(IJCO.NE.0)THEN
          ATEMP(1)='yes'
        ELSE
          ATEMP(1)='no'
        ENDIF
        WRITE(IUNIT,5270) TRIM(ATEMP(1))
5270    FORMAT(4X,'Save multiple JCO files',T66,': ',A)
        IF(IREI.NE.0)THEN
          ATEMP(1)='yes'
        ELSE
          ATEMP(1)='no'
        END IF
        WRITE(IUNIT,5280) TRIM(ATEMP(1))
5280    FORMAT(4X,'Save multiple REI files',T66,': ',A)
        IF(PARSAVEITN.NE.0)THEN
          ATEMP(1)='yes'
        ELSE
          ATEMP(1)='no'
        END IF
        WRITE(IUNIT,5290) TRIM(ATEMP(1))
5290    FORMAT(4X,'Save multiple PAR files',T66,': ',A)


        IF(JPREDICT.NE.0)THEN
          WRITE(IUNIT,1090,ERR=999)
1090       FORMAT(//,' Extra settings for predictive analysis:-',/)
          IF(NPMXMN.EQ.1)THEN
            ATEMP(1)='maximum'
          ELSE IF(NPMXMN.EQ.-1)THEN
            ATEMP(1)='minimum'
          END IF
          WRITE(IUNIT,1100,ERR=999) TRIM(ATEMP(1))
1100      FORMAT(4X,'Search for maximum or minimum prediction',
     +    T66,': ',A)
          WRITE(IUNIT,1130,ERR=999) PD0
1130      FORMAT(4X,'Phi defining calibration acceptance',T66,': ',
     +    G11.5)
          WRITE(IUNIT,1140,ERR=999) PD1
1140      FORMAT(4X,'Maximum acceptable phi',T66,': ',G11.5)
          WRITE(IUNIT,1150,ERR=999) PD2
1150      FORMAT(4X,'Phi at which relative phi reduction calculation ',
     +    'changes',T66,': ',G11.5)
          IF(PREDNOISE.EQ.0)THEN
            AYESNO='no'
          ELSE
            AYESNO='yes'
          END IF
          WRITE(IUNIT,1151,ERR=999) TRIM(AYESNO)
1151      FORMAT(4X,'Include effect of predictive noise',T66,': ',A)
          IF(PREDNOISE.GT.0)THEN
            WRITE(IUNIT,1152,ERR=999) PWTKP
1152        FORMAT(4X,'Predictive noise weight',T66,': ',G11.5)
          END IF

          WRITE(IUNIT,1160,ERR=999) ABPRED
1160      FORMAT(/,4X,'Limiting absolute prediction change between ',
     +    'lambdas',T66,': ',1PG11.5)
          WRITE(IUNIT,1170,ERR=999) REPRED
1170      FORMAT(4X,'Limiting relative prediction change between ',
     +    'lambdas',T66,': ',1PG11.5)
          IF(MAXLITN.LE.1)THEN
            ATEMP(1)='no'
          ELSE
            ATEMP(1)='yes'
          END IF
          WRITE(IUNIT,1172,ERR=999) TRIM(ATEMP(1))
1172      FORMAT(4X,'Use line search for calculating parameter ',
     +    'upgrade?',T66,': ',A)
          IF(MAXLITN.GT.1)THEN
            WRITE(IUNIT,1174) LTINITFAC
1174        FORMAT(4X,'Initial line search factor',T66,': ',1PG11.5)
            WRITE(IUNIT,1175) LTFACFAC
1175        FORMAT(4X,'Line search refinement factor',T66,': ',
     +      1PG11.5)
            WRITE(IUNIT,1178) MAXLITN
1178        FORMAT(4X,'Maximum number of line search runs per',
     +      ' lambda',T66,': ',I3)
          END IF

          J=0
          DO 1180 I=1,NPARGP
          IF(J2OR3(I).EQ.1) J=J+1
1180        CONTINUE
          IF(J.EQ.0) THEN
            ATEMP(2)='  na'
            ATEMP(3)='  na'
          ELSE
            WRITE(ATEMP(2),710) ASWPRD
            WRITE(ATEMP(3),710) RSWPRD
          END IF
          WRITE(IUNIT,1190,ERR=999) ATEMP(2)
1190      FORMAT(/,4X,'Absolute prediction change below which to ',
     +    'begin use of',/,4X,'central derivatives',T66,': ',A)
          WRITE(IUNIT,1200,ERR=999) ATEMP(3)
1200      FORMAT(4X,'Relative prediction change below which to ',
     +    'begin use of',/,4X,'central derivatives',T66,': ',A)

          WRITE(IUNIT,1210,ERR=999) NPRDLO
1210      FORMAT(/,4X,'Maximum number of consecutive failures to ',
     +    'improve prediction',T66,': ',I3)
          WRITE(IUNIT,1220,ERR=999) PRSTPA
1220      FORMAT(4X,'Minimal absolute prediction change indicating ',
     +    'convergence',T66,': ',G11.5)
          WRITE(IUNIT,1230,ERR=999) PRSTPR
1230      FORMAT(4X,'Minimal relative prediction change indicating ',
     +    'convergence',T66,': ',G11.5)
          WRITE(IUNIT,1240,ERR=999) NPRNRD
1240      FORMAT(4X,'No. of consecutive iterations with ',
     +    'minimal prediction change',T66,': ',I3)

        END IF

        IF(IREG.EQ.1)THEN
          WRITE(IUNIT,5020,ERR=999)
5020      FORMAT(//,' Extra settings for regularisation mode:-',/)
          IF(FRACPHIM.LE.0.0)THEN
            IF(IGTARG.EQ.0)THEN
              WRITE(IUNIT,5030,ERR=999) PHIMLIM
5030          FORMAT(4X,'Target measurement objective function value',
     +        T66,': ',1PG11.5)
              WRITE(IUNIT,5040,ERR=999) PD1R
5040          FORMAT(4X,'Acceptable measurement objective function ',
     +        'value',T66,': ',1PG11.5)
            ELSE
              WRITE(IUNIT,5031,ERR=999)
5031          FORMAT(4X,'Target measurement objective function value ',
     +        T66,': set by group')
            END IF
          ELSE
            WRITE(IUNIT,5045,ERR=999) FRACPHIM
5045        FORMAT(4X,'Target/current measurement objective function ',
     +      'ratio',T66,': ',1PG11.5)
            IF(IGTARG.EQ.0)THEN
              WRITE(IUNIT,5046,ERR=999) PD1R/PHIMLIM
5046          FORMAT(4X,'Acceptable/target measurement objective ',
     +        'function ratio ',T66,': ',1PG11.5)
              WRITE(IUNIT,5047,ERR=999) PHIMLIM
5047          FORMAT(4X,'Minimum target measurement objective ',
     +        'function value ',T66,': ',1PG11.5)
            ELSE
              WRITE(IUNIT,5044,ERR=999)
5044          FORMAT(4X,'Minimum target measurement objective ',
     +        'function value ',T66,': set by group')
            END IF
          END IF
          IF(IGTARG.NE.0)THEN
            DO I=1,NOBSGP
              IF(AOBSGP(I)(1:5).NE.'regul')THEN
                WRITE(IUNIT,5041,ERR=999) TRIM(AOBSGP(I)),GTARG(I)
5041            FORMAT(4X,'Target measurement objective function for ',
     +          'group "',a,'"',T66,': ',1PG11.5)
              END IF
            END DO
          END IF
          IF(REGCONTINUE.EQ.0)THEN
            AREG='yes'
          ELSE
            AREG='no'
          END IF
          WRITE(IUNIT,5048,ERR=999) TRIM(AREG)
5048      FORMAT(4X,'Terminate inversion when measurement obj. fn. ',
     +    'below target',T66,': ',A)

          WRITE(IUNIT,5050,ERR=999) WFINIT
5050      FORMAT(/,4X,'Initial regularisation weight factor',T66,': ',
     +    1PG11.5)
          WRITE(IUNIT,5070,ERR=999) WFMIN
5070      FORMAT(4X,'Minimum regularisation weight factor',T66,': ',
     +    1PG11.5)
          WRITE(IUNIT,5080,ERR=999) WFMAX
5080      FORMAT(4X,'Maximum regularisation weight factor',T66,': ',
     +    1PG11.5)
          WRITE(IUNIT,5060,ERR=999) WFFAC
5060      FORMAT(/,4X,'Weight factor solution adjustment ratio',
     +    T66,': ',1PG11.5)
          WRITE(IUNIT,5090,ERR=999) REGTOL
5090      FORMAT(4X,'Weight factor solution tolerance',T66,': ',
     +    1PG11.5)
c          WRITE(IUNIT,5100,ERR=999)REGITN
c5100      FORMAT(4X,'Maximum number of weight factor solution ',
c     +    'iterations',T66,':',I4)
          IF((IREGADJ.EQ.4).OR.(IREGADJ.EQ.5))THEN
            WRITE(IUNIT,5112,ERR=999) 'yes'
5112        FORMAT(/,4X,'Subspace-enhancement of Tikhonov ',
     +      'regularisation',T66,': ',A)
            CALL WRITINT(ANUM1,NOPTREGADJ)
            WRITE(IUNIT,5108,ERR=999) TRIM(ANUM1)
5108        FORMAT(4X,'Weights re-adjustment iteration interval',
     +      T66,': ',A)
            WRITE(ANUM1,'(1PG12.5)') ABS(REGWEIGHTRAT)
            ANUM1=ADJUSTL(ANUM1)
            WRITE(IUNIT,5109,ERR=999)TRIM(ANUM1)
5109        FORMAT(4X,'Maximum-to-minimum adjusted weight ratio ',
     +      T66,': ',A)
            IF(IREGADJ.EQ.4)THEN
              IF(REGWEIGHTRAT.GT.0.0D0)THEN
                WRITE(IUNIT,5106) 'natural'
              ELSE
                WRITE(IUNIT,5106) 'log'
              END IF
5106          FORMAT(4X,'Maximum-to-minimum adjustment type',
     +        T66,': ',A)
            END IF
            IF(IREGADJ.EQ.4)THEN
              WRITE(IUNIT,5121) 'continuous'
5121          FORMAT(4X,'Subpace cutoff type',T66,': ',A)
            ELSE
              WRITE(IUNIT,5121) 'sharp'
              WRITE(IUNIT,5122) 1.0D0/(REGSINGTHRESH*REGSINGTHRESH)
5122          FORMAT(4X,'Singular value ratio cutoff threshold',
     +        T66,': ',1PG11.5)
            END IF
          ELSE
            IF(IREGADJ.NE.0)THEN
              WRITE(IUNIT,5101,ERR=999) 'yes'
            ELSE
              WRITE(IUNIT,5101,ERR=999) 'no'
            END IF
5101        FORMAT(4X,'Automatic inter-regularisation group weights ',
     +      'adjustment',T66,': ',A)
            IF(IREGADJ.EQ.1)THEN
              WRITE(IUNIT,5103) 'sensitivities'
            ELSE IF (IREGADJ.EQ.2)THEN
              WRITE(IUNIT,5103) 'no. of items'
            ELSE IF (IREGADJ.EQ.3)THEN
              WRITE(IUNIT,5103)'sensitivities+user'
            END IF
5103        FORMAT(4X,'Use sensitivities or number of items',T66,': ',A)
          END IF
          IF(MEMSAV.EQ.1)THEN
            WRITE(IUNIT,5102,ERR=999) 'yes'
          ELSE
            WRITE(IUNIT,5102,ERR=999) 'no'
          END IF
5102      FORMAT(/,4X,'Implement memory conservation',T66,': ',A)
          IF(LINREG.EQ.0)THEN
            WRITE(IUNIT,5111,ERR=999) 'no'
          ELSE
            WRITE(IUNIT,5111,ERR=999) 'yes'
          END IF
5111      FORMAT(4X,'Declare all regularisation information ',
     +    'as linear',T66,': ',A)
        END IF

        IF(PESTMODE.EQ.4)THEN
          WRITE(IUNIT,5520)
5520      FORMAT(//,' Extra settings for Pareto mode:-',/)
          WRITE(IUNIT,5530) TRIM(AOBSGP(PAR_NOBGNM))
5530      FORMAT(4X,'Observation group subject to weights adjustment',
     +    T66,': ',A)
          WRITE(IUNIT,5540) PAR_WTFAC_START
5540      FORMAT(4X,'Starting weight factor',T66,': ',1PG11.5)
          WRITE(IUNIT,5550) PAR_WTFAC_FIN
5550      FORMAT(4X,'Final weight factor',T66,': ',1PG11.5)
          WRITE(IUNIT,5560) NUM_WTFAC_INC
5560      FORMAT(4X,'Number of weight factor increments',T66,': ',
     +    I3)
          WRITE(IUNIT,5570) NUM_ITER_START
5570      FORMAT(/,4X,'Number of optimization iterations for ',
     +    'initial weight factor',T66,': ',I3)
          WRITE(IUNIT,5580) NUM_ITER_GEN
5580      FORMAT(4X,'Number of optimization iterations for ',
     +    'general weight factor',T66,': ',I3)
          WRITE(IUNIT,5590) NUM_ITER_FIN
5590      FORMAT(4X,'Number of optimization iterations for ',
     +    'final weight factor',T66,': ',I3)
          IF(ALT_TERM.EQ.0)THEN
            AREG='no'
          ELSE
            AREG='yes'
          END IF
          WRITE(IUNIT,5600) TRIM(AREG)
5600      FORMAT(/,4x,'Use observation-based termination ',
     +    'threshold',T66,': ',A)
          IF(ALT_TERM.EQ.1)THEN
            WRITE(IUNIT,5610) TRIM(AOBS(NOBS_TERM))
5610        FORMAT(4X,'Name of observation with termination ',
     +      'threshold',T66,': ',A)
            IF(N_ABOVE_BELOW.EQ.1)THEN
              ANUM1='above'
            ELSE
              ANUM1='below'
            END IF
            WRITE(IUNIT,5620) TRIM(ANUM1)
5620        FORMAT(4X,'Observation above or below threshold ',
     +      'for termination',T66,': ',A)
            WRITE(IUNIT,5640) NUM_ITER_THRESH
5640        FORMAT(4X,'Over how many iterations',T66,': ',I3)
          END IF
          WRITE(IUNIT,5650) NOBS_REPORT
5650      FORMAT(/,4X,'Number of observations whose value to ',
     +    'report',T66,': ',I3)
          IF(NOBS_REPORT.EQ.1)THEN
            WRITE(IUNIT,5660) TRIM(AOBS(IOBS_REPORT(1)))
5660        FORMAT(4X,'Reported observation',T66,': ',A)
          ELSE IF(NOBS_REPORT.GT.1)THEN
            WRITE(IUNIT,5670)
5670        FORMAT(4X,'Names of reported observations follow:-')
            DO I=1,NOBS_REPORT
              WRITE(IUNIT,5680) TRIM(AOBS(IOBS_REPORT(I)))
5680          FORMAT(8X,A)
            END DO
          END IF
        END IF
        WRITE(IUNIT,*,ERR=999)
        WRITE(IUNIT,*,ERR=999)

        IF(NOPTMX.EQ.-3)THEN
          WRITE(IUNIT,5690)
5690      FORMAT(1X,'PEST HAS BEEN RUN WITH THE "/f" SWITCH.')
          WRITE(IUNIT,5700)
5700      FORMAT(' A SUITE OF MODEL RUNS WILL BE UNDERTAKEN BASED ON ',
     +    'PARAMETER VALUES RESIDING')
          WRITE(IUNIT,5710)
5710      FORMAT(' IN A SUITE OF PARAMETER VALUE FILES.')
          WRITE(IUNIT,5720) TRIM(PARFILEBASE)
5720      FORMAT(/,4X,'Parameter value files suite : ',A,'*.par')
          CALL WRITINT(AATEMP,PARFILE_START)
          WRITE(IUNIT,5730) TRIM(AATEMP)
5730      FORMAT(  4X,'Starting index              : ',A)
          CALL WRITINT(AATEMP,PARFILE_END)
          WRITE(IUNIT,5740) TRIM(AATEMP)
5740      FORMAT(  4X,'Finishing index             : ',A)
          WRITE(IUNIT,5750) TRIM(RUN_RESULTS_FILE)
5750      FORMAT(  4X,'Run results file            : ',A)
          WRITE(IUNIT,*)
          WRITE(IUNIT,*)
          WRITE(IUNIT,5760)
5760      FORMAT(' RUN RESULTS FOLLOW:-')
        END IF

        RETURN

999     CALL STPERR(26,0,' ',0,' ',CLINE)
        JFAIL=1
        RETURN

#ifdef PESTMOD
        END SUBROUTINE WRTALL
#else
        END
#endif



        SUBROUTINE WRTFIN(JFAIL,IUNIT,NPAR,NOBS,NXROW,NESPAR,NPRIOR,
     +  NPARGP,NWDIM,IOPT,ITRANS,PSILO,PVALO,IFLAG_X,W1,W2,OVAL,OBSLO,
     +  OWGHT,NORM,NORMLO,APAR,AOBS,ICOV,ICOR,IEIG,CLINE,NOBSGP,
     +  NOBGNM,AOBSGP,NEWOBS,JPREDICT,IPRED,PD1,IREG,WFLO,PHIMLO,PD1R,
     +  WFMIN,AGROUP,IPARGP,PWTKP,NCOVDIM1,NCOVDIM3,NUMCOV,TOTWT,
     +  NOBSCOV,COVGP,RWORK1,RWORK2,COVAR,AAPAR,BBPAR,ISTKFLG,IRGP,
     +  NMLODIM1,NMLODIM2,MEMSAV,WORK_CDIM,WORK_C,JUP_PAR_FILES,LBOUND,
     +  UBOUND,
     +  OUTNAM,J_UNIT,J_SS,ISNS,E_PREDLO,RES_PREDLO,JCOFILE,X,
     +  IFLAG_NOCOV,NDIM4,WORKVEC4,PSISUB,GFAC,IGTARG)

        USE PESTDATA,  ONLY: SQUARE_DMATRIX,XDIM1,XDIM2,
     +                       MAXCOMPDIM,NCOMPDIM,XC,IXC,
     +                       ISTART_C,IFOUND_C,IGNF,PREDNOISE,IRSF,
     +                       ERRMSG,SVDMODE,LSQRMODE,PESTMODE,
     +                       VERBOSEREC,COMPRESS_EASY,PI_PAR,IXPC,
     +                       NPR_NONZERO
#ifdef INTEL
        USE COMMON_MOD, ONLY: FLENME,CASEFL
#endif
        IMPLICIT NONE

        INTEGER NPAR,NOBS,NXROW,NESPAR,NPRIOR,IOPT,IES,JES,I,J,NT,NN,
     +  NTIED,NFIXED,ICOV,ICOR,IEIG,INORM,NOBSGP,ITEMP,IOBS,IPP,
     +  IPRED,JPREDICT,ICOUNT,NDEG,IMAX,IMIN,IG,IGG,IOK,NSPR1,NCOUNT,
     +  NWDIM,NB1,NB2,IFAIL,LDA,JFAIL,LNFLAG,J_UNIT,J_SS,ISNS,IGTARG

        INTEGER IUNIT,NSP4,INFO,IREG,NNZERO,NPARGP,JCOV,JWT,IWT,
     +  MEMSAV,WORK_CDIM,JUP_PAR_FILES,JFLAG,IFLAG_X,NMLODIM1,NMLODIM2,
     +  IFLAG_NOCOV

        INTEGER ITEMP_IXP,ICOL

        INTEGER NDIM4,LDU,LDVT

        INTEGER ITRANS(NPAR),NOBGNM(NXROW),IPARGP(NPAR),IRGP(NOBSGP)

        REAL STEMP
        DOUBLE PRECISION RTEMP,TT,PSILO,SS,RTEMP1,PD1,WGHTMX,
     +  WGHTMN,VARIANCE,WMEAN,FVAR,RMEAN1,RMEAN2,RSUM1,RSUM2,RSUM3,
     +  WFLO,PHIMLO,PD1R,PHIMW,PHIRW,WFMIN,SUM,PWTKP,PSIKP,SUM1,
     +  RTEMP_C,SD,STDDEVTRANS,PSIKP_SS,RTEMP10,E_PREDLO,RES_PREDLO,
     +  DTEMP

        DOUBLE PRECISION WORKVEC4(NDIM4),PSISUB(NOBSGP+1),GFAC(NOBSGP)

        INTEGER NCOVDIM1,NCOVDIM3,NUMCOV,TOTWT,IGROUP,NR,
     +  ISTKFLG,IGPNM,IROW
        INTEGER KK,NOBSNZERO
        INTEGER ROTATEFLAG,IPSTART
        INTEGER IT4

        INTEGER NOBSCOV(NCOVDIM1),COVGP(NCOVDIM1)
        DOUBLE PRECISION RWORK1(NCOVDIM3),RWORK2(NCOVDIM3)
        TYPE(SQUARE_DMATRIX) COVAR(NCOVDIM1)
        DOUBLE PRECISION PVALO(NPAR),W1(NWDIM),W2(NWDIM),OVAL(NXROW),
     +  OBSLO(NXROW),OWGHT(NXROW),NORM(NMLODIM1,NMLODIM1),NEWOBS(NXROW),
     +  NORMLO(NMLODIM2,NMLODIM2),STUDT(34),X(XDIM1,XDIM2),
     +  WORK_C(WORK_CDIM)
        DOUBLE PRECISION LBOUND(NPAR),UBOUND(NPAR)
        DOUBLE PRECISION U(1,1),VT(1,1)
        DOUBLE PRECISION DDET(2),PI,AIC,AICC,BIC,KIC,LNS2,NLNS2
        CHARACTER*12 ATEMP3,AANUM*5
        CHARACTER*20 ATEMPR
        CHARACTER*14 ATEMP1,ATEMP2
        CHARACTER*(*) APAR(NPAR),AOBS(NXROW),AOBSGP(NOBSGP),
     +  AGROUP(NPARGP),AAPAR(NPAR),BBPAR(NPAR)
#ifndef INTEL
        CHARACTER*100 CASEFL
        CHARACTER*200 FLENME
#endif
        CHARACTER*200 JUPFILE
        CHARACTER*(*) CLINE
        CHARACTER*(*) OUTNAM
        CHARACTER*(*) JCOFILE
#ifdef INTEL
        INTEGER (8) :: I8
#else
        INTEGER     :: I8
#endif

#ifndef INTEL
        COMMON /FLENME/FLENME
        COMMON /CASE/CASEFL
#endif

        DATA STUDT/12.706,4.303,3.182,2.776,2.571,2.447,2.365,
     +  2.306,2.262,2.228,2.201,2.179,2.160,2.145,2.131,2.120,
     +  2.110,2.101,2.093,2.086,2.080,2.074,2.069,2.064,2.060,
     +  2.056,2.052,2.048,2.045,2.042,2.021,2.000,1.980,1.960/

        JFAIL=0
        NT=0
        LDU=1
        LDVT=1
        PI=3.14159265358979323846
        DDET=-1.0D37                 ! AN ARRAY
        ROTATEFLAG=0

        IF(PESTMODE.EQ.4)THEN
          WRITE(IUNIT,520,ERR=9000)
          FLENME=TRIM(CASEFL)//'.ppd'
          WRITE(IUNIT,6) TRIM(FLENME)
6         FORMAT(/,' Objective functions defining Pareto ',
     +    'front are stored in file ',A)
          WRITE(IUNIT,5)
5         FORMAT(' This file can be imported into any plotting ',
     +    'or spreadsheet package.')
          FLENME=TRIM(CASEFL)//'.pod'
          WRITE(IUNIT,7) TRIM(FLENME)
7         FORMAT(/,' Pareto parameter data are stored in binary ',
     +    'file ',A)
          RETURN
        END IF

        IF(IFLAG_X.EQ.0) GO TO 3600

C -- SENSITIVITIES ARE READ FROM THE JCO FILE.

        FLENME=JCOFILE
        CALL FFOPEN(JFAIL,-IRSF,'r',' ',25,CLINE)
        IF(JFAIL.NE.0)THEN
          WRITE(IUNIT,11)
11        FORMAT(/,1X,'Cannot open JCO file to read sensitivities ',
     +    'pertaining to opimised parameters.',/)
          GO TO 3600
        END IF
        READ(IRSF,ERR=9300,END=9300) ITEMP,ITEMP
        IF(MAXCOMPDIM.LE.1)THEN
          READ(IRSF,ERR=9300,END=9300) ICOUNT
          X=0.00           ! AN ARRAY
          DO I=1,ICOUNT
            READ(IRSF,ERR=9300,END=9300) J,DTEMP
            IES=(J-1)/NXROW+1
            IROW=J-(IES-1)*NXROW
            X(IROW,IES)=DTEMP
          END DO
        ELSE
          READ(IRSF,ERR=9300,END=9300) ITEMP
          NCOMPDIM=ITEMP
          IF(COMPRESS_EASY.EQ.1)THEN
            ITEMP_IXP=1
            DO I=1,NESPAR
              I8=(I-1)*NOBS+PI_PAR(I)
              DO J=1,NOBS
                I8=I8+1
#ifdef SINGLE_XC
                READ(IRSF,ERR=9300,END=9300) ITEMP,STEMP
                XC(I8)=STEMP
#else
                READ(IRSF,ERR=9300,END=9300) ITEMP,XC(I8)
#endif
              END DO
              IF(NPRIOR.NE.0)THEN
401             CONTINUE
                IF(ITEMP_IXP.LE.NPR_NONZERO)THEN
                  ICOL=IXPC(ITEMP_IXP)
                  IF(ICOL.EQ.I)THEN
#ifdef SINGLE_XC
                    READ(IRSF,ERR=9300,END=9300)ITEMP,STEMP
#else
                    READ(IRSF,ERR=9300,END=9300)ITEMP,DTEMP
#endif
                    ITEMP_IXP=ITEMP_IXP+1
                    GO TO 401
                  END IF
                END IF
              END IF
            END DO
          ELSE
            DO I8=1,NCOMPDIM
#ifdef INTEL
              READ(IRSF,ERR=9300,END=9300) IT4,XC(I8)
              IXC(I8)=IT4
#else
              READ(IRSF,ERR=9300,END=9300) IXC(I8),XC(I8)
#endif
            END DO
          END IF
          CLOSE(UNIT=IRSF)
        END IF

C -- IF NECESSARY THE SENSITIVITY MATRIX IS ROTATED.

        IPSTART=1
        IF(NUMCOV.NE.0)THEN
          DO JCOV=1,NUMCOV
            IGROUP=COVGP(JCOV)
            DO I=1,NXROW
              IF(NOBGNM(I).EQ.IGROUP)THEN
                CALL ROTATE(2,NOBSCOV(JCOV),NOBSCOV(JCOV),NXROW,
     +          NESPAR,IPSTART,COVAR(JCOV)%DVAL,W1,X,W2,IGROUP,
     +          NXROW,NOBGNM)
                GO TO 6750
              END IF
            END DO
6750        CONTINUE
          END DO
          ROTATEFLAG=1
        END IF

C -- FIRST THE SENSITIVITY FILE IS COMPLETED.

        IF(IPRED.NE.0)OWGHT(NOBS)=PWTKP*PWTKP

        DO 3005 IPP=1,NPAR
          IF(ITRANS(IPP).LT.-1000001) ITRANS(IPP)=ITRANS(IPP)+2000000
3005    CONTINUE
        WRITE(ISNS,3010)
3010    FORMAT(/,/,/,T25,'COMPLETION OF OPTIMISATION PROCESS',/)
        IF(ISTKFLG.NE.0)THEN
          WRITE(ISNS,3012)
3012      FORMAT(1X,'Note that sensitivities for parameters glued ',
     +      'to their bounds may be in error.',/)
        END IF
        IF(NOBSGP.EQ.1) GOTO 3500
        DO 3300 I=1,NOBSGP
          WRITE(ISNS,3020) TRIM(AOBSGP(I))
3020      FORMAT(' Composite sensitivities for observation group "',A,
     +    '" ----->',/)
          NNZERO=0
          DO 3050 IOBS=1,NXROW
            IF(NOBGNM(IOBS).EQ.I)THEN
              IF(OWGHT(IOBS).GT.0.0D0)NNZERO=NNZERO+1
            END IF
3050      CONTINUE
          IF(NNZERO.EQ.0)THEN
            WRITE(ISNS,3060)
3060        FORMAT(/,1X,'No observations/prior_info with ',
     +      'non-zero weight belong to this group.')
            GO TO 3290
          ELSE
            WRITE(ISNS,3065) NNZERO
3065        FORMAT(' Number of observations with non-zero weight =',I6)
            WRITE(ISNS,3070)
3070        FORMAT(1X,'Parameter name',t20,'Group',t35,
     +      'Current value',t52,'Sensitivity')
          END IF
          IES=0
          ISTART_C=1
          DO 3080 IPP=1,NPAR
            SUM=0.0D0
            IF(ITRANS(IPP).GE.0)THEN
              IES=IES+1
              IF(MAXCOMPDIM.LE.1)THEN
                DO 3090 IOBS=1,NXROW
                  IF(NOBGNM(IOBS).EQ.I)THEN
                    IF(OWGHT(IOBS).GT.0.0D0)THEN
                      SUM=SUM+X(IOBS,IES)*X(IOBS,IES)*OWGHT(IOBS)
                    END IF
                  END IF
3090            CONTINUE
              ELSE
                CALL GET_VECTOR(NCOMPDIM,NXROW,XC,IXC,WORK_C,
     +          IES)
                ISTART_C=IFOUND_C
                DO IOBS=1,NXROW
                  IF(NOBGNM(IOBS).EQ.I)THEN
                    IF(OWGHT(IOBS).GT.0.0D0)THEN
                      SUM=SUM+WORK_C(IOBS)*WORK_C(IOBS)*OWGHT(IOBS)
                    END IF
                  END IF
                END DO
              END IF
              SUM=SQRT(SUM)/NNZERO
              WRITE(ISNS,3100) APAR(IPP),AGROUP(IPARGP(IPP)),PVALO(IPP),
     +        SUM
3100          FORMAT(3X,A12,T20,A12,T35,1PG13.6,T52,1PG13.6)
            END IF
3080      CONTINUE
3290      WRITE(ISNS,3295)
3295      FORMAT(/,/)
3300    CONTINUE
3500    CONTINUE

        IF(IPRED.NE.0)OWGHT(NOBS)=0.0D0

        NCOUNT=0
        NNZERO=0
        DO 3052 IOBS=1,NXROW
          IF(NOBGNM(IOBS).EQ.-1)THEN
            NCOUNT=NCOUNT+1
            IF(OWGHT(IOBS).GT.0.0D0)NNZERO=NNZERO+1
          END IF
3052    CONTINUE
        IF(NCOUNT.EQ.0) GO TO 3059
        WRITE(ISNS,3053)
3053    FORMAT(' Composite sensitivities for ungrouped prior ',
     +  'information ----->',/)
        IF(NNZERO.EQ.0)THEN
          WRITE(ISNS,3058)
3058      FORMAT(1X,'No ungrouped prior information items with ',
     +    'non-zero weight.',/,/)
          GO TO 3059
        END IF
        WRITE(ISNS,3065) NNZERO
        WRITE(ISNS,3070)
        IES=0
        ISTART_C=1
        DO 3055 IPP=1,NPAR
          SUM=0.0D0
          IF(ITRANS(IPP).GE.0)THEN
            IES=IES+1
            IF(MAXCOMPDIM.LE.1)THEN
              DO 3056 IOBS=1,NXROW
                IF(NOBGNM(IOBS).EQ.-1)THEN
                  IF(OWGHT(IOBS).GT.0.0D0)THEN
                    SUM=SUM+X(IOBS,IES)*X(IOBS,IES)*OWGHT(IOBS)
                  END IF
                END IF
3056          CONTINUE
            ELSE
              CALL GET_VECTOR(NCOMPDIM,NXROW,XC,IXC,WORK_C,IES)
              ISTART_C=IFOUND_C
              DO IOBS=1,NXROW
                IF(NOBGNM(IOBS).EQ.-1)THEN
                  IF(OWGHT(IOBS).GT.0.0D0)THEN
                    SUM=SUM+WORK_C(IOBS)*WORK_C(IOBS)*OWGHT(IOBS)
                  END IF
                END IF
              END DO
            END IF
            SUM=SQRT(SUM)/NNZERO
            WRITE(ISNS,3100) APAR(IPP),AGROUP(IPARGP(IPP)),PVALO(IPP),
     +      SUM
          END IF
3055    CONTINUE
        WRITE(ISNS,3295)
3059    CONTINUE

        WRITE(ISNS,3520)
3520    FORMAT(' Composite sensitivities for all ',
     +  'observations/prior info ----->',/)
        NNZERO=0
        DO 3550 IOBS=1,NXROW
          IF(OWGHT(IOBS).GT.0.0D0)NNZERO=NNZERO+1
3550    CONTINUE
        NOBSNZERO=NNZERO
        IF(NNZERO.EQ.0)THEN
          WRITE(ISNS,3061)
3061      FORMAT(/,1X,'No observations/prior_info with ',
     +    'non-zero weight.')
          GO TO 3600
        ELSE
          WRITE(ISNS,3065) NNZERO
          WRITE(ISNS,3070)
        END IF
        IES=0
        ISTART_C=0
        DO 3580 IPP=1,NPAR
          SUM=0.0D0
          IF(ITRANS(IPP).GE.0)THEN
            IES=IES+1
            IF(MAXCOMPDIM.LE.1)THEN
              DO 3590 IOBS=1,NXROW
                IF(OWGHT(IOBS).GT.0.0D0)THEN
                  SUM=SUM+X(IOBS,IES)*X(IOBS,IES)*OWGHT(IOBS)
                END IF
3590          CONTINUE
            ELSE
              CALL GET_VECTOR(NCOMPDIM,NXROW,XC,IXC,WORK_C,IES)
              ISTART_C=IFOUND_C
              DO IOBS=1,NXROW
                IF(OWGHT(IOBS).GT.0.0D0)THEN
                  SUM=SUM+WORK_C(IOBS)*WORK_C(IOBS)*OWGHT(IOBS)
                END IF
              END DO
            END IF
            SUM=SQRT(SUM)/NNZERO
            WRITE(ISNS,3100) APAR(IPP),AGROUP(IPARGP(IPP)),PVALO(IPP),
     +      SUM
          END IF
3580    CONTINUE

        IF(IPRED.NE.0)THEN
          WRITE(ISNS,3585)
3585      FORMAT(/,/,' It is important to note that the observation ',
     +    'assigned to the group "predict"',/' is ignored in ',
     +    'computing all composite sensitivities except for those ',
     +    /,' pertaining to the group "predict".')
        END IF

        IF(ISTKFLG.NE.0)THEN
          WRITE(ISNS,*)
          WRITE(ISNS,3012)
        END IF

3600    CONTINUE
        IF(ROTATEFLAG.EQ.1)THEN
          DO JCOV=1,NUMCOV
            IGROUP=COVGP(JCOV)
            DO I=1,NXROW
              IF(NOBGNM(I).EQ.IGROUP)THEN
                CALL ROTATE(-2,NOBSCOV(JCOV),NOBSCOV(JCOV),NXROW,
     +          NESPAR,IPSTART,COVAR(JCOV)%DVAL,W1,X,W2,IGROUP,
     +          NXROW,NOBGNM)
                GO TO 6751
              END IF
            END DO
6751        CONTINUE
          END DO
        END IF

        INORM=0
        IF(IOPT.EQ.0) GO TO 500
        IF(MEMSAV.NE.0) GO TO 500
        IF(IFLAG_NOCOV.NE.0) GO TO 500
        IF(NORMLO(1,1).LT.-0.99D30)THEN
          IF(NORM(1,1).LT.-0.99D30)THEN
            INORM=1
            GO TO 500
          ELSE
           DO 50 I=1,NESPAR
           DO 50 J=1,NESPAR
50         NORMLO(J,I)=NORM(J,I)
          END IF
        END IF
        NNZERO=0
        DO 51 IOBS=1,NXROW
          IF(OWGHT(IOBS).GT.0.0D0)NNZERO=NNZERO+1
51    CONTINUE
        NT=NNZERO-NESPAR
C        NT=NXROW-NESPAR
        IF(NT.LE.0) GO TO 500
        NSP4=NESPAR
        CALL DPOFA(NORMLO,NSP4,NSP4,INFO)
        IF(INFO.NE.0) THEN
          IOPT=-1
          GO TO 500
        END IF
        INFO=11
        CALL DPODI(NORMLO,NSP4,NSP4,DDET,INFO)
        DO 250 IES=1,NESPAR-1
        DO 250 JES=IES+1,NESPAR
250     NORMLO(JES,IES)=NORMLO(IES,JES)

        IF(NT.LE.30) THEN
          TT=STUDT(NT)
        ELSE IF(NT.LE.40) THEN
          TT=STUDT(30)+(NT-30)*(STUDT(31)-STUDT(30))/10.0D0
        ELSE IF(NT.LE.60) THEN
          TT=STUDT(31)+(NT-40)*(STUDT(32)-STUDT(31))/20.0D0
        ELSE IF(NT.LE.120)THEN
          TT=STUDT(32)+(NT-60)*(STUDT(33)-STUDT(32))/60.0D0
        ELSE
          TT=STUDT(34)
        END IF
        CALL OBJCLC(1,PSIKP,NXROW,NPRIOR,NOBS,OBSLO,OVAL,
     +  OWGHT,NOBGNM,NOBSGP,IRGP,SUM1)
        IF(PREDNOISE.GT.0) PSIKP=PSIKP+RES_PREDLO
        SS=PSIKP/NT
        IF(SS.LT.1.0D-50) SS=1.0D-50
        IES=0
        NTIED=0
        NFIXED=0
        DO 400 I=1,NPAR
        IF(ITRANS(I).LT.-1000001) ITRANS(I)=ITRANS(I)+2000000
        IF(ITRANS(I).EQ.-1000000) THEN
          NFIXED=NFIXED+1
          GO TO 400
        ELSE IF(ITRANS(I).LT.0) THEN
          NTIED=NTIED+1
          GO TO 400
        END IF
        IES=IES+1
        IF(NORMLO(IES,IES).LE.0.0D0) THEN
          IOPT=-1
          GO TO 500
        END IF
        RTEMP=SQRT(NORMLO(IES,IES)*SS)*TT
        IF(ITRANS(I).EQ.0) THEN
          W1(I)=PVALO(I)-RTEMP
          W2(I)=PVALO(I)+RTEMP
        ELSE
          IF(RTEMP.GT.300.0D0)RTEMP=300.0D0
          RTEMP1=10.0D0**RTEMP
          W1(I)=PVALO(I)/RTEMP1
          IF(LOG10(PVALO(I))+RTEMP.LT.300.0D0)THEN
            W2(I)=PVALO(I)*RTEMP1
          ELSE
            W2(I)=1.0D300
          END IF
        END IF
400     CONTINUE

500     WRITE(IUNIT,520,ERR=9000)
520     FORMAT(/,/,T30,'OPTIMISATION RESULTS')
        IF((JUP_PAR_FILES.NE.0).AND.(OUTNAM.NE.' '))THEN
          JFLAG=1
          JUPFILE=TRIM(OUTNAM)//'._pc'
          CALL FFOPEN(IFAIL,J_UNIT,'w',JUPFILE,6,CLINE)
          IF(IFAIL.NE.0) THEN
            JFAIL=1
            RETURN
          END IF
        ELSE
          JFLAG=0
        END IF
        IF(VERBOSEREC.EQ.0)THEN
          WRITE(IUNIT,521)
521       FORMAT(/,/,' Parameters ----->')
          WRITE(IUNIT,522)
522       FORMAT(/,' This information not provided as non-verbose run ',
     +    'record file requested.')
          GO TO 823
        END IF
        IF((IOPT.GE.1).AND.(NT.GT.0).AND.(INORM.EQ.0).AND.
     +    (MEMSAV.EQ.0).AND.(IFLAG_NOCOV.EQ.0))THEN
          IF(NTIED+NFIXED.EQ.0)THEN
            WRITE(IUNIT,777,ERR=9000)
          ELSE
            WRITE(IUNIT,540,ERR=9000)
540         FORMAT(/,/,' Adjustable parameters ----->')
          END IF
          WRITE(IUNIT,560,ERR=9000)
560       FORMAT(/,' Parameter',T19,'Estimated',T37,
     +    '95% percent confidence limits')
          WRITE(IUNIT,580,ERR=9000)
580       FORMAT(T19,'value',T37,'lower limit',T55,'upper limit')
          DO 700 I=1,NPAR
            IF(ITRANS(I).LT.0) GO TO 700
            WRITE(IUNIT,600,ERR=9000) APAR(I),PVALO(I),W1(I),W2(I)
600         FORMAT(2X,A,T17,1PG13.6,T35,1PG14.6,T53,1PG14.6)
700       CONTINUE
          IF((IREG.EQ.0).AND.(IPRED.EQ.0))THEN
            WRITE(IUNIT,701,ERR=9000)
701         FORMAT(/,' Note: confidence limits provide only an ',
     +      'indication of parameter uncertainty.')
            WRITE(IUNIT,702,ERR=9000)
702         FORMAT('       They rely on a linearity assumption which ',
     +      ' may not extend as far in ',/,
     +      '       parameter space as the confidence limits ',
     +      'themselves - see PEST manual.')
          ELSE
            WRITE(IUNIT,703,ERR=9000)
703         FORMAT(/,' Note: When PEST is used in predictive ',
     +      'analysis or regularisation modes,',/,
     +      '       parameter confidence limits are not really ',
     +      'applicable. They are ',/,'       provided ',
     +      'here as an indicator of RELATIVE uncertainty ',
     +      'only.')
          END IF
          IF(ISTKFLG.NE.0)THEN
            WRITE(IUNIT,704,ERR=9000)
704         FORMAT(/,' Note: the accuracy of the confidence ',
     +      'intervals listed above is further ',/,
     +      '       compromised due to the fact that sensitivies ',
     +      'for at least one ',/,
     +      '       parameter have not been calculated since that ',
     +      'parameter was glued ',/,'       at its bound.')
          END IF
          IF(NTIED.NE.0) THEN
            WRITE(IUNIT,705,ERR=9000)
705         FORMAT(/,/,' Tied parameters ----->')
            WRITE(IUNIT,780,ERR=9000)
            DO 710 I=1,NPAR
            IF((ITRANS(I).GE.0).OR.(ITRANS(I).EQ.-1000000)) GO TO 710
            WRITE(IUNIT,800,ERR=9000) APAR(I),PVALO(I)
710         CONTINUE
          END IF
          IF(NFIXED.NE.0) THEN
            WRITE(IUNIT,715,ERR=9000)
715         FORMAT(/,/,' Fixed parameters ----->')
            WRITE(IUNIT,716,ERR=9000)
716         FORMAT(/,' Parameter',T17,'Fixed value')
            DO 718 I=1,NPAR
            IF(ITRANS(I).NE.-1000000) GO TO 718
            WRITE(IUNIT,800,ERR=9000) APAR(I),PVALO(I)
718         CONTINUE
          END IF
          IF(JFLAG.NE.0)THEN
            WRITE(J_UNIT,725)
725         FORMAT(' "PARAMETER NAME" "LOG TRANSFORM" ',
     +      '"LOWER 95% CONFIDENCE LIMIT   " "OPTIMAL VALUE" ',
     +      '"UPPER 95% CONFIDENCE LIMIT   " "STANDARD DEVIATION" ',
     +      '"REASONABLE MINIMUM" "REASONABLE MAXIMUM"')
            IES=0
            LNFLAG=0
            DO I=1,NPAR
              IF(ITRANS(I).LT.0) CYCLE
              IES=IES+1
              IF(ITRANS(I).EQ.1) THEN
                LNFLAG=1
                WRITE(J_UNIT,721)APAR(I),ITRANS(I),LOG10(W1(I)),
     +          LOG10(PVALO(I)),LOG10(W2(I)),SQRT(NORMLO(IES,IES)*SS),
     +          LOG10(LBOUND(I)),LOG10(UBOUND(I))
721             FORMAT(1X,A,I15,6X,G15.7,16X,G15.7,6X,G15.7,16X,G15.7,
     +          5X,G15.7,5X,G15.7)
              ELSE
                WRITE(J_UNIT,721)APAR(I),ITRANS(I),W1(I),
     +          PVALO(I),W2(I),SQRT(NORMLO(IES,IES)*SS),LBOUND(I),
     +          UBOUND(I)
              END IF
            END DO
C -- If any parameters are log transformed, print native values
            IF(LNFLAG.NE.0) THEN
              WRITE(J_UNIT,*)'"TABLE IN NATIVE SPACE, NOTE: ',
     +        'TRANSFORMED STD DEV = ',
     +        ' (10.**((2.3*SD*SD)+(2.*LOG10(PARAMETER VALUE)))) * ',
     +        ' (10.**(2.3*SD*SD))-1.)"'
              IES=0
              DO I = 1,NPAR
                IF(ITRANS(I).LT.0) CYCLE
                IES=IES+1
                IF(ITRANS(I).EQ.1)THEN
                  SD=SQRT(NORMLO(IES,IES)*SS)
                  RTEMP10=2.3*SD*SD
                  IF(RTEMP10.GT.100.0D0) RTEMP10=100.0D0
                  STDDEVTRANS=
     +            (10.**(RTEMP10+(2.*LOG10(PVALO(I)))))*
     +            ((10.**RTEMP10)-1.)
                ELSE
                  STDDEVTRANS=SQRT(NORMLO(IES,IES)*SS)
                ENDIF
                IF(STDDEVTRANS.GT.1.0D98)STDDEVTRANS=1.0D98
                WRITE(J_UNIT,721)APAR(I),ITRANS(I),W1(I),PVALO(I),
     +          W2(I),STDDEVTRANS,LBOUND(I),UBOUND(I)
              END DO
            ENDIF
            CLOSE(UNIT=J_UNIT)
          END IF
        ELSE
          IF(JFLAG.NE.0)THEN
            WRITE(J_UNIT,725)
            LNFLAG=0
            DO I=1,NPAR
              IF(ITRANS(I).LT.0) CYCLE
              IF(ITRANS(I).EQ.1) THEN
                LNFLAG=1
                WRITE(J_UNIT,825)APAR(I),ITRANS(I),'    na',
     +          LOG10(PVALO(I)),'    na','    na',
     +          LOG10(LBOUND(I)),LOG10(UBOUND(I))
825             FORMAT(1X,A,I15,6X,A15,16X,G15.7,6X,A15,16X,A15,
     +          5X,G15.7,5X,G15.7)
              ELSE
                WRITE(J_UNIT,825)APAR(I),ITRANS(I),'    na',
     +          PVALO(I),'    na','    na',LBOUND(I),UBOUND(I)
              END IF
            END DO
            IF(LNFLAG.NE.0) THEN
              WRITE(J_UNIT,*)'"TABLE IN NATIVE SPACE"'
              DO I=1,NPAR
                IF(ITRANS(I).LT.0) CYCLE
                WRITE(J_UNIT,825)APAR(I),ITRANS(I),'    na',
     +          PVALO(I),'    na','    na',LBOUND(I),UBOUND(I)
              END DO
            END IF
            WRITE(J_UNIT,*)
          END IF
          WRITE(IUNIT,720,ERR=9000)
720       FORMAT(/,' Covariance matrix and parameter confidence ',
     +    'intervals cannot be determined:-')
          IF(JFLAG.NE.0) WRITE(J_UNIT,720)
          IF(IOPT.EQ.0) THEN
            WRITE(IUNIT,740,ERR=9000)
740         FORMAT(' No optimisation iterations carried out so ',
     +      'Jacobian never calculated.')
            IF(JFLAG.NE.0) WRITE(J_UNIT,740)
          ELSE IF(IFLAG_NOCOV.NE.0)THEN
            WRITE(IUNIT,741,ERR=9000)
741         FORMAT(' Some form or regularisation was implemented ',
     +      'so these are not applicable.')
            IF(JFLAG.NE.0) WRITE(J_UNIT,741)
            WRITE(IUNIT,7411)
7411        FORMAT(/,' Use the PREDUNC7 utility to obtain a ',
     +      'full posterior covariance matrix.')
            IF(JFLAG.NE.0) WRITE(J_UNIT,7411)
          ELSE IF(IOPT.EQ.-1) THEN
            WRITE(IUNIT,760,ERR=9000)
760         FORMAT(' Normal matrix nearly singular; cannot be ',
     +      'inverted.')
            IF(JFLAG.NE.0) WRITE(J_UNIT,760)
          ELSE IF(NT.LT.0) THEN
            WRITE(IUNIT,770,ERR=9000)
770         FORMAT(' Number of parameters exceeds non-zero-weighted ',
     +      'observations plus prior',/,' information.')
            IF(JFLAG.NE.0) WRITE(J_UNIT,770)
          ELSE IF(INORM.EQ.1)THEN
            WRITE(IUNIT,776,ERR=9000)
776         FORMAT(' Jacobian and/or Normal Matrix not yet ',
     +      'calculated or normal matrix singular.')
            IF(JFLAG.NE.0) WRITE(J_UNIT,776)
          ELSE IF(MEMSAV.NE.0)THEN
            WRITE(IUNIT,782,ERR=9000)
782         FORMAT(' Memory conservation is operative so that ',
     +      'covariance matrix is not calculated.')
            IF(JFLAG.NE.0) WRITE(J_UNIT,782)
          ELSE IF(NT.EQ.0) THEN
            WRITE(IUNIT,775,ERR=9000)
775         FORMAT(' No redundancy in non-zero-weighted observations ',
     +      'plus prior information.')
            IF(JFLAG.NE.0) WRITE(J_UNIT,775)
          END IF
          IF(JFLAG.NE.0) CLOSE(UNIT=J_UNIT)
          WRITE(IUNIT,777,ERR=9000)
777       FORMAT(/,/,' Parameters ----->')
          WRITE(IUNIT,780,ERR=9000)
780       FORMAT(/,' Parameter',T17,'Estimated value')
          DO 820 I=1,NPAR
            WRITE(IUNIT,800,ERR=9000) APAR(I),PVALO(I)
800         FORMAT(2X,A,T17,1PG13.6)
820       CONTINUE
        END IF
823     CONTINUE

        FLENME=TRIM(CASEFL)//'.sen'
        WRITE(IUNIT,821,ERR=9000) TRIM(FLENME)
821     FORMAT(/,' See file ',a,' for parameter sensitivities.')
        IF(IREG.NE.0)THEN
          WRITE(IUNIT,822,ERR=9000)
822       FORMAT(' Note that these sensitivities include effects of ',
     +    'regularisation observations.')
        END IF
        FLENME=TRIM(CASEFL)//'.rec'

        IF(IREG.EQ.1)THEN
          WRITE(IUNIT,5150,ERR=9000)
5150      FORMAT(/,/,' Regularisation Weight Factor ----->')
          WRITE(IUNIT,5160,ERR=9000) WFLO
5160      FORMAT(/,' Optimised regularisation weight factor = ',
     +    1PG11.5)
          WRITE(IUNIT,5170,ERR=9000)
5170      FORMAT(/,' Pertinent observation and prior information ',
     +    'weights shown below are ',/,' calculated using ',
     +    ' this weight factor.')
          IF(PHIMLO.GT.PD1R)THEN
            WRITE(IUNIT,5180,ERR=9000)
5180        FORMAT(/,' Note that the target measurement objective ',
     +      'function was not achieved.')
            IF(ABS(WFLO-WFMIN).LT.1.0D-5*WFMIN)THEN
              WRITE(IUNIT,5182,ERR=9000)
5182          FORMAT(' Consider reducing regularisation weight factor ',
     +        'lower limit.')
            END IF
          END IF
        END IF


C -- THE FOLLOWING CODE EVALUATES VARIANCES FOR USE IN THE RESIDUALS
C    FILE. IT IS DONE HERE, RATHER THAN BELOW, BEFORE OBSERVATIONS ARE
C    ROTATED.

        IF(IPRED.EQ.0)THEN
          NN=NOBS
        ELSE
          NN=NOBS-1
        END IF

        ICOUNT=0
        VARIANCE=0.0D0
        DO 1415 I=1,NN
          IF(IREG.NE.0)THEN
            IGPNM=NOBGNM(I)
            IF(IRGP(IGPNM).NE.0)GO TO 1415
          END IF
          IF(OWGHT(I).GT.0.0D0)THEN
            ICOUNT=ICOUNT+1
            RTEMP=OVAL(I)-OBSLO(I)
            RTEMP=RTEMP*RTEMP*OWGHT(I)
            VARIANCE=VARIANCE+RTEMP
          END IF
1415    CONTINUE
        IF(NPRIOR.GT.0)THEN
          DO 1416 I=NOBS+1,NXROW
            IF(IREG.NE.0)THEN
              IGPNM=NOBGNM(I)
              IF(IRGP(IGPNM).NE.0)GO TO 1416
            END IF
            IF(OWGHT(I).GT.0.0D0)THEN
              ICOUNT=ICOUNT+1
              RTEMP=OVAL(I)-OBSLO(I)
              RTEMP=RTEMP*RTEMP*OWGHT(I)
              VARIANCE=VARIANCE+RTEMP
            END IF
1416      CONTINUE
        END IF
        NDEG=ICOUNT-NESPAR
        IF(NDEG.LE.0) NDEG=ICOUNT
        IF((NDEG.GT.0).AND.(VARIANCE.GT.0.0D0))THEN
          VARIANCE=VARIANCE/NDEG
          FVAR=SQRT(VARIANCE)
        ELSE
          FVAR=1.0D301
        END IF


C -- IF ANY COVARIANCE MATRICES WERE PROVIDED, OBERVATION VALUES (MODELLED
C    AND OBSERVED) ARE NOW UNROTATED.

C -- BUT FIRST A COPY IS KEPT OF THE ROTATED VALUES

        IF(NUMCOV.GT.0)THEN

          IWT=0
          DO 850 JCOV=1,NUMCOV
            IF(NOBSCOV(JCOV).LE.0) GO TO 850
            IGROUP=COVGP(JCOV)
            I=0
            DO 860 J=1,NXROW
              IF(NOBGNM(J).EQ.IGROUP)THEN
                I=I+1
                W1(I)=OVAL(J)
                IWT=IWT+1
                RWORK1(IWT)=OVAL(J)
              END IF
860         CONTINUE
            CALL ROTATE(-1,NOBSCOV(JCOV),NOBSCOV(JCOV),1,1,1,
     +      COVAR(JCOV)%DVAL,W1,X,W2,IGROUP,NXROW,NOBGNM)
            I=0
            DO 870 J=1,NXROW
              IF(NOBGNM(J).EQ.IGROUP)THEN
                I=I+1
                OVAL(J)=W1(I)
              END IF
870        CONTINUE
850       CONTINUE

          IWT=0
          DO 851 JCOV=1,NUMCOV
            IF(NOBSCOV(JCOV).LE.0) GO TO 851
            IGROUP=COVGP(JCOV)
            I=0
            DO 861 J=1,NXROW
              IF(NOBGNM(J).EQ.IGROUP)THEN
                I=I+1
                W1(I)=OBSLO(J)
                IWT=IWT+1
                RWORK2(IWT)=OBSLO(J)
              END IF
861         CONTINUE
            CALL ROTATE(-1,NOBSCOV(JCOV),NOBSCOV(JCOV),1,1,1,
     +      COVAR(JCOV)%DVAL,W1,X,W2,IGROUP,NXROW,NOBGNM)
            I=0
            DO 871 J=1,NXROW
              IF(NOBGNM(J).EQ.IGROUP)THEN
                I=I+1
                OBSLO(J)=W1(I)
              END IF
871        CONTINUE
851       CONTINUE

        END IF


        WRITE(IUNIT,880,ERR=9000)
880     FORMAT(/,/,' Observations ----->')
        IF(VERBOSEREC.EQ.1)THEN
          WRITE(IUNIT,900,ERR=9000)
900       FORMAT(/,' Observation',T25,'Measured',T40,'Calculated',
     +    T55,'Residual',T70,'Weight',T81,'Group')
          WRITE(IUNIT,920,ERR=9000)
920       FORMAT(T25,'value',T40,'value')
          DO 950 I=1,NOBS-1
            IF(OWGHT(I).LE.0.0D0) THEN
              RTEMP=0.0D0
            ELSE
              RTEMP=SQRT(OWGHT(I))
            END IF
            ITEMP=NOBGNM(I)
            IF(NUMCOV.GT.0)THEN
              DO 935 JCOV=1,NUMCOV
                IF(NOBSCOV(JCOV).EQ.0)GO TO 935
                IF(ITEMP.EQ.COVGP(JCOV))THEN
                  WRITE(IUNIT,941,ERR=9000) AOBS(I),OVAL(I),OBSLO(I),
     +            OVAL(I)-OBSLO(I),AOBSGP(ITEMP)
941               FORMAT(1X,A,T23,1PG13.6,T38,1PG13.6,T53,1PG13.6,T68,
     +            ' Cov. Mat.',T81,A)
                  GO TO 950
                END IF
935           CONTINUE
            END IF
            WRITE(IUNIT,940,ERR=9000) AOBS(I),OVAL(I),OBSLO(I),
     +      OVAL(I)-OBSLO(I),RTEMP,AOBSGP(ITEMP)
940         FORMAT(1X,A,T23,1PG13.6,T38,1PG13.6,T53,1PG13.6,T68,1PG11.4,
     +      T81,A)
950       CONTINUE
          IF(IPRED.EQ.0)THEN
            I=NOBS
            IF(OWGHT(I).LE.0.0D0) THEN
              RTEMP=0.0D0
            ELSE
              RTEMP=SQRT(OWGHT(I))
            END IF
            ITEMP=NOBGNM(I)
            IF(NUMCOV.GT.0)THEN
              DO 936 JCOV=1,NUMCOV
                IF(NOBSCOV(JCOV).EQ.0)GO TO 936
                IF(ITEMP.EQ.COVGP(JCOV))THEN
                  WRITE(IUNIT,941,ERR=9000) AOBS(I),OVAL(I),OBSLO(I),
     +            OVAL(I)-OBSLO(I),AOBSGP(ITEMP)
                  GO TO 957
                END IF
936           CONTINUE
            END IF
            WRITE(IUNIT,940,ERR=9000) AOBS(I),OVAL(I),OBSLO(I),
     +      OVAL(I)-OBSLO(I),RTEMP,AOBSGP(ITEMP)
957         CONTINUE
          END IF
        ELSE
          WRITE(IUNIT,522)
        END IF

        IF(NPRIOR.NE.0) THEN
          WRITE(IUNIT,960,ERR=9000)
960       FORMAT(/,/,' Prior information ----->')
          IF(VERBOSEREC.EQ.0)THEN
            WRITE(IUNIT,522)
            GO TO 1021
          END IF
          WRITE(IUNIT,970,ERR=9000)
970       FORMAT(/,' Prior',T25,'Provided',T40,'Calculated',T55,
     +    'Residual',T70,'Weight',T81,'Group')
          WRITE(IUNIT,990,ERR=9000)
990       FORMAT(' information',T25,'value',T40,'value')
          DO 1020 I=NOBS+1,NXROW
          IF(OWGHT(I).LE.0.0D0) THEN
            RTEMP=0.0D0
          ELSE
            RTEMP=SQRT(OWGHT(I))
          END IF
          ITEMP=NOBGNM(I)
          IF(ITEMP.EQ.-1)THEN
            ATEMP3='pr_info'
          ELSE
            ATEMP3=AOBSGP(ITEMP)
          END IF
          IF(NUMCOV.GT.0)THEN
            DO 938 JCOV=1,NUMCOV
              IF(NOBSCOV(JCOV).EQ.0)GO TO 938
              IF(ITEMP.EQ.COVGP(JCOV))THEN
                WRITE(IUNIT,941,ERR=9000) AOBS(I),OVAL(I),OBSLO(I),
     +          OVAL(I)-OBSLO(I),AOBSGP(ITEMP)
                GO TO 1020
              END IF
938         CONTINUE
          END IF
          WRITE(IUNIT,940,ERR=9000) AOBS(I),OVAL(I),OBSLO(I),
     +    OVAL(I)-OBSLO(I),RTEMP,ATEMP3
1020      CONTINUE
1021      CONTINUE
        END IF

        DO 1025 I=1,NXROW
          NEWOBS(I)=SQRT(OWGHT(I))
1025    CONTINUE

C -- THE RESIDUALS FILE IS NOW WRITTEN.

        FLENME=TRIM(CASEFL)//'.res'
        CALL FFOPEN(JFAIL,IGNF,'w',' ',6,CLINE)
        IF(JFAIL.NE.0)THEN
          JFAIL=1
          RETURN
        END IF
        WRITE(IGNF,1410,ERR=9100)
1410    FORMAT(1x,'Name',T23,'Group',T38,'Measured',T55,'Modelled',
     +  T72,'Residual',t89,'Weight',t102,'Weight*Measured',t121,
     +  'Weight*Modelled',t138,'Weight*Residual',t155,
     +  'Measurement_sd',t172,'Natural_weight')

        DO 1430 I=1,NXROW
          IF((IPRED.NE.0).AND.(I.EQ.NOBS)) GO TO 1430
          IF(OWGHT(I).LE.0.0D0) THEN
            RTEMP=0.0D0
          ELSE
            RTEMP=SQRT(OWGHT(I))
          END IF
          IF(FVAR.GT.1.0D300)THEN
            ATEMP1='     na'
            ATEMP2='     na'
          ELSE IF(OWGHT(I).LE.0.0D0)THEN
            ATEMP1='     na'
            ATEMP2='     na'
          ELSE IF((IREG.NE.0).AND.(IRGP(NOBGNM(I)).NE.0))THEN
            ATEMP1='     na'
            ATEMP2='     na'
          ELSE
            WRITE(ATEMP1,1417) (1.0D0/RTEMP)*FVAR
            WRITE(ATEMP2,1417) RTEMP/FVAR
1417        FORMAT(1PG14.7)
          END IF
          ITEMP=NOBGNM(I)
          IF(ITEMP.EQ.-1)THEN
            ATEMP3='pr_info'
          ELSE
            ATEMP3=AOBSGP(ITEMP)
          END IF
          IF(NUMCOV.GT.0)THEN
            DO 1427 JCOV=1,NUMCOV
              IF(NOBSCOV(JCOV).EQ.0)GO TO 1427
              IF(ITEMP.EQ.COVGP(JCOV))THEN
                ATEMP1='     na'
                WRITE(IGNF,1428,ERR=9100)TRIM(AOBS(I)),
     +          TRIM(ATEMP3),OVAL(I),OBSLO(I),
     +          OVAL(I)-OBSLO(I),
     +          TRIM(ATEMP1),TRIM(ATEMP1),
     +          TRIM(ATEMP1),TRIM(ATEMP1),
     +          TRIM(ATEMP1)
1428            FORMAT(1X,A,T23,A,T36,1PG14.7,T53,1PG14.7,T70,1PG14.7,
     +          T87,'  Cov. Mat.',T104,A,T121,A,T138,A,T155,A,T172,A)
                GO TO 1430
              END IF
1427        CONTINUE
          END IF
          WRITE(IGNF,1420,ERR=9100) TRIM(AOBS(I)),
     +    TRIM(ATEMP3),OVAL(I),OBSLO(I),
     +    OVAL(I)-OBSLO(I),RTEMP,RTEMP*OVAL(I),
     +    RTEMP*OBSLO(I),RTEMP*(OVAL(I)-OBSLO(I)),ATEMP1,ATEMP2
1420      FORMAT(1X,A,T23,A,T36,1PG14.7,T53,1PG14.7,T70,1PG14.7,
     +    T87,1PG14.7,T104,1PG14.7,T121,1PG14.7,T138,1PG14.7,
     +    T155,A,T172,A)
1430    CONTINUE
        CLOSE(UNIT=IGNF)

        WRITE(IUNIT,1027,ERR=9000) TRIM(FLENME)
1027    FORMAT(/,' See file ',a,' for more details of residuals in ',
     +  'graph-ready format.')
        IF(TOTWT.GT.0)THEN
          FLENME=TRIM(CASEFL)//'.rsr'
          WRITE(IUNIT,1024,ERR=9000) TRIM(FLENME)
1024      FORMAT(/,' See file ',a,' for details of rotated ',
     +    'residuals in graph-ready format.')
        END IF

C -- The observation sensitivity file is written.

        FLENME=TRIM(CASEFL)//'.seo'
        CALL FFOPEN(JFAIL,IGNF,'w',' ',6,CLINE)
        IF(JFAIL.NE.0)THEN
          JFAIL=1
          RETURN
        END IF
        NSPR1=MAX(NESPAR,1)
        WRITE(IGNF,2900,ERR=9200)
2900    FORMAT(' Observation',t23,' Group',t38,'Measured',t56,
     +  'Modelled',t72,'Sensitivity')
        IF(IFLAG_X.EQ.0) GO TO 2945
        ISTART_C=1
        DO 2910 I=1,NXROW
          IF((IPRED.NE.0).AND.(I.EQ.NOBS)) GO TO 2910
          RSUM1=0.0D0
          IF(MAXCOMPDIM.LE.1)THEN
            DO 2920 J=1,NESPAR
              RSUM1=RSUM1+X(I,J)*X(I,J)
2920        CONTINUE
          ELSE
            DO J=1,NESPAR
              CALL GET_VALUE(NCOMPDIM,XC,IXC,RTEMP_C,I,J)
              RSUM1=RSUM1+RTEMP_C*RTEMP_C
              ISTART_C=IFOUND_C
            END DO
          END IF
          IF(OWGHT(I).GT.0.0D0)THEN
            RSUM1=SQRT(RSUM1*OWGHT(I))
          ELSE
            RSUM1=0.0D0
          END IF
          IF(NOBGNM(I).EQ.-1)THEN
            ATEMP1='pr_info'
          ELSE
            ATEMP1=AOBSGP(NOBGNM(I))
          END IF
          ITEMP=NOBGNM(I)
          IF(NUMCOV.GT.0)THEN
            DO 2938 JCOV=1,NUMCOV
              IF(NOBSCOV(JCOV).EQ.0)GO TO 2938
              IF(ITEMP.EQ.COVGP(JCOV))THEN
                WRITE(IGNF,2931,ERR=9200) TRIM(AOBS(I)),
     +          TRIM(ATEMP1),OVAL(I),OBSLO(I)
2931            FORMAT(1X,A,T24,A,T37,1PG14.7,T55,1PG14.7,T73,
     +          ' Cov. Mat.')
                GO TO 2910
              END IF
2938        CONTINUE
          END IF
          WRITE(IGNF,2930,ERR=9200) TRIM(AOBS(I)),
     +    TRIM(ATEMP1),OVAL(I),OBSLO(I),RSUM1/NSPR1
2930      FORMAT(1X,A,T24,A,T37,1PG14.7,T55,1PG14.7,T73,1PG14.7)
2910    CONTINUE
2945    CLOSE(UNIT=IGNF)

        WRITE(IUNIT,2960,ERR=9000) TRIM(FLENME)
2960    FORMAT(/,' See file ',a,' for composite observation ',
     +  'sensitivities.')


        FLENME=TRIM(CASEFL)//'.rec'

        IF(IPRED.NE.0)THEN
          WRITE(IUNIT,951,ERR=9000)
951       FORMAT(/,/,' Prediction ----->')
          IF(PREDNOISE.EQ.0)THEN
            WRITE(IUNIT,952,ERR=9000)
952         FORMAT(/,' Prediction',T23,'Calculated Value',T43,'Group')
            WRITE(IUNIT,953,ERR=9000) AOBS(NOBS),OBSLO(NOBS)
953         FORMAT(1X,A,T23,1PG13.6,T43,'predict')
          ELSE
            WRITE(IUNIT,955,ERR=9000)
955         FORMAT(/,' Prediction',T23,'Calculated Value',T43,'Group',
     +      T56,'Predictive Error')
            WRITE(IUNIT,956,ERR=9000) AOBS(NOBS),OBSLO(NOBS),E_PREDLO
956         FORMAT(1X,A,T23,1PG13.6,T43,'predict',T56,1PG13.6)
          END IF
          IF(PSILO.GT.PD1)THEN
            WRITE(IUNIT,954)
954         FORMAT('  However objective function is above PD1.')
          END IF
        END IF

C -- IF A COVARIANCE MATRIX WAS SUPPLIED, THE REMAINDER OF THE CALCULATIONS
C    REQUIRES THAT WE ROTATE THE OBSERVATIONS AND THEIR MODEL-GENERATED COUNTERPARTS
C    BACK AGAIN.

        IWT=0
        IF(NUMCOV.GT.0)THEN
          DO 3850 JCOV=1,NUMCOV
            IF(NOBSCOV(JCOV).LE.0) GO TO 3850
            IGROUP=COVGP(JCOV)
            DO 3860 J=1,NXROW
              IF(NOBGNM(J).EQ.IGROUP)THEN
                IWT=IWT+1
                OVAL(J)=RWORK1(IWT)
                OBSLO(J)=RWORK2(IWT)
              END IF
3860        CONTINUE
3850      CONTINUE
        END IF

C -- THE ROTATED RESIDUALS FILE IS NOW WRITTEN IF REQUIRED.
C    NOTE THAT OBSERVATIONS IN ROTATED GROUPS ARE RENAMED. THIS NAME
C    MAY BE USED IN INFORMATION AT END OF RUN RECORD FILE.

        IF(TOTWT.GT.0)THEN
        FLENME=TRIM(CASEFL)//'.rsr'
        CALL FFOPEN(JFAIL,IGNF,'w',' ',6,CLINE)
        IF(JFAIL.NE.0)THEN
          JFAIL=1
          RETURN
        END IF
        WRITE(IGNF,1410,ERR=9100)
        DO 4430 I=1,NXROW
          IF((IPRED.NE.0).AND.(I.EQ.NOBS)) GO TO 4430
          IF(OWGHT(I).LE.0.0D0) THEN
            RTEMP=0.0D0
          ELSE
            RTEMP=SQRT(OWGHT(I))
          END IF
          IF(FVAR.GT.1.0D300)THEN
            ATEMP1='     na'
            ATEMP2='     na'
          ELSE IF(OWGHT(I).LE.0.0D0)THEN
            ATEMP1='     na'
            ATEMP2='     na'
          ELSE IF((IREG.NE.0).AND.(IRGP(NOBGNM(I)).NE.0))THEN
            ATEMP1='     na'
            ATEMP2='     na'
          ELSE
            WRITE(ATEMP1,1417) (1.0D0/RTEMP)*FVAR
            WRITE(ATEMP2,1417) RTEMP/FVAR
1         END IF
          ITEMP=NOBGNM(I)
          IF(ITEMP.EQ.-1)THEN
            ATEMP3='pr_info'
          ELSE
            ATEMP3=AOBSGP(ITEMP)
          END IF
          ATEMPR=AOBS(I)
          DO 4420 JCOV=1,NUMCOV
            IF(NOBSCOV(JCOV).LE.0) GO TO 4420
            IF(ITEMP.EQ.COVGP(JCOV))THEN
              NR=MIN(LEN_TRIM(ATEMPR)+1,19)
              ATEMPR(NR:NR+1)='_r'
C             AOBS(I)=ATEMPR
              GO TO 4421
            END IF
4420      CONTINUE
4421      CONTINUE
          WRITE(IGNF,1420,ERR=9100) TRIM(ATEMPR),
     +    TRIM(ATEMP3),OVAL(I),OBSLO(I),
     +    OVAL(I)-OBSLO(I),RTEMP,RTEMP*OVAL(I),
     +    RTEMP*OBSLO(I),RTEMP*(OVAL(I)-OBSLO(I)),ATEMP1,ATEMP2
4430    CONTINUE
        CLOSE(UNIT=IGNF)
        END IF

        IF(TOTWT.GT.0)THEN
          FLENME=TRIM(CASEFL)//'.rec'
        END IF

C -- MORE INFORMATION IS NOW WRITTEN TO THE RUN RECORD FILE.

        WRITE(IUNIT,1029,ERR=9000)
1029    FORMAT(/,/,' Objective function ----->',/)
        IF(((NOBSGP.GT.1).AND.(IPRED.EQ.0)).OR.
     +     ((NOBSGP.GT.2).AND.(IPRED.NE.0)).OR.
     +      (NPRIOR.NE.0))THEN
            CALL OBJCLC(1,PSISUB(1),NXROW,NPRIOR,NOBS,OBSLO,OVAL,
     +      OWGHT,NOBGNM,NOBSGP,IRGP,SUM1)
            IF(PREDNOISE.GT.0) PSISUB(1)=PSISUB(1)+RES_PREDLO
          WRITE(IUNIT,1028,ERR=9000) PSISUB(1)
1028      FORMAT('   Sum of squared weighted residuals (ie phi)',T62,
     +    '= ',1PG12.5)
          IF(PREDNOISE.NE.0)THEN
            WRITE(IUNIT,1026,ERR=9000) RES_PREDLO
1026        FORMAT('   Contribution to phi from predictive error',T62,
     +      '= ',1PG12.5)
          END IF
          PSIKP_SS=PSISUB(1)
          DO 1031 I=1,NOBSGP
            IF(I.NE.JPREDICT)THEN
              CALL OBJCLC(-I,PSISUB(I),NXROW,NPRIOR,NOBS,OBSLO,OVAL,
     +        OWGHT,NOBGNM,NOBSGP,IRGP,SUM1)
              WRITE(IUNIT,1032,ERR=9000) TRIM(AOBSGP(I)),
     +        PSISUB(I)
1032          FORMAT('   Contribution to phi from observation group "',
     +        A,'"',T62,'= ',1PG12.5)
            END IF
1031      CONTINUE
        ELSE
          CALL OBJCLC(1,PSISUB(1),NXROW,NPRIOR,NOBS,OBSLO,OVAL,
     +    OWGHT,NOBGNM,NOBSGP,IRGP,SUM1)
          IF(PREDNOISE.GT.0) PSISUB(1)=PSISUB(1)+RES_PREDLO
          WRITE(IUNIT,1030,ERR=9000) PSISUB(1)
1030      FORMAT('   Sum of squared weighted residuals (ie phi)',T62,
     +    '= ',1PG11.4)
          IF(PREDNOISE.NE.0)THEN
            WRITE(IUNIT,1026,ERR=9000) RES_PREDLO
          END IF
          PSIKP_SS=PSISUB(1)
        END IF
        IF(NPRIOR.NE.0)THEN
          CALL OBJCLC(-999,DTEMP,NXROW,NPRIOR,NOBS,OBSLO,OVAL,OWGHT,
     +    NOBGNM,NOBSGP,IRGP,SUM1)
          IF(DTEMP.GT.-1.0D300)THEN
            WRITE(IUNIT,1033,ERR=9000) DTEMP
1033        FORMAT('   Contribution to phi from ungrouped prior ',
     +      'information',T62,'= ',1PG12.5)
          END IF
        END IF

        IF(IREG.EQ.1)THEN
          CALL OBJCLC(888888,PHIRW,NXROW,NPRIOR,NOBS,OBSLO,
     +    OVAL,OWGHT,NOBGNM,NOBSGP,IRGP,PHIMW)
          PHIRW=PHIRW/WFLO/WFLO
          WRITE(IUNIT,5200,ERR=9000) PHIMW
5200      FORMAT(/,'   Optimised measurement objective function',T62,
     +    '= ',1PG12.5)
          WRITE(IUNIT,5195,ERR=9000) WFLO
5195      FORMAT('   Optimised regularisation weight factor',T62,
     +    '= ',1PG12.5)
          WRITE(IUNIT,5210,ERR=9000) PHIRW
5210      FORMAT('   Optimised regularisation objective function',
     +    T62,'= ',1PG12.5)
        END IF

        IF(IGTARG.NE.0)THEN
          WRITE(IUNIT,5752,ERR=9000)
5752      FORMAT(/,'   Measurement objective functions with ',
     +    'original weights:-')
          DO I=1,NOBSGP
            IF(IRGP(I).EQ.0)THEN
              WRITE(IUNIT,5297,ERR=9000)
     +        TRIM(AOBSGP(I)),PSISUB(I)/GFAC(I)
5297          FORMAT('   Measurement phi for observation group "',
     +        A,'"',T62,'= ',1PG12.5)
            END IF
          END DO
        END IF

        IF((JUP_PAR_FILES.NE.0).AND.(OUTNAM.NE.' ').AND.
     +     (NT.GT.0))THEN
          WRITE(J_SS,5211) PSIKP_SS/NT
5211      FORMAT(' STANDARD ERROR OF REGRESSION = ',1PG14.7)
        END IF

2800    CONTINUE
        WRITE(IUNIT,2810,ERR=9000)
2810    FORMAT(/,/,' Correlation Coefficient ----->')
        IF(IPRED.EQ.0)THEN
          NN=NOBS
        ELSE
          NN=NOBS-1
        END IF
        ICOUNT=0
        RMEAN1=0.0D0
        RMEAN2=0.0D0
        DO 2820 I=1,NN
          IF((IREG.EQ.1).AND.(IRGP(NOBGNM(I)).NE.0)) GO TO 2820
          IF(OWGHT(I).GT.0.0D0)THEN
            ICOUNT=ICOUNT+1
            RTEMP=SQRT(OWGHT(I))
            RMEAN1=RMEAN1+RTEMP*OVAL(I)
            RMEAN2=RMEAN2+RTEMP*OBSLO(I)
          END IF
2820    CONTINUE
        IF(NPRIOR.GT.0)THEN
          DO 2830 I=NOBS+1,NXROW
            IF((IREG.EQ.1).AND.(IRGP(NOBGNM(I)).NE.0)) GO TO 2830
            IF(OWGHT(I).GT.0.0D0)THEN
              ICOUNT=ICOUNT+1
              RTEMP=SQRT(OWGHT(I))
              RMEAN1=RMEAN1+RTEMP*OVAL(I)
              RMEAN2=RMEAN2+RTEMP*OBSLO(I)
            END IF
2830      CONTINUE
        END IF
        IF(ICOUNT.NE.0)THEN
          RMEAN1=RMEAN1/ICOUNT
          RMEAN2=RMEAN2/ICOUNT
        ELSE
          WRITE(IUNIT,2840,ERR=9000)
2840      FORMAT(/,'   Correlation coefficient cannot be calculated.')
          GO TO 2000
        END IF
        RSUM1=0.0D0
        RSUM2=0.0D0
        RSUM3=0.0D0
        DO 2850 I=1,NN
          IF((IREG.EQ.1).AND.(IRGP(NOBGNM(I)).NE.0)) GO TO 2850
          IF(OWGHT(I).GT.0.0D0)THEN
            RTEMP=SQRT(OWGHT(I))
            RSUM1=RSUM1+(RTEMP*OVAL(I)-RMEAN1)*
     +                  (RTEMP*OBSLO(I)-RMEAN2)
            RSUM2=RSUM2+(RTEMP*OVAL(I)-RMEAN1)*
     +                  (RTEMP*OVAL(I)-RMEAN1)
            RSUM3=RSUM3+(RTEMP*OBSLO(I)-RMEAN2)*
     +                  (RTEMP*OBSLO(I)-RMEAN2)
          END IF
2850    CONTINUE
        IF(NPRIOR.GT.0)THEN
          DO 2860 I=NOBS+1,NXROW
            IF((IREG.EQ.1).AND.(IRGP(NOBGNM(I)).NE.0)) GO TO 2860
            IF(OWGHT(I).GT.0.0D0)THEN
              RTEMP=SQRT(OWGHT(I))
              RSUM1=RSUM1+(RTEMP*OVAL(I)-RMEAN1)*
     +                    (RTEMP*OBSLO(I)-RMEAN2)
              RSUM2=RSUM2+(RTEMP*OVAL(I)-RMEAN1)*
     +                    (RTEMP*OVAL(I)-RMEAN1)
              RSUM3=RSUM3+(RTEMP*OBSLO(I)-RMEAN2)*
     +                    (RTEMP*OBSLO(I)-RMEAN2)
            END IF
2860      CONTINUE
        END IF
        IF((RSUM2.LE.0.0D0).OR.(RSUM3.LE.0.0D0))THEN
          WRITE(IUNIT,2840,ERR=9000)
        ELSE
          RTEMP=RSUM1/SQRT(RSUM2*RSUM3)
          WRITE(IUNIT,2870,ERR=9000) RTEMP
2870      FORMAT(/,'   Correlation coefficient',T62,'= ',1PG12.5)
          IF(IREG.EQ.1)THEN
            WRITE(IUNIT,2875,ERR=9000)
2875        FORMAT('   (Includes measurement observations only.)')
          END IF
        END IF


2000    CONTINUE
        WRITE(IUNIT,2010,ERR=9000)
2010    FORMAT(/,/,' Analysis of residuals ----->')
        DO 2690 IG=1,NOBSGP+2
          IGG=-999
          IF(IG.EQ.1)THEN
            IF(NUMCOV.GT.0)THEN
              DO 2009 JCOV=1,NUMCOV
                IF(NOBSCOV(JCOV).LE.0) GO TO 2009
                WRITE(IUNIT,2021,ERR=9000)
2021            FORMAT(/,'   All residuals (rotated as necessary):-')
                GO TO 2008
2009          CONTINUE
            END IF
            WRITE(IUNIT,2020,ERR=9000)
2020        FORMAT(/,'   All residuals:-')
2008        CONTINUE
            IGG=0
          ELSE IF((IG.GE.2).AND.(IG.LE.NOBSGP+1))THEN
            IGG=IG-1
            IF(IPRED.NE.0)THEN
              IF(AOBSGP(IGG).EQ.'predict') THEN
                IGG=-999
              ELSE
                IF(NOBSGP.EQ.2) THEN
                  IGG=-999
                ELSE
                  WRITE(IUNIT,2022,ERR=9000)TRIM(AOBSGP(IGG))
                END IF
              END IF
            ELSE
              IF(NOBSGP.EQ.1)THEN
                IGG=-999
              ELSE
                IF(NUMCOV.GT.0)THEN
                  DO 2032 JCOV=1,NUMCOV
                    IF(NOBSCOV(JCOV).EQ.0) GO TO 2032
                    IF(IGG.EQ.COVGP(JCOV))THEN
                      WRITE(IUNIT,2033,ERR=9000) TRIM(AOBSGP(IGG))
2033                  FORMAT(/,'   Rotated residuals for ',
     +                'observation group "',A,'":-')
                      GO TO 2034
                    END IF
2032              CONTINUE
                END IF
                WRITE(IUNIT,2022,ERR=9000)TRIM(AOBSGP(IGG))
2022            FORMAT(/,'   Residuals for observation group "',
     +          A,'":-')
2034            CONTINUE
              END IF
            END IF
          ELSE
            IF(NPRIOR.EQ.0)THEN
              IGG=-999
            ELSE
              ICOUNT=0
              DO 2023 I=NOBS+1,NXROW
                IF(NOBGNM(I).EQ.-1) ICOUNT=ICOUNT+1
2023          CONTINUE
              IF(ICOUNT.EQ.0)THEN
                IGG=-999
              ELSE
                IGG=-1
                WRITE(IUNIT,2024,ERR=9000)
2024            FORMAT(/,'   Ungrouped prior information residuals:-')
              END IF
            END IF
          END IF
          IF(IGG.EQ.-999) GO TO 2690
          ICOUNT=0
          WGHTMN=1.0D300
          WGHTMX=-1.0D300
          WMEAN=0.0D0
          VARIANCE=0.0D0
          DO 2025 I=1,NXROW
            IF(OWGHT(I).GT.0.0D0) THEN
              IOK=0
              IF(IGG.EQ.0)THEN
                IF(I.GT.NOBS)THEN
                  IOK=1
                ELSE IF(NOBGNM(I).EQ.JPREDICT)THEN
                  IOK=0
                ELSE
                  IOK=1
                END IF
              ELSE IF(IGG.EQ.-999)THEN
                IOK=0
              ELSE IF(IGG.EQ.-1)THEN
                IF(NOBGNM(I).EQ.-1) IOK=1
              ELSE
                IF(NOBGNM(I).EQ.IGG) IOK=1
              END IF
              IF(IOK.EQ.0) GO TO 2025
              ICOUNT=ICOUNT+1
              RTEMP=(OVAL(I)-OBSLO(I))*SQRT(OWGHT(I))
              IF(RTEMP.GT.WGHTMX) THEN
                WGHTMX=RTEMP
                IMAX=I
              END IF
              IF(RTEMP.LT.WGHTMN) THEN
                WGHTMN=RTEMP
                IMIN=I
              END IF
              WMEAN=WMEAN+RTEMP
              VARIANCE=VARIANCE+RTEMP*RTEMP
            END IF
2025      CONTINUE
          WRITE(IUNIT,2030,ERR=9000) ICOUNT
2030      FORMAT('      Number of residuals with non-zero weight',T70,
     +    '=',I6)
          IF(ICOUNT.LE.0) GO TO 2690
          WRITE(IUNIT,2040,ERR=9000) WMEAN/ICOUNT
2040      FORMAT('      Mean value of non-zero weighted residuals',
     +    T70,'= ',1PG11.4)
          ATEMPR=AOBS(IMAX)
          IF(NUMCOV.NE.0)THEN
            ITEMP=NOBGNM(IMAX)
            DO JCOV=1,NUMCOV
              IF(NOBSCOV(JCOV).LE.0) CYCLE
                IF(ITEMP.EQ.COVGP(JCOV))THEN
                  NR=MIN(LEN_TRIM(ATEMPR)+1,19)
                  ATEMPR(NR:NR+1)='_r'
                  EXIT
                END IF
            END DO
          END IF
          WRITE(IUNIT,2050,ERR=9000) TRIM(ATEMPR),WGHTMX
2050      FORMAT('      Maximum weighted residual ',
     +    '[observation "',A,'"]',T70,'= ',1PG11.4)
          ATEMPR=AOBS(IMIN)
          IF(NUMCOV.NE.0)THEN
            ITEMP=NOBGNM(IMIN)
            DO JCOV=1,NUMCOV
              IF(NOBSCOV(JCOV).LE.0) CYCLE
                IF(ITEMP.EQ.COVGP(JCOV))THEN
                  NR=MIN(LEN_TRIM(ATEMPR)+1,19)
                  ATEMPR(NR:NR+1)='_r'
                  EXIT
                END IF
            END DO
          END IF
          WRITE(IUNIT,2055,ERR=9000) TRIM(ATEMPR),WGHTMN
2055      FORMAT('      Minimum weighted residual ',
     +    '[observation "',A,'"]',T70,'= ',1PG11.4)
          IF(IGG.EQ.0)THEN
            NDEG=ICOUNT-NESPAR
            IF(NDEG.LE.0) NDEG=ICOUNT
            WRITE(IUNIT,2060,ERR=9000) VARIANCE/NDEG
2060        FORMAT('      Standard variance of weighted residuals',
     +      T70,'= ',1PG11.4)
            WRITE(IUNIT,2070,ERR=9000) SQRT(VARIANCE/NDEG)
2070        FORMAT('      Standard error of weighted residuals',
     +      T70,'= ',1PG11.4)
          ELSE
            NDEG=ICOUNT
            WRITE(IUNIT,2062,ERR=9000) VARIANCE/NDEG
2062        FORMAT('      "Variance" of weighted residuals',
     +      T70,'= ',1PG11.4)
            WRITE(IUNIT,2072,ERR=9000) SQRT(VARIANCE/NDEG)
2072        FORMAT('      "Standard error" of weighted residuals',
     +      T70,'= ',1PG11.4)
          END IF

2500      CONTINUE
          IF(IGG.EQ.0)THEN
            WRITE(IUNIT,2510,ERR=9000)
2510        FORMAT(/,'      Note: the above variance was obtained ',
     +      'by dividing the objective ')
            WRITE(IUNIT,2520,ERR=9000)
2520        FORMAT('      function by the number of system degrees of ',
     +      'freedom (ie. number of ')
            WRITE(IUNIT,2530,ERR=9000)
2530        FORMAT('      observations with non-zero weight plus ',
     +      'number of prior information ')
            WRITE(IUNIT,2540,ERR=9000)
2540        FORMAT('      articles with non-zero weight minus the ',
     +      'number of adjustable parameters.)')
            WRITE(IUNIT,2560,ERR=9000)
2560        FORMAT('      If the degrees of freedom is negative ',
     +      'the divisor becomes ')
            WRITE(IUNIT,2570,ERR=9000)
2570        FORMAT('      the number of observations with ',
     +      'non-zero weight plus the number of ')
            WRITE(IUNIT,2580,ERR=9000)
2580        FORMAT('      prior information items with ',
     +      'non-zero weight.')
          ELSE
            WRITE(IUNIT,2512,ERR=9000)
2512        FORMAT(/,'      Note: the above "variance" was obtained ',
     +      'by dividing the sum of squared ')
            WRITE(IUNIT,2522,ERR=9000)
2522        FORMAT('      residuals by the number of items with ',
     +      'non-zero weight.')
          END IF

2690    CONTINUE

2700    CONTINUE
        IF((IOPT.LE.0).OR.(NT.LE.0).OR.(INORM.EQ.1).OR.
     +  (MEMSAV.NE.0)) THEN
          WRITE(IUNIT,2702,ERR=9000)
2702      FORMAT(/,/,' Covariance and other statistical matrices ',
     +    'cannot be determined:-')
          IF(IOPT.EQ.0) THEN
            WRITE(IUNIT,740,ERR=9000)
          ELSE IF(IFLAG_NOCOV.NE.0)THEN
            WRITE(IUNIT,741,ERR=9000)
            WRITE(IUNIT,7411)
          ELSE IF(IOPT.EQ.-1) THEN
            WRITE(IUNIT,760,ERR=9000)
          ELSE IF(NT.LT.0) THEN
            WRITE(IUNIT,770,ERR=9000)
          ELSE IF(INORM.EQ.1)THEN
            WRITE(IUNIT,776,ERR=9000)
          ELSE IF(MEMSAV.NE.0)THEN
            WRITE(IUNIT,782,ERR=9000)
          ELSE IF(NT.EQ.0) THEN
            WRITE(IUNIT,775,ERR=9000)
          END IF
          WRITE(IUNIT,*,ERR=9000)
          GO TO 5000
        END IF

        IES=0
        DO 3200 IPP=1,NPAR
          IF(ITRANS(IPP).GE.0)THEN
            IES=IES+1
            AAPAR(IES)=APAR(IPP)
            NB1=LEN_TRIM(AAPAR(IES))
            NB2=12-NB1
            IF(NB2.GT.1)THEN
              NB2=NB2/2
              IF(NB2.LT.1)NB2=1
              BBPAR(IES)=' '
              BBPAR(IES)(1+NB2:12)=AAPAR(IES)(1:NB1)
            ELSE
              BBPAR(IES)=AAPAR(IES)
            END IF
          END IF
3200    CONTINUE


        LDA=NESPAR

        IF(ISTKFLG.NE.0)THEN
          WRITE(IUNIT,1036,ERR=9000)
1036      FORMAT(/,/,1X,'Note that the matrices listed below ',
     +    'are likely to be somewhat in error due to',/,' the fact ',
     +    'that sensitivities for glued parameters have not been ',
     +    'calculated',/' since these parameters were glued to their ',
     +    'upper or lower bounds.')
        ELSE
          WRITE(IUNIT,*,ERR=9000)
        END IF

        IF((IREG.EQ.0).AND.(IPRED.EQ.0).AND.(NOBSNZERO.GT.0))THEN
          WRITE(IUNIT,4020,ERR=9000)
4020      FORMAT(/,' K-L information statistics ----->',/)
          LNS2=LOG(PSILO/NOBSNZERO)
          NLNS2=NOBSNZERO*LNS2
          KK=NESPAR+1
          AIC=NLNS2+2.0D0*KK
          IF(NOBSNZERO-KK-1.GT.0)THEN
            AICC=AIC+(2.0D0*KK*(KK+1))/(NOBSNZERO-KK-1)
          ELSE
            AICC=-1.0D36
          END IF
          BIC=NLNS2+KK*LOG(DBLE(NOBSNZERO))
          IF(DDET(1).GT.-1.0D36)THEN
            IF(DDET(1).GT.0.0D0)THEN
              KIC=(NOBSNZERO-(KK-1))*LNS2-(KK-1)*LOG(2.0D0*PI)+
     +        LOG(DDET(1)) + DDET(2) * LOG(10.0D0)
            ELSE
              KIC=-1.0D36
            END IF
          ELSE
            KIC=-1.0D36
          END IF
          WRITE(IUNIT,*)
          WRITE(IUNIT,4021) AIC
4021      FORMAT('   AIC   = ',1PG14.7)
          IF(AICC.GT.-1.0D35)THEN
            WRITE(IUNIT,4022) AICC
4022        FORMAT('   AICC  = ',1PG14.7)
          ELSE
            WRITE(IUNIT,4025)
4025        FORMAT('   AICC  = indeterminate')
          END IF
          WRITE(IUNIT,4023) BIC
4023      FORMAT('   BIC   = ',1PG14.7)
          IF(KIC.GT.-1.0D35)THEN
            WRITE(IUNIT,4024) KIC
4024        FORMAT('   KIC   = ',1PG14.7)
          ELSE
            WRITE(IUNIT,4026)
4026        FORMAT('   KIC   = indeterminate')
          END IF
       END IF

! -- The covariance matrix is recorded.

        DO 1035 I=1,NESPAR
        DO 1035 J=1,NESPAR
          NORMLO(J,I)=NORMLO(J,I)*SS
1035    CONTINUE

        WRITE(IUNIT,1040,ERR=9000)
1040    FORMAT(/,' Parameter covariance matrix ----->',/)
        IF(ICOV.EQ.0) THEN
          WRITE(IUNIT,1041,ERR=9000)
1041      FORMAT(' Not requested: ICOV in PEST control file set ',
     +    'to 0.')
          GO TO 1290
        END IF
        CALL WRITHED(IFAIL,IUNIT,8,NESPAR,BBPAR)
        IF(IFAIL.NE.0) GO TO 9000
        CALL WRITARR(IFAIL,LDA,IUNIT,8,NESPAR,NORMLO,AAPAR)
        IF(IFAIL.NE.0) GO TO 9000
1100    FORMAT(8(1X,1PG11.4,1X))
1200    CONTINUE

! -- If appropriate the JUPITER "_mv" file is recorded.

1290    CONTINUE
        IF((JUP_PAR_FILES.NE.0).AND.(OUTNAM.NE.' '))THEN
          JUPFILE=TRIM(OUTNAM)//'._mv'
          CALL FFOPEN(IFAIL,J_UNIT,'w',JUPFILE,6,CLINE)
          IF(IFAIL.NE.0) THEN
            JFAIL=1
            RETURN
          END IF
          ICOUNT=0
          DO I=1,NPAR
            IF(ITRANS(I).LT.0) CYCLE
            ICOUNT=ICOUNT+1
            IF(ICOUNT.EQ.501)THEN
              WRITE(J_UNIT,'(A)')' '
              ICOUNT=0
            END IF
            WRITE(J_UNIT,1202,ADVANCE='NO') APAR(I)
1202        FORMAT(1X,'"',A,'"')
          END DO
          WRITE(J_UNIT,'(A)') ' '
          DO I = 1, NESPAR
            WRITE(J_UNIT,1203) (NORMLO(J,I),J=1,NESPAR)
1203        FORMAT(500(1X,1PG14.7))
          END DO
          CLOSE(UNIT=J_UNIT)
        END IF

! -- The correlation coefficient matrix is recorded.

        IF(ICOR.EQ.0) THEN
          WRITE(IUNIT,1350,ERR=9000)
          WRITE(IUNIT,1351,ERR=9000)
1351      FORMAT(' Not requested: ICOR in PEST control file set ',
     +    'to 0.')
          IF((JUP_PAR_FILES.EQ.0).OR.(OUTNAM.EQ.' '))THEN
            GO TO 1550
          END IF
        END IF
C        IF(PSILO.LE.0.0D0)THEN
        IF(ICOR.NE.0)THEN
          IF(SS.LE.0.0D0)THEN
            WRITE(IUNIT,1350,ERR=9000)
            WRITE(IUNIT,1295,ERR=9000)
1295        FORMAT(' Correlation coefficient matrix indeterminate as ',
     +      'phi is zero.')
            GO TO 1550
          END IF
        END IF
        DO 1300 I=1,NESPAR
        RTEMP=SQRT(NORMLO(I,I))
        DO 1300 J=1,NESPAR
1300    NORM(J,I)=NORMLO(J,I)/RTEMP/SQRT(NORMLO(J,J))
        IF(ICOR.EQ.0) GO TO 1550
        WRITE(IUNIT,1350,ERR=9000)
1350    FORMAT(/,/,' Parameter correlation coefficient matrix ',
     +  '----->',/)
        CALL WRITHED(IFAIL,IUNIT,8,NESPAR,BBPAR)
        IF(IFAIL.NE.0) GO TO 9000
        CALL WRITARR(IFAIL,LDA,IUNIT,8,NESPAR,NORM,AAPAR)
        IF(IFAIL.NE.0) GO TO 9000
1370    CONTINUE

! -- If appropriate, the JUPITER "_mc" file is recorded.

        IF((JUP_PAR_FILES.NE.0).AND.(OUTNAM.NE.' '))THEN
          JUPFILE=TRIM(OUTNAM)//'._mc'
          CALL FFOPEN(IFAIL,J_UNIT,'w',JUPFILE,6,CLINE)
          IF(IFAIL.NE.0) THEN
            JFAIL=1
            RETURN
          END IF
          ICOUNT=0
          DO I=1,NPAR
            IF(ITRANS(I).LT.0) CYCLE
            ICOUNT=ICOUNT+1
            IF(ICOUNT.EQ.501)THEN
              WRITE(J_UNIT,'(A)')' '
              ICOUNT=0
            END IF
            WRITE(J_UNIT,1202,ADVANCE='NO') APAR(I)
          END DO
          WRITE(J_UNIT,'(A)') ' '
          DO I = 1, NESPAR
            WRITE(J_UNIT,1203) (NORM(J,I),J=1,NESPAR)
          END DO
          CLOSE(UNIT=J_UNIT)
        END IF

! -- Eigenvectors/eigenvalues of the covariance matrix are recorded.

1550    IF(IEIG.EQ.0) THEN
          WRITE(IUNIT,1580,ERR=9000)
          WRITE(IUNIT,1551,ERR=9000)
1551      FORMAT(' Not requested: IEIG in PEST control file set ',
     +    'to 0.')
          GO TO 5000
        END IF
        IF(SS.LE.0.0D0)THEN
          WRITE(IUNIT,1580,ERR=9000)
          WRITE(IUNIT,1555,ERR=9000)
1555      FORMAT(' Eigenvectors indeterminate as phi is zero.')
          GO TO 5000
        END IF
        CALL DGESVD('O','N',NSP4,NSP4,
     +  NORMLO,NSP4,W1,U,LDU,VT,LDVT,
     +  WORKVEC4,NDIM4,INFO)
1560    WRITE(IUNIT,1580,ERR=9000)
1580    FORMAT(/,/,' Normalized eigenvectors of parameter covariance ',
     +  'matrix ----->',/)
        IF(INFO.NE.0) THEN
          WRITE(IUNIT,1595,ERR=9000)
1595      FORMAT(' Eigenvector matrix cannot be computed.')
        ELSE
          CALL VECTOR_REVERSE(NSP4,W1)
          CALL MATRIX_REVERSE(NSP4,NSP4,NSP4,NORMLO)
          DO 1596 I=1,NESPAR
            WRITE(AANUM,'(I5)')I
            CALL SHIFTL(AANUM)
            BBPAR(I)=' Vector_'//TRIM(AANUM)
1596      CONTINUE
          CALL WRITHED(IFAIL,IUNIT,8,NESPAR,BBPAR)
          IF(IFAIL.NE.0) GO TO 9000
          CALL WRITARR(IFAIL,LDA,IUNIT,8,NESPAR,NORMLO,AAPAR)
          IF(IFAIL.NE.0) GO TO 9000
1600      CONTINUE
          WRITE(IUNIT,1620,ERR=9000)
1620      FORMAT(/,/,' Eigenvalues ----->',/)
          CALL WRITVEC(IFAIL,IUNIT,8,NESPAR,W1)
          IF(IFAIL.NE.0) GO TO 9000
        END IF
        IF(ISTKFLG.NE.0)THEN
          WRITE(IUNIT,1037,ERR=9000)
1037      FORMAT(/,1X,'Note that the matrices listed above ',
     +    'are likely to be somewhat in error due to',/,' the fact ',
     +    'that sensitivities for glued parameters have not been ',
     +    'calculated ',/,' since these parameters were glued to ',
     +    'their upper or lower bounds.')
        END IF

5000    RETURN

9000    CALL STPERR(26,0,' ',0,' ',CLINE)
        JFAIL=1
        RETURN
9100    CALL STPERR(133,0,' ',0,' ',CLINE)
        JFAIL=1
        RETURN
9200    CALL STPERR(144,0,' ',0,' ',CLINE)
        JFAIL=1
        RETURN
9300    WRITE(ERRMSG,9310) TRIM(JCOFILE)
9310    FORMAT('Error reading Jacobian matrix corresponding to ',
     +  'optimised parameters from file ',a,'.')
        JFAIL=2
        RETURN

#ifdef PESTMOD
        END SUBROUTINE WRTFIN
#else
        END
#endif


        SUBROUTINE WRITHED(IFAIL,IUNIT,NUM,NDIM,AHEAD)

        INTEGER NUM,NDIM,J1,J2,IUNIT,IFAIL,I,J
        CHARACTER*(*) AHEAD(NDIM)

        IFAIL=0
        J1=1
        J2=NUM

        IF(NDIM.GE.NUM)THEN
          DO 30 I=1,NDIM/NUM
            WRITE(IUNIT,20,ERR=9000) (AHEAD(J),J=J1,J2)
20          FORMAT(1X,13X,8(A12,1X))
            J1=J1+8
            J2=J2+8
30        CONTINUE
        END IF
        IF(NDIM.GE.J1)WRITE(IUNIT,20,ERR=9000) (AHEAD(J),J=J1,NDIM)
        IF(NDIM.GT.NUM) WRITE(IUNIT,*,ERR=9000)
        RETURN

9000    IFAIL=1
        RETURN

#ifdef PESTMOD
        END SUBROUTINE WRITHED
#else
        END
#endif


        SUBROUTINE WRITARR(IFAIL,LDA,IUNIT,NUM,NDIM,RARRAY,ANAME)

        INTEGER NUM,NDIM,J1,J2,IUNIT,IFAIL,I,J,II,LDA,IFLAG
        DOUBLE PRECISION RARRAY(LDA,NDIM)
        CHARACTER*(*) ANAME(NDIM)

        IFAIL=0

        DO 50 II=1,NDIM
          J1=1
          J2=NUM
          IFLAG=1
          IF(NDIM.GE.NUM)THEN
            DO 30 I=1,NDIM/NUM
              IF(IFLAG.EQ.1)THEN
                WRITE(IUNIT,20,ERR=9000)
     +          ANAME(II),(RARRAY(II,J),J=J1,J2)
20              FORMAT(1X,A12,1X,8(1X,1PG11.4,1X))
                IFLAG=0
              ELSE
                WRITE(IUNIT,21,ERR=9000) (RARRAY(II,J),J=J1,J2)
21              FORMAT(1X,12X,1X,8(1X,1PG11.4,1X))
              END IF
              J1=J1+8
              J2=J2+8
30          CONTINUE
          END IF
          IF(NDIM.GE.J1)THEN
            IF(IFLAG.EQ.1)THEN
              WRITE(IUNIT,20,ERR=9000)
     +        ANAME(II),(RARRAY(II,J),J=J1,NDIM)
              IFLAG=0
            ELSE
              WRITE(IUNIT,21,ERR=9000) (RARRAY(II,J),J=J1,NDIM)
            END IF
          END IF
          IF(NDIM.GT.NUM) WRITE(IUNIT,*,ERR=9000)
50      CONTINUE
        RETURN

9000    IFAIL=1
        RETURN

#ifdef PESTMOD
        END SUBROUTINE WRITARR
#else
        END
#endif

        SUBROUTINE WRITVEC(IFAIL,IUNIT,NUM,NDIM,VECTOR)

        INTEGER NUM,NDIM,J1,J2,IUNIT,IFAIL,I,J
        DOUBLE PRECISION VECTOR(NDIM)

        IFAIL=0

          J1=1
          J2=NUM
          IF(NDIM.GE.NUM)THEN
            DO 30 I=1,NDIM/NUM
              WRITE(IUNIT,21,ERR=9000) (VECTOR(J),J=J1,J2)
21            FORMAT(1X,12X,1X,8(1X,1PG11.4,1X))
              J1=J1+8
              J2=J2+8
30          CONTINUE
          END IF
          IF(NDIM.GE.J1)THEN
              WRITE(IUNIT,21,ERR=9000) (VECTOR(J),J=J1,NDIM)
          END IF
          RETURN

9000    IFAIL=1
        RETURN

#ifdef PESTMOD
        END SUBROUTINE WRITVEC
#else
        END
#endif



        SUBROUTINE WRTMAT(JFAIL,IUNIT,NPAR,NOBS,NXROW,NESPAR,NPRIOR,
     +  NPARGP,NWDIM,IOPT,ITRANS,PVAL,W1,W2,OVAL,REFOBS,OWGHT,NORM,
     +  NORMLO,AGROUP,IPARGP,APAR,ICOV,ICOR,IEIG,CLINE,NOBSGP,NOBGNM,
     +  IPRED,IREG,AAPAR,BBPAR,ISTKFLG,IRGP,MEMSAV,RES_PRED,NOPTMAX,
     +  NDIM4,WORKVEC4)

        USE PESTDATA,  ONLY: PREDNOISE,SVDMODE,LSQRMODE
#ifdef INTEL
        USE COMMON_MOD, ONLY: FLENME,CASEFL
#endif
        IMPLICIT NONE

        INTEGER NPAR,NOBS,NXROW,NESPAR,NPRIOR,IOPT,IES,JES,I,J,NT,
     +  ICOV,ICOR,IEIG,NOBSGP,IOBS,IERR,NOPTMAX,
     +  IPRED,NWDIM,NB1,NB2,IFAIL,LDA,NPARGP,JFAIL
        INTEGER IUNIT,NSP4,INFO,IREG,NNZERO,ISTKFLG,MEMSAV
        INTEGER NDIM4,LDU,LDVT
        INTEGER ITRANS(NPAR),NOBGNM(NXROW),IPARGP(NPAR),IRGP(NOBSGP)
        DOUBLE PRECISION RTEMP,SS,PSIKP,RTEMP1,SUM1,RES_PRED
        DOUBLE PRECISION PVAL(NPAR),W1(NWDIM),W2(NWDIM),OVAL(NXROW),
     +  REFOBS(NXROW),OWGHT(NXROW),NORM(NESPAR,NESPAR),
     +  NORMLO(NESPAR,NESPAR)
        DOUBLE PRECISION WORKVEC4(NDIM4)
        DOUBLE PRECISION U(1,1),VT(1,1)
        CHARACTER*5 AANUM,ATEMP*12
        CHARACTER*(*) APAR(NPAR),AAPAR(NPAR),BBPAR(NPAR),
     +  AGROUP(NPARGP)

#ifndef INTEL
        CHARACTER*100 CASEFL
        CHARACTER*200 FLENME
#endif
        CHARACTER*(*) CLINE

#ifndef INTEL
        COMMON /FLENME/FLENME
        COMMON /CASE/CASEFL
#endif

        JFAIL=0
        LDU=1
        LDVT=1

        FLENME=TRIM(CASEFL)//'.mtt'
        CALL FFOPEN(JFAIL,IUNIT,'w',' ',6,CLINE)
        IF(JFAIL.NE.0)THEN
          JFAIL=1
          RETURN
        END IF
        WRITE(IUNIT,10,ERR=9000) TRIM(CASEFL)
10      FORMAT(' PARAMETER STATISTICAL MATRICES: CASE ',A)
        WRITE(IUNIT,20,ERR=9000) IOPT
20      FORMAT(/,' OPTIMISATION ITERATION NUMBER',I3)

        IF(MEMSAV.EQ.1)THEN
          WRITE(IUNIT,23,ERR=9000)
23        FORMAT(/,' Parameter statistical matrices cannot be ',
     +    'calculated as memory conservation',/,' is being ',
     +    'implemented on this run.')
          GO TO 9900
        END IF
        IF((IREG.NE.0).OR.(SVDMODE.NE.0).OR.(LSQRMODE.NE.0))THEN
          WRITE(IUNIT,24,ERR=9000)
24        FORMAT(/,' Parameter statistical matrices are not ',
     +    'calculated as some form of ',/,' regularisation ',
     +    'is being implemented on this run.')
          IF((SVDMODE.EQ.0).AND.(LSQRMODE.EQ.0))THEN
            WRITE(IUNIT,25,ERR=9000)
25          FORMAT(/,' See the condition number (".CND") file ',
     +      'for problem condition numbers.')
          END IF
          WRITE(IUNIT,7411)
7411      FORMAT(/,' Use the PREDUNC7 utility to obtain a ',
     +    'full posterior covariance matrix.')
          GO TO 9900
        END IF


        IF((ICOV.EQ.0).AND.(ICOR.EQ.0).AND.(IEIG.EQ.0))THEN
          WRITE(IUNIT,1040,ERR=9000)
          IF(NOPTMAX.EQ.-2)THEN
            WRITE(IUNIT,33,ERR=9000)
33          FORMAT(' Not requested: NOPTMAX in PEST control file ',
     +      'set to -2.')
          ELSE
            WRITE(IUNIT,30,ERR=9000)
30          FORMAT(' Not requested: ICOV in PEST control file set ',
     +      'to 0.')
          END IF
          WRITE(IUNIT,1350,ERR=9000)
          IF(NOPTMAX.EQ.-2)THEN
            WRITE(IUNIT,33,ERR=9000)
          ELSE
            WRITE(IUNIT,31,ERR=9000)
31          FORMAT(' Not requested: ICOR in PEST control file set ',
     +      'to 0.')
          END IF
          WRITE(IUNIT,1580,ERR=9000)
          IF(NOPTMAX.EQ.-2)THEN
            WRITE(IUNIT,33,ERR=9000)
          ELSE
            WRITE(IUNIT,32,ERR=9000)
32          FORMAT(' Not requested: IEIG in PEST control file set ',
     +      'to 0.')
          END IF
          GO TO 9900
        END IF

        NNZERO=0
        DO 51 IOBS=1,NXROW
          IF(OWGHT(IOBS).GT.0.0D0)NNZERO=NNZERO+1
51      CONTINUE
        NT=NNZERO-NESPAR

        IF(NT.LE.0)THEN
          WRITE(IUNIT,53,ERR=9000)
53        FORMAT(/,' Cannot evaluate parameter covariance matrix ',
     +    'as the number of observations',/,' and prior ',
     +    'information items does not exceed the number of ',
     +    'adjustable ',/,' parameters.')
          GO TO 9900
        END IF

        IF(ISTKFLG.NE.0)THEN
          WRITE(IUNIT,3012)
3012      FORMAT(/,1X,'Note that the statistics listed in this file ',
     +    'are likely to be somewhat in ',/,' error due to the fact ',
     +    'that sensitivities for glued parameters have not been ',
     +    /,' calculated since they were glued to their ',
     +    'respective bounds.')
        END IF

        NSP4=NESPAR
        CALL DPOFA(NORMLO,NSP4,NSP4,INFO)
        IF(INFO.NE.0) THEN
          WRITE(IUNIT,60,ERR=9000)
60        FORMAT(/,' Cannot evaluate parameter covariance matrix ',
     +    'as the "normal matrix"',/,' for the ',
     +    'current parameter estimation problem is singular.')
          GO TO 9900
        END IF

        INFO=1
        CALL DPODI(NORMLO,NSP4,NSP4,W1,INFO)
        DO 250 IES=1,NESPAR-1
        DO 250 JES=IES+1,NESPAR
250     NORMLO(JES,IES)=NORMLO(IES,JES)


        IF(ICOV.NE.0)THEN
          WRITE(IUNIT,260,ERR=9000)
260       FORMAT(/,/,' Parameter standard deviations ----->',/)
          WRITE(IUNIT,270,ERR=9000)
270       FORMAT(' Adjustable',T20,'Group',T35,' Current',
     +    T52,' Standard')
          WRITE(IUNIT,271,ERR=9000)
271       FORMAT(' parameter',T35,' value',T52,' deviation')
        END IF

        CALL OBJCLC(1,PSIKP,NXROW,NPRIOR,NOBS,REFOBS,OVAL,
     +  OWGHT,NOBGNM,NOBSGP,IRGP,SUM1)
        IF(PREDNOISE.GT.0) PSIKP=PSIKP+RES_PRED
        SS=PSIKP/NT
        IF(SS.LE.0.0D0) SS=1.0D-20
        IES=0
        DO 400 I=1,NPAR
          IF(ITRANS(I).LT.0) THEN
            GO TO 400
          END IF
          IES=IES+1
          IF(NORMLO(IES,IES).LE.0.0D0) THEN
            WRITE(IUNIT,60,ERR=9000)
            GO TO 9900
          END IF
          AAPAR(IES)=APAR(I)
          NB1=LEN_TRIM(AAPAR(IES))
          NB2=12-NB1
          IF(NB2.GT.1)THEN
            NB2=NB2/2
            IF(NB2.LT.1)NB2=1
            BBPAR(IES)=' '
            BBPAR(IES)(1+NB2:12)=AAPAR(IES)(1:NB1)
          ELSE
            BBPAR(IES)=AAPAR(IES)
          END IF
          IF(ICOV.NE.0)THEN
            RTEMP=SQRT(NORMLO(IES,IES)*SS)
            ATEMP=AGROUP(IPARGP(I))
            WRITE(IUNIT,280,ERR=9000) TRIM(AAPAR(IES)),
     +      TRIM(ATEMP),PVAL(I),RTEMP
280         FORMAT(1X,A,T20,A,T35,1PG13.6,T52,1PG13.6)
          END IF
400     CONTINUE

        IF(ICOV.NE.0)THEN
        WRITE(IUNIT,290,ERR=9000)
290     FORMAT(/,' Note that if a parameter is log-transformed, the ',
     +  'standard deviation in the ',/,' above table refers to the ',
     +  'log of the parameter. Note also that the objective',
     +  /,' function used in the calculation of the covariance ',
     +  'matrix (and eigenvalues',/,' of the covariance matrix) ',
     +  'was calculated at the end of the previous iteration.')
        END IF

        LDA=NESPAR

        DO 1035 I=1,NESPAR
        DO 1035 J=1,NESPAR
          NORMLO(J,I)=NORMLO(J,I)*SS
1035    CONTINUE
        WRITE(IUNIT,1040,ERR=9000)
1040    FORMAT(/,/,' Parameter covariance matrix ----->',/)
        IF(ICOV.EQ.0)THEN
          WRITE(IUNIT,30,ERR=9000)
          GO TO 1290
        END IF
        CALL WRITHED(IFAIL,IUNIT,8,NESPAR,BBPAR)
        IF(IFAIL.NE.0) GO TO 9000
        CALL WRITARR(IFAIL,LDA,IUNIT,8,NESPAR,NORMLO,AAPAR)
        IF(IFAIL.NE.0) GO TO 9000
1200    CONTINUE

1290    CONTINUE
        DO 1300 I=1,NESPAR
        RTEMP=SQRT(NORMLO(I,I))
        DO 1300 J=1,NESPAR
1300    NORM(J,I)=NORMLO(J,I)/RTEMP/SQRT(NORMLO(J,J))
        WRITE(IUNIT,1350,ERR=9000)
1350    FORMAT(/,/,' Parameter correlation coefficient ',
     +  'matrix ----->',/)
        IF(ICOR.EQ.0)THEN
          WRITE(IUNIT,31,ERR=9000)
          GO TO 1550
        END IF
        CALL WRITHED(IFAIL,IUNIT,8,NESPAR,BBPAR)
        IF(IFAIL.NE.0) GO TO 9000
        CALL WRITARR(IFAIL,LDA,IUNIT,8,NESPAR,NORM,AAPAR)
        IF(IFAIL.NE.0) GO TO 9000
1370    CONTINUE

1550    CONTINUE
        IF(IEIG.NE.0)THEN
          CALL DGESVD('O','N',NSP4,NSP4,
     +     NORMLO,NSP4,W1,U,LDU,VT,LDVT,
     +     WORKVEC4,NDIM4,INFO)
        END IF
1560    WRITE(IUNIT,1580,ERR=9000)
1580    FORMAT(/,/,' Normalized eigenvectors of covariance ',
     +  'matrix ----->',/)
        IF(IEIG.EQ.0)THEN
          WRITE(IUNIT,32,ERR=9000)
          GO TO 5000
        END IF
        IF(INFO.NE.0) THEN
          WRITE(IUNIT,1595,ERR=9000)
1595      FORMAT(' Eigenvector matrix cannot be computed.')
        ELSE
          CALL VECTOR_REVERSE(NSP4,W1)
          CALL MATRIX_REVERSE(NSP4,NSP4,NSP4,NORMLO)
          DO 1596 I=1,NESPAR
            WRITE(AANUM,'(I5)')I
            CALL SHIFTL(AANUM)
            BBPAR(I)=' Vector_'//TRIM(AANUM)
1596      CONTINUE
          CALL WRITHED(IFAIL,IUNIT,8,NESPAR,BBPAR)
          IF(IFAIL.NE.0) GO TO 9000
          CALL WRITARR(IFAIL,LDA,IUNIT,8,NESPAR,NORMLO,AAPAR)
          IF(IFAIL.NE.0) GO TO 9000
1600      CONTINUE
          WRITE(IUNIT,1620,ERR=9000)
1620      FORMAT(/,/,' Eigenvalues ----->',/)
          CALL WRITVEC(IFAIL,IUNIT,8,NESPAR,W1)
          IF(IFAIL.NE.0) GO TO 9000
        END IF

        IF(ISTKFLG.NE.0)THEN
          WRITE(IUNIT,3012)
        END IF

5000    GO TO 9900

9000    CALL STPERR(26,0,' ',0,' ',CLINE)
        JFAIL=1

9900    CLOSE(UNIT=IUNIT,IOSTAT=IERR)

        RETURN

#ifdef PESTMOD
        END SUBROUTINE WRTMAT
#else
        END
#endif




        SUBROUTINE VECTOR_REVERSE(N,VEC)

        IMPLICIT NONE

        INTEGER N,I,N2,J
        DOUBLE PRECISION RTEMP
        DOUBLE PRECISION VEC(N)

        N2=N/2
        DO I=1,N2
          J=N-(I-1)
          RTEMP=VEC(I)
          VEC(I)=VEC(J)
          VEC(J)=RTEMP
        END DO

        RETURN

        END



        SUBROUTINE MATRIX_REVERSE(LDA,M,N,A)

        IMPLICIT NONE

        INTEGER LDA,N,M
        DOUBLE PRECISION A(LDA,N)

        INTEGER N2,I,J,K
        DOUBLE PRECISION RTEMP

        N2=N/2
        DO I=1,N/2
          J=N-(I-1)
          DO K=1,M
            RTEMP=A(K,I)
            A(K,I)=A(K,J)
            A(K,J)=RTEMP
          END DO
        END DO

        RETURN

        END

