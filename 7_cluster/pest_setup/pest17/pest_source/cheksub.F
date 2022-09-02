        SUBROUTINE PRRCHK(IFAIL,NPRWRN,NESPAR,NESPR1,NPRIOR,NPAR,
     +  OBGDIM,OBSDIM,OBNDIM,NOBSGP,NOBSGP1,NOBS,ILINE,ALRADY,MAXWID,
     +  ITRANS,X,Y,APRIOR,AOBSGP,AOBS,APAR,CLINE,AMXWID,ATEMP,FMT,
     +  PZROWT,IOBGNM,IW_PR,NOMOREIW_CHECK,NXROW,OWGHT,QUICKPRIOR,
     +  IPPCOUNT,NPARSEC,NPARFILE)

C -- SUBROUTINE PRRCHK READS AND CHECKS THE PRIOR INFORMATION SECTION
C -- OF THE PEST CONTROL FILE

        INTEGER NBLNK
        INTEGER NESPAR,NESPR1,NPRIOR,IERR,MCASE,MMLINE,NUMERR,
     +  NEWSTR,IPRIOR,IPAR,JMATH,JMUL,JOBS,JWGHT,JNAME,JX,NEG,
     +  JCONT,JLOG,ALRADY,I,N2,NBLC,MAXWID,N1,N3,N4,NPAR,IES,IFAIL,
     +  JSTART,PZROWT,OBGDIM,NOBSGP,NOBSGP1,NPRWRN,NOBS,OBSDIM,JGROUP,
     +  OBNDIM,IJ
        INTEGER NPARSEC,NPARFILE
        INTEGER QUICKPRIOR,NUMIND,IIPAR,IIOBS,ITEMP,ERRFLAG,IC
        INTEGER LW(10),RW(10)
        INTEGER IW_FLAG,IW_SIGN,IW_PAR,ICOUNT,J,K,JJ,NOMOREIW_CHECK
        INTEGER ITRANS(NPAR)
        INTEGER ILINE,IFAIL4
        INTEGER NXROW
        INTEGER IPPCOUNT
        INTEGER IOBGNM(OBNDIM),IW_PR(NPRIOR)
        DOUBLE PRECISION RTEMP,RRTEMP
        DOUBLE PRECISION X(NPRIOR,NESPR1),Y(NESPR1)
        DOUBLE PRECISION OWGHT(NXROW)
        CHARACTER*(*) CLINE,AMXWID,ATEMP,FMT
        CHARACTER*(*) APRIOR(NPRIOR),AOBSGP(OBGDIM),
     +  AOBS(OBSDIM)
        CHARACTER*(*) APAR(NPAR+NPARFILE+NPARSEC)
        CHARACTER*10 ERRMSG*2120,CASFL*100,ALINE*20,AALINE*200,TPAR*12,
     +  PRRNAM*50
        CHARACTER*12 AGPNME

        COMMON /IERR/IERR,MCASE
        COMMON /ERRMSG/ ERRMSG
        COMMON /CASFL/ CASFL
        COMMON /ALINE/ ALINE,AALINE
        COMMON /MMLINE/ MMLINE

        IFAIL=0
        NPRWRN=0
        IPAR=0
        DO I=1,NPRIOR
          IW_PR(I)=0
        END DO

        IW_FLAG=0
        IF(NOMOREIW_CHECK.NE.0)IW_FLAG=-1
        IF(ALRADY.EQ.1) GO TO 3064

7777    CONTINUE
        ILINE=ILINE+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7777
        CALL LOWCAS(CLINE)
        CALL SHIFTL(CLINE)
        IF(CLINE.NE.'* prior information') THEN
          CALL WRTINT(ATEMP,ILINE)
            WRITE(6,3060,ERR=9900) ATEMP(:NBLNK(ATEMP)),
     +      CASFL(1:MCASE)
3060       FORMAT(' Line ',A,' of file ',A,' should read:',
     +    /,' * prior information')
          GO TO 9800
        END IF
3064    NUMERR=IERR

        IF(QUICKPRIOR.EQ.1)THEN
          X=0.0D0                  ! AN ARRAY
          DO IPRIOR=1,NPRIOR
7778        CONTINUE
            ILINE=ILINE+1
            CALL WRTINT(ATEMP,ILINE)
            ALINE=' Line '//TRIM(ATEMP)//' of file '
            AALINE=TRIM(ALINE)//' '//CASFL(1:MCASE)//': '
            MMLINE=LEN_TRIM(AALINE)+1
            READ(30,'(A)',ERR=9600,END=9700) CLINE
            CALL PPROC(CLINE,IPPCOUNT)
            IF(CLINE.EQ.' ') GO TO 7778
            CALL LOWCAS(CLINE)
            CLINE=ADJUSTL(CLINE)
            IF(INDEX(CLINE,'&').NE.0)THEN
              WRITE(ERRMSG,5010) AALINE(1:MMLINE)
5010          FORMAT(A,'continuation character "&" not allowed ',
     +        'in line supplying indexed prior information.')
              CALL WRTERR
              GO TO 9900
            END IF
            CALL LINSPL(IFAIL,4,LW,RW,CLINE)
            IF(IFAIL.NE.0)THEN
              WRITE(ERRMSG,5020) AALINE(1:MMLINE)
5020          FORMAT(A,'4 entries expected on each line of first ',
     +        'part of "prior information" section when prior ',
     +        'information is supplied in indexed form.')
              CALL WRTERR
              CYCLE
            END IF
            CALL LINSPL(IFAIL,5,LW,RW,CLINE)
            IF(IFAIL.EQ.0)THEN
              WRITE(ERRMSG,5020) AALINE(1:MMLINE)
              CALL WRTERR
              CYCLE
            END IF

            IF(RW(1)-LW(1)+1.GT.20)THEN
              CALL ILLPRR(CLINE(LW(1):RW(1)))
              WRITE(ERRMSG,3100)
              CALL WRTERR
            END IF
            APRIOR(IPRIOR)=CLINE(LW(1):RW(1))
            IF(IPRIOR.GT.1) THEN
              DO I=1,IPRIOR-1
                IF(APRIOR(I).EQ.APRIOR(IPRIOR))THEN
                  WRITE(ERRMSG,3106)AALINE(1:MMLINE),
     +            TRIM(APRIOR(IPRIOR))
                  CALL WRTERR
                  GO TO 5030
                END IF
              END DO
            ENDIF
5030        CONTINUE
            IF(NOBS.GT.0)THEN
              DO I=1,NOBS
                IF(AOBS(I).EQ.APRIOR(IPRIOR))THEN
                  WRITE(ERRMSG,3102) AALINE(1:MMLINE),
     +            TRIM(APRIOR(IPRIOR))
                  CALL WRTERR
                  GO TO 5040
                END IF
              END DO
            END IF
5040        CONTINUE

            CALL NUMRD(IFAIL,1,LW(2),RW(2),ITEMP,X(IPRIOR,NESPR1),CLINE)
            IF(IFAIL.NE.0)THEN
              CALL ILLPRR(CLINE(LW(2):RW(2)))
              WRITE(ERRMSG,5050)
5050          FORMAT('   (Cannot read prior information value.)')
              CALL WRTERR
            END IF

            RTEMP=0.0D0
            CALL NUMRD(IFAIL,1,LW(3),RW(3),ITEMP,RTEMP,CLINE)
            IF(IFAIL.NE.0)THEN
              CALL ILLPRR(CLINE(LW(3):RW(3)))
              WRITE(ERRMSG,5060)
5060          FORMAT('   (Cannot read prior information weight.)')
             CALL WRTERR
            ELSE
              IF(RTEMP.LT.0.0D0) THEN
                CALL ILLPRR(CLINE(LW(3):RW(3)))
                WRITE(ERRMSG,5070)
5070            FORMAT('   (Prior information weight must be zero or ',
     +          'positive.)')
                CALL WRTERR
              END IF
            END IF
            OWGHT(IPRIOR+NOBS)=RTEMP

            IF(RW(4)-LW(4)+1.GT.12)THEN
              CALL ILLPRR(CLINE(LW(4):RW(4)))
              WRITE(ERRMSG,5080)
5080          FORMAT('   (Observation group name greater than 12 ',
     +        'characters long.)')
              CALL WRTERR
            ELSE
              AGPNME=CLINE(LW(4):RW(4))
              DO I=1,NOBSGP
                IF(AOBSGP(I).EQ.AGPNME) THEN
                  IOBGNM(IPRIOR+NOBS)=I
                  IF(AGPNME.EQ.'predict')THEN
                    WRITE(ERRMSG,3192)AALINE(1:MMLINE)
                    CALL WRTERR
                  END IF
                  GO TO 5095
                END IF
              END DO
              CALL ILLPRR(CLINE(LW(4):RW(4)))
              WRITE(ERRMSG,5090)
5090          FORMAT('   (Unrecognised observation group name.)')
              CALL WRTERR
              IOBGNM(IPRIOR+NOBS)=-1
5095          CONTINUE
            END IF
          END DO
          IF(IERR.NE.0) THEN
            IFAIL=4
            GO TO 9999
          END IF

7779      CONTINUE
          ILINE=ILINE+1
          CALL WRTINT(ATEMP,ILINE)
          ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
          AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
          MMLINE=NBLNK(AALINE)+1
          READ(30,'(A)',ERR=9600,END=52501) CLINE
          CALL PPROC(CLINE,IPPCOUNT)
          IF(CLINE.EQ.' ') GO TO 7779
          GO TO 52801
52501     CONTINUE
          WRITE(ERRMSG,5260)
          CALL WRTERR
          IFAIL=4
          GO TO 9999
52801     CONTINUE
          CALL LINSPL(IFAIL,1,LW,RW,CLINE)
          IF(IFAIL.NE.0)THEN
            WRITE(ERRMSG,5220) AALINE(1:MMLINE)
5220        FORMAT(A,'blank line found where single integer expected.')
            CALL WRTERR
            IFAIL=4
            GO TO 9999
          END IF
          CALL NUMRD(IFAIL,0,LW(1),RW(1),NUMIND,RTEMP,CLINE)
          IF(IFAIL.NE.0)THEN
            WRITE(ERRMSG,5230) AALINE(1:MMLINE)
5230        FORMAT(A,'cannot read integer value of number of ',
     +      'lines of indexed prior information data to follow.')
            CALL WRTERR
            IFAIL=4
            GO TO 9999
          END IF
          IF(NUMIND.LE.0)THEN
            WRITE(ERRMSG,5240) AALINE(1:MMLINE)
5240        FORMAT(A,'number indicating number of following lines ',
     +      'of indexed prior information data must not be zero or ',
     +      'negative.')
            CALL WRTERR
            IFAIL=4
            GO TO 9999
          END IF
          DO I =1,NUMIND
7780        CONTINUE
            ILINE=ILINE+1
            CALL WRTINT(ATEMP,ILINE)
            ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
            AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
            MMLINE=NBLNK(AALINE)+1
            READ(30,'(A)',ERR=9600,END=5250) CLINE
            CALL PPROC(CLINE,IPPCOUNT)
            IF(CLINE.EQ.' ') GO TO 7780
            GO TO 5280
5250        CONTINUE
            WRITE(ERRMSG,5260)
5260        FORMAT(' End of file encountered while reading ',
     +      'indexed prior information.')
            CALL WRTERR
            IFAIL=4
            GO TO 9999
5280        CONTINUE
            CALL LINSPL(IFAIL,3,LW,RW,CLINE)
            IF(IFAIL.NE.0)THEN
              WRITE(ERRMSG,5290) AALINE(1:MMLINE)
5290          FORMAT(A,'three entries expected for indexed prior ',
     +        'information.')
              CALL WRTERR
            ELSE
              ERRFLAG=0
              CALL NUMRD(IFAIL,0,LW(1),RW(1),IIPAR,RTEMP,CLINE)
              IF(IFAIL.NE.0)THEN
                WRITE(ERRMSG,5300) AALINE(1:MMLINE)
5300            FORMAT(A,'cannot read parameter index.')
                CALL WRTERR
                ERRFLAG=1
              ELSE
                IF((IIPAR.LT.1).OR.(IIPAR.GT.NPAR))THEN
                  WRITE(ERRMSG,5310) AALINE(1:MMLINE)
5310              FORMAT(A,'parameter index must be greater than ',
     +            'zero and less than or equal to NPAR.')
                  CALL WRTERR
                  ERRFLAG=1
                END IF
              END IF
              CALL NUMRD(IFAIL,0,LW(2),RW(2),IIOBS,RTEMP,CLINE)
              IF(IFAIL.NE.0)THEN
                WRITE(ERRMSG,5320) AALINE(1:MMLINE)
5320            FORMAT(A,'cannot read Jacobian row index.')
                CALL WRTERR
                ERRFLAG=1
              ELSE
                IF((IIOBS.LE.NOBS).OR.(IIOBS.GT.NXROW))THEN
                  WRITE(ERRMSG,5330) AALINE(1:MMLINE)
5330              FORMAT(A,'Jacobian row index must be greater ',
     +            'than NOBS and less than or equal to NOBS+NPRIOR.')
                  CALL WRTERR
                  ERRFLAG=1
                END IF
              END IF
              CALL NUMRD(IFAIL,1,LW(3),RW(3),ITEMP,RTEMP,CLINE)
              IF(IFAIL.NE.0)THEN
                WRITE(ERRMSG,5340) AALINE(1:MMLINE)
5340            FORMAT(A,'cannot read Jacobian value - third ',
     +          'item on line.')
                CALL WRTERR
                ERRFLAG=1
              ELSE
                IF(ERRFLAG.EQ.0) X(IIOBS-NOBS,IIPAR)=RTEMP
              END IF
            END IF
          END DO
          IF(IERR.EQ.NUMERR)THEN
            ICOUNT=0
            DO IPRIOR=1,NPRIOR
              DO IES=1,NESPAR
                IF(X(IPRIOR,IES).NE.0.0D0) GO TO 5350
              END DO
              ICOUNT=1
              GO TO 5351
5350          CONTINUE
            END DO
5351        CONTINUE
            IF(ICOUNT.NE.0)THEN
              WRITE(ERRMSG,5360) CASFL(1:MCASE)
5360          FORMAT(' Some items of prior information cited ',
     +        'in the "prior information" section of file ',A,
     +        ' are assigned no sensitivities. A list of empty prior ',
     +        'information equation names follows:-')
              CALL WRTERR
              ICOUNT=0
              IC=0
              CLINE=' '
              DO IPRIOR=1,NPRIOR
                DO IES=1,NESPAR
                  IF(X(IPRIOR,IES).NE.0.0D0) GO TO 5370
                END DO
                ICOUNT=ICOUNT+1
                IC=IC+1
                WRITE(CLINE((IC-1)*20+1:),'(A)')
     +          TRIM(APRIOR(IPRIOR))
                IF((ICOUNT.NE.0).AND.((ICOUNT/4)*4.EQ.ICOUNT))THEN
                  WRITE(6,'(A)') TRIM(CLINE)
                  CLINE=' '
                  IC=0
                  IF(ICOUNT.EQ.28)THEN
                    WRITE(6,5365)
5365                FORMAT('etc')
                    GO TO 5375
                  END IF
                END IF
5370            CONTINUE
              END DO
              IF(CLINE.NE.' ')WRITE(6,'(A)') TRIM(CLINE)
5375          CONTINUE
            END IF
          END IF

          GO TO 9999
        END IF

        NEWSTR=0
        IPRIOR=0
3066    IPAR=1
        IW_SIGN=0
        IW_PAR=0
        JMATH=0
        JMUL=0
        JOBS=0
        JWGHT=0
        JNAME=0
        JX=0
        NEG=0
        JCONT=0
        JGROUP=0
3065    IPRIOR=IPRIOR+1
        IF(IPRIOR.GT.NPRIOR) GO TO 3202
        APRIOR(IPRIOR)=' '
        DO 3067 I=1,NESPAR+1
3067    X(IPRIOR,I)=0.0D0
        JSTART=1
3070    ILINE=ILINE+1
        N2=0
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=3075) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 3070
        GO TO 3078
3075    IF(NEWSTR.EQ.1) THEN
          WRITE(ERRMSG,3074) CASFL(1:MCASE)
3074      FORMAT(' Unexpected end to file ',A,
     +    ': looking for beginning ',
     +    'of next prior information line.')
          CALL WRTERR
          GO TO 9800
        END IF
        IF(JCONT.EQ.0)THEN
          WRITE(ERRMSG,3072)CASFL(1:MCASE)
3072      FORMAT(' Unexpected end to file ',A,': cannot find all ',
     +    'prior information.')
          CALL WRTERR
          GO TO 9800
        END IF
        WRITE(ERRMSG,3076) CASFL(1:MCASE)
3076    FORMAT(' Unexpected end to file ',A,': continuation of ',
     +  'previous prior information line expected.')
        CALL WRTERR
        GO TO 9800
3078    NBLC=NBLNK(CLINE)
        IF(NBLC.GT.MAXWID)THEN
          WRITE(ERRMSG,3080)AALINE(1:MMLINE),
     +    AMXWID(:NBLNK(AMXWID))
3080      FORMAT(A,'line exceeds ',A,' characters.')
          CALL WRTERR
          IF(NEWSTR.EQ.1) THEN
            CALL SHIFTL(CLINE)
            CALL LOWCAS(CLINE)
            IF(CLINE(1:1).EQ.'&')IPRIOR=IPRIOR-1
          END IF
          GO TO 3900
        END IF
        IF(CLINE.EQ.' ') GO TO 3070
        CALL LOWCAS(CLINE)
        CALL SHIFTL(CLINE)
        IF(CLINE(1:21).EQ.'* predictive analysis')THEN
           WRITE(ERRMSG,3083) AALINE(1:MMLINE)
3083       FORMAT(A,'insufficient number of prior information ',
     +     'equations.')
           CALL WRTERR
           GO TO 9500
        END IF
        IF(JCONT.EQ.1) THEN
          IF(CLINE(1:1).NE.'&')THEN
            WRITE(ERRMSG,3085) AALINE(1:MMLINE)
3085        FORMAT(A,'previous prior information line incomplete ',
     +      '- continuation character ("&") expected.')
            CALL WRTERR
            NEWSTR=1
            IPRIOR=IPRIOR+1
            JCONT=0
          END IF
        END IF

        IF(NEWSTR.EQ.1)THEN
          IF(CLINE(1:1).NE.'&')THEN
            JMATH=0
            JMUL=0
            JOBS=0
            JWGHT=0
            JNAME=0
            JX=0
            NEG=0
            JSTART=1
            JCONT=0
            NEWSTR=0
            IF(IPRIOR.GT.NPRIOR) GO TO 3203
            APRIOR(IPRIOR)=' '
            DO 3086 I=1,NESPAR+1
3086        X(IPRIOR,I)=0.0D0
          ELSE
            GO TO 3070
          END IF
        END IF

3090    CALL GETINT(IFAIL4,CLINE,N1,N2,NBLC,' ')
        IF(N1.EQ.0) THEN
          JCONT=1
          GO TO 3070
        END IF
        IF(CLINE(N1:N1).EQ.'&')THEN
          IF(N1.NE.N2)THEN
            CALL ILLPRR(CLINE(N1:N2))
            GO TO 3900
          ENDIF
          IF(N1.NE.1)THEN
            CALL ILLPRR(CLINE(N1:N2))
            WRITE(ERRMSG,3092)
3092        FORMAT('  (Continuation character ("&") must be first ',
     +      'character of line.)')
            CALL WRTERR
            GO TO 3900
          ENDIF
          IF(JCONT.EQ.0) THEN
            WRITE(ERRMSG,3093) AALINE(1:MMLINE)
3093        FORMAT(A,'unexpected continuation character ("&") - ',
     +      'previous line complete.')
            CALL WRTERR
            GO TO 3070
          ENDIF
          JCONT=0
          GO TO 3090
        ELSE IF(JSTART.EQ.1)THEN
          IF(N2-N1+1.GT.20)THEN
            CALL ILLPRR(CLINE(N1:N2))
            WRITE(ERRMSG,3100)
3100         FORMAT('   (Prior information label greater ',
     +      'than 20 characters long.)')
            CALL WRTERR
          END IF
          IF(IPRIOR.GT.NPRIOR) GO TO 3203
          APRIOR(IPRIOR)=CLINE(N1:N2)
          PRRNAM=' - prior information article "'//
     +    APRIOR(IPRIOR)(:NBLNK(APRIOR(IPRIOR)))//'").'
          IF(IPRIOR.GT.1) THEN
            DO 3105 I=1,IPRIOR-1
            IF(APRIOR(I).EQ.APRIOR(IPRIOR))THEN
              WRITE(ERRMSG,3106)AALINE(1:MMLINE),
     +        APRIOR(IPRIOR)(:NBLNK(APRIOR(IPRIOR)))
3106          FORMAT(A,'prior information label "',A,'" used ',
     +        'more than once.')
              CALL WRTERR
              GO TO 3107
            END IF
3105        CONTINUE
3107        CONTINUE
          ENDIF
          IF(NOBS.GT.0)THEN
            DO 3101 I=1,NOBS
              IF(AOBS(I).EQ.APRIOR(IPRIOR))THEN
                WRITE(ERRMSG,3102) AALINE(1:MMLINE),
     +          APRIOR(IPRIOR)(:NBLNK(APRIOR(IPRIOR)))
3102            FORMAT(A,'Prior information label "',A,'" same ',
     +          'as an observation name.')
                CALL WRTERR
                GO TO 3103
              END IF
3101        CONTINUE
          END IF
3103      CONTINUE
          JSTART=0
          JX=1
          NEG=0
          GO TO 3090
        ELSE IF((CLINE(N1:N2).EQ.'=').AND.(N1.EQ.N2))THEN
          IF(JMATH.NE.1)THEN
            CALL ILLSGN('=')
            GO TO 3900
          END IF
          JMATH=0
          JOBS=1
          IF((IW_PAR.NE.0).AND.(IW_SIGN.NE.0))THEN
            WRITE(ERRMSG,3104) AALINE(1:MMLINE),TRIM(APRIOR(IPRIOR))
3104        FORMAT(A,'Prior information item "',a,'" cites an ',
     +      '"iw_" (adaptive regularisation) parameter; ',
     +      'thus it must be of the type "1.0 * log(iw_regul) = -3".')
            CALL WRTERR
            IW_FLAG=-1
          END IF
          GO TO 3090
        ELSE IF((CLINE(N1:N2).EQ.'+').AND.(N1.EQ.N2))THEN
          IF(JMATH.NE.1)THEN
            CALL ILLSGN('+')
            GO TO 3900
          END IF
          JMATH=0
          JX=1
          NEG=0
          IW_SIGN=1
          GO TO 3090
        ELSE IF((CLINE(N1:N2).EQ.'-').AND.(N1.EQ.N2))THEN
          IF(JMATH.NE.1) THEN
            CALL ILLSGN('-')
            GO TO 3900
          END IF
          JMATH=0
          JX=1
          NEG=1
          IW_SIGN=1
          GO TO 3090
        ELSE IF(CLINE(N1:N2).EQ.'*')THEN
          IF(JMUL.NE.1)THEN
            CALL ILLSGN('*')
            GO TO 3900
          END IF
          JNAME=1
          JMUL=0
          GO TO 3090
        ELSE IF(JX.EQ.1) THEN
          WRITE(FMT,3110) N2-N1+1
3110      FORMAT('(F',I3,'.0)')
          READ(CLINE(N1:N2),FMT,ERR=3120) RRTEMP
          GO TO 3150
3120      CALL ILLPRR(CLINE(N1:N2))
          WRITE(ERRMSG,3140)
3140      FORMAT('  (Cannot read parameter factor.)')
          CALL WRTERR
          GO TO 3900
3150      JX=0
          IF(NEG.EQ.1) RRTEMP=-RRTEMP
          NEG=0
          JMUL=1
          GO TO 3090
        ELSE IF(JNAME.EQ.1)THEN
          IF(CLINE(N1:N1+3).EQ.'log(')THEN
            IF((CLINE(N1+3:N1+3).NE.'(').OR.(CLINE(N2:N2).NE.')'))THEN
              CALL ILLPRR(CLINE(N1:N2))
              WRITE(ERRMSG,3160)
3160          FORMAT('  (Not in form "log(parameter_name)".)')
              CALL WRTERR
              GO TO 3800
            ELSE
              N3=N1+4
              N4=N2-1
              JLOG=1
            END IF
          ELSE
            N3=N1
            N4=N2
            JLOG=0
          END IF
          IF(N4-N3+1.GT.12) THEN
            CALL ILLPRR(CLINE(N1:N2))
            WRITE(ERRMSG,3170)
3170        FORMAT('  (Parameter name greater than 12 ',
     +      'characters long.)')
            CALL WRTERR
            GO TO 3800
          END IF
          IF(N4-N3.LT.0) THEN
            CALL ILLPRR(CLINE(N1:N2))
            WRITE(ERRMSG,3180)
3180        FORMAT('  (Blank parameter name.)')
            CALL WRTERR
            GO TO 3800
          END IF
          TPAR=CLINE(N3:N4)
          CALL WHICH1(IFAIL4,NPAR+NPARFILE+NPARSEC,IPAR,APAR,TPAR)
          IF(IFAIL4.NE.0)THEN
            CALL ILLPRR(CLINE(N1:N2))
            WRITE(ERRMSG,3190)
3190        FORMAT('  (Unrecognized parameter name.)')
            CALL WRTERR
            GO TO 3800
          END IF
          IF(NPARFILE.GT.0)THEN
            IF((IPAR.GT.NPAR).AND.(IPAR.LE.NPAR+NPARFILE))THEN
              CALL ILLPRR(CLINE(N1:N2))
              WRITE(ERRMSG,31911)
31911         FORMAT('  (A prior information equation cannot ',
     +        'reference a file-parameter.)')
              CALL WRTERR
              GO TO 3800
            END IF
          END IF
          IF(NPARSEC.GT.0)THEN
            IF((IPAR.GT.NPAR+NPARFILE).AND.
     +         (IPAR.LE.NPAR+NPARFILE+NPARSEC))THEN
              CALL ILLPRR(CLINE(N1:N2))
              WRITE(ERRMSG,31912)
31912         FORMAT('  (A prior information equation cannot ',
     +        'reference a secondary parameter.)')
              CALL WRTERR
              GO TO 3800
            END IF
          END IF
          IF(APAR(IPAR)(1:3).EQ.'iw_')THEN
            IW_PAR=1
            IF(IPRIOR.GT.1)THEN
              DO IJ=1,IPRIOR
                IF(IW_PR(IJ).EQ.IPAR)THEN
                  CALL ILLPRR(CLINE(N1:N2))
                  WRITE(ERRMSG,3193) TRIM(APAR(IPAR))
3193              FORMAT('  (Adaptive regularisation parameter "',a,
     +            '" cited in previous prior information equation.)')
                  CALL WRTERR
                  GO TO 3194
                END IF
              END DO
            END IF
3194        CONTINUE
            IW_PR(IPRIOR)=IPAR
            IF(IW_FLAG.NE.-1)IW_FLAG=1
          END IF
          IF(ITRANS(IPAR).EQ.-1000000) THEN
            CALL ILLPRR(CLINE(N1:N2))
            WRITE(ERRMSG,3200) PRRNAM(:NBLNK(PRRNAM))
3200        FORMAT('  (Prior information cannot be supplied for fixed ',
     +      'parameters',A)
            CALL WRTERR
            GO TO 3800
          ELSE IF(ITRANS(IPAR).LT.0)THEN
            CALL ILLPRR(CLINE(N1:N2))
            WRITE(ERRMSG,3205) PRRNAM(:NBLNK(PRRNAM))
3205        FORMAT('  (Prior information cannot be supplied for tied ',
     +      'parameters - only for the parameter to which they are ',
     +      'tied',A)
            CALL WRTERR
            GO TO 3800
          ELSE IF(ITRANS(IPAR).EQ.0)THEN
            IF(JLOG.EQ.1)THEN
              CALL ILLPRR(CLINE(N1:N2))
              WRITE(ERRMSG,3210) APAR(IPAR)(:NBLNK(APAR(IPAR))),
     +        PRRNAM(:NBLNK(PRRNAM))
3210          FORMAT('  (Parameter "',A,'" is not log-transformed',A)
              CALL WRTERR
              GO TO 3800
            END IF
            GO TO 3790
          ELSE IF(ITRANS(IPAR).EQ.1)THEN
            IF(JLOG.EQ.0)THEN
              CALL ILLPRR(CLINE(N1:N2))
              WRITE(ERRMSG,3130) APAR(IPAR)(:NBLNK(APAR(IPAR))),
     +        PRRNAM(:NBLNK(PRRNAM))
3130          FORMAT('  (Parameter "',A,
     +        '" is log-transformed and must ',
     +        'be referenced as such in prior information',A)
              CALL WRTERR
              GO TO 3800
            END IF
            GO TO 3790
          END IF
        ELSE IF(JOBS.EQ.1)THEN
          WRITE(FMT,3110) N2-N1+1
          IF(IPRIOR.GT.NPRIOR) GO TO 3203
          READ(CLINE(N1:N2),FMT,ERR=3141) X(IPRIOR,NESPR1)
          GO TO 3161
3141      CALL ILLPRR(CLINE(N1:N2))
          WRITE(ERRMSG,3151)
3151      FORMAT('  (Cannot read prior information value.)')
          CALL WRTERR
          GO TO 3900
3161      JOBS=0
          JWGHT=1
          GO TO 3090
        ELSE IF(JWGHT.EQ.1)THEN
          WRITE(FMT,3110) N2-N1+1
          READ(CLINE(N1:N2),FMT,ERR=3171) RTEMP
          GO TO 3191
3171      CALL ILLPRR(CLINE(N1:N2))
          WRITE(ERRMSG,3181)
3181      FORMAT('  (Cannot read prior information weight.)')
          CALL WRTERR
          PZROWT=-1
          GO TO 3900
3191      JWGHT=0
          NEWSTR=0
          IF(RTEMP.LT.0.0D0) THEN
            CALL ILLPRR(CLINE(N1:N2))
            WRITE(ERRMSG,3195) PRRNAM(:NBLNK(PRRNAM))
3195        FORMAT('  (Prior information weight must be zero or ',
     +      'positive',A)
            CALL WRTERR
          END IF
          OWGHT(IPRIOR+NOBS)=RTEMP
          IF(RTEMP.NE.0.0D0)PZROWT=0
          IF(NOBSGP1.NE.-999)THEN
            IF(CLINE(N2+1:).EQ.' ')THEN
              IOBGNM(IPRIOR+NOBS)=-1
              NPRWRN=1
              GO TO 3066
            ELSE
              JGROUP=1
              JWGHT=0
              GO TO 3090
            END IF
          ELSE
            IOBGNM(IPRIOR+NOBS)=-1
            GO TO 3066
          END IF
        ELSE IF(JGROUP.EQ.1)THEN
          IF(N2-N1+1.GT.12)THEN
            CALL ILLPRR(CLINE(N1:N2))
            WRITE(ERRMSG,3196)
3196        FORMAT('   (Observation group name greater than 12 ',
     +      'characters long.)')
            CALL WRTERR
          ELSE
            AGPNME=CLINE(N1:N2)
            DO 3197 I=1,NOBSGP
              IF(AOBSGP(I).EQ.AGPNME) THEN
                IOBGNM(IPRIOR+NOBS)=I
                IF(AGPNME.EQ.'predict')THEN
                  WRITE(ERRMSG,3192)AALINE(1:MMLINE)
3192              FORMAT(A,'an item of prior information cannot ',
     +            'belong to observation group "predict".')
                  CALL WRTERR
                END IF
                GO TO 3198
              END IF
3197        CONTINUE
            CALL ILLPRR(CLINE(N1:N2))
            WRITE(ERRMSG,3199)
3199        FORMAT('   (Unrecognised observation group name.)')
            CALL WRTERR
            IOBGNM(IPRIOR+NOBS)=-1
3198        CONTINUE
          END IF
          GO TO 3066
        ELSE
          CALL ILLPRR(CLINE(N1:N2))
          GO TO 3900
        END IF

3203    GO TO 9800
3790    IES=0
        DO 3139 I=1,NPAR
        IF(ITRANS(I).LT.0) GO TO 3139
        IES=IES+1
        IF(I.EQ.IPAR) GO TO 3138
3139    CONTINUE
3138    IF(IPRIOR.GT.NPRIOR) GO TO 3203
        IF((X(IPRIOR,IES).NE.0.0D0).AND.(X(IPRIOR,IES).LT.1.0D300))THEN
          WRITE(ERRMSG,3135) AALINE(1:MMLINE),
     +    APAR(IPAR)(:NBLNK(APAR(IPAR)))
3135      FORMAT(A,'parameter "',A,'" referenced more than once in ',
     +    'single prior information line.')
          CALL WRTERR
          X(IPRIOR,IES)=1.0D300
        ELSE
          IF(RRTEMP.NE.0.0D0)X(IPRIOR,IES)=RRTEMP
        END IF
3800    JNAME=0
        JMATH=1
        GO TO 3090
3900    NEWSTR=1
        JCONT=0
        GO TO 3066

3202    IF(IERR.NE.NUMERR) GO TO 9800
        DO 3230 IPRIOR=1,NPRIOR
        DO 3228 I=1,NESPAR
        IF(X(IPRIOR,I).NE.0.0D0) GO TO 3230
3228    CONTINUE
        WRITE(ERRMSG,3225) APRIOR(IPRIOR)(:NBLNK(APRIOR(IPRIOR)))
3225    FORMAT(' Prior information line "',A,'" contains only ',
     +  'zero-valued parameter factors.')
        CALL WRTERR
3230    CONTINUE
C        IF(NPRIOR.EQ.1) GO TO 3400
C        DO 3300 IPRIOR=1,NPRIOR-1
C        DO 3290 I=IPRIOR+1,NPRIOR
C        CALL MKRTIO(NESPR1,NPRIOR,IPRIOR,I,X,Y)
C        CALL ALLSAM(N2,NESPR1,Y)
C        IF(N2.EQ.1) THEN
C          WRITE(ERRMSG,3250) APRIOR(IPRIOR)(:NBLNK(APRIOR(IPRIOR))),
C     +    APRIOR(I)(:NBLNK(APRIOR(I)))
C3250      FORMAT(' Prior information equations "',A,'" and "',A,
C     +    '" contain the same information.')
C          CALL WRTERR
C        END IF
C3290    CONTINUE
C3300    CONTINUE
3400    CONTINUE

C -- SOME FINAL CHECKING OF ADAPTIVE REGULARISATION FUNCTIONALITY IS
C    CARRIED OUT.

        ICOUNT=0
        IF(IW_FLAG.EQ.1)THEN
          DO IPRIOR=1,NPRIOR
            IF(IW_PR(IPRIOR).NE.0)THEN
              ICOUNT=ICOUNT+1
              J=IOBGNM(NOBS+IPRIOR)
              IF(J.EQ.-1)THEN
                WRITE(ERRMSG,4010)
4010            FORMAT(' Any prior information equation which ',
     +          'cites an "iw_" (adaptive regularisation) parameter ',
     +          'must belong to an observation group ',
     +          'whose name begins with "regul".')
                CALL WRTERR
                GO TO 4100
              ELSE
                IF(AOBSGP(J)(1:5).NE.'regul')THEN
                  WRITE(ERRMSG,4010)
                  CALL WRTERR
                  GO TO 4100
                END IF
              END IF
              IF(ICOUNT.EQ.1)THEN
                JJ=J
              ELSE
                IF(JJ.NE.J)THEN
                  WRITE(ERRMSG,4020)
4020              FORMAT(' All prior information equations which ',
     +            'cite "iw_" (adaptive regularisation) parameters ',
     +            'must belong to the same observation group.')
                  CALL WRTERR
                  GO TO 4100
                END IF
              END IF
            END IF
          END DO

          DO I=1,NOBS
            IF(IOBGNM(I).EQ.JJ)THEN
              WRITE(ERRMSG,4030)
4030          FORMAT(' Prior information equations which cite ',
     +        'an "iw_" (adaptive regularisation) parameter must ',
     +        'belong to an observation group to which no other ',
     +        'observations or prior information equations belong.')
              CALL WRTERR
              GO TO 4100
            END IF
          END DO

          DO I=1,NPRIOR
            IF(IW_PR(I).NE.0) CYCLE
            IF(IOBGNM(I+NOBS).EQ.JJ)THEN
              WRITE(ERRMSG,4030)
              CALL WRTERR
              GO TO 4100
            END IF
          END DO

4100      CONTINUE

        END IF

        RETURN

9600    IFAIL=1
        GO TO 9999
9700    IFAIL=2
        GO TO 9999
9800    IFAIL=3
        GO TO 9999
9900    IFAIL=4
        GO TO 9999
9500    IFAIL=5
        GO TO 9999
9999    RETURN
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
        IF(IERR.GT.200) CALL EXIT(100)
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


        SUBROUTINE NOTNEG(NTYP,AVAR)

C -- SUBROUTINE NOTNEG ASSISTS IN THE WRITING OF OUT-OF-BOUNDS ERRORS

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
10        FORMAT(A,' ',A,': integer variable ',A,
     +    ' must not be negative.')
        ELSE IF(NTYP.EQ.1) THEN
          WRITE(ERRMSG,20) ALINE(:NBLNK(ALINE)),CASFL(1:MCASE),AVAR
20        FORMAT(A,' ',A,': real variable ',A,
     +    ' must not be negative.')
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

        SUBROUTINE ILLPRR(AINSTR)

C -- SUBROUTINE ILLPRR ASSISTS IN THE WRITING OF PRIOR INFORMATION ERRORS

        INTEGER MMLINE
        CHARACTER*100 ERRMSG*2120,ALINE*20,AALINE*200
        CHARACTER*(*) AINSTR

        COMMON /ERRMSG/ ERRMSG
        COMMON /ALINE/ ALINE,AALINE
        COMMON /MMLINE/MMLINE

        WRITE(ERRMSG,10) AALINE(1:MMLINE),AINSTR
10      FORMAT(A,'illegal prior information item - "',A,'".')
        CALL WRTERR

        RETURN
        END

        SUBROUTINE ILLSGN(AINSTR)

C -- SUBROUTINE ILLISGN ASSISTS IN THE WRITING OF PRIOR INFORMATION ERRORS

        INTEGER MMLINE
        CHARACTER*100 ERRMSG*2120,ALINE*20,AALINE*200
        CHARACTER*(*) AINSTR

        COMMON /ERRMSG/ ERRMSG
        COMMON /ALINE/ ALINE,AALINE
        COMMON /MMLINE/MMLINE

        WRITE(ERRMSG,10) AALINE(1:MMLINE),AINSTR
10      FORMAT(A,'"',A,'" sign occurs in unexpected place.')
        CALL WRTERR

        RETURN
        END


        SUBROUTINE MKRTIO(N,NPRIOR,IPRIOR,IROW,X,Y)

C -- SUBROUTINE MKRTIO ASSISTS IN CHECKING THE INTEGRITY OF PRIOR
C -- INFORMATION

        INTEGER N,NPRIOR,IROW,IPRIOR,J
        DOUBLE PRECISION X(NPRIOR,N),Y(N)

        DO 40 J=1,N
        IF((X(IPRIOR,J).EQ.0.0D0).AND.(X(IROW,J).EQ.0.0D0))THEN
          Y(J)=-1.0D300
        ELSE IF(X(IPRIOR,J).EQ.X(IROW,J))THEN
          Y(J)=1.0D0
        ELSE IF((ABS(X(IPRIOR,J)).GT.1.0D-150).AND.
     +    (ABS(X(IROW,J)).LT.1.0D150))THEN
          Y(J)=X(IROW,J)/X(IPRIOR,J)
        ELSE
          Y(J)=1.0D300
        ENDIF
40      CONTINUE

        RETURN
        END

        SUBROUTINE ALLSAM(YES,N,Y)

C -- SUBROUTINE ALLSAM ASSISTS IN CHECKING THE INTEGRITY OF PRIOR INFORMATION

        INTEGER YES,N,J
        DOUBLE PRECISION Y(N),RTEMP

        YES=0
        DO 10 J=1,N
        IF(Y(J).GT.-1.0D299) GO TO 15
10      CONTINUE
        RETURN
15      RTEMP=Y(J)
        IF(RTEMP.GT.1.0D299)RETURN
        DO 20 J=1,N
        IF(Y(J).LT.-1.0D299) GO TO 20
        IF(Y(J).NE.RTEMP) RETURN
20      CONTINUE
        YES=1
        RETURN
        END

        SUBROUTINE PRMCHK(ACODE,ACODEP,MODPRED,JFAIL,NPAR,NPARGP,ILINE,
     +  DMAX0,LBOUND,
     +  UBOUND,IPARGP,IDERIV,IDBND,DERVAL,DERBND,ITRANS,CHGTYP,LW,RW,
     +  PVAL,AGROUP,APAR,CLINE,ATEMP,PRECIS,JACFILE,NUMCOM,JCWARN,
     +  DERCOM,ISVDA,MAXABSPARMAX,ABSPARMAX,IREFOBS,IPPCOUNT,
     +  NPARSEC,NPARFILE,NEQUATION)

C -- SUBROUTINE PRMCHK CHECKS THE PARAMETER DATA SECTION OF THE PEST
C -- CONTROL FILE

        INTEGER NBLNK
        INTEGER JFAIL,NPAR,NPARGP,PVLERR,LBDERR,MMLINE,I,IFAIL,
     +  J,ITEMP,IPG,II,IERR,MCASE,UBDERR,NUMOPP,NUMZER,JACFILE,
     +  NUMCOM,JCWARN,ISVDA
        INTEGER NPARSEC,NPARFILE,NEQUATION
        INTEGER NTT,ABSIND,MAXABSPARMAX
        INTEGER IREFOBS
        INTEGER IPPCOUNT
        INTEGER IDERIV(NPARGP),IDBND(NPARGP),CHGTYP(NPAR),
     +  DERCOM(NPAR)
        INTEGER ITRANS(NPAR),LW(10),RW(10),IPARGP(NPAR)
        INTEGER ILINE,IFAIL4
        INTEGER NEQQ,IEQUATION,IPARSEC,IPARFILE,JTEMP
        DOUBLE PRECISION SCALE,OFFSET,RTEMP,RRTEMP,DMAX0
        DOUBLE PRECISION LBOUND(NPAR),UBOUND(NPAR),DERVAL(NPARGP),
     +  DERBND(NPARGP),PVAL(NPAR)
        DOUBLE PRECISION ABSPARMAX(MAXABSPARMAX)
        CHARACTER*(*) AGROUP(NPARGP),APAR(NPAR+NPARFILE+NPARSEC)
        CHARACTER*(*) CLINE,ATEMP,PRECIS,ACODE,ACODEP,MODPRED
        CHARACTER*2120 ERRMSG,CASFL*100,ALINE*20,AALINE*200,
     +  PARNAM*30
        CHARACTER*12 ATEMP12
        CHARACTER*20 ATEMP20
        CHARACTER*25 ATEMP25
        CHARACTER*30 ATEMP30

        COMMON /IERR/IERR,MCASE
        COMMON /ERRMSG/ ERRMSG
        COMMON /CASFL/ CASFL
        COMMON /ALINE/ ALINE,AALINE
        COMMON /MMLINE/ MMLINE

c       if jfail=0      no errors
c       if jfail=1      go to 9800 in main program
c       if jfail=2      go to 1300 in main program
c       if jfail=3      go to 9500 in main program
c       if jfail=4      go to 9600 in main program
c       if jfail=5      go to 9700 in main program

        JFAIL=0
        II=0
        J=0
        NUMOPP=0
        NUMZER=0
        IEQUATION=0
        IPARSEC=0
        IPARFILE=0
        APAR=' '         ! An array

        DO 1000 I=1,NPAR
        CHGTYP(I)=-1
7782    CONTINUE
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7782
        CALL LOWCAS(CLINE)
        CALL SHIFTL(CLINE)
        NEQQ=INDEX(CLINE,'=')
        IF(NEQQ.EQ.1)THEN
          WRITE(ERRMSG,393) AALINE(1:MMLINE)
393       FORMAT(A,'improper equation syntax.')
          CALL WRTERR
          GO TO 7782
        END IF
        IF((CLINE.EQ.'* observation data').OR.
     +     (CLINE.EQ.'* observation groups'))THEN
            CALL WRTINT(ATEMP,NPAR+NPARFILE+NEQUATION)
            WRITE(ERRMSG,810)AALINE(1:MMLINE),
     +      ATEMP(1:INDEX(ATEMP,' ')-1)
810         FORMAT(A,'too few "parameter data" lines - at least ',
     +      A,' expected.')
            CALL WRTERR
            JFAIL=2
            RETURN
        END IF
        IF(NEQQ.GT.0)THEN
          IEQUATION=IEQUATION+1
          IF(IEQUATION.EQ.NEQUATION+1)THEN
            ATEMP30='equations'
            ATEMP25='NEQUATIONS'
            WRITE(ERRMSG,391) AALINE(1:MMLINE),TRIM(ATEMP30),
     +      TRIM(ATEMP25)
391         FORMAT(A,'number of ',a,' exceeds ',a,'.')
            CALL WRTERR
          END IF
          ATEMP20=CLINE(1:NEQQ-1)
          ATEMP20=ADJUSTL(ATEMP20)
          ITEMP=LEN_TRIM(ATEMP20)
          IF(ITEMP.GT.12)THEN
            WRITE(ERRMSG,820) AALINE(1:MMLINE),
     +      TRIM(ATEMP20)
            CALL WRTERR
          END IF
          IF(NPARSEC.EQ.0) GO TO 394
          IF(IPARSEC.EQ.0) GO TO 394
          DO J=NPAR+NPARFILE+1,NPAR+NPARFILE+MIN(IPARSEC,NPARSEC)
            IF(ATEMP20.EQ.APAR(J)) GO TO 395
          END DO
394       CONTINUE
          ATEMP12=ATEMP20
          IPARSEC=IPARSEC+1
          IF(IPARSEC.EQ.NPARSEC+1)THEN
            ATEMP30='secondary parameters'
            ATEMP25='NPARSEC'
            WRITE(ERRMSG,391) AALINE(1:MMLINE),TRIM(ATEMP30),
     +      TRIM(ATEMP25)
            CALL WRTERR
          END IF
          IF(IPARSEC.LE.NPARSEC)
     +    APAR(NPAR+NPARFILE+IPARSEC)=ATEMP12
          IF(I.GT.0)THEN
            DO J=1,I
              IF(ATEMP12.EQ.APAR(J))THEN
                WRITE(ERRMSG,3962) AALINE(1:MMLINE),
     +          TRIM(ATEMP12)
3962            FORMAT(A,'secondary parameter "',A,
     +          '" has same name as a normal parameter.')
                CALL WRTERR
                GO TO 3965
              END IF
            END DO
          END IF
3965      CONTINUE
          IF(IPARFILE.GT.0)THEN
            DO J=NPAR+1,NPAR+MIN(IPARFILE,NPARFILE)
              IF(ATEMP12.EQ.APAR(J))THEN
                WRITE(ERRMSG,3966) AALINE(1:MMLINE),
     +          TRIM(ATEMP12)
3966            FORMAT(A,'secondary parameter "',A,
     +          '" has same name as a file-parameter.')
                CALL WRTERR
                GO TO 395
              END IF
            END DO
          END IF
395       CONTINUE
          GO TO 7782
        END IF
        CALL LINSPL(IFAIL,2,LW,RW,CLINE)
        IF(CLINE(LW(2):RW(2)).EQ.'file_parameter')THEN
          IPARFILE=IPARFILE+1
          IF(IPARFILE.EQ.NPARFILE+1)THEN
            ATEMP30='file-parameters'
            ATEMP25='NPARFILE'
            WRITE(ERRMSG,391) AALINE(1:MMLINE),TRIM(ATEMP30),
     +      TRIM(ATEMP25)
            CALL WRTERR
          END IF
          IF(RW(1)-LW(1)+1.GT.12)THEN
            WRITE(ERRMSG,820) AALINE(1:MMLINE),
     +      CLINE(LW(1):RW(1))
            CALL WRTERR
            APAR(NPAR+IPARFILE)=' '
          ELSE
            ATEMP12=CLINE(LW(1):RW(1))
            IF(IPARFILE.LE.NPARFILE)
     +      APAR(NPAR+IPARFILE)=ATEMP12
            IF(IPARFILE.GT.1)THEN
              DO J=NPAR+1,NPAR+MIN(IPARFILE,NPARFILE)-1
                IF(ATEMP12.EQ.APAR(J))THEN
                  WRITE(ERRMSG,396) AALINE(1:MMLINE),
     +            TRIM(ATEMP12)
396               FORMAT(A,'file-parameter name "',A,
     +            '" used more than once.')
                  CALL WRTERR
                  GO TO 3959
                END IF
              END DO
            END IF
3959        CONTINUE
            IF(I.GT.0)THEN
              DO J=1,I
                IF(ATEMP12.EQ.APAR(J))THEN
                  WRITE(ERRMSG,3968) AALINE(1:MMLINE),
     +            TRIM(ATEMP12)
3968              FORMAT(A,'file-parameter name "',A,
     +            '" is same as a normal parameter name.')
                  CALL WRTERR
                  GO TO 3969
                END IF
              END DO
            END IF
3969        CONTINUE
            IF(IPARSEC.GT.0)THEN
              DO J=NPAR+NPARFILE+1,NPAR+NPARFILE+MIN(IPARSEC,NPARSEC)
                IF(ATEMP12.EQ.APAR(J))THEN
                  WRITE(ERRMSG,3961) AALINE(1:MMLINE),
     +            TRIM(ATEMP12)
3961              FORMAT(A,'file-parameter name "',A,
     +            '" is same as a secondary parameter name.')
                  CALL WRTERR
                  GO TO 397
                END IF
              END DO
            END IF
397         CONTINUE
          END IF
          GO TO 7782
        END IF
        CALL LINSPL(IFAIL,9,LW,RW,CLINE)
        IF(IFAIL.NE.0) GO TO 9500
        IF(RW(1)-LW(1)+1.GT.12)THEN
          WRITE(ERRMSG,820) AALINE(1:MMLINE),
     +    CLINE(LW(1):RW(1))
820       FORMAT(A,'parameter name "',A,'" greater ',
     +    'than 12 characters long.')
          CALL WRTERR
        END IF
        APAR(I)=CLINE(LW(1):RW(1))
        PARNAM=' (parameter "'//APAR(I)(:NBLNK(APAR(I)))//'").'
        IF(I.GE.2)THEN
          DO 825 J=1,I-1
          IF(APAR(I).EQ.APAR(J))THEN
            WRITE(ERRMSG,823) AALINE(1:MMLINE),
     +      APAR(I)(:NBLNK(APAR(I)))
823         FORMAT(A,'parameter name "',A,'" used more than once.')
            CALL WRTERR
            GO TO 826
          END IF
825       CONTINUE
        END IF
826     CONTINUE
        IF(CLINE(LW(2):RW(2)).EQ.'none')THEN
          ITRANS(I)=0
        ELSE IF(CLINE(LW(2):RW(2)).EQ.'log')THEN
          ITRANS(I)=1
        ELSE IF(CLINE(LW(2):RW(2)).EQ.'fixed') THEN
          ITRANS(I)=-1000000
        ELSE IF(CLINE(LW(2):RW(2)).EQ.'tied') THEN
          ITRANS(I)=-999998
        ELSE
          WRITE(ERRMSG,830) AALINE(1:MMLINE)
830       FORMAT(A,'character variable PARTRANS must be "none", ',
     +    '"log", "fixed" or "tied".')
          CALL WRTERR
        ITRANS(I)=-999999
        END IF
        IF((ITRANS(I).EQ.0).OR.(ITRANS(I).EQ.1))THEN
          IF(CLINE(LW(3):RW(3)).EQ.'relative') THEN
            CHGTYP(I)=0
          ELSE IF(CLINE(LW(3):RW(3)).EQ.'factor') THEN
            CHGTYP(I)=1
          ELSE IF(CLINE(LW(3):LW(3)+7).EQ.'absolute')THEN
            ATEMP=CLINE(LW(3):RW(3))
            IF(ATEMP(9:9).NE.'(') THEN
              WRITE(ERRMSG,451) AALINE(1:MMLINE)
451           FORMAT(A,'"(" character should follow "absolute" string.')
              CALL WRTERR
              GO TO 459
            END IF
            ATEMP=ATEMP(10:)
            NTT=LEN_TRIM(ATEMP)
            IF(ATEMP(NTT:NTT).NE.')') THEN
              WRITE(ERRMSG,452) AALINE(1:MMLINE)
452           FORMAT(A,'ABSPARMAX index as "(n)" should ',
     +        'follow "absolute" string.')
              CALL WRTERR
              GO TO 459
            END IF
            ATEMP(NTT:NTT)=' '
            CALL INTREAD(IFAIL,ATEMP,ABSIND)
            IF(IFAIL.NE.0)THEN
              WRITE(ERRMSG,453) AALINE(1:MMLINE)
453           FORMAT(A,'cannot read ABSPARMAX index following ',
     +        '"absolute" string.')
              CALL WRTERR
              GO TO 459
            END IF
            IF((ABSIND.LT.1).OR.(ABSIND.GT.MAXABSPARMAX))THEN
              WRITE(ERRMSG,454) AALINE(1:MMLINE)
454           FORMAT(A,'ABSPARMAX index following "absolute" string ',
     +        'out of range.')
              CALL WRTERR
              GO TO 459
            END IF
            IF(ABSPARMAX(ABSIND).LT.-1.0E35)THEN
              CALL WRTINT(ATEMP,ABSIND)
              WRITE(ERRMSG,455)  AALINE(1:MMLINE), TRIM(ATEMP)
455           FORMAT(A,'ABSPARMAX index (',A,') not assigned ',
     +        'a value, yet referenced at this line.')
              CALL WRTERR
              GO TO 459
            END IF
459         CONTINUE
            IF(ITRANS(I).EQ.1)THEN
              WRITE(ERRMSG,456)  AALINE(1:MMLINE)
456           FORMAT(A,'absolute change limit must not be assigned ',
     +        'to a log-transformed parameter.')
              CALL WRTERR
            END IF
            CHGTYP(I)=-ABSIND
          ELSE
            WRITE(ERRMSG,840) AALINE(1:MMLINE)
840         FORMAT(A,'character variable PARCHGLIM must be "relative", ',
     +      '"factor" or "absolute(N)" where N is 1, 2, etc.')
            CALL WRTERR
          END IF
          IF((ITRANS(I).EQ.1).AND.(CHGTYP(I).EQ.0))THEN
            WRITE(ERRMSG,842) AALINE(1:MMLINE),
     +      PARNAM(:NBLNK(PARNAM))
842         FORMAT(A,'if PARTRANS is "log", PARCHGLIM must be ',
     +      '"factor"',A)
            CALL WRTERR
          END IF
        END IF
        PVLERR=0
        CALL NUMRD(IFAIL,1,LW(4),RW(4),ITEMP,PVAL(I),CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(1,'PARVAL1')
          PVLERR=1
        ELSE
          IF((PVAL(I).LE.0.0D0).AND.(ITRANS(I).EQ.1))THEN
            WRITE(ERRMSG,845) AALINE(1:MMLINE),
     +      PARNAM(:NBLNK(PARNAM))
845         FORMAT(A,'log-transformed parameter cannot be zero or ',
     +      'negative',A)
            CALL WRTERR
          END IF
          IF((ABS(PVAL(I)).GT.1.0D38).AND.(PRECIS.EQ.'single'))THEN
            WRITE(ERRMSG,846) AALINE(1:MMLINE),
     +      PARNAM(:NBLNK(PARNAM))
846         FORMAT(A,'parameter magnitude too large for single ',
     +      'precision protocol',A)
            CALL WRTERR
          END IF
        END IF
        LBDERR=0
        UBDERR=0
        CALL NUMRD(IFAIL,1,LW(5),RW(5),ITEMP,LBOUND(I),CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(1,'PARLBND')
          LBDERR=1
        ELSE
          IF((PVLERR.EQ.0).AND.(PVAL(I).LT.LBOUND(I)))THEN
            WRITE(ERRMSG,850) AALINE(1:MMLINE),
     +      PARNAM(:NBLNK(PARNAM))
850         FORMAT(A,'parameter initial value less than parameter ',
     +      'lower bound',A)
            CALL WRTERR
          END IF
          IF((ITRANS(I).EQ.1).AND.(LBOUND(I).LE.0.0D0))THEN
            WRITE(ERRMSG,853) AALINE(1:MMLINE),
     +      PARNAM(:NBLNK(PARNAM))
853         FORMAT(A,'parameter lower bound must be greater than ',
     +      'zero for a log-transformed parameter',A)
            CALL WRTERR
          END IF
        END IF
        CALL NUMRD(IFAIL,1,LW(6),RW(6),ITEMP,UBOUND(I),CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(1,'PARUBND')
          UBDERR=1
        ELSE
          IF((PVLERR.EQ.0).AND.(PVAL(I).GT.UBOUND(I)))THEN
            WRITE(ERRMSG,860) AALINE(1:MMLINE),
     +      PARNAM(:NBLNK(PARNAM))
860         FORMAT(A,
     +      'parameter initial value greater than parameter ',
     +      'upper bound',A)
            CALL WRTERR
          END IF
          IF((LBDERR.EQ.0).AND.(UBOUND(I).LE.LBOUND(I)))THEN
            WRITE(ERRMSG,862) AALINE(1:MMLINE),
     +      PARNAM(:NBLNK(PARNAM))
862         FORMAT(A,'parameter upper bound less than or equal to ',
     +      'parameter lower bound',A)
            CALL WRTERR
          END IF
        END IF
        IF((ITRANS(I).EQ.0).OR.(ITRANS(I).EQ.1))THEN
          IF((UBDERR.EQ.0).AND.(LBDERR.EQ.0))THEN
            IF(PRECIS.EQ.'single')THEN
              IF((ABS(LBOUND(I)).GT.1.0E38).OR.
     +           (ABS(UBOUND(I)).GT.1.0E38))THEN
                WRITE(ERRMSG,847) AALINE(1:MMLINE),
     +          PARNAM(:NBLNK(PARNAM))
847             FORMAT(A,' magnitude of parameter upper or lower ',
     +          'bound too large for single precision protocol',A)
                CALL WRTERR
              END IF
            END IF
            IF(CHGTYP(I).EQ.1)THEN
              IF(ITRANS(I).NE.1)THEN
                IF(UBOUND(I).GT.0.0D0)THEN
                  IF(LBOUND(I).LE.0.0D0) THEN
                    WRITE(ERRMSG,863)AALINE(1:MMLINE),
     +              PARNAM(:NBLNK(PARNAM))
863                 FORMAT(A,'parameter cannot be factor-limited ',
     +              'if upper or lower bound is zero or if upper ',
     +              'and lower bounds are of opposite sign',A)
                    CALL WRTERR
                  END IF
                ELSE IF(LBOUND(I).LT.0.0D0)THEN
                  IF(UBOUND(I).GE.0.0D0)THEN
                  WRITE(ERRMSG,863)AALINE(1:MMLINE),
     +            PARNAM(:NBLNK(PARNAM))
                    CALL WRTERR
                  END IF
                END IF
              END IF
            ELSE IF(CHGTYP(I).EQ.0)THEN
              IF(UBOUND(I).GT.0.0D0)THEN
                IF(LBOUND(I).LT.0.0D0)THEN
                  NUMOPP=NUMOPP+1
                ELSE IF(LBOUND(I).EQ.0.0D0)THEN
                  NUMZER=NUMZER+1
                END IF
              ELSE IF(LBOUND(I).LT.0.0D0)THEN
                IF(UBOUND(I).GT.0.0D0)THEN
                  NUMOPP=NUMOPP+1
                ELSE IF(UBOUND(I).EQ.0.0D0)THEN
                  NUMZER=NUMZER+1
                END IF
              END IF
            END IF
          END IF
        END IF
c        IF((ACODE.EQ.'m').OR.(ACODEP.EQ.'m')) THEN
        IF(CLINE(LW(7):RW(7)).EQ.'none') THEN
          IF((ITRANS(I).EQ.0).OR.(ITRANS(I).EQ.1))THEN
              WRITE(ERRMSG,865) AALINE(1:MMLINE),
     +        PARNAM(:NBLNK(PARNAM))
865           FORMAT(A,'only tied or fixed parameters can belong to ',
     +        'parameter group "none"',A)
              CALL WRTERR
          END IF
          IPARGP(I)=0
        ELSE
          DO 880 J=1,NPARGP
          IF(CLINE(LW(7):RW(7)).EQ.AGROUP(J))THEN
            IPARGP(I)=J
            GO TO 881
          END IF
880       CONTINUE
          WRITE(ERRMSG,870) AALINE(1:MMLINE),
     +    APAR(I)(:NBLNK(APAR(I))),
     +    CLINE(LW(7):RW(7))
870       FORMAT(A,'parameter "',A,'" belongs to unknown parameter ',
     +    'group ("',A,'").')
          CALL WRTERR
          IPARGP(I)=-999
        END IF
c        END IF
881     CONTINUE
        CALL NUMRD(IFAIL,1,LW(8),RW(8),ITEMP,SCALE,CLINE)
        IF(IFAIL.NE.0) THEN
          CALL RDERR(1,'SCALE')
        ELSE
          IF(SCALE.EQ.0.0D0)THEN
            WRITE(ERRMSG,885) AALINE(1:MMLINE),
     +      PARNAM(:NBLNK(PARNAM))
885         FORMAT(A,'real variable SCALE must not be zero',A)
            CALL WRTERR
          END IF
        END IF
        CALL NUMRD(IFAIL,1,LW(9),RW(9),ITEMP,OFFSET,CLINE)
        IF(IFAIL.NE.0) CALL RDERR(1,'OFFSET')
        IF((ACODE.EQ.'m').OR.(ACODEP.EQ.'m')) THEN
        IF((ITRANS(I).EQ.0).OR.(ITRANS(I).EQ.1))THEN
          IF((IPARGP(I).GT.0).AND.(PVLERR.EQ.0))THEN
            IF((PVAL(I).EQ.0.0D0).AND.(IDERIV(IPARGP(I)).EQ.1)
     +      .AND.(IDBND(IPARGP(I)).EQ.0))THEN
              WRITE(ERRMSG,890) AALINE(1:MMLINE),
     +        APAR(I)(:NBLNK(APAR(I))),
     +        AGROUP(IPARGP(I))(:NBLNK(AGROUP(IPARGP(I))))
890           FORMAT(A,'parameter "',A,'" initial value zero. ',
     +        'Derivative increment will be zero as it is calculated ',
     +        'as "relative" and no absolute increment lower bound is ',
     +        'provided for parameters in group "',A,'".')
              CALL WRTERR
            END IF
          END IF
        END IF
        END IF

        CALL LINSPL(IFAIL,10,LW,RW,CLINE)
        IF(IFAIL.NE.0)THEN
          IF(JCWARN.EQ.0)THEN
            WRITE(ERRMSG,910) AALINE(1:MMLINE)
910         FORMAT(A,'no value supplied for DERCOM yet a value was ',
     +      'supplied for JACFILE in the "* control data" section of ',
     +      'this file.')
            CALL WRTERR
          END IF
          GO TO 1000
        ELSE
          IF(JCWARN.EQ.1)THEN
            WRITE(ERRMSG,911) AALINE(1:MMLINE)
911         FORMAT(A,'a value was supplied for DERCOM yet no value ',
     +      'was supplied for NUMCOM in the "* control data" section ',
     +      'of this file.')
            CALL WRTERR
            GO TO 1000
          END IF
        END IF
        CALL NUMRD(IFAIL,0,LW(10),RW(10),DERCOM(I),RTEMP,CLINE)
        IF(IFAIL.NE.0)THEN
          WRITE(ERRMSG,914) AALINE(1:MMLINE)
914       FORMAT(A,'cannot read value for DERCOM.')
          CALL WRTERR
          GO TO 1000
        ELSE
          IF((ISVDA.EQ.1).AND.(JACFILE.NE.0))THEN
            IF(DERCOM(I).NE.0) THEN
              WRITE(ERRMSG,931) AALINE(1:MMLINE)
931           FORMAT(A,'if derivatives are supplied externally ',
     +        '(i.e. JACFILE is not set to zero) ',
     +        'and SVD-assist is activated, then DERCOM must be ',
     +        'set to zero for all super parameters.')
              CALL WRTERR
            END IF
          ELSE
            IF(DERCOM(I).LT.0)THEN
              WRITE(ERRMSG,915) AALINE(1:MMLINE)
915           FORMAT(A,'variable DERCOM must not be less than 0.')
              CALL WRTERR
            ELSE
              IF(JACFILE.EQ.0)THEN
                IF(DERCOM(I).EQ.0)THEN
                  WRITE(ERRMSG,920) AALINE(1:MMLINE)
920               FORMAT(A,'DERCOM is supplied as zero indicating that ',
     +            'derivatives wrt this parameter are supplied ',
     +            'externally; however JACFILE is set to zero.')
                  CALL WRTERR
                END IF
              END IF
              IF(DERCOM(I).GT.NUMCOM)THEN
                WRITE(ERRMSG,930) AALINE(1:MMLINE)
930             FORMAT(A,'DERCOM must not be greater than NUMCOM ',
     +          '(the number of command lines supplied in the ',
     +          '"* control data" section of the PEST control file.)')
                CALL WRTERR
              END IF
            END IF
          END IF
        END IF

1000    CONTINUE

        IF(IERR.NE.0)GO TO 9800

        DO 1020 I=1,NPAR
        IF(ITRANS(I).GE.0) GO TO 1021
1020    CONTINUE
        WRITE(ERRMSG,1010)
1010    FORMAT(' There are no parameters to optimise: all parameters ',
     +  'are either fixed or tied.')
        CALL WRTERR
        GO TO 9800
1021    CONTINUE

        IF(((ACODE.EQ.'d').AND.(ACODEP.EQ.'d')).OR.
     +     ((ACODE.EQ.'d').AND.(MODPRED.EQ.'t')).OR.
     +     ((ACODE.EQ.'d').AND.(MODPRED.EQ.' '))) GO TO 1101
        DO 1040 I=1,NPARGP
        IF((IDERIV(I).NE.3).OR.(IDBND(I).GT.0.0D0)) GO TO 1040
        ITEMP=0
        DO 1035 J=1,NPAR
        IF(IPARGP(J).EQ.I)THEN
          IF(ITRANS(J).GE.0) ITEMP=ITEMP+1
          IF(PVAL(J).NE.0.0D0) GO TO 1040
        ENDIF
1035    CONTINUE
        IF(ITEMP.EQ.0) GO TO 1040
        WRITE(ERRMSG,1030) AGROUP(I)(:NBLNK(AGROUP(I)))
1030    FORMAT(' All parameters belonging to group "',A,
     +'" have an initial value of zero: cannot implement "rel_to_max"',
     +' derivatives calculation (unless an increment lower bound is ',
     +'provided).')
        CALL WRTERR
1040    CONTINUE

        DO 1100 I=1,NPAR
        IF(ITRANS(I).LT.0) GO TO 1100
        IF(APAR(I)(1:3).EQ.'iw_') GO TO 1100
        IPG=IPARGP(I)
        RTEMP=(UBOUND(I)-LBOUND(I))/3.2D0
        IF(IDERIV(IPG).EQ.1)THEN
          IF(ABS(DERVAL(IPG)*PVAL(I)).GT.RTEMP) GO TO 1080
          IF((IDBND(IPG).EQ.1).AND.(DERBND(IPG).GT.RTEMP)) GO TO 1090
        ELSE IF(IDERIV(IPG).EQ.2) THEN
          IF(DERVAL(IPG).GT.RTEMP) THEN
            WRITE(ERRMSG,1036) APAR(I)(:NBLNK(APAR(I)))
1036        FORMAT(' Derivative increment for parameter "',A,
     +      '" greater than parameter range divided by 3.2.')
            CALL WRTERR
            GO TO 1100
          ENDIF
        ELSE IF(IDERIV(IPG).EQ.3) THEN
          RRTEMP=0.0D0
          DO 1050 J=1,NPAR
          IF(IPARGP(J).NE.IPG) GO TO 1050
          IF(ABS(PVAL(J)).GT.RRTEMP) RRTEMP=ABS(PVAL(J))
1050      CONTINUE
          IF(ABS(DERVAL(IPG)*RRTEMP).GT.RTEMP) GO TO 1080
          IF((IDBND(IPG).EQ.1).AND.(DERBND(IPG).GT.RTEMP)) GO TO 1090
        END IF
        GO TO 1100
1080    WRITE(ERRMSG,1085) APAR(I)(:NBLNK(APAR(I)))
1085    FORMAT(' Initial derivative increment for parameter "',A,
     +  '" greater than parameter range divided by 3.2.')
        CALL WRTERR
        GO TO 1100
1090    WRITE(ERRMSG,1095) APAR(I)(:NBLNK(APAR(I)))
1095    FORMAT(' Derivative increment lower bound for ',
     +  'parameter "',A,'" greater than parameter range ',
     +  'divided by 3.2.')
        CALL WRTERR
1100    CONTINUE
1101    CONTINUE

        IF((NUMOPP.NE.0).AND.(DMAX0.LE.1.0D0))THEN
          WRITE(ERRMSG,1105)
1105      FORMAT(' RELPARMAX less than or equal to 1, yet at least ',
     +    'one relative-limited parameter has an ',
     +    'upper and lower bound of opposite sign.')
          CALL WRTERR
        ELSE IF((NUMZER.NE.0).AND.(DMAX0.LT.1.0D0))THEN
          WRITE(ERRMSG,1106)
1106      FORMAT(' RELPARMAX less than 1, yet at least one relative-',
     +    'limited parameter has an upper or lower bound of zero.')
          CALL WRTERR
        END IF

        ITEMP=0
        DO 1120 I=1,NPAR
        IF((ITRANS(I).GE.0).OR.(ITRANS(I).EQ.-1000000)) GO TO 1120
        ITEMP=ITEMP+1
1120    CONTINUE
        IF((ITEMP.GT.0).OR.(IPARSEC.LT.NPARSEC).OR.
     +     (IPARFILE.LT.NPARFILE).OR.(IEQUATION.LT.NEQUATION))THEN
          I=0
          DO 1200
          I=I+1
7783      CONTINUE
          IF((I.GT.ITEMP).AND.(IPARSEC.GE.NPARSEC).AND.
     +       (IPARFILE.GE.NPARFILE).AND.(IEQUATION.GE.NEQUATION))
     +       GO TO 12001
          ILINE=ILINE+1
          CALL WRTINT(ATEMP,ILINE)
          ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
          AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
          MMLINE=NBLNK(AALINE)+1
          READ(30,'(A)',ERR=9600,END=9700) CLINE
          CALL PPROC(CLINE,IPPCOUNT)
          IF(CLINE.EQ.' ') GO TO 7783
          CALL LOWCAS(CLINE)
          CALL SHIFTL(CLINE)
          IF((CLINE.EQ.'* observation data').OR.
     +       (CLINE.EQ.'* observation groups'))THEN
            IF(I.LE.ITEMP)THEN
              WRITE(ERRMSG,1130) AALINE(1:MMLINE)
1130          FORMAT(A,'none or too few "parameter data" lines linking ',
     +        'tied parameters to parent parameters.')
              CALL WRTERR
            ELSE IF(IPARSEC.LT.NPARSEC)THEN
                WRITE(ERRMSG,1201)
                ATEMP30='secondary parameters'
                ATEMP20='NPARSEC'
                WRITE(ERRMSG,1201) AALINE(1:MMLINE),TRIM(ATEMP30),
     +          TRIM(ATEMP20)
1201            FORMAT(A,'the number of ',A,' named in the ',
     +          '"parameter data" section is less than ',A,'.')
                CALL WRTERR
            ELSE IF(IPARFILE.LT.NPARFILE)THEN
                WRITE(ERRMSG,1201)
                ATEMP30='file-parameters'
                ATEMP20='NPARFILE'
                WRITE(ERRMSG,1201) AALINE(1:MMLINE),TRIM(ATEMP30),
     +          TRIM(ATEMP20)
                CALL WRTERR
            ELSE IF(IEQUATION.LT.NEQUATION)THEN
                WRITE(ERRMSG,1201)
                ATEMP30='equations'
                ATEMP20='NEQUATION'
                WRITE(ERRMSG,1201) AALINE(1:MMLINE),TRIM(ATEMP30),
     +          TRIM(ATEMP20)
                CALL WRTERR
            END IF
            JFAIL=2
            RETURN
          END IF
          NEQQ=INDEX(CLINE,'=')
          IF(NEQQ.EQ.1)THEN
            WRITE(ERRMSG,393) AALINE(1:MMLINE)
            CALL WRTERR
            GO TO 7783
          END IF
          IF(NEQQ.GT.0)THEN
            IEQUATION=IEQUATION+1
            IF(IEQUATION.EQ.NEQUATION+1)THEN
              ATEMP30='equations'
              ATEMP25='NEQUATIONS'
              WRITE(ERRMSG,391) AALINE(1:MMLINE),TRIM(ATEMP30),
     +        TRIM(ATEMP25)
              CALL WRTERR
            END IF
            ATEMP20=CLINE(1:NEQQ-1)
            ATEMP20=ADJUSTL(ATEMP20)
            JTEMP=LEN_TRIM(ATEMP20)
            IF(JTEMP.GT.12)THEN
              WRITE(ERRMSG,820) AALINE(1:MMLINE),
     +        TRIM(ATEMP20)
              CALL WRTERR
            END IF
            IF(IPARSEC.EQ.0) GO TO 3941
            DO J=NPAR+NPARFILE+1,NPAR+NPARFILE+MIN(IPARSEC,NPARSEC)
              IF(ATEMP20.EQ.APAR(J)) GO TO 3951
            END DO
3941        CONTINUE
            ATEMP12=ATEMP20
            IPARSEC=IPARSEC+1
            IF(IPARSEC.EQ.NPARSEC+1)THEN
              ATEMP30='secondary parameters'
              ATEMP25='NPARSEC'
              WRITE(ERRMSG,391) AALINE(1:MMLINE),TRIM(ATEMP30),
     +        TRIM(ATEMP25)
              CALL WRTERR
            END IF
            IF(IPARSEC.LE.NPARSEC)
     +      APAR(NPAR+NPARFILE+IPARSEC)=ATEMP20
3951        CONTINUE
            DO J=1,NPAR
              IF(ATEMP12.EQ.APAR(J))THEN
                WRITE(ERRMSG,3962) AALINE(1:MMLINE),
     +          TRIM(ATEMP12)
                CALL WRTERR
                GO TO 3981
              END IF
            END DO
3981        CONTINUE
            IF(IPARFILE.GT.0)THEN
              DO J=NPAR+1,NPAR+MIN(IPARFILE,NPARFILE)
                IF(ATEMP12.EQ.APAR(J))THEN
                  WRITE(ERRMSG,3966) AALINE(1:MMLINE),
     +            TRIM(ATEMP12)
                  CALL WRTERR
                  GO TO 3982
                END IF
              END DO
            END IF
3982        CONTINUE
            GO TO 7783
          END IF
          CALL LINSPL(IFAIL,2,LW,RW,CLINE)
          IF(IFAIL.NE.0) GO TO 9500
          IF(CLINE(LW(2):RW(2)).EQ.'file_parameter')THEN
            IPARFILE=IPARFILE+1
            IF(IPARFILE.EQ.NPARFILE+1)THEN
              ATEMP30='file-parameters'
              ATEMP25='NPARFILE'
              WRITE(ERRMSG,391) AALINE(1:MMLINE),TRIM(ATEMP30),
     +        TRIM(ATEMP25)
              CALL WRTERR
            END IF
            IF(RW(1)-LW(1)+1.GT.12)THEN
              WRITE(ERRMSG,820) AALINE(1:MMLINE),
     +        CLINE(LW(1):RW(1))
              CALL WRTERR
              IF(IPARFILE.LE.NPARFILE)
     +        APAR(NPAR+IPARFILE)=' '
            ELSE
              ATEMP12=CLINE(LW(1):RW(1))
              IF(IPARFILE.LE.NPARFILE)
     +        APAR(NPAR+IPARFILE)=ADJUSTL(ATEMP12)
              IF(IPARFILE.GT.1)THEN
                DO J=NPAR+1,NPAR+MIN(IPARFILE,NPARFILE)-1
                  IF(ATEMP12.EQ.APAR(J))THEN
                    WRITE(ERRMSG,396) AALINE(1:MMLINE),
     +              TRIM(ATEMP12)
                    CALL WRTERR
                    GO TO 3971
                  END IF
                END DO
3971            CONTINUE
              END IF
              DO J=1,NPAR
                IF(ATEMP12.EQ.APAR(J))THEN
                  WRITE(ERRMSG,3968) AALINE(1:MMLINE),
     +            TRIM(ATEMP12)
                  CALL WRTERR
                  GO TO 3973
                END IF
              END DO
3973          CONTINUE
              IF(IPARSEC.GT.0)THEN
                DO J=NPAR+NPARFILE+1,NPAR+NPARFILE+MIN(IPARSEC,NPARSEC)
                  IF(ATEMP12.EQ.APAR(J))THEN
                    WRITE(ERRMSG,3961) AALINE(1:MMLINE),
     +              TRIM(ATEMP12)
                    CALL WRTERR
                    GO TO 3974
                  END IF
                END DO
              END IF
3974          CONTINUE
            END IF
            GO TO 7783
          END IF
          IF(RW(1)-LW(1)+1.GT.12) THEN
            WRITE(ERRMSG,820) AALINE(1:MMLINE),CLINE(LW(1):RW(1))
            CALL WRTERR
            GO TO 1199
          END IF
          CALL WHICH1(IFAIL4,NPAR,J,APAR,CLINE(LW(1):RW(1)))
          IF(IFAIL4.NE.0) THEN
            WRITE(ERRMSG,1140) AALINE(1:MMLINE), CLINE(LW(1):RW(1))
1140        FORMAT(A,'unrecognized parameter name ("',A,'").')
            CALL WRTERR
            GO TO 1199
          END IF
          IF((ITRANS(J).GE.0).OR.(ITRANS(J).EQ.-1000000)) THEN
            WRITE(ERRMSG,1150) AALINE(1:MMLINE),CLINE(LW(1):RW(1))
1150        FORMAT(A,'parameter "',A,'" is not a tied parameter.')
            CALL WRTERR
            GO TO 1199
          END IF
          IF(PVAL(J).EQ.0.0D0)THEN
            WRITE(ERRMSG,1146) AALINE(1:MMLINE),CLINE(LW(1):RW(1))
1146        FORMAT(A,'tied parameter "',A,'" cannot have initial value ',
     +      'of zero.')
            CALL WRTERR
          END IF
          IF((ITRANS(J).LT.0).AND.(ITRANS(J).NE.-999998))THEN
            WRITE(ERRMSG,1145) AALINE(1:MMLINE),CLINE(LW(1):RW(1))
1145        FORMAT(A,'tied parameter "',A,'" linked to another ',
     +      'parameter more than once.')
            CALL WRTERR
            GO TO 1199
          END IF
          IF(RW(2)-LW(2)+1.GT.12) THEN
            WRITE(ERRMSG,820) AALINE(1:MMLINE),CLINE(LW(2):RW(2))
            CALL WRTERR
            GO TO 1199
          END IF
          CALL WHICH1(IFAIL4,NPAR,II,APAR,CLINE(LW(2):RW(2)))
          IF(IFAIL4.NE.0)THEN
            WRITE(ERRMSG,1140) AALINE(1:MMLINE),CLINE(LW(2):RW(2))
            CALL WRTERR
            GO TO 1199
          END IF
          IF(II.EQ.J) THEN
            WRITE(ERRMSG,1160) AALINE(1:MMLINE),
     +      APAR(II)(:NBLNK(APAR(II)))
1160        FORMAT(A,'parameter "',A,'" cannot be tied to itself.')
            CALL WRTERR
            ITRANS(J)=-II
            GO TO 1199
          END IF
          IF(ITRANS(II).LT.0)THEN
            WRITE(ERRMSG,1170) AALINE(1:MMLINE),
     +      APAR(J)(:NBLNK(APAR(J)))
1170        FORMAT(A,'parameter "',A,'" cannot be tied to fixed or ',
     +      'tied parameter.')
            CALL WRTERR
            ITRANS(J)=-II
            GO TO 1199
          END IF
          IF(PVAL(II).EQ.0.0D0)THEN
            WRITE(ERRMSG,1175) AALINE(1:MMLINE),
     +      APAR(II)(:NBLNK(APAR(II))),
     +      APAR(J)(:NBLNK(APAR(J)))
1175        FORMAT(A,'parent parameter "',A,'" of tied parameter "',A,
     +      '" cannot have initial value of zero.')
            CALL WRTERR
            ITRANS(J)=-II
            GO TO 1199
          END IF
          ITRANS(J)=-II
          GO TO 1200
1199      CONTINUE
1200      CONTINUE
12001     CONTINUE
        END IF

        IF(IREFOBS.GT.0)THEN
          IF(NUMCOM.GT.1)THEN
            DO I=1,NPAR
              IF(ITRANS(I).GE.0)THEN
                IF(DERCOM(I).EQ.1)THEN
C                  WRITE(ERRMSG,2010) TRIM(APAR(I))
C2010              FORMAT(' If observation re-referencing is active ',
C     +            'and NUMCOM is greater than 1, no parameter should ',
C     +            'have a DERCOM value of 1. This is violated for ',
C     +            'parameter "',a,'".')
C                  CALL WRTERR
                  GO TO 2020
                END IF
              END IF
            END DO
2020        CONTINUE
            DO I=1,NPAR
              IF(ITRANS(I).GE.0)THEN
                IF(DERCOM(I).GT.1) GO TO 2050
              END IF
            END DO
            WRITE(ERRMSG,2045)
2045        FORMAT(' If observation re-referencing is active and ',
     +      'NUMCOM is greater than 1, then at least one parameter ',
     +      'must have a DERCOM value greater than 1 in the ',
     +       '"parameter data" section of the PEST control file.')
            CALL WRTERR
2050        CONTINUE
          ELSE IF(NUMCOM.EQ.1)THEN
            IF(JACFILE.NE.0)THEN
              DO I=1,NPAR
                IF(ITRANS(I).GE.0)THEN
                  IF(DERCOM(I).EQ.1)GO TO 2070
                END IF
              END DO
              WRITE(ERRMSG,2065)
2065          FORMAT(' If observation de-referencing is active and ',
     +        'JACFILE is non-zero, then at least one parameter ',
     +        'must have a DERCOM value of 1 in the "parameter data"',
     +        ' section of the PEST control file.')
              CALL WRTERR
2070          CONTINUE
            END IF
          END IF
        END IF

        RETURN

9600    JFAIL=4
        RETURN
9700    JFAIL=5
        RETURN
9500    JFAIL=3
        RETURN
9800    JFAIL=1
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


        SUBROUTINE SVDACHEK(IFAIL,ILINE,IASECT,DOAUI,NUMCOM,JACFILE,
     +  CLINE,SVDA_MULBPA,SVDA_NLSCALADJ,SVDA_EXTSUPER,SVDA_SUPDERCALC,
     +  SVDA_PAR_EXCL,PESTMODE,IPPCOUNT)

C -- SUBROUTINE SVDACHEK CHECKS THE SVD-ASSIST SECTION OF A PEST CONTROL FILE

        IMPLICIT NONE

        INTEGER IFAIL,ILINE,IERR,MCASE,MMLINE,L,LW(5),RW(5),IASECT,
     +  NBLNK,NUMCOM,JACFILE,SVDA_MULBPA,JFAIL,SVDA_NLSCALADJ,
     +  SVDA_EXTSUPER,SVDA_SUPDERCALC,LL,N
        INTEGER SVDA_PAR_EXCL,PESTMODE
        INTEGER IPPCOUNT
        DOUBLE PRECISION RTEMP
        CHARACTER*5 DOAUI
        CHARACTER*10 ATEMP
        CHARACTER*100 CASFL
        CHARACTER*20 ALINE
        CHARACTER*200 AALINE,AFILE
        CHARACTER*2120 ERRMSG
        CHARACTER*(*) CLINE

        COMMON /IERR/IERR,MCASE
        COMMON /ERRMSG/ ERRMSG
        COMMON /CASFL/ CASFL
        COMMON /ALINE/ ALINE,AALINE
        COMMON /MMLINE/ MMLINE

        IFAIL=0
        LL=0

C        IF(IASECT.EQ.1)THEN
C          WRITE(ERRMSG,5)
C5         FORMAT(' A PEST control file must not contain both an ',
C     +    '"automatic user intervention" and an "svd assist" section.')
C          CALL WRTERR
C        ELSE
C          IF(DOAUI.EQ.'aui')THEN
C            WRITE(ERRMSG,6)
C6           FORMAT(' Automatic user intervention must not be active ',
C     +      'if an "svd assist" section is present in PEST control ',
C     +      'file.')
C            CALL WRTERR
C          END IF
C        END IF
7784    CONTINUE
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7784
#ifndef UNIX
        CALL LOWCAS(CLINE)
#endif
        CALL SHIFTL(CLINE)
        IF(CLINE.EQ.' ') THEN
          WRITE(ERRMSG,10)AALINE(1:MMLINE)
10        FORMAT(A,'name of PEST control file expected.')
          CALL WRTERR
          GO TO 100
        END IF
        CALL SPACESUB(CLINE)
        CALL LINSPL(IFAIL,1,LW,RW,CLINE)
        AFILE=CLINE(LW(1):RW(1))
        CALL REMCHAR(AFILE,CHAR(211))
        L=NBLNK(AFILE)
        IF(AFILE(1:1).EQ.'''')THEN
          IF(AFILE(L:L).NE.'''')THEN
            WRITE(ERRMSG,30) AALINE(1:MMLINE)
30          FORMAT(A,'unbalanced quotes on filename.')
            CALL WRTERR
          END IF
        ELSE IF(AFILE(1:1).EQ.'"')THEN
          IF(AFILE(L:L).NE.'"')THEN
            WRITE(ERRMSG,30) AALINE(1:MMLINE)
            CALL WRTERR
          END IF
        ELSE
          IF((AFILE(L:L).EQ.'''').OR.(AFILE(L:L).EQ.'"'))THEN
            WRITE(ERRMSG,30) AALINE(1:MMLINE)
            CALL WRTERR
          END IF
        END IF
        IF((AFILE(L:L).EQ.'''').OR.(AFILE(L:L).EQ.'"'))L=L-1
        IF(L.LT.5)THEN
          WRITE(ERRMSG,40) AALINE(1:MMLINE)
          CALL WRTERR
        ELSE
          IF(AFILE(L-3:L).NE.'.pst')THEN
            WRITE(ERRMSG,40) AALINE(1:MMLINE)
40          FORMAT(A,'name of PEST control file must end in ".pst"')
            CALL WRTERR
          END IF
        END IF

100     CONTINUE
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 100
#ifndef UNIX
        CALL LOWCAS(CLINE)
#endif
        CALL SHIFTL(CLINE)
        IF(CLINE.EQ.' ') THEN
          WRITE(ERRMSG,110)AALINE(1:MMLINE)
110       FORMAT(A,'name of Jacobian matrix file or super parameter ',
     +    'definition file expected.')
          CALL WRTERR
          GO TO 200
        END IF
        IF(CLINE(1:1).EQ.'*')THEN
          WRITE(ERRMSG,110) AALINE(1:MMLINE)
          CALL WRTERR
          IFAIL=3
          RETURN
        END IF
        CALL SPACESUB(CLINE)
        CALL LINSPL(IFAIL,1,LW,RW,CLINE)
        AFILE=CLINE(LW(1):RW(1))
        CALL REMCHAR(AFILE,CHAR(211))
        L=NBLNK(AFILE)
        IF(AFILE(1:1).EQ.'''')THEN
          IF(AFILE(L:L).NE.'''')THEN
            WRITE(ERRMSG,30) AALINE(1:MMLINE)
            CALL WRTERR
          END IF
        ELSE IF(AFILE(1:1).EQ.'"')THEN
          IF(AFILE(L:L).NE.'"')THEN
            WRITE(ERRMSG,30) AALINE(1:MMLINE)
            CALL WRTERR
          END IF
        ELSE
          IF((AFILE(L:L).EQ.'''').OR.(AFILE(L:L).EQ.'"'))THEN
            WRITE(ERRMSG,30) AALINE(1:MMLINE)
            CALL WRTERR
          END IF
        END IF
        IF((AFILE(L:L).EQ.'''').OR.(AFILE(L:L).EQ.'"'))L=L-1
        LL=L
200     CONTINUE
        IF(NUMCOM.GT.1)THEN
          WRITE(ERRMSG,210)
210       FORMAT(' SVD assist functionality must not be activated '
     +    'if number of model command lines is greater ',
     +    'than zero.')
          CALL WRTERR
        END IF
C        IF(JACFILE.GT.0)THEN
C          WRITE(ERRMSG,211)
C211       FORMAT(' SVD assist functionality must not be activated '
C     +    'if derivatives are supplied externally by model.')
C          CALL WRTERR
C        END IF
        IF(JACFILE.EQ.2)THEN
          WRITE(ERRMSG,212)
212       FORMAT(' As presently programmed, JACFILE must not be set ',
     +    'to 2 (i.e. external derivatives must not be supplied in ',
     +    'JUPITER format) if SVD-assist is activated.')
          CALL WRTERR
        END IF

300     CONTINUE
        SVDA_MULBPA=-999
        SVDA_NLSCALADJ=-999
        SVDA_EXTSUPER=-999
        SVDA_SUPDERCALC=-999
        SVDA_PAR_EXCL=-999
7777    CONTINUE
        ILINE=ILINE+1
        CALL WRTINT(ATEMP,ILINE)
        ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
        AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(30,'(A)',ERR=9600,END=9700) CLINE
        CALL PPROC(CLINE,IPPCOUNT)
        IF(CLINE.EQ.' ') GO TO 7777
        CALL LOWCAS(CLINE)
        CALL SHIFTL(CLINE)
        IF(CLINE(1:1).EQ.'*') GO TO 9500
        IF(INDEX(CLINE,'group').NE.0) GO TO 9500
        IF(INDEX(CLINE,'param').NE.0) GO TO 9500
        IF(CLINE.EQ.' ') RETURN
        CALL LINSPL(JFAIL,1,LW,RW,CLINE)
        CALL NUMRD(JFAIL,0,LW(1),RW(1),SVDA_MULBPA,RTEMP,CLINE)
        IF(JFAIL.NE.0)THEN
          WRITE(ERRMSG,310) AALINE(1:MMLINE)
310       FORMAT(A,'cannot read value of SVDA_MULBPA variable.')
          CALL WRTERR
          SVDA_MULBPA=-998
        ELSE
          IF((SVDA_MULBPA.NE.0).AND.(SVDA_MULBPA.NE.1))THEN
            WRITE(ERRMSG,320) AALINE(1:MMLINE)
320         FORMAT(A,'value of SVDA_MULBPA variable must be ',
     +      'zero or one.')
            CALL WRTERR
            SVDA_MULBPA=-998
          END IF
        END IF
        CALL LINSPL(JFAIL,2,LW,RW,CLINE)
        IF(JFAIL.EQ.0)THEN
          CALL NUMRD(JFAIL,0,LW(2),RW(2),SVDA_NLSCALADJ,RTEMP,CLINE)
          IF(JFAIL.NE.0)THEN
            WRITE(ERRMSG,330) AALINE(1:MMLINE)
330         FORMAT(A,'cannot read value of SVDA_SCALADJ variable.')
            CALL WRTERR
            SVDA_NLSCALADJ=-998
          ELSE
            IF(ABS(SVDA_NLSCALADJ).GT.4)THEN
              WRITE(ERRMSG,340) AALINE(1:MMLINE)
340           FORMAT(A,'value of SVDA_SCALADJ variable must be ',
     +        '-4, -3, -2, -1, 0, 1, 2, 3, or 4.')
              CALL WRTERR
              SVDA_NLSCALADJ=-998
            END IF
          END IF
        END IF
        CALL LINSPL(JFAIL,3,LW,RW,CLINE)
        IF(JFAIL.EQ.0)THEN
          CALL NUMRD(JFAIL,0,LW(3),RW(3),SVDA_EXTSUPER,RTEMP,CLINE)
          IF(JFAIL.NE.0)THEN
            WRITE(ERRMSG,350) AALINE(1:MMLINE)
350         FORMAT(A,'cannot read value of SVDA_EXTSUPER variable.')
            CALL WRTERR
            SVDA_EXTSUPER=-998
          ELSE
            IF((SVDA_EXTSUPER.NE.0).AND.(SVDA_EXTSUPER.NE.1)
     +      .AND.(ABS(SVDA_EXTSUPER).NE.2).AND.
     +       (SVDA_EXTSUPER.NE.3))THEN
              WRITE(ERRMSG,360) AALINE(1:MMLINE)
360           FORMAT(A,'value for SVDA_EXTSUPER must be 0, 1, 2, ',
     +        '-2 or 3.')
              CALL WRTERR
              SVDA_EXTSUPER=-998
            END IF
            IF((SVDA_EXTSUPER.EQ.1).AND.(JACFILE.NE.0))THEN
              WRITE(ERRMSG,361) AALINE(1:MMLINE)
361           FORMAT(A,'if JACFILE is non-zero (i.e. derivatives ',
     +        'are calculated externally) then SVDA_EXTSUPER must ',
     +        'not be set to 1 (i.e. super parameters must not be ',
     +        'user-supplied.)')
              CALL WRTERR
            END IF
          END IF
        END IF

        CALL LINSPL(JFAIL,4,LW,RW,CLINE)
        IF(JFAIL.EQ.0)THEN
          CALL NUMRD(JFAIL,0,LW(4),RW(4),SVDA_SUPDERCALC,RTEMP,CLINE)
          IF(JFAIL.NE.0)THEN
            WRITE(ERRMSG,450) AALINE(1:MMLINE)
450         FORMAT(A,'cannot read value of SVDA_SUPDERCALC variable.')
            CALL WRTERR
            SVDA_SUPDERCALC=-998
          ELSE
            IF((SVDA_SUPDERCALC.NE.0).AND.(SVDA_SUPDERCALC.NE.1))
     +      THEN
              WRITE(ERRMSG,460) AALINE(1:MMLINE)
460           FORMAT(A,'value for SVDA_SUPDERCALC must be 0 or 1.')
              CALL WRTERR
              SVDA_SUPDERCALC=-998
            END IF
          END IF
          IF((SVDA_SUPDERCALC.EQ.1).AND.(SVDA_EXTSUPER.EQ.1))THEN
            WRITE(ERRMSG,470) AALINE(1:MMLINE)
470         FORMAT(A,'if SVDA_EXTSUPER is set to 1 then ',
     +      'SVDA_SUPDERCALC must not be set to 1.')
            CALL WRTERR
            SVDA_SUPDERCALC=-998
          END IF
        END IF

        CALL LINSPL(JFAIL,5,LW,RW,CLINE)
        IF(JFAIL.EQ.0)THEN
          CALL NUMRD(JFAIL,0,LW(5),RW(5),SVDA_PAR_EXCL,RTEMP,CLINE)
          IF(JFAIL.NE.0)THEN
            WRITE(ERRMSG,650) AALINE(1:MMLINE)
650         FORMAT(A,'cannot read value of SVDA_PAR_EXCL variable.')
            CALL WRTERR
            SVDA_PAR_EXCL=-998
          ELSE
            IF((SVDA_PAR_EXCL.NE.0).AND.(ABS(SVDA_PAR_EXCL).NE.1))
     +      THEN
              WRITE(ERRMSG,660) AALINE(1:MMLINE)
660           FORMAT(A,'value for SVDA_PAR_EXCL must be 0, 1 or -1.')
              CALL WRTERR
              SVDA_PAR_EXCL=-998
            END IF
          END IF
          IF((ABS(SVDA_PAR_EXCL).EQ.1).AND.(SVDA_EXTSUPER.EQ.1))THEN
            WRITE(ERRMSG,670) AALINE(1:MMLINE)
670         FORMAT(A,'if SVDA_EXTSUPER is set to 1 then ',
     +      'SVDA_PAR_EXCL must not be set to 1 or -1.')
            CALL WRTERR
            SVDA_PAR_EXCL=-998
          END IF
          IF(PESTMODE.NE.4)THEN
            IF(ABS(SVDA_PAR_EXCL).EQ.1)THEN
              WRITE(ERRMSG,690) AALINE(1:MMLINE)
690           FORMAT(A,' SVDA_PAR_EXCL should not be set to ',
     +        '1 or -1 unless PESTMODE is set to "pareto".')
              CALL WRTERR
              SVDA_PAR_EXCL=-998
            END IF
          END IF
        END IF

        IF(SVDA_EXTSUPER.LT.-2) RETURN
        IF(SVDA_EXTSUPER.NE.1) GO TO 9500
        L=LEN_TRIM(AFILE)
        N=L-3
        IF(N.LT.1)N=1
        IF(AFILE(N:L).EQ.'.jco')THEN
          CALL WRTINT(ATEMP,ILINE-1)
          ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
          AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
          MMLINE=NBLNK(AALINE)+1
          WRITE(ERRMSG,240) AALINE(1:MMLINE)
240       FORMAT(A,'name of super parameter file must not end in ',
     +    '".jco"; this extension represents a Jacobian matrix ',
     +    'file.')
          CALL WRTERR
          CALL WRTINT(ATEMP,ILINE)
          ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
          AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
          MMLINE=NBLNK(AALINE)+1
        END IF

        RETURN

9500    CONTINUE
        IF(LL.NE.0)THEN
          CALL WRTINT(ATEMP,ILINE-1)
          ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
          AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
          MMLINE=NBLNK(AALINE)+1
          IF(LL.LT.5)THEN
            WRITE(ERRMSG,140) AALINE(1:MMLINE)
            CALL WRTERR
          ELSE
            IF(AFILE(LL-3:LL).NE.'.jco')THEN
              WRITE(ERRMSG,140) AALINE(1:MMLINE)
140           FORMAT(A,'name of Jacobian matrix file must end ',
     +        'in ".jco"')
              CALL WRTERR
            END IF
          END IF
          CALL WRTINT(ATEMP,ILINE)
          ALINE=' Line '//ATEMP(:NBLNK(ATEMP))//' of file '
          AALINE=ALINE(:NBLNK(ALINE))//' '//CASFL(1:MCASE)//': '
          MMLINE=NBLNK(AALINE)+1
        END IF
        RETURN

9600    IFAIL=1
        GO TO 9999
9700    IFAIL=2
        GO TO 9999
9999    RETURN
        END




        SUBROUTINE DUP_CHECK(NOBS,AOBS,TEMPOBS)

        IMPLICIT NONE
        INTEGER, INTENT(IN)             :: NOBS
        CHARACTER(LEN=*), INTENT(IN)    :: AOBS(NOBS)
        CHARACTER(LEN=*), INTENT(INOUT) :: TEMPOBS(NOBS)

        CHARACTER*20 W
        INTEGER I, J
        CHARACTER*2120 ERRMSG

        COMMON /ERRMSG/ ERRMSG

        DO  I=1, NOBS
          TEMPOBS(I) =  AOBS(I)
        END DO
C Use insertion Sort to Sort TEMPOBS
        DO I = 2, NOBS
           W = TEMPOBS(I)
          DO J = I,2,-1
             IF ( W >= TEMPOBS(J-1) ) EXIT
             TEMPOBS(J) = TEMPOBS(J-1)
          END DO
          TEMPOBS(J) = W
        END DO
        DO  I=2, NOBS
          IF(TEMPOBS(I).EQ.TEMPOBS(I-1))THEN
            WRITE(ERRMSG,1325) TRIM(TEMPOBS(I))
1325        FORMAT(' Observation name "',A,'" used more than once.')
            CALL WRTERR
          END IF
        END DO

        RETURN
        END



       subroutine get_absparmax(ifail,maxabsparmax,absparmax,cline)

       implicit none

       integer, intent(out)          :: ifail
       integer, intent(in)           :: maxabsparmax
       double precision, intent(out) :: absparmax(maxabsparmax)
       character*(*), intent(inout)  :: cline

       integer                   :: n,m,absind,i
       double precision          :: dtemp
       character*20              :: atemp

       integer                   :: ierr,mcase,mmline
       character*100             :: casfl
       character*2120            :: errmsg
       character*20              :: aline
       character*200             :: aaline

       common /ierr/ierr,mcase
       common /errmsg/ errmsg
       common /casfl/ casfl
       common /aline/ aline,aaline
       common /mmline/ mmline

       ifail=0
       do i=1,MAXABSPARMAX
         absparmax(i)=-1.1e35
       end do

10     continue
       n=index(cline,'absparmax')
       if(n.eq.0) go to 9990

       cline(n:)=adjustl(cline(n+9:))
       if(cline(n:n).ne.'(') go to 9000
       m=index(cline(n:),')')
       if(m.eq.0) go to 9000
       if(m.eq.2) go to 9000
       atemp=cline(n+1:n+m-2)
       call intread(ifail,atemp,absind)
       if(ifail.ne.0) go to 9000
       if((absind.lt.1).or.(absind.gt.MAXABSPARMAX)) then
         call wrtint(atemp,absind)
         write(errmsg,11) aline(1:16),casfl(1:mcase),trim(atemp)
11       format(a,a,': ABSPARMAX index "',a,'" out of range.')
         call wrterr
       else
         if(absparmax(absind).gt.-1.0e35) then
           call wrtint(atemp,absind)
           write(errmsg,12) aline(1:16),casfl(1:mcase),trim(atemp)
12         format(a,a,': ABSPARMAX index "',a,'" referenced more ',
     +     'than once.')
           call wrterr
         end if
       end if
       cline(n:)=adjustl(cline(n+m:))
       m=index(cline(n:),'=')
       if(m.eq.0) go to 9000
       if(m.gt.1)then
         if(cline(1:m-1).ne.' ') go to 9000
       end if
       cline(n:)=adjustl(cline(n+m:))
       m=index(cline(n:),' ')
       if(m.le.1) then
         call wrtint(atemp,absind)
         write(errmsg,51) aline(1:16),casfl(1:mcase),trim(atemp)
51       format(a,a,': cannot read value of ABSPARMAX variable ',
     +   'with index "',a,'".')
         call wrterr
       else
         atemp=cline(n:n+m-1)
         call drealread(ifail,atemp,dtemp)
         if(ifail.ne.0) then
           call wrtint(atemp,absind)
           write(errmsg,51) aline(1:16),casfl(1:mcase),trim(atemp)
           call wrterr
         else
          if(dtemp.le.0.0d0) then
            write(errmsg,13) aline(1:16),casfl(1:mcase),trim(atemp)
13          format(a,a,': ABSPARMAX variable value "',a,
     +      '" must not be negative.')
            call wrterr
          end if
        end if
       end if
       if((absind.ge.1).and.(absind.le.MAXABSPARMAX))then
         absparmax(absind)=dtemp
       end if
       cline(n:)=cline(n+m:)
       go to 10

9000   write(errmsg,9010) aline(1:16),casfl(1:mcase)
9010   format(a,a,': incorrect specification of ABSPARMAX data.')
       call wrterr
       go to 9890

9890   continue
       ifail=1

9990   continue

       return

       end



        subroutine intread(ifail,cline,itemp)

C -- Subroutine INTREAD reads a real number from a string.

        integer ifail
        integer itemp
        character*6 afmt
        character*(*) cline

        ifail=0
        if(cline.eq.' ') go to 100
        afmt='(i   )'
        write(afmt(3:5),'(i3)') len(cline)
        read(cline,afmt,err=100) itemp
        return

100     ifail=1
        return
        end




        SUBROUTINE DREALREAD(IFAIL,CLINE,RTEMP)

C -- Subroutine DREALREAD reads a real number from a string.

        INTEGER IFAIL
        double precision RTEMP
        CHARACTER*8 AFMT
        CHARACTER*(*) CLINE

        IFAIL=0
        IF(CLINE.EQ.' ') GO TO 100
        AFMT='(F   .0)'
        WRITE(AFMT(3:5),'(I3)') LEN(CLINE)
        READ(CLINE,AFMT,ERR=100) RTEMP
        RETURN

100     IFAIL=1
        RETURN
        END



        SUBROUTINE PC_READ_STOPHOURS(IFAIL,NALINE,ITYPE,STOPHOURS,CLINE)

        IMPLICIT NONE

        INTEGER IFAIL
        INTEGER NALINE
        INTEGER ITYPE
        DOUBLE PRECISION STOPHOURS
        CHARACTER*(*) CLINE

        INTEGER IERR,MCASE
        CHARACTER*2120 ERRMSG
        CHARACTER*100  CASFL
        CHARACTER*20 ALINE
        CHARACTER*200 AALINE

        INTEGER NN,I
        CHARACTER*13 AVAR,AVARL
        CHARACTER*25 ATEMP25
        CHARACTER*1000 DLINE

        COMMON /IERR/IERR,MCASE
        COMMON /ERRMSG/ERRMSG
        COMMON /CASFL/ CASFL
        COMMON /ALINE/ ALINE,AALINE

        IFAIL=0
        IF(ITYPE.EQ.1)THEN
          AVAR='HARDSTOPHOURS'
        ELSE
          AVAR='SOFTSTOPHOURS'
        END IF
        AVARL=AVAR
        CALL LOWCAS(AVARL)
        DLINE=CLINE
        CALL LOWCAS(DLINE)
        NN = INDEX(DLINE,TRIM(AVARL))
        IF(NN.EQ.0)THEN
          STOPHOURS=-1.1D100
          RETURN
        END IF

        ATEMP25=ADJUSTL(DLINE(NN+13:))
        IF(ATEMP25(1:1).NE.'=')THEN
          WRITE(ERRMSG,10) ALINE(1:NALINE),CASFL(1:MCASE),TRIM(AVAR)
10        FORMAT(A,A,': cannot read value of ',A,' variable.')
          CALL WRTERR
          IFAIL=1
          GO TO 9900
        END IF
        ATEMP25=ADJUSTL(ATEMP25(2:))
        NN=INDEX(ATEMP25,' ')
        IF((NN.EQ.0).OR.(NN.EQ.1))THEN
          WRITE(ERRMSG,10) ALINE(1:NALINE),CASFL(1:MCASE),TRIM(AVAR)
          CALL WRTERR
          IFAIL=1
          GO TO 9900
        END IF
        ATEMP25(NN:)=' '
        CALL DREALREAD(IFAIL,ATEMP25,STOPHOURS)
        IF(IFAIL.NE.0) THEN
          WRITE(ERRMSG,10) ALINE(1:NALINE),CASFL(1:MCASE),TRIM(AVAR)
          CALL WRTERR
          IFAIL=1
          GO TO 9900
        END IF
        IF(STOPHOURS.LE.0.0D0)THEN
          WRITE(ERRMSG,20) ALINE(1:NALINE),CASFL(1:MCASE),TRIM(AVAR)
20        FORMAT(A,A,': ',A,' must be greater than zero.')
          CALL WRTERR
          IFAIL=1
        END IF
        NN=INDEX(DLINE,TRIM(AVARL))
        CLINE(NN:)=CLINE(NN+13:)
        DO I=NN,LEN(CLINE)
          IF(CLINE(I:I).EQ.'=')THEN
            CLINE(NN:)=CLINE(I+1:)
            GO TO 30
          END IF
        END DO
30      CONTINUE
        CLINE(NN:)=ADJUSTL(CLINE(NN:))
        DO I=NN,LEN(CLINE)
          IF(CLINE(I:I).EQ.' ')THEN
            CLINE(NN:I-1)=' '
            GO TO 40
          END IF
        END DO
40      CONTINUE
        DLINE=CLINE
        CALL LOWCAS(DLINE)
        NN=INDEX(DLINE,TRIM(AVARL))
        IF(NN.NE.0)THEN
          WRITE(ERRMSG,50) ALINE(1:NALINE),CASFL(1:MCASE),TRIM(AVAR)
50        FORMAT(A,A,': ',A,' variable appears twice.')
          CALL WRTERR
          IFAIL=1
        END IF

9900    CONTINUE
        RETURN
        END




        SUBROUTINE READ_MRUN_VAR_PC(IFAIL,DVAL,VARTEXT,UPVARTEXT,
     +  CLINE,ITYPE,BFILE,NALINE)

        IMPLICIT NONE
        INTEGER IFAIL,ITYPE
        INTEGER NALINE
        DOUBLE PRECISION DVAL
        CHARACTER*1 AA
        CHARACTER*(*) VARTEXT,UPVARTEXT
        CHARACTER*(*) CLINE,BFILE

        INTEGER  NN,NNLT,I
        CHARACTER*25 ATEMP25
        CHARACTER*256 ATEMP256
        CHARACTER*2000 DLINE

        INTEGER IERR,MCASE
        CHARACTER*20 ALINE
        CHARACTER*200 AALINE
        CHARACTER*100 CASFL
        CHARACTER*2120 ERRMSG

        COMMON /IERR/IERR,MCASE
        COMMON /ERRMSG/ERRMSG
        COMMON /CASFL/ CASFL
        COMMON /ALINE/ ALINE,AALINE

        IFAIL=0
        DLINE=CLINE
        CALL LOWCAS(DLINE)
        AA=' '
        IF(ITYPE.EQ.2)BFILE=' '
        NNLT=LEN_TRIM(VARTEXT)
        NN = INDEX(DLINE,TRIM(VARTEXT))
        IF(NN.EQ.0) THEN
          GO TO 9900
        ELSE
          ATEMP256=ADJUSTL(CLINE(NN+NNLT:))
          IF(ATEMP256(1:1).NE.'=')THEN
            WRITE(ERRMSG,2105) ALINE(1:NALINE),CASFL(1:MCASE),
     +      TRIM(UPVARTEXT)
2105        FORMAT(A,A,': cannot read value of ',A,' variable.')
            GO TO 9890
          END IF
          ATEMP256=ADJUSTL(ATEMP256(2:))
          NN=INDEX(ATEMP256,' ')
          IF((NN.EQ.0).OR.(NN.EQ.1))THEN
            WRITE(ERRMSG,2105) ALINE(1:NALINE),CASFL(1:MCASE),
     +      TRIM(UPVARTEXT)
            GO TO 9890
          END IF
          IF(ITYPE.EQ.1)THEN
            ATEMP25=ATEMP256
            ATEMP25(NN:)=' '
            CALL DREALRD1(IFAIL,ATEMP25,DVAL)
            IF(IFAIL.NE.0) THEN
              WRITE(ERRMSG,2105) ALINE(1:NALINE),CASFL(1:MCASE),
     +        TRIM(UPVARTEXT)
              GO TO 9890
            END IF
          ELSE
            AA=' '
            IF(ATEMP256(1:1).EQ.'''')THEN
              AA=''''
            ELSE IF(ATEMP256(1:1).EQ.'"')THEN
              AA='"'
            END IF
            IF(AA.NE.' ')THEN
              NN=INDEX(ATEMP256(2:),AA)
              IF(NN.EQ.0)THEN
                WRITE(ERRMSG,2106) ALINE(1:NALINE),CASFL(1:MCASE),
     +          TRIM(UPVARTEXT)
2106            FORMAT(A,A,': cannot read ',a,' filename.')
                GO TO 9890
              END IF
              ATEMP256(NN+1:)=' '
              BFILE=ATEMP256(2:)
            ELSE
              BFILE=ADJUSTL(ATEMP256(1:NN-1))
            END IF
            IF(BFILE.EQ.' ')THEN
              WRITE(ERRMSG,2106) ALINE(1:NALINE),CASFL(1:MCASE),
     +        TRIM(UPVARTEXT)
              GO TO 9890
            ELSE
              NN=LEN_TRIM(BFILE)
              IF((BFILE(NN:NN).EQ.'''').OR.(BFILE(NN:NN).EQ.'"'))THEN
                WRITE(ERRMSG,2106) ALINE(1:NALINE),CASFL(1:MCASE),
     +          TRIM(UPVARTEXT)
                GO TO 9890
              END IF
            END IF
          END IF
          NN=INDEX(DLINE,TRIM(VARTEXT))
          DLINE(NN:)=DLINE(NN+NNLT:)
          DO I=NN,LEN(DLINE)
            IF(DLINE(I:I).EQ.'=')THEN
              DLINE(NN:)=DLINE(I+1:)
              GO TO 21061
            END IF
          END DO
21061     CONTINUE
          DLINE(NN:)=ADJUSTL(DLINE(NN:))
          IF(AA.NE.' ')DLINE(NN:)=ADJUSTL(DLINE(NN+1:))
          DO I=NN,LEN(DLINE)
            IF(DLINE(I:I).EQ.AA)THEN
              IF(AA.NE.' ')DLINE(I:I)=' '
              DLINE(NN:I-1)=' '
              GO TO 2108
            END IF
          END DO
2108      CONTINUE
          NN=INDEX(DLINE,TRIM(VARTEXT))
          IF(NN.NE.0)THEN
            WRITE(ERRMSG,2107)  ALINE(1:NALINE),CASFL(1:MCASE),
     +      TRIM(UPVARTEXT)
2107        FORMAT(A,A,': ',A,' variable appears twice on this line.')
            GO TO 9890
          END IF
        END IF
        GO TO 9900

9890    CONTINUE
        CALL WRTERR
        IFAIL=1

9900    CONTINUE
        CLINE=DLINE
        RETURN
        END



        SUBROUTINE DREALRD1(IFAIL,CLINE,RTEMP)

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




        SUBROUTINE PPROC(CLINE,IPPCOUNT)

        INTEGER IPPCOUNT
        CHARACTER*(*) CLINE
        LOGICAL SKIPLINE1,LTEMP

        LTEMP=SKIPLINE1(CLINE,IPPCOUNT)
        IF(LTEMP) CLINE=' '

        RETURN
        END



       logical function skipline1(cline,ippcount)

       implicit none
       character*(*) cline
       integer ippcount
       integer nn,ll,icount,jcount,i

       skipline1=.FALSE.
       if(cline.eq.' ')then
         skipline1=.TRUE.
         go to 200
       end if
       cline=adjustl(cline)
       if(cline(1:2).eq.'++') then
         skipline1=.TRUE.
         ippcount=ippcount+1
         go to 200
       end if
       nn=index(cline,'#')
       if(nn.eq.0)then
         go to 200
       else if(nn.eq.1)then
         skipline1=.TRUE.
         ippcount=ippcount+1
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
                 skipline1=.TRUE.
               else
                 skipline1=.FALSE.
               end if
               ippcount=ippcount+1
               go to 200
             end if
           end if
         end if
       end do

200    continue
       return
       end

