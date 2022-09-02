	PROGRAM PARREP_RRF

C -- PROGRAM PARREP_RRF REPLACES PARAMETER DATA IN A PEST CONTROL FILE WITH THOSE
C    IN A RUN RESULTS FILE.

	IMPLICIT NONE

        LOGICAL SKIPLINE
        INTEGER IFAIL,IERR,ILINE,IIPAR,I
        INTEGER INEEDED,IPASS,NPAR,NOBS,NPARGP,NOBSGP,NPRIOR,IPAR,JPAR
        INTEGER NNPAR,NNOBS,IWRITE,IINDEX,IPINDEX,IOBS
        INTEGER NEQQ
        INTEGER LW(20),RW(20)
        DOUBLE PRECISION RVAL,RSCALE,ROFFSET
        DOUBLE PRECISION DTEMP
        CHARACTER*256 PESTFILE1,PESTFILE2,RRFFILE
        CHARACTER*256 PARSOURCE
        CHARACTER*12 ATRANS,AAPAR1,AAPAR2
        CHARACTER*12 AVERSION
        CHARACTER*10 ALINE,ATYPE,ANUM
        CHARACTER*20 AAOBS
        CHARACTER*30 ASECTION
        CHARACTER*500 CLINE,DLINE
        CHARACTER*1500 ERRMSG

        INTEGER ITRANS(:)
	DOUBLE PRECISION VALUE(:),PVAL(:),NEWPVAL(:)
	CHARACTER*12 APAR(:),AAPAR(:)

	ALLOCATABLE::VALUE,APAR,PVAL,NEWPVAL,ITRANS,
     +  AAPAR

        INCLUDE 'version.inc'
	WRITE(6,10) TRIM(AVERSION)
10	FORMAT(' PARREP_RRF Version ',A,'. Watermark Numerical ',
     +  'Computing.')
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
	RRFFILE=CLINE(LW(1):RW(1))
	PESTFILE1=CLINE(LW(3):RW(3))
	PESTFILE2=CLINE(LW(4):RW(4))
        CALL REMCHAR(RRFFILE,CHAR(211))
        CALL REMCHAR(PESTFILE1,CHAR(211))
        CALL REMCHAR(PESTFILE2,CHAR(211))
#ifdef CAPFILE
        CALL UPCAS(PARFILE)
        CALL UPCAS(PESTFILE1)
        CALL UPCAS(PESTFILE2)
#endif

	IF(PESTFILE1.EQ.PESTFILE2)THEN
	  WRITE(errmsg,40)
40	  FORMAT('Input and output PEST control files cannot have ',
     +    'same name.')
	  GO TO 9890
	END IF

	CALL INTREAD(IFAIL,CLINE(LW(2):RW(2)),IPINDEX)
	IF(IFAIL.NE.0) GO TO 9000
	IF(IPINDEX.LE.0)THEN
	  WRITE(ERRMSG,41)
41	  FORMAT('The parameter set index provided in the PARREP_RRF ',
     +    'command line must be positive.')
          GO TO 9890
        END IF

C -- INITIALISATION

        INEEDED=0
        IPASS=1

	OPEN(UNIT=10,FILE=RRFFILE,STATUS='OLD',IOSTAT=IERR)
	IF(IERR.NE.0)THEN
	  WRITE(ERRMSG,50) TRIM(RRFFILE)
50	  FORMAT('Cannot open run results file ',A,'.')
	  GO TO 9890
	END IF
	OPEN(UNIT=11,FILE=PESTFILE1,STATUS='OLD',IOSTAT=IERR)
	IF(IERR.NE.0) THEN
	  WRITE(ERRMSG,60) TRIM(PESTFILE1)
60	  FORMAT('Cannot open PEST control file ',A,'.')
	  GO TO 9890
	END IF
	OPEN(UNIT=20,FILE=PESTFILE2,ACTION='WRITE',ERR=9300)

c -- The run results file is read.

	WRITE(6,120) TRIM(RRFFILE)
120	FORMAT(' Reading run results file ',A,' ----->')
        iline=0
        asection='* case dimensions'
        iline=iline+1
        read(10,'(a)',err=9500,end=9550) cline
        call lowcas(cline)
        if(index(cline,trim(asection)).eq.0) then
          call wrtint(aline,iline)
          write(errmsg,1153) trim(asection),trim(aline),trim(rrffile)
1153      format('"',a,'" section header expected at line ',
     +    a,' of file ',a,'.')
          go to 9890
        end if
        iline=iline+1
        read(10,*,err=9600,end=9600) nnpar,nnobs
        atype='NPAR'
        if(nnpar.le.0)then
          call wrtint(aline,iline)
          write(errmsg,1160) trim(atype),trim(aline),trim(rrffile)
1160      format(a,' must be 1 or greater at line ',a,' of file ',
     +    a,'.')
          go to 9890
        end if
        if(nnobs.le.0)then
          atype='NOBS'
          call wrtint(aline,iline)
          write(errmsg,1160) trim(atype),trim(aline),trim(rrffile)
          go to 9890
        end if

! -- Memory is allocated.

        allocate(apar(nnpar),value(nnpar),stat=ierr)
        if(ierr.ne.0) go to 9200

! -- The "* parameter names" section of the header is now read.

        asection='* parameter names'
        iline=iline+1
        read(10,'(a)',err=9500,end=9500)cline
        if(index(cline,trim(asection)).eq.0)then
          call wrtint(aline,iline)
          write(errmsg,1170) trim(asection),trim(aline),trim(rrffile)
1170      format('"',a,'" section expected at line ',a,
     +    ' of file ',a,'.')
          go to 9890
        end if
        do ipar=1,nnpar
          iline=iline+1
          read(10,*,err=9600,end=9600) apar(ipar)
          call lowcas(apar(ipar))
        end do

        asection='* observation names'
        iline=iline+1
        read(10,'(a)',err=9500,end=9500)cline
        if(index(cline,trim(asection)).eq.0)then
          call wrtint(aline,iline)
          write(errmsg,1170) trim(asection),trim(aline),trim(rrffile)
          go to 9890
        end if
        do iobs=1,nnobs
          iline=iline+1
          read(10,*,err=9600,end=9600) aaobs
        end do

C -- We now look for the parameter set index.

        iwrite=0
        do
          asection='* parameter set index'
1400      continue
          iline=iline+1
          read(10,'(a)',end=1700) cline
          if(cline.eq.' ') go to 1400
          call lowcas(cline)
          if(index(cline,trim(asection)).eq.0) then
            call wrtint(aline,iline)
            go to 9850
          end if
          iline=iline+1
          read(10,*,err=9650,end=1700) iindex
          asection='* parameter values source'
1405      continue
          iline=iline+1
          read(10,'(a)',end=1700) cline
          if(cline.eq.' ') go to 1405
          call lowcas(cline)
          if(index(cline,trim(asection)).eq.0) then
            call wrtint(aline,iline)
            go to 9850
          end if
          iline=iline+1
          read(10,'(a)',end=1700) parsource
          asection='* parameter values'
1407      continue
          iline=iline+1
          read(10,'(a)',end=1700) cline
          if(cline.eq.' ') go to 1407
          call lowcas(cline)
          if(index(cline,trim(asection)).eq.0) then
            call wrtint(aline,iline)
            go to 9850
          end if
          do ipar=1,nnpar
            iline=iline+1
            read(10,*,err=9700,end=1700) value(ipar)
          end do
          asection='* model output values'
1406      continue
          iline=iline+1
          read(10,'(a)',end=1700) cline
          if(cline.eq.' ') go to 1406
          call lowcas(cline)
          if(index(cline,trim(asection)).eq.0) then
            call wrtint(aline,iline)
            go to 9850
          end if
          do iobs=1,nnobs
            iline=iline+1
            read(10,*,err=9700,end=1700) dtemp
          end do
          if(ipindex.eq.iindex)then
            iwrite=1
            go to 1700
          end if

        end do

1700    continue
        close(unit=10,iostat=ierr)
        if(iwrite.eq.0)then
          call wrtint(anum,ipindex)
          write(errmsg,1710) trim(anum),trim(rrffile)
1710      format('Cannot find parameter set index ',a,' in file ',a,'.')
          go to 9890
        end if

        write(6,1718) trim(rrffile)
1718    format(' - file ',a,' read ok.')

! -- Parameter values are now transferred to the new PEST control file.

	WRITE(6,450) TRIM(PESTFILE1),TRIM(PESTFILE2)
450	FORMAT(/,' Reading file ',A,' and writing file ',A,' ----->')

451     CONTINUE
	IIPAR=0
	ILINE=1
	READ(11,'(A)',END=9400,ERR=9400) CLINE
	CLINE=ADJUSTL(CLINE)
	IF(CLINE(1:4).NE.'pcf ')THEN
	  WRITE(ERRMSG,460) TRIM(PESTFILE1)
460	  FORMAT('First line of file ',A,' should be "pcf".')
	  GO TO 9890
	END IF
        IF(IPASS.EQ.2)THEN
          WRITE(20,'(A)',ERR=9300) TRIM(CLINE)
        END IF
        DO I=1,2
465       CONTINUE
          ILINE=ILINE+1
	  READ(11,'(A)',ERR=9400,END=9480) CLINE
	  IF(IPASS.EQ.2) WRITE(20,'(A)',ERR=9300) TRIM(CLINE)
          IF(SKIPLINE(CLINE)) GO TO 465
        END DO
        IF(IPASS.EQ.1)THEN
468       CONTINUE
          ILINE=ILINE+1
          READ(11,'(A)',ERR=9400,END=9480) CLINE
          IF(SKIPLINE(CLINE)) GO TO 468
          CALL LINSPL(IFAIL,5,LW,RW,CLINE)
          IF(IFAIL.NE.0) GO TO 9400
          CALL INTREAD(IFAIL,CLINE(LW(1):RW(1)),NPAR)
          IF(IFAIL.NE.0) GO TO 9400
          CALL INTREAD(IFAIL,CLINE(LW(2):RW(2)),NOBS)
          IF(IFAIL.NE.0) GO TO 9400
          CALL INTREAD(IFAIL,CLINE(LW(3):RW(3)),NPARGP)
          IF(IFAIL.NE.0) GO TO 9400
          CALL INTREAD(IFAIL,CLINE(LW(4):RW(4)),NPRIOR)
          IF(IFAIL.NE.0) GO TO 9400
          CALL INTREAD(IFAIL,CLINE(LW(5):RW(5)),NOBSGP)
          IF(IFAIL.NE.0) GO TO 9400
          IF(NPAR.LE.0) GO TO 9400
          ALLOCATE(PVAL(NPAR),NEWPVAL(NPAR),ITRANS(NPAR),AAPAR(NPAR),
     +    STAT=IERR)
          IF(IERR.NE.0) GO TO 9200
          ITRANS=0            !An array
          PVAL=-1.1D37        !An array
          NEWPVAL=0.0D0       !An array
        ELSE
469       CONTINUE
          ILINE=ILINE+1
	  READ(11,'(A)',ERR=9400,END=9480) CLINE
	  WRITE(20,'(A)',ERR=9300) TRIM(CLINE)
          IF(SKIPLINE(CLINE)) GO TO 469
        END IF
470     CONTINUE
	  ILINE=ILINE+1
	  READ(11,'(A)',ERR=9400,END=9480) CLINE
	  IF(CLINE(1:1).EQ.'*') THEN
	    CALL LOWCAS(CLINE)
	    IF(INDEX(CLINE,'parameter').NE.0) THEN
	      IF(INDEX(CLINE,'data').NE.0)THEN
	        IIPAR=1
	      END IF
	    END IF
	  END IF
	  IF(IPASS.EQ.2) WRITE(20,'(A)',ERR=9300) TRIM(CLINE)
	  IF(IIPAR.EQ.1) GO TO 500
        GO TO 470

C -- THE "* PARAMETER DATA" OF THE PEST CONTROL FILE IS READ AND SUBSTITUTIONS
C    MADE.

500	CONTINUE
        DO IPAR=1,NPAR
501       CONTINUE
	  ILINE=ILINE+1
	  READ(11,'(A)',ERR=9400,END=9480) CLINE
	  CALL LOWCAS(CLINE)
	  IF(CLINE(1:1).EQ.'*') GO TO 9400
          DLINE=CLINE
          IF(SKIPLINE(CLINE))THEN
            IF(IPASS.EQ.2)WRITE(20,'(A)') TRIM(DLINE)
            GO TO 501
          END IF
          NEQQ=INDEX(CLINE,'=')
          IF(NEQQ.GT.1)THEN
            IF(IPASS.EQ.2) WRITE(20,'(A)',ERR=9300) TRIM(DLINE)
            GO TO 501
          END IF
          CALL LINSPL(IFAIL,2,LW,RW,CLINE)
          IF(IFAIL.NE.0) GO TO 9400
          IF(CLINE(LW(2):RW(2)).EQ.'file_parameter')THEN
            IF(IPASS.EQ.2) WRITE(20,'(A)',ERR=9300) TRIM(DLINE)
            GO TO 501
          END IF
          DLINE=ADJUSTL(DLINE)
          CALL LINSPL(IFAIL,9,LW,RW,CLINE)
	  IF(IFAIL.NE.0) GO TO 9400
          IF(RW(1)-LW(1).GT.11)THEN
	    CALL WRTINT(ALINE,ILINE)
	    WRITE(ERRMSG,550) TRIM(ALINE),TRIM(PESTFILE1)
550	    FORMAT('Parameter name greater than 12 characters at ',
     +      'line ',A,' of file ',A,'.')
	    GO TO 9890
	  END IF
	  AAPAR(IPAR)=CLINE(LW(1):RW(1))
          DO I=1,NNPAR
	    IF(AAPAR(IPAR).EQ.APAR(I)) GO TO 570
          END DO
          ATRANS=CLINE(LW(2):RW(2))
          CALL LOWCAS(ATRANS)
          IF(ATRANS.EQ.'tied')THEN
            IF(IPASS.EQ.1)THEN
              CALL DREALRD(IFAIL,CLINE(LW(4):RW(4)),PVAL(IPAR))
              IF(IFAIL.NE.0) THEN
                CALL WRTINT(ALINE,ILINE)
                WRITE(ERRMSG,600) TRIM(ALINE),TRIM(PESTFILE1)
                GO TO 9890
              END IF
              ITRANS(IPAR)=-1
              INEEDED=INEEDED+1
            ELSE IF(IPASS.EQ.2)THEN
              WRITE(20,630,ERR=9300) CLINE(LW(1):RW(3)),NEWPVAL(IPAR),
     +        DLINE(LW(5):LEN_TRIM(DLINE))
            END IF
            CYCLE
          ELSE IF(ATRANS.EQ.'fixed')THEN
            IF(IPASS.EQ.2)THEN
              WRITE(20,'(A)',ERR=9300) TRIM(DLINE)
            END IF
            CYCLE
          ELSE
            CALL WRTINT(ALINE,ILINE)
            WRITE(ERRMSG,560) TRIM(ALINE),TRIM(PESTFILE1),
     +      TRIM(AAPAR(IPAR)),TRIM(RRFFILE)
560	    FORMAT('Line ',A,' of file ',A,': ',
     +      'parameter "',A,'" not cited in RRF file ',A,'.')
	    GO TO 9890
          END IF
570	  CALL DREALRD(IFAIL,CLINE(LW(4):RW(4)),RVAL)
	  IF(IFAIL.EQ.0)
     +    CALL DREALRD(IFAIL,CLINE(LW(8):RW(8)),RSCALE)
	  IF(IFAIL.EQ.0)
     +    CALL DREALRD(IFAIL,CLINE(LW(9):RW(9)),ROFFSET)
	  IF(IFAIL.NE.0) THEN
	    CALL WRTINT(ALINE,ILINE)
	    WRITE(ERRMSG,600) TRIM(ALINE),TRIM(PESTFILE1)
600	    FORMAT('Data input error ',
     +      'at line ',A,' of file ',A,'.')
	    GO TO 9890
	  END IF
          IF(IPASS.EQ.1)THEN
            PVAL(IPAR)=RVAL
            NEWPVAL(IPAR)=VALUE(I)
          ELSE IF(IPASS.EQ.2)THEN
            WRITE(20,630,ERR=9300) CLINE(LW(1):RW(3)),VALUE(I),
     +      DLINE(LW(5):LEN_TRIM(DLINE))
630         FORMAT(A,2X,1PG14.7,2X,A)
          END IF
	END DO

C -- IF REQUIRED, THE NATURE OF TIED PARAMETERS IS ASCERTAINED.

        IF(IPASS.EQ.1)THEN
          IF(INEEDED.NE.0)THEN
            DO
631           CONTINUE
              ILINE=ILINE+1
              READ(11,'(A)',ERR=9400,END=9400) CLINE
              CLINE=ADJUSTL(CLINE)
              IF(CLINE(1:1).EQ.'*') GO TO 640
              IF(SKIPLINE(CLINE)) GO TO 631
              NEQQ=INDEX(CLINE,'=')
              IF(NEQQ.GT.1) GO TO 631
              CALL LINSPL(IFAIL,2,LW,RW,CLINE)
              IF(IFAIL.NE.0) THEN
                CALL WRTINT(ALINE,ILINE)
                WRITE(ERRMSG,600) TRIM(ALINE),TRIM(PESTFILE1)
                GO TO 9890
              END IF
              IF(CLINE(LW(2):RW(2)).EQ.'file_parameter')GO TO 631
              AAPAR1=CLINE(LW(1):RW(1))
              CALL LOWCAS(AAPAR1)
              AAPAR2=CLINE(LW(2):RW(2))
              CALL LOWCAS(AAPAR2)
              IF(AAPAR1.EQ.AAPAR2) THEN
                CALL WRTINT(ALINE,ILINE)
                WRITE(ERRMSG,600) TRIM(ALINE),TRIM(PESTFILE1)
                GO TO 9890
              END IF
              DO IPAR=1,NPAR
                IF(AAPAR1.EQ.AAPAR(IPAR))THEN
                  IF(ITRANS(IPAR).EQ.-1)THEN
                    DO JPAR=1,NPAR
                      IF(AAPAR2.EQ.AAPAR(JPAR))THEN
                        IF(ITRANS(JPAR).LT.0) THEN
                          CALL WRTINT(ALINE,ILINE)
                          WRITE(ERRMSG,600) TRIM(ALINE),TRIM(PESTFILE1)
                          GO TO 9890
                        END IF
                        IF(PVAL(JPAR).LT.-1.0D37)THEN
                          WRITE(ERRMSG,639) TRIM(PESTFILE1)
639                       FORMAT('File ',a,' is in error - check ',
     +                    'it with PESTCHEK.')
                          GO TO 9890
                        END IF
                        NEWPVAL(IPAR)=
     +                  NEWPVAL(JPAR)/PVAL(JPAR)*PVAL(IPAR)
                        ITRANS(IPAR)=-2
                        GO TO 641
                      END IF
                    END DO
                    CALL WRTINT(ALINE,ILINE)
                    WRITE(ERRMSG,638) TRIM(ALINE),TRIM(PESTFILE1)
                    GO TO 9890
                  ELSE
                    IF(ITRANS(IPAR).EQ.-2) GO TO 9400
                    GO TO 641
                  END IF
                END IF
              END DO
              CALL WRTINT(ALINE,ILINE)
              WRITE(ERRMSG,638) TRIM(ALINE),TRIM(PESTFILE1)
638           FORMAT('Unknown parameter at line ',a,' of file ',a,'.')
              GO TO 9890
641           CONTINUE
            END DO
640         DO IPAR=1,NPAR
              IF(ITRANS(IPAR).EQ.-1)THEN
                WRITE(ERRMSG,660) TRIM(AAPAR(IPAR)),TRIM(PESTFILE1)
660             FORMAT('No parent for parameter "',a,'" in file ',
     +          a,'.')
                GO TO 9890
              END IF
            END DO
          END IF
          IPASS=2
          ILINE=0
          REWIND(UNIT=11)
          GO TO 451
        END IF


C -- THE REMAINDER OF THE CONTROL DATA IS TRANSFERRED FROM ONE FILE TO THE
C    OTHER.

700	CONTINUE
720	ILINE=ILINE+1
	READ(11,'(A)',ERR=9400,END=800) CLINE
	WRITE(20,'(A)',ERR=9300) TRIM(CLINE)
	GO TO 720

800	WRITE(6,810) TRIM(PESTFILE2)
810	FORMAT(' - file ',A,' written ok.')
	CLOSE(UNIT=20)

	GO TO 9990

9000	WRITE(6,9010)
9010	FORMAT(' PARREP_RRF is run using the command:',/)
	WRITE(6,9020)
9020	FORMAT('     parrep_rrf rrffile parsetindex pestfile1 ',
     +  'pestfile2',/)
	WRITE(6,9030)
9030	FORMAT(' where',/)
	WRITE(6,9040)
9040	FORMAT('     rrffile     is the name of a run results ',
     +  'file,')
        WRITE(6,9045)
9045    FORMAT('     parsetindex is a parameter set index in ',
     +  'this file,')
	WRITE(6,9050)
9050	FORMAT('     pestfile1   is the name of the input PEST ',
     +  'control file, and')
	WRITE(6,9060)
9060	FORMAT('     pestfile2   is the name of the output PEST ',
     +  'control file.')
	GO TO 9999

9200    write(errmsg,9210)
9210    format('Cannot allocate sufficient memory to continue ',
     +  'execution.')
        go to 9890

9300	WRITE(ERRMSG,9310) TRIM(PESTFILE2)
9310	FORMAT('Cannot write to file ',A,'.')
	GO TO 9890

9400	WRITE(ERRMSG,9410) TRIM(PESTFILE1)
9410	FORMAT('Error reading file ',A,' - check it with PESTCHEK.')
	GO TO 9890

9480	WRITE(ERRMSG,9490) TRIM(PESTFILE1)
9490	FORMAT('Unexpected end encountered to file ',A,'.')
	GO TO 9890

9500   write(errmsg,9510) trim(rrffile)
9510   format('Error or premature end encountered to run results ',
     + 'file ',a,' while reading header section of this file.')
       go to 9890

9550   write(errmsg,9560) trim(asection),trim(rrffile)
9560   format('Cannot find "',a,'" section in file ',a,'.')
       go to 9890

9600   write(errmsg,9610) trim(asection),trim(rrffile)
9610   format('Error reading data from "',a,'" section of file ',a,'.')
       go to 9890

9650   call wrtint(aline,iline)
       write(errmsg,9660) trim(aline),trim(rrffile)
9660   format('Cannot read parameter set index from line ',a,
     + ' of file ',a,'.')
       go to 9890

9700   call wrtint(aline,iline)
       write(errmsg,9710) trim(aline),trim(rrffile)
9710   format('Error reading data from line ',a,' of file ',a,'.')
       go to 9890

9850   write(errmsg,9860) trim(asection),trim(aline),trim(rrffile)
9860   format('"',a,'" header expected at line ',a,' of file ',a,'.')
       go to 9890

9890   continue
       errmsg=' '//trim(errmsg)
       call writmess(6,errmsg)
       close(unit=20,status='delete',iostat=ierr)

9990	CONTINUE
	DEALLOCATE(APAR,VALUE,STAT=IERR)
        DEALLOCATE(PVAL,NEWPVAL,ITRANS,AAPAR,STAT=IERR)

	CLOSE(UNIT=10,IOSTAT=IERR)
	CLOSE(UNIT=11,IOSTAT=IERR)

9999	END


        SUBROUTINE LINSPL(IFAIL,NUM,LW,RW,CLINE)

C -- SUBROUTINE LINSPL SPLITS A LINE INTO WHITESPACE-SEPARATED SUBSTRINGS

        INTEGER IFAIL,NW,NBLC,J,I
        INTEGER NUM
        INTEGER LW(NUM),RW(NUM)
        CHARACTER*(*) CLINE

        IFAIL=0
        NW=0
        NBLC=len_trim(CLINE)
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

        DO 10 I=1,len_trim(ASTRNG)
        J=ICHAR(ASTRNG(I:I))
        IF((J.GE.65).AND.(J.LE.90)) ASTRNG(I:I)=CHAR(J+32)
10      CONTINUE
        RETURN
        END


        SUBROUTINE UPCAS(ASTRNG)

C -- SUBROUTINE UPCAS CONVERTS A STRING TO UPPER CASE

        INTEGER I,J
        CHARACTER*(*) ASTRNG

        DO 10 I=1,len_trim(ASTRNG)
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




        subroutine intread(ifail,cline,itemp)

C -- Subroutine INTREAD reads a real number from a string.

        integer ifail
        integer itemp
        character*6 afmt
        character*(*) cline

        ifail=0
        afmt='(i   )'
        write(afmt(3:5),'(i3)') len(cline)
        read(cline,afmt,err=100) itemp
        return

100     ifail=1
        return
        end



      subroutine writmess(iunit,amessage)

        implicit none

	integer iunit,jend,i,nblc,junit,leadblank,itake,j
        character*(*) amessage
	character (len=20) ablank

	ablank=' '
	itake=0
	j=0
	junit=iunit

        if(amessage.eq.' ')then
          write(junit,*)
          return
        end if
        write(junit,*)
	do i=1,min(20,len(amessage))
	  if(amessage(i:i).ne.' ')go to 21
20      end do
21	leadblank=i-1
	nblc=len_trim(amessage)
5       jend=j+78-itake
	if(jend.ge.nblc) go to 100
	do i=jend,j+1,-1
	if(amessage(i:i).eq.' ') then
	  if(itake.eq.0) then
	     write(junit,'(a)') amessage(j+1:i)
	     itake=2+leadblank
	  else
	     write(junit,'(a)') ablank(1:leadblank+2)//
     +       amessage(j+1:i)
	  end if
	  j=i
	  go to 5
	end if
	end do
	if(itake.eq.0)then
	  write(junit,'(a)') amessage(j+1:jend)
	  itake=2+leadblank
	else
	  write(junit,'(a)') ablank(1:leadblank+2)//
     +    amessage(j+1:jend)
	end if
	j=jend
	go to 5
100     jend=nblc
	if(itake.eq.0)then
	  write(junit,'(a)') amessage(j+1:jend)
	else
	  write(junit,'(a)') ablank(1:leadblank+2)//
     +    amessage(j+1:jend)
	end if
	return


      end



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
