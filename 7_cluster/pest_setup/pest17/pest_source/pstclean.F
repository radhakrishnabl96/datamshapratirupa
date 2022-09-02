	PROGRAM PSTCLEAN

C -- Program PSTCLEAN removes PEST++ data lines and comments from a PEST control file.

        IMPLICIT NONE
        LOGICAL SKIPLINE
        INTEGER IFAIL,LW(10),RW(10),IERR
        INTEGER N,NB
        INTEGER ILINE,II,JJ,I
        INTEGER NOWRITE,REGFLAG
        DOUBLE PRECISION DTEMP
        CHARACTER*5 AEXT
        CHARACTER*12 AVERSION
        CHARACTER*20 ATEMP20
        CHARACTER*50 VARTEXT,UPVARTEXT
        CHARACTER*256 PESTFILE1,PESTFILE2,AFILE,DUMFILE
        CHARACTER*1000 CLINE,AMESSAGE

        INCLUDE 'version.inc'
        WRITE(6,10) TRIM(AVERSION)
10      FORMAT(' PSTCLEAN Version ',A,'. Watermark Numerical ',
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
	CALL LINSPL(IFAIL,3,LW,RW,CLINE)
	IF(IFAIL.EQ.0) GO TO 9000
	CALL LINSPL(IFAIL,2,LW,RW,CLINE)
	IF(IFAIL.NE.0) GO TO 9000
	PESTFILE1=CLINE(LW(1):RW(1))
	PESTFILE2=CLINE(LW(2):RW(2))
        CALL REMCHAR(PESTFILE1,CHAR(211))
        CALL REMCHAR(PESTFILE2,CHAR(211))
#ifndef UNIX
        CALL LOWCAS(PESTFILE1)
        CALL LOWCAS(PESTFILE2)
#endif

        NB=LEN_TRIM(PESTFILE1)
        N=NB-3
        IF(N.LT.1)N=1
        AEXT=PESTFILE1(N:NB)
        IF(AEXT(1:1).EQ.'.')THEN
          IF(AEXT.NE.'.pst')THEN
            CALL ADDQUOTE(PESTFILE1,AFILE)
            WRITE(6,30) TRIM(AFILE)
30          FORMAT(/,' File ',A,' must have an extension of ".pst".')
            GO TO 9900
          END IF
        END IF
        IF(AEXT.NE.'.pst') PESTFILE1=TRIM(PESTFILE1)//'.pst'
        NB=LEN_TRIM(PESTFILE2)
        N=NB-3
        IF(N.LT.1)N=1
        AEXT=PESTFILE2(N:NB)
        IF(AEXT(1:1).EQ.'.')THEN
          IF(AEXT.NE.'.pst')THEN
            CALL ADDQUOTE(PESTFILE2,AFILE)
            WRITE(6,30) TRIM(AFILE)
            GO TO 9900
          END IF
        END IF
        IF(AEXT.NE.'.pst') PESTFILE2=TRIM(PESTFILE2)//'.pst'
        IF(PESTFILE1.EQ.PESTFILE2)THEN
          WRITE(6,40)
40        FORMAT(/,' Input and output PEST control files cannot have ',
     +    'the same name.')
          GO TO 9900
        END IF

C -- The two PEST control files are open.

        OPEN(UNIT=10,FILE=PESTFILE1,STATUS='OLD',IOSTAT=IERR)
        IF(IERR.NE.0)THEN
          CALL ADDQUOTE(PESTFILE1,AFILE)
	  WRITE(6,50) TRIM(AFILE)
50	  FORMAT(/,' Cannot open PEST control file ',A,'.')
	  GO TO 9900
	END IF
	OPEN(UNIT=20,FILE=PESTFILE2,ACTION='WRITE',IOSTAT=IERR)
	IF(IERR.NE.0) THEN
	  CALL ADDQUOTE(PESTFILE2,AFILE)
	  WRITE(6,60) TRIM(AFILE)
60	  FORMAT(/,' Cannot write to PEST control file ',A,'.')
	  GO TO 9900
	END IF

        CALL ADDQUOTE(PESTFILE1,AFILE)
        ILINE=1
        READ(10,'(A)',ERR=200,END=200) CLINE
        ATEMP20=CLINE(1:20)
        CALL LOWCAS(ATEMP20)
        ATEMP20=ADJUSTL(ATEMP20)
        IF(ATEMP20(1:4).NE.'pcf ')THEN
          WRITE(AMESSAGE,65) TRIM(PESTFILE1)
65        FORMAT('File ',A,' does not appear to be a PEST ',
     +    'control file.')
          GO TO 9890
        END IF
        WRITE(20,'(A)') 'pcf'

        DO I=1,3
66        CONTINUE
	  ILINE=ILINE+1
	  READ(10,'(A)',ERR=9100,END=9150) CLINE
	  IF(SKIPLINE(CLINE)) GO TO 66
          WRITE(20,'(A)') TRIM(CLINE)
	END DO

        ILINE=ILINE+1
67      CONTINUE
	READ(10,'(A)',ERR=9100,END=9150) CLINE
	IF(SKIPLINE(CLINE)) GO TO 67
	CALL LOWCAS(CLINE)
        II=INDEX(CLINE,'orr_not_first')
        IF(II.NE.0)THEN
          DO JJ=II,LEN_TRIM(CLINE)
            IF(CLINE(JJ:JJ).EQ.' ') EXIT
            CLINE(JJ:JJ)=' '
          END DO
          CLINE(II:)=' '//ADJUSTL(CLINE(II:))
          WRITE(20,'(A)') TRIM(CLINE)
        ELSE
          WRITE(20,'(A)') TRIM(CLINE)
        END IF

        ILINE=ILINE+1
68      CONTINUE
	READ(10,'(A)',ERR=9100,END=9150) CLINE
	IF(SKIPLINE(CLINE)) GO TO 68
	CALL LOWCAS(CLINE)
        VARTEXT='run_slow_fac'
        UPVARTEXT='RUN_SLOW_FAC'
        CALL READ_MRUN_VAR(IFAIL,DTEMP,VARTEXT,UPVARTEXT,
     +  ILINE,AFILE,CLINE,AMESSAGE,1,DUMFILE)
        IF(IFAIL.NE.0) GO TO 9890
        VARTEXT='run_abandon_fac'
        UPVARTEXT='RUN_ABANDON_FAC'
        CALL READ_MRUN_VAR(IFAIL,DTEMP,VARTEXT,UPVARTEXT,
     +  ILINE,AFILE,CLINE,AMESSAGE,1,DUMFILE)
        IF(IFAIL.NE.0) GO TO 9890
        VARTEXT='win_mrun_hours'
        UPVARTEXT='WIN_MRUN_HOURS'
        CALL READ_MRUN_VAR(IFAIL,DTEMP,VARTEXT,UPVARTEXT,
     +  ILINE,AFILE,CLINE,AMESSAGE,1,DUMFILE)
        IF(IFAIL.NE.0) GO TO 9890
        VARTEXT='uptestlim'
        UPVARTEXT='UPTESTLIM'
        CALL READ_MRUN_VAR(IFAIL,DTEMP,VARTEXT,UPVARTEXT,
     +  ILINE,AFILE,CLINE,AMESSAGE,1,DUMFILE)
        IF(IFAIL.NE.0) GO TO 9890
        VARTEXT='uptestmin'
        UPVARTEXT='UPTESTMIN'
        CALL READ_MRUN_VAR(IFAIL,DTEMP,VARTEXT,UPVARTEXT,
     +  ILINE,AFILE,CLINE,AMESSAGE,1,DUMFILE)
        IF(IFAIL.NE.0) GO TO 9890
        WRITE(20,'(A)') TRIM(CLINE)

69      CONTINUE
        ILINE=ILINE+1
	READ(10,'(A)',ERR=9100,END=9150) CLINE
	IF(SKIPLINE(CLINE)) GO TO 69
        WRITE(20,'(A)') TRIM(CLINE)

71      CONTINUE
        ILINE=ILINE+1
	READ(10,'(A)',ERR=9100,END=9150) CLINE
	IF(SKIPLINE(CLINE)) GO TO 71
	CALL LOWCAS(CLINE)
        VARTEXT='zerosenval'
        UPVARTEXT='ZEROSENVAL'
        CALL READ_MRUN_VAR(IFAIL,DTEMP,VARTEXT,UPVARTEXT,
     +  ILINE,AFILE,CLINE,AMESSAGE,1,DUMFILE)
        IF(IFAIL.NE.0) GO TO 9890
        VARTEXT='jcowarnthresh'
        UPVARTEXT='JCOWARNTHRESH'
        CALL READ_MRUN_VAR(IFAIL,DTEMP,VARTEXT,UPVARTEXT,
     +  ILINE,AFILE,CLINE,AMESSAGE,1,DUMFILE)
        IF(IFAIL.NE.0) GO TO 9890
        VARTEXT='jcozerothresh'
        UPVARTEXT='JCOZEROTHRESH'
        CALL READ_MRUN_VAR(IFAIL,DTEMP,VARTEXT,UPVARTEXT,
     +  ILINE,AFILE,CLINE,AMESSAGE,1,DUMFILE)
        IF(IFAIL.NE.0) GO TO 9890
        WRITE(20,'(A)') TRIM(CLINE)

72      CONTINUE
        ILINE=ILINE+1
	READ(10,'(A)',ERR=9100,END=9150) CLINE
	IF(SKIPLINE(CLINE)) GO TO 72
	CALL LOWCAS(CLINE)
        VARTEXT='softstophours'
        UPVARTEXT='SOFTSTOPHOURS'
        CALL READ_MRUN_VAR(IFAIL,DTEMP,VARTEXT,UPVARTEXT,
     +  ILINE,AFILE,CLINE,AMESSAGE,1,DUMFILE)
        IF(IFAIL.NE.0) GO TO 9890
        VARTEXT='hardstophours'
        UPVARTEXT='HARDSTOPHOURS'
        CALL READ_MRUN_VAR(IFAIL,DTEMP,VARTEXT,UPVARTEXT,
     +  ILINE,AFILE,CLINE,AMESSAGE,1,DUMFILE)
        IF(IFAIL.NE.0) GO TO 9890
        WRITE(20,'(A)') TRIM(CLINE)

73      CONTINUE
        ILINE=ILINE+1
	READ(10,'(A)',ERR=9100,END=9150) CLINE
	IF(SKIPLINE(CLINE)) GO TO 73
	CALL LOWCAS(CLINE)
        II=INDEX(CLINE,'rrfsave')
        IF(II.NE.0)THEN
          DO JJ=II,LEN_TRIM(CLINE)
            IF(CLINE(JJ:JJ).EQ.' ') EXIT
            CLINE(JJ:JJ)=' '
          END DO
          CLINE(II:)=' '//ADJUSTL(CLINE(II:))
        END IF
        WRITE(20,'(A)') TRIM(CLINE)

C -- Now the rest of the PST file is read.

	NOWRITE=0
	DO
	  ILINE=ILINE+1
	  READ(10,'(A)',ERR=9100,END=200) CLINE
	  IF(.NOT.SKIPLINE(CLINE)) THEN
	    IF(CLINE(1:1).EQ.'*')THEN
	      NOWRITE=0
	      CALL LOWCAS(CLINE)
	      IF(CLINE.EQ.'* model input')THEN
	        CLINE='* model input/output'
	      ELSE IF(CLINE.EQ.'* model output')THEN
	        CYCLE
	      ELSE IF(CLINE(1:8).EQ.'* simult')THEN
	        NOWRITE=1
	      ELSE IF(CLINE(1:9).eq.'* randomi')THEN
                NOWRITE=1
              ELSE IF(CLINE(1:7).EQ.'* regul')THEN
                REGFLAG=1
	      ELSE IF(CLINE(1:9).EQ.'* distrib')THEN
	        NOWRITE=1
	      ELSE IF(CLINE(1:5).EQ.'* rsi')THEN
	        NOWRITE=1
	      END IF
	    END IF
	    IF(REGFLAG.EQ.1)THEN
              VARTEXT='reg2measrat'
              UPVARTEXT='REG2MEASRAT'
              CALL READ_MRUN_VAR(IFAIL,DTEMP,VARTEXT,UPVARTEXT,
     +        ILINE,AFILE,CLINE,AMESSAGE,1,DUMFILE)
              IF(IFAIL.NE.0) GO TO 9890
            END IF
	    IF(NOWRITE.EQ.0)WRITE(20,'(A)') TRIM(CLINE)
	  END IF
	END DO
200     CONTINUE
        WRITE(6,210) TRIM(PESTFILE1)
210     FORMAT(' - file ',a,' read ok.')
        WRITE(6,220) TRIM(PESTFILE2)
220     FORMAT(' - file ',a,' written ok.')

	GO TO 9900

9000	WRITE(6,9010)
9010	FORMAT(' PSTCLEAN is run using the command:',/)
	WRITE(6,9020)
9020	FORMAT('     PSTCLEAN pestfile1 pestfile2',/)
	WRITE(6,9030)
9030	FORMAT(' where',/)
	WRITE(6,9050)
9050	FORMAT('     pestfile1 is the name of an existing PEST ',
     +  'control file, and')
	WRITE(6,9060)
9060	FORMAT('     pestfile2 is a new PEST ',
     +  'control file to be written by PSTCLEAN.')
	GO TO 9999

9100    WRITE(AMESSAGE,9110) TRIM(AFILE)
9110    FORMAT('Error encountered when reading PEST control file ',
     +  A,': check this file with PESTCHEK.')
        GO TO 9890

9150    WRITE(AMESSAGE,9160) TRIM(AFILE)
9160    FORMAT('Premature end encountered to file ',A,'.')
        GO TO 9890

9890    CONTINUE
        AMESSAGE=' '//TRIM(AMESSAGE)
        CALL WRITMESS(6,AMESSAGE)

9900	CONTINUE


9999	END



        subroutine linspl(ifail,num,lw,rw,cline)

c -- Subroutine LINSPL splits a line into whitespace-separated substrings.

        integer ifail,nw,nblc,j,i
        integer num
        integer lw(num),rw(num)
        character*(*) cline

        ifail=0
        nw=0
        nblc=len_trim(cline)
        if(nblc.eq.0) then
          ifail=1
          return
        end if
        j=0
5       if(nw.eq.num) return
        do 10 i=j+1,nblc
        if((cline(i:i).ne.' ').and.(cline(i:i).ne.',')
     +  .and.(ichar(cline(i:i)).ne.9)) go to 20
10      continue
        ifail=1
        return
20      nw=nw+1
        lw(nw)=i
        do 30 i=lw(nw)+1,nblc
        if((cline(i:i).eq.' ').or.(cline(i:i).eq.',')
     +  .or.(ichar(cline(i:i)).eq.9)) go to 40
30      continue
        rw(nw)=nblc
        if(nw.lt.num) ifail=1
        return
40      rw(nw)=i-1
        j=rw(nw)
        go to 5

        end


        subroutine lowcas(astrng)

C -- Subroutine LOWCAS converts a string to lower case.

        integer i,j
        character*(*) astrng

        do 10 i=1,len_trim(astrng)
        j=ichar(astrng(i:i))
        if((j.ge.65).and.(j.le.90)) astrng(i:i)=char(j+32)
10      continue
        return
        end



        subroutine addquote(afile,aqfile)

C -- Subroutine ADDQUOTE adds quotes to a filename if it has a space in it.

        implicit none

        character*(*) afile
        character*(*) aqfile
        integer nbb

        if(index(trim(afile),' ').eq.0)then
          aqfile=afile
        else
          aqfile(1:1)='"'
          aqfile(2:)=trim(afile)
          nbb=len_trim(aqfile)+1
          aqfile(nbb:nbb)='"'
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



        SUBROUTINE READ_MRUN_VAR(IFAIL,DVAL,VARTEXT,UPVARTEXT,ILINE,
     +  AFILE,CLINE,ERRMSG,ITYPE,BFILE)

        IMPLICIT NONE
        INTEGER IFAIL,ILINE,ITYPE
        DOUBLE PRECISION DVAL
        CHARACTER*1 AA
        CHARACTER*(*) VARTEXT,UPVARTEXT
        CHARACTER*(*) AFILE,CLINE,ERRMSG,BFILE

        INTEGER  NN,NNLT,I
        CHARACTER*20 ALINE
        CHARACTER*25 ATEMP25
        CHARACTER*256 ATEMP256
        CHARACTER*2000 DLINE

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
            CALL WRITINT(ALINE,ILINE)
            WRITE(ERRMSG,2105) TRIM(UPVARTEXT),TRIM(ALINE),TRIM(AFILE)
2105        FORMAT('Cannot read value of ',A,' variable ',
     +      'on line ',A,' of file ',A,'.')
            GO TO 9890
          END IF
          ATEMP256=ADJUSTL(ATEMP256(2:))
          NN=INDEX(ATEMP256,' ')
          IF((NN.EQ.0).OR.(NN.EQ.1))THEN
            CALL WRITINT(ALINE,ILINE)
            WRITE(ERRMSG,2105) TRIM(UPVARTEXT),TRIM(ALINE),TRIM(AFILE)
            GO TO 9890
          END IF
          IF(ITYPE.EQ.1)THEN
            ATEMP25=ATEMP256
            ATEMP25(NN:)=' '
            CALL DREALRD(IFAIL,ATEMP25,DVAL)
            IF(IFAIL.NE.0) THEN
              CALL WRITINT(ALINE,ILINE)
              WRITE(ERRMSG,2105) TRIM(UPVARTEXT),TRIM(ALINE),TRIM(AFILE)
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
                CALL WRITINT(ALINE,ILINE)
                WRITE(ERRMSG,2106) TRIM(UPVARTEXT),TRIM(ALINE),
     +                             TRIM(AFILE)
2106            FORMAT('Cannot read ',a,' filename on line ',A,
     +          ' of file ',A,'.')
                GO TO 9890
              END IF
              ATEMP256(NN+1:)=' '
              BFILE=ATEMP256(2:)
            ELSE
              BFILE=ADJUSTL(ATEMP256(1:NN-1))
            END IF
            IF(BFILE.EQ.' ')THEN
              CALL WRITINT(ALINE,ILINE)
              WRITE(ERRMSG,2106) TRIM(UPVARTEXT),TRIM(ALINE),
     +                           TRIM(AFILE)
              GO TO 9890
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
            CALL WRITINT(ALINE,ILINE)
            WRITE(ERRMSG,2107) TRIM(UPVARTEXT),TRIM(ALINE),TRIM(AFILE)
2107        FORMAT(A,' variable appears twice on line ',A,
     +      ' of file ',A,'.')
            GO TO 9890
          END IF
        END IF
        GO TO 9900

9890    CONTINUE
        IFAIL=1
9900    CONTINUE
        CLINE=DLINE
        RETURN
        END


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

	subroutine writint(atemp,ival)

c	Subroutine WRITINT writes an integer to a character variable.

	integer*4 ival
	character*6 afmt
	character*(*) atemp

	afmt='(i   )'
	write(afmt(3:5),'(i3)') len(atemp)
	write(atemp,afmt)ival
	atemp=adjustl(atemp)
	return
	end


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

