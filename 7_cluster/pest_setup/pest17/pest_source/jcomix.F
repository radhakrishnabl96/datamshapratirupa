       program jcomix

! --   Program JCOMIX reads a PST file and two JCO files. It build a JCO file for the PST file
!      from the contents of the two JCO files.

       use pestdata
       implicit none

       integer      :: ifail,n,n1,ierr,icount,i,j
       integer      :: irestart,pfile
       integer      :: bn
       integer      :: ies,irow,jes,jrow,ipar
       integer      :: nespar2,nxrow2,nespar1,nxrow1
       integer      :: lw(10),rw(10)
       double precision :: dtemp
       double precision :: dumval,dumtest
       character*5  :: aext
       character*10 :: how
       character*12 :: aversion
       character*12 :: aapar
       character*20 :: aaobs
       character*256 :: jcofile1,jcofile2,jcofile3
       character*256 :: pestfile3,afile

       integer, allocatable          :: parloc(:),obsloc(:)
       double precision, allocatable :: x1(:,:),x2(:,:)
       character*12, allocatable     :: apar1(:),apar2(:)
       character*20, allocatable     :: aobs1(:),aobs2(:)

! -- Initialisation

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(' JCOMIX Version ',a,'. Watermark Numerical Computing.')
#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) CLINE
#else
       call pgetcl(cline)
#endif

       dumval=-1.1d300
       dumtest=-1.0d300

! -- The command line is parsed.

       if(cline.eq.' ') go to 9000
       call spacesub(cline)
       call linspl(ifail,5,lw,rw,cline)
       if(ifail.eq.0) go to 9000
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.ne.0) go to 9000
       jcofile1=cline(lw(2):rw(2))
       jcofile2=cline(lw(3):rw(3))
       jcofile3=cline(lw(4):rw(4))
       pestfile3=cline(lw(1):rw(1))
       call remchar(jcofile1,char(211))
       call remchar(jcofile2,char(211))
       call remchar(jcofile3,char(211))
       call remchar(pestfile3,char(211))
       n=len_trim(pestfile3)
       n1=n-3
       if(n1.lt.1)n1=1
       aext=pestfile3(n1:n)
       call lowcas(aext)
       if(aext.ne.'.pst ')then
         call addquote(pestfile3,afile)
         write(errmsg,20) trim(afile)
20       format('PEST control file ',a,' must have an extension of ',
     +   '".pst".')
         go to 9890
       end if
       n=len_trim(jcofile3)
       n1=n-3
       if(n1.lt.1)n1=1
       aext=jcofile3(n1:n)
       call lowcas(aext)
       if(aext.ne.'.jco ')then
         call addquote(jcofile3,afile)
         write(errmsg,30) trim(afile)
30       format('JCO file ',a,' must have an extension of ',
     +   '".jco".')
         go to 9890
       end if
       n=len_trim(jcofile1)
       n1=n-3
       if(n1.lt.1)n1=1
       aext=jcofile1(n1:n)
       call lowcas(aext)
       if(aext.ne.'.jco ')then
         call addquote(jcofile1,afile)
         write(errmsg,30) trim(afile)
         go to 9890
       end if
       n=len_trim(jcofile2)
       n1=n-3
       if(n1.lt.1)n1=1
       aext=jcofile2(n1:n)
       call lowcas(aext)
       if(aext.ne.'.jco ')then
         call addquote(jcofile2,afile)
         write(errmsg,30) trim(afile)
         go to 9890
       end if

! -- The PEST control file is read.

       write(6,*)
       call addquote(pestfile3,afile)
       write(6,110) trim(afile)
110    format(' - reading PEST control file ',a,'....')
       irestart=0
       p1start=0
       pfile=0
       call read_pest_data(ifail,irestart,p1start,pfile,pestfile3)
       if(ifail.ne.0) go to 9890
       if(maxcompdim.gt.1)then
         write(errmsg,120) trim(afile)
120      format('JCOMIX requires that MAXCOMPDIM be set to 1 ',
     +   'or less in PEST control file ',a,'.')
         go to 9890
       end if
       write(6,125) trim(afile)
125    format(' - file ',a,' read ok.')

! -- The Jacobian matrix is filled with dummy values (except for prior information).

       x(1:nobs,:)=dumval                   ! an array

! -- The first Jacobian matrix file is read.

       write(6,*)
       call addquote(jcofile1,afile)
       write(6,130) trim(afile)
130    format(' - reading first Jacobian matrix file ',a,'....')
       how='read'
       call open_unformatted_file(ifail,15,how,jcofile1,errmsg)
       if(ifail.ne.0) go to 9890
       read(15,err=9250,end=9250)nespar1,nxrow1
       bn=abs(nxrow1)
       nxrow1=bn
       if(nespar1.lt.0)then
         nespar1=-nespar1
       else
         write(errmsg,140) trim(afile)
140      format('Jacobian matrix file ',a,' was not produced by ',
     +   'version 8 or later of PEST; use program JCOTRANS to ',
     +   'convert this to new format.')
         go to 9890
       end if
       allocate(x1(nxrow1,nespar1),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(apar1(nespar1),aobs1(nxrow1),stat=ierr)
       if(ierr.ne.0) go to 9200
       x1=0.0d0             ! an array
       read(15,err=9250,end=9250)icount
       do i=1,icount
         read(15,err=9250,end=9250) j,dtemp
         ies=(j-1)/bn+1
         irow=j-(ies-1)*bn
         x1(irow,ies)=dtemp
       end do
       do ies=1,nespar1
         read(15,err=9250,end=9250) apar1(ies)
         call lowcas(apar1(ies))
       end do
       do irow=1,nxrow1
         read(15,err=9250,end=9250) aobs1(irow)
         call lowcas(aobs1(irow))
       end do
       close(unit=15)
       write(6,125) trim(afile)

! -- Values are now assigned to elements of the new JCO file. However to make this efficient
!    we define two location arrays.

       allocate(parloc(nespar),obsloc(nxrow),stat=ierr)
       if(ierr.ne.0) go to 9200
       parloc=0           ! an array
       obsloc=0           ! an array

       ies=0
       jes=1
       jrow=1
       do ipar=1,npar
         if(itrans(ipar).ge.0)then
           ies=ies+1
           aapar=apar(ipar)
           call which1(ifail,nespar1,jes,apar1,aapar)
           if(ifail.eq.0)parloc(ies)=jes
         end if
       end do
       do irow=1,nobs
         aaobs=aobs(irow)
         call which1(ifail,nxrow1,jrow,aobs1,aaobs)
         if(ifail.eq.0) obsloc(irow)=jrow
       end do

! -- Now we fill what we can of the Jacobian.

       write(6,*)
       write(6,141)
141    format(' - assigning values to new Jacobian matrix....')

       do ies=1,nespar
         jes=parloc(ies)
         if(jes.ne.0)then
           do irow=1,nobs
             jrow=obsloc(irow)
             if(jrow.ne.0) x(irow,ies)=x1(jrow,jes)
           end do
         end if
       end do

! -- Memory is deallocated.

       deallocate(x1,apar1,aobs1,stat=ierr)
       if(ierr.ne.0) then
         write(errmsg,210)
210      format('Memory management error: cannot deallocate space ',
     +   'for first Jacobian.')
         go to 9890
       end if

! -- The second Jacobian matrix file is read.

       write(6,*)
       call addquote(jcofile2,afile)
       write(6,230) trim(afile)
230    format(' - reading second Jacobian matrix file ',a,'....')
       how='read'
       call open_unformatted_file(ifail,15,how,jcofile2,errmsg)
       if(ifail.ne.0) go to 9890
       read(15,err=9250,end=9250)nespar2,nxrow2
       bn=abs(nxrow2)
       nxrow2=bn
       if(nespar2.lt.0)then
         nespar2=-nespar2
       else
         write(errmsg,140) trim(afile)
         go to 9890
       end if
       allocate(x2(nxrow2,nespar2),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(apar2(nespar2),aobs2(nxrow2),stat=ierr)
       if(ierr.ne.0) go to 9200
       x2=0.0d0             ! an array
       read(15,err=9250,end=9250)icount
       do i=1,icount
         read(15,err=9250,end=9250) j,dtemp
         ies=(j-1)/bn+1
         irow=j-(ies-1)*bn
         x2(irow,ies)=dtemp
       end do
       do ies=1,nespar2
         read(15,err=9250,end=9250) apar2(ies)
         call lowcas(apar2(ies))
       end do
       do irow=1,nxrow2
         read(15,err=9250,end=9250) aobs2(irow)
         call lowcas(aobs2(irow))
       end do
       close(unit=15)
       write(6,125) trim(afile)

! -- Values are now assigned to elements of the new JCO file.

       write(6,*)
       write(6,141)
       parloc=0       ! an array
       obsloc=0       ! an array
       ies=0
       jes=0
       jrow=0
       do ipar=1,npar
         if(itrans(ipar).ge.0)then
           ies=ies+1
           aapar=apar(ipar)
           call which1(ifail,nespar2,jes,apar2,aapar)
           if(ifail.eq.0)parloc(ies)=jes
         end if
       end do
       do irow=1,nobs
         aaobs=aobs(irow)
         call which1(ifail,nxrow2,jrow,aobs2,aaobs)
         if(ifail.eq.0) obsloc(irow)=jrow
       end do

! -- Now we fill what we can of the Jacobian.

       do ies=1,nespar
         jes=parloc(ies)
         if(jes.ne.0)then
           do irow=1,nobs
             if(x(irow,ies).lt.dumtest)then
               jrow=obsloc(irow)
               if(jrow.ne.0) x(irow,ies)=x2(jrow,jes)
             end if
           end do
         end if
       end do

! -- We can now dispense with the second array.

       deallocate(x2,apar2,aobs2,stat=ierr)

! -- We check to see if any elements of the Jacobian are unfilled.

       icount=count(x.lt.dumtest)
       if(icount.ne.0)then
         write(6,*)
         write(6,250)
250      format(' Some elements of the Jacobian matrix have not ',
     +   'been assigned a value.')
         ies=0
         do ipar=1,npar
           if(itrans(ipar).ge.0)then
             ies=ies+1
             do irow=1,nxrow
               if(x(irow,ies).lt.dumtest)then
                 write(6,*)
                 write(6,260)
260              format(' Details of first missing element are as ',
     +           'follows:')
                 write(6,270) trim(apar(ipar))
270              format('    Parameter   = ',a)
                 write(6,280) trim(aobs(irow))
280              format('    Observation = ',a)
                 go to 9999
               end if
             end do
           end if
         end do
       end if

! -- We now record the new Jacobian matrix.

       call addquote(jcofile3,afile)
       how='write'
       call open_unformatted_file(ifail,15,how,jcofile3,errmsg)
       if(ifail.ne.0) go to 9890
       write(15,err=9400) -nespar,-nxrow
       icount=0
       do i=1,nespar
          do j=1,nxrow
            if(x(j,i).ne.0.0d0) icount=icount+1
          end do
       end do
       write(15,err=9400) icount
       do i=1,nespar
         do j=1,nxrow
           if(x(j,i).ne.0.0d0)then
             icount=(i-1)*nxrow+j
             write(15,err=9400)icount,x(j,i)
           end if
         end do
       end do
       do ipar=1,npar
         if(itrans(ipar).ge.0)then
            write(15,err=9400) apar(ipar)
         end if
       end do
       do irow=1,nxrow
         write(15,err=9400) aobs(irow)
       end do
       close(unit=15)
       write(6,490) trim(afile)
490    format(' - file ',a,' written ok.')

       go to 9900

9000   continue

       write(6,9010)
9010   format(/,' JCOMIX is run using the command:',/)
       write(6,9020)
9020   format('     jcomix pestfile3 jcofile1 jcofile2 jcofile3',/)
       write(6,9030)
9030   format(' where',/)
       write(6,9050)
9050   format('     pestfile3 is the name of a PEST control file,')
       write(6,9051)
9051   format('     jcofile1  is the name of a "primary" JCO file,')
       write(6,9052)
9052   format('     jcofile2  is the name of a "secondary" JCO ',
     + 'file, and')
       write(6,9053)
9053   format('     jcofile3  is the name of a new JCO file to ',
     + 'match the PEST control file.')
       go to 9999

9200   write(errmsg,9210)
9210   format('Cannot allocate sufficent memory to continue execution.')
       go to 9890

9250   write(errmsg,9260) trim(afile)
9260   format('Error encountered in reading Jacobian matrix file ',
     + a,'.')
       go to 9890

9400   write(errmsg,9410) trim(afile)
9410   format('Error writing to file ',a,'.')
       go to 9890

9890   call write_message(errmsg)
9900   continue

9999   continue
       call pest_data_deallocate(ifail)
       deallocate(parloc,obsloc,stat=ierr)
       deallocate(x1,x2,stat=ierr)
       deallocate(apar1,apar2,stat=ierr)
       deallocate(aobs1,aobs2,stat=ierr)

       end


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

C -- Subroutine lowcas converts a string to lower case.

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

        character*(*) afile,aqfile
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


        subroutine write_message(errmsg)

C -- Subroutine write_message formats and writes a message to the screen.

        implicit none

        integer  junit
        integer  jend,i,nblc,ierr,j
        character*(*) errmsg

        junit=6
        write(junit,*)

        j=0
        nblc=len_trim(errmsg)
5       jend=j+78
        if(jend.ge.nblc) go to 100
        do i=jend,j+1,-1
          if(errmsg(i:i).eq.' ') then
            write(junit,50,iostat=ierr) errmsg(j+1:i)
50          format(1x,a)
            j=i
            go to 5
          end if
        end do
        write(junit,50,iostat=ierr) errmsg(j+1:jend)
        j=jend
        go to 5
100     jend=nblc
        write(junit,50,iostat=ierr) errmsg(j+1:jend)
        write(junit,*)
        return

        end



        SUBROUTINE WHICH1(IFAIL,NPAR,IPAR,APAR,TPAR)

C -- SUBROUTINE WHICH1 LOCATES A STRING IN AN ARRAY

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



	subroutine writint(atemp,ival)

!	Subroutine WRITINT writes an integer to a character variable.

	integer*4 ival
	character*6 afmt
	character*(*) atemp

	afmt='(i   )'
	write(afmt(3:5),'(i3)') len(atemp)
	write(atemp,afmt)ival
	atemp=adjustl(atemp)
	return
	end

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


        SUBROUTINE GETINT(IFAIL,BUF,N1,N2,NBLB,MRKDEL)

C -- SUBROUTINE GETINT GETS THE NEXT STORED INSTRUCTION FOR PROCESSING

        INTEGER N1,N2,NBLB,I,II
        INTEGER IFAIL
        CHARACTER MRKDEL
        CHARACTER*(*) BUF

        IFAIL=0
        IF(N2.GE.NBLB) THEN
          N1=0
          RETURN
        END IF
        DO 10 I=N2+1,NBLB
        IF((BUF(I:I).NE.' ').AND.(ICHAR(BUF(I:I)).NE.9)) GO TO 50
10      CONTINUE
        N1=0
        RETURN
50      N1=I
        IF(BUF(N1:N1).NE.MRKDEL)THEN
          I=INDEX(BUF(N1:NBLB),' ')
          II=INDEX(BUF(N1:NBLB),CHAR(9))
          IF((I.EQ.0).AND.(II.EQ.0))THEN
            I=0
          ELSE IF(I.EQ.0)THEN
            I=II
          ELSE IF(II.EQ.0) THEN
            I=I
          ELSE
            I=MIN(I,II)
          END IF
          IF(I.NE.0) THEN
            N2=N1+I-2
          ELSE
            N2=NBLB
          END IF
        ELSE
          IF(N1.EQ.NBLB)THEN
            IFAIL=1
            RETURN
          END IF
          I=INDEX(BUF(N1+1:NBLB),MRKDEL)
          IF(I.EQ.0) THEN
            IFAIL=1
            RETURN
          END IF
          N2=N1+I
        END IF

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



