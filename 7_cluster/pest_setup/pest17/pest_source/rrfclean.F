       program rrfclean

C -- Program RRFCLEAN ensures that parameter set indices are recorded in increasing
C    order, starting at 1.
C -- Any parameter sets associated with failed model runs are removed.
C    Also, if the last set of run results is incomplete, it is removed.

       implicit none

       integer ierr,iline,ifail
       integer npar,nobs,ipar,iobs
       integer ireject,iindex
       integer numsetread,numsetreject,numset,numindchange,incomplete
       integer lw(10),rw(10)
       character*3 ayn
       character*10 aline,atype,anum
       character*12 aversion
       character*40 asection
       character*256 rrffile1,rrffile2
       character*256 qrrffile1,qrrffile2
       character*256 parsource
       character*500 cline
       character*1500 errmsg

       real,    allocatable :: pval(:),oval(:)

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,'RRFCLEAN Version ',a,
     + '. Watermark Numerical Computing.')

#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) cline
#else
       call pgetcl(cline)
#endif

C -- The RRFCLEAN command line is read.

       if(cline.eq.' ') go to 8900
       call spacesub(cline)
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,2,lw,rw,cline)
       if(ifail.ne.0) go to 8900
       rrffile1=cline(lw(1):rw(1))
       call remchar(rrffile1,char(211))
       rrffile2=cline(lw(2):rw(2))
       call remchar(rrffile2,char(211))

C -- Initialization

       numsetread=0
       numsetreject=0
       numset=0
       numindchange=0
       incomplete=0
       call addquote(rrffile1,qrrffile1)
       call addquote(rrffile2,qrrffile2)
       write(6,*)

C -- Files are opened.

       open(unit=10,file=rrffile1,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(errmsg,20) trim(qrrffile1)
20       format('Cannot open run results file ',a,'.')
         go to 9890
       end if
       open(unit=20,file=rrffile2,action='write',iostat=ierr)
       if(ierr.ne.0)then
         write(errmsg,21) trim(qrrffile2)
21       format('Cannot write to file ',a,'.')
         go to 9890
       end if

C -- The header to the existing run results file is read and transferred to the new run results file.

       iline=0
       asection='* case dimensions'
       iline=iline+1
       read(10,'(a)',err=9000,end=9050) cline
       call lowcas(cline)
       if(index(cline,trim(asection)).eq.0) then
         call writint(aline,iline)
         write(errmsg,153) trim(asection),trim(aline),trim(qrrffile1)
153      format('"',a,'" section header expected at line ',
     +   a,' of file ',a,'.')
         go to 9890
       end if
       write(20,'(a)') trim(cline)
       iline=iline+1
       read(10,*,err=9100,end=9100) npar,nobs
       atype='NPAR'
       if(npar.le.0)then
         call writint(aline,iline)
         write(errmsg,160) trim(atype),trim(aline),trim(qrrffile1)
160      format(a,' must be 1 or greater at line ',a,' of file ',
     +   a,'.')
         go to 9890
       end if
       if(nobs.le.0)then
         atype='NOBS'
         call writint(aline,iline)
         write(errmsg,160) trim(atype),trim(aline),trim(qrrffile1)
         go to 9890
       end if
       write(20,161) npar,nobs
161    format(2i10)

! -- The "* parameter names" section of the header is now read.

       asection='* parameter names'
       iline=iline+1
       read(10,'(a)',err=9000,end=9000)cline
       if(index(cline,trim(asection)).eq.0)then
         call writint(aline,iline)
         write(errmsg,170) trim(asection),trim(aline),trim(qrrffile1)
170      format('"',a,'" section expected at line ',a,
     +   ' of file ',a,'.')
         go to 9890
       end if
       write(20,'(a)') trim(cline)
       do ipar=1,npar
         iline=iline+1
         read(10,'(a)',err=9100,end=9100) cline
         write(20,'(a)') trim(cline)
       end do

       asection='* observation names'
       iline=iline+1
       read(10,'(a)',err=9000,end=9000)cline
       if(index(cline,trim(asection)).eq.0)then
         call writint(aline,iline)
         write(errmsg,170) trim(asection),trim(aline),trim(qrrffile1)
         go to 9890
       end if
       write(20,'(a)') trim(cline)
       do iobs=1,nobs
         iline=iline+1
         read(10,'(a)',err=9100,end=9100) cline
         write(20,'(a)') trim(cline)
       end do

C -- We allocate some memory.

       allocate(pval(npar),oval(nobs),stat=ierr)
       if(ierr.ne.0) go to 9200

C -- We now transfer contents, section by section, from the old RRF file to the new one.

       do
         asection='* parameter set index'
400      continue
         iline=iline+1
         read(10,'(a)',end=700) cline
         if(cline.eq.' ') go to 400
         call lowcas(cline)
         if(index(cline,trim(asection)).eq.0) then
           call writint(aline,iline)
           go to 9350
         end if
         numsetread=numsetread+1
         iline=iline+1
         read(10,'(a)',err=9150,end=699) cline
         if(cline.eq.' ')then
           call writint(aline,iline)
           write(errmsg,401) trim(aline),trim(qrrffile1)
401        format('Parameter set index expected on line ',a,
     +     ' of file ',a,'.')
           go to 9890
         end if
         call linspl(ifail,1,lw,rw,cline)
         call intread(ifail,cline(lw(1):rw(1)),iindex)
         if(ifail.ne.0)then
           call writint(aline,iline)
           write(errmsg,402) trim(aline),trim(qrrffile1)
402        format('Cannot read parameter set index from line ',a,
     +     ' of file ',a,'.')
           go to 9890
         end if
         asection='* parameter values source'
405      continue
         iline=iline+1
         read(10,'(a)',end=699) cline
         if(cline.eq.' ') go to 405
         call lowcas(cline)
         if(index(cline,trim(asection)).eq.0) then
           call writint(aline,iline)
           go to 9350
         end if
         iline=iline+1
         read(10,'(a)',end=699) parsource
         asection='* parameter values'
407      continue
         iline=iline+1
         read(10,'(a)',end=699) cline
         if(cline.eq.' ') go to 407
         call lowcas(cline)
         if(index(cline,trim(asection)).eq.0) then
           call writint(aline,iline)
           go to 9350
         end if
         do ipar=1,npar
           iline=iline+1
           read(10,*,err=9400,end=699) pval(ipar)
         end do
         asection='* model output values'
406      continue
         iline=iline+1
         read(10,'(a)',end=699) cline
         if(cline.eq.' ') go to 406
         call lowcas(cline)
         if(index(cline,trim(asection)).eq.0) then
           call writint(aline,iline)
           go to 9350
         end if
         ireject=0
         do iobs=1,nobs
           iline=iline+1
           read(10,*,err=9400,end=699) oval(iobs)
           if(oval(iobs).lt.-1.0e35) ireject=1
         end do
         if(ireject.eq.0)then
           numset=numset+1
           if(numset.ne.iindex) numindchange=numindchange+1
           write(20,510)
510        format('* parameter set index')
           write(20,512) numset
512        format(i10)
           write(20,514)
514        format('* parameter values source')
           write(20,'(a)') trim(parsource)
           write(20,516)
516        format('* parameter values')
           do ipar=1,npar
             write(20,518) pval(ipar)
518          format(1x,1pg14.7)
           end do
           write(20,520)
520        format('* model output values')
           do iobs=1,nobs
             write(20,518) oval(iobs)
           end do
         else
           numsetreject=numsetreject+1
         end if
       end do

699    continue
       incomplete=1
700    continue
       if(numsetread.eq.0)then
         write(errmsg,710) trim(qrrffile1)
710      format('No complete parameter/model-output sets found in ',
     +   'file ',a,'.')
         go to 9890
       end if
       close(unit=20)
       call writint(anum,numsetread)
       write(6,720) trim(anum),trim(qrrffile1)
720    format(' - ',a,' parameter sets found in file ',a,'.')
       call writint(anum,numset)
       write(6,730) trim(anum),trim(qrrffile2)
730    format(' - ',a,' parameter sets written to file ',a,'.')
       write(6,*)
       write(6,740)
740    format(' Statistics:-')
       call writint(anum,numsetreject)
       write(6,750) trim(anum)
750    format('   Number of failed model runs:         ',a)
       if(incomplete.eq.1)then
         ayn='yes'
       else
         ayn='no'
       end if
       write(6,760) trim(ayn)
760    format('   Incomplete final parameter set?      ',a)
       call writint(anum,numindchange)
       write(6,770) trim(anum)
770    format('   Number of altered param set indices: ',a)


       go to 9900

8900   continue
       write(6,8910)
8910   format(/,'RRFCLEAN is run using the command:',/)
       write(6,8920)
8920   format('    rrfclean rrffile1 rrffile2',/)
       write(6,8930)
8930   format('where',/)
       write(6,8950)
8950   format('    rrffile1 is an existing run results file, and')
       write(6,8980)
8980   format('    rrffile2 is a cleaned run results file.')
       go to 9999


9000   write(errmsg,9010) trim(qrrffile1)
9010   format('Error or premature end encountered to run results ',
     + 'file ',a,' while reading header section of this file.')
       go to 9890

9050   write(errmsg,9060) trim(asection),trim(qrrffile1)
9060   format('Cannot find "',a,'" section in file ',a,'.')
       go to 9890

9100   write(errmsg,9110) trim(asection),trim(qrrffile1)
9110   format('Error reading data from "',a,'" section of file ',a,'.')
       go to 9890

9150   call writint(aline,iline)
       write(errmsg,9160) trim(aline),trim(qrrffile1)
9160   format('Cannot read parameter set index from line ',a,
     + ' of file ',a,'.')
       go to 9890

9200   write(errmsg,9210)
9210   format('Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9890

9350   write(errmsg,9360) trim(asection),trim(aline),trim(qrrffile1)
9360   format('"',a,'" header expected at line ',a,' of file ',a,'.')
       go to 9890

9400   call writint(aline,iline)
       write(errmsg,9410) trim(aline),trim(qrrffile1)
9410   format('Error reading data from line ',a,' of file ',a,'.')
       go to 9890

9890   continue
       errmsg=' '//trim(errmsg)
       call writmess(6,errmsg)
       close(unit=20,status='delete',iostat=ierr)

9900   continue
       close(unit=20,status='delete',iostat=ierr)
       close(unit=10,iostat=ierr)

       deallocate(pval,oval,stat=ierr)

9999   continue

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


