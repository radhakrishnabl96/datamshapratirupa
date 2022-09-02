       program rrf2parobs

C -- Program RRF2PAROBS writes a PAROBS file by extracting a parameter/observation
C    set from a run results file.

       implicit none

       integer ifail,ierr,iline
       integer ipindex,iindex,ipar,iobs,npar,nobs
       integer iwrite,ireject
       integer lw(10),rw(10)
       character*10 anum,aline
       character*12 aversion
       character*30 asection,atype
       character*256 rrffile,parobsfile,qrrffile,qparobsfile
       character*300 parsource
       character*500 cline
       character*1500 errmsg

       double precision, allocatable :: pval(:),oval(:)
       character*12, allocatable :: apar(:)
       character*20, allocatable :: aobs(:)

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' RRF2PAROBS Version ',a,
     + '. Watermark Numerical Computing.')

#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) cline
#else
       call pgetcl(cline)
#endif

C -- The RRF2PAROBS command line is read.

       if(cline.eq.' ') go to 8900
       call spacesub(cline)
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.ne.0) go to 8900
       rrffile=cline(lw(1):rw(1))
       call remchar(rrffile,char(211))
       call intread(ifail,cline(lw(2):rw(2)),ipindex)
       if(ifail.ne.0) go to 8900
       parobsfile=cline(lw(3):rw(3))
       call remchar(parobsfile,char(211))

       write(6,*)
       if(ipindex.lt.1)then
         call writint(anum,ipindex)
         write(errmsg,18)
18       format('The parameter set index supplied with the ',
     +   'RRF2PAROBS command line must be positive.')
         go to 9890
       end if

C -- Quoted filenames are made.

       call addquote(rrffile,qrrffile)
       call addquote(parobsfile,qparobsfile)

C -- The run results file is opened.

       open(unit=10,file=rrffile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(errmsg,20) trim(qrrffile)
20       format('Cannot open run results file ',a,'.')
         go to 9890
       end if

C -- The PAROBS file is opened.

       open(unit=20,file=parobsfile,action='write',iostat=ierr)
       if(ierr.ne.0)then
         write(errmsg,22) trim(qparobsfile)
22       format('Cannot write to file ',a,'.')
         go to 9890
       end if

C -- The first two lines of the header to the rrf file are read.

       iline=0
       asection='* case dimensions'
       iline=iline+1
       read(10,'(a)',err=9000,end=9050) cline
       call lowcas(cline)
       if(index(cline,trim(asection)).eq.0) then
         call writint(aline,iline)
         write(errmsg,153) trim(asection),trim(aline),trim(qrrffile)
153      format('"',a,'" section header expected at line ',
     +   a,' of file ',a,'.')
         go to 9890
       end if
       iline=iline+1
       read(10,*,err=9100,end=9100) npar,nobs
       atype='NPAR'
       if(npar.le.0)then
         call writint(aline,iline)
         write(errmsg,160) trim(atype),trim(aline),trim(qrrffile)
160      format(a,' must be 1 or greater at line ',a,' of file ',
     +   a,'.')
         go to 9890
       end if
       if(nobs.le.0)then
         atype='NOBS'
         call writint(aline,iline)
         write(errmsg,160) trim(atype),trim(aline),trim(qrrffile)
         go to 9890
       end if

! -- Memory is allocated.

       allocate(apar(npar),pval(npar),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(aobs(nobs),oval(nobs),stat=ierr)
       if(ierr.ne.0) go to 9200

! -- The "* parameter names" section of the header is now read.

       asection='* parameter names'
       iline=iline+1
       read(10,'(a)',err=9000,end=9000)cline
       if(index(cline,trim(asection)).eq.0)then
         call writint(aline,iline)
         write(errmsg,170) trim(asection),trim(aline),trim(qrrffile)
170      format('"',a,'" section expected at line ',a,
     +   ' of file ',a,'.')
         go to 9890
       end if
       do ipar=1,npar
         iline=iline+1
         read(10,*,err=9100,end=9100) apar(ipar)
         call lowcas(apar(ipar))
       end do

       asection='* observation names'
       iline=iline+1
       read(10,'(a)',err=9000,end=9000)cline
       if(index(cline,trim(asection)).eq.0)then
         call writint(aline,iline)
         write(errmsg,170) trim(asection),trim(aline),trim(qrrffile)
         go to 9890
       end if
       do iobs=1,nobs
         iline=iline+1
         read(10,*,err=9100,end=9100) aobs(iobs)
         call lowcas(aobs(iobs))
       end do

C -- We now look for the parameter set index.

       iwrite=0
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
         iline=iline+1
         read(10,*,err=9150,end=700) iindex
         asection='* parameter values source'
405      continue
         iline=iline+1
         read(10,'(a)',end=700) cline
         if(cline.eq.' ') go to 405
         call lowcas(cline)
         if(index(cline,trim(asection)).eq.0) then
           call writint(aline,iline)
           go to 9350
         end if
         iline=iline+1
         read(10,'(a)',end=700) parsource
         asection='* parameter values'
407      continue
         iline=iline+1
         read(10,'(a)',end=700) cline
         if(cline.eq.' ') go to 407
         call lowcas(cline)
         if(index(cline,trim(asection)).eq.0) then
           call writint(aline,iline)
           go to 9350
         end if
         do ipar=1,npar
           iline=iline+1
           read(10,*,err=9400,end=700) pval(ipar)
         end do
         asection='* model output values'
406      continue
         iline=iline+1
         read(10,'(a)',end=700) cline
         if(cline.eq.' ') go to 406
         call lowcas(cline)
         if(index(cline,trim(asection)).eq.0) then
           call writint(aline,iline)
           go to 9350
         end if
         ireject=0
         do iobs=1,nobs
           iline=iline+1
           read(10,*,err=9400,end=700) oval(iobs)
           if(oval(iobs).lt.-1.0e35) ireject=1
         end do

! -- If appropriate, transfer data to the output file.

         if(iindex.eq.ipindex)then
           write(20,420)
420        format(' single point')
           do ipar=1,npar
             write(20,430) trim(apar(ipar)),pval(ipar)
430          format(1x,a,t15,1pg14.7,2x,'1.0    0.0')
           end do
           do iobs=1,nobs
             write(20,440) trim(aobs(iobs)),oval(iobs)
440          format(1x,a,t25,1pg14.7)
           end do
           iwrite=1
           go to 700
         end if

       end do

700    continue
       close(unit=10,iostat=ierr)
       if(iwrite.eq.0)then
         call writint(anum,ipindex)
         write(errmsg,710) trim(anum),trim(qrrffile)
710      format('Cannot find parameter set index ',a,' in file ',a,'.')
         go to 9890
       else
         close(unit=20)
         write(6,718) trim(qrrffile)
718      format(' - file ',a,' read ok.')
         write(6,720) trim(qparobsfile)
720      format(' - file ',a,' written ok.')
         if(ireject.ne.0)then
           write(6,*)
           write(6,730)
730        format(' Warning: transferred model output values ',
     +     'indicate run failure.')
         end if
       end if


       go to 9900

8900   continue
       write(6,8910)
8910   format(/,' RRF2PAROBS is run using the command:',/)
       write(6,8920)
8920   format('     rrf2parobs rrffile psetindex parobsfile',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8950)
8950   format('     rrffile    is an existing run results file,')
       write(6,8980)
8980   format('     psetindex  is a parameter set index, and')
       write(6,8990)
8990   format('     parobsfile is the PAROBS file which ',
     +        'must be written.')
       go to 9999

9000   write(errmsg,9010) trim(qrrffile)
9010   format('Error or premature end encountered to run results ',
     + 'file ',a,' while reading header section of this file.')
       go to 9890

9050   write(errmsg,9060) trim(asection),trim(qrrffile)
9060   format('Cannot find "',a,'" section in file ',a,'.')
       go to 9890

9100   write(errmsg,9110) trim(asection),trim(qrrffile)
9110   format('Error reading data from "',a,'" section of file ',a,'.')
       go to 9890

9150   call writint(aline,iline)
       write(errmsg,9160) trim(aline),trim(qrrffile)
9160   format('Cannot read parameter set index from line ',a,
     + ' of file ',a,'.')
       go to 9890

9200   write(errmsg,9210)
9210   format('Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9890

9350   write(errmsg,9360) trim(asection),trim(aline),trim(qrrffile)
9360   format('"',a,'" header expected at line ',a,' of file ',a,'.')
       go to 9890

9400   call writint(aline,iline)
       write(errmsg,9410) trim(aline),trim(qrrffile)
9410   format('Error reading data from line ',a,' of file ',a,'.')
       go to 9890

9890   continue
       errmsg=' '//trim(errmsg)
       call writmess(6,errmsg)
       close(unit=20,status='delete',iostat=ierr)

9900   continue
       close(unit=10,iostat=ierr)

       deallocate(pval,oval,stat=ierr)
       deallocate(apar,aobs,stat=ierr)

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
