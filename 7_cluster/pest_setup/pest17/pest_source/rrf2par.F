       program rrf2par

C -- Program RRF2PAR reads a run results file. It builds a series of PEST parameter value files
C    based on parameter sets recorded in that file.

       implicit none

       integer       :: i,ierr,iline,ifail
       integer       :: npar,ipar
       integer       :: istart,ifin
       integer       :: nobs,npargp,nobsgp,nprior
       integer       :: lastindex,inum
       integer       :: icount,ncount
       integer       :: lw(10),rw(10)

       character*10   :: aversion
       character*10   :: anum,aline
       character*50   :: asection
       character*256  :: rrffile,qrrffile,parbase,outfile
       character*500  :: cline
       character*1000 :: errmsg

       integer, allocatable          :: found(:)
       double precision, allocatable :: pval(:)
       character*12, allocatable     :: apar(:)

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' RRF2PAR Version ',a,
     + '. Watermark Numerical Computing.')

#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) cline
#else
       call pgetcl(cline)
#endif

C -- The RRF2PAR command line is read.

       if(cline.eq.' ') go to 8900
       call spacesub(cline)
       call linspl(ifail,5,lw,rw,cline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.ne.0) go to 8900
       rrffile=cline(lw(1):rw(1))
       call remchar(rrffile,char(211))
       parbase=cline(lw(2):rw(2))
       call remchar(parbase,char(211))
       call intread(ifail,cline(lw(3):rw(3)),istart)
       if(ifail.ne.0) go to 8900
       call intread(ifail,cline(lw(4):rw(4)),ifin)
       if(ifail.ne.0) go to 8900

#ifndef UNIX
       call lowcas(rrffile)
       call lowcas(parbase)
#endif


C -- Some aspects of the command line are checked for error.

       if(istart.lt.1) then
         write(errmsg,120)
120      format('Starting parameter set index must be 1 or greater.')
         go to 9890
       end if
       if(ifin.lt.istart) then
         write(errmsg,130)
130      format('Finishing parameter set index must not be less than',
     +   ' starting parameter set index.')
         go to 9890
       end if
       call addquote(rrffile,qrrffile)
       ncount=ifin-istart+1
       icount=0

C -- The run results file is opened.

       open(unit=10,file=rrffile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(errmsg,140) trim(qrrffile)
140      format('Cannot open run results file ',a,'.')
         go to 9890
       end if
       iline=0

C -- Information is obtained from the header to this file.

       asection='* case dimensions'
       iline=iline+1
       read(10,'(a)',end=9050,err=9000) cline
       call lowcas(cline)
       if(index(cline,trim(asection)).eq.0) then
         call writint(aline,iline)
         write(errmsg,153) trim(aline),trim(qrrffile)
153      format('"* case dimensions" section header expected at line ',
     +   a,' of file ',a,'.')
         go to 9890
       end if
       iline=iline+1
       read(10,*,err=9100,end=9100) npar,nobs
       if(npar.le.0)then
         call writint(aline,iline)
         write(errmsg,160) trim(aline),trim(qrrffile)
160      format('NPAR must be 1 or greater at line ',a,' of file ',
     +   a,'.')
         go to 9890
       end if
       allocate(apar(npar),pval(npar),stat=ierr)
       if(ierr.ne.0) go to 9200
       asection='* parameter names'
       do
         iline=iline+1
         read(10,'(a)',end=9050,err=9000) cline
         call lowcas(cline)
         if(index(cline,trim(asection)).ne.0) exit
       end do
       do ipar=1,npar
         iline=iline+1
         read(10,*,err=9100,end=9100) apar(ipar)
         call lowcas(apar(ipar))
       end do

C -- More memory is allocated.

       allocate(found(istart:ifin),stat=ierr)
       if(ierr.ne.0) go to 9200
       found=0                    ! an array

! -- We now look for parameter sets.

       write(6,*)
       lastindex=0
       asection='* parameter set index'
165    continue
       do
         iline=iline+1
         read(10,'(a)',end=500,err=500) cline
         call lowcas(cline)
         if(index(cline,trim(asection)).ne.0) exit
       end do
       iline=iline+1
       read(10,*,err=9150,end=9150) inum
       if(inum.le.lastindex)then
         call writint(aline,iline)
         write(errmsg,170) trim(qrrffile),trim(aline)
170      format('Parameter set indices are not in increasing order ',
     +   'in file ',a,'. Error at line ',a,' of this file. Use ',
     +   'RRFCLEAN to fix this file.')
         go to 9890
       end if
       lastindex=inum
       if(inum.lt.istart) then
         go to 165
       else if(inum.le.ifin)then
         if(found(inum).ne.0)then
           call writint(anum,inum)
           write(errmsg,180) trim(anum),trim(aline),trim(rrffile)
180        format('More than one incidence of parameter set ',a,
     +     ' found in file ',a,'. Use RRFCLEAN to fix this file.')
           go to 9890
         end if
       else if(inum.gt.ifin) then
         go to 165
       end if
       do i=1,3
         iline=iline+1
         read(10,'(a)',err=500,end=500) cline
       end do
       call lowcas(cline)
       if(cline.ne.'* parameter values')then
         call writint(aline,iline)
         write(errmsg,190) trim(aline),trim(qrrffile)
190      format('Line ',a,' of file ',a,' expected to be ',
     +   '"* parameter values".')
         go to 9890
       end if
       do ipar=1,npar
         iline=iline+1
         read(10,*,iostat=ierr) pval(ipar)
         if(ierr.ne.0)then
           call writint(aline,iline)
           write(errmsg,200) trim(aline),trim(qrrffile)
200        format('Error reading parameter value from line ',a,
     +     ' of file ',a,'.')
           go to 9890
         end if
       end do
       call writint(anum,inum)
       outfile=trim(parbase)//trim(anum)//'.par'
       open(unit=20,file=outfile,action='write',iostat=ierr)
       if(ierr.ne.0)then
         write(errmsg,201) trim(outfile)
201      format('Cannot write to file ',a,'.')
         go to 9890
       end if
       write(20,210)
210    format('single point')
       do ipar=1,npar
         write(20,220) trim(apar(ipar)),pval(ipar)
220      format(1x,a,t15,1pg14.7,'   1.0    0.0')
       end do
       close(unit=20)
       write(6,230) trim(outfile)
230    format(' - file ',a,' written ok.')
       found(inum)=1
       icount=icount+1
       if(icount.eq.ncount) go to 500
       go to 165

! -- We tidy up.

500    continue
       close(unit=10)
       if(icount.eq.ncount) then
         write(6,*)
         write(6,240) trim(rrffile)
240      format(' - file ',a,' read ok.')
       else
         write(6,*)
         write(6,250)
250      format(' The following requested parameter set indices ',
     +   'not found in run results file:-')
         do i=istart,ifin
           if(found(i).eq.0)then
             call writint(anum,i)
             write(6,260) trim(anum)
260          format('    - parameter set index ',a)
           end if
         end do
       end if
       go to 9900

8900   continue
       write(6,8910)
8910   format(/,' RRF2PAR is run using the command:',/)
       write(6,8920)
8920   format('     rrf2par rrffile parfilebase istart ifinish',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8950)
8950   format('     rrffile     is the name of an existing run ',
     + 'results file,')
       write(6,8980)
8980   format('     parfilebase is the base name of a set of ',
     + 'output parameter value files,')
       write(6,8990)
8990   format('     istart      is the starting parameter set ',
     + 'index, and')
       write(6,8991)
8991   format('     ifinish     is the final parameter set ',
     + 'index.')
       go to 9999

9000   write(errmsg,9010) trim(qrrffile)
9010   format('Error or premature end encountered to run results ',
     + 'file ',a,' while reading header to this file.')
       go to 9890

9050   write(errmsg,9060) trim(asection),trim(qrrffile)
9060   format('Cannot find "',a,'" section in header to file ',a,'.')
       go to 9890

9100   write(errmsg,9110) trim(asection),trim(qrrffile)
9110   format('Error reading data from "',a,'" section in header ',
     + 'to file ',a,'.')
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

9890   continue
       errmsg=' '//trim(errmsg)
       call writmess(6,errmsg)

9900   continue
       deallocate(found,pval,apar,stat=ierr)

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

C -- Subroutine LOWCAS converts a string to lower case.

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

