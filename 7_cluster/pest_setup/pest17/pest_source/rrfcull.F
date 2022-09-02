       program rrfcull

C -- Program RRFCULL removes parameter/model output sets from a run results file.

       implicit none

       integer, parameter :: MAXCULL=10000
       integer, parameter :: MAXOUT=10000
       integer ifail,ierr,iline,itemp,ii
       integer icull,jcull,ncull,iout,iin,icount
       integer iindex,cullflag
       integer npar,nobs,ipar,iobs
       integer lw(10),rw(10)
       integer ipset(MAXCULL),rejectset(MAXOUT)
       double precision pval,oval
       character*10 aline,acull,atype,anum,aindex
       character*12 aversion
       character*12 aapar
       character*20 aaobs
       character*40 asection
       character*256 rrfinfile,rrfoutfile,cullfile
       character*256 qrrfinfile,qrrfoutfile,qcullfile
       character*500 cline
       character*1500 errmsg

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' RRFCULL Version ',a,
     + '. Watermark Numerical Computing.')

#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) cline
#else
       call pgetcl(cline)
#endif

C -- Initialization

       write(6,*)
       rejectset=0    ! an array

C -- The RRFCULL command line is read.

       if(cline.eq.' ') go to 8900
       call spacesub(cline)
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.ne.0) go to 8900
       rrfinfile=cline(lw(1):rw(1))
       call remchar(rrfinfile,char(211))
       cullfile=cline(lw(2):rw(2))
       call remchar(cullfile,char(211))
       rrfoutfile=cline(lw(3):rw(3))
       call remchar(rrfoutfile,char(211))

C -- Quoted filenames are made.

       call addquote(rrfinfile,qrrfinfile)
       call addquote(cullfile,qcullfile)
       call addquote(rrfoutfile,qrrfoutfile)

C -- The existing run results file is opened.

       open(unit=10,file=rrfinfile,status='old',action='read',
     + iostat=ierr)
       if(ierr.ne.0)then
         write(errmsg,20) trim(qrrfinfile)
20       format('Cannot open run results file ',a,'.')
         go to 9890
       end if

! -- The new run results file is opened.

       open(unit=20,file=rrfoutfile,status='old',action='write',
     + iostat=ierr)
       if(ierr.ne.0)then
         write(errmsg,21) trim(qrrfoutfile)
21       format('Cannot write to file ',a,'.')
         go to 9890
       end if

C -- The culling file is opened.

       open(unit=11,file=cullfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(errmsg,22) trim(qcullfile)
22       format('Cannot open culling file ',a,'.')
         go to 9890
       end if

C -- The contents of the culling file are read.

      iline=0
      do
800     continue
        iline=iline+1
        read(11,'(a)',end=850) cline
        if(cline.eq.' ') go to 800
        cline=adjustl(cline)
        if(cline(1:1).eq.'#') go to 800
        icull=icull+1
        if(icull.gt.MAXCULL)then
          write(errmsg,810) trim(qcullfile)
810       format('Too many entries in file ',a,'. Increase ',
     +    'MAXCULL and re-compile program.')
          go to 9890
        end if
        call linspl(ifail,1,lw,rw,cline)
        call intread(ifail,cline(lw(1):rw(1)),ipset(icull))
        if(ifail.ne.0)then
          call writint(aline,iline)
          write(errmsg,820) trim(aline),trim(qcullfile)
820       format('Cannot read parameter set index at line ',a,
     +    ' of file ',a,'.')
          go to 9890
        end if
        if(ipset(icull).le.0)then
          call writint(aline,iline)
          write(errmsg,830) trim(aline),trim(qcullfile)
830       format('Parameter set index must be positive at line ',a,
     +    ' of file ',a,'.')
          go to 9890
        end if
        if(icull.gt.1)then
          itemp=ipset(icull)
          do jcull=1,icull-1
            if(itemp.eq.ipset(jcull))then
              call writint(aline,iline)
              write(errmsg,840) trim(aline),trim(qcullfile)
840           format('Parameter set index repeated at line ',a,
     +        ' of file ',a,'.')
              go to 9890
            end if
          end do
        end if
      end do
850   continue
      ncull=icull
      if(ncull.eq.0)then
        write(errmsg,860) trim(qcullfile)
860     format('No culling indices found in file ',a,'.')
        go to 9890
      end if
      call writint(acull,ncull)
      write(6,870) trim(acull),trim(qcullfile)
870   format(' - ',a,' culling indices read from file ',a)

C -- We now read the first RRF file and write the second RRF file.

C -- First we transfer the header to the new file.

       iline=0
       asection='* case dimensions'
       iline=iline+1
       read(10,'(a)',err=9000,end=9050) cline
       call lowcas(cline)
       if(index(cline,trim(asection)).eq.0) then
         call writint(aline,iline)
         write(errmsg,153) trim(asection),trim(aline),trim(qrrfinfile)
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
         write(errmsg,160) trim(atype),trim(aline),trim(qrrfinfile)
160      format(a,' must be 1 or greater at line ',a,' of file ',
     +   a,'.')
         go to 9890
       end if
       if(nobs.le.0)then
         atype='NOBS'
         call writint(aline,iline)
         write(errmsg,160) trim(atype),trim(aline),trim(qrrfinfile)
         go to 9890
       end if
       write(20,154) npar,nobs
154    format(2i10)

! -- The "* parameter names" section of the header is now read.

       asection='* parameter names'
       iline=iline+1
       read(10,'(a)',err=9000,end=9000)cline
       if(index(cline,trim(asection)).eq.0)then
         call writint(aline,iline)
         write(errmsg,170) trim(asection),trim(aline),trim(qrrfinfile)
170      format('"',a,'" section expected at line ',a,
     +   ' of file ',a,'.')
         go to 9890
       end if
       write(20,'(a)') trim(cline)
       do ipar=1,npar
         iline=iline+1
         read(10,*,err=9100,end=9100) aapar
         call lowcas(aapar)
         write(20,'(1x,a)') trim(aapar)
       end do
       asection='* observation names'
       iline=iline+1
       read(10,'(a)',err=9000,end=9000)cline
       if(index(cline,trim(asection)).eq.0)then
         call writint(aline,iline)
         write(errmsg,170) trim(asection),trim(aline),trim(qrrfinfile)
         go to 9890
       end if
       write(20,'(a)') trim(cline)
       do iobs=1,nobs
         iline=iline+1
         read(10,*,err=9100,end=9100) aaobs
         call lowcas(aaobs)
         write(20,'(1x,a)') trim(aaobs)
       end do

! -- Data from the first file is now transferred to the new file.

       iout=0
       iin=0
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
         cullflag=0
         do icull=1,ncull
           if(iindex.eq.ipset(icull))then
             cullflag=1
             ipset(icull)=-ipset(icull)
             go to 402
           end if
         end do
         write(20,401)
401      format('* parameter set index')
         iout=iout+1
         if(iout.gt.MAXOUT)then
           write(errmsg,390) trim(qrrfinfile)
390        format('Too many parameter sets transferred to new file ',
     +     a,'. Increase MAXOUT and re-compile program.')
           go to 9890
         end if
         call writint(anum,iout)
         write(20,'(1x,a)') trim(anum)
402      continue
         iin=iin+1
         asection='* parameter values source'
405      continue
         iline=iline+1
         read(10,'(a)',end=9420) cline
         if(cline.eq.' ') go to 405
         call lowcas(cline)
         if(index(cline,trim(asection)).eq.0) then
           call writint(aline,iline)
           go to 9350
         else
           if(cullflag.eq.0) write(20,'(a)') trim(cline)
         end if
         iline=iline+1
         read(10,'(a)',end=9420)cline
         if(cline.eq.' ')then
           call writint(aline,iline)
           write(errmsg,408) trim(aline),trim(rrfinfile)
408        format('Blank parameter values source at line ',a,' of ',
     +     'file ',a,'.')
           go to 9890
         end if
         if(cullflag.eq.0) write(20,'(a)') trim(cline)
         asection='* parameter values'
407      continue
         iline=iline+1
         read(10,'(a)',end=9420) cline
         if(cline.eq.' ') go to 407
         call lowcas(cline)
         if(index(cline,trim(asection)).eq.0) then
           call writint(aline,iline)
           go to 9350
         else
           if(cullflag.eq.0) write(20,'(a)') trim(cline)
         end if
         do ipar=1,npar
           iline=iline+1
           read(10,*,err=9400,end=9420) pval
           if(cullflag.eq.0) write(20,409) pval
409        format(1x,1pg14.7)
         end do
         asection='* model output values'
406      continue
         iline=iline+1
         read(10,'(a)',end=9420) cline
         if(cline.eq.' ') go to 406
         call lowcas(cline)
         if(index(cline,trim(asection)).eq.0) then
           call writint(aline,iline)
           go to 9350
         else
           if(cullflag.eq.0) write(20,'(a)') trim(cline)
         end if
         do iobs=1,nobs
           iline=iline+1
           read(10,*,err=9400,end=9420) oval
           if(oval.lt.-1.0e35) rejectset(iout)=1
           if(cullflag.eq.0) write(20,409) oval
         end do
       end do

700    continue

C -- The input and output files are closed.

       close(unit=10)
       close(unit=20)
       call writint(anum,iin)
       write(6,710) trim(anum),trim(qrrfinfile)
710    format(' - ',a,' run result sets read from file ',a)
       call writint(anum,iout)
       write(6,720) trim(anum),trim(qrrfoutfile)
720    format(' - ',a,' run result sets written to file ',a)

! -- We now check for warnings.

       icount=0
       do icull=1,ncull
         if(ipset(icull).gt.0) icount=icount+1
       end do
       if(icount.gt.0)then
         write(6,*)
         write(6,730)
730      format(' Warning: the following to-be-culled parameter set ',
     +   'indices were not found ')
         write(6,740) trim(qrrfinfile)
740      format(' in file ',a,'.')
         cline=' '
         do icull=1,ncull
           if(ipset(icull).gt.0)then
             call writint(acull,ipset(icull))
             if(cline.eq.' ')then
               cline=' '//trim(acull)
             else
               if(len_trim(cline).gt.74)then
                 write(6,'(a)') trim(cline)
                 cline=' '//trim(acull)
               else
                 cline=trim(cline)//' '//trim(acull)
               end if
             end if
           end if
         end do
         if(cline.ne.' ') write(6,'(a)') trim(cline)
       end if

       icount=0
       do ii=1,iout
         if(rejectset(iout).ne.0) icount=icount+1
       end do
       if(icount.gt.0)then
         write(6,*)
         write(6,750)
750      format(' Warning: the following parameter set indices in the ',
     +   'new run results file')
         write(6,760)
760      format(' have at least one observation value less than ',
     +   '-1.0e35, this indicating')
         write(6,780)
780      format(' a failed model run. These should be culled from ',
     +   'this new file.')
         cline=' '
         do ii=1,iout
           if(rejectset(ii).gt.0)then
             call writint(acull,ii)
             if(cline.eq.' ')then
               cline=' '//trim(acull)
             else
               if(len_trim(cline).gt.74)then
                 write(6,'(a)') trim(cline)
                 cline=' '//trim(acull)
               else
                 cline=trim(cline)//' '//trim(acull)
               end if
             end if
           end if
         end do
         if(cline.ne.' ') write(6,'(a)') trim(cline)
       end if

       go to 9900

8900   continue
       write(6,8910)
8910   format(/,' RRFCULL is run using the command:',/)
       write(6,8920)
8920   format('     rrfcull rrfinfile cullfile rrfoutfile',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8950)
8950   format('     rrfinfile  is an existing run results file,')
       write(6,8980)
8980   format('     cullfile   lists parameter set indices to be ',
     + 'removed from the file, and')
       write(6,8990)
8990   format('     rrfoutfile is a new run results file.')
       go to 9999

9000   write(errmsg,9010) trim(qrrfinfile)
9010   format('Error or premature end encountered to run results ',
     + 'file ',a,' while reading header section of this file.')
       go to 9890

9050   write(errmsg,9060) trim(asection),trim(qrrfinfile)
9060   format('Cannot find "',a,'" section in file ',a,'.')
       go to 9890

9100   write(errmsg,9110) trim(asection),trim(qrrfinfile)
9110   format('Error reading data from "',a,'" section of file ',a,'.')
       go to 9890

9150   call writint(aline,iline)
       write(errmsg,9160) trim(aline),trim(qrrfinfile)
9160   format('Cannot read parameter set index from line ',a,
     + ' of file ',a,'.')
       go to 9890

9350   write(errmsg,9360) trim(asection),trim(aline),trim(qrrfinfile)
9360   format('"',a,'" header expected at line ',a,' of file ',a,'.')
       go to 9890

9400   call writint(aline,iline)
       write(errmsg,9410) trim(aline),trim(qrrfinfile)
9410   format('Error reading data from line ',a,' of file ',a,'.')
       go to 9890

9420   call writint(aindex,iindex)
       write(errmsg,9430) trim(qrrfinfile),trim(aindex)
9430   format('Unexpected end encountered to file ',a,' while ',
     + 'reading data for parameter set index ',a,'.')
       go to 9890

9890   continue
       errmsg=' '//trim(errmsg)
       call writmess(6,errmsg)
!       close(unit=20,status='delete',iostat=ierr)

9900   continue
       close(unit=20,iostat=ierr)
       close(unit=10,iostat=ierr)

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

