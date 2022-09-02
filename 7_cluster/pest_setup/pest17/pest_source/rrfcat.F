       program rrfcat

C -- Program RRFCAT appends one run results file to another.

       implicit none

       integer       :: ierr,ifail,i,itemp
       integer       :: iline1,iline2,jline
       integer       :: ireject
       integer       :: npar,nobs
       integer       :: npar2,nobs2
       integer       :: ipar,jpar,iobs
       integer       :: index_out,icount1_in_ok,icount1_in_nok,
     +                  icount2_in_ok,icount2_in_nok
       integer       :: lw(10),rw(10)

       character*10   :: aline
       character*12   :: aversion
       character*12   :: atemp12
       character*20   :: atemp20
       character*35   :: atype
       character*50   :: asection
       character*256  :: rrffile1,rrffile2,rrffile3
       character*256  :: qrrffile1,qrrffile2,qrrffile3
       character*256  :: qinfile,parsource
       character*500  :: cline
       character*1000 :: errmsg

       integer, allocatable :: ifound_apar(:),
     +                         ifound_aobs(:)
       integer, allocatable :: jfound_apar(:),
     +                         jfound_aobs(:)
       character*12, allocatable :: apar(:)
       character*20, allocatable :: aobs(:)

       double precision, allocatable :: pval(:),oval(:)

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' RRFCAT Version ',a,
     + '. Watermark Numerical Computing.')

#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) cline
#else
       call pgetcl(cline)
#endif

C -- The RRFCAT command line is read.

       if(cline.eq.' ') go to 8900
       call spacesub(cline)
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.ne.0) go to 8900
       rrffile1=cline(lw(1):rw(1))
       call remchar(rrffile1,char(211))
       rrffile2=cline(lw(2):rw(2))
       call remchar(rrffile2,char(211))
       rrffile3=cline(lw(3):rw(3))
       call remchar(rrffile3,char(211))

C -- Quoted filenames are made.

       call addquote(rrffile1,qrrffile1)
       call addquote(rrffile2,qrrffile2)
       call addquote(rrffile3,qrrffile3)

C -- The two existing run results files are opened.

       open(unit=10,file=rrffile1,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(errmsg,20) trim(qrrffile1)
20       format('Cannot open run results file ',a,'.')
         go to 9890
       end if
       open(unit=11,file=rrffile2,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(errmsg,20) trim(qrrffile2)
         go to 9890
       end if

C -- The first two lines of the header to the first file is read.

       iline1=0
       qinfile=qrrffile1

C -- Information is obtained from the header to the first file.

       asection='* case dimensions'
       iline1=iline1+1
       read(10,'(a)',err=9000,end=9050) cline
       call lowcas(cline)
       if(index(cline,trim(asection)).eq.0) then
         call writint(aline,iline1)
         write(errmsg,153) trim(asection),trim(aline),trim(qinfile)
153      format('"',a,'" section header expected at line ',
     +   a,' of file ',a,'.')
         go to 9890
       end if
       iline1=iline1+1
       read(10,*,err=9100,end=9100) npar,nobs
       atype='NPAR'
       if(npar.le.0)then
         call writint(aline,iline1)
         write(errmsg,160) trim(atype),trim(aline),trim(qinfile)
160      format(a,' must be 1 or greater at line ',a,' of file ',
     +   a,'.')
         go to 9890
       end if
       if(nobs.le.0)then
         atype='NOBS'
         call writint(aline,iline1)
         write(errmsg,160) trim(atype),trim(aline),trim(qinfile)
         go to 9890
       end if

C -- The header to the second file is read and checked for
C    compatibility with the first file.

       iline2=0
       qinfile=qrrffile2
       iline2=iline2+1
       read(11,'(a)',end=9050,err=9000) cline
       call lowcas(cline)
       if(index(cline,trim(asection)).eq.0) then
         call writint(aline,iline2)
         write(errmsg,153) trim(asection),trim(aline),trim(qinfile)
         go to 9890
       end if
       iline2=iline2+1
       read(11,*,err=9100,end=9100) npar2,nobs2
       atype='NPAR'
       if(npar2.le.0)then
         call writint(aline,iline2)
         write(errmsg,160) trim(atype),trim(aline),trim(qinfile)
         go to 9890
       end if
       if(nobs2.le.0)then
         atype='NOBS'
         call writint(aline,iline2)
         write(errmsg,160) trim(atype),trim(aline),trim(qinfile)
         go to 9890
       end if

       if(npar.ne.npar2)then
         write(errmsg,165)
165      format('The two existing run results files contain different ',
     +   'numbers of parameters.')
         go to 9890
       end if
       if(nobs.ne.nobs2)then
         write(errmsg,166)
166      format('The two existing run results files contain ',
     +   'different numbers of observations.')
         go to 9890
       end if

! -- Memory is allocated.

       allocate(apar(npar),ifound_apar(npar),pval(npar),stat=ierr)
       if(ierr.ne.0) go to 9200
       ifound_apar=0                 ! an array
       allocate(jfound_apar(npar),stat=ierr)
       if(ierr.ne.0) go to 9200
       jfound_apar=0                 ! an array
       allocate(aobs(nobs),ifound_aobs(nobs),oval(nobs),stat=ierr)
       if(ierr.ne.0) go to 9200
       ifound_aobs=0                 ! an array
       allocate(jfound_aobs(nobs),stat=ierr)
       if(ierr.ne.0) go to 9200
       jfound_aobs=0                 ! an array

! -- The "* parameter names" section of the header is now read.

       asection='* parameter names'
       qinfile=qrrffile1
       iline1=iline1+1
       read(10,'(a)',err=9000,end=9000)cline
       if(index(cline,trim(asection)).eq.0)then
         call writint(aline,iline1)
         write(errmsg,170) trim(asection),trim(aline),trim(qinfile)
170      format('"',a,'" section expected at line ',a,
     +   ' of file ',a,'.')
         go to 9890
       end if
       do ipar=1,npar
         iline1=iline1+1
         read(10,*,err=9100,end=9100) apar(ipar)
         call lowcas(apar(ipar))
         if(ipar.gt.1)then
           atemp12=apar(ipar)
           do jpar=1,ipar-1
             if(atemp12.eq.apar(jpar))then
               call writint(aline,iline1)
               write(errmsg,175) trim(aline),trim(qinfile)
175            format('Duplicate parameter name at line ',a,
     +         ' of file ',a,'.')
               go to 9890
             end if
           end do
         end if
       end do
       qinfile=qrrffile2
       iline2=iline2+1
       read(11,'(a)',err=9000,end=9000)cline
       if(index(cline,trim(asection)).eq.0)then
           call writint(aline,iline2)
           write(errmsg,170) trim(asection),trim(aline),trim(qinfile)
           go to 9890
         end if
       do i=1,npar
         iline2=iline2+1
         read(11,*,err=9100,end=9100) atemp12
         call lowcas(atemp12)
         call which1(ifail,npar,ipar,apar,atemp12)
         if(ifail.ne.0) then
           write(errmsg,180) trim(atemp12),trim(qrrffile2),
     +                       trim(qrrffile1)
180        format('Parameter "',a,'" cited in file ',a,' is not ',
     +     'cited in file ',a,'.')
           go to 9890
         end if
         if(jfound_apar(ipar).ne.0) then
           write(errmsg,190) trim(atemp12),trim(qrrffile2)
190        format('Parameter name "',a,'" duplicated in ',
     +     '"* parameter names" section of file ',a,'.')
           go to 9890
         end if
         ifound_apar(i)=ipar
         jfound_apar(ipar)=i
       end do

       asection='* observation names'
       qinfile=qrrffile1
       iline1=iline1+1
       read(10,'(a)',err=9000,end=9000)cline
       if(index(cline,trim(asection)).eq.0)then
         call writint(aline,iline1)
         write(errmsg,170) trim(asection),trim(aline),trim(qinfile)
         go to 9890
       end if
       do iobs=1,nobs
         iline1=iline1+1
         read(10,*,err=9100,end=9100) aobs(iobs)
         call lowcas(aobs(iobs))
       end do
       qinfile=qrrffile2
       iline2=iline2+1
       read(11,'(a)',end=9000,err=9000)cline
       if(index(cline,trim(asection)).eq.0)then
         call writint(aline,iline2)
         write(errmsg,170) trim(asection),trim(aline),trim(qinfile)
         go to 9890
       end if
       do i=1,nobs
         iline2=iline2+1
         read(11,*,err=9100,end=9100) atemp20
         call lowcas(atemp20)
         call which1(ifail,nobs,iobs,aobs,atemp20)
         if(ifail.ne.0) then
           write(errmsg,200) trim(atemp20),trim(qrrffile2),
     +     trim(qrrffile1)
200        format('Observation "',a,'" cited in file ',a,' is not ',
     +     'cited in file ',a,'.')
           go to 9890
         end if
         if(jfound_aobs(iobs).ne.0) then
           write(errmsg,210) trim(atemp20),trim(qrrffile2)
210        format('Observation name "',a,'" is duplicated in ',
     +     '"* observation names" section of file ',a,'.')
           go to 9890
         end if
         ifound_aobs(i)=iobs
         jfound_aobs(iobs)=i
       end do

C -- All checks for compatibility have been done. The header to the new file is now written.

       open(unit=20,file=rrffile3)
       write(20,310)
310    format('* case dimensions')
       write(20,320) npar,nobs
320    format(5i10)
       write(20,330)
330    format('* parameter names')
       do ipar=1,npar
         write(20,'(a)') trim(apar(ipar))
       end do
       write(20,350)
350    format('* observation names')
       do iobs=1,nobs
         write(20,'(a)') trim(aobs(iobs))
       end do

! -- Data from the first file is now transferred to the new file.

       index_out=0
       icount1_in_ok=0
       icount1_in_nok=0
       qinfile=qrrffile1
       do
         asection='* parameter set index'
400      continue
         iline1=iline1+1
         read(10,'(a)',end=700) cline
         if(cline.eq.' ') go to 400
         call lowcas(cline)
         if(index(cline,trim(asection)).eq.0) then
           call writint(aline,iline1)
           go to 9350
         end if
         iline1=iline1+1
         jline=iline1
         read(10,*,err=9150,end=700) itemp
         asection='* parameter values source'
405      continue
         iline1=iline1+1
         read(10,'(a)',end=700) cline
         if(cline.eq.' ') go to 405
         call lowcas(cline)
         if(index(cline,trim(asection)).eq.0) then
           call writint(aline,iline1)
           go to 9350
         end if
         iline1=iline1+1
         read(10,'(a)',end=700) parsource
         parsource=adjustl(parsource)
         asection='* parameter values'
407      continue
         iline1=iline1+1
         read(10,'(a)',end=700) cline
         if(cline.eq.' ') go to 407
         call lowcas(cline)
         if(index(cline,trim(asection)).eq.0) then
           call writint(aline,iline1)
           go to 9350
         end if
         do ipar=1,npar
           iline1=iline1+1
           jline=iline1
           read(10,*,err=9400,end=700) pval(ipar)
         end do
         asection='* model output values'
406      continue
         iline1=iline1+1
         read(10,'(a)',end=700) cline
         if(cline.eq.' ') go to 406
         call lowcas(cline)
         if(index(cline,trim(asection)).eq.0) then
           call writint(aline,iline1)
           go to 9350
         end if
         ireject=0
         do iobs=1,nobs
           iline1=iline1+1
           jline=iline1
           read(10,*,err=9400,end=700) oval(iobs)
           if(oval(iobs).lt.-1.0e35) ireject=1
         end do
         if(ireject.eq.1)then
           icount1_in_nok=icount1_in_nok+1
         else
           icount1_in_ok=icount1_in_ok+1
           index_out=index_out+1
           write(20,410)
410        format('* parameter set index')
           call writint(atemp12,index_out)
           write(20,'(a)') trim(atemp12)
           write(20,420)
420        format('* parameter values source')
           write(20,'(a)') trim(parsource)
           write(20,430)
430        format('* parameter values')
           do ipar=1,npar
             write(20,440) pval(ipar)
440          format(1x,1pg14.7)
           end do
           write(20,450)
450        format('* model output values')
           do iobs=1,nobs
             write(20,440) oval(iobs)
           end do
         end if
       end do

700    continue

C -- Contents of the second file are now transferred.

       icount2_in_ok=0
       icount2_in_nok=0
       qinfile=qrrffile2
       do
         asection='* parameter set index'
702      continue
         iline2=iline2+1
         read(11,'(a)',end=900) cline
         if(cline.eq.' ') go to 702
         call lowcas(cline)
         if(index(cline,trim(asection)).eq.0) then
           call writint(aline,iline2)
           go to 9350
         end if
         iline2=iline2+1
         jline=iline2
         read(11,*,err=9150,end=900) itemp
         asection='* parameter values source'
703      continue
         iline2=iline2+1
         read(11,'(a)',end=900) cline
         if(cline.eq.' ') go to 703
         call lowcas(cline)
         if(index(cline,trim(asection)).eq.0) then
           call writint(aline,iline2)
           go to 9350
         end if
         iline2=iline2+1
         read(11,'(a)',end=900) parsource
         parsource=adjustl(parsource)
         asection='* parameter values'
704      continue
         iline2=iline2+1
         read(11,'(a)',end=900) cline
         if(cline.eq.' ') go to 704
         call lowcas(cline)
         if(index(cline,trim(asection)).eq.0) then
           call writint(aline,iline2)
           go to 9350
         end if
         do ipar=1,npar
           iline2=iline2+1
           jline=iline2
           read(11,*,err=9400,end=900) pval(ipar)
         end do
         asection='* model output values'
705      continue
         iline2=iline2+1
         read(11,'(a)',end=900) cline
         if(cline.eq.' ') go to 705
         call lowcas(cline)
         if(index(cline,trim(asection)).eq.0) then
           call writint(aline,iline2)
           go to 9350
         end if
         ireject=0
         do iobs=1,nobs
           iline2=iline2+1
           jline=iline2
           read(11,*,err=9400,end=900) oval(iobs)
           if(oval(iobs).lt.-1.0e35) ireject=1
         end do
         if(ireject.eq.1)then
           icount2_in_nok=icount2_in_nok+1
         else
           icount2_in_ok=icount2_in_ok+1
           index_out=index_out+1
           write(20,410)
           call writint(atemp12,index_out)
           write(20,'(a)') trim(atemp12)
           write(20,420)
           write(20,'(a)') trim(parsource)
           write(20,430)
           do i=1,npar
             ipar=ifound_apar(i)
             write(20,440) pval(ipar)
           end do
           write(20,450)
           do i=1,nobs
             iobs=ifound_aobs(i)
             write(20,440) oval(iobs)
           end do
         end if
       end do

900    continue

C -- Tidy up and reporting.

       write(6,*)
       itemp=icount1_in_ok+icount1_in_nok
       call writint(atemp12,itemp)
       write(6,920) trim(atemp12)
920    format(' - ',a,' run results found in first run results file.')
       call writint(atemp12,icount1_in_ok)
       write(6,930) trim(atemp12)
930    format(' - ',a,' of these transferred to output run results ',
     + 'file.')

       write(6,*)
       itemp=icount2_in_ok+icount2_in_nok
       call writint(atemp12,itemp)
       write(6,940) trim(atemp12)
940    format(' - ',a,' run results found in second run results file.')
       call writint(atemp12,icount2_in_ok)
       write(6,950) trim(atemp12)
950    format(' - ',a,' of these transferred to output run results ',
     + 'file.')

       write(6,*)
       call writint(atemp12,index_out)
       write(6,960) trim(atemp12)
960    format(' - ',a,' results written to output run record file.')
       write(6,970) trim(qrrffile3)
970    format(' - file ',a,' written ok.')

       go to 9900

8900   continue
       write(6,8910)
8910   format(/,' RRFCAT is run using the command:',/)
       write(6,8920)
8920   format('     rrfcat rrffile1 rrffile2 rrffile3',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8950)
8950   format('     rrffile1 is an existing run results file,')
       write(6,8980)
8980   format('     rrffile2 is another existing run results file, and')
       write(6,8990)
8990   format('     rrffile3 is a new run results file ',
     +        'which concatenates the two.')
       go to 9999

9000   write(errmsg,9010) trim(qinfile)
9010   format('Error or premature end encountered to run results ',
     + 'file ',a,' while reading header section of this file.')
       go to 9890

9050   write(errmsg,9060) trim(asection),trim(qinfile)
9060   format('Cannot find "',a,'" section in file ',a,'.')
       go to 9890

9100   write(errmsg,9110) trim(asection),trim(qinfile)
9110   format('Error reading data from "',a,'" section of file ',a,'.')
       go to 9890

9150   call writint(aline,jline)
       write(errmsg,9160) trim(aline),trim(qinfile)
9160   format('Cannot read parameter set index from line ',a,
     + ' of file ',a,'.')
       go to 9890

9200   write(errmsg,9210)
9210   format('Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9890

9350   write(errmsg,9360) trim(asection),trim(aline),trim(qinfile)
9360   format('"',a,'" header expected at line ',a,' of file ',a,'.')
       go to 9890

9400   call writint(aline,jline)
       write(errmsg,9410) trim(aline),trim(qinfile)
9410   format('Error reading data from line ',a,' of file ',a,'.')
       go to 9890

9890   continue
       errmsg=' '//trim(errmsg)
       call writmess(6,errmsg)
       close(unit=20,status='delete',iostat=ierr)

9900   continue
       close(unit=20,iostat=ierr)
       close(unit=10,iostat=ierr)
       close(unit=11,iostat=ierr)

       deallocate(ifound_apar,ifound_aobs,stat=ierr)
       deallocate(jfound_apar,jfound_aobs,stat=ierr)
       deallocate(apar,stat=ierr)
       deallocate(aobs,stat=ierr)
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


        subroutine getfile(ifail,cline,filename,ibeg,iend)

C-- Subroutine getfile extracts a filename from a string.

C -- Arguments are as follows:-
C       ifail: returned as zero if filename successfully read
C       cline: a character string containing the file name
C       filename: the name of the file read from the string
C       ibeg: character position at which to begin search for filename
C       iend: on input  - character position at which to end search for filename
C             on output - character postion at which filename ends


        integer, intent(out)               :: ifail
        integer, intent(in)                :: ibeg
        integer, intent(inout)             :: iend
        character (len=*), intent(in)      :: cline
        character (len=*), intent(out)     :: filename

        integer                            :: i,j,k
        character (len=1)                  :: aa

        ifail=0
        do i=ibeg,iend
          aa=cline(i:i)
          if((aa.ne.' ').and.(aa.ne.',').and.(aa.ne.char(9)))go to 50
        end do
        ifail=1
        return

50      if((aa.eq.'"').or.(aa.eq.''''))then
          do j=i+1,iend
            if(cline(j:j).eq.aa) go to 60
          end do
          ifail=1
          return
60        iend=j
          if(i+1.gt.j-1)then
            ifail=1
            return
          else
            filename=cline(i+1:j-1)
          end if
        else
          do j=i+1,iend
            if((cline(j:j).eq.' ').or.(cline(j:j).eq.',').or.
     +         (cline(j:j).eq.char(9)))then
              k=j-1
              go to 100
            end if
          end do
          k=iend
100       filename=cline(i:k)
          if(cline(k:k).eq.'"')then
            ifail=1
            return
          else if(cline(k:k).eq.'''')then
            ifail=1
            return
          end if

          iend=k
        end if
        filename=adjustl(filename)
        return

        end


