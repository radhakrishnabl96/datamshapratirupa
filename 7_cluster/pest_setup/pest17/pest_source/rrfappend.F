       program rrfappend

C -- Program RRFAPPEND adds a set of parameter values and model outputs
C    to a run results file. These are read from a PAROBS file.

       implicit none

       logical rrfexist,nowobs,lexist,lopened
       integer ifail,ierr,iline,i
       integer iblank,ok_to_end
       integer iindex,ipar,iobs,npar,nobs
       integer lw(10),rw(10)
       character*10 aline,aindex
       character*12 aversion,aapar
       character*20 aaobs,atemp20
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
10     format(/,' RRFAPPEND Version ',a,
     + '. Watermark Numerical Computing.')

#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) cline
#else
       call pgetcl(cline)
#endif

C -- The RRFAPPEND command line is read.

       if(cline.eq.' ') go to 8900
       call spacesub(cline)
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,2,lw,rw,cline)
       if(ifail.ne.0) go to 8900
       parobsfile=cline(lw(1):rw(1))
       call remchar(parobsfile,char(211))
       rrffile=cline(lw(2):rw(2))
       call remchar(rrffile,char(211))

C -- Quoted filenames are made.

       call addquote(rrffile,qrrffile)
       call addquote(parobsfile,qparobsfile)

C -- The run results file is opened.

       write(6,*)
       inquire(file=rrffile,exist=lexist)
       if(lexist)then
         rrfexist=.true.
       else
         rrfexist=.false.
       end if
       if(rrfexist)then
         open(unit=10,file=rrffile,status='old',action='readwrite',
     +   iostat=ierr)
         if(ierr.ne.0)then
           write(errmsg,15) trim(qrrffile)
           go to 9890
         end if
       else
         open(unit=10,file=rrffile,status='new',action='write',
     +   iostat=ierr)
         if(ierr.ne.0)then
           write(errmsg,15) trim(qrrffile)
15         format('Cannot open file ',a,' for output.')
           go to 9890
         end if
       end if

C -- The PAROBS file is opened.

       open(unit=11,file=parobsfile,status='old',action='read',
     + iostat=ierr)
       if(ierr.ne.0)then
         write(errmsg,22) trim(qparobsfile)
22       format('Cannot open parobs file ',a,'.')
         go to 9890
       end if

C -- The first two lines of the header to the rrf file are read.

       if(.not.rrfexist) then
         iindex=0
         go to 7091
       end if
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

       do
         iblank=0
         ok_to_end=1
         asection='* parameter set index'
400      continue
         iline=iline+1
         read(10,'(a)',end=700) cline
         if(cline.eq.' ') then
           iblank=iblank+1
           go to 400
         end if
         ok_to_end=0
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
         do iobs=1,nobs
           iline=iline+1
           read(10,*,err=9400,end=700) oval(iobs)
         end do
       end do

700    continue
       backspace(unit=10)
       if(ok_to_end.eq.0)then
         write(10,*)
         write(10,*)
       else
         if(iblank.ne.0)then
           do i=1,iblank
             backspace(unit=10)
           end do
         end if
       end if
7091   continue

! -- The PAROBS file is now read.

       iline=1
       read(11,'(a)',err=9500,end=9520) cline
       call lowcas(cline)
       call linspl(ifail,2,lw,rw,cline)
       if(ifail.ne.0) go to 9560
       atemp20=cline(lw(1):rw(1))
       if((atemp20.ne.'single').and.(atemp20.ne.'double'))
     + go to 9560
       atemp20=cline(lw(2):rw(2))
       if((atemp20.ne.'point').and.(atemp20.ne.'nopoint'))
     + go to 9560
       if(.not.rrfexist)then
         nowobs=.false.
         npar=0
         nobs=0
         do
709        continue
           iline=iline+1
           read(11,'(a)',end=702,err=9500) cline
           if(cline.eq.' ') go to 709
           if(.not.nowobs)then
             call linspl(ifail,4,lw,rw,cline)
             if(ifail.eq.0)then
               npar=npar+1
             else
               call linspl(ifail,2,lw,rw,cline)
               if(ifail.ne.0) go to 9680
               nobs=nobs+1
               nowobs=.true.
             end if
           else
             call linspl(ifail,2,lw,rw,cline)
             if(ifail.ne.0) go to 9680
             nobs=nobs+1
           end if
         end do
702      continue
         if(npar.eq.0)then
           write(errmsg,703) 'parameters',trim(qparobsfile)
703        format('There appear to be no ',a,' feeatured ',
     +     'in PAROBS file ',a,'.')
           go to 9890
         end if
         if(nobs.eq.0)then
           write(errmsg,703) 'observations',trim(qrrffile)
           go to 9890
         end if
         allocate(pval(npar),apar(npar),stat=ierr)
         if(ierr.ne.0) go to 9200
         allocate(oval(nobs),aobs(nobs),stat=ierr)
         if(ierr.ne.0) go to 9200
         rewind(unit=11)
         read(11,*)
         iline=1
       end if
       do ipar=1,npar
710      continue
         iline=iline+1
         read(11,'(a)',end=9520,err=9500) cline
         if(cline.eq.' ') go to 710
         call linspl(ifail,4,lw,rw,cline)
         if(ifail.ne.0) go to 9680
         aapar=cline(lw(1):rw(1))
         call lowcas(aapar)
         if(rrfexist)then
           if(aapar.ne.apar(ipar)) go to 9700
         else
           apar(ipar)=aapar
         end if
         call drealrd(ifail,cline(lw(2):rw(2)),pval(ipar))
         if(ifail.ne.0)then
           call writint(aline,iline)
           write(errmsg,720) trim(aline),trim(qparobsfile)
720        format('Cannot read parameter value at line ',a,
     +     ' of file ',a,'.')
           go to 9890
         end if
       end do
       do iobs=1,nobs
730      continue
         iline=iline+1
         read(11,'(a)',end=9520,err=9500) cline
         if(cline.eq.' ') go to 730
         call linspl(ifail,2,lw,rw,cline)
         if(ifail.ne.0) go to 9680
         aaobs=cline(lw(1):rw(1))
         call lowcas(aaobs)
         if(rrfexist)then
           if(aaobs.ne.aobs(iobs)) go to 9720
         else
           aobs(iobs)=aaobs
         end if
         call drealrd(ifail,cline(lw(2):rw(2)),oval(iobs))
         if(ifail.ne.0)then
           call writint(aline,iline)
           write(errmsg,740) trim(aline),trim(qparobsfile)
740        format('Cannot read model output value at line ',a,
     +     ' of file ',a,'.')
           go to 9890
         end if
       end do
       close(unit=11)
       write(6,750) trim(qparobsfile)
750    format(' - file ',a,' read ok.')

C -- Now the results are appended to the run results file.

       iindex=iindex+1
       if(.not.rrfexist)then
         write(10,151)
151      format('* case dimensions')
         write(10,152) npar,nobs
152      format(2i10)
         write(10,1532)
1532     format('* parameter names')
         do ipar=1,npar
           write(10,1531) trim(apar(ipar))
1531       format(1x,a)
         end do
         write(10,154)
154      format('* observation names')
         do iobs=1,nobs
           write(10,1531) trim(aobs(iobs))
         end do
       end if
       write(10,760)
760    format('* parameter set index')
       call writint(aindex,iindex)
       write(10,770) trim(aindex)
770    format(1x,a)
       write(10,780)
780    format('* parameter values source')
       write(10,790) trim(qparobsfile)
790    format('file ',a)
       write(10,800)
800    format('* parameter values')
       do ipar=1,npar
         write(10,810) pval(ipar)
810      format(1x,1pg14.7)
       end do
       write(10,820)
820    format('* model output values')
       do iobs=1,nobs
         write(10,810) oval(iobs)
       end do
       close(unit=10)
       write(6,830) trim(qrrffile)
830    format(' - file ',a,' written ok.')

       go to 9900

8900   continue
       write(6,8910)
8910   format(/,' RRFAPPEND is run using the command:',/)
       write(6,8920)
8920   format('     rrfappend parobsfile rrffile',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8950)
8950   format('     parobsfile is an existing parobs file, and')
       write(6,8980)
8980   format('     rrffile    is an existing run results file.')
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

9500   continue
       call writint(aline,iline)
       write(errmsg,9510) trim(aline),trim(qparobsfile)
9510   format('Error reading line ',a,' of parobs file ',a,'.')
       go to 9890

9520   write(errmsg,9530) trim(qparobsfile)
9530   format('Unexpected end encountered to parobs file ',a,'.')
       go to 9890

9560   write(errmsg,9570) trim(qparobsfile)
9570   format('File ',a,' does not appear to be a parobs file.')
       go to 9890

9680   continue
       call writint(aline,iline)
       write(errmsg,9690) trim(aline),trim(qparobsfile)
9690   format('Insufficient entries on line ',a,' of file ',a,'.')
       go to 9890

9700   write(errmsg,9710) trim(qparobsfile)
9710   format('Parameters featured in file ',a,' are not the same, ',
     + 'or are not in the same order, as in the run results file.')
       go to 9890

9720   write(errmsg,9730) trim(qparobsfile)
9730   format('Observations featured in file ',a,' are not the same, ',
     + 'or are not in the same order, as in the run results file.')
       go to 9890

9890   continue
       errmsg=' '//trim(errmsg)
       call writmess(6,errmsg)
       if(.not.rrfexist)then
         inquire(unit=10,opened=lopened)
         if(lopened) close(unit=10,status='delete')
       end if

9900   continue
       close(unit=11,iostat=ierr)
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


