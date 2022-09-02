       program jcoaddz

C --   Program jcoaddz adds zero valued rows and/or columns to a Jacobian matrix.

       implicit none

       integer, parameter :: MAXADDPAR=2000
       integer, parameter :: MAXADDOBS=1000
       integer  ifail,ierr,nespar1,nxrow1,ies,irow,icount,i,j,ipar,
     + iobs,iline,npar,nobs,nespar2,nxrow2
       integer  lw(5),rw(5)
       double precision dtemp
       double precision, allocatable :: x1(:,:),x2(:,:)
       character*10  how,aline
       character*200 jcofile1,jcofile2,addfile,afile,bfile
       character*500 cline,amessage
       character*12              :: aversion
       character*12              :: bapar(MAXADDPAR)
       character*20              :: baobs(MAXADDOBS)
       character*12, allocatable :: apar1(:)
       character*20, allocatable :: aobs1(:)

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' JCOADDZ Version ',a,'. Watermark Numerical ',
     + 'Computing.')
#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) CLINE
#else
       call pgetcl(cline)
#endif

! -- The command line is parsed.

       if(cline.eq.' ') go to 9000
       call spacesub(cline)
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.eq.0) go to 9000
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.ne.0) go to 9000
       jcofile1=cline(lw(1):rw(1))
       addfile=cline(lw(2):rw(2))
       jcofile2=cline(lw(3):rw(3))
       call remchar(jcofile1,char(211))
       call remchar(addfile,char(211))
       call remchar(jcofile2,char(211))
#ifndef UNIX
       call lowcas(jcofile1)
       call lowcas(jcofile2)
       call lowcas(addfile)
#endif


C -- Input files are opened.

       how='read'
       call open_unformatted_file(ifail,10,how,jcofile1,amessage)
       if(ifail.ne.0) go to 9890
       open(unit=11,file=addfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(addfile,afile)
         write(amessage,11) trim(afile)
11       format(' Cannot open parameter/observation addition file ',
     +   a,'.')
         go to 9890
       end if

C -- Read the JCO file.

       call addquote(jcofile1,afile)
       read(10,err=9250,end=9250)nespar1,nxrow1
       if(nespar1.lt.0)then
         continue
       else
         write(amessage,20) trim(afile)
20       format(' Jacobian matrix file ',a,' was not produced by ',
     +   'version 8 or later of PEST; use program JCOTRANS to ',
     +   'convert this to new format.')
         go to 9890
       end if
       nespar1=abs(nespar1)
       nxrow1=abs(nxrow1)
       allocate(x1(nxrow1,nespar1),apar1(nespar1),aobs1(nxrow1),
     + stat=ierr)
       if(ierr.ne.0) go to 9100
       do ies=1,nespar1
         do irow=1,nxrow1
           x1(irow,ies)=0.0d0
         end do
       end do
       read(10,err=9250,end=9250)icount
       do i=1,icount
         read(10,err=9250,end=9250) j,dtemp
         ies=(j-1)/nxrow1+1
         irow=j-(ies-1)*nxrow1
         x1(irow,ies)=dtemp
       end do
       do i=1,nespar1
         read(10,err=9250,end=9250) apar1(i)
         call lowcas(apar1(i))
       end do
       do i=1,nxrow1
         read(10,err=9250,end=9250) aobs1(i)
         call lowcas(aobs1(i))
       end do
       close(unit=10)
       write(6,*)
       write(6,80) trim(afile)
80     format(' - file ',a,' read ok.')

C -- Read the addition file.

       call addquote(addfile,afile)

       npar=0
       nobs=0
       iline=0
       ipar=0
110    iline=iline+1
       read(11,'(a)',err=9500,end=9550) cline
       if(cline.eq.' ') go to 110
       cline=adjustl(cline)
       if(cline(1:1).eq.'#') go to 110
       call lowcas(cline)
       if(cline(1:1).ne.'*') go to 9600
       if(index(cline,'parameter').eq.0) go to 9600
       do
         iline=iline+1
         read(11,'(a)',err=9500,end=9650) cline
         if(cline.eq.' ') cycle
         cline=adjustl(cline)
         if(cline(1:1).eq.'#') cycle
         if(cline(1:1).eq.'*') go to 150
         ipar=ipar+1
         if(ipar.gt.MAXADDPAR)then
           write(amessage,115) trim(afile)
115        format(' Too many parameters cited in file ',a,': increase ',
     +     'MAXADDPAR and re-compile program.')
           go to 9890
         end if
         call linspl(ifail,1,lw,rw,cline)
         bapar(ipar)=cline(lw(1):rw(1))
         call lowcas(bapar(ipar))
         do ies=1,nespar1
           if(bapar(ipar).eq.apar1(ies))then
             call addquote(jcofile1,bfile)
             write(amessage,117) trim(bapar(ipar)),trim(afile),
     +       trim(bfile)
117          format(' Parameter "',a,'" cited in ',
     +       'parameter/observation ',
     +       'addition file ',a,' is already cited in Jacobian ',
     +       'matrix file ',a,'.')
             go to 9890
           end if
         end do
       end do
150    continue
       npar=ipar
       iobs=0
       do
         iline=iline+1
         read(11,'(a)',err=9500,end=180) cline
         if(cline.eq.' ') cycle
         cline=adjustl(cline)
         if(cline(1:1).eq.'#') cycle
         if(cline(1:1).eq.'*') go to 9500
         call lowcas(cline)
         iobs=iobs+1
         if(iobs.gt.MAXADDOBS)then
           write(amessage,155) trim(afile)
155        format(' Too many observations cited in file ',a,
     +     ': increase MAXADDOBS and re-compile program.')
           go to 9890
         end if
         call linspl(ifail,1,lw,rw,cline)
         baobs(iobs)=cline(lw(1):rw(1))
         do irow=1,nxrow1
           if(baobs(iobs).eq.aobs1(irow))then
             call addquote(jcofile1,bfile)
             write(amessage,156) trim(baobs(iobs)),trim(afile),
     +       trim(bfile)
156          format(' Observation "',a,'" cited in ',
     +       'parameter/observation ',
     +       'addition file ',a,' is already cited in Jacobian ',
     +       'matrix file ',a,'.')
             go to 9890
           end if
         end do
       end do
180    continue
       nobs=iobs
       if((nobs.eq.0).and.(npar.eq.0))then
         write(amessage,185) trim(afile)
185      format(' No new observations or parameters were cited in ',
     +   'parameter/observation addition file ',a,'.')
         go to 9890
       end if
       close(unit=11)
       write(6,80) trim(afile)

C -- A new Jacobian matrix file is formed

       nxrow2=nxrow1+nobs
       nespar2=nespar1+npar

       allocate(x2(nxrow2,nespar2),stat=ierr)
       if(ierr.ne.0) go to 9100
       x2=0.0d0                    ! an array
       do ies=1,nespar1
         do irow=1,nxrow1
           x2(irow,ies)=x1(irow,ies)
         end do
       end do

C -- The new Jacobian matrix file is now written.

       how='write'
       call open_unformatted_file(ifail,15,how,jcofile2,amessage)
       if(ifail.ne.0) go to 9890

       call addquote(jcofile2,afile)
       write(15,err=9400) -nespar2,-nxrow2
       icount=0
       do i=1,nespar2
          do j=1,nxrow2
            if(x2(j,i).ne.0.0d0) icount=icount+1
          end do
       end do
       write(15,err=9400) icount
       do i=1,nespar2
         do j=1,nxrow2
           if(x2(j,i).ne.0.0d0)then
             icount=(i-1)*nxrow2+j
             write(15,err=9400)icount,x2(j,i)
           end if
         end do
       end do
       do ies=1,nespar1
         write(15,err=9400) apar1(ies)
       end do
       if(npar.ne.0)then
         do ipar=1,npar
           write(15,err=9400) bapar(ipar)
         end do
       end if
       do irow=1,nxrow1
         write(15,err=9400) aobs1(irow)
       end do
       if(nobs.ne.0)then
         do iobs=1,nobs
           write(15,err=9400) baobs(iobs)
         end do
       end if
       close(unit=15)
       write(6,490) trim(afile)
490    format(' - file ',a,' written ok.')

       go to 9900

9000   continue
       write(6,9010)
9010   format(/,' JCOADDZ is run using the command:',/)
       write(6,9020)
9020   format('     jcoaddz jcofile1 addfile jcofile2',/)
       write(6,9030)
9030   format(' where',/)
       write(6,9050)
9050   format('     jcofile1  is an existing Jacobian matrix file,')
       write(6,9051)
9051   format('     addfile   is a parameter/observation addition file',
     + ', and')
       write(6,9052)
9052   format('     jcofile2  is a new Jacobian matrix file.')
       go to 9999

9100   continue
       write(amessage,9110)
9110   format(' Insufficient memory to continue execution.')
       go to 9890
9250   continue
       write(amessage,9260) trim(afile)
9260   format(' Error reading Jacobian matrix file ',a,'.')
       go to 9890
9400   call addquote(jcofile2,afile)
       write(amessage,9410) trim(afile)
9410   format('Cannot write to file ',a,'.')
       go to 9890

9500   call writint(aline,iline)
       write(amessage,9510) trim(aline),trim(afile)
9510   format(' Error reading line ',a,' of parameter/observation ',
     + 'addition file ',a,'.')
       go to 9890
9550   write(amessage,9560) trim(afile)
9560   format(' Unexpected end encountered to parameter/',
     + 'observation addition file ',a,'.')
       go to 9890
9600   write(amessage,9610) trim(afile)
9610   format(' First data line of file ',a,' should be ',
     + '"* parameters".')
       go to 9890
9650   write(amessage,9660) trim(afile)
9660   format(' Could not find "* observations" section in ',
     + 'parameter/observation addition file ',a,'.')
       go to 9890

9890   call writmess(6,amessage)
9900   continue

       deallocate (x1,x2,apar1,aobs1,stat=ierr)

9999   continue
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


        subroutine whichone(ifail,npar,ipar,apar,tpar)

C -- Subroutine whichone locates a string in an array. Note that both the
C    search string and the existing array of strings are assumed to be
C    in the same case.

        integer npar,ipar,i
        integer ifail
        character*(*) tpar
        character*(*) apar(npar)

        ifail=0
        if((ipar.lt.1).or.(ipar.gt.npar)) ipar=1
        if(tpar.eq.apar(ipar)) return
        if(ipar.ne.npar)then
          do 20 i=ipar+1,npar
          if(tpar.eq.apar(i))then
            ipar=i
            return
          end if
20        continue
        end if
        if(ipar.ne.1)then
          do 40 i=ipar-1,1,-1
          if(tpar.eq.apar(i)) then
            ipar=i
            return
          end if
40        continue
        end if
        ifail=1
        return
        end



        subroutine writint(atemp,ival)

c       Subroutine WRITINT writes an integer to a character variable.

        integer*4 ival
        character*6 afmt
        character*(*) atemp

        afmt='(i   )'
        write(afmt(3:5),'(i3)') len(atemp)
        write(atemp,afmt)ival
        atemp=adjustl(atemp)
        return
#ifdef PESTMOD
        end subroutine writint
#else
        end
#endif

