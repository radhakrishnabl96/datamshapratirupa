       program jcosub
       
C --   Program jcosub subsitutes some elements of a JCO file for another.

       implicit none

       integer  ifail,ierr,nespar1,nxrow1,ies,irow,icount,i,j,ipar,
     +          nespar2,nxrow2,ies1,ies2,irow1,irow2
       integer lw(5),rw(5)
       double precision dtemp
       double precision, allocatable :: x1(:,:),x2(:,:)
       character*10  how
       character*12  aversion
       character*12 aapar
       character*20 aaobs
       character*200 jcofile1,jcofile2,jcofile3,afile
       character*500 cline,amessage
       character*12, allocatable :: apar1(:),apar2(:)
       character*20, allocatable :: aobs1(:),aobs2(:)

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif       
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' JCOSUB Version ',a,'. Watermark Numerical ',
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
       jcofile2=cline(lw(2):rw(2))
       jcofile3=cline(lw(3):rw(3))
       call remchar(jcofile1,char(211))
       call remchar(jcofile2,char(211))
       call remchar(jcofile3,char(211))
#ifndef UNIX
       call lowcas(jcofile1)
       call lowcas(jcofile2)
       call lowcas(jcofile3)
#endif

C -- Input files are opened.

       how='read'
       call open_unformatted_file(ifail,10,how,jcofile1,amessage)
       if(ifail.ne.0) go to 9890
       call open_unformatted_file(ifail,11,how,jcofile2,amessage)
       if(ifail.ne.0) go to 9890

C -- Read the first JCO file.

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
         if((ies.gt.nespar1).or.(irow.gt.nxrow1)) go to 9250
         if((ies.le.0).or.(irow.le.0)) go to 9250
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
       write(6,80) trim(afile)
80     format(' - file ',a,' read ok.')

C -- Read the second JCO file.

       call addquote(jcofile2,afile)
       read(11,err=9250,end=9250)nespar2,nxrow2
       if(nespar2.lt.0)then
         continue
       else
         write(amessage,20) trim(afile)
         go to 9890
       end if
       nespar2=abs(nespar2)
       nxrow2=abs(nxrow2)
       allocate(x2(nxrow2,nespar2),apar2(nespar2),aobs2(nxrow2),
     + stat=ierr)
       if(ierr.ne.0) go to 9100
       do ies=1,nespar2
         do irow=1,nxrow2
           x2(irow,ies)=0.0d0
         end do
       end do
       read(11,err=9250,end=9250)icount
       do i=1,icount
         read(11,err=9250,end=9250) j,dtemp
         ies=(j-1)/nxrow2+1
         irow=j-(ies-1)*nxrow2
         if((ies.gt.nespar2).or.(irow.gt.nxrow2)) go to 9250
         if((ies.le.0).or.(irow.le.0)) go to 9250
         x2(irow,ies)=dtemp
       end do
       do i=1,nespar2
         read(11,err=9250,end=9250) apar2(i)
         call lowcas(apar2(i))
       end do
       do i=1,nxrow2
         read(11,err=9250,end=9250) aobs2(i)
         call lowcas(aobs2(i))
       end do
       close(unit=11)
       write(6,80) trim(afile) 

C -- The new Jacobian matrix file is formed.

       irow1=1
       do ies2=1,nespar2
         aapar=apar2(ies2)
         do ies1=1,nespar1
           if(apar1(ies1).eq.aapar)then
             do irow2=1,nxrow2
               aaobs=aobs2(irow2)
               call whichone(ifail,nxrow1,irow1,aobs1,aaobs)
               if(ifail.eq.0) x1(irow1,ies1)=x2(irow2,ies2)
             end do
           end if
         end do
       end do

C -- The new Jacobian matrix file is now written.

       how='write'
       call open_unformatted_file(ifail,15,how,jcofile3,amessage)
       if(ifail.ne.0) go to 9890

       call addquote(jcofile3,afile)
       write(15,err=9400) -nespar1,-nxrow1
       icount=0
       do i=1,nespar1
          do j=1,nxrow1
            if(x1(j,i).ne.0.0d0) icount=icount+1
          end do
       end do
       write(15,err=9400) icount
       do i=1,nespar1
         do j=1,nxrow1
           if(x1(j,i).ne.0.0d0)then
             icount=(i-1)*nxrow1+j
             write(15,err=9400)icount,x1(j,i)
           end if
         end do
       end do
       do ies=1,nespar1
         write(15,err=9400) apar1(ies)
       end do
       do irow=1,nxrow1
         write(15,err=9400) aobs1(irow)
       end do
       close(unit=15)
       write(6,490) trim(afile)
490    format(' - file ',a,' written ok.')

       go to 9900

9000   continue
       write(6,9010)
9010   format(/,' JCOSUB is run using the command:',/)
       write(6,9020)
9020   format('     jcosub jcofile1 jcofile2 jcofile3',/)
       write(6,9030)
9030   format(' where',/)
       write(6,9050)
9050   format('     jcofile1 is an existing Jacobian matrix file,')
       write(6,9051)
9051   format('     jcofile2 is a Jacobian matrix file with substitute',
     + ' obs sensitivites, and')
       write(6,9052)
9052   format('     jcofile3 is a new Jacobian matrix file.')
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

9890   call writmess(6,amessage)
9900   continue

       deallocate (x1,x2,apar1,aobs1,apar2,aobs2,stat=ierr)
     
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

