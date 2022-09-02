       program jcodiff
       
C -- Program JCODIFF subtracts one Jacobian matrix from another, storing the result
C    in a Jacobian matrix file.

       implicit none

       integer          :: ierr,ifail,itemp1,itemp2,nespar,nxrow,
     +                     icount,irow,ies,nb,i,j,nespar2,nxrow2,ii
       integer          :: lw(5),rw(5)
       double precision :: dtemp
       character*4      :: aext
       character*12     :: aversion,aapar
       character*20     :: aaobs
       character*200    :: jcofile1,jcofile2,jcofile3,afile,bfile,
     +                     jcofile
       character*500    :: cline,amessage

       double precision, allocatable  :: x(:,:)
       character*12, allocatable      :: apar(:)
       character*20, allocatable      :: aobs(:)

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif       
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' JCODIFF version ',a,'. Watermark Numerical ',
     + 'Computing.')
#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) CLINE
#else
       call pgetcl(cline)
#endif

C -- The command line is parsed.

       if(cline.eq.' ') go to 8900
       call spacesub(cline)
       call linspl(ifail,4,lw,lw,cline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.ne.0) go to 8900
       jcofile1=cline(lw(1):rw(1))
       call remchar(jcofile1,char(211))
       jcofile2=cline(lw(2):rw(2))
       call remchar(jcofile2,char(211))
       jcofile3=cline(lw(3):rw(3))
       call remchar(jcofile3,char(211))
#ifndef UNIX
       call lowcas(jcofile1)
       call lowcas(jcofile2)
       call lowcas(jcofile3)
#endif

       do ii=1,3
         if(ii.eq.1)then
           jcofile=jcofile1
         else if(ii.eq.2)then
           jcofile=jcofile2
         else
           jcofile=jcofile3
         end if
         nb=len_trim(jcofile)
         i=nb-3
         if(i.lt.1)i=1
         aext=jcofile(i:nb)
         call lowcas(aext)
         if(aext.ne.'.jco')then
           call addquote(jcofile,afile)
           write(amessage,15) trim(afile)
15         format(' Expected extension for Jacobian matrix file ',a,
     +     ' is ".jco".')
           go to 9800
         end if
       end do

C -- The first Jacobian matrix is read.

       write(6,*)
       call addquote(jcofile1,afile)
       write(6,30) trim(afile)
30     format(' - reading first Jacobian matrix file ',a,'...')

       call open_unformatted_file(ifail,10,'read',jcofile1,amessage)
       if(ifail.ne.0) go to 9800

       read(10,err=9350,end=9350)itemp1,itemp2
       if(itemp1.ge.0)then
         write(amessage,60) trim(afile)
60       format(' Jacobian matrix file ',a,' uses old format; ',
     +   'use JCOTRANS utility to translate it to new format.')
         go to 9800
       end if
       nespar=abs(itemp1)
       nxrow=abs(itemp2)
       if((nespar.le.0).or.(nxrow.le.0))then
         write(amessage,65) trim(afile)
65       format(' NROW or NCOL not positive in file ',a,
     +   '; is this a valid Jacobian matrix file?')
         go to 9800
       end if
       allocate(x(nxrow,nespar),apar(nespar),aobs(nxrow),stat=ierr)
       if(ierr.ne.0) go to 9200
       do i=1,nespar
         do j=1,nxrow
           x(j,i)=0.0d0
         end do
       end do
       read(10,err=9350,end=9350)icount
       do i=1,icount
         read(10,err=9350,end=9350) j,dtemp
         ies=(j-1)/nxrow+1
         irow=j-(ies-1)*nxrow
         x(irow,ies)=dtemp
       end do
       do ies=1,nespar
         read(10,err=9350,end=9350) apar(ies)
       end do
       do irow=1,nxrow
         read(10,err=9350,end=9350) aobs(irow)
       end do
       close(unit=10)
       write(6,70) trim(afile)
70     format(' - Jacobian matrix file ',a,' read ok.')

C -- The second Jacobian matrix file is read.

       write(6,*)
       call addquote(jcofile2,afile)
       write(6,32) trim(afile)
32     format(' - reading second Jacobian matrix file ',a,'...')

       call open_unformatted_file(ifail,10,'read',jcofile2,amessage)
       if(ifail.ne.0) go to 9800
       read(10,err=9350,end=9350)itemp1,itemp2
       if(itemp1.ge.0)then
         write(amessage,60) trim(afile)
         go to 9800
       end if
       nespar2=abs(itemp1)
       nxrow2=abs(itemp2)
       if((nespar2.le.0).or.(nxrow2.le.0))then
         write(amessage,65) trim(afile)
         go to 9800
       end if
       if((nespar2.ne.nespar).or.(nxrow2.ne.nxrow)) go to 9400
       read(10,err=9350,end=9350)icount
       do i=1,icount
         read(10,err=9350,end=9350) j,dtemp
         ies=(j-1)/nxrow+1
         irow=j-(ies-1)*nxrow
         x(irow,ies)=x(irow,ies)-dtemp
       end do
       do ies=1,nespar
         read(10,err=9350,end=9350) aapar
         if(aapar.ne.apar(ies)) go to 9400
       end do
       do irow=1,nxrow
         read(10,err=9350,end=9350) aaobs
         if(aaobs.ne.aobs(irow)) go to 9400
       end do
       close(unit=10)
       write(6,70) trim(afile)

C -- The matrix file is now written.

       write(6,*)
       call addquote(jcofile3,afile)
       write(6,33) trim(afile)
33     format(' - writing Jacobian matrix file ',a,'...')
       call open_unformatted_file(ifail,20,'write',jcofile3,amessage)
       if(ifail.ne.0) go to 9800
       write(20,err=9100) -nespar,-nxrow
       icount=0
       do ies=1,nespar
         do irow=1,nxrow
           if(x(irow,ies).ne.0.0d0) icount=icount+1
         end do
       end do
       write(20,err=9100) icount
       do ies=1,nespar
         do irow=1,nxrow
           if(x(irow,ies).ne.0.0d0)then
             icount=(ies-1)*nxrow+irow
             write(20) icount,x(irow,ies)
           end if
         end do
       end do
       do ies=1,nespar
         write(20,err=9100) apar(ies)
       end do
       do irow=1,nxrow
         write(20,err=9100) aobs(irow)
       end do
       close(unit=20)
       write(6,320) trim(afile)
320    format(' - file ',a,' written ok.')

       go to 9999

8900   continue
       write(6,8910)
8910   format(/,' JCODIFF is run using the command:',/)
       write(6,8920)
8920   format('     jco2diff jcofile1 jcofile2 jcofile3',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8940)
8940   format('     jcofile1 is the name of the first Jacobian ',
     + 'matrix file,')
       write(6,8945)
8945   format('     jcofile2 is the name of the second Jacobian ',
     + 'matrix file, and')
       write(6,8960)
8960   format('     jcofile3 = jcofile1 - jcofile2.')

       go to 9999

9100   write(amessage,9110) trim(afile)
9110   format(' Cannot write to matrix file ',a,'.')
       go to 9800
9200   write(amessage,9210)
9210   format(' Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9800
9350   write(amessage,9360) trim(afile)
9360   format(' Error reading Jacobian matrix from file ',a,'.')
       go to 9800
9400   call addquote(jcofile1,afile)
       call addquote(jcofile2,bfile)
       write(amessage,9410) trim(bfile),trim(afile)
9410   format(' The Jacobian matrix contained in file ',a,' does ',
     + 'not cite the same adjustable parameters/observations in the ',
     + 'same order as that contained in file ',a,'.')
       go to 9800

9800   continue
       call writmess(6,amessage)

9999   deallocate(x,apar,aobs,stat=ierr)

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

