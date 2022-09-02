       program ejrow2vec

C --   Program EJROW2VEC extracts a row from a PESTPP enhanced Jacobian matrix file and re-writes it
C      as a column vector.

       implicit none

       integer          :: ierr,ifail,nb,i,itemp1,itemp2,nespar,nxrow,
     +                     icount,j,irow,ies,iirow
       integer          :: lw(5),rw(5)
       double precision :: dtemp
       character*4      :: aext
       character*12     :: aversion
       character*200    :: aaobs,acol,arow
       character*200    :: jcofile,matfile,afile
       character*600    :: cline,amessage

       double precision, allocatable   :: x(:,:)
       character*200, allocatable      :: apar(:)
       character*200, allocatable      :: aobs(:)

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' EJROW2VEC version ',a,'. Watermark Numerical ',
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
       jcofile=cline(lw(1):rw(1))
       call remchar(jcofile,char(211))
       aaobs=cline(lw(2):rw(2))
       call remchar(aaobs,char(211))
       call lowcas(aaobs)
       aaobs=adjustl(aaobs)
       matfile=cline(lw(3):rw(3))
       call remchar(matfile,char(211))
#ifndef UNIX
       call lowcas(jcofile)
       call lowcas(matfile)
#endif

       write(6,*)
       nb=len_trim(jcofile)
       i=nb-3
       if(i.lt.1)i=1
       aext=jcofile(i:nb)
       call lowcas(aext)
       if(aext.ne.'.jcb')then
         call addquote(jcofile,afile)
         write(amessage,15) trim(afile)
15       format(' Expected extension for enhanced Jacobian matrix ',
     +   'file ',a,' is ".jcb".')
         go to 9800
       end if

C -- The Jacobian matrix is read.

       call addquote(jcofile,afile)
       write(6,30) trim(afile)
30     format(' - reading enhanced Jacobian matrix file ',a,'...')

       call open_unformatted_file(ifail,10,'read',jcofile,amessage)
       if(ifail.ne.0) go to 9800

       read(10,err=9350,end=9350)itemp1,itemp2
       if(itemp1.le.0)then
         write(amessage,60) trim(afile)
60       format(' File ',a,' does not appear to use enhanced ',
     +   'Jacobian matrix file format. The first integer in the ',
     +   'file must be positive.')
         go to 9800
       end if
       nespar=abs(itemp1)
       nxrow=abs(itemp2)
       if((nespar.le.0).or.(nxrow.le.0))then
         write(amessage,65) trim(afile)
65       format(' NROW or NCOL not positive in file ',a,
     +   '; is this a valid enhanced Jacobian matrix file?')
         go to 9800
       end if
       allocate(x(nxrow,nespar),apar(nespar),aobs(nxrow),stat=ierr)
       if(ierr.ne.0) go to 9200
       x=0.0d0   ! an array
       read(10,err=9350,end=9350)icount
       do i=1,icount
         read(10,err=9350,end=9350) irow,ies,dtemp
         irow=irow+1
         ies=ies+1
         if((ies.le.0).or.(irow.le.0).or.(ies.gt.nespar)
     +   .or.(irow.gt.nxrow))then
           call writint(acol,ies-1)
           call writint(arow,irow-1)
           write(amessage,111) trim(arow),trim(acol),trim(afile)
111        format(' [row,col] [',a,',',a,
     +     '] out of range in file ',a,'.')
           go to 9800
         end if
         x(irow,ies)=dtemp
       end do
       do ies=1,nespar
         read(10,err=9350,end=9350) apar(ies)
         apar(ies)=adjustl(apar(ies))
       end do
       do irow=1,nxrow
         read(10,err=9350,end=9350) aobs(irow)
         aobs(irow)=adjustl(aobs(irow))
       end do
       close(unit=10)
       write(6,70) trim(afile)
70     format(' - enhanced Jacobian matrix file ',a,' read ok.')

C -- The row is identified.

       do irow=1,nxrow
         if(aobs(irow).eq.aaobs) go to 90
       end do
       write(amessage,95) trim(aaobs),trim(afile)
95     format(' No row named "',
     + a,'" is cited in enhanced Jacobian matrix file ',a,'.')
       go to 9800
90     iirow=irow

C -- The matrix file is written.

       write(6,*)
       call addquote(matfile,afile)
       write(6,100) trim(afile)
100    format(' - writing file ',a,'...')
       open(unit=20,file=matfile,action='write',err=9100)
       write(20,110,err=9100) nespar,1,2
110    format(3i7)
       do ies=1,nespar
         write(20,120,err=9100) x(iirow,ies)
120      format(1x,1pg14.7)
       end do
       write(20,130,err=9100)
130    format('* row names')
       do ies=1,nespar
         write(20,140,err=9100) trim(apar(ies))
       end do
       write(20,150,err=9100)
150    format('* column names')
       write(20,140,err=9100) trim(aaobs)
140    format(a)
       close(unit=20)
       write(6,160) trim(afile)
160    format(' - file ',a,' written ok.')
       go to 9999

8900   continue
       write(6,8910)
8910   format(/,' EJROW2VEC is run using the command:',/)
       write(6,8920)
8920   format('     ejrow2vec ejcofile rowname matfile',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8940)
8940   format('     ejcofile is the name of an enhanced ',
     + 'Jacobian matrix file, ')
       write(6,8950)
8950   format('     rowname is a row name, and')
       write(6,8960)
8960   format('     matfile is a 1-column matrix file to which the ',
     + 'extracted row is written.')

       go to 9999

9100   write(amessage,9110) trim(afile)
9110   format(' Cannot write to matrix file ',a,'.')
       go to 9800
9200   write(amessage,9210)
9210   format(' Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9800
9350   write(amessage,9360) trim(afile)
9360   format(' Error reading matrix from ',
     + 'file ',a,'.')
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
