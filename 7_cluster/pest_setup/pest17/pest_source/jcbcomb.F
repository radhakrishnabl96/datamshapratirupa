       program jcbcomb

C -- Program JCBCOMB combines the contents of 2 JCB files.

       implicit none

       integer :: ibeg,iend,ifail,nb,i,ierr,itemp1,itemp2
       integer :: ncol1,nrow1,nrow2,ncol2,irow1,irow2,jrow1,jrow2,
     +            icol,irow,iirow,jjrow,kkrow,nrow3
       integer :: icount,iir,iii,jj,icolflag

       double precision       :: dtemp

       character (len=4)      :: aext
       character (len=10)     :: prefix1,prefix2
       character (len=12)     :: aversion
       character (len=15)     :: atemp1,atemp2,aacol,aarow
       character (len=200)    :: bcol
       character (len=256)    :: afile,jcbfile1,jcbfile2,jcbfile3
       character (len=1500)   :: amessage

       double precision, allocatable    :: x(:,:)
       character (len=200), allocatable :: acol1(:),arow1(:),arow2(:)

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' JCBCOMB version ',a,'. Watermark Numerical ',
     + 'Computing.')

! -- We acquire details of the first JCB file.

       write(6,*)
20     write(6,25,advance='no')
25     format(' Enter name of first JCB file: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 20
       ibeg=1
       iend=len(afile)
       call getfile(ifail,afile,jcbfile1,ibeg,iend)
       if(ifail.ne.0) go to 20
#ifndef UNIX
       call lowcas(jcbfile1)
#endif
       nb=len_trim(jcbfile1)
       i=nb-3
       if(i.lt.1)i=1
       aext=jcbfile1(i:nb)
       call lowcas(aext)
       if(aext.ne.'.jcb')then
         write(6,30)
30       format(' Extension must be ".jcb". Try again.')
         go to 20
       end if
       call open_unformatted_file(ifail,10,'read',jcbfile1,amessage)
       if(ifail.ne.0)then
         write(6,35)
35       format(' Cannot open file. Try again.')
         go to 20
       end if
       call addquote(jcbfile1,afile)
       read(10,err=9350,end=9350)itemp1,itemp2
       if(itemp1.le.0)then
         write(amessage,60) trim(afile)
60       format(' File ',a,' does not appear to use enhanced ',
     +   'Jacobian matrix file format. The first integer in the ',
     +   'file must be positive.')
         go to 9800
       end if
       ncol1=abs(itemp1)
       nrow1=abs(itemp2)
       if((ncol1.le.0).or.(nrow1.le.0))then
         write(amessage,65) trim(afile)
65       format(' NROW or NCOL are not positive in file ',a,
     +   '; is this a valid enhanced Jacobian matrix file?')
         go to 9800
       end if
       allocate(acol1(ncol1),arow1(nrow1),stat=ierr)
       if(ierr.ne.0) go to 9200
       call writint(atemp1,nrow1)
69     write(6,70,advance='no') trim(atemp1)
70     format('   Enter lowest row number to transfer [1 - ',a,']: ')
       read(5,*,err=69) irow1
       if((irow1.lt.1).or.(irow1.gt.nrow1)) go to 69
       call writint(atemp2,irow1)
79     write(6,80,advance='no') trim(atemp2),trim(atemp1)
80     format('   Enter highest row number to ',
     + 'transfer [',a,' - ',a,']: ')
       read(5,*,err=79) irow2
       if((irow2.lt.irow1).or.(irow2.gt.nrow1)) go to 79
90     write(6,91,advance='no')
91     format('   Enter prefix for new row names (5 chars or less): ')
       read(5,'(a)') prefix1
       if(prefix1.eq.' ') go to 90
       call lowcas(prefix1)
       prefix1=adjustl(prefix1)
       nb=len_trim(prefix1)
       if(nb.gt.5) go to 90

! -- We acquire details of the second JCB file.

       write(6,*)
120    write(6,125,advance='no')
125    format(' Enter name of second JCB file: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 120
       ibeg=1
       iend=len(afile)
       call getfile(ifail,afile,jcbfile2,ibeg,iend)
       if(ifail.ne.0) go to 120
#ifndef UNIX
       call lowcas(jcbfile2)
#endif
       if(jcbfile2.eq.jcbfile1) go to 120
       nb=len_trim(jcbfile2)
       i=nb-3
       if(i.lt.1)i=1
       aext=jcbfile2(i:nb)
       call lowcas(aext)
       if(aext.ne.'.jcb')then
         write(6,30)
         go to 120
       end if
       call open_unformatted_file(ifail,11,'read',jcbfile2,amessage)
       if(ifail.ne.0)then
         write(6,35)
         go to 120
       end if
       call addquote(jcbfile2,afile)
       read(11,err=9350,end=9350)itemp1,itemp2
       if(itemp1.le.0)then
         write(amessage,60) trim(jcbfile2)
         go to 9800
       end if
       ncol2=abs(itemp1)
       nrow2=abs(itemp2)
       if((ncol2.le.0).or.(nrow2.le.0))then
         write(amessage,65) trim(jcbfile2)
         go to 9800
       end if
       if(ncol2.ne.ncol1)then
         write(amessage,128)
128      format(' The second JCB file must have the same number of ',
     +   'columns as the first JCB file.')
         go to 9800
       end if
       allocate(arow2(nrow2),stat=ierr)
       if(ierr.ne.0) go to 9200
       call writint(atemp1,nrow2)
169    write(6,70,advance='no') trim(atemp1)
       read(5,*,err=169) jrow1
       if((jrow1.lt.1).or.(jrow1.gt.nrow2)) go to 169
       call writint(atemp2,jrow1)
179    write(6,80,advance='no') trim(atemp2),trim(atemp1)
       read(5,*,err=179) jrow2
       if((jrow2.lt.jrow1).or.(jrow2.gt.nrow2)) go to 179
190    write(6,91,advance='no')
       read(5,'(a)') prefix2
       if(prefix2.eq.' ') go to 190
       call lowcas(prefix2)
       prefix2=adjustl(prefix2)
       nb=len_trim(prefix2)
       if(nb.gt.5) go to 190

! -- Acquire details of the file that must be written.

       write(6,*)
200    write(6,210,advance='no')
210    format(' Enter name for new JCB file: ')
       read(5,'(a)',end=200) afile
       if(afile.eq.' ') go to 200
       ibeg=1
       iend=len(afile)
       call getfile(ifail,afile,jcbfile3,ibeg,iend)
       if(ifail.ne.0) go to 200
#ifndef UNIX
       call lowcas(jcbfile3)
#endif
       nb=len_trim(jcbfile3)
       i=nb-3
       if(i.lt.1)i=1
       aext=jcbfile3(i:nb)
       call lowcas(aext)
       if(aext.ne.'.jcb')then
         write(6,30)
         go to 200
       end if
       call open_unformatted_file(ifail,20,'write',jcbfile3,amessage)
       if(ifail.ne.0)then
         write(6,220)
220      format(' Cannot write to this file. Try again.')
         go to 200
       end if

! -- We write the header to the new file.

       iirow=(irow2-irow1+1)
       jjrow=(jrow2-jrow1+1)
       kkrow=max(iirow,jjrow)
       nrow3=iirow+jjrow
       write(20) ncol1,nrow3

! -- We read the first file.

       allocate(x(kkrow,ncol1),stat=ierr)
       if(ierr.ne.0) go to 9200
       call addquote(jcbfile1,afile)
       read(10,err=9350,end=9350)icount
       do i=1,icount
         read(10,err=9350,end=9350) irow,icol,dtemp
         irow=irow+1
         icol=icol+1
         if((icol.le.0).or.(irow.le.0).or.(icol.gt.ncol1)
     +   .or.(irow.gt.nrow1))then
           call writint(aacol,icol-1)
           call writint(aarow,irow-1)
           write(amessage,111) trim(aarow),trim(aacol),trim(afile)
111        format(' [row,col] [',a,',',a,
     +     '] out of range in file ',a,'.')
           go to 9800
         end if
         if((irow.ge.irow1).and.(irow.le.irow2))
     +   x(irow-irow1+1,icol)=dtemp
       end do
       do icol=1,ncol1
         read(10,err=9350,end=9350) acol1(icol)
         acol1(icol)=adjustl(acol1(icol))
       end do
       do irow=1,nrow1
         read(10,err=9350,end=9350) arow1(irow)
         arow1(irow)=adjustl(arow1(irow))
       end do
       close(unit=10)
       write(6,*)
       write(6,170) trim(afile)
170    format(' - binary matrix file ',a,' read ok.')

! -- Contents from this file are transferred to the third file.

       icount=ncol1*(iirow+jjrow)
       write(20) icount
       iir=-1
       iii=0
       do irow=irow1,irow2
         iir=iir+1
         iii=iii+1
         jj=-1
         do icol=1,ncol1
           jj=jj+1
           write(20) iir,jj,x(iii,icol)
         end do
       end do

! -- We read the second file.

       call addquote(jcbfile2,afile)
       read(11,err=9350,end=9350)icount
       do i=1,icount
         read(11,err=9350,end=9350) irow,icol,dtemp
         irow=irow+1
         icol=icol+1
         if((icol.le.0).or.(irow.le.0).or.(icol.gt.ncol2)
     +   .or.(irow.gt.nrow2))then
           call writint(aacol,icol-1)
           call writint(aarow,irow-1)
           write(amessage,111) trim(aarow),trim(aacol),trim(afile)
           go to 9800
         end if
         if((irow.ge.jrow1).and.(irow.le.jrow2))
     +   x(irow-jrow1+1,icol)=dtemp
       end do
       icolflag=0
       do icol=1,ncol1
         read(11,err=9350,end=9350) bcol
         bcol=adjustl(bcol)
         if(bcol.ne.acol1(icol))icolflag=1
       end do
       do irow=1,nrow2
         read(11,err=9350,end=9350) arow2(irow)
         arow2(irow)=adjustl(arow2(irow))
       end do
       close(unit=11)
       write(6,170) trim(afile)

! -- Contents from this file are transferred to the third file.

       iii=0
       do irow=jrow1,jrow2
         iir=iir+1
         iii=iii+1
         jj=-1
         do icol=1,ncol1
           jj=jj+1
           write(20) iir,jj,x(iii,icol)
         end do
       end do

! -- We now transfer names to the new file.

       do icol=1,ncol1
         write(20) acol1(icol)
       end do
       do irow=irow1,irow2
         arow1(irow)=trim(prefix1)//trim(arow1(irow))
       end do
       do irow=jrow1,jrow2
         arow2(irow)=trim(prefix2)//trim(arow2(irow))
       end do
       do irow=irow1,irow2
         write(20) arow1(irow)
       end do
       do irow=jrow1,jrow2
         write(20) arow2(irow)
       end do

       close(unit=20)
       write(6,270) trim(jcbfile3)
270    format(' - binary ',a,' written ok.')

       if(icolflag.ne.0)then
         write(6,*)
         write(6,280)
280      format(' Warning: Column names in two JCB files are ',
     +   'different.')
         write(6,290)
290      format(' Those in first JCB file are transfered to ',
     +   'third JCB file.')
       end if
       go to 9999

9200   write(amessage,9210)
9210   format(' Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9800
9350   write(amessage,9360) trim(afile)
9360   format(' Error reading data from ',
     + 'file ',a,'.')
       go to 9800

9800   continue
       call writmess(6,amessage)

9999   deallocate(x,acol1,arow1,arow2,stat=ierr)
       close(unit=10,iostat=ierr)
       close(unit=11,iostat=ierr)
       close(unit=20,iostat=ierr)

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

