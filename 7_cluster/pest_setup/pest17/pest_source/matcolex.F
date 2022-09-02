       include 'matman.F'


       program matcolex

C -- Program MATCOLEX extracts the first N columns from a matrix.

       use matrix_manipulate
       implicit none

       logical       :: lopened
       integer       :: ifail,irow,icol,nrow,ncol,ierr,nexcol,mexcol,
     +                  ii
       integer       :: lw(4),rw(4)
       character*10  :: atemp
       character*12  :: aversion
       character*20  :: acol
       character*200 :: matfile, matoutfile
       character*200 :: afile
       character*500 :: cline
       character*600 :: amessage
       type(dmatrix) :: mat,matex

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' MATCOLEX version ',a,'. Watermark Numerical ',
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
       matfile=cline(lw(1):rw(1))
       call remchar(matfile,char(211))
       acol=cline(lw(2):rw(2))
       call remchar(acol,char(211))
       call intread(ifail,acol,nexcol)
       if(ifail.ne.0) go to 8900
       if(nexcol.eq.0) go to 8900
       matoutfile=cline(lw(3):rw(3))
       call remchar(matoutfile,char(211))
#ifndef UNIX
       call lowcas(matfile)
       call lowcas(matoutfile)
#endif

C -- The matrices is read.

       call mat_read(ifail,10,mat,matfile,amessage,cline)
       if(ifail.ne.0) go to 9800
       call addquote(matfile,afile)
       write(6,100) trim(afile)
100    format(' - file ',a,' read ok.')

C -- Checks are made.


       if(abs(nexcol).gt.mat%ncol)then
         call writint(atemp,mat%ncol)
         write(amessage,110) trim(afile),trim(atemp)
110      format(' The matrix contained in file ',a,' has ',a,
     +   ' columns; the number of extracted columns must not ',
     +   'be greater than this.')
         go to 9800
       end if

C -- The new matrix is formed.

       mexcol=abs(nexcol)
       ncol=mat%ncol
       nrow=mat%nrow
       matex%nrow=nrow
       matex%ncol=mexcol
       matex%icode=2
       allocate(matex%array(nrow,mexcol),stat=ierr)
       if(ierr.ne.0) go to 9100
       allocate(matex%arow(nrow),matex%acol(mexcol),stat=ierr)
       if(ierr.ne.0) go to 9100

       if(mat%icode.eq.-1)then
         matex%array=0.0d0
         if(nexcol.gt.0)then
           do icol=1,nexcol
             matex%array(icol,icol)=mat%vector(icol)
           end do
         else
           ii=0
           do icol=ncol,ncol-mexcol+1,-1
             ii=ii+1
             matex%array(ii,ii)=mat%vector(icol)
           end do
         end if
       else
         if(nexcol.gt.0)then
           do icol=1,nexcol
             do irow=1,nrow
               matex%array(irow,icol)=mat%array(irow,icol)
             end do
           end do
         else
           ii=0
           do icol=ncol,ncol-mexcol+1,-1
           ii=ii+1
             do irow=1,nrow
               matex%array(irow,ii)=mat%array(irow,icol)
             end do
           end do
         end if
       end if
       do irow=1,nrow
         matex%arow(irow)=mat%arow(irow)
       end do
       if(mat%icode.ne.2)then
         if(nexcol.gt.0)then
           do icol=1,nexcol
             matex%acol(icol)=mat%arow(icol)
           end do
         else
           ii=0
           do icol=ncol,ncol-mexcol+1,-1
             ii=ii+1
             matex%acol(ii)=mat%arow(icol)
           end do
         end if
       else
         if(nexcol.gt.0)then
           do icol=1,nexcol
             matex%acol(icol)=mat%acol(icol)
           end do
         else
           ii=0
           do icol=ncol,ncol-mexcol+1,-1
             ii=ii+1
             matex%acol(ii)=mat%acol(icol)
           end do
         end if
       end if

C -- The new matrix file is written.

       call mat_write(ifail,20,matex,matoutfile,amessage,cline)
       if(ifail.ne.0) go to 9800
       call addquote(matoutfile,afile)
       write(6,200) trim(afile)
200    format(' - file ',a,' written ok.')

       go to 9900

8900   continue
       write(6,8910)
8910   format(/,' MATCOLEX is run using the command:',/)
       write(6,8920)
8920   format('     matcolex matfile ncol matoutfile',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8940)
8940   format('     matfile    is an existing matrix file,')
       write(6,8950)
8950   format('     ncol       is the no of cols to be extracted, '
     + 'starting from the 1st')
       write(6,8951)
8951   format('                (provide a negative ncol to start ',
     + 'from the last), and')
       write(6,8960)
8960   format('     matoutfile is the file to which the final matrix ',
     + 'is to be written.')

       go to 9999


9100   write(amessage,9110)
9110   format(' Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9800

9800   continue
       call writmess(6,amessage)

9900   continue
       call mat_deallocate(ifail,mat)
       call mat_deallocate(ifail,matex)
       inquire(unit=10,opened=lopened)
       if(lopened) close(unit=10,iostat=ierr)
       inquire(unit=20,opened=lopened)
       if(lopened) close(unit=20,iostat=ierr)


9999   continue

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

c       Subroutine WRITINT writes an integer to a character variable.

        integer*4 ival
        character*6 afmt
        character*(*) atemp

        afmt='(i   )'
        write(afmt(3:5),'(i3)') len(atemp)
        write(atemp,afmt)ival
        atemp=adjustl(atemp)
        return
        end

