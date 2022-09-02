       include 'matman.F'


       program matmatch

C -- Program MATMATCH re-arranges rows or columns in a matrix to match a matrix
C    in another matrix file.

       use matrix_manipulate
       implicit none

       logical       :: lopened
       integer       :: ifail,irow,icol,nrow,ncol,ierr,nexcol,mexcol,
     +                  ii
       integer       :: lw(4),rw(4)
       character*10  :: atemp
       character*12  :: aversion
       character*20  :: acolrow
       character*200 :: matinfile, matoutfile, matmatchfile
       character*200 :: afile
       character*500 :: cline
       character*600 :: amessage
       type(dmatrix) :: mat1,mat2

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' MATMATCH version ',a,'. Watermark Numerical ',
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
       call linspl(ifail,5,lw,lw,cline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.ne.0) go to 8900
       matinfile=cline(lw(1):rw(1))
       call remchar(matinfile,char(211))
       matmatchfile=cline(lw(2):rw(2))
       call remchar(matmatchfile,char(211))
       acolrow=cline(lw(3):rw(3))
       call remchar(acolrow,char(211))
       call lowcas(acolrow)
       if((acolrow.ne.'column').and.(acolrow.ne.'row')) go to 9800
       matoutfile=cline(lw(4):rw(4))
       call remchar(matoutfile,char(211))
#ifndef UNIX
       call lowcas(matinfile)
       call lowcas(matmatchfile)
       call lowcas(matoutfile)
#endif
       call addquote(matinfile,amatinfile)
       call addquote(matmatchfile,amatmatchfile)
       call addquote(matoutfile,amatoutfile)

C -- The matrices are read.

       call mat_read(ifail,10,mat1,matinfile,amessage,cline)
       if(ifail.ne.0) go to 9800
       call addquote(matfile,afile)
       write(6,100) trim(afile)
100    format(' - file ',a,' read ok.')

       call mat_read(ifail,10,mat2,matmatchfile,amessage,cline)
       if(ifail.ne.0) go to 9800
       call addquote(matfile,afile)
       write(6,100) trim(afile)
100    format(' - file ',a,' read ok.')

C -- The names of the row or column of the matching matrix is extracted. The matrix
C    itself can then be deallocated.

       if((abs(mat2%icode).eq.1).or.(acolrow.eq.'row'))then
         nmatch=mat2%nrow
         do irow=1,nmatch
           amatch(irow)=mat2%arow(irow)
         end do
       else
         nmatch=mat2%ncol
         do icol=1,nmatch
           amatch(icol)=mat2%acol(icol)
         end do
       end do

       call mat_deallocate(ifail,mat2)

C -- If necessary, the first matrix is made into a full matrix. If it is thus
C    expanded, and if it remains square, it will be re-compressed later.

       icode1=mat1%icode               ! this may be needed later
       call mat_diag_expand(ifail,mat1,amessage)
       if(ifail.ne.0) go to 9800
       call mat_icode_expand(ifail,mat,amessage)
       if(ifail.ne.0) go to 9800

C -- Now a new matrix module is opened.

       if(acolrow.eq.'row')then
         mat3%nrow=mat1%nmatch
         mat3%ncol=mat1%ncol
       else
         mat3%nrow=mat1%nrow
         mat3%ncol=nmatch
       end if
       nrow=mat3%nrow
       ncol=mat3%ncol
       allocate(mat3%arow(nrow),mat3%acol(ncol),mat3%array(nrow,ncol),
     + stat=ierr)
       if(ierr.ne.0) go to 9100

C -- The names of the dimension that is not rearranged is added.

       if(acolrow.eq.'row')then
         do icol=1,ncol
           mat3%acol(icol)=mat1%acol(icol)
         end do
       else
         do irow=1,nrow
           mat3%arow(irow)=mat1%arow(irow)
         end do
       end if

C -- Now the rearrangement takes place.

       j=0
       do i=1,nmatch
         if(acolrow.eq.'row')then
           matchtype='Row'
           call whichone(ifail,nrow,j,mat1%arow,amatch(i))
         else
           matchtype='Column'
           call whichone(ifail,ncol,j,mat1%acol,amatch(i))
         end if
         if(ifail.ne.0)then
           write(amessage,120) trim(amatch(i)),trim(amatmatchfile),
     +     trim(amatinfile)
120        format(' ',a,' name "',a,'" featured in matrix contained ',
     +     'in file ',a,' is not featured in matrix contained in file ',
     +     a,'.')
           go to 9800
         end if
         if(acolrow.eq.'row')then
           mat3%arow(i)=amatch(i)
           do k=1,ncol
             mat3%array(k,i)=mat1%array(k,j)
           end do
         else
           mat3%acol(i)=amatch(i)
           do k=1,nrow
             mat3%array(i,k)=mat1%array(j,k)
           end do
         end if
       end do

C -- The new matrix is compressed again if possible.

       if(ncol.eq.nrow)then
         if(abs(icode).eq.1)then
           mat3%icode=icode1
           if(icode1.eq.-1)then
             allocate(mat3%vector(nrow),stat=ierr)
             if(ierr.ne.0) go to 9100
             do irow=1,nrow
               mat3%vector(irow)=mat3%array(irow,irow)
             end do
           end if
         end if
       end if

C -- The new matrix file is written.

       call mat_write(ifail,20,mat3,matoutfile,amessage,cline)
       if(ifail.ne.0) go to 9800
       call addquote(matoutfile,afile)
       write(6,200) trim(afile)
200    format(' - file ',a,' written ok.')

       go to 9900

8900   continue
       write(6,8910)
8910   format(/,' MATMATCH is run using the command:',/)
       write(6,8920)
8920   format('     matmatch matinfile matmatchfile rowcol ',
     + 'matoutfile',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8940)
8940   format('     matfile      is an existing matrix file,')
       write(6,8945)
8945   format('     matmatchfile contains the matrix to be matched,')
       write(6,8950)
8950   format('     rowcol       is "row" or "column", and'
       write(6,8960)
8960   format('     matoutfile   is the file to which the ',
     + rearranged matrix is written.')

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

