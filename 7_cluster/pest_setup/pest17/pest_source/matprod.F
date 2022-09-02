       include 'matman.F'


       program matprod

C -- Program MATPROD multiplies one matrix by another.

       use matrix_manipulate
       implicit none

       logical       :: lopened
       integer       :: ifail,iwarn,irow,icol,ierr
       integer       :: nrow1,nrow2,nrow3,ncol1,ncol2,ncol3,
     +                  icode1,icode2,icode3,k
       integer       :: lw(4),rw(4)
       double precision :: dtemp
       character*12  :: aversion
       character*200 :: matfile1,matfile2,matfile3
       character*200 :: afile,bfile
       character*500 :: cline
       character*600 :: amessage
       type(dmatrix) :: mat1,mat2,mat3

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' MATPROD version ',a,'. Watermark Numerical ',
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
       matfile1=cline(lw(1):rw(1))
       call remchar(matfile1,char(211))
       matfile2=cline(lw(2):rw(2))
       call remchar(matfile2,char(211))
       matfile3=cline(lw(3):rw(3))
       call remchar(matfile3,char(211))
#ifndef UNIX
       call lowcas(matfile1)
       call lowcas(matfile2)
       call lowcas(matfile3)
#endif

C -- The two matrices are read.

       call mat_read(ifail,10,mat1,matfile1,amessage,cline)
       if(ifail.ne.0) go to 9800
       call addquote(matfile1,afile)
       write(6,100) trim(afile)
100    format(' - file ',a,' read ok.')

       call mat_read(ifail,10,mat2,matfile2,amessage,cline)
       if(ifail.ne.0) go to 9800
       call addquote(matfile2,bfile)
       write(6,100) trim(bfile)

C -- The two matrices are checked for compatibility.

       if(mat1%ncol.ne.mat2%nrow)then
         write(amessage,110) trim(afile),trim(bfile)
110      format(' The matrix in file ',a,' cannot be multpiled by the ',
     +   'matrix in file ',a,' as the number of rows in the latter ',
     +   'matrix does not equal the number of columns in the former ',
     +   'matrix.')
         go to 9800
       end if

       iwarn=0
       nrow1=mat1%nrow
       ncol1=mat1%ncol
       nrow2=mat2%nrow
       ncol2=mat2%ncol
       nrow3=nrow1
       ncol3=ncol2
       mat3%nrow=nrow3
       mat3%ncol=ncol3
       icode1=mat1%icode
       icode2=mat2%icode

C -- First we handle the situation where both matrices are diagonal.

       if((icode1.eq.-1).and.(icode2.eq.-1)) then
         mat3%icode=-1
         allocate(mat3%vector(nrow3),mat3%arow(nrow3),stat=ierr)
         if(ierr.ne.0) go to 9100
         do irow=1,nrow3
           mat3%vector(irow)=mat1%vector(irow)*mat2%vector(irow)
         end do
         do irow=1,nrow3
           mat3%arow(irow)=mat1%arow(irow)
         end do
         do irow=1,nrow3
           if(mat1%arow(irow).ne.mat2%arow(irow)) go to 90
         end do
         go to 350
90       continue
         call mat_diag_expand(ifail,mat3,amessage)
         if(ifail.ne.0) go to 9800
         call mat_icode_expand(ifail,mat3,amessage)
         if(ifail.ne.0) go to 9800
         do irow=1,nrow3
           mat3%acol(irow)=mat2%arow(irow)
         end do
         iwarn=1
         go to 350
       else
         mat3%icode=2
         allocate(mat3%array(nrow3,ncol3),mat3%arow(nrow3),
     +   mat3%acol(ncol3),stat=ierr)
         if(ierr.ne.0) go to 9100
         if(icode1.eq.-1)then
           do irow=1,nrow3
             dtemp=mat1%vector(irow)
             do icol=1,ncol3
               mat3%array(irow,icol)=dtemp*mat2%array(irow,icol)
             end do
           end do
         else if(icode2.eq.-1)then
           do icol=1,ncol3
             dtemp=mat2%vector(icol)
             do irow=1,nrow3
               mat3%array(irow,icol)=mat1%array(irow,icol)*dtemp
             end do
           end do
         else
           do icol=1,ncol3
             do irow=1,nrow3
               dtemp=0.0d0
               do k=1,ncol1
                 dtemp=dtemp+mat1%array(irow,k)*mat2%array(k,icol)
               end do
               mat3%array(irow,icol)=dtemp
             end do
           end do
         end if
         do irow=1,nrow3
           mat3%arow(irow)=mat1%arow(irow)
         end do
         if(icode2.eq.2)then
           do icol=1,ncol3
             mat3%acol(icol)=mat2%acol(icol)
           end do
         else
           do icol=1,ncol3
             mat3%acol(icol)=mat2%arow(icol)
           end do
         end if

         if(ncol3.eq.nrow3)then
           do irow=1,nrow3
             if(mat3%arow(irow).ne.mat3%acol(irow)) go to 175
           end do
           deallocate(mat3%acol,stat=ierr)
           nullify(mat3%acol)
           mat3%icode=1
         end if
175      continue

       end if

180    continue

       iwarn=0
       if(icode1.eq.2)then
         do icol=1,ncol1
           if(mat1%acol(icol).ne.mat2%arow(icol))then
             iwarn=1
             go to 250
           end if
         end do
       else
         do irow=1,nrow1
           if(mat1%arow(irow).ne.mat2%arow(irow))then
             iwarn=1
             go to 250
           end if
         end do
       end if
250    continue

350    continue

C -- The new matrix file is written.

       call mat_write(ifail,20,mat3,matfile3,amessage,cline)
       if(ifail.ne.0) go to 9800
       call addquote(matfile3,afile)
       write(6,200) trim(afile)
200    format(' - file ',a,' written ok.')

C -- The warning is written if necessary.

       if(iwarn.ne.0)then
         call addquote(matfile1,afile)
         call addquote(matfile2,bfile)
         write(amessage,255) trim(afile),trim(bfile)
255      format(' Warning: column names pertaining to the matrix ',
     +   'contained in file ',a,' are not the same as the row ',
     +   'names pertaining to the matrix contained in file ',a,'.')
         go to 9800
       end if

       go to 9900

8900   continue
       write(6,8910)
8910   format(/,' MATPROD is run using the command:',/)
       write(6,8920)
8920   format('     matprod matfile1 matfile2 matoutfile',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8940)
8940   format('     matfile1 contains an m*n matrix A')
       write(6,8950)
8950   format('     matfile2 contains an n*p matrix B, and')
       write(6,8960)
8960   format('     matoutfile will contain the m*p matrix AB.')

       go to 9999


9100   write(amessage,9110)
9110   format(' Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9800

9800   continue
       call writmess(6,amessage)

9900   continue
       call mat_deallocate(ifail,mat1)
       call mat_deallocate(ifail,mat2)
       call mat_deallocate(ifail,mat3)
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

