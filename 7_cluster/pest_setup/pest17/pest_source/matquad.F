       include 'matman.F'


       program matquad

C -- Program MATQUAD computes y'My

       use matrix_manipulate
       implicit none

       logical          :: lopened
       integer          :: ifail,iwarn,ncol,ierr,i,j
       integer          :: lw(4),rw(4)
       double precision :: answer,rtemp
       character*12     :: aversion
       character*200    :: vecfile,matfile,outfile
       character*200    :: afile,bfile
       character*500    :: cline
       character*600    :: amessage
       type(dmatrix)    :: vec,mat

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' MATQUAD version ',a,'. Watermark Numerical ',
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
       vecfile=cline(lw(1):rw(1))
       call remchar(vecfile,char(211))
       matfile=cline(lw(2):rw(2))
       call remchar(matfile,char(211))
       outfile=cline(lw(3):rw(3))
       call remchar(outfile,char(211))
#ifndef UNIX
       call lowcas(vecfile)
       call lowcas(matfile)
       call lowcas(outfile)
#endif

C -- The two matrices are read.

       call mat_read(ifail,10,vec,vecfile,amessage,cline)
       if(ifail.ne.0) go to 9800
       call addquote(vecfile,afile)
       write(6,100) trim(afile)
100    format(' - file ',a,' read ok.')

       call mat_read(ifail,10,mat,matfile,amessage,cline)
       if(ifail.ne.0) go to 9800
       call addquote(matfile,bfile)
       write(6,100) trim(bfile)

C -- The two matrices are checked for compatibility.

       if(vec%ncol.ne.1)then
         write(amessage,110) trim(afile)
110      format(' The matrix in file ',a,' must have only one column.')
         go to 9800
       end if
       if(mat%ncol.ne.mat%nrow)then
         write(amessage,120) trim(bfile)
120      format(' The matrix in file ',a,' must be square.')
         go to 9800
       end if
       if(vec%nrow.ne.mat%ncol)then
         write(amessage,130) trim(afile),trim(bfile)
130      format(' The number of rows in the matrix of file ',a,
     +   ' must be the same as the number of rows/columns in the ',
     +   'matrix of file ',a,'.')
         go to 9800
       end if

       ncol=mat%ncol

C -- The quadratic form is calculated. (Note that this is inefficient where
C    the matrix is symmetric.

       if(mat%icode.eq.-1)then
         answer=0.0d0
         do i=1,ncol
           answer=answer+vec%array(i,1)*vec%array(i,1)*mat%vector(i)
         end do
       else
         answer=0.0d0
         do i=1,ncol
           rtemp=vec%array(i,1)
           do j=1,ncol
             answer=answer+vec%array(j,1)*mat%array(j,i)*rtemp
           end do
         end do
       end if

C -- The new matrix file is written.

       call addquote(outfile,afile)
       open(unit=20,file=outfile,err=9000)
       write(20,200,err=9000) 1,1,1
200    format(3i6)
       write(20,210,err=9000) answer
210    format(1x,1pg14.7)
       write(20,220)
220    format('* row and column names')
       write(20,230,err=9000) trim(vec%arow(1))
230    format(a)
       close(unit=20)
       write(6,250) trim(afile)
250    format(' - file ',a,' written ok.')

C -- The scalar computed quadratic form is written to the screen.

       write(6,260) answer
260    format(/,' Scalar quadratic form = ',1pg12.5)


C -- The warning is written if necessary.

       iwarn=0
       do i=1,ncol
         if(vec%arow(i).ne.mat%arow(i))then
           iwarn=1
           exit
         end if
       end do
       if(iwarn.ne.0)then
         write(amessage,280) trim(afile),trim(bfile)
280      format(' Warning: row names are not ',
     +   'identical (and/or not in the same order) for the vector ',
     +   'in file ',a,' and the matrix in file ',a,'.')
         go to 9800
       end if
       if(mat%icode.eq.2)then
         do i=1,ncol
           if(mat%arow(i).ne.mat%acol(i))then
             iwarn=1
             exit
           end if
         end do
       end if
       if(iwarn.ne.0)then
         write(amessage,290) trim(bfile)
290      format(' Warning: row and column names in the matrix ',
     +   'contained in file ',a,' are not identical.')
         go to 9800
       end if

       go to 9900

8900   continue
       write(6,8910)
8910   format(/,' MATQUAD is run using the command:',/)
       write(6,8920)
8920   format('     matquad vecfile matfile outfile',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8940)
8940   format('     vecfile contains a Nx1 matrix (ie a vector),')
       write(6,8950)
8950   format('     matfile contains a MxN square matrix, and')
       write(6,8960)
8960   format('     outfile contains the calculated y''My .')

       go to 9999


9000   write(amessage,9010) trim(afile)
9010   format(' Cannot write to file ',a,'.')
       go to 9800

9800   continue
       call writmess(6,amessage)

9900   continue
       call mat_deallocate(ifail,vec)
       call mat_deallocate(ifail,mat)
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

