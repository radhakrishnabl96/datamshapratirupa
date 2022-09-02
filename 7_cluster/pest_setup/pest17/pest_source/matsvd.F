       include 'matman.F'


       program matsvd

C -- Program MATSVD performs singular value decomposition on a matrix.

       use matrix_manipulate
       implicit none

       logical       :: lopened
       integer       :: ifail,irow,icol,nrow,ncol,ierr,nnrow,
     +                  lwork,info
       integer       :: lw(5),rw(5)

       double precision, allocatable :: work(:),a(:,:),u(:,:),vt(:,:),
     +                                  s(:)

       character*5   :: atemp
       character*12  :: aversion
       character*200 :: matfile,ufile,sfile,vtfile
       character*200 :: afile
       character*500 :: cline
       character*600 :: amessage
       type(dmatrix) :: mat,matu,mats,matvt

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' MATSVD version ',a,'. Watermark Numerical ',
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
       matfile=cline(lw(1):rw(1))
       call remchar(matfile,char(211))
       ufile=cline(lw(2):rw(2))
       call remchar(ufile,char(211))
       sfile=cline(lw(3):rw(3))
       call remchar(sfile,char(211))
       vtfile=cline(lw(4):rw(4))
       call remchar(vtfile,char(211))
#ifndef UNIX
       call lowcas(matfile)
       call lowcas(ufile)
       call lowcas(sfile)
       call lowcas(vtfile)
#endif

C -- The matrix is read.

       call mat_read(ifail,10,mat,matfile,amessage,cline)
       if(ifail.ne.0) go to 9800
       call addquote(matfile,afile)
       write(6,100) trim(afile)
100    format(' - file ',a,' read ok.')

C -- We allocate the memory that we will need.

       nrow=mat%nrow
       ncol=mat%ncol
       nnrow=min(ncol,nrow)
       lwork=2*max(3*min(nrow,ncol)+max(nrow,ncol),5*min(nrow,ncol))
       allocate(a(nrow,ncol),u(nrow,nrow),s(nnrow),
     + vt(ncol,ncol),work(lwork),stat=ierr)
       if(ierr.ne.0) go to 9100

       if(mat%icode.eq.-1)then
         a=0.0d0
         do irow=1,nrow
           a(irow,irow)=mat%vector(irow)
         end do
         deallocate(mat%vector,stat=ierr)
       else
         do icol=1,ncol
           do irow=1,nrow
             a(irow,icol)=mat%array(irow,icol)
           end do
         end do
         deallocate(mat%array,stat=ierr)
       end if


C -- Now undertake SVD.

       write(6,120)
120    format(/,' Working...',/)

       call dgesvd('A','A',nrow,ncol,a,nrow,s,u,nrow,vt,ncol,
     + work,lwork,info)

       if(info.ne.0)then
         write(amessage,130)
130      format(' SVD was not successful.')
         go to 9800
       end if

       deallocate(work,a,stat=ierr)

C -- The matrices produced as an outcome of the SVD process are now
C    transferred to dmatrix structures and written to respective files.

C -- First the U matrix.

       matu%nrow=nrow
       matu%ncol=nrow
       matu%icode=2
       allocate(matu%array(nrow,nrow),matu%arow(nrow),
     + matu%acol(nrow),stat=ierr)
       if(ierr.ne.0) go to 9100
       do icol=1,nrow
         do irow=1,nrow
           matu%array(irow,icol)=u(irow,icol)
         end do
       end do
       do irow=1,nrow
         matu%arow(irow)=mat%arow(irow)
       end do
       do icol=1,nrow
         call writint(atemp,icol)
         matu%acol(icol)='eig'//trim(atemp)
       end do

       call mat_write(ifail,20,matu,ufile,amessage,cline)
       if(ifail.ne.0) go to 9800
       call addquote(ufile,afile)
       write(6,200) trim(afile)
200    format(' - file ',a,' written ok.')

       call mat_deallocate(ifail,matu)

C -- Now the S matrix.

       mats%nrow=nnrow
       mats%ncol=nnrow
       mats%icode=-1
       allocate(mats%vector(nnrow),mats%arow(nnrow),stat=ierr)
       if(ierr.ne.0) go to 9100
       do irow=1,nnrow
         mats%vector(irow)=s(irow)
       end do
       do irow=1,nnrow
         call writint(atemp,irow)
         mats%arow(irow)='eig'//trim(atemp)
       end do

       call mat_write(ifail,20,mats,sfile,amessage,cline)
       if(ifail.ne.0) go to 9800
       call addquote(sfile,afile)
       write(6,200) trim(afile)

       call mat_deallocate(ifail,mats)

C -- Now the VT matrix.

       matvt%nrow=ncol
       matvt%ncol=ncol
       matvt%icode=2
       allocate(matvt%array(ncol,ncol),matvt%arow(ncol),
     + matvt%acol(ncol),stat=ierr)
       if(ierr.ne.0) go to 9100
       do icol=1,ncol
         do irow=1,ncol
           matvt%array(irow,icol)=vt(irow,icol)
         end do
       end do
       do irow=1,ncol
         call writint(atemp,irow)
         matvt%arow(irow)='eig'//trim(atemp)
       end do
       if(mat%icode.eq.2)then
         do icol=1,ncol
           matvt%acol(icol)=mat%acol(icol)
         end do
       else
         do icol=1,ncol
           matvt%acol(icol)=mat%arow(icol)
         end do
       end if

       call mat_write(ifail,20,matvt,vtfile,amessage,cline)
       if(ifail.ne.0) go to 9800
       call addquote(vtfile,afile)
       write(6,200) trim(afile)

       go to 9900


8900   continue
       write(6,8910)
8910   format(/,' MATSVD is run using the command:',/)
       write(6,8920)
8920   format('     matsvd matfile umatfile smatfile vtmatfile',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8940)
8940   format('     matfile   is a matrix file,')
       write(6,8950)
8950   format('     umatfile  will contain the U matrix,')
       write(6,8960)
8960   format('     smatfile  will contain the S (singular value) ',
     + 'matrix, and')
       write(6,8970)
8970   format('     vtmatfile will contain the VT matrix.')
       go to 9999


9100   write(amessage,9110)
9110   format(' Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9800

9800   continue
       call writmess(6,amessage)

9900   continue
       call mat_deallocate(ifail,mat)
       call mat_deallocate(ifail,matu)
       call mat_deallocate(ifail,mats)
       call mat_deallocate(ifail,matvt)
       deallocate(a,u,s,vt,work,stat=ierr)

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

