       include 'matman.F'


       program mat2jco

C -- Program MAT2JCO rewrites a matrix file as a JCO file.

       use matrix_manipulate
       implicit none

       logical       :: lopened
       integer       :: ifail,irow,icol,ierr,n,m,icount
       integer       :: lw(4),rw(4)
       character*4   :: aext
       character*12  :: aversion,apar12
       character*200 :: matfile,jcofile
       character*200 :: afile
       character*500 :: cline
       character*600 :: amessage
       type(dmatrix) :: mat

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' MAT2JCO version ',a,'. Watermark Numerical ',
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
       call linspl(ifail,3,lw,lw,cline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,2,lw,rw,cline)
       if(ifail.ne.0) go to 8900
       matfile=cline(lw(1):rw(1))
       call remchar(matfile,char(211))
       jcofile=cline(lw(2):rw(2))
       call remchar(jcofile,char(211))
#ifndef UNIX
       call lowcas(matfile)
       call lowcas(jcofile)
#endif

       n=len_trim(jcofile)
       m=n-3
       if(m.lt.1)m=1
       aext=jcofile(m:n)
       call lowcas(aext)
       if(aext.ne.'.jco')then
         write(amessage,20) trim(jcofile)
20       format(' JCO file ',a,' should have an extension of ".jco".')
         go to 9800
       end if

C -- The matrix file is read.

       call mat_read(ifail,10,mat,matfile,amessage,cline)
       if(ifail.ne.0) go to 9800
       call addquote(matfile,afile)
       write(6,100) trim(afile)
100    format(' - file ',a,' read ok.')

       call mat_diag_expand(ifail,mat,amessage)
       if(ifail.ne.0) go to 9800
       if(mat%ncol.eq.mat%nrow)then
         call mat_icode_expand(ifail,mat,amessage)
         if(ifail.ne.0) go to 9800
       end if

C -- The JCO file is written.

#ifdef LAHEY
#ifdef LF90
       open(unit=20,file=jcofile,form='unformatted',
     + access='transparent')
#else
       open(unit=20,file=jcofile,form='binary')
#endif
#else
       open(unit=20,file=jcofile,form='unformatted',
     + access='stream',status='replace')
#endif

       write(20) -mat%ncol,-mat%nrow
       icount=0
       do icol=1,mat%ncol
         do irow=1,mat%nrow
           if(mat%array(irow,icol).ne.0.0d0) icount=icount+1
         end do
       end do
       write(20) icount
       do icol=1,mat%ncol
         do irow=1,mat%nrow
           if(mat%array(irow,icol).ne.0.0d0)then
             icount=(icol-1)*mat%nrow+irow
             write(20) icount,mat%array(irow,icol)
           end if
         end do
       end do
       do icol=1,mat%ncol
         apar12=adjustl(mat%acol(icol))
         write(20) apar12
       end do
       do irow=1,mat%nrow
         write(20) mat%arow(irow)
       end do
       close(unit=20)
       write(6,200) trim(jcofile)
200    format(' - JCO file ',a,' written ok.')

       go to 9900

8900   continue
       write(6,8910)
8910   format(/,' MAT2JCO is run using the command:',/)
       write(6,8920)
8920   format('     mat2jco matfile jcofile ',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8940)
8940   format('     matfile is a matrix file, and')
       write(6,8950)
8950   format('     jcofile is a binary Jacobian matrix file.')

       go to 9999

9800   continue
       call writmess(6,amessage)

9900   continue
       call mat_deallocate(ifail,mat)
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

