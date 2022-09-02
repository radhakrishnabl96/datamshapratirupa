       include 'matman.F'

       program matspec

C -- Program MATSPEC writes useful matrix statistics to the screen and to a file.

       use matrix_manipulate
       implicit none

       logical          :: lopened
       integer          :: ifail,ncol,nrow,ierr,icol,irow,
     +                     irowmax,irowmin,irowamax,irowamin,
     +                     icolmax,icolmin,icolamax,icolamin
       integer          :: lw(5),rw(5)
       real             :: rtemp,rmax,rmin,ramax,ramin
       character*9      :: arow,acol,anum
       character*12     :: aversion
       character*200    :: matfile,outfile
       character*200    :: afile
       character*500    :: cline
       character*600    :: amessage
       type(dmatrix)    :: mat

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' MATSPEC version ',a,'. Watermark Numerical ',
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
       outfile=cline(lw(2):rw(2))
       call remchar(outfile,char(211))
#ifndef UNIX
       call lowcas(matfile)
       call lowcas(outfile)
#endif

C -- The matrix is read.

       call mat_read(ifail,10,mat,matfile,amessage,cline)
       if(ifail.ne.0) go to 9800
       call addquote(matfile,afile)
       write(6,100) trim(afile)
100    format(' - file ',a,' read ok.')

C -- The matrix is expanded if necessary.

       if(mat%icode.eq.-1)then
         call mat_diag_expand(ifail,mat,amessage)
         if(ifail.gt.0) go to 9800
       end if
       if(mat%icode.eq.1)then
         call mat_icode_expand(ifail,mat,amessage)
         if(ifail.gt.0) go to 9800
       end if

C -- Some properties of the matrix are evaluated and written.

       call addquote(outfile,afile)
       open(unit=20,file=outfile,action='write',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,130) trim(afile)
130      format(' Cannot open file ',a,' for output.')
         go to 9800
       end if

       ncol=mat%ncol
       nrow=mat%nrow
       call writint(anum,nrow)
       write(20,140) trim(anum)
140    format(' Number of rows    in matrix    = ',a)
       call writint(anum,ncol)
       write(20,150) trim(anum)
150    format(' Number of columns in matrix    = ',a)

       write(20,*)
       write(20,170)
170    format(' Specification',t27,'Value',t39,'Row No',t48,
     + 'Col No',t57,'Row Name',t78,'Col Name')
       write(20,180)
180    format(1x,84('-'))

C -- Max/min values and abs values.

       rmax=-1.0e35
       rmin=1.0e35
       ramax=-1.0e35
       ramin=1.0e35
       do icol=1,ncol
         do irow=1,nrow
           rtemp=mat%array(irow,icol)
           if(rtemp.gt.rmax)then
             rmax=rtemp
             irowmax=irow
             icolmax=icol
           end if
           if(rtemp.lt.rmin)then
             rmin=rtemp
             irowmin=irow
             icolmin=icol
           end if
           rtemp=abs(rtemp)
           if(rtemp.gt.ramax)then
             ramax=rtemp
             irowamax=irow
             icolamax=icol
           end if
           if(rtemp.lt.ramin)then
             ramin=rtemp
             irowamin=irow
             icolamin=icol
           end if
         end do
       end do

       call writint(arow,irowmax)
       call writint(acol,icolmax)
       write(20,200) rmax,trim(arow),trim(acol),
     +               trim(mat%arow(irowmax)),trim(mat%acol(icolmax))
200    format(' Max element',t25,1pg12.5,t39,a,t48,a,t57,a,t78,a)
       call writint(arow,irowmin)
       call writint(acol,icolmin)
       write(20,210) rmin,trim(arow),trim(acol),
     +               trim(mat%arow(irowmin)),trim(mat%acol(icolmin))
210    format(' Min element',t25,1pg12.5,t39,a,t48,a,t57,a,t78,a)
       call writint(arow,irowamax)
       call writint(acol,icolamax)
       write(20,220) ramax,trim(arow),trim(acol),
     +               trim(mat%arow(irowamax)),trim(mat%acol(icolamax))
220    format(' Max abs element',t25,1pg12.5,t39,a,t48,a,t57,a,t78,a)
       call writint(arow,irowamin)
       call writint(acol,icolamin)
       write(20,230) ramin,trim(arow),trim(acol),
     +               trim(mat%arow(irowamin)),trim(mat%acol(icolamin))
230    format(' Min abs element',t25,1pg12.5,t39,a,t48,a,t57,a,t78,a)


C -- If it is square the diagonal element is analysed.

       if(ncol.eq.nrow)then
         rmax=-1.0e35
         rmin=1.0e35
         ramax=-1.0e35
         ramin=1.0e35
         do irow=1,nrow
           rtemp=mat%array(irow,irow)
           if(rtemp.gt.rmax)then
             rmax=rtemp
             irowmax=irow
           end if
           if(rtemp.lt.rmin)then
             rmin=rtemp
             irowmin=irow
           end if
           rtemp=abs(rtemp)
           if(rtemp.gt.ramax)then
             ramax=rtemp
             irowamax=irow
           end if
           if(rtemp.lt.ramin)then
             ramin=rtemp
             irowamin=irow
           end if
         end do
         icolmax=irowmax
         icolmin=irowmin
         icolamax=irowamax
         icolamin=irowamin
         call writint(arow,irowmax)
         call writint(acol,icolmax)
         write(20,250) rmax,trim(arow),trim(acol),
     +                 trim(mat%arow(irowmax)),trim(mat%acol(icolmax))
250      format(' Max diag elem',t25,1pg12.5,t39,a,t48,a,t57,a,t78,a)
         call writint(arow,irowmin)
         call writint(acol,icolmin)
         write(20,260) rmin,trim(arow),trim(acol),
     +                 trim(mat%arow(irowmin)),trim(mat%acol(icolmin))
260      format(' Min diag elem',t25,1pg12.5,t39,a,t48,a,t57,a,t78,a)
         call writint(arow,irowamax)
         call writint(acol,icolamax)
         write(20,270) ramax,trim(arow),trim(acol),
     +                 trim(mat%arow(irowamax)),trim(mat%acol(icolamax))
270      format(' Max abs diag elem',t25,1pg12.5,t39,a,t48,a,t57,a,
     +   t78,a)
         call writint(arow,irowamin)
         call writint(acol,icolamin)
         write(20,280) ramin,trim(arow),trim(acol),
     +                 trim(mat%arow(irowamin)),trim(mat%acol(icolamin))
280      format(' Min abs diag elemt',t25,1pg12.5,t39,a,t48,a,t57,a,
     +   t78,a)
       end if

       close(unit=20)
       write(6,1010)trim(afile)
1010   format(' - file ',a,' written ok.')
       go to 9900

8900   continue
       write(6,8910)
8910   format(/,' MATSPEC is run using the command:',/)
       write(6,8920)
8920   format('     matspec matfile outfile',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8940)
8940   format('     matfile contains a matrix, and ')
       write(6,8960)
8960   format('     outfile lists some useful matrix specifications.')

       go to 9999

9800   continue
       call writmess(6,amessage)

9900   continue
       call mat_deallocate(ifail,mat)

       inquire(unit=10,opened=lopened)
       if(lopened) close(unit=10,iostat=ierr)
       inquire(unit=20,opened=lopened)
       if(lopened) close(unit=20,iostat=ierr)


9999   continue

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

