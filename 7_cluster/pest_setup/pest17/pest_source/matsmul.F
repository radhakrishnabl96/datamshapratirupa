       include 'matman.F'


       program matsmul

C -- Program MATSMUL multiplies a matrix by a scalar.

       use matrix_manipulate
       implicit none

       logical          :: lopened
       integer          :: ifail,irow,icol,nrow,ncol,ierr
       integer          :: lw(4),rw(4)
       double precision :: dnum
       character*12     :: aversion
       character*20     :: anum
       character*200    :: matinfile,matoutfile,amatinfile,amatoutfile
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
10     format(/,' MATSMUL version ',a,'. Watermark Numerical ',
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
       matinfile=cline(lw(1):rw(1))
       call remchar(matinfile,char(211))
       anum=cline(lw(2):rw(2))
       call remchar(anum,char(211))
       call drealread(ifail,anum,dnum)
       if(ifail.ne.0) go to 8900
       matoutfile=cline(lw(3):rw(3))
       call remchar(matoutfile,char(211))
#ifndef UNIX
       call lowcas(matinfile)
       call lowcas(matoutfile)
#endif

       call addquote(matinfile,amatinfile)
       call addquote(matoutfile,amatoutfile)

C -- The matrix is read.

       call mat_read(ifail,10,mat,matinfile,amessage,cline)
       if(ifail.ne.0) go to 9800
       write(6,100) trim(amatinfile)
100    format(' - file ',a,' read ok.')

C -- Scalar multiplication is undertaken.

       nrow=mat%nrow
       ncol=mat%ncol

       if(mat%icode.eq.-1)then
         do irow=1,nrow
           mat%vector(irow)=mat%vector(irow)*dnum
         end do
       else
         do icol=1,ncol
           do irow=1,nrow
             mat%array(irow,icol)=mat%array(irow,icol)*dnum
           end do
         end do
       end if

C -- The new matrix file is written.

       call mat_write(ifail,20,mat,matoutfile,amessage,cline)
       if(ifail.ne.0) go to 9800
       write(6,200) trim(amatoutfile)
200    format(' - file ',a,' written ok.')

       go to 9900

8900   continue
       write(6,8910)
8910   format(/,' MATSMUL is run using the command:',/)
       write(6,8920)
8920   format('     matsmul matinfile number matoutfile',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8940)
8940   format('     matinfile  is a file containing a matrix,')
       write(6,8945)
8945   format('     number     is the matrix scalar multiplier, ',
     + 'and')
       write(6,8950)
8950   format('     matoutfile is the file to which the new matrix ',
     + 'will be written.')

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



        SUBROUTINE DREALREAD(IFAIL,CLINE,RTEMP)

C -- Subroutine DREALREAD reads a real number from a string.

        INTEGER IFAIL
        double precision RTEMP
        CHARACTER*8 AFMT
        CHARACTER*(*) CLINE

        IFAIL=0
        AFMT='(F   .0)'
        WRITE(AFMT(3:5),'(I3)') LEN(CLINE)
        READ(CLINE,AFMT,ERR=100) RTEMP
        RETURN

100     IFAIL=1
        RETURN
        END


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


