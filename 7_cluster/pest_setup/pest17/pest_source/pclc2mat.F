       include 'matman.F'


       program pclc2mat

C -- Program PCLC2MAT lists an eigenvector from a PARCAL.IN file as a vector
C    in a matrix file.


       use matrix_manipulate
       implicit none


       logical          :: lopened
       integer          :: ifail,irow,icol,nrow,ncol,ierr,ieig,i,j,ipar
       integer          :: npar,nsup,nespar,ntpfle
       integer          :: lw(4),rw(4)
       double precision :: value,svalue,sval,lbound,ubound,scale,offset
       character*12     :: aversion
       character*15     :: aeig,asup
       character*200    :: parcalcfile,matfile,aparcalcfile,amatfile
       character*200    :: tplfle,infle
       character*500    :: cline
       character*600    :: amessage
       type(dmatrix)    :: mat

       integer, allocatable          :: itrans(:)
       double precision, allocatable :: eigmat(:,:)
       character*12, allocatable     :: apar(:)

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' PCLC2MAT version ',a,'. Watermark Numerical ',
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
       parcalcfile=cline(lw(1):rw(1))
       call remchar(parcalcfile,char(211))
       call intread(ifail,cline(lw(2):rw(2)),ieig)
       if(ifail.ne.0) go to 8900
       if(ieig.eq.0) go to 8900
       matfile=cline(lw(3):rw(3))
       call remchar(matfile,char(211))
#ifndef UNIX
       call lowcas(parcalcfile)
       call lowcas(matfile)
#endif

       call addquote(parcalcfile,aparcalcfile)
       call addquote(matfile,amatfile)

! -- The parcalc input file is now read.

        open(unit=10,file=parcalcfile,status='old',iostat=ierr)
        if(ierr.ne.0)then
          write(amessage,15) trim(aparcalcfile)
15        format(' Cannot open PARCALC input file ',a,'.')
          go to 9800
        end if
        read(10,*,err=9000,end=9100)
        read(10,*,err=9000,end=9100) npar,nsup,nespar
        read(10,*,err=9000,end=9100) ntpfle
        allocate(apar(npar),itrans(npar),stat=ierr)
        if(ierr.ne.0) go to 9200
        allocate(eigmat(nespar,nsup),stat=ierr)
        if(ierr.ne.0) go to 9200
        read(10,*,err=9000,end=9100)
        do i=1,nsup
          read(10,*,err=9000,end=9100) value,svalue
        end do
        read(10,*,err=9000,end=9100)
        do i=1,npar
          read(10,*,err=9000,end=9100) apar(i),itrans(i),sval,lbound,
     +    ubound,scale,offset
        end do
        read(10,*,err=9000,end=9100)
        do i=1,ntpfle
          read(10,*,err=9000,end=9100) tplfle,infle
        end do
        read(10,*,err=9000,end=9100)
        do i=1,nespar
          read(10,*,err=9000,end=9100) (eigmat(i,j),j=1,nsup)
        end do
        close(unit=10)
        write(6,120) trim(aparcalcfile)
120     format(' - file ',a,' read ok.')


        if(ieig.gt.0) then

C -- A matrix is now formed out of the nominated column.

          if(ieig.gt.nsup)then
            call writint(aeig,ieig)
            call writint(asup,nsup)
            write(amessage,130) trim(aeig),trim(asup),trim(aparcalcfile)
130         format(' Cannot write super parameter number ',a,
     +      ' to a matrix ',
     +      'file as there is data for only ',a,' super parameters ',
     +      'recorded in file ',a,'.')
            go to 9800
          end if

          mat%nrow=nespar
          mat%ncol=1
          mat%icode=2
          allocate(mat%array(nespar,1),mat%arow(nespar),
     +    mat%acol(1),stat=ierr)
          if(ierr.ne.0) go to 9800

          irow=0
          do ipar=1,npar
            if(itrans(ipar).ge.0)then
              irow=irow+1
              mat%arow(irow)=apar(ipar)
            end if
          end do
          call writint(aeig,ieig)
          mat%acol(1)='eig'//trim(aeig)
          do irow=1,nespar
            mat%array(irow,1)=eigmat(irow,ieig)
          end do

       else

          ieig=-ieig
          if(ieig.gt.nsup) ieig=nsup

          mat%nrow=nespar
          mat%ncol=ieig
          mat%icode=2
          allocate(mat%array(nespar,ieig),mat%arow(nespar),
     +    mat%acol(ieig),stat=ierr)
          if(ierr.ne.0) go to 9800

          irow=0
          do ipar=1,npar
            if(itrans(ipar).ge.0)then
              irow=irow+1
              mat%arow(irow)=apar(ipar)
            end if
          end do
          do i=1,ieig
            call writint(aeig,i)
            mat%acol(i)='eig'//trim(aeig)
          end do
          do i=1,ieig
            do irow=1,nespar
              mat%array(irow,i)=eigmat(irow,i)
            end do
          end do

       end if

C -- The new matrix file is written.

       call mat_write(ifail,20,mat,matfile,amessage,cline)
       if(ifail.ne.0) go to 9800
       write(6,210) trim(amatfile)
210    format(' - file ',a,' written ok.')

       go to 9900

8900   continue
       write(6,8910)
8910   format(/,' PCLC2MAT is run using the command:',/)
       write(6,8920)
8920   format('     pclc2mat parcalcfile ipar matoutfile',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8940)
8940   format('     parcalcfile  is a PARCALC input file ',
     + '(normally parcalc.in),')
       write(6,8950)
8950   format('     ipar         is a super parameter number, and')
       write(6,8960)
8960   format('     matoutfile   will contain the components of ',
     + 'the ipar''th super parameter.')
       write(6,8970)
8970   format(/,'     Note: supply a negative ipar to write first ',
     + 'ipar super parameters ',/,'     to matrix file.')

       go to 9999

9000   write(amessage,9010) trim(aparcalcfile)
9010   format(' Error reading PARCALC input file ',a,'.')
       go to 9800
9100   write(amessage,9110) trim(aparcalcfile)
9110   format(' Unexpected end encountered to PARCALC input file ',
     + a,'.')
       go to 9800
9200   write(amessage,9210)
9210   format(' Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9800

9800   continue
       call writmess(6,amessage)

9900   continue
       call mat_deallocate(ifail,mat)
       if(allocated(eigmat))deallocate(eigmat,stat=ierr)
       if(allocated(apar))deallocate(apar,stat=ierr)
       if(allocated(itrans))deallocate(itrans,stat=ierr)
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
