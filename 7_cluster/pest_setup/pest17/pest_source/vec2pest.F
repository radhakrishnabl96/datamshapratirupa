       include 'matman.F'


       program vec2pest

C -- Program VEC2PEST generates instructions to read a vector and makes appropriate
C    alterations to a PEST control file.

       use matrix_manipulate
       implicit none

       logical          :: lopened
       integer          :: ifail,irow,nrow,ierr,itemp,nb,i,ii
       integer          :: npar,nobs,npargp,nprior,nobsgp,nobs1,
     +                     nobsgp1,ntpfle,ninsfle,ninsfle1
       integer          :: lw(6),rw(6)
       character*12     :: aobsgp
       character*12     :: aversion
       character*20     :: aobs,anum
       character*200    :: vecfile,pestfile1,pestfile2,insfile,covfile
       character*200    :: avecfile,apestfile1,apestfile2,ainsfile,
     +                     acovfile
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
10     format(/,' VEC2PEST version ',a,'. Watermark Numerical ',
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
       call linspl(ifail,6,lw,lw,cline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.ne.0) go to 8900
       vecfile=cline(lw(1):rw(1))
       call remchar(vecfile,char(211))
       pestfile1=cline(lw(2):rw(2))
       call remchar(pestfile1,char(211))
       pestfile2=cline(lw(3):rw(3))
       call remchar(pestfile2,char(211))
       insfile=cline(lw(4):rw(4))
       call remchar(insfile,char(211))
       covfile=' '
       call linspl(ifail,5,lw,rw,cline)
       if(ifail.eq.0)then
         covfile=cline(lw(5):rw(5))
         call remchar(covfile,char(211))
       end if


#ifndef UNIX
       call lowcas(vecfile)
       call lowcas(pestfile1)
       call lowcas(pestfile2)
       call lowcas(insfile)
       call lowcas(covfile)
#endif
       call addquote(vecfile,avecfile)
       call addquote(pestfile1,apestfile1)
       call addquote(pestfile2,apestfile2)
       call addquote(insfile,ainsfile)
       if(covfile.ne.' ') call addquote(covfile,acovfile)

C -- The vector is read.

       call mat_read(ifail,10,mat,vecfile,amessage,cline)
       if(ifail.ne.0) go to 9800
       write(6,100) trim(avecfile)
100    format(' - file ',a,' read ok.')

       if(mat%ncol.ne.1)then
         write(amessage,110) trim(avecfile)
110      format(' The matrix contained in file ',a,' must contain a ',
     +   'single column.')
         go to 9800
       end if
       nrow=mat%nrow

C -- First the instruction file is written.

       open(unit=20,file=insfile,err=9000)
       write(20,120,err=9000)
120    format('pif $')
       write(20,130,err=9000) trim(mat%arow(1))
130    format('l2   [',a,']1:20')
       if(nrow.gt.1)then
         do irow=2,nrow
           write(20,140,err=9000) trim(mat%arow(irow))
140        format('l1   [',a,']1:20')
         end do
       end if
       close(unit=20,err=9000)
       write(6,150) trim(ainsfile)
150    format(' - file ',a,' written ok.')

C -- An attempt is made to find a name for the new observation group

       aobsgp='mat_data'
       aobs=mat%arow(1)
       nb=len_trim(aobs)
       if(nb.ge.2)then
         do ii=nb-1,1,-1
           anum=aobs(1:ii)
           do irow=1,nrow
             if(index(mat%arow(irow),trim(anum)).eq.0) go to 153
           end do
           aobsgp=anum
           go to 155
153        continue
         end do
       end if
155    continue

! -- Now the new PEST control file is written.

       open(unit=10,file=pestfile1,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,160) trim(apestfile1)
160      format(' Cannot open PEST control file ',a,'.')
         go to 9800
       end if
       open(unit=21,file=pestfile2,iostat=ierr)
       if(ierr.ne.0) go to 9400

       read(10,'(a)',err=9300,end=9300) cline
       cline=adjustl(cline)
       call lowcas(cline)
       if(cline(1:3).ne.'pcf') go to 9300
       write(21,'(a)',err=9400) trim(cline)
       do i=1,2
         read(10,'(a)',err=9300,end=9300) cline
         write(21,'(a)',err=9400) trim(cline)
       end do
       read(10,*,err=9300,end=9300) npar,nobs,npargp,nprior,nobsgp
       nobs1=nobs+nrow
       nobsgp1=nobsgp+1
       write(21,180,err=9400) npar,nobs1,npargp,nprior,nobsgp1
180    format(5i6)
       read(10,'(a)',err=9300,end=9300) cline
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.ne.0) go to 9300
       call intread(ifail,cline(lw(1):rw(1)),ntpfle)
       if(ifail.ne.0) go to 9300
       call intread(ifail,cline(lw(2):rw(2)),ninsfle)
       if(ifail.ne.0) go to 9300
       ninsfle1=ninsfle+1
       write(21,190) ntpfle,ninsfle1,trim(cline(rw(2)+1:))
190    format(2i6,3x,a)
       do
         read(10,'(a)',end=9300,err=9300) cline
         if(index(cline,'* observation data').ne.0) then
           if(covfile.eq.' ')then
             write(21,'(a)',err=9400) trim(aobsgp)
           else
             write(21,191,err=9400) trim(aobsgp),trim(acovfile)
191          format(a,2x,a)
           end if
           write(21,'(a)',err=9400) trim(cline)
           go to 200
         end if
         write(21,'(a)',err=9400) trim(cline)
       end do
200    continue
       do
         read(10,'(a)',end=9300,err=9300) cline
         if(index(cline,'* model comm').ne.0) then
           do irow=1,nrow
             write(21,210,err=9400) trim(mat%arow(irow)),trim(aobsgp)
210          format(1x,a,t24,'0.0    1.0',3x,a)
           end do
           write(21,'(a)',err=9400) trim(cline)
           go to 250
         end if
         write(21,'(a)',err=9400) trim(cline)
       end do
250    continue
       do i=1,ntpfle+ninsfle
         read(10,'(a)',end=9300,err=9300) cline
         write(21,'(a)',err=9400) trim(cline)
       end do
       write(21,270,err=9400) trim(ainsfile),trim(avecfile)
270    format(1x,a,3x,a)
       do
         read(10,'(a)',end=1100,err=9300) cline
         write(21,'(a)',err=9400) trim(cline)
       end do
1100   continue
       close(unit=10)
       close(unit=21)
       write(6,100) trim(apestfile1)
       write(6,150) trim(apestfile2)
       if(covfile.eq.' ')then
         write(amessage,1120) trim(pestfile2)
1120     format(' Note: you may need to edit file ',a,
     +   ', supplying values ',
     +   'and weights (and/or a covariance matrix) for the new ',
     +   'observations.')
       else
         write(amessage,1121) trim(pestfile2)
1121     format(' Note: you may need to edit file ',a,
     +   ' to supply values for the new observations.')
       end if
       call writmess(6,amessage)
       write(amessage,1122) trim(pestfile2)
1122   format(' Note: check file ',a,' with PESTCHEK before use.')
       call writmess(6,amessage)

       go to 9900

8900   continue
       write(6,8910)
8910   format(/,' VEC2PEST is run using the command:',/)
       write(6,8920)
8920   format('     vec2pest vecfile pestfile1 pestfile2 insfile2 ',
     + '[covfile]'/)
       write(6,8930)
8930   format(' where',/)
       write(6,8940)
8940   format('     vecfile   contains a column matrix,')
       write(6,8950)
8950   format('     pestfile1 is an existing PEST control file,')
       write(6,8955)
8955   format('     pestfile2 is a new PEST control file with added ',
     + 'observations,')
       write(6,8960)
8960   format('     insfile2  is the corresponding instruction ',
     + 'file, and')
       write(6,8965)
8965   format('     covfile   is an optional covariance matrix for ',
     + 'the new observation group.')

       go to 9999

9000   write(amessage,9010) trim(ainsfile)
9010   format(' Cannot write to instruction file ',a,'.')
       go to 9800
9300   write(amessage,9310) trim(apestfile1)
9310   format(' Error reading PEST control file ',a,'; check ',
     + 'this file with PESTCHEK.')
       go to 9800
9400   write(amessage,9410) trim(apestfile2)
9410   format(' Cannot write to new PEST control file ',a,'.')
       go to 9800


9800   continue
       call writmess(6,amessage)

9900   continue
       call mat_deallocate(ifail,mat)
       inquire(unit=10,opened=lopened)
       if(lopened) close(unit=10,iostat=ierr)
       inquire(unit=20,opened=lopened)
       if(lopened) close(unit=20,iostat=ierr)
       inquire(unit=21,opened=lopened)
       if(lopened) close(unit=21,iostat=ierr)


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

