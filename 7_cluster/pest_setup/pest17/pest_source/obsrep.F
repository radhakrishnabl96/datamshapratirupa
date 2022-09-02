       program obsrep

C -- Program OBSREP replaces observations and prior-information
C    observed values with their best-fit equivalents as read from a RES file.

       implicit none

       integer, parameter  :: MAXRESOBS=100000

       logical          :: lopened
       integer          :: ifail,iline,ierr,iobs,nresobs,npar,npargp,
     +                     nobs,nprior,nobsgp,currentobs,iobswait,
     +                     iequal,ne
       integer          :: lw(5),rw(5)
       double precision :: dtemp
       double precision :: modobs(MAXRESOBS)
       character*200    :: resfile,pestfile1,pestfile2,aresfile,
     +                     apestfile1,apestfile2
       character*12     :: aversion
       character*12     :: aline
       character*20     :: atemp,aobs
       character*20     :: aresobs(MAXRESOBS)
       character*400    :: cline,dline
       character*500    :: amessage


       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' OBSREP version ',a,'. Watermark Numerical ',
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
       resfile=cline(lw(1):rw(1))
       call remchar(resfile,char(211))
       pestfile1=cline(lw(2):rw(2))
       call remchar(pestfile1,char(211))
       pestfile2=cline(lw(3):rw(3))
       call remchar(pestfile2,char(211))
#ifndef UNIX
       call lowcas(resfile)
       call lowcas(pestfile1)
       call lowcas(pestfile2)
#endif

C -- The residuals file is first read.

       call addquote(resfile,aresfile)
       open(unit=10,file=resfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,210) trim(aresfile)
210      format(' Cannot open residuals file ',a,'.')
         go to 9800
       end if

       iline=1
       read(10,15,err=9400,end=9400) cline
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.ne.0)then
         write(amessage,220) trim(aresfile)
220      format(' Insufficient entries on first line of residuals ',
     +   'file ',a,'.')
         go to 9800
       end if
       call lowcas(cline)
       if(index(cline,'name').eq.0) go to 9300
       if(index(cline,'modelled').eq.0) go to 9300
       if(index(cline,'measured').eq.0) go to 9300
       if(index(cline,'name').eq.0) go to 9300

       iobs=0
       do
         iline=iline+1
         read(10,15,err=9400,end=300) cline
         if(cline.eq.' ') cycle
         iobs=iobs+1
         if(iobs.gt.MAXRESOBS)then
           call writint(aline,MAXRESOBS)
           write(amessage,230) trim(aresfile),trim(aline)
230        format(' Too many lines of data in residuals file ',a,
     +     '; at present only ',a,' lines allowed. Increase ',
     +     'MAXRESOBS and re-compile program.')
           go to 9800
         end if
         call linspl(ifail,4,lw,rw,cline)
         if(ifail.ne.0) go to 9450
         aresobs(iobs)=cline(lw(1):rw(1))
         call lowcas(aresobs(iobs))
         call drealrd(ifail,cline(lw(4):rw(4)),modobs(iobs))
         if(ifail.ne.0) go to 9400
       end do
300    continue
       close(unit=10)
       nresobs=iobs
       if(nresobs.eq.0)then
         write(amessage,320) trim(aresfile)
320      format(' No observations listed in residuals file ',a,'.')
         go to 9800
       end if
       write(6,*)
       write(6,330) trim(aresfile)
330    format(' - residuals file ',a,' read ok.')


C -- The PEST control file is read and a new one written.

       call addquote(pestfile1,apestfile1)
       open(unit=10,file=pestfile1,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,14) trim(apestfile1)
14       format(' Cannot open PEST control file ',a,'.')
         go to 9800
       end if

       call addquote(pestfile2,apestfile2)
       open(unit=20,file=pestfile2,iostat=ierr)
       if(ierr.ne.0) go to 9200


       iline=1
       read(10,15,err=9000,end=9050) cline
15     format(a)
       cline=adjustl(cline)
       call lowcas(cline)
       if(cline(1:3).ne.'pcf')then
         write(amessage,20) trim(apestfile1)
20       format(' First line of PEST control file ',a,' should be ',
     +   '"pcf".')
         go to 9800
       end if
       write(20,15,err=9200) trim(cline)

       iline=2
       read(10,15,err=9000,end=9050) cline
       cline=adjustl(cline)
       call lowcas(cline)
       if(index(cline,'* control d').eq.0)then
         write(amessage,30) trim(apestfile1)
30       format(' Second line of PEST control file ',a,' expected ',
     +   'to be "* control data".')
         go to 9800
       end if
       write(20,15,err=9200) trim(cline)

       iline=3
       read(10,15,err=9000,end=9050) cline
       write(20,15,err=9200) trim(cline)

       iline=4
       read(10,15,err=9000,end=9050) cline
       call linspl(ifail,5,lw,rw,cline)
       if(ifail.ne.0) then
         write(amessage,40) trim(apestfile1)
40       format(' Five entries expected on line 4 of PEST control ',
     +   'file ',a,'.')
         go to 9800
       end if
       call intread(ifail,cline(lw(1):rw(1)),npar)
       if(ifail.ne.0) go to 9000
       call intread(ifail,cline(lw(2):rw(2)),nobs)
       if(ifail.ne.0) go to 9000
       call intread(ifail,cline(lw(3):rw(3)),npargp)
       if(ifail.ne.0) go to 9000
       call intread(ifail,cline(lw(4):rw(4)),nprior)
       if(ifail.ne.0) go to 9000
       call intread(ifail,cline(lw(5):rw(5)),nobsgp)
       if(ifail.ne.0) go to 9000
       write(20,15,err=9200) trim(cline)

C -- We now seek the "observation data" section of this file.

       do
         iline=iline+1
         read(10,'(a)',err=9000,end=60) cline
         write(20,'(a)',err=9200) trim(cline)
         call lowcas(cline)
         if(index(cline,'* observation da').ne.0) go to 80
         cycle
60       write(amessage,70) trim(apestfile1)
70       format(' Unexpected end encountered to PEST control ',
     +   'file ',a,' while looking for "observation data" section.')
         go to 9800
       end do

C -- The "observation data" section is read and replacements made.

80     continue
       currentobs=0
       do iobs=1,nobs
         iline=iline+1
         read(10,15,err=9100,end=9000) cline
         call linspl(ifail,4,lw,rw,cline)
         if(ifail.ne.0) go to 9150
         aobs=cline(lw(1):rw(1))
         call lowcas(aobs)
         call whichone(ifail,nresobs,currentobs,aresobs,aobs)
         if(ifail.ne.0)then
           call writint(aline,iline)
           write(amessage,85) trim(aobs),trim(aline),trim(apestfile1),
     +     trim(aresfile)
85         format(' Observation "',a,'" listed on line ',a,
     +     ' of PEST control file ',a,' is not listed in residuals ',
     +     'file ',a,'.')
           go to 9800
         end if
         write(20,100) trim(aobs),modobs(currentobs),
     +   trim(cline(lw(3):))
100      format(1x,a,t22,1pg14.7,t38,a)
       end do

C -- If there is no prior information we write the rest of the file and
C    finish up.

       if(nprior.eq.0)then
         do
           iline=iline+1
           read(10,15,err=9100,end=150) cline
           write(20,15,err=9200) trim(cline)
         end do
150      continue
         go to 500
       end if

C -- We look for the "prior information" section.

       do
         iline=iline+1
         read(10,'(a)',err=9100,end=160) cline
         write(20,'(a)',err=9200) trim(cline)
         call lowcas(cline)
         if(index(cline,'* prior inf').ne.0) go to 180
         cycle
160      write(amessage,170) trim(apestfile1)
170      format(' Unexpected end encountered to PEST control ',
     +   'file ',a,' while looking for "prior information" section.')
         go to 9800
       end do

       iobswait=0
       iequal=0
180    continue
       iline=iline+1
       read(10,15,err=9100,end=500) cline
       if(cline.eq.' ') go to 180
       cline=adjustl(cline)
       if(cline(1:1).eq.'*')then
         dline=cline
         call lowcas(dline)
         if(index(dline,'predictive').ne.0) go to 400
         if(index(dline,'regula').ne.0) go to 400
         if(index(dline,'pareto').ne.0) go to 400
         go to 9600
       end if
       if(cline(1:1).ne.'&')then
         if(iobswait.eq.1)then
           call writint(aline,iline)
           write(amessage,185) trim(aline),trim(apestfile1)
185        format(' Continuation character expected as first item ',
     +     'in prior information at line ',a,' of PEST control ',
     +     'file ',a,'.')
           go to 9800
         end if
         iobswait=1
         call linspl(ifail,1,lw,rw,cline)
         aobs=cline(lw(1):rw(1))
         call lowcas(aobs)
         call whichone(ifail,nresobs,currentobs,aresobs,aobs)
         if(ifail.ne.0)then
           write(amessage,190) trim(aobs),trim(apestfile1),
     +     trim(aresfile)
190        format(' Prior information item "',a,'" listed in PEST ',
     +     'control file ',a,' is not listed in residuals file ',a,'.')
           go to 9800
         end if
       end if
       if(iobswait.eq.0)then
         write(20,15,err=9200) trim(cline)
         go to 180
       end if
       if(iequal.eq.1)then
         call linspl(ifail,2,lw,rw,cline)
         if(ifail.ne.0) go to 9600
         atemp=cline(lw(2):rw(2))
         call drealrd(ifail,atemp,dtemp)
         if(ifail.ne.0) go to 9600
         write(20,214,err=9200) modobs(currentobs),' ',
     +   trim(cline(rw(2)+1:))
214      format('&',1x,1pg14.7,a,a)
         iequal=0
         iobswait=0
         go to 180
       end if
       ne=index(cline,'=')
       if(ne.eq.0)then
         write(20,15,err=9200) trim(cline)
         go to 180
       end if
       if(cline(ne+1:).eq.' ')then
         write(20,15,err=9200) trim(cline)
         iequal=1
         go to 180
       end if
       dline=cline(ne+1:)
       call linspl(ifail,1,lw,rw,dline)
       atemp=dline(lw(1):rw(1))
       call drealrd(ifail,atemp,dtemp)
       if(ifail.ne.0) go to 9600
       write(20,280) cline(1:ne),' ',modobs(currentobs),' ',
     + trim(dline(rw(1)+1:))
280    format(a,a,1pg14.7,a,a)
       iobswait=0
       go to 180

400    continue
       write(20,15,err=9200) trim(cline)
       do
         iline=iline+1
         read(10,15,err=9100,end=500) cline
         write(20,15,err=9200) trim(cline)
       end do

C -- We have reached the end of the PEST control file. Wind up.

500    continue
       if(iobswait.ne.0) go to 9500
       close(unit=10)
       close(unit=20)
       write(6,510) trim(apestfile1)
510    format(' - file ',a,' read ok.')
       write(6,520) trim(apestfile2)
520    format(' - file ',a,' written ok.')

       go to 9900

8900   continue
       write(6,8910)
8910   format(/,' OBSREP is run using the command:',/)
       write(6,8920)
8920   format('     obsrep resfile pestfile1 pestfile2',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8940)
8940   format('     resfile   is the name of a residuals file,')
       write(6,8950)
8950   format('     pestfile1 is the name of the input PEST control ',
     + 'file, and')
       write(6,8960)
8960   format('     pestfile2 is the name of the output PEST control ',
     + 'file.')


       go to 9999

9000   write(amessage,9010) trim(apestfile1)
9010   format(' Error reading PEST control file ',a,
     + '; check it with PESTCHEK.')
       go to 9800
9050   write(amessage,9060) trim(apestfile1)
9060   format(' Unexpected end encountered to PEST control ',
     + 'file ',a,'.')
       go to 9800
9100   call writint(aline,iline)
       write(amessage,9110) trim(aline),trim(apestfile1)
9110   format(' Error reading line ',a,' of PEST control file ',
     + a,'.')
       go to 9800
9150   call writint(aline,iline)
       write(amessage,9160) trim(aline)
9160   format(' Insufficient entries on line ',a,' of PEST control ',
     + 'file ',a,'.')
       go to 9800


9200   write(amessage,9210) trim(apestfile2)
9210   format(' Cannot write to new PEST control file ',a,'.')
       go to 9800

9300   write(amessage,9310) trim(aresfile)
9310   format(' Header line to residuals file ',a,' does not appear ',
     + 'to be correct. Are you sure it is a residuals file?')
       go to 9800

9400   call writint(aline,iline)
       write(amessage,9410) trim(aline),trim(aresfile)
9410   format(' Error encountered while reading line ',a,
     + ' of residuals file ',a,'.')
       go to 9800

9450   call writint(aline,iline)
       write(amessage,9460) trim(aline),trim(aresfile)
9460   format(' Insufficient entries on line ',a,' of residuals ',
     + 'file ',a,'.')
       go to 9800
9500   write(amessage,9510) trim(apestfile1)
9510   format(' Unexpected end encountered to PEST control file ',a,
     + ' while reading "prior information" section.')
       go to 9800
9600   write(amessage,9610) trim(apestfile1)
9610   format(' Error encountered while reading "prior information" ',
     + 'section of PEST control file ',a,'; check this file with ',
     + 'PESTCHEK.')
       go to 9800

9800   continue
       call writmess(6,amessage)

9900   continue
       inquire(unit=10,opened=lopened)
       if(lopened) close(unit=10)
       inquire(unit=20,opened=lopened)
       if(lopened)then
         close(unit=20,status='delete')
       end if


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

        SUBROUTINE DREALRD(IFAIL,CLINE,RTEMP)

C -- Subroutine DREALRD reads a real number from a string.

        INTEGER IFAIL
        DOUBLE PRECISION RTEMP
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


	subroutine writint(atemp,ival)

c	Subroutine WRITINT writes an integer to a character variable.

	integer*4 ival
	character*6 afmt
	character*(*) atemp

	afmt='(i   )'
	write(afmt(3:5),'(i3)') len(atemp)
	write(atemp,afmt)ival
	atemp=adjustl(atemp)
	return
	end



        subroutine whichone(ifail,npar,ipar,apar,tpar)

C -- Subroutine whichone locates a string in an array. Note that both the
C    search string and the existing array of strings are assumed to be
C    in the same case.

        integer npar,ipar,i
        integer ifail
        character*(*) tpar
        character*(*) apar(npar)

        ifail=0
        if((ipar.lt.1).or.(ipar.gt.npar)) ipar=1
        if(tpar.eq.apar(ipar)) return
        if(ipar.ne.npar)then
          do 20 i=ipar+1,npar
          if(tpar.eq.apar(i))then
            ipar=i
            return
          end if
20        continue
        end if
        if(ipar.ne.1)then
          do 40 i=ipar-1,1,-1
          if(tpar.eq.apar(i)) then
            ipar=i
            return
          end if
40        continue
        end if
        ifail=1
        return
        end


