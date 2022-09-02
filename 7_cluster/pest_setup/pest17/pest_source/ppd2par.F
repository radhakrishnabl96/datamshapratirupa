	program ppd2par

C -- Program PPD2PAR writes a set of parameters recorded in a Pareto parameter
C    data file in PEST parameter value file format.

        implicit none

        integer ifail,ierr
        integer nb,i,mpar,icount
        integer npar,nobsgp,nobs_report
        integer ipar,iobsgp,iobs
        integer lw(5),rw(5)
        character*4 aext
        character*12 acount
        character*12 aversion
        character*200 ppdfile,afile,outfile,bfile
        character*500 cline,amessage

        double precision, allocatable :: scale(:),offset(:),pval(:),
     +  refobs(:),phisub(:)
        character*12, allocatable     :: aobsgp(:),apar(:)
        character*20, allocatable     :: aobs(:)

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

        include 'version.inc'
        write(6,1) trim(aversion)
1       format(' PPD2PAR version ',a,'. ',
     +  'Watermark Numerical Computing.',/)


#ifdef NO_CMDLINE
        write(*,*)
        write(*,3) ' Enter command line arguments > '
        read('(a)',3) cline
3       format(a)
#else
	call pgetcl(cline)
#endif

C -- The command line is parsed.

        if(cline.eq.' ') go to 9900
        call spacesub(cline)
        call linspl(ifail,4,lw,lw,cline)
        if(ifail.eq.0) go to 9900
        call linspl(ifail,3,lw,rw,cline)
        if(ifail.ne.0) go to 9900
        ppdfile=cline(lw(1):rw(1))
        call remchar(ppdfile,char(211))
        outfile=cline(lw(2):rw(2))
        call remchar(outfile,char(211))
        call intread(ifail,cline(lw(3):rw(3)),mpar)
        if(ifail.ne.0) go to 9900
        if(mpar.lt.0) go to 9900
#ifndef UNIX
        call lowcas(ppdfile)
        call lowcas(outfile)
#endif

        nb=len_trim(ppdfile)
        i=nb-3
        if(i.lt.1)i=1
        aext=ppdfile(i:nb)
        call lowcas(aext)
        if(aext.ne.'.ppd')then
          write(amessage,15)
15        format(' Expected extension for Pareto parameter data ',
     +    'file is ".ppd".')
          go to 9800
        end if

C -- The Pareto parameter data file is read.

        call addquote(ppdfile,afile)
        write(6,30) trim(afile)
30      format(' - opening Pareto parameter data file ',a,'...')
        call open_unformatted_file(ifail,10,'read',ppdfile,amessage)
        if(ifail.ne.0) go to 9800
        read(10,err=9000,end=9100)npar
        allocate(scale(npar),offset(npar),pval(npar),
     +  apar(npar),stat=ierr)
        if(ierr.ne.0) go to 9200
        read(10,err=9000,end=9100) (scale(ipar),ipar=1,npar)
        read(10,err=9000,end=9100) (offset(ipar),ipar=1,npar)
        read(10,err=9000,end=9100) (apar(ipar),ipar=1,npar)
        read(10,err=9000,end=9100) nobsgp
        allocate(aobsgp(nobsgp),phisub(nobsgp),stat=ierr)
        if(ierr.ne.0) go to 9200
        read(10,err=9000,end=9100) (aobsgp(iobsgp),iobsgp=1,nobsgp)
        read(10,err=9000,end=9100) nobs_report
        if(nobs_report.ne.0)then
          allocate(aobs(nobs_report),refobs(nobs_report),stat=ierr)
          if(ierr.ne.0) go to 9200
          read(10,err=9000,end=9100) (aobs(iobs),iobs=1,nobs_report)
        end if

        do ipar=1,npar
          apar(ipar)=adjustl(apar(ipar))
        end do

C -- Data is transferred from one file to the other.

        call addquote(outfile,bfile)
        write(6,50) trim(bfile)
50      format(' - opening text output file ',a,'...')
        open(unit=20,file=outfile,iostat=ierr)
        if(ierr.ne.0) go to 9300
        write(20,60,err=9300)
60      format(' single point')

C -- Data is now transferred from one file to the other.

        icount=-1
        do
          icount=icount+1
          read(10,err=500,end=500) (phisub(iobsgp),iobsgp=1,nobsgp)
          if(nobs_report.ne.0)then
            read(10,err=500,end=500) (refobs(iobs),iobs=1,nobs_report)
          end if
          read(10,err=500,end=500) (pval(ipar),ipar=1,npar)
          if(icount.eq.mpar)then
            write(6,90)
90          format(' - transferring data...')
            do ipar=1,npar
              write(20,100) trim(apar(ipar)),pval(ipar),scale(ipar),
     +        offset(ipar)
100           format(1x,a,t20,1pg14.7,4x,1pg14.7,4x,1pg14.7)
            end do
            icount=-9999
            go to 500
          end if
        end do

500     continue
        close(unit=10,iostat=ierr)
        close(unit=20,iostat=ierr)
        if(icount.eq.-9999)then
          write(6,*)
          write(6,510) trim(afile)
510       format(' - file ',a,' read ok.')
          write(6,520) trim(bfile)
520       format(' - file ',a,' written ok.')
        else
          call writint(acount,icount-1)
          write(amessage,550) trim(bfile),trim(acount),trim(afile)
550       format(' Cannot write file ',a,' as only ',a,
     +    ' parameter sets were found in file ',a,'.')
          go to 9800
        end if

        go to 9990

9000    write(amessage,9010) trim(afile)
9010    format(' Error encountered in reading file ',a,'.')
        go to 9800
9100    write(amessage,9110) trim(afile)
9110    format(' Unexpected end encountered to file ',a,'.')
        go to 9800
9200    write(amessage,9210)
9210    format(' Cannot allocate sufficient memory to continue ',
     +  'execution.')
        go to 9800
9300    write(amessage,9310) trim(bfile)
9310    format(' Error writing data to file ',a,'.')
        go to 9800

9800    continue
        call writmess(6,amessage)
        go to 9990

9900    write(6,*)
        write(6,9910)
9910    format(' PPD2PAR is run using the command:',/)
        write(6,9920)
9920    format('    ppd2par ppdfile parfile N',/,/,
     +  ' where',/)
        write(6,9930)
9930    format('    ppdfile is a binary Pareto parameter ',
     +  'data ("ppd") file,')
        write(6,9940)
9940    format('    parfile is a parameter value ("par") file to be ',
     +  'written by PPD2PAR, and')
        write(6,9950)
9950    format('    N       is the iteration number for ',
     +  'extraction (zero or greater).')

        go to 9990

9990    continue

        deallocate (scale,offset,pval,refobs,phisub,stat=ierr)
        deallocate (aobsgp,apar,aobs,stat=ierr)

        end



        subroutine intread(ifail,cline,itemp)

C -- Subroutine intREAD reads a real number from a string.

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
