       program pest2vec

C -- Program PEST2VEC writes PEST adjustable parameters to a template of a
C    matrix file

       implicit none

       logical          :: lopened
       integer          :: ifail,ierr,ies,nespar,ipar,i
       integer          :: npar,nobs,npargp,nprior,nobsgp,
     +                     ntpfle
       integer          :: lw(8),rw(8)
       double precision :: ppval
       character*12     :: aapar,atrans,arelfac
       character*12     :: aversion
       character*200    :: pestfile,tplfile,matfile,logfile,
     +                     apestfile,atplfile,amatfile,alogfile,
     +                     pestfile2,apestfile2
       character*500    :: cline
       character*600    :: amessage

       integer, allocatable          :: itrans(:)
       double precision, allocatable :: pval(:)
       character*12, allocatable     :: apar(:)


#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif       
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' PEST2VEC version ',a,'. Watermark Numerical ',
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
       call linspl(ifail,5,lw,rw,cline)
       if(ifail.ne.0) go to 8900
       pestfile=cline(lw(1):rw(1))
       call remchar(pestfile,char(211))
       pestfile2=cline(lw(2):rw(2))
       call remchar(pestfile2,char(211))
       tplfile=cline(lw(3):rw(3))
       call remchar(tplfile,char(211))
       matfile=cline(lw(4):rw(4))
       call remchar(matfile,char(211))
       logfile=cline(lw(5):rw(5))
       call remchar(logfile,char(211))
#ifndef UNIX
       call lowcas(pestfile)
       call lowcas(pestfile2)
       call lowcas(tplfile)
       call lowcas(logfile)
       call lowcas(matfile)
#endif
       call addquote(pestfile,apestfile)
       call addquote(pestfile2,apestfile2)
       call addquote(tplfile,atplfile)
       call addquote(logfile,alogfile)
       call addquote(matfile,amatfile)

C -- The PEST control file is read.

       open(unit=10,file=pestfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,60) trim(apestfile)
60       format(' Cannot open PEST control file ',a,'.')
         go to 9800
       end if
       open(unit=20,file=pestfile2,err=9600)
       read(10,'(a)',err=9300,end=9300) cline
       cline=adjustl(cline)
       call lowcas(cline)
       if(cline(1:3).ne.'pcf') go to 9300
       write(20,'(a)',err=9600) trim(cline)
       do i=1,2
         read(10,'(a)',err=9300,end=9300) cline
         write(20,'(a)',err=9600) trim(cline)
       end do
       read(10,*,err=9300,end=9300) npar,nobs,npargp,nprior,nobsgp
       allocate(apar(npar),pval(npar),itrans(npar),stat=ierr)
       if(ierr.ne.0) go to 9200
       write(20,61,err=9600) npar,nobs,npargp,nprior,nobsgp
61     format(5i6)
       read(10,'(a)',err=9300,end=9300) cline
       call linspl(ifail,1,lw,rw,cline)
       if(ifail.ne.0) go to 9300
       call intread(ifail,cline(lw(1):rw(1)),ntpfle)
       if(ifail.ne.0) go to 9300
       ntpfle=ntpfle+1
       write(20,62,err=9600) ntpfle,trim(cline(rw(1)+1:))
62     format(i4,' ',a)
       do
         read(10,'(a)',end=9300,err=9300) cline
         write(20,'(a)',err=9600) trim(cline)
         call lowcas(cline)
         if(index(cline,'* parameter data').ne.0) go to 80
       end do
80     continue
       nespar=0
       do ipar=1,npar
         read(10,'(a)',err=9300,end=9300) cline
         write(20,'(a)',err=9600) trim(cline)
         call linspl(ifail,4,lw,rw,cline)
         if(ifail.ne.0) go to 9300
         aapar=cline(lw(1):rw(1))
         atrans=cline(lw(2):rw(2))
         arelfac=cline(lw(3):rw(3))
         call drealread(ifail,cline(lw(4):rw(4)),ppval)
         if(ifail.ne.0) go to 9300
         call lowcas(atrans)
         if((atrans.eq.'none').or.(atrans.eq.'log'))then
           nespar=nespar+1
           apar(nespar)=aapar
           call lowcas(apar(nespar))
           pval(nespar)=ppval
           itrans(nespar)=0
           if(atrans.eq.'log')itrans(nespar)=1
         end if
       end do
       do
         read(10,'(a)',err=9300,end=9300) cline
         write(20,'(a)',err=9600) trim(cline)
         call lowcas(cline)
         if(index(cline,'* model input/out').ne.0) go to 380
       end do
380    continue
       write(20,381) trim(atplfile),trim(amatfile)
381    format(1x,a,2x,a)
       do
         read(10,'(a)',err=9300,end=390) cline
         write(20,'(a)',err=9600) trim(cline)
       end do
390    continue
       close(unit=10)
       write(6,81) trim(apestfile)
81     format(' - file ',a,' read ok.')
       close(unit=20)
       write(6,82) trim(apestfile2)
82     format(' - file ',a,' written ok.')

C -- The template file is now written.

       open(unit=20,file=tplfile,err=9000)
       write(20,85,err=9000)
85     format('ptf $')
       write(20,90,err=9000) nespar,1,2
90     format(3i6)
       do ies=1,nespar
         write(20,120,err=9000) trim(apar(ies))
120      format(' $',a,t15,'$')
       end do
       write(20,130,err=9000)
130    format('* row names')
       do ies=1,nespar
         write(20,140) trim(apar(ies))
140      format(1x,a)
       end do
       write(20,142,err=9000)
142    format('* column names')
       write(20,150,err=9000)
150    format(' col1')
       close(unit=20)
       write(6,180) trim(atplfile)
180    format(' - file ',a,' written ok.')


C -- The vector transformation file is now written.

       open(unit=20,file=logfile,err=9500)
       write(20,90,err=9500) nespar,1,2
       do ies=1,nespar
         write(20,122,err=9500) itrans(ies)
122      format(1x,i2)
       end do
       write(20,130,err=9500)
       do ies=1,nespar
         write(20,140) trim(apar(ies))
       end do
       write(20,142,err=9500)
       write(20,150,err=9500)
       close(unit=20)
       write(6,180) trim(alogfile)

C -- If necessary the matrix file is now written.

       if(matfile.ne.' ')then
         open(unit=20,file=matfile,err=9100)
         write(20,90,err=9100) nespar,1,2
         do ies=1,nespar
           write(20,125,err=9100) pval(ies)
125        format(1x,1pg14.7)
         end do
         write(20,130,err=9100)
         do ies=1,nespar
           write(20,140) trim(apar(ies))
         end do
         write(20,142,err=9100)
         write(20,150,err=9100)
         close(unit=20)
         write(6,180) trim(amatfile)
       end if

       go to 9900

8900   continue
       write(6,8910)
8910   format(/,' PEST2VEC is run using the command:',/)
       write(6,8920)
8920   format('     PEST2VEC pestfile1 pestfile2 tplfile matfile ',
     + 'logfile',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8940)
8940   format('     pestfile1 is an existing PEST control file,')
       write(6,8941)
8941   format('     pestfile2 is a new PEST control file,')
       write(6,8945)
8945   format('     tplfile   is a template file of a matrix file,')
       write(6,8950)
8950   format('     matfile   is a matrix file citing adjustable ',
     + 'parameter values, and')
       write(6,8946)
8946   format('     logfile   is the name of a transformation vector',
     + ' file.')

       go to 9999

9000   write(amessage,9010) trim(atplfile)
9010   format(' Cannot write to template file ',a,'.')
       go to 9800
9100   write(amessage,9110) trim(amatfile)
9110   format(' Cannot write to matrix file ',a,'.')
       go to 9800
9500   write(amessage,9510) trim(alogfile)
9510   format(' Cannot write to transformation vector file ',a,'.')
       go to 9800

9200   write(amessage,9210)
9210   format(' Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9800
9300   write(amessage,9310) trim(apestfile)
9310   format(' Error reading PEST control file ',a,'; check ',
     + 'this file with PESTCHEK.')
       go to 9800
9600   write(amessage,9610) trim(apestfile2)
9610   format(' Cannot write to PEST control file ',a,'.')
       go to 9800



9800   continue
       call writmess(6,amessage)

9900   continue
       deallocate(apar,pval,itrans,stat=ierr)
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

