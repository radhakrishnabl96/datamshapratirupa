       program paramid

C -- Program PARAMID identifies adjustable parameter contributions to highest eigenvectors.

       implicit none

       integer                   :: ifail,n,n1,ierr,npar,ipar,nespar
       integer                   :: nsing,nb,numsing,ising,jsing,ies,jes
       integer                   :: iopt,ksing
       integer                   :: lw(5),rw(5)
       integer, allocatable      :: itrans(:)
       real                      :: lambda,hcontrib,rtemp,lcontrib
       real, allocatable         :: v(:,:),singval(:)
       character*4               :: aext
       character*12              :: atrans
       character*12              :: aversion
       character*30              :: atemp
       character*12, allocatable :: apar(:)
       character*200             :: infile,outfile,aoutfile
       character*200             :: pestfile,svdfile,apestfile,asvdfile
       character*500             :: cline
       character*500             :: amessage

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' PARAMID version ',a,'. Watermark Numerical ',
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
       infile=cline(lw(1):rw(1))
       call remchar(infile,char(211))
       outfile=cline(lw(2):rw(2))
       call remchar(outfile,char(211))
#ifndef UNIX
       call lowcas(infile)
       call lowcas(outfile)
#endif
       n=len_trim(infile)
       n1=n-3
       if(n1.lt.1)n1=1
       aext=infile(n1:n)
       call lowcas(aext)
       if(aext.eq.'.pst')infile(n1:n)=' '
       svdfile=infile
       pestfile=infile
       n=len_trim(infile)
       pestfile(n+1:)='.pst'
       svdfile(n+1:)='.svd'

C -- First the PEST control file is read to obtain names of adjustable parameters.

       call addquote(pestfile,apestfile)
       write(6,90) trim(apestfile)
90     format(/,' - reading PEST control file ',a,'...')
       open(unit=10,file=pestfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,100) trim(apestfile)
100      format(' Cannot open PEST control file ',a,'.')
         go to 9890
       end if
       read(10,'(a)',err=9000,end=9000) cline
       cline=adjustl(cline)
       call lowcas(cline)
       if(cline(1:4).ne.'pcf ') go to 9000
       read(10,'(a)',err=9000,end=9000) cline
       read(10,'(a)',err=9000,end=9000) cline
       read(10,*,err=9000,end=9000) npar
       allocate(apar(npar),itrans(npar),stat=ierr)
       if(ierr.ne.0) go to 9200
       do
         read(10,'(a)',err=9000,end=130) cline
         cline=adjustl(cline)
         call lowcas(cline)
         if(index(cline,'* parameter da').ne.0) go to 140
       end do
130    write(amessage,135) trim(apestfile)
135    format(' Cannot find "* parameter data" section in file ',a,'.')
       go to 9890
140    do ipar=1,npar
         read(10,*,err=9000,end=9000) apar(ipar),atrans
         call lowcas(apar(ipar))
         call lowcas(atrans)
         if(atrans.eq.'log')then
           itrans(ipar)=1
         else if(atrans.eq.'none')then
           itrans(ipar)=0
         else if(atrans.eq.'tied')then
           itrans(ipar)=-1
         else if(atrans.eq.'fixed')then
           itrans(ipar)=-1000000
         else
           go to 9000
         end if
       end do
       close(unit=10)
       write(6,180) trim(apestfile)
180    format(' - file ',a,' read ok.')

C -- The number of adjustable parameters is worked out.

       nespar=0
       do ipar=1,npar
         if(itrans(ipar).ge.0) nespar=nespar+1
       end do
       if(nespar.eq.0)then
         write(amessage,190) trim(apestfile)
190      format(' No adjustable parameters are cited in file ',a,'.')
         go to 9890
       end if
       allocate(v(nespar,nespar),singval(nespar),stat=ierr)
       if(ierr.ne.0) go to 9200

C -- The SVD file is now read.

       nsing=0
       call addquote(svdfile,asvdfile)
       write(6,200) trim(asvdfile)
200    format(/,' - reading file ',a,'...')
       open(unit=10,file=svdfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,210) trim(asvdfile)
210      format(' Cannot open SVD file ',a,'.')
         go to 9890
       end if

       read(10,'(a)',err=9100,end=9150) cline
       if(index(cline,'SVD').eq.0) go to 9300
315    continue
       do
         read(10,'(a)',err=9100,end=500) cline
         if(index(cline,'OPTIMISATION').ne.0) go to 241
       end do
241    nb=index(cline,':')
       if(nb.eq.0) go to 9300
       atemp=cline(nb+1:)
       call intread(ifail,atemp,iopt)
       if(ifail.ne.0)then
         write(amessage,242) trim(asvdfile)
242      format(' Cannot read optimisation iteration number from ',
     +   'file ',a,'.')
         go to 9890
       end if
       do
         read(10,'(a)',err=9100,end=9300) cline
         if(index(cline,'MARQUARDT').ne.0) go to 250
       end do
250    nb=index(cline,'=')
       if(nb.eq.0) go to 9300
       atemp=cline(nb+1:)
       nb=index(atemp,'--')
       if(nb.gt.1) atemp=atemp(1:nb-1)
       call realread(ifail,atemp,lambda)
       if(ifail.ne.0)then
         write(amessage,260) trim(asvdfile)
260      format(' Cannot read Marquardt lambda from file ',a,'.')
         go to 9890
       end if
       if(lambda.ne.0.0)then
         if(lambda.lt.1.0e-7) then
           lambda=0.0
         else
           write(amessage,310) trim(asvdfile)
310        format(' File ',a,' cites a non-zero Marquardt lambda. ',
     +     'The Marquardt lambda must be set to zero for parameter ',
     +     'identifiability analysis to occur.')
           go to 9890
         end if
       end if
       do
         read(10,'(a)',err=9000,end=500) cline
         if(index(cline,'SINGULAR').ne.0) go to 320
       end do
320    read(10,*,iostat=ierr,end=9150) (singval(ies),ies=1,nespar)
       if(ierr.ne.0)then
         write(amessage,330) trim(asvdfile)
330      format(' Error reading singular values from file ',a,'.')
         go to 9890
       end if
       nsing=nsing+1
       do
         read(10,'(a)',err=9000,end=350) cline
         if(index(cline,'EIGENVEC').ne.0) go to 370
       end do
350    write(amessage,360) trim(asvdfile)
360    format(' Cannot find eigenvector matrix in file ',a,': ',
     + 'was EIGWRITE set to 1 in PEST control file? ')
       go to 9890
370    do ies=1,nespar
         read(10,*,err=380,end=380) (v(ies,jes),jes=1,nespar)
       end do
       go to 390
380    write(amessage,385) trim(asvdfile)
385    format(' Error reading eigenvector matrix from file ',a,'.')
       go to 9890
390    continue
       do
         read(10,'(a)',err=9100,end=9150) cline
         if(index(cline,'Number of').ne.0) go to 420
       end do
420    nb=index(cline,'=')
       if(nb.eq.0) go to 9100
       atemp=cline(nb+1:)
       call intread(ifail,atemp,numsing)
       if(ifail.ne.0) go to 9100
       go to 315

500    if(nsing.eq.0) go to 9300
       close(unit=10)
       write(6,510) trim(asvdfile)
510    format(' - file ',a,' read ok.')

C -- The output file is now written.

       call addquote(outfile,aoutfile)
       write(6,530) trim(aoutfile)
530    format(/,' - writing file ',a,'...')
       open(unit=20,file=outfile)
       write(20,550)
550    format(/,t20,' PARAMETER SOLUTION SPACE EIGENVECTOR ',
     + 'CONTRIBUTIONS')
       call writint(atemp,iopt)
       write(20,552) trim(atemp)
552    format(/,' Eigenvector matrix from optimisation iteration ',
     + a,' used in analysis.')
       call writint(atemp,numsing)
       write(20,555) trim(atemp)
555    format(/,' Dimensionality of solution space = ',a)
       write(20,560)
560    format(/,' Adjustable_parameter',t25,'Highest_contribution',
     + t50,'Eigenvector',t65,'Lowest_contribution',
     + t90,'Eigenvector')
       ies=0
       do ipar=1,npar
         if(itrans(ipar).lt.0) cycle
         ies=ies+1
         hcontrib=0.0
         lcontrib=1.0e25
         do ising=1,numsing
           rtemp=abs(v(ies,ising))
           if(rtemp.gt.hcontrib)then
             hcontrib=rtemp
             jsing=ising
           end if
           if(rtemp.lt.lcontrib)then
             lcontrib=rtemp
             ksing=ising
           end if
         end do
         write(20,580) trim(apar(ipar)),hcontrib,jsing,lcontrib,ksing
580      format(t3,a,t25,1pg13.6,t50,i5,t65,1pg13.6,t90,i5)
       end do
       close(unit=20)
       write(6,590) trim(aoutfile)
590    format(' - file ',a,' written ok.')


       go to 9900

8900   continue
       write(6,8910)
8910   format(/,' PARAMID is run using the command:',/)
       write(6,8920)
8920   format('     PARAMID pestfile outfile',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8940)
8940   format('     pestfile is the name of a PEST control file, and')
       write(6,8950)
8950   format('     outfile  contains a table of parameter ',
     + 'identifiability information.')
       go to 9900


9000   write(amessage,9010) trim(apestfile)
9010   format(' Error reading PEST control file ',a,': check this ',
     + 'file using PESTCHEK.')
       go to 9890

9100   write(amessage,9110) trim(asvdfile)
9110   format(' Error encountered when reading SVD file ',a,'.')
       go to 9890

9150   write(amessage,9160) trim(asvdfile)
9160   format(' Premature end encountered to SVD file ',a,'.')
       go to 9890

9200   write(amessage,9210)
9210   format(' Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9890

9300   write(amessage,9310) trim(asvdfile)
9310   format(' File ',a,' does not appear to be a valid PEST ',
     +   'SVD file.')
       go to 9890


9890   continue
       call writmess(6,amessage)

9900   continue

       deallocate(v,singval,apar,itrans,stat=ierr)

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


        subroutine realread(ifail,cline,rtemp)

C -- Subroutine REALREAD reads a real number from a string.

        integer ifail
        real rtemp
        character*8 afmt
        character*(*) cline

        ifail=0
        afmt='(f   .0)'
        write(afmt(3:5),'(i3)') len_trim(cline)
        read(cline,afmt,err=100) rtemp
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

