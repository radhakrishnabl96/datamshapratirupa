       program jcocomb

! --   Program jcocomb combines observations to form a new JCO file.

       implicit none

       logical equals

       integer idiff1,idiff2,idiff3,ifail,n,n1,ierr,i,j,k,icount,
     + itemp1,itemp2,iline,ibeg,iend,icurrent

       integer npar1,nobs1,npargp1,nprior1,nxrow1,iobs,nespar1,bn,ies,
     + irow,itrans2,npar2,nobs2,npargp2,nprior2,nxrow2,nespar2,ipar

       double precision dtemp,pval2,scale2,offset2

       character*10 aext,aline
       character*12 aversion
       character*12 atemp,atemp1,atemp2,aapar,apar2
       character*20 aaobs,obscomb,anum
       character*200 pestfile1,pestfile2,comfile,afile,bfile,
     + jcofile1,jcofile2
       character*300 cline
       character*500 amessage

       integer lw(5),rw(5)

       integer, allocatable :: itrans1(:)
       double precision, allocatable :: pval1(:),scale1(:),offset1(:),
     +                                  x1(:,:),x2(:,:)
       character*12, allocatable :: apar1(:)
       character*20, allocatable :: aobs1(:),aobs2(:)

! -- Initialisation

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(' JCOCOMB Version ',a,'. Watermark Numerical Computing.')
#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) CLINE
#else
       call pgetcl(cline)
#endif

       idiff1=0
       idiff2=0
       idiff3=0
       icurrent=0

! -- The command line is parsed.

       if(cline.eq.' ') go to 9000
       call spacesub(cline)
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.eq.0) go to 9000
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.ne.0) go to 9000
       pestfile1=cline(lw(1):rw(1))
       pestfile2=cline(lw(2):rw(2))
       comfile=cline(lw(3):rw(3))
       call remchar(pestfile1,char(211))
       call remchar(pestfile2,char(211))
       call remchar(comfile,char(211))
#ifndef UNIX
       call lowcas(pestfile1)
       call lowcas(pestfile2)
       call lowcas(comfile)
#endif

       n=len_trim(pestfile1)
       n1=n-3
       if(n1.lt.1)n1=1
       aext=pestfile1(n1:n)
       call lowcas(aext)
       if(aext.eq.'.pst')pestfile1(n1:n)=' '
       jcofile1=pestfile1
       n=len_trim(pestfile1)
       pestfile1(n+1:)='.pst'
       jcofile1(n+1:)='.jco'
       n=len_trim(pestfile2)
       n1=n-3
       if(n1.lt.1)n1=1
       aext=pestfile2(n1:n)
       call lowcas(aext)
       if(aext.eq.'.pst')pestfile2(n1:n)=' '
       jcofile2=pestfile2
       n=len_trim(pestfile2)
       pestfile2(n+1:)='.pst'
       jcofile2(n+1:)='.jco'

! -- Input files are opened.

       open(unit=10,file=pestfile1,status='old',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(pestfile1,afile)
         write(amessage,110) trim(afile)
110      format(' Cannot open PEST control file ',a,'.')
         go to 9890
       end if

       call open_unformatted_file(ifail,15,'read',jcofile1,amessage)
       if(ifail.ne.0)then
         call addquote(jcofile1,afile)
         write(amessage,120) trim(afile)
120      format(' Cannot open unformatted Jacobian matrix file ',a,'.')
         go to 9890
       end if

! -- First the first PEST control file is read.

       call addquote(pestfile1,afile)
       write(6,252) trim(afile)
       do i=1,3
         read(10,*,err=9100,end=9100)
       end do
       read(10,*,err=9100,end=9100) npar1,nobs1,npargp1,nprior1
       nxrow1=nobs1+nprior1
       allocate(apar1(npar1),pval1(npar1),itrans1(npar1),
     + aobs1(nxrow1),scale1(npar1),offset1(npar1),stat=ierr)
       if(ierr.ne.0)then
         write(amessage,125)
125      format(' Cannot allocate sufficient memory to continue ',
     +   'execution.')
         go to 9890
       end if
       do
         read(10,'(a)',err=9100,end=130) cline
         call lowcas(cline)
         if(index(cline,'* parameter data').ne.0) go to 150
       end do
130    continue
       write(amessage,140) trim(afile)
140    format(' Cannot find "parameter data" section of PEST ',
     + 'control file ',a,'.')
       go to 9890
150    do i=1,npar1
         read(10,*,err=9100,end=9100) apar1(i),atemp,atemp1,pval1(i),
     +   dtemp,dtemp,atemp1,scale1(i),offset1(i)
         call lowcas(apar1(i))
         call lowcas(atemp)
         if(atemp.eq.'log')then
           itrans1(i)=1
         else if(atemp.eq.'none')then
           itrans1(i)=0
         else if(atemp.eq.'fixed')then
           itrans1(i)=-1000000
         else if(atemp.eq.'tied')then
           itrans1(i)=-1
         else
           go to 9100
         end if
       end do
       icount=0
       do i=1,npar1
         if(itrans1(i).eq.-1)icount=icount+1
       end do
       if(icount.gt.0)then
         do i=1,icount
           read(10,*,err=9100,end=9100) atemp1,atemp2
           call lowcas(atemp1)
           call lowcas(atemp2)
           do j=1,npar1
             if(atemp1.eq.apar1(j)) go to 170
           end do
           go to 9100
170        continue
           do k=1,npar1
             if(atemp2.eq.apar1(k)) go to 180
           end do
           go to 9100
180        continue
           if(itrans1(j).ne.-1) go to 9100
           if(itrans1(k).lt.0) go to 9100
           itrans1(j)=-k
         end do
       end if

       do
         read(10,'(a)',err=9100,end=200) cline
         call lowcas(cline)
         if(index(cline,'* observation data').ne.0) go to 220
       end do
200    write(amessage,210) trim(afile)
210    format(' Cannot find "observation data" section of file ',a,'.')
       go to 9890
220    do i=1,nobs1
         read(10,*,err=9100,end=9100) aobs1(i)
         call lowcas(aobs1(i))
       end do

C -- The prior information section is now read.

       if(nprior1.ne.0)then
         do
           read(10,'(a)',err=9100,end=1200) cline
           call lowcas(cline)
           if(index(cline,'* prior in').ne.0) go to 1220
         end do
1200     write(amessage,1201) trim(afile)
1201     format(' Cannot find "prior information" section of file ',
     +   a,'.')
         go to 9890
1220     do iobs=nobs1+1,nxrow1
1222       read(10,'(a)',err=9100,end=9150) cline
           cline=adjustl(cline)
           if(cline(1:1).eq.'&') go to 1222
           call linspl(ifail,1,lw,rw,cline)
           aobs1(iobs)=cline(lw(1):rw(1))
           call lowcas(aobs1(iobs))
         end do
       end if
       close(unit=10)

       nespar1=0
       do i=1,npar1
         if(itrans1(i).ge.0)nespar1=nespar1+1
       end do
       if(nespar1.eq.0)then
         write(amessage,231) trim(afile)
231      format(' There are no adjustable parameters in file ',a,'.')
         go to 9890
       end if

       write(6,230) trim(afile)
230    format(' - file ',a,' read ok.')

! -- The Jacobian matrix file is read.

       call addquote(jcofile1,afile)
       write(6,232) trim(afile)
232    format(/,' - reading Jacobian matrix file ',a,'...')
       read(15,err=9250,end=9250)itemp1,itemp2
       if(abs(itemp1).ne.nespar1) go to 9200
       if(abs(itemp2).ne.nxrow1) go to 9200
       bn=abs(itemp2)
       if(itemp1.lt.0)then
         continue
       else
         write(amessage,240) trim(afile)
240      format(' Jacobian matrix file ',a,' was not produced by ',
     +   'version 8 or later of PEST; use program JCOTRANS to ',
     +   'convert this to new format.')
         go to 9890
       end if
       allocate(x1(nxrow1,nespar1),stat=ierr)
       if(ierr.ne.0)then
         write(amessage,125)
         go to 9890
       end if

       do i=1,nespar1
         do j=1,nxrow1
           x1(j,i)=0.0d0
         end do
       end do
       read(15,err=9250,end=9250)icount
       do i=1,icount
         read(15,err=9250,end=9250) j,dtemp
         ies=(j-1)/bn+1
         irow=j-(ies-1)*bn
         x1(irow,ies)=dtemp
       end do
       do i=1,npar1
         if(itrans1(i).lt.0) cycle
         read(15,err=9250,end=9250) aapar
         call lowcas(aapar)
         if(aapar.ne.apar1(i)) go to 9200
       end do
       do i=1,nobs1
         read(15,err=9250,end=9250) aaobs
         call lowcas(aaobs)
         if(aaobs.ne.aobs1(i)) go to 9200
       end do
       close(unit=15)
       write(6,230) trim(afile)

! -- The second PEST control file is read and its Jacobian file opened, ready
!    for writing.

       call open_unformatted_file(ifail,15,'write',jcofile2,amessage)
       if(ifail.ne.0)then
         call addquote(jcofile2,afile)
         write(amessage,250) trim(afile)
250      format(' Cannot open Jacobian matrix file ',a,' for output.')
         go to 9890
       end if

! -- Data is read from the second PEST control file.

       call addquote(pestfile2,afile)
       write(6,252) trim(afile)
252    format(/,' - reading PEST control file ',a,'...')
       open(unit=10,file=pestfile2,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,110) trim(afile)
         go to 9890
       end if

       do i=1,3
         read(10,*,err=9100,end=9100)
       end do
       read(10,*,err=9100,end=9100) npar2,nobs2,npargp2,nprior2
       if(nprior2.ne.0)then
         write(amessage,260) trim(afile)
260      format(' JCOCOMB requires that the second PEST control file ',
     +   a,' possess no prior information.')
         go to 9890
       end if
       if(npar2.ne.npar1) go to 9500
       allocate(aobs2(nobs2),stat=ierr)
       if (ierr.ne.0)then
         write(amessage,125)
         go to 9890
       end if
       do
         read(10,'(a)',err=9100,end=330) cline
         call lowcas(cline)
         if(index(cline,'* parameter data').ne.0) go to 350
       end do
330    continue
       write(amessage,140) trim(afile)
       go to 9890
350    do i=1,npar2
         read(10,*,err=9100,end=9100) apar2,atemp,atemp1,pval2,
     +   dtemp,dtemp,atemp1,scale2,offset2
         call lowcas(apar2)
         if(apar2.ne.apar1(i)) go to 9500
         call lowcas(atemp)
         if(atemp.eq.'log')then
           itrans2=1
         else if(atemp.eq.'none')then
           itrans2=0
         else if(atemp.eq.'fixed')then
           itrans2=-1000000
         else if(atemp.eq.'tied')then
           itrans2=-1
         else
           go to 9100
         end if
         if((itrans2.le.-1000000).or.(itrans2.ge.0))then
           if(itrans2.ne.itrans1(i)) go to 9500
         else
           if((itrans1(i).ge.0).or.(itrans1(i).le.-1000000)) go to 9500
         end if
         if(itrans2.ge.0)then
           if(.not.equals(pval2,pval1(i))) idiff1=1
           if(.not.equals(scale2,scale1(i))) idiff2=1
           if(.not.equals(offset2,offset1(i))) idiff3=1
         end if
       end do
       do
         read(10,'(a)',err=9100,end=400) cline
         call lowcas(cline)
         if(index(cline,'* observation data').ne.0) go to 420
       end do
400    continue
       write(amessage,210) trim(afile)
       go to 9890
420    do i=1,nobs2
         read(10,*,err=9100,end=9100) aobs2(i)
         call lowcas(aobs2(i))
       end do
       close(unit=10)
       write(6,230) trim(afile)

C -- Memory is allocated.

       nespar2=nespar1
       nxrow2=nobs2
       allocate(x2(nxrow2,nespar2),stat=ierr)
       if(ierr.ne.0) then
         write(amessage,125)
         go to 9890
       end if
       do irow=1,nxrow2
         x2(irow,1)=-1.1d201
       end do

C -- The observation combination file is read.

       write(6,*)
       call addquote(comfile,afile)
       write(6,1300) trim(afile)
1300   format(' - reading observation combination file ',a,'...')
       open(unit=10,file=comfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,1310) trim(afile)
1310     format(' Cannot open observation combination file ',a,'.')
         go to 9890
       end if

       iline=0
       iline=iline+1
       read(10,'(a)',end=1400) cline
       call lowcas(cline)
       cline=adjustl(cline)
       if(cline(1:23).ne.'* composite_observation')then
         write(amessage,1320) trim(afile)
1320     format(' First line of file ',a,' should be ',
     +   '"* composite_observation" ',
     +   'followed by an observation name.')
         go to 9890
       end if
1321   continue
       cline=cline(24:)
       cline=adjustl(cline)
       ibeg=1
       iend=len_trim(cline)
       call getfile(ifail,cline,obscomb,ibeg,iend)
       if(ifail.ne.0)then
         call writint(aline,iline)
         write(amessage,1330) trim(aline),trim(afile)
1330     format(' Cannot read observation name from line ',a,
     +   ' of file ',a,'.')
         go to 9890
       end if
       call lowcas(obscomb)
       do iobs=1,nobs2
         if(obscomb.eq.aobs2(iobs)) go to 1350
       end do
       call addquote(pestfile2,bfile)
       call writint(aline,iline)
       write(amessage,1345) trim(obscomb),trim(aline),trim(afile),
     + trim(bfile)
1345   format(' Observation "',a,'" featured on line ',a,' of file ',
     + a,' is not cited in PEST control file ',a,'.')
       go to 9890
1350   continue
       do ies=1,nespar2
         x2(iobs,ies)=0.0d0
       end do
       do
         iline=iline+1
         read(10,'(a)',end=1400) cline
         if(cline.eq.' ') cycle
         call lowcas(cline)
         cline=adjustl(cline)
         if(cline(1:1).eq.'*')then
           if(cline(1:23).eq.'* composite_observation') then
             go to 1321
           else
             call writint(aline,iline)
             write(amessage,1351) trim(aline),trim(afile)
1351         format(' Line ',a,' of file ',a,' should be ',
     +       '"composite_observation" followed by an observation ',
     +       'name.')
             go to 9890
           end if
         end if
         ibeg=1
         iend=len_trim(cline)
         call getfile(ifail,cline,aaobs,ibeg,iend)
         aaobs=adjustl(aaobs)
         call lowcas(aaobs)
         call whichone(ifail,nxrow1,icurrent,aobs1,aaobs)
         if(ifail.ne.0)then
           call addquote(pestfile1,bfile)
           call writint(aline,iline)
           write(amessage,1360) trim(aaobs),trim(aline),trim(afile),
     +     trim(bfile)
1360       format(' Observation "',a,'" named at line ',a,' of file ',
     +     a,' is not cited in PEST control file ',a,'.')
           go to 9890
         end if
         cline=cline(iend+1:)
         if(cline.eq.' ')then
           call writint(aline,iline)
           write(amessage,1370) trim(aline),trim(afile)
1370       format(' There is no number following observation at ',
     +     'line ',a,' of file ',a,'.')
           go to 9890
         end if
         cline=adjustl(cline)
         n=index(cline,' ')
         anum=cline(1:n-1)
         call drealrd(ifail,anum,dtemp)
         if(ifail.ne.0)then
           call writint(aline,iline)
           write(amessage,1380) trim(aline),trim(afile)
1380       format(' Cannot read number following observation name ',
     +     'at line ',a,' of file ',a,'.')
           go to 9890
         end if
         do ies=1,nespar2
           x2(iobs,ies)=x2(iobs,ies)+x1(icurrent,ies)*dtemp
         end do
       end do
1400   continue

1500   do irow=1,nxrow2
         if(x2(irow,1).lt.-1.0d200)then
           call addquote(pestfile2,bfile)
           write(amessage,1510) trim(aobs2(irow)),trim(bfile),
     +     trim(afile)
1510       format(' Observation "',a,'" cited in PEST control file ',
     +     a,' is not cited in observation combination file ',a,'.')
           go to 9890
         end if
       end do
       close(unit=10)
       write(6,1520) trim(afile)
1520   format(' - file ',a,' read ok.')

C -- The new JCO file is written.

       call addquote(jcofile2,afile)
       write(6,1530) trim(afile)
1530   format(/,' - writing Jacobian matrix file ',a,'...')
       write(15,err=9400) -nespar2,-nobs2
       icount=0
       do i=1,nespar2
          do j=1,nobs2
            if(x2(j,i).ne.0.0d0) icount=icount+1
          end do
       end do
       write(15,err=9400) icount
       do i=1,nespar2
         do j=1,nobs2
           if(x2(j,i).ne.0.0d0)then
             icount=(i-1)*nobs2+j
             write(15,err=9400)icount,x2(j,i)
           end if
         end do
       end do
       do ipar=1,npar2
         if((itrans1(ipar).eq.0).or.(itrans1(ipar).eq.1))then
            write(15,err=9400) apar1(ipar)
         end if
       end do
       do iobs=1,nobs2
         write(15,err=9400) aobs2(iobs)
       end do
       close(unit=15)
       write(6,490) trim(afile)
490    format(' - file ',a,' written ok.')
       if(idiff1.ne.0)then
         write(6,495)
495      format(/,' Warning: at least one adjustable parameter from ',
     +   'second PEST control file has ',/,
     +   ' different value from that of parameter of same name in ',
     +   'first PEST control file.')
       end if
       if((idiff2.ne.0).or.(idiff3.ne.0))then
         write(6,496)
496      format(/,' Warning: at least one adjustable parameter from ',
     +   'second PEST control file has ',/,
     +   ' different scale or offset from that of parameter of same ',
     +   'name in first PEST ',/,' control file.')
       end if

       go to 9900

9000   continue
       write(6,9010)
9010   format(/,' JCOCOMB is run using the command:',/)
       write(6,9020)
9020   format('     jcocomb casename1 casename2 combfile',/)
       write(6,9030)
9030   format(' where',/)
       write(6,9050)
9050   format('     casename1 pertains to a case for which a ',
     + 'JCO file exists,')
       write(6,9060)
9060   format('     casename2 pertains to a case for which a ',
     + 'JCO file must be written, and')
       write(6,9070)
9070   format('     combfile  is the name of an observation ',
     + 'combination file.')
       go to 9999


9100   write(amessage,9110) trim(afile)
9110   format(' Error reading PEST control file ',a,'; check this ',
     + 'file with PESTCHEK.')
       go to 9890
9150   write(amessage,9160) trim(afile)
9160   format(' End of PEST control file ',a,' found before all ',
     + 'prior information read.')
       go to 9890

9200   continue
       call addquote(jcofile1,afile)
       call addquote(pestfile1,bfile)
       write(amessage,9210) trim(afile),trim(bfile)
9210   format(' Jacobian matrix file ',a,' is incompatible with PEST ',
     + 'control file ',a,'.')
       go to 9890
9250   write(amessage,9260) trim(afile)
9260   format(' Error reading Jacobian matrix file ',a,'.')
       go to 9890
9400   call addquote(jcofile2,afile)
       write(amessage,9410) trim(afile)
9410   format(' Cannot write to file ',a,'.')
       go to 9890
9500   call addquote(pestfile1,bfile)
       write(amessage,9510) trim(afile),trim(bfile)
9510   format(' JCOCOMB requires that the second PEST control ',
     + 'file ',a,' possess the same parameters in the same order ',
     + 'with the same transformation and scale/offset status as ',
     + 'first PEST control file ',a,'.')
       go to 9890

9890   call writmess(6,amessage)

9900   continue
       deallocate (itrans1,pval1,scale1,offset1,x1,x2,apar1,aobs1,
     + aobs2,stat=ierr)
9999   continue
       end


       logical function equals(r1,r2)

         double precision,  intent(in)      :: r1,r2
         double precision                   :: rtemp

         rtemp=abs(3.0*spacing(r1))
         if(abs(r1-r2).lt.rtemp)then
           equals=.true.
         else
           equals=.false.
         end if

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

C -- Subroutine lowcas converts a string to lower case.

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

        character*(*) afile,aqfile
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


        subroutine getfile(ifail,cline,filename,ibeg,iend)

C-- Subroutine getfile extracts a filename from a string.

C -- Arguments are as follows:-
C       ifail: returned as zero if filename successfully read
C       cline: a character string containing the file name
C       filename: the name of the file read from the string
C       ibeg: character position at which to begin search for filename
C       iend: on input  - character position at which to end search for filename
C             on output - character postion at which filename ends


        integer, intent(out)               :: ifail
        integer, intent(in)                :: ibeg
        integer, intent(inout)             :: iend
        character (len=*), intent(in)      :: cline
        character (len=*), intent(out)     :: filename

        integer                            :: i,j,k
        character (len=1)                  :: aa

        ifail=0
        do i=ibeg,iend
          aa=cline(i:i)
          if((aa.ne.' ').and.(aa.ne.',').and.(aa.ne.char(9)))go to 50
        end do
        ifail=1
        return

50      if((aa.eq.'"').or.(aa.eq.''''))then
          do j=i+1,iend
            if(cline(j:j).eq.aa) go to 60
          end do
          ifail=1
          return
60        iend=j
          if(i+1.gt.j-1)then
            ifail=1
            return
          else
            filename=cline(i+1:j-1)
          end if
        else
          do j=i+1,iend
            if((cline(j:j).eq.' ').or.(cline(j:j).eq.',').or.
     +         (cline(j:j).eq.char(9)))then
              k=j-1
              go to 100
            end if
          end do
          k=iend
100       filename=cline(i:k)
          if(cline(k:k).eq.'"')then
            ifail=1
            return
          else if(cline(k:k).eq.'''')then
            ifail=1
            return
          end if

          iend=k
        end if
        filename=adjustl(filename)
        return

        end


