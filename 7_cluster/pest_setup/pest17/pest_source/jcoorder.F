       program jcoorder

C --   Program jcoorder re-arranges columns and rows in a Jacobian matrix.

       implicit none

       integer  ifail,ierr,nespar1,nxrow1,ies,irow,icount,i,j,ipar,
     + iobs,iline,npar,nobs,nespar3,nxrow3,ipest,nb,nnpar,nnobs,
     + nnprior,nnobsgp,nnpargp
       integer  lw(5),rw(5)
       double precision dtemp
       double precision, allocatable :: x1(:,:),x2(:,:),x3(:,:)
       character*1 aa
       character*4 aext
       character*10  how,aline
       character*12  aversion
       character*12  aapar,atrans
       character*20  aaobs
       character*200 jcofile1,jcofile2,orderfile,afile,bfile
       character*500 cline,amessage
       character*12, allocatable :: apar1(:),bapar(:)
       character*20, allocatable :: aobs1(:),baobs(:)

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' JCOORDER Version ',a,'. Watermark Numerical ',
     + 'Computing.')
#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) CLINE
#else
       call pgetcl(cline)
#endif

! -- The command line is parsed.

       if(cline.eq.' ') go to 9000
       call spacesub(cline)
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.eq.0) go to 9000
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.ne.0) go to 9000
       jcofile1=cline(lw(1):rw(1))
       orderfile=cline(lw(2):rw(2))
       jcofile2=cline(lw(3):rw(3))
       call remchar(jcofile1,char(211))
       call remchar(orderfile,char(211))
       call remchar(jcofile2,char(211))
#ifndef UNIX
       call lowcas(jcofile1)
       call lowcas(jcofile2)
       call lowcas(orderfile)
#endif

C -- It is ascertained whether the ordering file is a PEST control file
C    or not.

       ipest=0
       nb=len_trim(orderfile)
       i=nb-3
       if(i.lt.1)i=1
       aext=orderfile(i:nb)
       call lowcas(aext)
       if(aext.eq.'.pst')ipest=1

C -- Input files are opened.

       how='read'
       call open_unformatted_file(ifail,10,how,jcofile1,amessage)
       if(ifail.ne.0) go to 9890
       open(unit=11,file=orderfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(orderfile,afile)
         write(amessage,11) trim(afile)
11       format(' Cannot open parameter/observation order file ',a,'.')
         go to 9890
       end if

C -- Read the JCO file.

       call addquote(jcofile1,afile)
       read(10,err=9250,end=9250)nespar1,nxrow1
       if(nespar1.lt.0)then
         continue
       else
         write(amessage,20) trim(afile)
20       format(' Jacobian matrix file ',a,' was not produced by ',
     +   'version 8 or later of PEST; use program JCOTRANS to ',
     +   'convert this to new format.')
         go to 9890
       end if
       nespar1=abs(nespar1)
       nxrow1=abs(nxrow1)
       allocate(x1(nxrow1,nespar1),apar1(nespar1),aobs1(nxrow1),
     + stat=ierr)
       if(ierr.ne.0) go to 9100
       do ies=1,nespar1
         do irow=1,nxrow1
           x1(irow,ies)=0.0d0
         end do
       end do
       read(10,err=9250,end=9250)icount
       do i=1,icount
         read(10,err=9250,end=9250) j,dtemp
         ies=(j-1)/nxrow1+1
         irow=j-(ies-1)*nxrow1
         x1(irow,ies)=dtemp
       end do
       do i=1,nespar1
         read(10,err=9250,end=9250) apar1(i)
         call lowcas(apar1(i))
       end do
       do i=1,nxrow1
         read(10,err=9250,end=9250) aobs1(i)
         call lowcas(aobs1(i))
       end do
       close(unit=10)
       write(6,80) trim(afile)
80     format(' - file ',a,' read ok.')

C -- Read the ordering file.

       call addquote(orderfile,afile)
       allocate(bapar(nespar1),baobs(nxrow1),stat=ierr)
       if(ierr.ne.0) go to 9100

       if(ipest.eq.0)then
         iline=0
         ipar=0
110      iline=iline+1
         read(11,'(a)',err=9500,end=9550) cline
         if(cline.eq.' ') go to 110
         cline=adjustl(cline)
         if(cline(1:1).eq.'#') go to 110
         call lowcas(cline)
         if(cline(1:1).ne.'*') go to 9600
         if(index(cline,'parameter').eq.0) go to 9600
         do
           iline=iline+1
           read(11,'(a)',err=9500,end=9650) cline
           if(cline.eq.' ') cycle
           cline=adjustl(cline)
           if(cline(1:1).eq.'#') cycle
           if(cline(1:1).eq.'*') go to 150
           ipar=ipar+1
           if(ipar.gt.nespar1)then
             call addquote(jcofile1,bfile)
             write(amessage,120) trim(afile),trim(bfile)
120          format(' Parameter/observation ordering file ',a,
     +       ' must not cite more parameters than Jacobian matrix ',
     +       'file ',a,'.')
             go to 9890
           end if
           call linspl(ifail,1,lw,rw,cline)
           bapar(ipar)=cline(lw(1):rw(1))
         end do
150      continue
         npar=ipar
         iobs=0
         do
           iline=iline+1
           read(11,'(a)',err=9500,end=180) cline
           if(cline.eq.' ') cycle
           cline=adjustl(cline)
           if(cline(1:1).eq.'#') cycle
           if(cline(1:1).eq.'*') go to 9500
           call lowcas(cline)
           iobs=iobs+1
           if(iobs.gt.nxrow1)then
             call addquote(jcofile1,bfile)
             write(amessage,160) trim(afile),trim(bfile)
160          format(' Parameter/observation ordering file ',a,
     +       ' must not cite more observations than Jacobian matrix ',
     +       'file ',a,'.')
             go to 9890
           end if
           call linspl(ifail,1,lw,rw,cline)
           baobs(iobs)=cline(lw(1):rw(1))
         end do
180      continue
         nobs=iobs
         close(unit=11)
         write(6,80) trim(afile)
       else
         read(11,'(a)',err=9700,end=9750) cline
         cline=adjustl(cline)
         call lowcas(cline)
         if(cline(1:3).ne.'pcf')then
           write(amessage,181) trim(afile)
181        format(' First line of PEST control file ',a,
     +     ' should be "pcf".')
           go to 9890
         end if
         read(11,'(a)',err=9700,end=9750) cline
         read(11,'(a)',err=9700,end=9750) cline
         read(11,*,err=9700,end=9750) nnpar,nnobs,nnpargp,nnprior,
     +   nnobsgp
         if(nnprior.ne.0)then
           write(6,*)
           write(6,183)
183        format(' Prior info in PEST control file',
     +     ' will not be included in observation list.')
182        write(6,184,advance='no')
184        format(' Is this ok?  [y/n]: ')
           read(5,'(a)') aa
           if(aa.eq.' ') go to 182
           call lowcas(aa)
           if((aa.ne.'y').and.(aa.ne.'n')) go to 182
           if(aa.eq.'n')then
             write(amessage,185)
185          format(' Then you will have to create a parameter/',
     +       'observation ordering file.')
             go to 9890
           end if
         end if
         do
           read(11,'(a)',err=9700,end=9770) cline
           cline=adjustl(cline)
           if(cline(1:1).eq.'*')then
             call lowcas(cline)
             if(index(cline,'parameter dat').ne.0) go to 186
           end if
         end do
186      continue
         ipar=0
         do i=1,nnpar
           read(11,*,err=9700,end=9750) aapar,atrans
           call lowcas(aapar)
           call lowcas(atrans)
           if((atrans.eq.'fixed').or.(atrans.eq.'tied')) cycle
           ipar=ipar+1
           if(ipar.gt.nespar1)then
             call addquote(jcofile1,bfile)
             write(amessage,187) trim(afile),trim(bfile)
187          format(' PEST control file ',a,' possesses more ',
     +       'adjustable parameters than are cited in Jacobian ',
     +       'matrix file ',a,'.')
             go to 9890
           end if
           bapar(ipar)=aapar
         end do
         npar=ipar
         do
           read(11,'(a)',err=9700,end=9790) cline
           cline=adjustl(cline)
           if(cline(1:1).eq.'*')then
             call lowcas(cline)
             if(index(cline,'observation dat').ne.0) go to 201
           end if
         end do
201      continue
         iobs=0
         do i=1,nnobs
           read(11,*,err=9700,end=9750) aaobs
           call lowcas(aaobs)
           iobs=iobs+1
           if(iobs.gt.nxrow1)then
             call addquote(jcofile1,bfile)
             write(amessage,202) trim(afile),trim(bfile)
202          format(' PEST control file ',a,' possesses more ',
     +       'observations than are cited in Jacobian ',
     +       'matrix file ',a,'.')
             go to 9890
           end if
           baobs(iobs)=aaobs
         end do
         nobs=iobs
         close(unit=11)
         write(6,80) trim(afile)
       end if

C -- The Jacobian matrix is now sorted by column (i.e. parameter).

       allocate(x2(nxrow1,npar),stat=ierr)
       if(ierr.ne.0) go to 9100

       call addquote(orderfile,afile)
       call addquote(jcofile1,bfile)
       j=0
       do ipar=1,npar
         call whichone(ifail,nespar1,j,apar1,bapar(ipar))
         if(ifail.ne.0)then
           if(ipest.eq.0)then
           write(amessage,210) trim(bapar(ipar)),trim(afile),
     +     trim(bfile)
210        format(' Parameter "',a,'" cited in parameter/observation ',
     +     'ordering file ',a,' not found in Jacobian matrix file ',
     +     a,'.')
           else
           write(amessage,2101) trim(bapar(ipar)),trim(afile),
     +     trim(bfile)
2101       format(' Parameter "',a,'" cited in PEST control ',
     +     'file ',a,' not found in Jacobian matrix file ',
     +     a,'.')
           end if
           go to 9890
         end if
         do irow=1,nxrow1
           x2(irow,ipar)=x1(irow,j)
         end do
       end do

C -- The Jacobian matrix is now sorted by row (i.e. observation).

       allocate(x3(nobs,npar),stat=ierr)
       if(ierr.ne.0) go to 9100
       j=0
       do iobs=1,nobs
         call whichone(ifail,nxrow1,j,aobs1,baobs(iobs))
         if(ifail.ne.0)then
           if(ipest.eq.0)then
           write(amessage,211) trim(baobs(iobs)),trim(afile),
     +     trim(bfile)
211        format(' Observation "',a,'" cited in parameter/observation ',
     +     'ordering file ',a,' not found in Jacobian matrix file ',
     +     a,'.')
           else
           write(amessage,2111) trim(baobs(iobs)),trim(afile),
     +     trim(bfile)
2111       format(' Observation "',a,'" cited in PEST control ',
     +     'file ',a,' not found in Jacobian matrix file ',
     +     a,'.')
           end if
           go to 9890
         end if
         do ipar=1,npar
           x3(iobs,ipar)=x2(j,ipar)
         end do
       end do

C -- The new Jacobian matrix file is now written.

       how='write'
       call open_unformatted_file(ifail,15,how,jcofile2,amessage)
       if(ifail.ne.0) go to 9890

       nespar3=npar
       nxrow3=nobs
       call addquote(jcofile2,afile)
       write(15,err=9400) -nespar3,-nxrow3
       icount=0
       do i=1,nespar3
          do j=1,nxrow3
            if(x3(j,i).ne.0.0d0) icount=icount+1
          end do
       end do
       write(15,err=9400) icount
       do i=1,nespar3
         do j=1,nxrow3
           if(x3(j,i).ne.0.0d0)then
             icount=(i-1)*nxrow3+j
             write(15,err=9400)icount,x3(j,i)
           end if
         end do
       end do
       do ies=1,nespar3
         write(15,err=9400) bapar(ies)
       end do
       do irow=1,nxrow3
         write(15,err=9400) baobs(irow)
       end do
       close(unit=15)
       write(6,490) trim(afile)
490    format(' - file ',a,' written ok.')

       go to 9900

9000   continue
       write(6,9010)
9010   format(/,' JCOORDER is run using the command:',/)
       write(6,9020)
9020   format('     jcoorder jcofile1 orderfile jcofile2',/)
       write(6,9030)
9030   format(' where',/)
       write(6,9050)
9050   format('     jcofile1  is an existing Jacobian matrix file,')
       write(6,9051)
9051   format('     orderfile is a parameter/observation ordering ',
     + 'or PEST control file, and')
       write(6,9052)
9052   format('     jcofile2  is a new Jacobian matrix file.')
       go to 9999

9100   continue
       write(amessage,9110)
9110   format(' Insufficient memory to continue execution.')
       go to 9890
9250   continue
       write(amessage,9260) trim(afile)
9260   format(' Error reading Jacobian matrix file ',a,'.')
       go to 9890
9400   call addquote(jcofile2,afile)
       write(amessage,9410) trim(afile)
9410   format('Cannot write to file ',a,'.')
       go to 9890

9500   call writint(aline,iline)
       write(amessage,9510) trim(aline),trim(afile)
9510   format(' Error reading line ',a,' of parameter/observation ',
     + 'ordering file ',a,'.')
       go to 9890
9550   write(amessage,9560) trim(afile)
9560   format(' Unexpected end encountered to parameter/',
     + 'observation ordering file ',a,'.')
       go to 9890
9600   write(amessage,9610) trim(afile)
9610   format(' First data line of file ',a,' should be ',
     + '"* parameters".')
       go to 9890
9650   write(amessage,9660) trim(afile)
9660   format(' Could not find "* observations" section in ',
     + 'parameter/observation ordering file ',a,'.')
       go to 9890
9700   write(amessage,9710) trim(afile)
9710   format(' Error encountered in reading PEST control file ',
     + a,': check it with PESTCHEK.')
       go to 9890
9750   write(amessage,9760) trim(afile)
9760   format(' Unexpected end encountered to PEST control file ',
     + a,': check it with PESTCHEK.')
       go to 9890
9770   write(amessage,9780) trim(afile)
9780   format(' Cannot find "* parameter data" section in ',
     + 'PEST control file ',a,'.')
       go to 9890
9790   write(amessage,9795) trim(afile)
9795   format(' Cannot find "* observation data" section in ',
     + 'PEST control file ',a,'.')
       go to 9890

9890   call writmess(6,amessage)
9900   continue

       deallocate (x1,x2,x3,apar1,aobs1,bapar,baobs,stat=ierr)

9999   continue
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
#ifdef PESTMOD
        end subroutine writint
#else
        end
#endif

