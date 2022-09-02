       program GENLIN

C -- GENLIN is a general linear model - that is, a single matrix multiplication.

       implicit none
       integer              :: npar,nobs,nespar,nespar1,nxrow1,ipar,
     +                         iobs,ierr,icount,i,j,ies,irow,nb,icheck,
     +                         ifail,ijco,iunit,nrow,ncol,icode,icol,
     +                         ideriv,nn,iderivcalc
       integer              :: lw(5),rw(5)
       integer, allocatable :: itrans(:)
       double precision     :: dtemp,dfactor
       double precision, allocatable :: initpval(:),pval(:),
     +                                  initoval(:),x(:,:),gval(:),
     +                                  y(:,:)
       character*4               :: aext
       character*6               :: aarow,atrans,acol,arow
       character*10              :: aversion
       character*12              :: apar1
       character*20              :: aobs1
       character*200             :: infile,outfile,jcofile,ainfile,
     +                              aoutfile,ajcofile,derivfile,
     +                              aderivfile
       character*300             :: cline
       character*500             :: amessage
       character*12, allocatable :: apar(:)
       character*20, allocatable :: aobs(:)


#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif       
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' GENLIN version ',a,'. Watermark Numerical ',
     + 'Computing.')
#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) CLINE
#else
       call pgetcl(cline)
#endif

C -- Initialisation

       iderivcalc=0

C -- The command line is parsed.

       if(cline.eq.' ') go to 8900
       call spacesub(cline)
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
       call addquote(infile,ainfile)
       call addquote(outfile,aoutfile)

C -- Is checking functionality active?

      cline=cline(rw(2)+1:)
      cline=adjustl(cline)
      icheck=0
      nn=index(cline,'/c')
      if(nn.ne.0)then
        icheck=1
        cline(nn:nn+1)=' '
      end if


C -- Does a derivatives file need to be written?

      ideriv=0
      if(cline.ne.' ')then
        call linspl(ifail,1,lw,rw,cline)
        derivfile=cline(lw(1):rw(1))
        call remchar(derivfile,char(211))
#ifndef UNIX
        call lowcas(derivfile)
#endif
        call addquote(derivfile,aderivfile)
        cline(lw(1):rw(1))=' '
        ideriv=1
        dfactor=1.0d0/log(10.0d0)
      end if
      if(cline.ne.' ') go to 8900

C -- Input and output files are opened.

       open(unit=10,file=infile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,20) trim(ainfile)
20       format(' Cannot open GENLIN input file ',a,'.')
         go to 9800
       end if
       open(unit=20,file=outfile,action='write',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,30) trim(aoutfile)
30       format(' Cannot open file ',a,' for output.')
         go to 9800
       end if

C -- The GENLIN input file is read.

C -- First the "dimensions" section.

       read(10,'(a)',err=9000,end=9300) cline
       call lowcas(cline)
       cline=adjustl(cline)
       if((cline(1:1).ne.'*').or.
     +    (index(cline,'dimensions').eq.0))then
          write(amessage,40) trim(ainfile)
40        format(' First line of GENLIN input file ',a,' should ',
     +    'be "* dimensions".')
          go to 9800
       end if
       read(10,*,iostat=ierr) npar,nobs
       if(ierr.ne.0)then
         write(amessage,50) trim(ainfile)
50       format(' Error reading problem dimensions from second line ',
     +   'of GENLIN input file ',a,'.')
         go to 9800
       end if
       allocate(apar(npar),initpval(npar),pval(npar),itrans(npar),
     + stat=ierr)
       if(ierr.ne.0) go to 9400
       allocate(aobs(nobs),initoval(nobs),stat=ierr)
       if(ierr.ne.0) go to 9400

C -- Next the "parameters" section.

       read(10,'(a)',iostat=ierr) cline
       if(ierr.ne.0)then
         write(amessage,60) trim(ainfile)
         go to 9800
       end if
       cline=adjustl(cline)
       call lowcas(cline)
       if((cline(1:1).ne.'*').or.
     +    (index(cline,'parameter').eq.0))then
          write(amessage,60) trim(ainfile)
60        format(' Third line of GENLIN input file ',a,' should ',
     +    'be "* parameters".')
          go to 9800
       end if
       do ipar=1,npar
         read(10,*,iostat=ierr)
     +   apar(ipar),atrans,pval(ipar),initpval(ipar)
         if(ierr.ne.0)then
           write(amessage,70) trim(ainfile)
70         format(' Error reading data from "parameters" section ',
     +     'of GENLIN input file ',a,'.')
           go to 9800
         end if
         call lowcas(apar(ipar))
         call lowcas(atrans)
         atrans=adjustl(atrans)
         if(atrans.eq.'log')then
           itrans(ipar)=1
         else if(atrans.eq.'none')then
           itrans(ipar)=0
         else
           write(amessage,75) trim(ainfile)
75         format(' Transformation status for any parameter must be ',
     +     '"log" or "none" in "parameters" section of GENLIN ',
     +     'input file ',a,'.')
           go to 9800
         end if
         if(itrans(ipar).eq.1)then
           if((pval(ipar).le.0.0d0).or.(initpval(ipar).lt.0.0d0))then
             write(amessage,76) trim(apar(ipar)),trim(ainfile)
76           format(' Parameter "',a,'" is log transformed in file ',
     +       a,': values supplied for this parameter must therefore ',
     +       'be positive.')
             go to 9800
           end if
         end if
       end do

C -- Next the "observations" section

       read(10,'(a)',iostat=ierr) cline
       if(ierr.ne.0)then
         write(amessage,80) trim(ainfile)
         go to 9800
       end if
       call lowcas(cline)
       cline=adjustl(cline)
       if((cline(1:1).ne.'*').or.
     +    (index(cline,'observation').eq.0))then
          write(amessage,80) trim(ainfile)
80        format(' "Observations" section should follow "parameters" ',
     +    'section of GENLIN input file ',a,'.')
          go to 9800
       end if
       do iobs=1,nobs
         read(10,*,iostat=ierr) aobs(iobs),initoval(iobs)
         if(ierr.ne.0)then
           write(amessage,90) trim(ainfile)
90         format(' Error reading data from "observations" section ',
     +     'of GENLIN input file ',a,'.')
           go to 9800
         end if
         call lowcas(aobs(iobs))
       end do

C -- Next the "sensitivities" section

       read(10,'(a)',iostat=ierr) cline
       if(ierr.ne.0)then
         write(amessage,100) trim(ainfile)
         go to 9800
       end if
       call lowcas(cline)
       cline=adjustl(cline)
       if((cline(1:1).ne.'*').or.
     +    (index(cline,'sensitivit').eq.0))then
          write(amessage,100) trim(ainfile)
100       format(' "Sensitivities" section should follow ',
     +    '"observations" section of GENLIN input file ',a,'.')
          go to 9800
       end if
       read(10,'(a)',iostat=ierr) cline
       if(ierr.ne.0)then
         write(amessage,110) trim(ainfile)
         go to 9800
       end if
       call spacesub(cline)
       call linspl(ifail,1,lw,rw,cline)
       if(ifail.ne.0)then
         write(amessage,110) trim(ainfile)
110      format(' Sensitivity file not provided in GENLIN input ',
     +   'file ',a,'.')
         go to 9800
       end if
       jcofile=cline(lw(1):rw(1))
       call remchar(jcofile,char(211))
#ifndef UNIX
       call lowcas(jcofile)
#endif
       nb=len_trim(jcofile)
       nb=nb-3
       if(nb.lt.1)nb=1
       aext=jcofile(nb:)
       call lowcas(aext)
       if(aext.eq.'.jco')then
         ijco=1
       else
         ijco=0
       end if
       call addquote(jcofile,ajcofile)

C -- The file is closed.

       write(6,*)
       close(unit=10)
       write(6,180) trim(ainfile)
180    format(' - file ',a,' read ok.')


C -- More memory is allocated

       allocate(x(nobs,npar),gval(npar),stat=ierr)
       if(ierr.ne.0) go to 9400
       x=0.0d0
       if(ideriv.ne.0)then
         allocate(y(nobs,npar),stat=ierr)
         if(ierr.ne.0) go to 9400
       end if

C -- If required, the Jacobian matrix file is opened and read.

       nespar=npar

       if(ijco.eq.1)then
         call open_unformatted_file(ifail,15,'read',jcofile,amessage)
         if(ifail.ne.0) go to 9800
         read(15,err=9100,end=9100) nespar1,nxrow1
         if(nespar1.gt.0)then
           write(amessage,205) trim(ajcofile)
205        format(' JCO file ',a,' uses old PEST JCO file format. Use ',
     +     'JCOTRANS utility to convert to new format.')
           go to 9800
         end if
         nespar1=-nespar1
         nxrow1=-nxrow1
         if(nespar.ne.nespar1) go to 9200
         if(nxrow1.lt.nobs) go to 9200
         read(15,err=9100,end=9100)icount
         do i=1,icount
           read(15,err=9100,end=9100) j,dtemp
           ies=(j-1)/nxrow1+1
           irow=j-(ies-1)*nxrow1
           if(irow.le.nobs) x(irow,ies)=dtemp
         end do
         if(icheck.eq.1)then
           do ipar=1,nespar1
             read(15,err=9100,end=9100) apar1
             call lowcas(apar1)
             if(apar(ipar).ne.apar1) then
               write(amessage,206) trim(ajcofile),trim(ainfile)
206            format(' Parameter names in Jacobian matrix file ',a,
     +         ' do not match those in GENLIN input file ',a,'.')
               go to 9800
             end if
           end do
           do iobs=1,nobs
             read(15,err=9100,end=9100) aobs1
             call lowcas(aobs1)
             if(aobs(iobs).ne.aobs1) then
               write(amessage,207) trim(ajcofile),trim(ainfile)
207            format(' Observation names in Jacobian matrix file ',a,
     +         ' do not match those in GENLIN input file ',a,'.')
               go to 9800
             end if
           end do
         end if
         close(unit=15)
         write(6,180) trim(ajcofile)
       else

C -- Otherwise a standard matrix file is read

         iunit=15
         open(unit=iunit,file=jcofile,status='old',iostat=ierr)
         if(ierr.ne.0)then
           write(amessage,510) trim(ajcofile)
510        format(' Cannot open matrix file ',a,'.')
           go to 9800
         end if
         read(iunit,'(a)',err=9500,end=9500) cline
         call linspl(ifail,3,lw,rw,cline)
         if(ifail.ne.0)then
           write(amessage,540) trim(ajcofile)
540        format(' Three integers are expected on first line of ',
     +     'matrix file ',a,'.')
           go to 9800
         end if
         call intread(ifail,cline(lw(1):rw(1)),nrow)
         if(ifail.ne.0) go to 9500
         call intread(ifail,cline(lw(2):rw(2)),ncol)
         if(ifail.ne.0) go to 9500
         call intread(ifail,cline(lw(3):rw(3)),icode)
         if(ifail.ne.0) go to 9500
         if((nrow.ne.nobs).or.(ncol.ne.npar))then
           write(amessage,545) trim(ajcofile),trim(ainfile)
545        format(' Dimensions of matrix in matrix file ',a,
     +     ' do not match those of linear model specified in GENLIN ',
     +     'input file ',a,'.')
           go to 9800
         end if
         if((icheck.eq.1).and.(icode.ne.2))then
           write(amessage,546) trim(ajcofile)
546        format(' ICODE on first line of matrix file ',a,' must ',
     +     'be set to "2" if name-checking functionality is ',
     +     'activated.')
           go to 9800
         end if
         if(abs(icode).eq.1)then
           if(ncol.ne.nrow)then
             write(amessage,560) trim(ajcofile)
560           format(' ICODE cannot be "1" or "-1" on first line of ',
     +       'file ',a,' unless matrix is square.')
             go to 9800
           end if
         end if
         if((icode.ne.-1).and.(icode.ne.1).and.(icode.ne.2))then
           write(amessage,570) trim(ajcofile)
570        format(' ICODE must be "1", "2" or "-1" on first line of ',
     +     'matrix file ',a,'.')
           go to 9800
         end if
         if(icode.ne.-1)then
           do irow=1,nrow
             read(iunit,*,err=9600,end=9700)(x(irow,icol),icol=1,ncol)
           end do
         else
           do irow=1,nrow
             read(iunit,*,err=9600,end=9700) x(irow,irow)
           end do
         end if
         if(icheck.eq.1)then
           read(iunit,'(a)',err=9720,end=9720) cline
           call lowcas(cline)
           if(index(cline,'* row names').eq.0)then
             write(amessage,630) trim(ajcofile)
630          format(' "* row names" header expected immediately ',
     +       'folowing matrix in file ',a,'.')
             go to 9800
           end if
           do irow=1,nrow
631          read(iunit,*,err=9720,end=9720) aobs1
             aobs1=adjustl(aobs1)
             call lowcas(aobs1)
             if(aobs1.ne.aobs(irow)) then
               write(amessage,632) trim(ajcofile),trim(ainfile)
632            format(' Row names in matrix file ',a,' do not match ',
     +         'observation names in GENLIN input file ',a,'.')
               go to 9800
             end if
           end do
           read(iunit,'(a)',err=9740,end=9740) cline
           call lowcas(cline)
           if(index(cline,'* column names').eq.0) go to 9740
           do icol=1,ncol
             read(iunit,*,err=9720,end=9720) apar1
             apar1=adjustl(apar1)
             call lowcas(apar1)
             if(apar1.ne.apar(icol)) then
               write(amessage,633) trim(ajcofile),trim(ainfile)
633            format(' Column names in matrix file ',a,' do not match ',
     +         'parameter names in GENLIN input file ',a,'.')
               go to 9800
             end if
           end do
         end if
         close(unit=iunit)
         write(6,180) trim(ajcofile)
       end if

C -- If only derivatives are required, these are now written.

       if(ideriv.ne.0)then
         open(unit=22,file=derivfile,action='write',iostat=ierr)
         if(ierr.ne.0)then
           write(amessage,636) trim(aderivfile)
636        format(' Cannot open file ',a,' for output.')
           go to 9800
         end if
         write(22,637) npar,nobs
637      format(2i6)
         if(iderivcalc.eq.0)then
           y=x                 ! arrays
           do ipar=1,npar
             if(itrans(ipar).eq.1)then
               dtemp=dfactor/pval(ipar)
               do iobs=1,nobs
                 y(iobs,ipar)=y(iobs,ipar)*dtemp
               end do
             end if
           end do
           iderivcalc=1
         end if
         do iobs=1,nobs
           write(22,638) (y(iobs,ipar),ipar=1,npar)
638        format(1x,8(1pg14.7,1x))
         end do
         close(unit=22)
         write(6,310) trim(aderivfile)
       end if


C --  Model outputs are now calculated and written to the output file

       write(20,640) nobs,1,2
640    format(3(i6))
       do ipar=1,npar
         if(itrans(ipar).eq.1) then
           gval(ipar)=log10(pval(ipar))-log10(initpval(ipar))
         else
           gval(ipar)=pval(ipar)-initpval(ipar)
         end if
       end do
       do iobs=1,nobs
         dtemp=initoval(iobs)
         do ies=1,nespar
           dtemp=dtemp+x(iobs,ies)*gval(ies)
         end do
         write(20,300) dtemp
300      format(1x,1pg15.8)
       end do
       write(20,660)
660    format('* row names')
       do i=1,nobs
         write(20,670) trim(aobs(i))
670      format(1x,a)
       end do
       write(20,680)
680    format('* column names')
       write(20,670) 'col1'

       close(unit=20)
       write(6,310) trim(aoutfile)
310    format(' - file ',a,' written ok.')
       go to 9900



8900   continue
       write(6,8910)
8910   format(/,' GENLIN is run using the command:',/)
       write(6,8920)
8920   format('     genlin infile outfile [derivfile] [/c]',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8940)
8940   format('     infile    is a GENLIN input file,')
       write(6,8950)
8950   format('     outfile   is a GENLIN output file,')
       write(6,8956)
8956   format('     derivfile is an optional derivatives output file, ',
     + 'and')
       write(6,8960)
8960   format('     "c"       is an optional name-checking switch.')

       go to 9900

9000   write(6,9010) trim(ainfile)
9010   format(' Error encountered while reading GENLIN',
     + ' input file ',a,'.')
       go to 9800
9100   write(amessage,9110) trim(ajcofile)
9110   format(' Error encountered while reading binary Jacobian ',
     + 'matrix file ',a,'.')
       go to 9800
9200   call writint(arow,nxrow1)
       call writint(acol,nespar1)
       write(amessage,9210) trim(ajcofile),trim(acol),trim(arow),
     + trim(ainfile)
9210   format(' Jacobian matrix contained in binary JCO file ',a,
     + ' has different dimensions (NPAR = ',a,'; NOBS = ',a,
     + ') from those of linear problem ',
     + 'specified in GENLIN input file ',a,'.')
       go to 9800
9300   write(amessage,9310) trim(ainfile)
9310   format(' Unexpected end encountered to GENLIN input file ',
     + a,'.')
       go to 9800
9400   write(amessage,9410)
9410   format(' Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9800
9500   write(amessage,9510) trim(ajcofile)
9510   format(' Error reading integer matrix header from first ',
     + 'line of matrix file ',a,'.')
       go to 9800
9600   write(aarow,'(i6)') irow
       aarow=adjustl(aarow)
       write(amessage,9610) trim(aarow),trim(ajcofile)
9610   format(' Error reading matrix row number ',a,' from matrix ',
     + 'file ',a,'.')
       go to 9800
9700   write(amessage,9710) trim(ajcofile)
9710   format(' Unexpected end encountered to file ',a,' while ',
     + 'reading matrix.')
       go to 9800
9720   write(amessage,9730) trim(ajcofile)
9730   format(' Error reading row and/or column names from matrix ',
     + 'file ',a,'.')
       go to 9800
9740   write(amessage,9750) trim(ajcofile)
9750   format(' "* column names" header expected immediately ',
     + 'following row names in matrix file ',a,'.')
       go to 9800

9800   continue
       call writmess(6,amessage)

9900   continue
       deallocate(apar,aobs,pval,initpval,
     + initoval,x,gval,stat=ierr)
       if(allocated(y))deallocate(y,stat=ierr)

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
