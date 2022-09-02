       program pest2lhs

C -- Program PEST2LHS writes an input file for the Sandia National Lab LHS
C    program based on parameters featured in a PEST control file. The LHS
C    input file may need manual editing as much is assumed in the translation.

       implicit none

       integer ifail,ierr
       integer ibeg,iend,nb,n1,iline,it,itemp,idist
       integer i,nsamp,iseed
       integer npar,npargp,ipargp,jpargp,ipar,jpar
       integer ies,jes,nespar
       integer uncertflag,ichoice,jchoice,initflag,corrflag
       integer iunit,junit,itype,vardim,covdim,jpartype
       integer lw(15),rw(15)
       double precision a,b,c,mean,stdev,dtemp,dtemp1,dtemp2,corr
       double precision m1,m2,d11,d22,d12,s11,s22,s12
       double precision logfac
       character*1 aa
       character*4 aext
       character*10 anum,atype,asamp,aseed,aline
       character*12 aapargp,aapar
       character*12 aversion
       character*25 atemp
       character*256 afile
       character*256 pestfile,qpestfile
       character*256 uncertfile,quncertfile
       character*256 lhsfilebase
       character*256 lhsmsgfile,lhsoutfile,lhsinfile,qlhsinfile
       character*300 cline
       character*600 amessage

       integer, allocatable :: itrans(:),igp(:),distrib(:)
       double precision, allocatable :: lbound(:),ubound(:)
       double precision, allocatable :: scale(:),offset(:),pval(:)
       double precision, allocatable :: parvar(:),cp(:,:)
       character*12, allocatable :: apargp(:),apar(:),bpar(:)

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,5) trim(aversion)
5      format(' PEST2LHS Version ',a,'. Watermark Numerical Computing.')

       logfac=2.302585093

       write(6,*)
120    write(6,130,advance='no')
130    format(' Enter name of PEST control file: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 120
       afile=adjustl(afile)
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,pestfile,ibeg,iend)
       if(ifail.ne.0) go to 120
#ifndef UNIX
       call lowcas(pestfile)
#endif
       nb=len_trim(pestfile)
       n1=nb-3
       if(n1.lt.1)n1=1
       aext=pestfile(n1:nb)
       call lowcas(aext)
       if(aext.ne.'.pst') pestfile=trim(pestfile)//'.pst'
       call addquote(pestfile,qpestfile)
       open(unit=10,file=pestfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(6,140) trim(qpestfile)
140      format(/,' Cannot open file ',a,' - try again.',/)
         go to 120
       end if

C -- The PEST control file is read.

C -- The first few lines are read.

       write(6,*)
       write(6,205) trim(qpestfile)
205    format(' - reading PEST control file ',a,'...')
       iline=1
       read(10,'(a)',err=9300,end=9350) cline
       cline=adjustl(cline)
       call lowcas(cline)
       if((cline(1:4).ne.'pcf ').and.(cline(1:4).ne.'jcf '))then
         write(amessage,210) trim(qpestfile)
210      format('File ',a,' does not have a "pcf" header line. It ',
     +   'does not therefore appear to be a PEST control file.')
         go to 9890
       end if
       do i=1,2
         iline=iline+1
         read(10,'(a)',err=9300,end=9350) cline
       end do
       iline=iline+1
       read(10,'(a)',err=9300,end=9350) cline
       call linspl(ifail,4,lw,rw,cline)
       if(ifail.ne.0)then
         call writint(aline,iline)
         write(amessage,215) trim(aline),trim(qpestfile)
215      format('Insufficient entries found on line ',a,' of PEST ',
     +   'control file ',a,'.')
         go to 9890
       end if
       call intread(ifail,cline(lw(1):rw(1)),npar)
       if(ifail.ne.0)then
         call writint(aline,iline)
         write(amessage,220) trim(aline),trim(qpestfile)
220      format('Error encountered in reading NPAR (number of ',
     +   'parameters) from line ',a,' of PEST control file ',a,'.')
         go to 9890
       end if
       if(npar.le.0)then
         call writint(aline,iline)
         write(amessage,230) trim(aline),trim(qpestfile)
230      format('Zero or negative value supplied for NPAR (number ',
     +   'of parameters) at line ',a,' of PEST control file ',a,'.')
         go to 9890
       end if
       call intread(ifail,cline(lw(3):rw(3)),npargp)
       if(ifail.ne.0)then
         call writint(aline,iline)
         write(amessage,235) trim(aline),trim(qpestfile)
235      format('Error encountered in reading NPARGP (number of ',
     +   'parameter groups) from line ',a,' of PEST control file ',
     +   a,'.')
         go to 9890
       end if
       if(npargp.le.0)then
         call writint(aline,iline)
         write(amessage,240) trim(aline),trim(qpestfile)
240      format('Zero or negative value supplied for NPARGP (number ',
     +   'of parameter groups) at line ',a,' of PEST control file ',
     +   a,'.')
         go to 9890
       end if

C -- The "parameter groups" section is now sought.

       do
         iline=iline+1
         read(10,'(a)',err=9300,end=9100) cline
         cline=adjustl(cline)
         if(cline(1:1).ne.'*') cycle
         if(index(cline,'* parameter gr').ne.0) go to 250
       end do
250    continue

C -- Parameter group names are now read.

       allocate(apargp(npargp),stat=ierr)
       if(ierr.ne.0) go to 9200
       do ipargp=1,npargp
260      continue
         iline=iline+1
         read(10,'(a)',err=9300,end=9150) cline
         if(cline.eq.' ') go to 260
         cline=adjustl(cline)
         if(cline(1:1).eq.'*')then
           call writint(aline,iline)
           write(amessage,270) trim(qpestfile),trim(aline)
270        format('Premature end encountered to "* parameter groups" ',
     +     'section of file ',a,' at line ',a,' of this file.')
           go to 9890
         end if
         call linspl(ifail,7,lw,rw,cline)
         if(ifail.ne.0)then
           call writint(aline,iline)
           write(amessage,275) trim(aline),trim(qpestfile)
275        format('Insufficient entries found on line ',a,' of ',
     +     'PEST control file ',a,'.')
           go to 9890
         end if
         apargp(ipargp)=cline(lw(1):rw(1))
         call lowcas(apargp(ipargp))
         if(ipargp.gt.1)then
           aapargp=apargp(ipargp)
           do jpargp=1,ipargp-1
             if(apargp(jpargp).eq.aapargp)then
               write(amessage,280) trim(apargp(ipargp)),trim(qpestfile)
280            format('Duplicate parameter group name "',a,'" in ',
     +         '"* parameter groups" section of PEST control file ',
     +         a,'.')
               go to 9890
             end if
           end do
         end if
       end do

C -- The "parameter data" section is now sought.

       do
440      continue
         iline=iline+1
         read(10,'(a)',err=9300,end=9400) cline
         cline=adjustl(cline)
         if(cline(1:1).ne.'*') cycle
         if(index(cline,'* parameter da').ne.0) go to 450
       end do
450    continue

C -- The names and some other data about the parameters are now read.

       allocate(apar(npar),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(itrans(npar),igp(npar),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(lbound(npar),ubound(npar),scale(npar),offset(npar),
     + pval(npar),stat=ierr)
       if(ierr.ne.0) go to 9200
       do ipar=1,npar
460      continue
         iline=iline+1
         read(10,'(a)',err=9300,end=9450) cline
         if(cline.eq.' ') go to 460
         cline=adjustl(cline)
         if(cline(1:1).eq.'*')then
           call writint(aline,iline)
           write(amessage,470) trim(qpestfile),trim(aline)
470        format('Premature end encountered to "* parameter data" ',
     +     'section of file ',a,' at line ',a,' of this file.')
           go to 9890
         end if
         call linspl(ifail,9,lw,rw,cline)
         if(ifail.ne.0)then
           call writint(aline,iline)
           write(amessage,475) trim(aline),trim(qpestfile)
475        format('Insufficient entries found on line ',a,' of ',
     +     'PEST control file ',a,'.')
           go to 9890
         end if
         apar(ipar)=cline(lw(1):rw(1))
         call lowcas(apar(ipar))
         if(ipar.gt.1)then
           aapar=apar(ipar)
           do jpar=1,ipar-1
             if(apar(jpar).eq.aapar)then
               write(amessage,480) trim(apar(ipar)),trim(qpestfile)
480            format('Duplicate parameter name "',a,'" in ',
     +         '"* parameter data" section of PEST control file ',
     +         a,'.')
               go to 9890
             end if
           end do
         end if
         atemp=cline(lw(2):rw(2))
         call lowcas(atemp)
         if(atemp.eq.'log')then
           itrans(ipar)=1
         else if(atemp.eq.'none')then
           itrans(ipar)=0
         else if(atemp.eq.'tied')then
           itrans(ipar)=-1
         else if(atemp.eq.'fixed')then
           itrans(ipar)=-100000
         else
           call writint(aline,iline)
           write(amessage,476) trim(aline),trim(qpestfile)
476        format('Second entry on line ',a,' of file ',a,' should ',
     +     'be "none", "log", "tied" or "fixed".')
           go to 9890
         end if
         call drealrd(ifail,cline(lw(8):rw(8)),scale(ipar))
         if(ifail.ne.0)then
           call writint(aline,iline)
           write(amessage,456) trim(aline),trim(qpestfile)
456        format('Cannot read parameter scale from line ',a,' of ',
     +     'PEST control file ',a,'.')
           go to 9890
         end if
         call drealrd(ifail,cline(lw(9):rw(9)),offset(ipar))
         if(ifail.ne.0)then
           call writint(aline,iline)
           write(amessage,469) trim(aline),trim(qpestfile)
469        format('Cannot read parameter offset from line ',a,' of ',
     +     'PEST control file ',a,'.')
           go to 9890
         end if
         call drealrd(ifail,cline(lw(5):rw(5)),lbound(ipar))
         if(ifail.ne.0)then
           call writint(aline,iline)
           write(amessage,477) trim(aline),trim(qpestfile)
477        format('Cannot read parameter lower bound from line ',
     +     a,' of PEST control file ',a,'.')
           go to 9890
         end if
         if(itrans(ipar).eq.1)then
           if(lbound(ipar).le.0.0d0)then
             call writint(aline,iline)
             write(amessage,467) trim(apar(ipar)),trim(aline),
     +       trim(qpestfile)
467          format('Parameter "',a,'" must have a positive lower ',
     +       'bound at line ',a,' of PEST control file ',a,
     +       ' as it is log transformed.')
             go to 9890
           end if
         end if
         call drealrd(ifail,cline(lw(6):rw(6)),ubound(ipar))
         if(ifail.ne.0)then
           call writint(aline,iline)
           write(amessage,478) trim(aline),trim(qpestfile)
478        format('Cannot read parameter upper bound from line ',
     +     a,' of PEST control file ',a,'.')
           go to 9890
         end if
         if(lbound(ipar).ge.ubound(ipar))then
           call writint(aline,iline)
           write(amessage,471) trim(aline),trim(qpestfile)
471        format('Upper parameter bound is not greater than ',
     +     'lower parameter bound at line ',a,' of PEST control ',
     +     'file ',a,'.')
           go to 9890
         end if
         call drealrd(ifail,cline(lw(4):rw(4)),pval(ipar))
         if(ifail.ne.0)then
           call writint(aline,iline)
           write(amessage,479) trim(aline),trim(qpestfile)
479        format('Cannot read parameter value from line ',
     +     a,' of PEST control file ',a,'.')
           go to 9890
         end if
         if(itrans(ipar).eq.1)then
           if(pval(ipar).le.0.0d0)then
             call writint(aline,iline)
             write(amessage,447) trim(apar(ipar)),trim(aline),
     +       trim(qpestfile)
447          format('Parameter "',a,'" must have a positive value ',
     +       'at line ',a,' of PEST control file ',a,' as it is log ',
     +       'transformed.')
             go to 9890
           end if
         end if
         if((pval(ipar).lt.lbound(ipar)).or.
     +      (pval(ipar).gt.ubound(ipar)))then
           call writint(aline,iline)
           write(amessage,481) trim(aline),trim(qpestfile)
481        format('Parameter value lies outside the range of ',
     +     'its bounds at line ',a,' of PEST control file ',a,'.')
           go to 9890
         end if
         atemp=cline(lw(7):rw(7))
         call lowcas(atemp)
         do ipargp=1,npargp
           if(atemp.eq.apargp(ipargp))then
             igp(ipar)=ipargp
             go to 484
           end if
         end do
         call writint(aline,iline)
         write(amessage,483) trim(atemp),trim(aline),trim(qpestfile)
483      format('Unknown parameter group "',a,'" cited at line ',a,
     +   ' of PEST control file ',a,'.')
         go to 9890
484      continue
       end do
       close(unit=10)
       nespar=0
       do ipar=1,npar
         if(itrans(ipar).ge.0) nespar=nespar+1
       end do
       if(nespar.eq.0)then
         write(amessage,485) trim(qpestfile)
485      format('All parameters cited in "* parameter data" section ',
     +   'of PEST control file ',a,' are either tied or fixed.')
         go to 9890
       end if
       call writint(anum,npar)
       write(6,490) trim(anum)
490    format(' - data for ',a,' parameters read from PEST control ',
     + 'file.')

       uncertflag=0
       write(6,*)
       write(6,145)
145    format(' There are two options for specifying ',
     + 'parameter probability distributions:')
       write(6,150)
150    format('   For (log)normal distributions and an uncertainty ',
     + 'file            - enter 1')
       write(6,155)
155    format('   For user-specified distributions with bounds-derive',
     + 'd descriptors - enter 2')
156    write(6,157,advance='no')
157    format(' Enter your choice: ')
       read(5,*,err=156) ichoice
       if((ichoice.ne.1).and.(ichoice.ne.2)) go to 156
       write(6,*)
       write(6,161)
161    format(' For maximum value of non-uniform distributions:-')
       write(6,162)
162    format('   If halfway between (log) bounds',
     + '                     - enter 1')
       write(6,163)
163    format('   If parameter initial value f',
     + 'rom PEST control file   - enter 2')
164    write(6,157,advance='no')
       read(5,*,err=164) jchoice
       if((jchoice.ne.1).and.(jchoice.ne.2)) go to 164
       initflag=0
       if(jchoice.eq.2) initflag=1

       uncertflag=0
       if(ichoice.eq.1)then
         write(6,*)
160      write(6,170,advance='no')
170      format(' Enter name of parameter uncertainty file: ')
         read(5,'(a)') afile
         if(afile.eq.' ') go to 160
         afile=adjustl(afile)
         ibeg=1
         iend=len_trim(afile)
         call getfile(ifail,afile,uncertfile,ibeg,iend)
         if(ifail.ne.0) go to 160
#ifndef UNIX
         call lowcas(uncertfile)
#endif
         call addquote(uncertfile,quncertfile)
         open(unit=11,file=uncertfile,status='old',iostat=ierr)
         if(ierr.ne.0)then
           write(6,141) trim(quncertfile)
141        format(/,' Cannot open file ',a,' - try again.',/)
           go to 160
         end if
         uncertflag=1

C -- The parameter uncertainty file is read.

         write(6,*)
         write(6,142) trim(quncertfile)
142      format(' - reading parameter uncertainty file ',a,'...')
         iunit=11
         junit=15
         itype=2
         vardim=1
         covdim=nespar
         allocate(parvar(vardim),cp(covdim,covdim),stat=ierr)
         if(ierr.ne.0) go to 9200
         allocate(bpar(nespar),stat=ierr)
         if(ierr.ne.0) go to 9200
         ies=0
         do ipar=1,npar
           if(itrans(ipar).ge.0)then
             ies=ies+1
             bpar(ies)=apar(ipar)
           end if
         end do
         atype='parameter'
         call read_uncert_data_file(ifail,iunit,junit,itype,jpartype,
     +   nespar,vardim,covdim,parvar,cp,atype,uncertfile,amessage,
     +   cline,bpar)
         if(ifail.ne.0) then
           amessage=adjustl(amessage)
           go to 9890
         end if
         write(6,510)
510      format(' - parameter uncertainty file read ok.')

! -- Elements pertaining to logged parameters are converted from log10
!    to natural log.

         ies=0
         do ipar=1,npar
           if(itrans(ipar).ge.0)then
             ies=ies+1
             if(itrans(ipar).eq.1)then
               do jes=1,nespar
                 cp(jes,ies)=cp(jes,ies)*logfac
                 cp(ies,jes)=cp(jes,ies)
               end do
               cp(ies,ies)=cp(ies,ies)*logfac
             end if
           end if
         end do

       else

C -- Information pertaining to probability distributions is now obtained
C    from the user.

         write(6,*)
         write(6,*)
         write(6,530)
530      format(' Parameter probability distributions must now be ',
     +   'provided. This must be done',/,'   on a ',
     +   'group-by-group basis.')
         write(6,*)
         write(6,540)
540      format(' Select uniform/triangular/normal distributions ',
     +   'as [u/t/n].')
         write(6,*)
         write(6,550)
550      format(' Note that triangular is not allowed if some ',
     +   'group members are log-transformed.')
         write(6,*)

         allocate(distrib(npargp),stat=ierr)
         if(ierr.ne.0) go to 9200
         do ipargp=1,npargp
569        write(6,570,advance='no') trim(apargp(ipargp))
570        format('    Enter distribution for parameter group "',a,
     +     '" [u/t/n]: ')
           read(5,'(a)') aa
           if(aa.eq.' ') go to 569
           call lowcas(aa)
           if((aa.ne.'u').and.(aa.ne.'t').and.(aa.ne.'n')) go to 569
           if(aa.eq.'t')then
             do ipar=1,npar
               if(igp(ipar).eq.ipargp)then
                 if(itrans(ipar).eq.1)then
                   write(6,*)
                   write(6,580) trim(apar(ipar))
580                format('   Parameter "',a,'" belonging to this ',
     +             'group is log-transformed - try again.')
                   write(6,*)
                   go to 569
                 end if
               end if
             end do
             distrib(ipargp)=2
           else if(aa.eq.'u')then
             distrib(ipargp)=1
           else if(aa.eq.'n')then
             distrib(ipargp)=3
           end if
         end do
       end if

! -- The name of the LHS input file to write is obtained.

       write(6,*)
610    write(6,620,advance='no')
620    format(' Enter filename base for LHS files: ')
       read(5,'(a)') lhsfilebase
       if(lhsfilebase.eq.' ') go to 610
       n1=len_trim(lhsfilebase)
       do i=n1,1,-1
         aa=lhsfilebase(i:i)
         if((aa.eq.'/').or.(aa.eq.'\')) go to 628
         if(aa.eq.'.')then
           write(6,624)
624        format(/,' Do not provide an extension - just filename ',
     +     'root - try again',/)
           go to 610
         end if
       end do
628    continue

#ifdef UNIX
       call lowcas(lhsfilebase)
#endif

       lhsmsgfile=trim(lhsfilebase)//'.lmo'
       lhsoutfile=trim(lhsfilebase)//'.lsp'
       lhsinfile=trim(lhsfilebase)//'.dat'
       call addquote(lhsinfile,qlhsinfile)
       open(unit=20,file=lhsinfile,action='write',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,630) trim(qlhsinfile)
630      format(' Cannot open LHS input file ',a,' to write ',
     +   'LHS input data  - try again.')
         call write_message(amessage)
         go to 610
       end if
650    write(6,660,advance='no')
660    format(' How many parameter sample sets must be generated? ')
       read(5,*,err=650) nsamp
       if(nsamp.le.0) go to 650
680    write(6,690,advance='no')
690    format(' Enter random number seed (a big integer): ')
       read(5,*,err=680) iseed

C -- We see if there are any parameter correlations.

       corrflag=0
       if(uncertflag.eq.1)then
         if(jpartype.eq.2)then
           do ies=2,nespar
             do jes=1,nespar-1
               if(cp(jes,ies).ne.0.0d0) then
                 corrflag=1
                 go to 695
               end if
             end do
           end do
695        continue
         end if
       end if

C -- We start writing the LHS input file.

       write(6,*)
       write(6,696)
696    format(' - writing LHS input file....')

       write(20,710) trim(qpestfile)
710    format('LHSTITL LHS dataset built from PEST control file ',
     + a,'.')
       call addquote(lhsmsgfile,afile)
       write(20,712) trim(afile)
712    format('LHSMSG ',a)
       call addquote(lhsoutfile,afile)
       write(20,714) trim(afile)
714    format('LHSOUT ',a)
       call writint(asamp,nsamp)
       write(20,720) trim(asamp)
720    format('LHSOBS ',a)
       call writint(aseed,iseed)
       write(20,730) trim(aseed)
730    format('LHSSEED ',a)
       if(corrflag.ne.0)then
         write(20,740)
740      format('LHSRPTS CORR')
       end if

C -- The data part of the LHS input file is now written.

       write(20,850)
850    format('DATASET:')
       if(uncertflag.eq.0)then
         ies=0
         do ipar=1,npar
           it=itrans(ipar)
           if(it.lt.0) cycle
           ies=ies+1
           itemp=igp(ipar)
           idist=distrib(itemp)
           if(idist.eq.1)then
             a=lbound(ipar)
             b=ubound(ipar)
             if(it.eq.0)then
               write(20,860) trim(apar(ipar)),a,b
860            format(a,t16,'UNIFORM',t37,2(1pg14.7,2x))
             else
               write(20,870) trim(apar(ipar)),a,b
870            format(a,t16,'LOGUNIFORM',t37,2(1pg14.7,2x))
             end if
           else if(idist.eq.2)then
             if(it.eq.0)then
               a=lbound(ipar)
               c=ubound(ipar)
               if(initflag.eq.1)then
                 b=pval(ipar)
               else
                 b=(a+c)*0.5
               end if
               write(20,880) trim(apar(ipar)),a,b,c
880            format(a,t16,'TRIANGULAR',t37,3(1pg14.7,2x))
             end if
           else if(idist.eq.3)then
             if(it.eq.0)then
               if(initflag.eq.0)then
                 mean=(lbound(ipar)+ubound(ipar))*0.5
                 stdev=(ubound(ipar)-lbound(ipar))/6.0
                 write(20,890) trim(apar(ipar)),mean,stdev,
     +           lbound(ipar),ubound(ipar)
890              format(a,t16,'BOUNDED NORMAL',t37,4(1pg14.7,2x))
               else
                 mean=pval(ipar)
                 stdev=pval(ipar)-lbound(ipar)
                 dtemp=ubound(ipar)-mean
                 if(dtemp.gt.stdev)dtemp=stdev
                 stdev=stdev/3.0
                 write(20,890) trim(apar(ipar)),mean,stdev,
     +           lbound(ipar),ubound(ipar)
               end if
             else
               if(initflag.eq.0)then
                 mean=(log(lbound(ipar))+log(ubound(ipar)))*0.5
                 stdev=(log(ubound(ipar))-log(lbound(ipar)))/6.0
                 write(20,900) trim(apar(ipar)),mean,stdev,
     +           lbound(ipar),ubound(ipar)
900              format(a,t16,'BOUNDED LOGNORMAL-N',t37,4(1pg14.7,2x))
               else
                 dtemp1=log(lbound(ipar))
                 dtemp2=log(ubound(ipar))
                 mean=log(pval(ipar))
                 stdev=mean-dtemp1
                 dtemp=dtemp2-mean
                 if(dtemp.gt.stdev)dtemp=stdev
                 stdev=stdev/3.0
                 write(20,900) trim(apar(ipar)),mean,stdev,
     +           lbound(ipar),ubound(ipar)
               end if
             end if
           end if
         end do
       else
         ies=0
         do ipar=1,npar
           it=itrans(ipar)
           if(it.lt.0) cycle
           ies=ies+1
           stdev=sqrt(cp(ies,ies))
           if(it.eq.0)then
             if(initflag.eq.0)then
               mean=(lbound(ipar)+ubound(ipar))*0.5
             else
               mean=pval(ipar)
             end if
             write(20,890) trim(apar(ipar)),mean,stdev,
     +       lbound(ipar),ubound(ipar)
           else
             if(initflag.eq.0)then
               mean=0.5*(log(ubound(ipar))+log(lbound(ipar)))
             else
               mean=log(pval(ipar))
             end if
             write(20,900) trim(apar(ipar)),mean,stdev,
     +       lbound(ipar),ubound(ipar)
           end if
         end do

C -- Now we take care of correlations if needed.
C -- Where two parameters are log transformed their correlations are converted
C    to that of native parameters. If one is log-transformed and the other
C    is not then the correlation is left as it is.

         if(jpartype.eq.2)then
           do jes=2,nespar
             do ies=1,jes-1
               if(cp(ies,jes).ne.0.0) go to 920
             end do
           end do
           go to 1100
920        continue

           jes=0
           do jpar=1,npar
             if(itrans(jpar).ge.0)then
               jes=jes+1
               ies=0
               do ipar=1,jpar
                 if(itrans(ipar).ge.0)then
                   ies=ies+1
                   if(ies.ne.jes)then
                     if(cp(ies,jes).ne.0.0)then
                       if(itrans(jpar)+itrans(ipar).le.1)then
                         corr=cp(ies,jes)/sqrt(cp(jes,jes)*cp(ies,ies))
                       else
                         if(initflag.eq.0)then
                           m1=0.5*(log(lbound(ipar))+log(ubound(ipar)))
                           m2=0.5*(log(lbound(jpar))+log(ubound(jpar)))
                         else
                           m1=log(pval(ipar))
                           m2=log(pval(jpar))
                         end if
                         d11=cp(ies,ies)
                         d22=cp(jes,jes)
                         d12=cp(ies,jes)
                         s11=(exp((m1+m1)+(d11+d11)/2))*(exp(d11)-1)
                         s22=(exp((m2+m2)+(d22+d22)/2))*(exp(d22)-1)
                         s12=(exp((m1+m2)+(d11+d22)/2))*(exp(d12)-1)
                         corr=s12/sqrt(s11*s22)
                       end if
                       write(20,930) trim(apar(jpar)),
     +                 trim(apar(ipar)),corr
930                    format('CORRELATE ',a,' ',a,' ',1pg14.7)
                     end if
                   end if
                 end if
               end do
             end if
           end do
         end if
       end if

1100   continue
       close(unit=20)
       write(6,940) trim(qlhsinfile)
940    format(' - file ',a,' written ok.')

       go to 9900

9100   write(amessage,9110) trim(qpestfile)
9110   format('Cannot find "* parameter groups" section in ',
     + 'PEST control file ',a,'.')
       go to 9890

9150   write(amessage,9160) trim(qpestfile)
9160   format('End of file ',a,' encountered while reading ',
     + '"* parameter groups" section of that file.')
       go to 9890

9200   write(amessage,9210)
9210   format('Cannot allocate memory necessary for continuation ',
     + 'of execution.')
       go to 9890

9300   continue
       call writint(anum,iline)
       write(amessage,9310) trim(anum),trim(qpestfile)
9310   format('Error encountered when reading line ',a,' of PEST ',
     + 'control file ',a,'.')
       go to 9890

9350   write(amessage,9360) trim(qpestfile)
9360   format('Premature end encountered to PEST control file ',a,'.')
       go to 9890

9400   write(amessage,9410) trim(qpestfile)
9410   format('Cannot find "* parameter data" section in ',
     + 'PEST control file ',a,'.')
       go to 9890

9450   write(amessage,9460) trim(qpestfile)
9460   format('End of file ',a,' encountered while reading ',
     + '"* parameter data" section of that file.')
       go to 9890

9890   call write_message(amessage)

9900   continue
       close(unit=10,iostat=ierr)
       close(unit=11,iostat=ierr)
       close(unit=15,iostat=ierr)
       close(unit=20,iostat=ierr)

       if(allocated(itrans)) deallocate(itrans,stat=ierr)
       if(allocated(igp)) deallocate(igp,stat=ierr)
       if(allocated(distrib)) deallocate(distrib,stat=ierr)
       if(allocated(lbound)) deallocate(lbound,stat=ierr)
       if(allocated(ubound)) deallocate(ubound,stat=ierr)
       if(allocated(scale)) deallocate(scale,stat=ierr)
       if(allocated(offset)) deallocate(offset,stat=ierr)
       if(allocated(pval)) deallocate(pval,stat=ierr)
       if(allocated(parvar)) deallocate(parvar,stat=ierr)
       if(allocated(cp)) deallocate(cp,stat=ierr)
       if(allocated(apargp)) deallocate(apargp,stat=ierr)
       if(allocated(apar)) deallocate(apar,stat=ierr)
       if(allocated(bpar)) deallocate(bpar,stat=ierr)

       end



        subroutine write_message(errmsg)

C -- Subroutine write_message formats and writes a message to the screen.

        implicit none

        integer  junit
        integer  jend,i,nblc,ierr,j
        character*(*) errmsg

        junit=6
        write(junit,*)

        j=0
        nblc=len_trim(errmsg)
5       jend=j+78
        if(jend.ge.nblc) go to 100
        do i=jend,j+1,-1
          if(errmsg(i:i).eq.' ') then
            write(junit,50,iostat=ierr) errmsg(j+1:i)
50          format(1x,a)
            j=i
            go to 5
          end if
        end do
        write(junit,50,iostat=ierr) errmsg(j+1:jend)
        j=jend
        go to 5
100     jend=nblc
        write(junit,50,iostat=ierr) errmsg(j+1:jend)
        write(junit,*)
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

