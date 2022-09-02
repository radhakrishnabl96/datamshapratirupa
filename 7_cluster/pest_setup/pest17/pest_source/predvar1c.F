       program predvar1c

C -- Program PREDVAR1C calculates predictive variance for different numbers of
C    singular values using SVD of Q(1/2)X matrix.
C -- It's calculations include the White, Doherty and Hughes (2014) model error term.
C -- Unlike PREDVAR1B, PREDVAR1C requires that the prior covariance matrix have no
C    off-diagonal elements.
C -- PREDVAR1C (unlike PREDVAR1B) optionally computes residual variances.

       use pestdata, only  : maxcompdim,pestmode,npar,nobsgp,nxrow,
     +                       errmsg,apar,obgnme,aobs,itrans,numcov,
     +                       covind,owght,x,pval_o,modval_o,
     +                       parchglim,pval,parlbnd,parubnd,
     +                       ipargp,scale,offset,dercom,covgp,nespar,
     +                       nobs,nobgnm,covar,nobscov,npargp,pargnme


       implicit none

       integer, parameter :: MAXSING=2000
       logical lexist
       integer lwork,ibeg,iend,ifail,nb_pst,ierr,numsing,irestart,
     + iobsgp,itemp,irow,itemp1,itemp2,bn,icount,i,j,ies,nrow,ncol,
     + icode,icurrent,ipar,iunit,junit,itype,vardim,
     + jrow,icov,info,k,jsing,nb,igroup,iline
       integer maxobs
       integer p1start,pfile
       integer ldvt,iicount,nxrow1,nsing,icovrow,ipass,nrowold
       integer ipargroup,kcount,jcount
       integer nesparc,nespara,iesa,iesc,jesa,jesc
       integer covdima,covdimc,jpartypea,jpartypec
       integer krow,restat
       integer ndimc
       integer ising(MAXSING)
       double precision refvar,dtemp,first,second,third,rtemp
       double precision fourth
       double precision sum,sum1,sum2,sum3
       double precision parvar(1),vt_dummy(1,1)
       character*1 aa
       character*4 aext
       character*10 atype,asing,anum,aline
       character*12 aversion
       character*12 aapar
       character*15 acov
       character*20 aaobs,apred
       character*30 atemp30
       character*200 afile,bfile,pestfile,jcofile,uncertfile,predfile,
     + outfile,resfile
       character*200 predfile1
       character*300 dline

       integer, allocatable :: iindex(:)
       integer, allocatable :: iregobs(:),iregobsgp(:)
       integer, allocatable :: numinpargp(:),corpargp(:)
       integer, allocatable :: cories(:)
       double precision, allocatable :: ytv2(:)
       double precision, allocatable :: u(:,:)
       double precision, allocatable :: workvec(:),w1(:),y1(:),
     + vt(:,:),w1x(:),yy(:)
       double precision, allocatable :: ya(:),yc(:)
       double precision, allocatable :: xxa(:,:),xxc(:,:)
       double precision, allocatable :: uu(:,:)
       double precision, allocatable :: cpa(:,:),cpc(:,:)
       double precision, allocatable :: w2(:)
       double precision, allocatable :: sda(:),sdc(:)
       double precision, allocatable :: z0z0t(:,:),u2u2t(:,:)
       double precision, allocatable :: resvar1(:),resvar2(:),resvar3(:)
       character*12, allocatable :: apar1(:),adjapar(:),corapar(:)

C -- Initialization

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       nrow=0
       nrowold=0
       ipass=0

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' PREDVAR1C Version ',a,'. Watermark Numerical ',
     + 'Computing.',/)

C -- Input data is acquired from the user.

20     write(6,30,advance='no')
30     format(' Enter name of PEST control file: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 20
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,pestfile,ibeg,iend)
#ifndef UNIX
       call lowcas(pestfile)
#endif
       if(ifail.ne.0) go to 20
       nb_pst=len_trim(pestfile)
       nb_pst=nb_pst-3
       if(nb_pst.lt.1)nb_pst=1
       aext=pestfile(nb_pst:)
       call lowcas(aext)
       if(aext.ne.'.pst')then
         write(6,40)
40       format(/,' PEST control file must have an extension of ',
     +   '".pst" - try again.',/)
         go to 20
       end if
       inquire(file=pestfile,exist=lexist)
       if(.not.lexist)then
         call addquote(pestfile,afile)
         write(6,50) trim(afile)
50       format(/,' Cannot find file ',a,' - try again.',/)
         go to 20
       end if

C -- A check is made that the corresponding Jacobian matrix file is present.

       jcofile=pestfile(1:nb_pst)//'jco'
       call open_unformatted_file(ifail,11,'read',jcofile,errmsg)
       if(ifail.ne.0)then
         call addquote(jcofile,afile)
         call addquote(pestfile,bfile)
         write(errmsg,60) trim(afile),trim(bfile)
60       format('Cannot open Jacobian matrix file ',a,
     +   ' corresponding to PEST control file ',a,'.')
         go to 9890
       end if

51     write(6,52,advance='no')
52     format(' Enter observation reference variance ',
     + 'i.e. phi(nonreg)/nobs(nonreg): ')
       read(5,*,err=51)refvar
       if(refvar.le.0.0d0) go to 51

C -- More input is sought.

       write(6,*)
80     write(6,90,advance='no')
90     format(' Enter name of parameter uncertainty file: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 80
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,uncertfile,ibeg,iend)
       if(ifail.ne.0) go to 80
#ifndef UNIX
       call lowcas(uncertfile)
#endif
       open(unit=12,file=uncertfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(uncertfile,afile)
         write(6,95) trim(afile)
95       format(/,' Cannot open file ',a,' - try again.',/)
         go to 80
       end if

100    write(6,110,advance='no')
110    format(' Enter name of predictive sensitivity matrix list ',
     + 'file: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 100
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,predfile,ibeg,iend)
       if(ifail.ne.0) go to 100
#ifndef UNIX
       call lowcas(predfile)
#endif
       open(unit=13,file=predfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(predfile,afile)
         write(6,95) trim(afile)
         go to 100
       end if

       write(6,*)
       numsing=0
129    numsing=numsing+1
       if(numsing.gt.MAXSING)then
         write(errmsg,130)
130      format('Too many singular values - increase MAXSING and ',
     +   're-compile program.')
         go to 9890
       end if
140    write(6,150,advance='no')
150    format(' Enter no. of singular values before truncation ',
     + '[<Enter> if no more]: ')
       read(5,'(a)') anum
       if(anum.eq.' ')then
         if(numsing.eq.1) go to 140
         go to 200
       end if
       call intread(ifail,anum,ising(numsing))
       if(ifail.ne.0) then
         write(6,151)
151      format(/,' Illegal input - try again.',/)
         go to 140
       end if
       if(ising(numsing).lt.0)then
         write(6,151)
         go to 140
       end if
       go to 129

200    numsing=numsing-1

C -- All input data has now been acquired. The complete PEST input
C    dataset is now read.

       write(6,*)
       call addquote(pestfile,afile)
       write(6,235) trim(afile)
235    format(' - reading PEST control file ',a,'....')
       irestart=0
       p1start=0
       pfile=0
       call read_pest_data(ifail,irestart,p1start,pfile,pestfile)
       if(ifail.ne.0) go to 9890
       if(maxcompdim.gt.1)then
         write(errmsg,240) trim(afile)
240      format('PREDVAR1C requires that MAXCOMPDIM be set to 1 ',
     +   'or less in PEST control file ',a,'.')
         go to 9890
       end if
       if((pestmode.eq.2).or.(pestmode.eq.4))then
         write(errmsg,250) trim(afile)
250      format('PREDVAR1C requires that PEST not be run in ',
     +   'predictive analysis or Pareto modes in PEST control ',
     +   'file ',a,'. PREDVAR1C does not allow this.')
         go to 9890
       end if
       if(pestmode.eq.3)then
260      write(errmsg,270) trim(afile)
270      format(' Warning: file ',a,' tells PEST to run in ',
     +   'regularisation mode. All regularisation observations and ',
     +   'regularisation prior information will be ignored.')
         call writmess(6,errmsg)
         write(6,*)
       end if
       if(nespar.eq.0)then
         write(errmsg,272) trim(afile)
272      format('There are no adjustable parameters cited in PEST ',
     +   'control file ',a,'.')
         go to 9890
       end if
       if(nxrow.eq.0)then
         write(errmsg,273) trim(afile)
273      format('No observations or prior information items are ',
     +   'cited in PEST control file ',a,'.')
         go to 9890
       end if

C -- We find out which parameters are "adjustable parameters" and which
C    parameters are "simplicity-correction" parameters.

       if(npargp.eq.1)then
         write(errmsg,2010) trim(afile)
2010     format('File ',a,' cites only one parameter group. ',
     +   'More than one group is required in this file to separate ',
     +   'simplicity-correction parameters from adjustable ',
     +   'parameters.')
         go to 9890
       end if
       allocate(numinpargp(npargp),corpargp(npargp),stat=ierr)
       if(ierr.ne.0) go to 9200
       numinpargp=0                    ! an array
       corpargp=0                      ! an array
       do ipargroup=1,npargp
         do ipar=1,npar
           if(ipargp(ipar).eq.ipargroup)then
             if(itrans(ipar).ge.0)then
               numinpargp(ipargroup)=numinpargp(ipargroup)+1
             end if
           end if
         end do
       end do
       jcount=0
       do ipargroup=1,npargp
         if(numinpargp(ipargroup).gt.0) jcount=jcount+1
       end do
       if(jcount.le.1)then
         write(errmsg,2020) trim(afile)
2020     format('Only one parameter group in file ',a,' contains ',
     +   'non-fixed and non-tied parameters. This prevents ',
     +   'separation of parameters into ',
     +   'adjustable parameters and simplicity-correction parameters.')
         go to 9890
       end if
       write(6,275) trim(afile)
275    format(' - file ',a,' read ok.')
2027   continue
       write(6,*)
       write(6,2028)
2028   format(' Parameter groups containing a non-zero number of ',
     + 'non-tied and non-fixed')
       write(6,2029)
2029   format(' parameters will now be listed.')
       write(6,*)
       write(6,2030)
2030   format(' Identify simplicity-correction parameter groups.')
       write(6,2040)
2040   format(' Other groups are presumed to contain only ',
     + 'calibration-adjustable parameters.')
2041   continue
       write(6,*)
       kcount=0
       jcount=0
       do ipargroup=1,npargp
         if(numinpargp(ipargroup).gt.0)then
2049       write(6,2050,advance='no') trim(pargnme(ipargroup))
2050       format('   Parameter group "',a,'"?  [y/n]: ')
           read(5,'(a)') aa
           if((aa.eq.'y').or.(aa.eq.'Y'))then
             corpargp(ipargroup)=1
             jcount=jcount+1
           else if((aa.eq.'n').or.(aa.eq.'N'))then
             corpargp(ipargroup)=0
             kcount=kcount+1
           else
             go to 2049
           end if
         end if
       end do
C       if(jcount.eq.0)then
C         write(6,2060)
C2060     format(/,' No simplicity-correction parameter ',
C     +   'groups have been identified.')
C         write(6,2070)
2070     format(' Try again.')
C         go to 2041
C       end if
       if(kcount.eq.0)then
         write(6,2080)
2080     format(/,' All parameter groups have been identified as ',
     +   'containing correction parameters.')
         write(6,2090)
2090     format(' No groups contain calibration-adjustable ',
     +   'parameters.')
         write(6,2070)
         go to 2041
       end if
       nesparc=0
       do ipargroup=1,npargp
         if(corpargp(ipargroup).eq.1)then
           nesparc=nesparc+numinpargp(ipargroup)
         end if
       end do
       nespara=nespar-nesparc

C -- We find out if residual stats information needs to be calculated.

       restat=0
       write(6,*)
2091   write(6,2092,advance='no')
2092   format(' Calculate residuals statistics? [y/n]: ')
       read(5,'(a)',err=2091) aa
       if((aa.eq.'Y').or.(aa.eq.'y'))then
         restat=1
       else if((aa.eq.'N').or.(aa.eq.'n'))then
         restat=0
       else
         go to 2091
       end if
       if(restat.ne.0)then
2093     write(6,2094,advance='no')
2094     format(' Enter name for residuals statistics file: ')
         read(5,*,err=2093) resfile
         open(unit=30,file=resfile,err=2093)
       end if

C -- We can delete some unused PEST memory.

       deallocate(pval_o,modval_o,parchglim,pval,parlbnd,parubnd,
     + scale,offset,dercom,stat=ierr)

C -- If PEST is running in regularisation mode, regularisation
C    observations and prior information equations are now identified.

       allocate(iregobs(nxrow),iregobsgp(nobsgp),stat=ierr)
       if(ierr.ne.0) go to 9200
       if(pestmode.ne.3)then
         do irow=1,nxrow
           iregobs(irow)=0
         end do
         do iobsgp=1,nobsgp
           iregobsgp(iobsgp)=0
         end do
       else
         do iobsgp=1,nobsgp
           call lowcas(obgnme(iobsgp))
           if(obgnme(iobsgp)(1:5).eq.'regul')then
             iregobsgp(iobsgp)=1
           else
             iregobsgp(iobsgp)=0
           end if
         end do
         do irow=1,nxrow
           itemp=nobgnm(irow)
           iregobs(irow)=iregobsgp(itemp)
         end do
       end if

C -- The Jacobian matrix file is now read.

       call addquote(jcofile,afile)
       write(6,276) trim(afile)
276    format(/,' - reading Jacobian matrix file ',a,'....')
       read(11,err=9000,end=9000)itemp1,itemp2
       if(abs(itemp1).ne.nespar) go to 9050
       if(itemp1.lt.0)then
         continue
       else
         write(errmsg,290) trim(afile)
290      format('Jacobian matrix file ',a,' was not produced by ',
     +   'version 8 or later of PEST; use program JCOTRANS to ',
     +   'convert this to new format.')
         go to 9890
       end if
       itemp2=abs(itemp2)
       bn=itemp2
       if((itemp2.ne.nxrow).and.(itemp2.ne.nobs)) go to 9050
       do i=1,nespar
         do j=1,nobs
           x(j,i)=0.0d0
         end do
       end do
       read(11,err=9000,end=9000)icount
       do i=1,icount
         read(11,err=9000,end=9000) j,dtemp
         ies=(j-1)/bn+1
         irow=j-(ies-1)*bn
         x(irow,ies)=dtemp
       end do
       do i=1,npar
         if(itrans(i).lt.0) cycle
         read(11,err=9000,end=9000) aapar
         if(aapar.ne.apar(i)) go to 9050
       end do
       do i=1,bn
         read(11,err=9000,end=9000) aaobs
         call lowcas(aaobs)
         if(aaobs.ne.aobs(i)) go to 9050
       end do
       close(unit=11)
       write(6,275) trim(afile)

C -- New parameter name arrays are built comprised only of adjustable parameters
C    on the one hand and correction parameters on the other.

       ndimc=max(1,nesparc)
       allocate(corapar(ndimc),adjapar(nespara),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(ya(nespara),yc(ndimc),stat=ierr)
       if(ierr.ne.0) go to 9200

       iesa=0
       iesc=0
       do ipar=1,npar
         if(itrans(ipar).ge.0) then
           ipargroup=ipargp(ipar)
           if(corpargp(ipargroup).eq.0)then
             iesa=iesa+1
             adjapar(iesa)=apar(ipar)
           else
             iesc=iesc+1
             corapar(iesc)=apar(ipar)
           end if
         end if
       end do

C -- The name of the next predictive sensitivity and corresponding output file are read.

       iline=0
1010   continue
       ipass=ipass+1
       iline=iline+1
       read(13,'(a)',end=1200) dline
       if(dline.eq.' ') go to 1010
       ibeg=1
       iend=len_trim(dline)
       call getfile(ifail,dline,predfile1,ibeg,iend)
       if(ifail.ne.0)then
         call addquote(predfile,afile)
         call writint(aline,iline)
         write(errmsg,1011) trim(aline),trim(afile)
1011     format('Cannot read first filename from line ',a,
     +   ' of file ',a,'.')
         go to 9890
       end if
#ifndef UNIX
       call lowcas(predfile1)
#endif
       open(unit=16,file=predfile1,status='old',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(predfile1,afile)
         call addquote(predfile,bfile)
         call writint(aline,iline)
         write(errmsg,1020) trim(afile),trim(aline),trim(bfile)
1020     format('Cannot open predictive sensitivity file ',a,
     +   ' cited on line ',a,' of file ',a,'.')
         go to 9890
       end if
       ibeg=iend+1
       iend=len_trim(dline)
       call getfile(ifail,dline,outfile,ibeg,iend)
       if(ifail.ne.0)then
         call addquote(predfile,afile)
         call writint(aline,iline)
         write(errmsg,1012) trim(aline),trim(afile)
1012     format('Cannot read second filename from line ',a,
     +   ' of file ',a,'.')
         go to 9890
       end if
#ifndef UNIX
       call lowcas(outfile)
#endif

C -- The predictive sensitivity file is now read. Note that it is not necessarily
C    assumed that the same parameters are cited in the same order.

       call addquote(predfile1,afile)
       write(6,505) trim(afile)
505    format(/' - reading predictive sensitivity matrix file ',a,
     + '....')
       nrowold=max(nrow,nrowold)
       read(16,*,err=9100,end=9150) nrow,ncol,icode
       if(ncol.ne.1)then
         write(errmsg,520) trim(afile)
520      format('The matrix contained in predictive sensitivity ',
     +   'matrix file ',a,' must contain only one column.')
         go to 9890
       end if
       if(icode.ne.2)then
         write(errmsg,530) trim(afile)
530      format('File ',a,' does not have correct matrix file ',
     +   'format; ICODE must be set to 2 in this file.')
         go to 9890
       end if
       if(ipass.eq.1)then
         allocate(y1(nrow),apar1(nrow),stat=ierr)
         if(ierr.ne.0) go to 9200
       else
         if(nrow.gt.nrowold)then
           deallocate(y1,apar1,stat=ierr)
           allocate(y1(nrow),apar1(nrow),stat=ierr)
           if(ierr.ne.0) go to 9200
         end if
       end if
       do irow=1,nrow
         read(16,*,err=9100,end=9150) y1(irow)
       end do
       read(16,'(a)',err=9100,end=9150) dline
       call lowcas(dline)
       dline=adjustl(dline)
       if(dline(1:1).ne.'*') go to 9250
       if(index(dline,'row').eq.0) go to 9250
       if(index(dline,'name').eq.0) go to 9250
       do irow=1,nrow
         read(16,*,err=9100,end=9150) apar1(irow)
         call lowcas(apar1(irow))
       end do
       read(16,'(a)',err=9100,end=9150) dline
       call lowcas(dline)
       dline=adjustl(dline)
       if(dline(1:1).ne.'*') go to 9300
       if(index(dline,'col').eq.0) go to 9300
       if(index(dline,'name').eq.0) go to 9300
       read(16,*,err=9100,end=9150) apred
       call lowcas(apred)
       close(unit=16)

C -- The predictive sensitivity vector is now split between sensitivities to
C    adjustable and simplicity-correction parameters.

       icurrent=1
       do iesa=1,nespara
         call which1(ifail,nrow,icurrent,apar1,adjapar(iesa))
         if(ifail.ne.0)then
           write(errmsg,550) trim(adjapar(iesa)),trim(afile)
550        format('Parameter "',a,'" is an adjustable parameter; ',
     +     'however it is not cited in ',
     +     'predictive sensitivity matrix file ',a,'.')
           go to 9890
         end if
         ya(iesa)=y1(icurrent)
       end do

       if(nesparc.gt.0)then
         do iesc=1,nesparc
           call which1(ifail,nrow,icurrent,apar1,corapar(iesc))
           if(ifail.ne.0)then
             write(errmsg,551) trim(corapar(iesc)),trim(afile)
551          format('Parameter "',a,'" is a simplicity correction ',
     +       'parameter; however it is not cited in ',
     +       'predictive sensitivity matrix file ',a,'.')
             go to 9890
           end if
           yc(iesc)=y1(icurrent)
         end do
       end if

       write(6,560) trim(afile)
560    format(' - file ',a,' read ok.')
       if(ipass.gt.1) go to 1100

C -- The maximum number of singular values is worked out. This is the minimum of
C    NESPARA and number of non-regularisation observations for which the weight
C    is nonzero.

       nxrow1=0
       do irow=1,nxrow
         if(iregobs(irow).eq.0)then
           itemp=nobgnm(irow)
           if((covind(itemp)).or.(owght(irow).ne.0.0d0))nxrow1=nxrow1+1
         end if
       end do
       nsing=min(nxrow1,nespara)

C -- If any covariance matrices were in the original PEST dataset these
C    are now inverted and the square root of them found.

       maxobs=0
       if(numcov.ne.0)then
         do icov=1,numcov
           if(nobscov(icov).gt.maxobs) maxobs=nobscov(icov)
         end do
       end if
       itemp=max(maxobs,nespar)
       allocate(w1(itemp),stat=ierr)
       if(ierr.ne.0) go to 9200
       if(maxobs.eq.0) go to 584
       if(numcov.ne.0)then
         lwork=8*maxobs
         allocate(workvec(lwork),stat=ierr)
         if(ierr.ne.0) go to 9200
         allocate(u(maxobs,maxobs),stat=ierr)
         if(ierr.ne.0) go to 9200
         write(6,581)
581      format(/,' - undertaking SVD on observation covariance ',
     +   'matrixes...')
         iicount=0
         ldvt=1
         do icov=1,numcov
           itemp=nobscov(icov)
           if(itemp.eq.0) cycle
           iicount=iicount+1
           call dgesvd('A','N',itemp,itemp,covar(icov)%dval,itemp,
     +     w1,u,maxobs,vt_dummy,ldvt,workvec,lwork,info)
           if(info.ne.0)then
             call writint(acov,icov)
             write(errmsg,587) trim(acov)
587          format('Cannot undertake SVD on covariance matrix ',
     +       'number ',a,' cited in PEST control file.')
             go to 9890
           end if
           do i=1,itemp
             if(w1(i).le.1.0d-200)then
               call writint(acov,icov)
               write(errmsg,582) trim(acov)
582            format('Observation covariance matrix number ',a,
     +         ' cited in PEST control file is singular. ',
     +         'PREDVAR1C cannot proceed.')
               go to 9890
             end if
             w1(i)=1.0d0/(sqrt(w1(i)))
           end do
           do j=1,itemp
             do i=1,j
               rtemp=0.0d0
               do k=1,itemp
                 rtemp=rtemp+u(i,k)*u(j,k)*w1(k)
               end do
               covar(icov)%dval(i,j)=rtemp
             end do
           end do
           do j=1,itemp-1
             do i=j+1,itemp
               covar(icov)%dval(i,j)=covar(icov)%dval(j,i)
             end do
           end do
         end do
         call writint(anum,iicount)
         write(6,583) trim(anum)
583      format(' - ',a,' covariance matrices decomposed.')
         deallocate(u,stat=ierr)
       end if
584    continue

C -- Two Jacobian matrices are formed in which all regularisation observations and
C    non-zero-weighted observations are ommitted. The matrices are also split
C    on the basis of whether they contain adjustable or correction parameters.

       allocate(xxa(nxrow1,nespara),xxc(nxrow1,ndimc),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(cories(nespar),stat=ierr)
       if(ierr.ne.0) go to 9200
       cories=0                               ! an array
       ies=0
       do ipar=1,npar
         if(itrans(ipar).ge.0)then
           ies=ies+1
           ipargroup=ipargp(ipar)
           if(corpargp(ipargroup).eq.1)then
             cories(ies)=1
           end if
         end if
       end do

C -- These new matrices are now filled - first where there is no covariance matrix.

       jrow=0
       do irow=1,nxrow
         if(iregobs(irow).ne.0) cycle
         itemp=nobgnm(irow)
         if(.not.covind(itemp).and.(owght(irow).le.0.0d0))cycle
         jrow=jrow+1
         rtemp=sqrt(owght(irow))
         iesa=0
         iesc=0
         do ies=1,nespar
           if(cories(ies).eq.0)then
             iesa=iesa+1
             xxa(jrow,iesa)=x(irow,ies)*rtemp
           else
             iesc=iesc+1
             xxc(jrow,iesc)=x(irow,ies)*rtemp
           end if
         end do
       end do

C -- Now where there are observation covariance matrices.

       if(numcov.ne.0)then
         allocate(iindex(maxobs),stat=ierr)
         if(ierr.ne.0) go to 9200
         do icov=1,numcov
           if(nobscov(icov).eq.0) cycle
           igroup=covgp(icov)
           if(iregobsgp(igroup).ne.0) cycle
           icovrow=0
           do irow=1,nxrow
             if(nobgnm(irow).eq.igroup)then
               icovrow=icovrow+1
               iindex(icovrow)=irow
               if(icovrow.eq.nobscov(icov)) go to 585
             end if
           end do
585        continue
           jrow=0
           icovrow=0
           do irow=1,nxrow
             if(iregobs(irow).ne.0) cycle
             itemp=nobgnm(irow)
             if(.not.covind(itemp).and.(owght(irow).le.0.0d0))cycle
             jrow=jrow+1
             if(nobgnm(irow).eq.igroup)then
               icovrow=icovrow+1
               iesa=0
               iesc=0
               do ies=1,nespar
                 rtemp=0.0d0
                 do k=1,nobscov(icov)
                   rtemp=rtemp+
     +                   covar(icov)%dval(icovrow,k)*x(iindex(k),ies)
                 end do
                 if(cories(ies).eq.1)then
                   iesc=iesc+1
                   xxc(jrow,iesc)=rtemp
                 else
                   iesa=iesa+1
                   xxa(jrow,iesa)=rtemp
                 end if
               end do
               if(icovrow.eq.nobscov(icov)) go to 589
             end if
           end do
589        continue
         end do
         deallocate(iindex,stat=ierr)
       end if

       deallocate(x,stat=ierr)

C -- The parameter uncertainty file is read (twice).

       allocate(cpa(nespara,nespara),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(cpc(ndimc,ndimc),stat=ierr)
       if(ierr.ne.0) go to 9200
       call addquote(uncertfile,afile)
       write(6,570) trim(afile)
570    format(/,' - reading parameter uncertainty file ',a,'....')
       iunit=12
       junit=15
       itype=2
       vardim=1
       atype='parameter'
       covdima=nespara
       call read_uncert_data_file(ifail,iunit,junit,itype,jpartypea,
     + nespara,vardim,covdima,parvar,cpa,atype,uncertfile,errmsg,
     + dline,adjapar)
       if(ifail.ne.0) then
         errmsg=adjustl(errmsg)
         if(index(errmsg,'must be pertinent to').ne.0) go to 9400
         go to 9890
       end if
       if(nesparc.gt.0)then
         rewind(unit=12)
         covdimc=nesparc
         call read_uncert_data_file(ifail,iunit,junit,itype,jpartypec,
     +   nesparc,vardim,covdimc,parvar,cpc,atype,uncertfile,errmsg,
     +   dline,corapar)
         if(ifail.ne.0) then
           errmsg=adjustl(errmsg)
           if(index(errmsg,'must be pertinent to').ne.0) go to 9400
           go to 9890
         end if
       end if
       close(unit=12)

C -- We normalize the covariance matrix file and sensitivities.

       if(nesparc.eq.0)then
         jpartypec=jpartypea
       end if
       if((jpartypea.ne.1).or.(jpartypec.ne.1))then
         write(errmsg,3010)
3010     format('PREDVAR1C requires that the parameter covariance ',
     +   'matrix have no diagonal elements.')
         go to 9890
       end if
       allocate(sda(nespara),sdc(ndimc),stat=ierr)
       if(ierr.ne.0) go to 9200
       atemp30='adjustable'
       do iesa=1,nespara
         dtemp=cpa(iesa,iesa)
         if(dtemp.le.0.0d0) then
           write(errmsg,2582) trim(atemp30)
2582       format('The prior covariance matrix for ',a,
     +     ' parameters has at least one zero-valued diagonal ',
     +     'element.')
           go to 9890
         end if
         dtemp=sqrt(dtemp)
         do irow=1,nxrow1
           xxa(irow,iesa)=xxa(irow,iesa)*dtemp
         end do
         cpa(iesa,iesa)=1.0d0
         sda(iesa)=dtemp
       end do
       atemp30='simplicity-correction'
       if(nesparc.gt.0)then
         do iesc=1,nesparc
           dtemp=cpc(iesc,iesc)
           if(dtemp.le.0.0d0) then
             write(errmsg,2582) trim(atemp30)
             go to 9890
           end if
           dtemp=sqrt(dtemp)
           do irow=1,nxrow1
             xxc(irow,iesc)=xxc(irow,iesc)*dtemp
           end do
           cpc(iesc,iesc)=1.0d0
           sdc(iesc)=dtemp
         end do
       end if

       write(6,580) trim(afile)
580    format(' - parameter uncertainty file ',a,' read ok.')

C -- Now we do SVD on the xxa matrix.

       write(6,300)
300    format(/,' - carrying out singular value decomposition ',
     + 'of Q(1/2)X....')
       if(allocated(workvec))deallocate(workvec)
       allocate(vt(nespara,nespara),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(uu(nxrow1,nsing),stat=ierr)
       if(ierr.ne.0) go to 9200
       lwork=max(5*min(nespara,nxrow1)+max(nespara,nxrow1),
     +         5*min(nespara,nxrow1))
       allocate(workvec(lwork),stat=ierr)
       if(ierr.ne.0) go to 9200
       call dgesvd('S','A',nxrow1,nespara,xxa,nxrow1,w1,uu,
     + nxrow1,vt,nespara,workvec,lwork,info)
       if(info.ne.0)then
         write(errmsg,310)
310      format('Singular value decomposition process did not ',
     +   'converge.')
         go to 9890
       end if
       if(nsing.lt.nespara)then
         do i=nsing+1,nespara
           w1(i)=0.0d0
         end do
       end if
       deallocate(xxa,stat=ierr)

! -- If residuals stats are being computed we calculate xcxct here.

       if(restat.ne.0)then
         write(6,311)
311      format(/,' - calculating ZcZct....')
         allocate (z0z0t(nxrow1,nxrow1),stat=ierr)
         if(ierr.ne.0)then
           write(errmsg,346)
           go to 9890
         end if
         if(nesparc.gt.0)then
           do i=1,nxrow1
             do j=1,i
               sum=0.0d0
               do k=1,nesparc
                 sum=sum+xxc(i,k)*xxc(j,k)
               end do
               z0z0t(i,j)=sum
               z0z0t(j,i)=sum
             end do
           end do
         end if
       end if

C -- A header is written to the output file.

1100   continue

       do iesa=1,nespara
         ya(iesa)=ya(iesa)*sda(iesa)
       end do
       if(nesparc.gt.0)then
         do iesc=1,nesparc
           yc(iesc)=yc(iesc)*sdc(iesc)
         end do
       end if

#ifndef UNIX
       call lowcas(outfile)
#endif
#ifdef LF90
       open(unit=20,file=outfile,action='readwrite,denynone',
     + iostat=ierr)
#else
       open(unit=20,file=outfile,action='write',iostat=ierr)
#endif
       if(ierr.ne.0)then
         call addquote(outfile,afile)
         write(errmsg,230) trim(afile)
230      format('Cannot open file ',a,' for output.')
         go to 9890
       end if
       write(dline,344) trim(apred)
344    format(' Model predictive error variances for prediction "',
     +   a,'" ---->')
       write(20,'(a)') trim(dline)
       nb=len_trim(dline)
       write(20,343)' ',('-',i=1,nb-1)
343    format(a,100(a))
       write(20,345)
345    format(1x,'sing_val',t12,'variance_1',t32,'variance_2',
     + t51,'variance_3',t71,'total_variance',t91,'standard_deviation')
#ifdef FLUSHFILE
       call flush(20)
#endif
       if((restat.ne.0).and.(ipass.eq.1))then
         write(30,3461)
3461     format(1x,'sing_val',t12,'first_term',t30,
     +   'second_term',t48,'third_term',t66,'sum_of_terms')
#ifdef FLUSHFILE
         call flush(30)
#endif
       end if

C -- We now start the singular value cycle.

       if(ipass.eq.1)then
         allocate(w1x(nespar),ytv2(nespar),yy(nespar),stat=ierr)
         if(ierr.ne.0) go to 9200
         allocate(w2(nxrow1),stat=ierr)
         if(ierr.ne.0) go to 9200
         if(restat.eq.1)then
           allocate(u2u2t(nxrow1,nxrow1),resvar1(nxrow1),
     +     resvar2(nxrow1),resvar3(nxrow1),stat=ierr)
           if(ierr.ne.0)then
             write(errmsg,346)
346          format('Insufficient memory available for computation ',
     +       'of residuals stats.')
             go to 9890
           end if
         end if
       else
          if(restat.eq.1)then
            deallocate(u2u2t,resvar1,resvar2,resvar3,z0z0t,stat=ierr)
          end if
       end if

       do jsing=1,numsing
         call writint(asing,ising(jsing))

         write(6,350) trim(asing)
350      format(/,' - computing error variance terms for truncation ',
     +   'at ',a,' singular values  ....')

         if(ising(jsing).gt.nespara)then
           call addquote(outfile,bfile)
           write(errmsg,351) trim(bfile)
351        format('Cannot compute error variance for this number ',
     +     'of singular values; this number ',
     +     'of singular values exceeds the number of adjustable ',
     +     '(i.e. non-simplicity-correction) parameters. See file ',
     +     a,' however for outputs pertaining to singular ',
     +     'values processed before this.')
           errmsg=' '//trim(errmsg)
           call writmess(6,errmsg)
           go to 621
         end if

C -- The first term is formed

         write(6,352)
352      format(' - computing first term ....')

         if(ising(jsing).ge.nespara)then
           first=0.0d0
         else
           j=0
           do i=ising(jsing)+1,nespara
             j=j+1
             rtemp=0.0d0
             do k=1,nespara
               rtemp=rtemp+ya(k)*vt(i,k)
             end do
             ytv2(j)=rtemp
           end do
           do i=1,nespara
             j=0
             rtemp=0.0d0
             do k=ising(jsing)+1,nespara
               j=j+1
               rtemp=rtemp+ytv2(j)*vt(k,i)
             end do
             yy(i)=rtemp
           end do
           first=0.0d0
           do i=1,nespara
             first=first+yy(i)*yy(i)*cpa(i,i)
           end do
         end if

         write(6,353)
353      format(' - computing second term ....')

C -- ytv1 is formed.

         if(ising(jsing).eq.0)then
           second=0.0d0
         else
           if(ising(jsing).gt.nespara)ising(jsing)=nespara
           do i=1,ising(jsing)
             if(w1(i).le.1.0e-17)then
               second=1.0e35
               go to 421
             end if
             w1x(i)=1.0/w1(i)
           end do
           do i=1,ising(jsing)
             rtemp=0.0d0
             do k=1,nespara
               rtemp=rtemp+ya(k)*vt(i,k)
             end do
             ytv2(i)=rtemp
           end do
           second=0.0d0
           do i=1,ising(jsing)
             second=second+ytv2(i)*w1x(i)*w1x(i)*ytv2(i)
           end do
         end if
         second=second*refvar
421      continue

C -- The third term is calculated.

         write(6,422)
422      format(' - computing third term ....')

         if(nesparc.eq.0)then
           third=0.0d0
           go to 2400
         end if
         if(ising(jsing).eq.0)then
           do iesc=1,nesparc
             yy(iesc)=-yc(iesc)
           end do
         else
           if(ising(jsing).gt.nespara)ising(jsing)=nespara
           do i=1,ising(jsing)
             if(w1(i).le.1.0e-17)then
               third=1.0e35
               go to 2400
             end if
             w1x(i)=1.0/w1(i)
           end do
           do i=1,ising(jsing)
             ytv2(i)=ytv2(i)*w1x(i)
           end do
           do i=1,nxrow1
             rtemp=0.0d0
             do j=1,ising(jsing)
               rtemp=rtemp+ytv2(j)*uu(i,j)
             end do
             w2(i)=rtemp
           end do
           do iesc=1,nesparc
             rtemp=0.0d0
             do j=1,nxrow1
               rtemp=rtemp+w2(j)*xxc(j,iesc)
             end do
             yy(iesc)=rtemp-yc(iesc)
           end do
         end if
         third=0.0d0
         do iesc=1,nesparc
           third=third+yy(iesc)*yy(iesc)*cpc(iesc,iesc)
         end do
2400     continue

C -- The fourth term is calculated

         fourth=first+second+third

C -- Data is written to the output file.

         write(20,620) ising(jsing),first,second,third,fourth,
     +   sqrt(fourth)
620      format(i6,t10,1pg14.7,t30,1pg14.7,t50,1pg14.7,t70,
     +   1pg14.7,t90,1pg14.7)
#ifdef FLUSHFILE
         call flush(20)
#endif

C -- If residual stats are calculated this is now done.

         if((restat.eq.1).and.(ipass.eq.1))then

C -- We calculate the vector of residual variances. This has three terms.
C -- Note that this really has to be done only for the first prediction as it is prediction-independent.

           write(6,1210)
1210       format(' - computing residuals stats....')

C -- The first residual term

           resvar1=0.0d0                              ! an array
           resvar2=0.0d0                              ! an array
           resvar3=0.0d0                              ! an array
           if(ising(jsing).lt.nsing)then
             do irow=1,nxrow1
               sum=0.0d0
               do k=ising(jsing)+1,nsing
                 if(w1(k).gt.0.0d0)then
                   dtemp=uu(irow,k)*w1(k)
                   sum=sum+dtemp*dtemp
                 end if
               end do
               resvar1(irow)=sum
             end do
           end if

C -- The second residual term

           if(ising(jsing).eq.0)then
             do irow=1,nxrow1
               resvar2(irow)=1.0d0
             end do
           else if(ising(jsing).ge.nxrow1)then
             continue
           else
             do irow=1,nxrow1
               do jrow=1,irow
                 sum=0.0d0
                 do k=1,ising(jsing)
                   sum=sum+uu(irow,k)*uu(jrow,k)
                 end do
                 u2u2t(irow,jrow)=-sum
                 u2u2t(jrow,irow)=-sum
               end do
             end do
             do irow=1,nxrow1
               u2u2t(irow,irow)=1.0+u2u2t(irow,irow)
               resvar2(irow)=u2u2t(irow,irow)
             end do
           end if

C -- The third residual term

           if(nesparc.eq.0)then
             do irow=1,nxrow1
               resvar3(irow)=0.0d0
             end do
           else
             if(ising(jsing).eq.0)then
               do irow=1,nxrow1
                 resvar3(irow)=z0z0t(irow,irow)
               end do
             else if(ising(jsing).ge.nxrow1)then
               continue
             else
               do irow=1,nxrow1
                 sum=0.0d0
                 do jrow=2,nxrow1
                   do krow=1,jrow-1
                     sum=sum+u2u2t(irow,jrow)*u2u2t(irow,krow)*
     +                   z0z0t(jrow,krow)
                   end do
                 end do
                 sum=sum*2.0d0
                 do jrow=1,nxrow1
                   sum=sum+u2u2t(irow,jrow)*u2u2t(irow,jrow)*
     +                   z0z0t(jrow,jrow)
                 end do
                 resvar3(irow)=sum
               end do
             end if
           end if

C -- Data is written to the residuals output file.

           sum=0.0d0
           do irow=1,nxrow1
             sum=sum+resvar1(irow)
           end do
           sum1=sum/nxrow1
           sum=0.0d0
           do irow=1,nxrow1
             sum=sum+resvar2(irow)
           end do
           sum2=sum/nxrow1
           sum=0.0d0
           do irow=1,nxrow1
             sum=sum+resvar3(irow)
           end do
           sum3=sum/nxrow1
           write(30,619) ising(jsing),sum1,sum2,sum3,sum1+sum2+sum3
619        format(i6,t12,1pg14.7,t30,1pg14.7,t48,1pg14.7,t66,1pg14.7)
#ifdef FLUSHFILE
           call flush(30)
#endif
         end if

621      continue
       end do

       close(unit=20)
       call addquote(outfile,afile)
       write(6,800) trim(afile)
800    format(/,' - predictive variance file ',a,' written ok.')
       if((restat.ne.0).and.(ipass.eq.1))then
         call addquote(resfile,afile)
         write(6,801) trim(afile)
801      format(/,' - residuals statistics file ',a,' written ok.')
         close(unit=30)
       end if

       go to 1010

1200   continue

       go to 9900

9000   write(errmsg,9010) trim(afile)
9010   format('Error encountered in reading Jacobian matrix ',
     + 'file ',a,'.')
       go to 9890
9050   call addquote(pestfile,bfile)
       write(errmsg,9060) trim(afile),trim(bfile)
9060   format('Jacobian matrix file ',a,' is not compatible ',
     + 'with PEST control file ',a,'; at least one observation ',
     + 'or adjustable parameter is different between them.')
       go to 9890
9100   write(errmsg,9110) trim(afile)
9110   format('Error encountered in reading predictive sensitivity ',
     + 'matrix file ',a,'.')
       go to 9890
9150   write(errmsg,9160) trim(afile)
9160   format('Unexpected end encountered to predictive sensitivity ',
     + 'matrix file ',a,'.')
       go to 9890

9200   write(errmsg,9210)
9210   format('Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9890

9250   write(errmsg,9260) trim(afile)
9260   format('Matrix file ',a,' either does not have a "* row names"',
     + ' header, or this header is in the wrong place in this file.')
       go to 9890
9300   write(errmsg,9310) trim(afile)
9310   format('Matrix file ',a,' either does not have a ',
     + '"* column names" header, or this header is in the wrong ',
     + 'place in this file.')
       go to 9890

9400   write(errmsg,9410) trim(afile)
9410   format('Information in the uncertainty file ',a,' indicates ',
     + 'correlation between adjustable and correction parameters ',
     + 'and/or between either of these and fixed, tied or unknown ',
     + 'parameters. This is not allowed.')
       go to 9890

9890   continue
       errmsg=' '//trim(errmsg)
       call writmess(6,errmsg)


9900   continue
       call pest_data_deallocate(ifail)

       deallocate(iindex,stat=ierr)
       deallocate(iregobs,iregobsgp,stat=ierr)
       deallocate(numinpargp,corpargp,stat=ierr)
       deallocate(cories,stat=ierr)
       deallocate(ytv2,stat=ierr)
       deallocate(u,stat=ierr)
       deallocate(workvec,w1,y1,vt,w1x,yy,stat=ierr)
       deallocate(ya,yc,stat=ierr)
       deallocate(xxa,xxc,stat=ierr)
       deallocate(uu,stat=ierr)
       deallocate(cpa,cpc,stat=ierr)
       deallocate(w2,stat=ierr)
       deallocate(apar1,adjapar,corapar,stat=ierr)
       deallocate(sda,sdc,stat=ierr)
       deallocate( z0z0t,u2u2t,stat=ierr)
       deallocate(resvar1,resvar2,resvar3,stat=ierr)

       close(unit=10,iostat=ierr)
       close(unit=11,iostat=ierr)
       close(unit=12,iostat=ierr)
       close(unit=13,iostat=ierr)
       close(unit=15,iostat=ierr)
       close(unit=20,iostat=ierr)
       close(unit=21,iostat=ierr)
       close(unit=30,iostat=ierr)

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



        SUBROUTINE NUMRD(IFAIL,ITYPE,LW,RW,IVAL,RVAL,CLINE)

C -- SUBROUTINE NUMRD READS A NUMBER FROM PART OF A STRING

        INTEGER IFAIL,RW,LW,LN,IVAL
        INTEGER ITYPE
        DOUBLE PRECISION RVAL
        CHARACTER*(*) CLINE
        CHARACTER*10 FMT

        IFAIL=0
        LN=RW-LW+1
        IF(ITYPE.EQ.0) THEN
          WRITE(FMT,10) LN
10        FORMAT('(I',I2,')')
          READ(CLINE(LW:RW),FMT,ERR=50) IVAL
        ELSE IF(ITYPE.EQ.1) THEN
          WRITE(FMT,20) LN
20        FORMAT('(F',I2,'.0)')
          READ(CLINE(LW:RW),FMT,ERR=50) RVAL
        END IF
        RETURN

50      IFAIL=1
        RETURN

        END


        SUBROUTINE WHICH1(IFAIL,NPAR,IPAR,APAR,TPAR)

C -- SUBROUTINE WHICH1 LOCATES A STRING IN AN ARRAY

        INTEGER NPAR,IPAR,I
        INTEGER IFAIL
        CHARACTER*(*) TPAR
        CHARACTER*(*) APAR(NPAR)

        IFAIL=0
        IF((IPAR.LT.1).OR.(IPAR.GT.NPAR)) IPAR=1
        CALL LOWCAS(TPAR)
        IF(TPAR.EQ.APAR(IPAR)) RETURN
        IF(IPAR.NE.NPAR)THEN
          DO 20 I=IPAR+1,NPAR
          IF(TPAR.EQ.APAR(I))THEN
            IPAR=I
            RETURN
          END IF
20        CONTINUE
        END IF
        IF(IPAR.NE.1)THEN
          DO 40 I=IPAR-1,1,-1
          IF(TPAR.EQ.APAR(I)) THEN
            IPAR=I
            RETURN
          END IF
40        CONTINUE
        END IF
        IFAIL=1
        RETURN

        END



        SUBROUTINE UPCAS(ASTRNG)

C -- SUBROUTINE UPCAS CONVERTS A STRING TO UPPER CASE

        INTEGER I,J
        CHARACTER*(*) ASTRNG

        DO 10 I=1,LEN_TRIM(ASTRNG)
        J=ICHAR(ASTRNG(I:I))
        IF((J.GE.97).AND.(J.LE.122)) ASTRNG(I:I)=CHAR(J-32)
10      CONTINUE
        RETURN

        END


        SUBROUTINE GETINT(IFAIL,BUF,N1,N2,NBLB,MRKDEL)

C -- SUBROUTINE GETINT GETS THE NEXT STORED INSTRUCTION FOR PROCESSING

        INTEGER N1,N2,NBLB,I,II
        INTEGER IFAIL
        CHARACTER MRKDEL
        CHARACTER*(*) BUF

        IFAIL=0
        IF(N2.GE.NBLB) THEN
          N1=0
          RETURN
        END IF
        DO 10 I=N2+1,NBLB
        IF((BUF(I:I).NE.' ').AND.(ICHAR(BUF(I:I)).NE.9)) GO TO 50
10      CONTINUE
        N1=0
        RETURN
50      N1=I
        IF(BUF(N1:N1).NE.MRKDEL)THEN
          I=INDEX(BUF(N1:NBLB),' ')
          II=INDEX(BUF(N1:NBLB),CHAR(9))
          IF((I.EQ.0).AND.(II.EQ.0))THEN
            I=0
          ELSE IF(I.EQ.0)THEN
            I=II
          ELSE IF(II.EQ.0) THEN
            I=I
          ELSE
            I=MIN(I,II)
          END IF
          IF(I.NE.0) THEN
            N2=N1+I-2
          ELSE
            N2=NBLB
          END IF
        ELSE
          IF(N1.EQ.NBLB)THEN
            IFAIL=1
            RETURN
          END IF
          I=INDEX(BUF(N1+1:NBLB),MRKDEL)
          IF(I.EQ.0) THEN
            IFAIL=1
            RETURN
          END IF
          N2=N1+I
        END IF

        RETURN

        END

