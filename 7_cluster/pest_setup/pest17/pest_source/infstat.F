       program infstat

C -- Program INFSTAT calculates Cooks D and DFBeta

       use pestdata, only  : maxcompdim,pestmode,nobsgp,nxrow,
     +                       errmsg,obgnme,aobs,numcov,
     +                       covind,owght,x,pval_o,modval_o,
     +                       parchglim,pval,parlbnd,parubnd,
     +                       ipargp,scale,offset,dercom,covgp,nespar,
     +                       nobs,nobgnm,covar,nobscov,apar,itrans,
     +                       npar,svdmode,svda,numlam,rlambda1,nprior,
     +                       lsqrmode

       implicit none

       logical          :: lexist
       integer          :: ifail,n,n1,ierr,irestart,itemp1,itemp2,bn,
     +                     icount,ies,jes,j,i,irow,iline,maxnobscov,
     +                     icov,lwork,nrow,ncol,icol,k,jj,numnonzero,
     +                     info,ii,n_p,ipar,itemp,ipp,numrec,maxrow,
     +                     irec,ibad,nsig,r_numnz,m_numnz,nlist90,
     +                     nlist80,ipass,maxival,idec
       integer          :: nespar1,nxrow1,nobsgp1,icountregobs,
     +                     icountregnz,ireg,ireg1,svdmode1,svda1,
     +                     numeiglst,imcovflag,lsqrmode1
       integer             p1start,pfile
       integer          :: lw(10),rw(10)
       double precision :: dtemp,phi,dtemp1,dtemp2,invrefvar,refvar,
     +                     sum,lamlst,alphlst,phim,phir,critval,
     +                     mean_ha,std_ha,maxval
       double precision :: u(1,1),vt(1,1)
       character*5      :: aext
       character*10     :: aversion
       character*10     :: aline,anum,atemp1
       character*12     :: aapar
       character*20     :: aaobs
       character*25     :: atemp
       character*200    :: casename,pestfile,apestfile,afile,outfile,
     +                     resfile,jcofile,rsdfile
       character*300    :: dline,eline
       character*2000   :: fline


       integer, allocatable          :: rownum(:),irgp(:),icpar(:),
     + nrec(:)
       double precision, allocatable :: resid(:),tempcov(:,:),s(:),
     +                                  workvec(:),qtq(:,:),h(:),d(:),
     +                                  den(:),ha(:),ibsum(:),
     +                                  d1(:),x_o(:,:)


C -- Initialization

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' INFSTAT Version ',a,'. Watermark Numerical ',
     + 'Computing.')


#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) dline
#else
       call pgetcl(dline)
#endif

C -- The casename is read from the INFSTAT command line.

       if(dline.eq.' ') go to 8900
       call spacesub(dline)
       call linspl(ifail,4,lw,rw,dline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,3,lw,rw,dline)
       if(ifail.eq.0) then
         atemp1=dline(lw(3):rw(3))
         call intread(ifail,atemp1,nsig)
         if(ifail.ne.0) go to 8900
         if(nsig.lt.1) go to 8900
       else
         nsig=-9999
       end if
       call linspl(ifail,2,lw,rw,dline)
       if(ifail.ne.0) go to 8900
       casename=dline(lw(1):rw(1))
       call remchar(casename,char(211))
       n=len_trim(casename)
       n1=n-3
       if(n1.lt.1)n1=1
       aext=casename(n1:)
       call lowcas(aext)
       if(aext.eq.'.pst ')casename(n1:n)=' '
       if(casename.eq.' ') go to 8900
#ifndef UNIX
       call lowcas(casename)
#endif

C -- The PEST control file is opened.

       pestfile=trim(casename)//'.pst'
       call addquote(pestfile,apestfile)
       inquire(file=pestfile,exist=lexist)
       if(.not.lexist)then
         write(errmsg,11) trim(apestfile)
11       format('Cannot find PEST control file ',a,'.')
         go to 9890
       end if

C -- The residuals file is opened.

       resfile=trim(casename)//'.res'
       open(unit=12,file=resfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(resfile,afile)
         write(errmsg,15) trim(afile),trim(apestfile)
15       format(' Cannot open residuals file ',a,' corresponding ',
     +   'to PEST control file ',a,'.')
         go to 9890
       end if

C -- The JCO file is opened.

       jcofile=trim(casename)//'.jco'
       call open_unformatted_file(ifail,11,'read',jcofile,errmsg)
       if(ifail.ne.0)then
         call addquote(jcofile,afile)
         write(errmsg,60) trim(afile),trim(apestfile)
60       format('Cannot open Jacobian matrix file ',a,
     +   ' corresponding to PEST control file ',a,'.')
         go to 9890
       end if

C -- The INFSTAT output file is opened.

       outfile=dline(lw(2):rw(2))
       call remchar(outfile,char(211))
       open(unit=20,file=outfile,action='write',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(outfile,afile)
         write(errmsg,40) trim(afile)
40       format('Cannot open file ',a,' for output.')
         go to 9890
       end if

C -- All input data has now been acquired. The complete PEST input
C    dataset is now read.

       write(6,*)
       write(6,235) trim(apestfile)
235    format(' - reading PEST control file ',a,'....')
       irestart=0
       p1start=0
       pfile=0
       call read_pest_data(ifail,irestart,p1start,pfile,pestfile)
       if(ifail.ne.0) go to 9890
       if(maxcompdim.gt.1)then
         write(errmsg,240) trim(apestfile)
240      format('INFSTAT requires that MAXCOMPDIM be set to 1 ',
     +   'or less in PEST control file ',a,'.')
         go to 9890
       end if
       if(pestmode.eq.2)then
         write(errmsg,250) trim(apestfile)
250      format('INFSTAT requires that PEST be run in ',
     +   'estimation or regularisation mode in PEST control ',
     +   'file ',a,'.')
         go to 9890
       end if
!rh       if(svdmode.ne.0)then
!rh         write(errmsg,251) trim(apestfile)
!rh251      format('INFSTAT cannot calculate influence statistics as ',
!rh     +   'PEST''s singular value decomposition functionality ',
!rh     +   'was activated in previous PEST run based on PEST ',
!rh     +   'control file ',a,'.')
!rh         go to 9890
!rh       end if
       if(nespar.eq.0)then
         write(errmsg,272) trim(apestfile)
272      format('There are no adjustable parameters cited in PEST ',
     +   'control file ',a,'.')
         go to 9890
       end if
       if(nxrow.eq.0)then
         write(errmsg,273) trim(apestfile)
273      format('No observations or prior information items are ',
     +   'cited in PEST control file ',a,'.')
         go to 9890
       end if
       write(6,275) trim(apestfile)
275    format(' - PEST control file ',a,' read ok.')

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
       if(itemp2.ne.nxrow) go to 9050
       do i=1,nespar
         do j=1,nxrow
           x(j,i)=0.0d0
         end do
       end do
       read(11,err=9000,end=9000)icount
       do i=1,icount
         read(11,err=9000,end=9000) j,dtemp
         ies=(j-1)/bn+1
         irow=j-(ies-1)*bn
         if(nsig.ne.-9999) call sigtruncate(dtemp,nsig)
         x(irow,ies)=dtemp
       end do
       do i=1,npar
         if(itrans(i).lt.0) cycle
         read(11,err=9000,end=9000) aapar
         call lowcas(aapar)
         if(aapar.ne.apar(i)) go to 9050
       end do
       do i=1,bn
         read(11,err=9000,end=9000) aaobs
         call lowcas(aaobs)
         if(aaobs.ne.aobs(i)) go to 9050
       end do
       close(unit=11)
       write(6,300) trim(afile)
300    format(' - Jacobian matrix file ',a,' read ok.')

C -- If PEST was run in regularision mode the RSD file is read for
C    optimised weights.

       allocate(resid(nxrow),irgp(nobsgp),icpar(nxrow),
     + ibsum(nxrow),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(x_o(nxrow,nespar),stat=ierr)
       if(ierr.ne.0) go to 9200
       maxnobscov=0
       do icov=1,numcov
         if(nobscov(icov).gt.maxnobscov)then
           maxnobscov=nobscov(icov)
         end if
       end do
       lwork=8*maxnobscov
       itemp=max(lwork,nxrow)
       itemp=max(itemp,nespar)
       allocate(workvec(itemp),stat=ierr)
       if(ierr.ne.0) go to 9200
       itemp=max(nespar,nxrow)
       allocate(nrec(itemp),stat=ierr)
       if(ierr.ne.0) go to 9200
       nrec=0        ! an array

       if(pestmode.eq.3)then
         rsdfile=trim(casename)//'.rsd'
         call addquote(rsdfile,afile)
         write(6,705) trim(afile)
705      format(/,' - reading resolution data file ',a,'....')
         call open_unformatted_file(ifail,11,'read',rsdfile,errmsg)
         if(ifail.ne.0)then
           write(errmsg,700) trim(afile),trim(apestfile)
700        format('Cannot open resolution data file ',a,
     +     ' corresponding to PEST control file ',a,'.')
           go to 9890
         end if
         read(11,err=9400,end=9400) nespar1,nxrow1,nobsgp1
         if((nespar1.ne.nespar).or.(nxrow1.ne.nxrow).or.
     +     (nobsgp1.ne.nobsgp))then
           write(errmsg,720) trim(afile),
     +     trim(apestfile)
720        format('Resolution data file ',a,' appears to be ',
     +     'incompatible with PEST control file ',a,'.')
           go to 9890
         end if
         read(11,err=9400,end=9400) icountregobs,icountregnz
         read(11,err=9400,end=9400) ireg1,svdmode1,svda1,
     +   lsqrmode1
         ireg=0
         if(pestmode.eq.3)ireg=1
         if(ireg1.ne.ireg)then
           write(errmsg,730) trim(apestfile),trim(afile)
730        format('PESTMODE setting in PEST control file ',a,
     +     ' differs from that recorded in resolution data file ',a,'.')
           go to 9890
         end if
         if(svdmode1.ne.svdmode)then
           write(errmsg,740) trim(apestfile),trim(afile)
740        format('SVDMODE setting in PEST control file ',a,
     +     ' differs from that recorded in resolution data file ',a,'.')
           go to 9890
         end if
         if(lsqrmode1.ne.lsqrmode)then
           write(errmsg,741) trim(apestfile),trim(afile)
741        format('LSQRMODE setting in PEST control file ',a,
     +     ' differs from that recorded in resolution data file ',a,'.')
           go to 9890
         end if
         if(svda1.ne.svda)then
           write(errmsg,750) trim(apestfile),trim(afile)
750        format('SVD-assist setting in PEST control file ',a,
     +     ' differs from that recorded in resolution data file ',a,'.')
           go to 9890
         end if
         read(11,err=9400,end=9400) numlam,rlambda1,lamlst,alphlst,
     +   numeiglst
         read(11,err=9400,end=9400) imcovflag
         read(11,err=9400,end=9400) phim, phir

C -- The (rotated) weight matrix is read.

         read(11,err=9400,end=9400) (workvec(i),i=1,nxrow)

C -- The observation group indicator matrix is read.

         read(11,err=9400,end=9400) (nobgnm(i),i=1,nxrow)

C -- The observation group regularisation indicator array is read.

         read(11,err=9400,end=9400) (irgp(i),i=1,nobsgp)

         close(unit=11)
         write(6,760) trim(afile)
760      format(' - resolution data file ',a,' read ok.')

         if(numcov.ne.0)then
           do icov=1,numcov
             ii=covgp(icov)
             if(irgp(ii).ne.0)then
               write(errmsg,770) trim(apestfile)
770            format('As presently programmed INFSTAT will not ',
     +         'allow a covariance matrix to be assigned to a ',
     +         'regularisation group in PEST control file ',a,'.')
               go to 9890
             end if
           end do
         end if
         do irow=1,nxrow
           ii=nobgnm(irow)
           if(irgp(ii).ne.0) owght(irow)=workvec(irow)
         end do

       end if

C -- We can delete some unused PEST memory.

       deallocate(pval_o,modval_o,parchglim,pval,parlbnd,parubnd,
     + ipargp,scale,offset,dercom,stat=ierr)

C -- The residuals file is now read.

       write(6,301) trim(apestfile)
301    format(/,' - reading residuals file ',a,'....')

       iline=1
       read(12,'(a)',err=9300,end=9300) dline
       dline=adjustl(dline)
       if(dline(1:4).ne.'Name') then
         call addquote(resfile,afile)
         write(errmsg,330) trim(afile)
330      format('File ',a,' does not appear to be a PEST-produced ',
     +   'residuals file.')
         go to 9890
       end if
       do irow=1,nxrow
         iline=iline+1
         read(12,*,err=9300,end=9300) aaobs,atemp,dtemp1,dtemp2
         resid(irow)=dtemp1-dtemp2
         call lowcas(aaobs)
         if(aaobs.ne.aobs(irow))then
           write(6,331) trim(aaobs),trim(aobs(irow)),irow    !debug
331        format('aaobs = ',a,'  aobs(irow) = ',a,' irow = ',i6)           !debug
           call addquote(resfile,afile)
           write(errmsg,340) trim(afile),trim(apestfile)
340        format('Residuals file ',a,' does not cite same ',
     +     'observations and/or prior information items in same ',
     +     'order as PEST control file ',a,'.')
           go to 9890
         end if
       end do
       close(unit=12)
       call addquote(resfile,afile)
       write(6,350) trim(afile)
350    format(' - residuals file ',a,' read ok.')

C -- If any covariance matrices are supplied, C**(-1/2) is calculated.

C -- First we find the biggest covariance matrix and dimension a work matrix appropriately.

       if((numcov.eq.0).or.(maxnobscov.eq.0)) go to 450
       itemp=max(maxnobscov,nespar)
       allocate(tempcov(maxnobscov,itemp),s(maxnobscov),
     + stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(rownum(maxnobscov),stat=ierr)
       if(ierr.ne.0) go to 9200

C -- Now SVD is undertaken and the inverse square root of each matrix is calculated.

       do icov=1,numcov
         nrow=nobscov(icov)
         if(nrow.eq.0) cycle
         ncol=nrow
         do icol=1,ncol
           do irow=1,nrow
             tempcov(irow,icol)=covar(icov)%dval(irow,icol)
           end do
         end do
         call dgesvd('O','N',nrow,ncol,tempcov,maxnobscov,s,u,
     +   1,vt,1,workvec,lwork,info)
         if(info.ne.0)then
           call writint(anum,icov)
           write(errmsg,310) trim(anum),trim(apestfile)
310        format('Singular value decomposition process did not ',
     +     'converge for covariance matrix number ',a,' cited in ',
     +     'PEST control file ',a,'.')
           go to 9890
         end if
         do irow=1,nrow
           if(s(irow).le.1.0d-30)then
             call writint(anum,icov)
             write(errmsg,320) trim(anum),trim(apestfile)
320          format('Covariance matrix ',a,' supplied in PEST control ',
     +       'file ',a,' is singular.')
             go to 9890
           else
             s(irow)=sqrt(1.0d0/s(irow))
           end if
         end do
         do icol=1,ncol
           do irow=1,icol
             dtemp=0.0d0
             do k=1,ncol
               dtemp=dtemp+tempcov(irow,k)*s(k)*tempcov(icol,k)
             end do
             covar(icov)%dval(irow,icol)=dtemp
           end do
         end do
         do icol=1,ncol-1
           do irow=icol+1,ncol
             covar(icov)%dval(irow,icol)=covar(icov)%dval(icol,irow)
           end do
         end do
       end do

C -- Rotated residuals are now formed. Also the submatrix of the x matrix is rotated.

       do icov=1,numcov
         jj=covgp(icov)
         i=0
         do irow=1,nxrow
           ii=nobgnm(irow)
           if(ii.eq.jj)then
             i=i+1
             if(i.gt.nobscov(icov))then
               call writint(anum,icov)
               write(errmsg,351) trim(obgnme(ii)),trim(apestfile)
351            format('Observation group "',a,'" has more members ',
     +         'than there are dimensions in the covariance matrix ',
     +         'assigned to it in PEST control file ',a,'.')
               go to 9890
             end if
             workvec(i)=resid(irow)
             owght(irow)=1.0d0
             rownum(i)=irow
             do j=1,nespar
               tempcov(i,j)=x(irow,j)
             end do
           end if
         end do
         do i=1,nobscov(icov)
           irow=rownum(i)
           dtemp=0.0d0
           do k=1,nobscov(icov)
             dtemp=dtemp+covar(icov)%dval(i,k)*workvec(k)
           end do
           resid(irow)=dtemp
           do ies=1,nespar
             dtemp=0.0d0
             do k=1,nobscov(icov)
               dtemp=dtemp+covar(icov)%dval(i,k)*tempcov(k,ies)
             end do
             x(irow,ies)=dtemp
           end do
         end do
       end do

450    continue

C -- The total objective function is now formed.

       phi=0.0d0
       do irow=1,nxrow
         phi=phi+resid(irow)*resid(irow)*owght(irow)
         resid(irow)=resid(irow)*sqrt(owght(irow))
       end do

C -- The number of observations and prior information equations with non-zero weight
C    is found.

       numnonzero=0
       do irow=1,nxrow
         if(owght(irow).gt.0.0d0) numnonzero=numnonzero+1
       end do

C -- If PEST is running in regularisation mode we count number of regularisation
C    constraints with non zero weight and number of measurements with non zero weight.

       if(pestmode.eq.3)then
         do irow=1,nxrow
           ii=nobgnm(irow)
           if(irgp(ii).ne.0)then
             if(owght(irow).gt.0.0d0) r_numnz=r_numnz+1
           else
             if(owght(irow).gt.0.0d0) m_numnz=m_numnz+1
           end if
         end do
       end if

C -- And now the reference variance.

       if(numnonzero-nespar.le.0)then
         write(errmsg,460) trim(apestfile)
460      format('The number of non-zero-weighted observations and ',
     +   'prior information items in file ',a,' must exceed the ',
     +   'number of observations.')
         go to 9890
       end if
       refvar=phi/(numnonzero-nespar)

C -- The Q matrix is formed.

       allocate(qtq(nespar,nespar),h(nxrow),d(nxrow),ha(nxrow),
     + d1(nxrow),stat=ierr)
       if(ierr.ne.0) go to 9200

       do irow=1,nxrow
         dtemp=sqrt(owght(irow))
         do ies=1,nespar
           x(irow,ies)=x(irow,ies)*dtemp
         end do
       end do

C -- QtQ is now formed.

       do ies=1,nespar
         do jes=1,ies
           dtemp=0.0d0
           do k=1,nxrow
             dtemp=dtemp+x(k,ies)*x(k,jes)
           end do
           qtq(jes,ies)=dtemp
         end do
       end do
       do ies=1,nespar-1
         do jes=ies+1,nespar
           qtq(jes,ies)=qtq(ies,jes)
         end do
       end do

C -- And now it is inverted.

       call dpofa(qtq,nespar,nespar,info)
       if(info.ne.0)then
         write(errmsg,470)
470      format('Cannot invert QtQ matrix for current parameter ',
     +   'estimation problem.')
         go to 9890
       end if
       info=1
       call dpodi(qtq,nespar,nespar,workvec,info)
       do ies=1,nespar-1
         do jes=ies+1,nespar
           qtq(jes,ies)=qtq(ies,jes)
         end do
       end do

C -- We now form a vector which is in fact the diagonal of the h matrix.

       do irow=1,nxrow
         dtemp=0.0d0
         do ies=1,nespar
           dtemp1=x(irow,ies)
           dtemp2=0.0d0
           do jes=1,nespar
             dtemp2=dtemp2+qtq(ies,jes)*x(irow,jes)
           end do
           dtemp=dtemp+dtemp2*dtemp1
         end do
         h(irow)=dtemp
       end do

C -- We now form D

       invrefvar=1.0d0/refvar
       do irow=1,nxrow
         dtemp=1.0d0-h(irow)
         dtemp1=resid(irow)*resid(irow)
         if(abs(dtemp).gt.0.0d0)then
           ha(irow)=nespar/dtemp*dtemp1/(phi-dtemp1) +
     +     h(irow)/dtemp
         else
           ha(irow)=0.0d0
         end if
         dtemp=dtemp*dtemp
         if(dtemp.gt.0.0d0)then
         d(irow)=resid(irow)*resid(irow)*h(irow)*invrefvar/dtemp/nespar
         else
         d(irow)=0.0d0
         end if
       end do

C -- A header is written to the output file.

       write(20,901)
901    format(' Inversion problem specifications ----->',/)
       call writint(atemp,npar)
       write(20,911) trim(atemp)
911    format('   Number of parameters',t73,': ',a)
       call writint(atemp,nespar)
       write(20,921) trim(atemp)
921    format('   Number of adjustable parameters',t73,': ',a)
       call writint(atemp,nobs)
       write(20,930) trim(atemp)
930    format('   Number of observations',t73,': ',a)
       call writint(atemp,nprior)
       write(20,940) trim(atemp)
940    format('   Number of prior information equations',t73,': ',a)
       call writint(atemp,nxrow)
       write(20,950) trim(atemp)
950    format('   Number of observations plus prior information ',
     + 'equations ',t73,': ',a)
       call writint(atemp,numnonzero)
       write(20,960) trim(atemp)
960    format('   Number of observations plus prior info. eqns. with ',
     + 'non-zero weight',t73,': ',a)
       if(pestmode.eq.3)then
         call writint(atemp,m_numnz)
         write(20,961) trim(atemp)
961      format('   Number of non-zero-weighted ',
     +   'measurements/prior info',t73,': ',a)
         call writint(atemp,r_numnz)
         write(20,962) trim(atemp)
962      format('   Number of non-zero-weighted regularization ',
     +   'constraints',t73,': ',a)
       end if
       write(20,*)
       write(20,*)

C -- Data is written to the output file.

       write(20,490)
490    format(' Cook''s D ----->',/)
       write(20,500)
500    format(' Observation_name',t25,'Stud_res',t41,'Leverage',t57,
     + 'var(h)/var(f)',t73,'Cooks_D',t89,'Hadis_stat')
       do irow=1,nxrow
         if(numcov.eq.0)then
           atemp=aobs(irow)
         else
           ii=nobgnm(irow)
           do i=1,numcov
             if(ii.eq.covgp(i))then
               atemp=trim(aobs(irow))//'_'
               go to 510
             end if
           end do
           atemp=aobs(irow)
         end if
510      continue
         dtemp=1.0d0-h(irow)
         dtemp1=refvar*dtemp
         if(dtemp1.gt.0.0d0)then
           dtemp1=resid(irow)/sqrt(dtemp1)
         else
           dtemp1=0.0d0
         end if
         if(dtemp.gt.0.0d0)then
           dtemp2=h(irow)/dtemp
         else
           dtemp2=0.0d0
         end if
         write(20,520) trim(atemp),dtemp1,h(irow),dtemp2,d(irow),
     +   ha(irow)
520      format(1x,a,t25,1pg14.7,t41,1pg14.7,t57,1pg14.7,t73,
     +   1pg14.7,t89,1pg14.7)
         d1(irow)=d(irow)
       end do

C -- An analysis of Cook's D follows.

       write(20,800)
800    format(/,/,' Analysis of Cook''s D ----->',/)
       critval=4.0d0/(numnonzero)
       write(atemp,'(1pg12.5)') critval
       atemp=adjustl(atemp)
       write(20,810) trim(atemp)
810    format(' Influential observations with Cook''s D > critical ',
     + 'value [4/(nobs+nprior))] = 'a)
       write(20,811)
811    format(' (arranged in order of decreasing Cook''s D)')
       write(20,820)
820    format(/,' Observation_name',t25,'Cooks_D',t42,'Hadis_stat')
       numrec=0
823    continue
       maxval=-1.0d35
       maxrow=-9999
       do irow=1,nxrow
         if(d(irow).gt.critval)then
           if(d(irow).gt.maxval)then
             if(numrec.ne.0)then
               do irec=1,numrec
                 if(nrec(irec).eq.irow) go to 821
               end do
             end if
             maxval=d(irow)
             maxrow=irow
           end if
         end if
821      continue
       end do
       if(maxrow.eq.-9999) go to 822
       if(numcov.eq.0)then
         atemp=aobs(maxrow)
       else
         ii=nobgnm(maxrow)
         do i=1,numcov
           if(ii.eq.covgp(i))then
              atemp=trim(aobs(maxrow))//'_'
              go to 830
            end if
          end do
          atemp=aobs(maxrow)
       end if
830    continue
       write(20,840) trim(atemp),d(maxrow),ha(maxrow)
840    format(1x,a,t25,1pg14.7,t42,1pg14.7)
       numrec=numrec+1
       nrec(numrec)=maxrow
       go to 823
822    continue
       if(numrec.eq.0)then
         write(20,824)
824      format(' *** No observations with Cook''s D greater than ',
     +   'critical value ***')
       end if

C -- An analysis of Hadi's statistic follows. First some statistics are worked out.

       mean_ha=0.0d0
       do irow=1,nxrow
         mean_ha=mean_ha+ha(irow)
       end do
       mean_ha=mean_ha/numnonzero
       std_ha=0.0d0
       do irow=1,nxrow
         std_ha=std_ha+(ha(irow)-mean_ha)*(ha(irow)-mean_ha)
       end do
       if(numnonzero.gt.1) std_ha=std_ha/(numnonzero-1)
       std_ha=sqrt(std_ha)
       critval=mean_ha+2.0*std_ha
       write(20,850)
850    format(/,/,' Analysis of Hadi''s H^2 influence statistic ',
     + '----->',/)
       write(atemp,'(1pg12.5)') critval
       atemp=adjustl(atemp)
       write(20,860) trim(atemp)
860    format(' Influential observations with Hadi''s statistic > ',
     + 'critical value [mean(H^2)+2*sqrt(var(H^2)] = 'a)
       write(20,861)
861    format(' (arranged in order of decreasing Hadi''s statistic)')
       write(20,870)
870    format(/,' Observation_name',t25,'Hadis_stat',t42,'Cooks_D')
       numrec=0
873    continue
       maxval=-1.0d35
       maxrow=-9999
       do irow=1,nxrow
         if(ha(irow).gt.critval)then
           if(ha(irow).gt.maxval)then
             if(numrec.ne.0)then
               do irec=1,numrec
                 if(nrec(irec).eq.irow) go to 871
               end do
             end if
             maxval=ha(irow)
             maxrow=irow
           end if
         end if
871      continue
       end do
       if(maxrow.eq.-9999) go to 872
       if(numcov.eq.0)then
         atemp=aobs(maxrow)
       else
         ii=nobgnm(maxrow)
         do i=1,numcov
           if(ii.eq.covgp(i))then
             atemp=trim(aobs(maxrow))//'_'
             go to 880
           end if
         end do
         atemp=aobs(maxrow)
       end if
880    continue
       write(20,840) trim(atemp),ha(maxrow),d(maxrow)
       numrec=numrec+1
       nrec(numrec)=maxrow
       go to 873
872    continue
       if(numrec.eq.0)then
         write(20,874)
874      format(' *** No observations with Hadi''s statisic greater ',
     +   'than critical value ***')
       end if

       write(20,*)
       nlist90=float(numnonzero)*0.90
       nlist90=numnonzero-nlist90
       nlist80=float(numnonzero)*0.80
       nlist80=numnonzero-nlist80
       ipass=1
891    continue
       if(ipass.eq.1)then
         write(20,881)
881      format(' Observations constituting 90th to 100th ',
     +   'percentile of Hadi''s statistic.')
       else
         write(20,*)
         write(20,892)
892      format(' Observations constituting 80th to 90th ',
     +   'percentile of Hadi''s statistic.')
       end if
       write(20,861)
       write(20,870)
       if(ipass.eq.1)numrec=0
883    continue
       if(ipass.eq.1)then
         if(numrec.eq.nlist90) go to 882
       else
         if(numrec.eq.nlist80) go to 888
       end if
       maxval=-1.0d35
       maxrow=-9999
       do irow=1,nxrow
         if(ha(irow).gt.maxval)then
           if(numrec.ne.0)then
             do irec=1,numrec
               if(nrec(irec).eq.irow) go to 885
             end do
           end if
           maxval=ha(irow)
           maxrow=irow
         end if
885      continue
       end do
       if(numcov.eq.0)then
         atemp=aobs(maxrow)
       else
         ii=nobgnm(maxrow)
         do i=1,numcov
           if(ii.eq.covgp(i))then
             atemp=trim(aobs(maxrow))//'_'
             go to 889
           end if
         end do
         atemp=aobs(maxrow)
       end if
889    continue
       write(20,840) trim(atemp),ha(maxrow),d(maxrow)
       numrec=numrec+1
       nrec(numrec)=maxrow
       go to 883
882    continue
       ipass=2
       go to 891
888    continue

#ifdef FLUSHFILE
       call flush(20)
#endif

C -- We now form the transpose of (Q'Q)(-1)Q'

       n_p=numnonzero-nespar
       if(n_p.le.1)then
         write(errmsg,530) trim(apestfile)
530      format('DFBETAS cannot be computed because the number of ',
     +   'non-zero-weighted observations in file ',a,
     +   ' must exceed the number of ',
     +   'adjustable parameters by more than one.')
         go to 9890
       end if
       do ies=1,nespar
         do irow=1,nxrow
           dtemp=0.0d0
           do k=1,nespar
             dtemp=dtemp+qtq(ies,k)*x(irow,k)
           end do
           x_o(irow,ies)=dtemp
         end do
       end do

       allocate(den(nespar),stat=ierr)
       if(ierr.ne.0) go to 9200

       do ies=1,nespar
         sum=0.0d0
         do irow=1,nxrow
           sum=sum+x_o(irow,ies)*x_o(irow,ies)
         end do
         if(sum.gt.0.0d0)then
           den(ies)=1.0d0/sqrt(sum)
         else
           den(ies)=-1.0d35
         end if
       end do
       do irow=1,nxrow
         d(irow)=(n_p*refvar-resid(irow)*resid(irow)/(1.0d0-h(irow)))
     +           /(n_p-1)
         if(d(irow).lt.0.0d0)then
           d(irow)=-1.0d35
         else
           d(irow)=1.0d0/(sqrt(d(irow))*(1.0d0-h(irow)))
         end if
       end do
       ibad=0
       do ies=1,nespar
         do irow=1,nxrow
           if((d(irow).gt.-1.0d34).and.(den(ies).gt.-1.0d34))then
             x_o(irow,ies)=x_o(irow,ies)*den(ies)*resid(irow)*d(irow)
           else
             x_o(irow,ies)=-1.0d35
             ibad=1
           end if
         end do
       end do

C -- DFBETAs are now written to the file.
C -- But before we do this we get rid of tied and fixed parameters from apar.

       ies=0
       do ipar=1,npar
         if(itrans(ipar).lt.0) cycle
         ies=ies+1
         apar(ies)=apar(ipar)
       end do

       write(20,600)
600    format(/,/,/,' DFBETAS ----->',/)
       i=0
631    continue
c       write(20,630) (trim(apar(ies)),ies=i+1,min(nespar,i+8))
c630    format(1x,22x,8(a12,2x))
c       i=i+8
c       if(i.ge.nespar) go to 635
c       go to 631
c635    continue
       write(20,630) (trim(apar(ies)),ies=i+1,nespar)
630    format(1x,22x,2000(a12,2x))
c       write(20,*)
       do irow=1,nxrow
         if(numcov.eq.0)then
           atemp=aobs(irow)
         else
           ii=nobgnm(irow)
           do i=1,numcov
             if(ii.eq.covgp(i))then
               atemp=trim(aobs(irow))//'_'
               go to 649
             end if
           end do
           atemp=aobs(irow)
         end if
649      continue
c         write(20,650) trim(atemp),
c     +   (x_o(irow,ies),ies=1,min(nespar,8))
c650      format(1x,a,t24,8(1x,1pg13.6))
c         i=8
c         if(i.ge.nespar) go to 655
c651      continue
c         write(20,660) (x_o(irow,ies),ies=i+1,min(i+8,nespar))
c660      format(t24,8(1x,1pg13.6))
c         i=i+8
c         if(i.ge.nespar) go to 655
c         go to 651
c655      continue
c         write(20,*)
         write(20,650) trim(atemp),
     +   (x_o(irow,ies),ies=1,nespar)
650      format(1x,a,t24,2000(1x,1pg13.6))
       end do
       if(ibad.ne.0)then
         write(20,954)
954      format(/,' Note: a DFBETAS value of -1.0e35 indicates that it',
     +   ' cannot be calculated due ',/,'       to a zero or negative ',
     +   'denominator.')
       end if

C -- An analysis of DFBETAS follows.

       write(20,900)
900    format(/,/,' Analysis of DFBETAS ----->',/)
       critval=2.0d0/sqrt(float(numnonzero))
       write(atemp,'(1pg12.5)') critval
       atemp=adjustl(atemp)
       write(20,910) trim(atemp)
910    format(' Influential observations with abs(DFBETAS) > '
     + 'critical value [2/(nobs+nprior)^0.5)] = ',a,/)
       write(20,920)
920    format(' Observation_name',t25,'Parameters -->')
       nrec=0             ! An array
       do irow=1,nxrow
         icpar(irow)=0
         ibsum(irow)=0.0d0
         if(numcov.eq.0)then
           atemp=aobs(irow)
         else
           ii=nobgnm(irow)
           do i=1,numcov
             if(ii.eq.covgp(i))then
               atemp=trim(aobs(irow))//'_'
               go to 949
             end if
           end do
           atemp=aobs(irow)
         end if
949      continue
         eline=' '
         eline(2:)=trim(atemp)
         ii=25
         do ies=1,nespar
           if(x_o(irow,ies).lt.-1.0d34) cycle
           if(abs(x_o(irow,ies)).ge.critval)then
             icpar(irow)=icpar(irow)+1
             ibsum(irow)=ibsum(irow)+abs(x_o(irow,ies))
             eline(ii:)=trim(apar(ies))   ! APAR already altered as above
             nrec(ies)=1
             if(ii.ge.90)then
               write(20,'(a)') trim(eline)
               eline=' '
               ii=25
             else
               ii=ii+14
             end if
           end if
         end do
         if(eline.ne.' ')then
           write(20,'(a)') trim(eline)
         end if
       end do

C -- Dick Yager's table is now inserted.

       write(20,1405)
1405   format(/,t25,'Parameter_number')
       write(20,1410) (ies,ies=1,nespar)
1410   format(' Observation_name',t25,2000i3)
       do irow=1,nxrow
         if(numcov.eq.0)then
           atemp=aobs(irow)
         else
           ii=nobgnm(irow)
           do i=1,numcov
             if(ii.eq.covgp(i))then
               atemp=trim(aobs(irow))//'_'
               go to 1420
             end if
           end do
           atemp=aobs(irow)
         end if
1420     continue
         fline=' '
         do ies=1,nespar
           if(x_o(irow,ies).lt.-1.0d34) cycle
           if(abs(x_o(irow,ies)).ge.critval)then
             ii=(ies-1)*3
             fline(ii+3:ii+3)='#'
           end if
         end do
         write(20,1430) trim(atemp),trim(fline)
1430     format(1x,a,t25,a)
       end do

       write(20,*)
       write(20,922)
922    format(' Observation_name',t25,'Number_of_parameters_influenced',
     + t60,'Sum_abs(DFBETAS)_for_influenced_params')
       do irow=1,nxrow
         if(numcov.eq.0)then
           atemp=aobs(irow)
         else
           ii=nobgnm(irow)
           do i=1,numcov
             if(ii.eq.covgp(i))then
               atemp=trim(aobs(irow))//'_'
               go to 932
             end if
           end do
           atemp=aobs(irow)
         end if
932      continue
         write(20,931) trim(atemp),icpar(irow),ibsum(irow)
931      format(1x,a,t30,i4,t65,1pg14.7)
       end do


       write(20,*)
       write(20,1020)
1020   format(' Above table ordered below by influenced parameters ',
     + '(primary) and sum of abs(DFBETAS) (secondary).')
       write(20,*)
       write(20,922)
       numrec=0
1030   continue
       maxval=-1.0d35
       maxival=-9999
       maxrow=-9999
       do irow=1,nxrow
         if(icpar(irow).gt.0)then
           if(icpar(irow).ge.maxival)then
             idec=0
             if(icpar(irow).gt.maxival)then
               idec=1
             else
               if(ibsum(irow).gt.maxval)then
                 idec=1
               end if
             end if
             if(idec.eq.1)then
               if(numrec.ne.0)then
                 do irec=1,numrec
                   if(nrec(irec).eq.irow) go to 1040
                 end do
               end if
               maxval=ibsum(irow)
               maxival=icpar(irow)
               maxrow=irow
             end if
           end if
         end if
1040     continue
       end do
       if(maxrow.eq.-9999) go to 1060
       if(numcov.eq.0)then
         atemp=aobs(maxrow)
       else
         ii=nobgnm(maxrow)
         do i=1,numcov
           if(ii.eq.covgp(i))then
              atemp=trim(aobs(maxrow))//'_'
              go to 1070
            end if
          end do
          atemp=aobs(maxrow)
       end if
1070   continue
       write(20,931) trim(atemp),icpar(maxrow),ibsum(maxrow)
       numrec=numrec+1
       nrec(numrec)=maxrow
       go to 1030
1060   continue

C -- We now list any observations for which DFBETAS statistics were not computable.

       do ies=1,nespar
         do irow=1,nxrow
           if(x_o(irow,ies).lt.-1.0d34) go to 1300
         end do
       end do
       go to 1200

1300   continue
       write(20,*)
       write(20,1080)
1080   format(' The following observations are probably influential. ',
     + 'However DFBETAS could not be calculated ',/,' for them ',
     + 'because of a negative value under the square root sign in ',
     + 'the denominator.')
       write(20,820)
       do irow=1,nxrow
         do ies=1,nespar
           if(x_o(irow,ies).lt.-1.0d34) go to 1100
         end do
         go to 1090
1100     continue
         if(numcov.eq.0)then
           atemp=aobs(irow)
         else
           ii=nobgnm(irow)
           do i=1,numcov
             if(ii.eq.covgp(i))then
                atemp=trim(aobs(irow))//'_'
                go to 1105
             end if
           end do
           atemp=aobs(irow)
         end if
1105     continue
         write(20,840) trim(atemp),d1(irow),ha(irow)
1090     continue
       end do

C -- Finalisation of printout

1200   continue
       icount=0
       do irec=1,nespar
         if(nrec(irec).gt.0) icount=icount+1
       end do
       call writint(atemp,nespar)
       write(20,952) trim(atemp)
952    format(//,' Number of parameters estimated                   ',
     + '                      : ',a)
       call writint(atemp,icount)
       write(20,953) trim(atemp)
953    format( ' Number influenced by at least one observation as ',
     + 'calculated by DFBETAS : ',a)
       close(unit=20)
       call addquote(outfile,afile)
       write(6,670) trim(afile)
670    format(/,' - file ',a,' written ok.')

       go to 9900

8900   continue

       write(6,8910)
8910   format(/,' INFSTAT is run using the command:',/)
       write(6,8920)
8920   format('     infstat casename outfile [nsig]',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8950)
8950   format('     casename is the filename base of a ',
     + 'completed PEST run,')
       write(6,8980)
8980   format('     outfile  is the INFSTAT output file, and')
       write(6,8990)
8990   format('     nsig     (optional) is the sensitivity ',
     + 'truncation limit.')

       go to 9999


9000   write(errmsg,9010) trim(afile)
9010   format('Error encountered in reading Jacobian matrix ',
     + 'file ',a,'.')
       go to 9890
9050   continue
       write(errmsg,9060) trim(afile),trim(apestfile)
9060   format('Jacobian matrix file ',a,' is not compatible ',
     + 'with PEST control file ',a,'; at least one observation ',
     + 'or adjustable parameter is different between them.')
       go to 9890

9200   write(errmsg,9210)
9210   format('Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9890

9300   continue
       call addquote(resfile,afile)
       call writint(aline,iline)
       write(errmsg,9310) trim(aline),trim(afile)
9310   format('Error encountered when reading line ',a,
     + ' of residuals file ',a,'.')
       go to 9890

9400   write(errmsg,9410) trim(afile),trim(apestfile)
9410   format('Error reading resolulution data file ',a,
     + ' corresponding to PEST control file ',a,'.')
       go to 9890


9890   continue
       errmsg=' '//trim(errmsg)
       call writmess(6,errmsg)


9900   continue
       call pest_data_deallocate(ifail)
       deallocate(rownum,resid,tempcov,s,workvec,qtq,h,d,den,ha,
     + irgp,nrec,icpar,ibsum,d1,stat=ierr)

       close(unit=10,iostat=ierr)
       close(unit=11,iostat=ierr)
       close(unit=12,iostat=ierr)
       close(unit=20,iostat=ierr)

9999   continue
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



        subroutine sigtruncate(dtemp,nsig)

        implicit none
        integer nsig,ifail
        character*10 afmt
        character*25 atemp
        double precision dtemp,dtemp1

        if(nsig.lt.1) return
        if(nsig.gt.23)return
        write(afmt,10)nsig
10      format('(g23.',i2,')')
        write(atemp,afmt) dtemp
        atemp=adjustl(atemp)
        call drealread(ifail,atemp,dtemp1)
        if(ifail.ne.0) then
          return
        else
          dtemp=dtemp1
        end if

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

