       program supcalc

C -- Program SUPCALC estimates how many super parameters are suitable for use
C    for a particular inversion problem.

       use pestdata, only  : maxcompdim,pestmode,npar,nobsgp,nxrow,
     +                       errmsg,apar,obgnme,aobs,itrans,numcov,
     +                       covind,owght,x,pval_o,modval_o,
     +                       parchglim,pval,parlbnd,parubnd,
     +                       ipargp,scale,offset,dercom,covgp,nespar,
     +                       nobs,nobgnm,covar,nobscov

       implicit none

       logical lexist
       integer lwork,ibeg,iend,ifail,nb_pst,ierr,numsing,irestart,
     + iobsgp,itemp,irow,itemp1,itemp2,bn,icount,i,j,ies,
     + ipar,iunit,junit,itype,vardim,covdim,jpartype,
     + jes,jrow,icov,info,jsing,ii,jj,igroup
       integer p1start,pfile
       integer maxobs,job,iicount,nxrow1,lowflag,highflag
       integer isquare,qlwork,qldvt,icovrow,nnsing,ldu,k
       integer, allocatable :: iregobs(:),iregobsgp(:)
       integer, allocatable :: iindex(:)
       double precision refvar,dtemp,first,second,rtemp,phi,stddev,
     + eighigh
       double precision u(1,1),vt(1,1),parvar(1)
       double precision det(2),rcond
       double precision, allocatable :: workvec(:),w1(:),
     + cp(:,:),lhs(:,:),total(:),cp1(:)
       double precision, allocatable :: z(:)
       double precision, allocatable :: qworkvec(:),qw1(:),qu(:,:),
     + xx(:,:)
       character*1 ua
       character*4 aext
       character*10 atype,anum
       character*10 acov
       character*12 aversion
       character*12 aapar
       character*20 aaobs
       character*200 afile,bfile,pestfile,jcofile,uncertfile,
     + outfile
       character*300 dline
       character*12, allocatable :: adjapar(:)

C -- Initialization

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' SUPCALC Version ',a,'. Watermark Numerical ',
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
52     format(' Enter expected value of measurement objective ',
     + 'function: ')
       read(5,*,err=51)phi
       if(phi.le.0.0d0) go to 51

       write(6,*)
55     write(6,56)
56     format(' To conduct SVD on Q^(1/2)X  - enter 1')
       write(6,57)
57     format(' To conduct SVD on XtQX      - enter 2')
59     write(6,58,advance='no')
58     format(' Enter your choice: ')
       read(5,*,err=59) itemp
       if((itemp.ne.1).and.(itemp.ne.2)) go to 59
       if(itemp.eq.1)then
         isquare=0
       else
         isquare=1
       end if

C -- More input is sought.

       write(6,*)
69     write(6,70,advance='no')
70     format(' Use uncertainty file or bounds to ',
     + 'specify parameter variability?  [u/b]: ')
       read(5,'(a)') ua
       if((ua.eq.'U').or.(ua.eq.'u'))then
         ua='u'
       else if((ua.eq.'B').or.(ua.eq.'b'))then
         ua='b'
       else
         go to 69
       end if
       if(ua.eq.'u')then
80       write(6,90,advance='no')
90       format(' Enter name of parameter uncertainty file: ')
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
95         format(/,' Cannot open file ',a,' - try again.',/)
           go to 80
         end if
       end if

       write(6,*)
97     write(6,96,advance='no')
96     format(' Enter name for eigenvector gain/loss output file: ')
       read(5,'(a)') outfile
       if(outfile.eq.' ') go to 97
       open(unit=20,file=outfile,action='write',iostat=ierr)
       if(ierr.ne.0)then
         write(6,98)
98       format(/,' Cannot open this file for output - try again.',/)
         go to 97
       end if
       write(20,99)
99     format(' Singular_value',t25,'fall_in_null_space_term',
     + t50,'rise_in_soln_space_term',t78,'total_fall_in_error_variance')


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
240      format('SUPCALC requires that MAXCOMPDIM be set to 1 ',
     +   'or less in PEST control file ',a,'.')
         go to 9890
       end if
       if(pestmode.eq.2)then
         write(errmsg,250) trim(afile)
250      format('SUPCALC requires that PEST not be run in predictive ',
     +   'analysis mode in PEST control file ',a,'.')
         go to 9890
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
       write(6,275) trim(afile)
275    format(' - file ',a,' read ok.')

C -- We can delete some unused PEST memory.

       deallocate(pval_o,modval_o,parchglim,pval,
     + ipargp,scale,offset,dercom,stat=ierr)

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

       allocate(adjapar(nespar),total(nespar),stat=ierr)
       if(ierr.ne.0) go to 9200

C -- A new parameter name array is built comprised only of adjustable parameters.

       ies=0
       do ipar=1,npar
         if(itrans(ipar).ge.0) then
           ies=ies+1
           adjapar(ies)=apar(ipar)
         end if
       end do

C -- The parameter uncertainty file is read.

       if(ua.eq.'u')then
         allocate(cp(nespar,nespar),stat=ierr)
         if(ierr.ne.0) go to 9200
         call addquote(uncertfile,afile)
         write(6,570) trim(afile)
570      format(/,' - reading parameter uncertainty file ',a,'....')
         iunit=12
         junit=15
         itype=2
         vardim=1
         covdim=nespar
         atype='parameter'
         call read_uncert_data_file(ifail,iunit,junit,itype,jpartype,
     +   nespar,vardim,covdim,parvar,cp,atype,uncertfile,errmsg,
     +   dline,adjapar)
         if(ifail.ne.0) then
           errmsg=adjustl(errmsg)
           go to 9890
         end if
         if(jpartype.eq.1)then
           allocate(cp1(nespar),stat=ierr)
           if(ierr.ne.0) go to 9200
           do ies=1,nespar
             cp1(ies)=cp(ies,ies)
           end do
           deallocate(cp,stat=ierr)
         end if
         write(6,580) trim(afile)
580      format(' - parameter uncertainty file ',a,' read ok.')
       else

C -- C(p) is evaluated from parameter bounds.

         allocate(cp1(nespar),stat=ierr)
         if(ierr.ne.0) go to 9200
         jpartype=1
         cp1=0.0d0
         call addquote(pestfile,afile)
         ies=0
         do ipar=1,npar
           if(itrans(ipar).ge.0)then
             ies=ies+1
             if(parubnd(ipar).le.parlbnd(ipar))then
               write(errmsg,305) trim(apar(ipar)),trim(afile)
305            format('Upper bound for parameter "',a,'" is not ',
     +         'greater than lower bound in PEST control file ',a,'.')
               go to 9890
             end if
             if(itrans(ipar).eq.1)then
               if((parlbnd(ipar).le.0.0d0).or.
     +            (parubnd(ipar).le.0.0d0))then
                 write(errmsg,310) trim(apar(ipar)),trim(afile)
310              format(' Parameter "',a,'" is log transformed in ',
     +           'PEST control file ',a,'. It must have a positive ',
     +           'lower bound.')
                 go to 9890
               end if
               stddev=log10(parubnd(ipar)/parlbnd(ipar))*0.3
             else
               stddev=(parubnd(ipar)-parlbnd(ipar))*0.3
             end if
             cp1(ies)=stddev*stddev
           end if
         end do
       end if

C --   If any covariance matrices were in the original PEST dataset these
C      are now inverted.

       if(numcov.ne.0)then
         maxobs=0
         iicount=0
         do icov=1,numcov
           if(nobscov(icov).gt.maxobs) maxobs=nobscov(icov)
         end do
         if(maxobs.eq.0) go to 584

         if(isquare.eq.1)then

           write(6,581)
581        format(/,' - inverting observation covariance matrixes...')
           allocate(z(maxobs),stat=ierr)
           if(ierr.ne.0) go to 9200
           do icov=1,numcov
             itemp=nobscov(icov)
             if(itemp.eq.0) go to 585
             iicount=iicount+1
             call dpoco(covar(icov)%dval,itemp,itemp,rcond,z,info)
             if(info.ne.0)then
               write(errmsg,582) trim(obgnme(covgp(icov)))
582            format('The observation covariance matrix supplied for ',
     +         'PEST observation group "',a,'" is not positive ',
     +         'definite.')
               go to 9890
             end if
             job=1
             call dpodi(covar(icov)%dval,itemp,itemp,det,job)
             if(itemp.gt.1)then
               do i=1,itemp-1
                 do j=i+1,itemp
                   covar(icov)%dval(j,i)=covar(icov)%dval(i,j)
                 end do
               end do
             end if
585          continue
           end do
           deallocate(z,stat=ierr)
           call writint(anum,iicount)
           write(6,583) trim(anum)
583        format(' - ',a,' covariance matrices inverted.')

         else

           write(6,579)
579        format(/,' - undertaking SVD of observation ',
     +     'covariance matrices...')
           qlwork=8*maxobs
           allocate(qworkvec(qlwork),qw1(maxobs),stat=ierr)
           if(ierr.ne.0) go to 9200
           allocate(qu(maxobs,maxobs),stat=ierr)
           if(ierr.ne.0) go to 9200
           iicount=0
           qldvt=1
           do icov=1,numcov
             itemp=nobscov(icov)
             if(itemp.eq.0) cycle
             iicount=iicount+1
             call dgesvd('A','N',itemp,itemp,covar(icov)%dval,itemp,
     +       qw1,qu,maxobs,vt,qldvt,qworkvec,qlwork,info)
             if(info.ne.0)then
               call writint(acov,icov)
               write(errmsg,587) trim(acov)
587            format('Cannot undertake SVD on covariance matrix ',
     +         'number ',a,' cited in PEST control file.')
               go to 9890
             end if
             do i=1,itemp
               if(qw1(i).le.1.0d-14)then
                 call writint(acov,icov)
                 write(errmsg,5821) trim(acov)
5821             format('Observation covariance matrix number ',a,
     +           ' cited in PEST control file is singular. ',
     +           'SUPCALC cannot proceed.')
                 go to 9890
               end if
               qw1(i)=1.0d0/(sqrt(qw1(i)))
             end do
             do j=1,itemp
               do i=1,j
                 rtemp=0.0d0
                 do k=1,itemp
                   rtemp=rtemp+qu(i,k)*qu(j,k)*qw1(k)
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
           write(6,5831) trim(anum)
5831       format(' - ',a,' covariance matrices decomposed.')
           deallocate(qu,qworkvec,qw1,stat=ierr)
         end if

       end if
584    continue

C -- The reference variance is evaluated. But first we need to compute the
C    number of non-zero-weighted, non-regularisation, observations.

       nxrow1=0
       do irow=1,nxrow
         if(iregobs(irow).eq.0)then
           itemp=nobgnm(irow)
           if((covind(itemp)).or.(owght(irow).ne.0.0d0))
     +     nxrow1=nxrow1+1
         end if
       end do
       if(nxrow1.eq.0)then
         call addquote(pestfile,afile)
         write(errmsg,5841) trim(afile)
5841     format('There are no non-regularisation observations with ',
     +   'non-zero weight supplied in file ',a,'.')
         go to 9890
       end if
       refvar=phi/nxrow1

C -- If there are any covariance matrixes in PEST control file, pertinent
C    weights are set to zero.

       if((numcov.gt.0).and.(maxobs.gt.0))then
         do irow=1,nxrow
           itemp=nobgnm(irow)
           if(covind(itemp))owght(irow)=0.0d0
         end do
       end if

C -- The weights pertaining to regularisation obervations are set to zero.

       if(pestmode.eq.3)then
         do irow=1,nxrow
           if(iregobs(irow).ne.0) owght(irow)=0.0d0
         end do
         do icov=1,numcov
           igroup=covgp(icov)
           if(iregobsgp(igroup).ne.0)then
             itemp=nobscov(icov)
             do i=1,itemp
               do j=1,itemp
                 covar(icov)%dval(j,i)=0.0d0
               end do
             end do
           end if
         end do
       end if

       if(isquare.eq.1)then

C -- The LHS matrix is zeroed.

         allocate(lhs(nespar,nespar),stat=ierr)
         if(ierr.ne.0) go to 9200
         do ies=1,nespar
           do jes=1,nespar
             lhs(jes,ies)=0.0d0
           end do
         end do

C -- The component of XtQX is worked out for observation groups that do not
C    have a covariance matrix.

         write(6,295)
295      format(/,' - forming XtQX matrix....')
         do ies=1,nespar
           do jes=1,ies
             rtemp=0.0d0
             do irow=1,nxrow
               rtemp=rtemp+owght(irow)*x(irow,ies)*x(irow,jes)
             end do
             lhs(jes,ies)=rtemp
           end do
         end do

C -- Now the effect of the covariance matrices is taken into account.

         if((numcov.gt.0).and.(maxobs.gt.0))then
           do icov=1,numcov
             iobsgp=covgp(icov)
             do ies=1,nespar
               do jes=1,ies
                 rtemp=lhs(jes,ies)
                 ii=0
                 do irow=1,nxrow
                   if(nobgnm(irow).ne.iobsgp)cycle
                   ii=ii+1
                   jj=0
                   do jrow=1,nxrow
                     if(nobgnm(jrow).ne.iobsgp)cycle
                     jj=jj+1
                     rtemp=rtemp+
     +               x(irow,ies)*x(jrow,jes)*covar(icov)%dval(jj,ii)
                   end do
                 end do
                 lhs(jes,ies)=rtemp
               end do
             end do
           end do
         end if

         do ies=1,nespar-1
           do jes=ies+1,nespar
             lhs(jes,ies)=lhs(ies,jes)
           end do
         end do

         deallocate(x,stat=ierr)

C -- Singular value decomposition is now carried out.

         write(6,300)
300      format(/,' - carrying out singular value decomposition of ',
     +   'XtQX....')
         lwork=8*nespar
         allocate(workvec(lwork),w1(nespar),stat=ierr)
         if(ierr.ne.0) go to 9200
         call dgesvd('O','N',nespar,nespar,lhs,nespar,w1,u,
     +   nespar,vt,nespar,workvec,lwork,info)
         if(info.ne.0)then
           write(errmsg,306)
306        format('Singular value decomposition process did not ',
     +     'converge.')
           go to 9890
         end if

       else

         nnsing=min(nxrow1,nespar)

C -- A new matrix is formed in which all regularisation observations and
C    non-zero-weighted observations are ommitted.

         allocate(xx(nxrow1,nespar),stat=ierr)
         if(ierr.ne.0) go to 9200

C -- This new matrix is now filled - first where there is no covariance matrix.

         jrow=0
         do irow=1,nxrow
           if(iregobs(irow).ne.0) cycle
           itemp=nobgnm(irow)
           if(.not.covind(itemp).and.(owght(irow).le.0.0d0))cycle
           jrow=jrow+1
           rtemp=sqrt(owght(irow))
           do ies=1,nespar
             xx(jrow,ies)=x(irow,ies)*rtemp
           end do
         end do

C -- Now where there are observation covariance matrices.

         if((numcov.ne.0).and.(maxobs.ne.0))then
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
                 if(icovrow.eq.nobscov(icov)) go to 5851
               end if
             end do
5851         continue
             jrow=0
             icovrow=0
             do irow=1,nxrow
               if(iregobs(irow).ne.0) cycle
               itemp=nobgnm(irow)
               if(.not.covind(itemp).and.(owght(irow).le.0.0d0))cycle
               jrow=jrow+1
               if(nobgnm(irow).eq.igroup)then
                 icovrow=icovrow+1
                 do ies=1,nespar
                   rtemp=0.0d0
                   do k=1,nobscov(icov)
                     rtemp=rtemp+
     +                     covar(icov)%dval(icovrow,k)*x(iindex(k),ies)
                   end do
                   xx(jrow,ies)=rtemp
                 end do
                 if(icovrow.eq.nobscov(icov)) go to 589
               end if
             end do
589          continue
           end do
           deallocate(iindex,stat=ierr)
         end if

         deallocate(x,stat=ierr)

C -- Now we do SVD on the xx matrix.

         write(6,3001)
3001     format(/,' - carrying out singular value decomposition ',
     +   'of Q^(1/2)X....')
         if(allocated(workvec))deallocate(workvec)
         allocate(lhs(nespar,nespar),w1(nespar),stat=ierr)
         if(ierr.ne.0) go to 9200
         lwork=max(5*min(nespar,nxrow1)+max(nespar,nxrow1),
     +         5*min(nespar,nxrow1))
         allocate(workvec(lwork),stat=ierr)
         if(ierr.ne.0) go to 9200
         ldu=1
         call dgesvd('N','A',nxrow1,nespar,xx,nxrow1,w1,vt,
     +   ldu,lhs,nespar,workvec,lwork,info)                         ! Note that LHS is back to front from previously
         if(info.ne.0)then
           write(errmsg,3101)
3101       format('Singular value decomposition process did not ',
     +     'converge.')
           go to 9890
         end if
         if(nnsing.lt.nespar)then
           do i=nnsing+1,nespar
             w1(i)=0.0d0
           end do
         end if
         do i=1,nespar
           w1(i)=w1(i)*w1(i)
         end do
         deallocate(xx,stat=ierr)

       end if

C -- We now start the singular value cycle.

       write(6,311)
311    format(/,' - analyzing singular values and writing output ',
     + 'file....')
       numsing=nespar

       do jsing=1,numsing
         first=0.0d0
         if(jpartype.eq.1)then
           if(isquare.eq.1)then
             do i=1,nespar
               first=first+lhs(i,jsing)*lhs(i,jsing)*cp1(i)
             end do
           else
             do i=1,nespar
               first=first+lhs(jsing,i)*lhs(jsing,i)*cp1(i)
             end do
           end if
         else
           do i=1,nespar
             rtemp=0.0d0
             if(isquare.eq.1)then
               do j=1,nespar
                 rtemp=rtemp+lhs(j,jsing)*cp(j,i)
               end do
             else
               do j=1,nespar
                 rtemp=rtemp+lhs(jsing,j)*cp(j,i)
               end do
             end if
             workvec(i)=rtemp
           end do
           first=0.0d0
           if(isquare.eq.1)then
             do i=1,nespar
               first=first+workvec(i)*lhs(i,jsing)
             end do
           else
             do i=1,nespar
               first=first+workvec(i)*lhs(jsing,i)
             end do
           end if
         end if
         if(w1(jsing).gt.1.0d-35)then
           second=refvar/w1(jsing)
           if(second.gt.1.0d35)second=1.0d35
           second=-second
         else
           second=-1.0d35
         end if
         total(jsing)=first+second
         write(20,320) w1(jsing),first,-second,total(jsing)
320      format(2x,1pg14.7,t30,1pg14.7,t55,1pg14.7,t78,1pg14.7)
       end do
       close(unit=20)
       write(6,330) trim(outfile)
330    format(' - file ',a,' written ok.')

C -- Some recomendations are made.

       lowflag=0
       highflag=0
       eighigh=w1(1)

       do jsing=1,numsing
         if(w1(jsing)/eighigh.gt.1.0e-7) highflag=jsing
         if(total(jsing).ge.0.0d0) lowflag=jsing
       end do
       if(highflag.eq.0) highflag=nespar
C       if(lowflag.eq.0) lowflag=min(highflag,nespar)
       if(lowflag.gt.highflag) lowflag=highflag

       write(6,*)
       call writint(anum,lowflag)
       write(6,340) trim(anum)
340    format(' Minimum number of super parameters to use = ',a)
       call writint(anum,highflag)
       write(6,350) trim(anum)
350    format(' Maximum number of super parameters to use = ',a)

       write(errmsg,370)
370    format('Note: the optimum number of super parameters can ',
     + 'often be ',
     + 'somewhat above the minimum shown above if used in ',
     + 'conjunction with ',
     + 'pervasive Tikhonov regularisation for numerical stability ',
     + 'and high likelihood parameter values. See documentation ',
     + 'of ADDREG1 utility.')
       call writmess(6,errmsg)
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
9200   write(errmsg,9210)
9210   format('Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9890

9890   continue
       errmsg=' '//trim(errmsg)
       call writmess(6,errmsg)

9900   continue
       call pest_data_deallocate(ifail)
       deallocate(iregobs,iregobsgp,adjapar,lhs,workvec,
     + w1,total,stat=ierr)
       deallocate(z,stat=ierr)
       if(allocated(cp))  deallocate(cp,stat=ierr)
       if(allocated(cp1)) deallocate(cp1,stat=ierr)
       if(allocated(qworkvec)) deallocate(qworkvec,stat=ierr)
       if(allocated(qw1)) deallocate(qw1,stat=ierr)
       if(allocated(qu)) deallocate(qu,stat=ierr)
       if(allocated(xx)) deallocate(xx,stat=ierr)
       if(allocated(iindex)) deallocate(iindex,stat=ierr)

       close(unit=10,iostat=ierr)
       close(unit=11,iostat=ierr)
       close(unit=12,iostat=ierr)
       close(unit=15,iostat=ierr)
       close(unit=20,iostat=ierr)

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

