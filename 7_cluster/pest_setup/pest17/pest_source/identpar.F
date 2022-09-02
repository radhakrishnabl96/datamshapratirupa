       program identpar

C -- Program IDENTPAR computes parameter identifiability.

       use pestdata, only  : maxcompdim,pestmode,npar,nobsgp,nxrow,
     +                       errmsg,apar,obgnme,aobs,itrans,numcov,
     +                       covind,owght,x,pval_o,modval_o,
     +                       parchglim,pval,parlbnd,parubnd,
     +                       ipargp,scale,offset,dercom,covgp,nespar,
     +                       nobs,nobgnm,covar,nobscov

       implicit none

       integer lwork,ifail,ierr,irestart,n1,n,
     + iobsgp,itemp,irow,itemp1,itemp2,bn,icount,i,j,ies,
     + ipar,jes,jrow,icov,info,ii,jj,igroup,icol,ncol
       integer maxobs,job,iicount
       integer ising,nsing
       integer isquare,qlwork,qldvt,nxrow1,nnsing,icovrow,ldu,k
       integer p1start,pfile
       integer lw(10),rw(10)
       integer, allocatable :: iregobs(:),iregobsgp(:)
       integer, allocatable :: iindex(:)
       double precision dtemp,rtemp
       double precision u(1,1),vt(1,1)
       double precision det(2),rcond
       double precision, allocatable :: workvec(:),w1(:),z(:),ident(:)
       double precision, allocatable :: contrib(:)
       double precision, allocatable :: lhs(:,:)
       double precision, allocatable :: qworkvec(:),qw1(:)
       double precision, allocatable :: qu(:,:),xx(:,:)
       character*4 aext
       character*5 anull
       character*10 anum,atemp,acov
       character*12 aversion
       character*12 aapar
       character*20 aaobs
       character*200 pestfile,jcofile,outfile,outbase,casename,
     + matfile,identfile
       character*200 afile,bfile
       character*600 dline
       character*10, allocatable :: acol(:)

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' IDENTPAR Version ',a,'. Watermark Numerical ',
     + 'Computing.',/)

#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) dline
#else
       call pgetcl(dline)
#endif

C -- The casename is read from the IDENTPAR command line.

       if(dline.eq.' ') go to 8900
       call spacesub(dline)
       call linspl(ifail,7,lw,rw,dline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,6,lw,rw,dline)
       if(ifail.eq.0) then
         atemp=dline(lw(6):rw(6))
         call lowcas(atemp)
         if(atemp.eq.'/s')then
           isquare=1
         else if(atemp.eq.'/r')then
           isquare=0
         else
           go to 8900
         end if
       else
         isquare=1
       end if

       call linspl(ifail,5,lw,rw,dline)
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

       call intread(ifail,dline(lw(2):rw(2)),nsing)
       if(ifail.ne.0) go to 8900
       if(nsing.le.0) go to 8900

       outbase=dline(lw(3):rw(3))
       call remchar(outbase,char(211))
#ifndef UNIX
       call lowcas(outbase)
#endif
       anull=outbase(1:5)
       call lowcas(anull)
       if((anull.eq.'nul  ').or.(anull.eq.'null '))then
         outbase='null'
       end if

       matfile=dline(lw(4):rw(4))
       call remchar(matfile,char(211))
#ifndef UNIX
       call lowcas(matfile)
#endif
       anull=matfile(1:5)
       call lowcas(anull)
       if((anull.eq.'nul  ').or.(anull.eq.'null '))then
         matfile='null'
       end if

       identfile=dline(lw(5):rw(5))
       call remchar(identfile,char(211))
#ifndef UNIX
       call lowcas(identfile)
#endif
       anull=identfile(1:5)
       call lowcas(anull)
       if((anull.eq.'nul  ').or.(anull.eq.'null '))then
         identfile='null'
       end if

       pestfile=trim(casename)//'.pst'
       jcofile=trim(casename)//'.jco'

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
240      format('IDENTPAR requires that MAXCOMPDIM be set to 1 ',
     +   'or less in PEST control file ',a,'.')
         go to 9890
       end if
       if(pestmode.eq.2)then
         write(errmsg,250) trim(afile)
250      format('IDENTPAR requires that PEST not be run in predictive ',
     +   'analysis mode in PEST control file ',a,'.')
         go to 9890
       end if
       if(pestmode.eq.3)then
260      write(errmsg,270) trim(afile)
270      format(' Warning: file ',a,' tells PEST to run in ',
     +   'regularisation mode. All regularisation observations and ',
     +   'regularisation prior information will be ignored when ',
     +   'evaluating parameter identifiability.')
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
       if(nsing.gt.nespar)then
         write(errmsg,274) trim(afile)
274      format('There are less adjustable parameters cited in file ',
     +   a,' than number of singular values requested on IDENTPAR ',
     +   'command line.')
         go to 9890
       end if
       write(6,275) trim(afile)
275    format(' - file ',a,' read ok.')

C -- We can delete some unused PEST memory.

       deallocate(pval_o,modval_o,parchglim,pval,parlbnd,parubnd,
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
       call open_unformatted_file(ifail,11,'read',jcofile,errmsg)
       if(ifail.ne.0)then
         call addquote(pestfile,bfile)
         write(errmsg,60) trim(afile),trim(bfile)
60       format('Cannot open Jacobian matrix file ',a,
     +   ' corresponding to PEST control file ',a,'.')
         go to 9890
       end if
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

C --   If any covariance matrices were in the original PEST dataset these
C      are now inverted. Alternatively, the inverse square root of these
C      is formed.

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
     +           'IDENTPAR cannot proceed.')
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

C -- The component of XtQX is worked out for observation groups that do not
C    have a covariance matrix.

         allocate(lhs(nespar,nespar),stat=ierr)
         if(ierr.ne.0) go to 9200
         do ies=1,nespar
           do jes=1,nespar
             lhs(jes,ies)=0.0d0
           end do
         end do
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
300      format(' - carrying out singular value decomposition....')
         lwork=8*nespar
         allocate(workvec(lwork),w1(nespar),stat=ierr)
         if(ierr.ne.0) go to 9200
         call dgesvd('O','N',nespar,nespar,lhs,nespar,w1,u,
     +   nespar,vt,nespar,workvec,lwork,info)
         if(info.ne.0)then
           write(errmsg,310)
310        format('Singular value decomposition process did not ',
     +     'converge.')
           go to 9890
         end if
         if(w1(1).eq.0.0)then
           write(errmsg,312)
           go to 9890
         end if
         if(nsing.le.nespar)then
           if(w1(nsing)/w1(1).lt.1.0d-13)then
             call writint(anum,nsing)
             write(errmsg,313) trim(anum)
             go to 9890
           end if
         end if

       else

C -- The number of singular values is worked out. This is the minimum of NESPAR and number of
C    non-regularisation observations for which the weight is nonzero.

         nxrow1=0
         do irow=1,nxrow
           if(iregobs(irow).eq.0)then
             itemp=nobgnm(irow)
             if((covind(itemp)).or.(owght(irow).ne.0.0d0))
     +       nxrow1=nxrow1+1
           end if
         end do
         if(nxrow1.eq.0)then
           write(errmsg,312)
312        format('There are no non-zero-weighted non-regularisation ',
     +     'observations cited in PEST control file.')
           go to 9890
         end if
         nnsing=min(nxrow1,nespar)
         if(nnsing.lt.nsing)then
           call writint(anum,nsing)
           write(errmsg,313) trim(anum)
313        format('The calibration dataset will not support the use ',
     +     'of ',a,' singular values. Decrease this number and ',
     +     're-run program.')
           go to 9890
         end if

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
         do i=1,nsing
           w1(i)=w1(i)*w1(i)
         end do
         deallocate(xx,stat=ierr)

       end if

C -- Predictive sensitivity vector files are now written.

       if(outbase.ne.'null')then
         write(6,311)
311      format(/,' - writing vector output files....')
         do ising=1,min(nsing,nespar)
           call writint(anum,ising)
           outfile=trim(outbase)//trim(anum)//'.vec'
           open(unit=20,file=outfile)
           write(20,330) nespar,1,2
330        format(3i10)
           if(isquare.eq.1)then
             do ies=1,nespar
               write(20,340) lhs(ies,ising)
340            format(1x,1pg14.7)
             end do
           else
             do ies=1,nespar
               write(20,340) lhs(ising,ies)
             end do
           end if
           write(20,350)
350        format('* row names')
           ies=0
           do ipar=1,npar
             if(itrans(ipar).ge.0) then
               write(20,'(1x,a)') trim(apar(ipar))
             end if
           end do
           write(20,360)
360        format('* column names')
           anum='eig'//trim(anum)
           write(20,'(1x,a)') trim(anum)
           close(unit=20)
           call addquote(outfile,afile)
           write(6,370) trim(afile)
370        format(' - file ',a,' written ok.')
         end do
       end if

C -- The matrix output file is written.

       if(matfile.ne.'null')then
         write(6,371)
371      format(/,' - writing matrix output file....')
         open(unit=20,file=matfile)
         ncol=min(nsing,nespar)
         write(20,330) nespar,ncol,2
         if(isquare.eq.1)then
           do ies=1,nespar
             write(20,380) (lhs(ies,icol),icol=1,ncol)
380          format(8(1x,1pg14.7))
           end do
         else
           do ies=1,nespar
             write(20,380) (lhs(icol,ies),icol=1,ncol)
           end do
         end if
         write(20,350)
         ies=0
         do ipar=1,npar
           if(itrans(ipar).ge.0) then
             write(20,'(1x,a)') trim(apar(ipar))
           end if
         end do
         write(20,360)
         do icol=1,ncol
           call writint(anum,icol)
           anum='eig'//trim(anum)
           write(20,'(1x,a)') trim(anum)
         end do
         close(unit=20)
         call addquote(matfile,afile)
         write(6,370) trim(afile)
       end if

       if(identfile.ne.'null')then
         write(6,372)
372      format(/,' - writing parameter identifiability file....')
         allocate(ident(nespar),stat=ierr)
         if(ierr.ne.0) go to 9200
         ncol=min(nsing,nespar)
         allocate(acol(ncol),contrib(ncol),stat=ierr)
         if(ierr.ne.0) go to 9200
         do ies=1,nespar
           dtemp=0.0d0
           if(isquare.eq.1)then
             do icol=1,ncol
               dtemp=dtemp+lhs(ies,icol)*lhs(ies,icol)
             end do
           else
             do icol=1,ncol
               dtemp=dtemp+lhs(icol,ies)*lhs(icol,ies)
             end do
           end if
           ident(ies)=dtemp
           if(ident(ies).lt.1.0d-35)ident(ies)=0.0d0
         end do
         open(unit=20,file=identfile)
         do icol=1,ncol
           call writint(acol(icol),icol)
           acol(icol)='eig'//trim(acol(icol))
         end do
         write(20,410) 'parameter',(trim(acol(icol)),icol=1,ncol),
     +   'identifiability','sqrt(ident)'
410      format(1000a16)
         ies=0
         do ipar=1,npar
           if(itrans(ipar).ge.0)then
             ies=ies+1
             if(isquare.eq.1)then
               do icol=1,ncol
                 contrib(icol)=lhs(ies,icol)*lhs(ies,icol)
                 if(contrib(icol).lt.1.0d-35) contrib(icol)=0.0d0
               end do
             else
               do icol=1,ncol
                 contrib(icol)=lhs(icol,ies)*lhs(icol,ies)
                 if(contrib(icol).lt.1.0d-35) contrib(icol)=0.0d0
               end do
             end if
             write(20,420) trim(apar(ipar)),
     +       (contrib(icol),icol=1,ncol),ident(ies),sqrt(ident(ies))
420          format(1x,a,t15,1000(2x,1pg14.7))
           end if
         end do
         close(unit=20)
         call addquote(identfile,afile)
         write(6,370) trim(afile)
       end if

       go to 9900

8900   continue

       write(6,8910)
8910   format(/,' IDENTPAR is run using the command:',/)
       write(6,8920)
8920   format('     IDENTPAR casename numvec outbase matfile ',
     + 'identfile [/s or /r]',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8935)
8935   format('     casename  is a PEST control file basename,')
       write(6,8950)
8950   format('     numvec    is the number of singular values to ',
     + 'use, ')
       write(6,8980)
8980   format('     outbase   is the filename base of sensitivity ',
     + 'vector output files,')
       write(6,8981)
8981   format('     outfile   is the name of a matrix output file')
       write(6,8982)
8982   format('     identfile is the name of a parameter ',
     + 'identifiability output file, and')
       write(6,8984)
8984   format('     /s or /r  instigates SVD on XtQX or Q^(1/2)X ',
     + 'respectively (/s is default).')

       write(6,8993)
8993   format(/,'     Note: enter a filename of "null" for no ',
     + 'pertinent output file.')

       go to 9999

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
       deallocate(iregobs,iregobsgp,lhs,workvec,w1,z,ident,acol,
     + contrib,stat=ierr)
       deallocate(iindex,qworkvec,qw1,qu,xx,stat=ierr)

       close(unit=10,iostat=ierr)
       close(unit=11,iostat=ierr)
       close(unit=20,iostat=ierr)

9999   continue

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

