       program ssstat


       use pestdata, only  : maxcompdim,pestmode,npar,nobsgp,nxrow,
     +                       errmsg,apar,obgnme,aobs,itrans,numcov,
     +                       covind,owght,x,pval_o,modval_o,
     +                       parchglim,pval,parlbnd,parubnd,
     +                       ipargp,scale,offset,dercom,covgp,nespar,
     +                       nobs,nobgnm,covar,nobscov,nprior,oval

       implicit none

       logical lexist
       integer lwork,ibeg,iend,ifail,nb_pst,ierr,irestart,
     + itemp,irow,itemp1,itemp2,bn,icount,i,j,ies,
     + ipar,iunit,junit,itype,vardim,covdim,jpartype,
     + jes,jrow,icov,info,igroup
       integer p1start,pfile
       integer maxobs,iicount,nxrow1
       integer qlwork,qldvt,icovrow,nnsing,k
       integer itrunc,iipoint
       integer ii
       double precision refvar,dtemp,rtemp,phi,stddev,
     + refvarinv
       double precision sum,sum1
       double precision vt(1,1),parvar(1)
       character*1 ua
       character*4 aext
       character*10 atype,anum
       character*10 acov
       character*10 atrunc
       character*12 aversion
       character*12 aapar
       character*20 aaobs,atemp,atemp1
       character*200 afile,bfile,pestfile,jcofile,uncertfile,
     + outfile,outfile1,outfile2
       character*300 dline,cline

       integer, allocatable :: iindex(:)
       integer, allocatable :: row_pointer(:)
       integer, allocatable :: irgp(:)
       double precision, allocatable :: workvec(:),w1(:),cp(:,:),cp1(:)
       double precision, allocatable :: qworkvec(:),qw1(:),qu(:,:),
     + xx(:,:),v1(:,:),g(:,:)
       double precision, allocatable :: ee(:),ff(:,:)
       double precision, allocatable :: vvt(:,:),uu(:,:)
       double precision, allocatable :: sse(:),nse(:)
       character*12, allocatable :: adjapar(:)

C -- Initialization

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' SSSTAT Version ',a,'. Watermark Numerical ',
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
52     format(' Enter expected value of (measurement) objective ',
     + 'function: ')
       read(5,*,err=51)phi
       if(phi.le.0.0d0) go to 51

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

C -- The name of the output file is acquired.

       write(6,*)
97     write(6,96,advance='no')
96     format(' Enter name for SSSTAT output file: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 97
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,outfile,ibeg,iend)
       if(ifail.ne.0) go to 97
#ifndef UNIX
       call lowcas(outfile)
#endif
       open(unit=20,file=outfile,err=97)

99     write(6,98,advance='no')
98     format(' Enter name for G matrix output file: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 99
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,outfile1,ibeg,iend)
       if(ifail.ne.0) go to 99
#ifndef UNIX
       call lowcas(outfile1)
#endif
       open(unit=21,file=outfile1,err=99)

89     write(6,88,advance='no')
88     format(' Enter name for Z matrix output file: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 89
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,outfile2,ibeg,iend)
       if(ifail.ne.0) go to 89
#ifndef UNIX
       call lowcas(outfile2)
#endif
       open(unit=22,file=outfile2,err=89)

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
240      format('SSSTAT requires that MAXCOMPDIM be set to 1 ',
     +   'or less in PEST control file ',a,'.')
         go to 9890
       end if
       if((pestmode.ne.1).and.(pestmode.ne.3))then
         write(errmsg,250) trim(afile)
250      format('SSSTAT requires that PEST be run in parameter ',
     +   'estimation or regularisation mode in PEST control file ',
     +   a,'.')
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
       allocate(irgp(nobsgp),stat=ierr)
       if(ierr.ne.0) go to 9200
       irgp=0        ! an array
       if(pestmode.eq.3)then
         do igroup=1,nobsgp
           atemp=obgnme(igroup)
           call lowcas(atemp)
           if(atemp(1:5).eq.'regul') irgp(igroup)=1
         end do
       end if
       if(nprior.ne.0)then
         if(pestmode.eq.1)then
           write(errmsg,274) trim(afile)
274        format('SSSTAT requires that no prior information be ',
     +     'used in PEST control file ',a,'.')
           go to 9890
         else
           do irow=nobs+1,nxrow
             ii=nobgnm(irow)
             if(irgp(ii).eq.0)then
               write(errmsg,274) trim(afile)
               go to 9890
             end if
           end do
         end if
       end if
       write(6,275) trim(afile)
275    format(' - file ',a,' read ok.')

C -- We can delete some unused PEST memory.

       deallocate(pval_o,modval_o,parchglim,
     + ipargp,scale,offset,dercom,stat=ierr)

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

       allocate(adjapar(nespar),stat=ierr)
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

C -- If any covariance matrices were in the original PEST dataset these
C    are now inverted.

       if(numcov.ne.0)then
         maxobs=0
         iicount=0
         do icov=1,numcov
           if(nobscov(icov).gt.maxobs) maxobs=nobscov(icov)
         end do
         if(maxobs.gt.0) then
           write(6,579)
579        format(/,' - undertaking SVD of observation ',
     +     'covariance matrices....')
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
               if(qw1(i).le.1.0d-35)then
                 call writint(acov,icov)
                 write(errmsg,5821) trim(acov)
5821             format('Observation covariance matrix number ',a,
     +           ' cited in PEST control file is singular. ',
     +           'SSTAT cannot proceed.')
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

C -- The reference variance is evaluated. But first we need to compute the
C    number of non-zero-weighted observations.

       nxrow1=0
       do irow=1,nxrow
         itemp=nobgnm(irow)
         if(irgp(itemp).eq.0)then
           if((covind(itemp)).or.(owght(irow).ne.0.0d0))then
             nxrow1=nxrow1+1
           end if
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

C -- All squared weights are now multiplied by 1/refvar.

       refvarinv=1.0d0/refvar
       do irow=1,nxrow
         owght(irow)=owght(irow)*refvarinv
       end do
       do icov=1,numcov
         igroup=covgp(icov)
         if(irgp(igroup).eq.0)then
           itemp=nobscov(icov)
           do i=1,itemp
             do j=1,itemp
               covar(icov)%dval(j,i)=covar(icov)%dval(j,i)
     +         *refvarinv
             end do
           end do
         end if
       end do

       nnsing=min(nxrow1,nespar)

C -- A new matrix is formed in which all non-zero-weighted observations
C -- are ommitted. All regularisation observations are also ommitted.

       allocate(xx(nxrow1,nespar),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(row_pointer(nxrow1),stat=ierr)
       if(ierr.ne.0) go to 9200

C -- This new matrix is now filled - first where there is no covariance matrix.

       write(6,2999)
2999   format(/,' - transforming Jacobian matrix....')

       jrow=0
       do irow=1,nxrow
         itemp=nobgnm(irow)
         if(irgp(itemp).ne.0) cycle
         if((.not.covind(itemp)).and.(owght(irow).le.0.0d0))cycle
         jrow=jrow+1
         row_pointer(jrow)=irow
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
           if(irgp(igroup).eq.0)then
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
               itemp=nobgnm(irow)
               if(irgp(itemp).ne.0) cycle
               if((.not.covind(itemp)).and.(owght(irow).le.0.0d0))cycle
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
           end if
         end do
         deallocate(iindex,stat=ierr)
       end if

       if(jpartype.eq.1)then
         deallocate(x,stat=ierr)
       end if

C -- We now form a Jacobian matrix which takes account of the fact that
C    we are doing a KL transformation on parameters.

       if(jpartype.eq.1)then
         do ies=1,nespar
           dtemp=sqrt(cp1(ies))
           do irow=1,nxrow1
             xx(irow,ies)=xx(irow,ies)*dtemp
           end do
         end do
       else
         allocate(ee(nespar),ff(nespar,nespar),stat=ierr)
         if(ierr.ne.0) go to 9200
         allocate(qworkvec(8*nespar),stat=ierr)
         if(ierr.ne.0) go to 9200
         qlwork=8*nespar
         qldvt=1
         call dgesvd('A','N',nespar,nespar,cp,nespar,
     +   ee,ff,nespar,vt,qldvt,qworkvec,qlwork,info)
         if(info.ne.0)then
           write(errmsg,810)
810        format('Cannot undertake SVD on prior parameter ',
     +     'covariance matrix in order to implement K-L ',
     +     'transformation.')
           go to 9890
         end if
         do ies=1,nespar
           if(ee(ies).le.1.0d-35)then
             write(errmsg,820)
820          format('Parameter prior covariance matrix is singular. ',
     +       'K-L transformation cannot be implemented.')
             go to 9890
           end if
         end do
         do ies=1,nespar
           do irow=1,nxrow1
             sum=0.0d0
             do k=1,nespar
               sum=sum+xx(irow,k)*ff(k,ies)
             end do
             x(irow,ies)=sum
           end do
         end do
         do ies=1,nespar
           dtemp=sqrt(ee(ies))
           do irow=1,nxrow1
             x(irow,ies)=x(irow,ies)*dtemp
           end do
         end do
         do ies=1,nespar
           do irow=1,nxrow1
             xx(irow,ies)=x(irow,ies)
           end do
         end do
         deallocate(x,qworkvec,stat=ierr)
         deallocate(ee,ff,stat=ierr)
       end if

C -- The first part of the output file is written.

       write(20,1200)
1200   format(t30,'SUBSPACE INVERSION STATISTICS',/,/)
       call addquote(pestfile,afile)
       write(20,1210) trim(afile)
1210   format(' Name of PEST control file',t60,': ',a)
       write(atemp,'(1pg12.5)') phi
       atemp=adjustl(atemp)
       if(pestmode.eq.3)then
         write(20,1220) trim(atemp)
1220     format(' Expected value of measurement objective function',
     +   t60,': ',a)
       else
         write(20,1221) trim(atemp)
1221     format(' Expected value of objective function',t60,': ',a)
       end if
       write(atemp,'(1pg12.5)') refvar
       atemp=adjustl(atemp)
       write(20,1230) trim(atemp)
1230   format(' Reference variance',t60,': ',a)
       if(ua.eq.'b')then
         atemp='parameter bounds'
       else
         atemp='a file'
       end if
       write(20,1240) trim(atemp)
1240   format(' Pre-calibration parameter uncertainties obtained ',
     + 'from',t60,': ',a)
       if(ua.ne.'b')then
         call addquote(uncertfile,afile)
         write(20,1250) trim(afile)
1250     format(' Name of uncertainty file',t60,': ',a)
       end if
       if(pestmode.eq.3)then
         write(20,*)
         write(20,1260)
1260     format(' NOTE: REGULARISATION IS IGNORED IN CALCULATION OF ',
     +   'ALL STATISTICS.')
       end if

       write(20,*)
       write(20,*)
       write(20,850)
850    format(' ZZt ----->')
       write(cline,851)
851    format(2x,'Observation',t25,'Diag[ZZt]',t45,'Sqrt(Diag[ZZt])',
     + t65,'Obs_value',t85,'Weight_in_PST_file',t105,
     + 'Normalized_weight')
       dline=' '
       do i=1,len_trim(cline)
         if(cline(i:i).ne.' ')dline(i:i)='-'
       end do
       write(20,*)
       write(20,'(a)') trim(cline)
       write(20,'(a)') trim(dline)
       do irow=1,nxrow1
         iipoint=row_pointer(irow)
         aaobs=aobs(iipoint)
         sum=0.0d0
         do ies=1,nespar
           sum=sum+xx(irow,ies)*xx(irow,ies)
         end do
         if(sum.le.0.0d0)then
           sum1=0.0d0
         else
           sum1=sqrt(sum)
         end if
         itemp=nobgnm(iipoint)
         if(covind(itemp))then
           atemp='cov_mat'
           atemp1=atemp
         else
           write(atemp,'(1pg14.7)') sqrt(owght(iipoint)*refvar)
           write(atemp1,'(1pg14.7)') sqrt(owght(iipoint))
           atemp1=adjustl(atemp1)
           atemp=adjustl(atemp)
         end if
         write(20,860) trim(aaobs),sum,sum1,oval(iipoint),
     +   trim(atemp),trim(atemp1)
860      format(2x,a,t25,1pg14.7,t45,1pg14.7,t65,1pg14.7,t85,a,t105,a)
       end do

       write(20,*)
       write(20,*)
       write(20,870)
870    format(' ZtZ ----->')
       write(cline,871)
871    format(2x,'Parameter',t25,'Diag[ZtZ]',t45,'Sqrt(Diag[ZtZ])',
     + t65,'Param_value')
       dline=' '
       do i=1,len_trim(cline)
         if(cline(i:i).ne.' ')dline(i:i)='-'
       end do
       write(20,*)
       write(20,'(a)') trim(cline)
       write(20,'(a)') trim(dline)
       ies=0
       do ipar=1,npar
         if(itrans(ipar).ge.0)then
           ies=ies+1
           aapar=apar(ipar)
           sum=0.0d0
           do irow=1,nxrow1
             sum=sum+xx(irow,ies)*xx(irow,ies)
           end do
           if(sum.le.0.0d0)then
             sum1=0.0d0
           else
             sum1=sqrt(sum)
           end if
           write(20,941) trim(aapar),sum,sum1,pval(ipar)
941        format(2x,a,t25,1pg14.7,t45,1pg14.7,t65,1pg14.7)
         end if
       end do

C -- The Z matrix is written to its output file.

       write(22,1020) nxrow1,nespar,2
       do irow=1,nxrow1
         write(22,1030) (xx(irow,ies),ies=1,nespar)
       end do
       write(22,1040)
       do irow=1,nxrow1
         write(22,1050) trim(aobs(row_pointer(irow)))
       end do
       write(22,1060)
       do ies=1,nespar
         write(22,1050) trim(adjapar(ies))
       end do

C -- Singular value decomposition is now carried out.

       write(20,*)
       write(20,*)
       write(20,880)
880    format(100('*'))
       write(20,881)
881    format(35('*'),' Singular value decomposition ',35('*'))
       write(20,880)

C -- Now we calculate the optimal place to do the cutoff. We do
C    SVD on the xx matrix.

       write(6,3001)
3001   format(' - carrying out singular value decomposition ',
     + 'of Q^(1/2)XFE(1/2)....')
       if(allocated(workvec))deallocate(workvec)
       allocate(vvt(nespar,nespar),w1(nespar),stat=ierr)
       if(ierr.ne.0) go to 9200
       lwork=max(5*min(nespar,nxrow1)+max(nespar,nxrow1),
     +       5*min(nespar,nxrow1))
       allocate(workvec(lwork),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(uu(nxrow1,nxrow1),stat=ierr)
       if(ierr.ne.0) go to 9200
       call dgesvd('A','A',nxrow1,nespar,xx,nxrow1,w1,uu,
     + nxrow1,vvt,nespar,workvec,lwork,info)
       if(info.ne.0)then
         write(errmsg,3101)
3101     format('Singular value decomposition process did not ',
     +   'converge.')
         go to 9890
       end if
       if(nnsing.lt.nespar)then
         do i=nnsing+1,nespar
           w1(i)=0.0d0
         end do
       end if

       write(20,*)
       write(20,*)
       write(20,890)
890    format(' Singular values ----->')
       write(cline,891)
891    format(3x,'Index',t25,'Value')
       dline=' '
       do i=1,len_trim(cline)
         if(cline(i:i).ne.' ')dline(i:i)='-'
       end do
       write(20,*)
       write(20,'(a)') trim(cline)
       write(20,'(a)') trim(dline)
       do i=1,min(nespar,nxrow1)
         write(20,892) i,w1(i)
892      format(2x,i5,t23,1pg14.7)
       end do
       deallocate(xx,stat=ierr)

C -- We now determine the truncation point.

       itrunc=0
       do i=nnsing,1,-1
         if(w1(i).gt.1.0d0)then
           itrunc=i
           go to 900
         end if
       end do
900    continue
       call writint(atrunc,itrunc)
       write(20,909) trim(atrunc)
909    format(/,' Optimal truncation point = ',a,' singular values.')
       write(6,910) trim(atrunc)
910    format(' - optimal truncation point = ',a,' singular values.')
       if(itrunc.eq.0)then
         write(20,920)
920      format(/,' Execution can proceed no further.')
         write(6,920)
         go to 9900
       end if

       write(6,1010)
1010   format(' - forming G matrix....')
       allocate(g(nespar,nxrow1),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(v1(nespar,itrunc),stat=ierr)
       if(ierr.ne.0) go to 9200
       do ies=1,nespar
         do k=1,itrunc
           v1(ies,k)=vvt(k,ies)
         end do
       end do
       do ies=1,nespar
         dtemp=1.0d0/w1(ies)
         do k=1,itrunc
           v1(ies,k)=v1(ies,k)*dtemp
         end do
       end do
       do ies=1,nespar
         do irow=1,nxrow1
           sum=0.0d0
           do k=1,itrunc
             sum=sum+v1(ies,k)*uu(irow,k)
           end do
           g(ies,irow)=sum
         end do
       end do
       write(21,1020) nespar,nxrow1,2
1020   format(3i10)
       do ies=1,nespar
         write(21,1030) (g(ies,irow),irow=1,nxrow1)
1030     format(8(1x,1pg14.7))
       end do
       write(21,1040)
1040   format('* row names')
       do ies=1,nespar
         write(21,1050) trim(adjapar(ies))
       end do
       write(21,1060)
1060   format('* column names')
       do irow=1,nxrow1
         write(21,1050) trim(aobs(row_pointer(irow)))
1050     format(1x,a)
       end do
       deallocate(g,v1,stat=ierr)
       close(unit=21)
       write(6,*)
       write(6,1070) trim(outfile1)
1070   format(' - file ',a,' written ok.')
       write(6,1070) trim(outfile2)

C -- singular values are squared.

       do i=1,nespar
         w1(i)=w1(i)*w1(i)
       end do

C -- Output is written.

       allocate(nse(nespar),sse(nespar),stat=ierr)
       if(ierr.ne.0) go to 9200

       write(20,*)
       write(20,*)
       write(20,930)
930    format(' V1V1t and V2V2t ----->')
       write(cline,931)
931    format(2x,'Parameter',t25,'Diag[V1V1t]',t45,'Diag[V2V2t]')
       dline=' '
       do i=1,len_trim(cline)
         if(cline(i:i).ne.' ')dline(i:i)='-'
       end do
       write(20,*)
       write(20,'(a)') trim(cline)
       write(20,'(a)') trim(dline)
       do ies=1,nespar
         aapar=adjapar(ies)
         sum=0.0d0
         do jes=1,itrunc
           sum=sum+vvt(jes,ies)*vvt(jes,ies)
         end do
         write(20,940) trim(aapar),sum,1.0d0-sum
940      format(2x,a,t25,1pg14.7,t45,1pg14.7)
         nse(ies)=1.0d0-sum
       end do

       write(20,*)
       write(20,*)
       write(20,950)
950    format(' V1S1(2)V1t and V1S1(-2)V1t ----->')
       write(cline,952)
952    format(2x,'Parameter',t25,'Diag[V1S1(2)V1t]',t45,
     + 'Diag[V1S1(-2)V1t]')
       dline=' '
       do i=1,len_trim(cline)
         if(cline(i:i).ne.' ')dline(i:i)='-'
       end do
       write(20,*)
       write(20,'(a)') trim(cline)
       write(20,'(a)') trim(dline)
       do ies=1,nespar
         aapar=adjapar(ies)
         sum=0.0d0
         sum1=0
         do jes=1,itrunc
           sum=sum+vvt(jes,ies)*vvt(jes,ies)/w1(jes)
           sum1=sum1+vvt(jes,ies)*vvt(jes,ies)*w1(jes)
         end do
         write(20,940) trim(aapar),sum1,sum
960      format(2x,a,t25,1pg14.7)
         sse(ies)=sum
       end do

       write(20,*)
       write(20,*)
       write(20,961)
961    format(' KL-scaled parameter error variance ----->')
       write(cline,962)
962    format(2x,'Parameter',t25,'Soln_space',t45,
     + 'Null_space',t65,'Total')
       dline=' '
       do i=1,len_trim(cline)
         if(cline(i:i).ne.' ')dline(i:i)='-'
       end do
       write(20,*)
       write(20,'(a)') trim(cline)
       write(20,'(a)') trim(dline)
       do ies=1,nespar
         aapar=adjapar(ies)
         write(20,963) trim(aapar),sse(ies),nse(ies),sse(ies)+nse(ies)
963      format(2x,a,t24,1pg14.7,t44,1pg14.7,t64,1pg14.7)
       end do

       write(20,*)
       write(20,*)
       write(20,951)
951    format(' U1S1(2)U1t ----->')
       write(cline,964)
964    format(2x,'Observation',t25,'Diag[U1S1(2)U1t]')
       dline=' '
       do i=1,len_trim(cline)
         if(cline(i:i).ne.' ')dline(i:i)='-'
       end do
       write(20,*)
       write(20,'(a)') trim(cline)
       write(20,'(a)') trim(dline)
       do irow=1,nxrow1
         aaobs=aobs(row_pointer(irow))
         sum=0.0d0
         do jrow=1,itrunc
           sum=sum+uu(irow,jrow)*uu(irow,jrow)*w1(jrow)
         end do
         write(20,960) trim(aaobs),sum
       end do

       close(unit=20)
       write(6,970) trim(outfile)
970    format(' - file ',a,' written ok.')
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

       deallocate(iindex,row_pointer,stat=ierr)
       deallocate(workvec,w1,cp,cp1,qworkvec,qw1,qu,xx,stat=ierr)
       deallocate(v1,g,stat=ierr)
       deallocate(ee,ff,vvt,uu,stat=ierr)
       deallocate(adjapar,stat=ierr)
       deallocate(sse,nse,stat=ierr)
       deallocate(irgp,stat=ierr)

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

