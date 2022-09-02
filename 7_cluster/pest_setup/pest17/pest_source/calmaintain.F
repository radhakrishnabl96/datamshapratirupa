       program calmaintain

C -- Program CALMAINTAIN adjusts some parameters so that the model remains in a calibrated
C    state if other parameters are assigned new values.

       use pestdata
       implicit none

       logical          :: lexist,wantse,equals
       integer          :: ifail,ierr,iline,info,istop,itn
       integer          :: n,n1,bn,i,j,icount,jj,nrow,ncol,k,ii
       integer          :: irestart,pfile
       integer          :: ies,iess,irow,icol,ipar,igroup,irow1,nxrow1
       integer          :: lwork,maxnobscov,iicov,workvecdim
       integer          :: leniw,lenrw,itnlim,nout
       integer          :: itemp1,itemp2,itemp
       integer          :: ntrunc,ibeg,iend,nb_pst
       integer          :: ldu,ldvt
       integer          :: lw(10),rw(10),iw(1)
       double precision :: dtemp,sum
       double precision :: damp,atol,btol,conlim
       double precision :: anorm, acond, rnorm, arnorm, xnorm
       double precision :: fract,highest_s,thresh
       double precision :: u(1,1),vt(1,1),rrw(1),se(1)
       character*1      :: asoln
       character*4      :: aext
       character*10     :: aversion,acount
       character*12     :: aapar,atemp,anum
       character*15     :: aline,aprecis,adpoint
       character*20     :: aaobs
       character*256    :: paradjfile,parvalfile,pestfile,
     +                     apestfile,jcofile,afile,bfile
       character*300    :: dline
       external            aaprod

       integer, allocatable             :: irgp(:),rownum(:)
       double precision, allocatable    :: radjpar(:),workvec(:)
       double precision, allocatable    :: tempcov(:,:),s(:)
       double precision, allocatable    :: rhs(:)
       double precision, allocatable    :: v(:),w(:),soln(:)
       double precision, allocatable    :: uu(:,:),vvt(:,:),ss(:),
     +                                     wwork(:)

C       double precision, allocatable    :: xtemp(:,:)                    !debug
C       double precision, allocatable    :: vectemp(:)                    !debug
C       double precision, allocatable    :: temp_rhs(:)                   !debug
C       double precision, allocatable    :: x_keep(:,:)                   !debug

C -- Initialization

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' CALMAINTAIN Version ',a,'. Watermark Numerical ',
     + 'Computing.')

C -- Input data is acquired from the user.

       write(6,*)
20     write(6,30,advance='no')
30     format(' Enter name of PEST control file: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 20
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,pestfile,ibeg,iend)
       if(ifail.ne.0) go to 20
#ifndef UNIX
       call lowcas(pestfile)
#endif
       nb_pst=len_trim(pestfile)
       nb_pst=nb_pst-3
       if(nb_pst.lt.1)nb_pst=1
       if(index(pestfile(nb_pst:),'.').eq.0)
     + pestfile=trim(pestfile)//'.pst'
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

C -- The name of the parameter adjustment file is obtained.

120    write(6,130,advance='no')
130    format(' Enter name of parameter adjustment file: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 120
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,paradjfile,ibeg,iend)
       if(ifail.ne.0) go to 120
#ifndef UNIX
       call lowcas(paradjfile)
#endif
       inquire(file=paradjfile,exist=lexist)
       if(.not.lexist)then
         call addquote(paradjfile,afile)
         write(6,50) trim(afile)
         go to 120
       end if

C -- The name of the parameter value file is obtained.

140    write(6,150,advance='no')
150    format(' Enter name for output parameter value file: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 140
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,parvalfile,ibeg,iend)
#ifndef UNIX
       call lowcas(parvalfile)
#endif
       if(ifail.ne.0) go to 140

       open(unit=20,file=parvalfile,action='write',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(parvalfile,afile)
         write(errmsg,152) trim(afile)
152      format('Cannot open file ',a,' for output.')
         go to 9890
       end if

       write(6,*)
160    write(6,170,advance='no')
170    format(' Use SVD or LSQR to solve equation? [l/s]: ')
       read(5,'(a)') asoln
       if((asoln.eq.'l').or.(asoln.eq.'L'))then
         asoln='l'
       else if((asoln.eq.'s').or.(asoln.eq.'S'))then
         asoln='s'
       else
         go to 160
       end if

2751   write(6,2752,advance='no')
2752   format(' Truncate at how many singular values: ')
       read(5,*,err=2751) ntrunc
       if(ntrunc.le.0) go to 2751

2755   write(6,2756,advance='no')
2756   format(' Adopt what fraction of upgrade vector: ')
       read(5,*,err=2755) fract

C -- All input data has now been acquired. The complete PEST input
C    dataset is now read.

       write(6,*)
       call addquote(pestfile,apestfile)
       write(6,235) trim(apestfile)
235    format(' - reading PEST control file ',a,'....')
       irestart=0
       p1start=0
       pfile=0
       call read_pest_data(ifail,irestart,p1start,pfile,pestfile)
       if(ifail.ne.0) go to 9890
C       if(maxcompdim.gt.1)then
C         write(errmsg,240) trim(apestfile)
C240      format('CALMAINTAIN requires that MAXCOMPDIM be set to 1 ',
C     +   'or less in PEST control file ',a,'.')
C         go to 9890
C       end if
       if((pestmode.eq.2).or.(pestmode.eq.4))then
         write(errmsg,250) trim(apestfile)
250      format('CALMAINTAIN requires that PEST be run in ',
     +   'estimation or regularisation mode in PEST control ',
     +   'file ',a,'.')
         go to 9890
       end if
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


C -- We keep a copy of original parameters.

       pval_o=pval      ! an array

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

C -- The parameter adjustment file is now read.

7000   continue
7010   call addquote(paradjfile,afile)
       write(6,7011) trim(afile)
7011   format(/,' - reading parameter adjustment file ',a,'....')
       open(unit=11,file=paradjfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(errmsg,7020) trim(afile)
7020     format('Cannot open parameter adjustment file ',a,'.')
         go to 9890
       end if
       allocate(radjpar(npar),stat=ierr)
       if(ierr.ne.0) go to 9200
       radjpar=-1.1d301                 ! an array
       iline=0
       ipar=1
       icount=0
       do
         iline=iline+1
         read(11,'(a)',end=7100) dline
         dline=adjustl(dline)
         if(dline(1:1).eq.'#') cycle
         if(dline.eq.' ') cycle
         icount=icount+1
         call linspl(ifail,2,lw,rw,dline)
         if(ifail.ne.0)then
           call writint(aline,iline)
           write(errmsg,7025) trim(aline),trim(afile)
7025       format('Insufficient entries on line ',a,' of file ',a,'.')
           go to 9890
         end if
         aapar=dline(lw(1):rw(1))
         call lowcas(aapar)
         call which1(ifail,npar,ipar,apar,aapar)
         if(ifail.ne.0)then
           call writint(aline,iline)
           write(errmsg,7030) trim(aapar),trim(aline),trim(afile),
     +     trim(apestfile)
7030       format('Parameter "',a,'" cited on line ',a,' of file ',a,
     +     ' is not cited in PEST control file ',a,'.')
           go to 9890
         end if
         if(radjpar(ipar).gt.-1.0d300)then
           call writint(aline,iline)
           write(errmsg,7032) trim(aapar),trim(aline),trim(afile)
7032       format('Parameter "',a,'" is repeated at line ',a,
     +     ' of file ',a,'.')
           go to 9890
         end if
         call drealread(ifail,dline(lw(2):rw(2)),dtemp)
         if(ifail.ne.0)then
           call writint(aline,iline)
           write(errmsg,7040) trim(aline),trim(afile)
7040       format('Cannot read adjusted parameter value at line ',a,
     +     ' of file ',a,'.')
           go to 9890
         end if
         if(itrans(ipar).lt.0)then
           write(errmsg,7050) trim(aapar)
7050       format('Fixed or tied parameter "',a,'" must not be cited ',
     +     'in parameter adjustment file.')
           go to 9890
         else
           if(equals(dtemp,pval(ipar)))then
             radjpar(ipar)=0.0d0
           else
             if(itrans(ipar).eq.0)then
               radjpar(ipar)=dtemp-pval(ipar)
             else
               if(dtemp.le.0.0d0)then
                 call writint(aline,iline)
                 write(errmsg,7052) trim(aapar),trim(aline),trim(afile)
7052             format('Altered value for parameter "',a,'" at line ',
     +           a,' of file ',a,' must be positive as parameter is ',
     +           'log-transformed in PEST control file.')
                 go to 9890
               end if
               radjpar(ipar)=log10(dtemp)-log10(pval(ipar))
             end if
           end if
         end if
       end do
7100   continue
       close(unit=11)
       if(icount.eq.0)then
         write(errmsg,7110) trim(afile)
7110     format('No parameters are featured in file ',a,'.')
         go to 9890
       end if
       icount=0
       do ipar=1,npar
         if(radjpar(ipar).gt.-1.0d300)then
           if(itrans(ipar).ge.0)then
             if(radjpar(ipar).ne.0.0d0) icount=icount+1
           end if
         end if
       end do
       if(icount.eq.0)then
         write(errmsg,7120) trim(afile)
7120     format('No change to any parameter ',
     +   'is featured in file ',a,'.')
         go to 9890
       else
         call writint(acount,icount)
         write(6,7130) trim(acount),trim(afile)
7130     format(' - altered values for ',a,' params read from file ',
     +   a,'.')
       end if

C -- We evaluate the maximum number of observations in an observation group which
C    has a covariance matrix.

       lwork=0
       maxnobscov=0
       if(numcov.gt.0)then
         do iicov=1,numcov
           if(nobscov(iicov).gt.maxnobscov)then
             maxnobscov=nobscov(iicov)
           end if
         end do
         lwork=5*maxnobscov
       end if
       workvecdim=max(2,lwork)
       allocate(workvec(workvecdim))

C -- We can delete some unused PEST memory.

       deallocate(modval_o,parchglim,ipargp,dercom,oval,stat=ierr)

C -- Regularisation observations are identified.

       allocate(irgp(nobsgp),stat=ierr)
       if(ierr.ne.0) go to 9200
       irgp=0              ! an array
       if(pestmode.eq.3)then
         do igroup=1,nobsgp
           atemp=obgnme(igroup)
           call lowcas(atemp)
           if(atemp(1:5).eq.'regul') irgp(igroup)=1
         end do
       end if

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

       do iicov=1,numcov
         jj=covgp(iicov)
         if(irgp(jj).ne.0)cycle
         nrow=nobscov(iicov)
         if(nrow.eq.0) cycle
         ncol=nrow
         do icol=1,ncol
           do irow=1,nrow
             tempcov(irow,icol)=covar(iicov)%dval(irow,icol)
           end do
         end do
         call dgesvd('O','N',nrow,ncol,tempcov,maxnobscov,s,u,
     +   1,vt,1,workvec,lwork,info)
         if(info.ne.0)then
           call writint(anum,iicov)
           write(errmsg,310) trim(anum),trim(apestfile)
310        format('Singular value decomposition process encountered '
     +     'an error for covariance matrix number ',a,' cited in ',
     +     'PEST control file ',a,'.')
           go to 9890
         end if
         if(s(1).le.0.0d0)then
           call writint(anum,iicov)
           write(errmsg,320) trim(anum),trim(apestfile)
           go to 9890
         end if
         dtemp=s(1)
         do irow=1,nrow
           if(s(irow)/dtemp.le.1.0d-25)then
             call writint(anum,iicov)
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
             covar(iicov)%dval(irow,icol)=dtemp
           end do
         end do
         do icol=1,ncol-1
           do irow=icol+1,ncol
             covar(iicov)%dval(irow,icol)=covar(iicov)%dval(icol,irow)
           end do
         end do
       end do

C -- Q(1/2)X is formed for submatrix of X matrix.

       do iicov=1,numcov
         jj=covgp(iicov)
         if(irgp(jj).ne.0)cycle
         i=0
         do irow=1,nxrow
           ii=nobgnm(irow)
           if(ii.eq.jj)then
             i=i+1
             if(i.gt.nobscov(iicov))then
               call writint(anum,iicov)
               write(errmsg,351) trim(obgnme(ii)),trim(apestfile)
351            format('Observation group "',a,'" has more members ',
     +         'than there are dimensions in the covariance matrix ',
     +         'assigned to it in PEST control file ',a,'.')
               go to 9890
             end if
             owght(irow)=1.0d0
             rownum(i)=irow
             do j=1,nespar
               tempcov(i,j)=x(irow,j)
             end do
           end if
         end do
         do i=1,nobscov(iicov)
           irow=rownum(i)
           do ies=1,nespar
             dtemp=0.0d0
             do k=1,nobscov(iicov)
               dtemp=dtemp+covar(iicov)%dval(i,k)*tempcov(k,ies)
             end do
             x(irow,ies)=dtemp
           end do
         end do
       end do

450    continue

C -- If running in regularisation mode we now get rid of all regularisation.

       if(pestmode.eq.3)then
         do irow=1,nxrow
           jj=nobgnm(irow)
           if(irgp(jj).ne.0) owght(irow)=0.0d0
         end do
       end if

C -- We now get rid of any observations with a weight of zero.

       irow1=0
       do irow=1,nxrow
         if(owght(irow).gt.0.0d0)then
           irow1=irow1+1
           if(irow1.ne.irow)then
             owght(irow1)=owght(irow)
             do ies=1,nespar
               x(irow1,ies)=x(irow,ies)
             end do
           end if
         end if
       end do
       nxrow1=nxrow
       nxrow=irow1

C -- Weights are incorporated into the sensitivity matrix.

       do irow=1,nxrow
         dtemp=sqrt(owght(irow))
         do ies=1,nespar
           x(irow,ies)=x(irow,ies)*dtemp
         end do
       end do

C       allocate(xtemp(nxrow,nespar))                 !debug
C       do ies=1,nespar                               !debug
C         do irow=1,nxrow                             !debug
C            xtemp(irow,ies)=x(irow,ies)              !debug
C          end do                                     !debug
C        end do                                       !debug

C -- We now form the right hand side of the adjustment equation.

       allocate(rhs(nxrow),stat=ierr)
       if(ierr.ne.0) go to 9200
       do irow=1,nxrow
         sum=0.0d0
         ies=0
         do ipar=1,npar
           if(itrans(ipar).ge.0)then
             ies=ies+1
             if(radjpar(ipar).gt.-1.0d300)then
               sum=sum+x(irow,ies)*radjpar(ipar)
             end if
           end if
         end do
         rhs(irow)=-sum
       end do

C       allocate(temp_rhs(nxrow))             !debug
C       temp_rhs=rhs                          !debug

C -- We now form a revised sensitivity matrix with adjusted components removed.

       do irow=1,nxrow
         sum=0.0d0
         ies=0
         iess=0
         do ipar=1,npar
           if(itrans(ipar).ge.0)then
             ies=ies+1
             if(radjpar(ipar).lt.-1.0d300)then
               iess=iess+1
               if(iess.ne.ies)then
                 x(irow,iess)=x(irow,ies)
               end if
             end if
           end if
         end do
       end do
       nespar=iess

C -- We adjust ntrunc down as necessary.

       if(ntrunc.gt.nespar)ntrunc=nespar
       if(ntrunc.gt.nxrow)ntrunc=nxrow

C -- We now solve the equation.

       allocate(soln(nespar),stat=ierr)
       if(ierr.ne.0) go to 9200
       if(asoln.eq.'l')then
         write(6,444)
444      format(/,' - using LSQR to solve for parameter change ',
     +   'vector...')
         damp=0.0d0
         wantse=.false.
         leniw=1
         lenrw=1
         iw=0           ! an array
         rrw=0.0d0       ! an array
         allocate(v(nespar),w(nespar),stat=ierr)
         if(ierr.ne.0) go to 9200
         atol=1.0d-7
         btol=1.0d-7
         conlim=1.0d4
         itnlim=4*nespar
         nout=-1
         call lsqr(nxrow,nespar, aaprod, damp, wantse,
     $                     leniw, lenrw, iw, rrw,
     $                     rhs, v, w, soln, se,
     $                     atol, btol, conlim, ntrunc, nout,
     $                     istop, itn, anorm, acond, rnorm, arnorm,
     $                     xnorm)
       else
         write(6,445)
445      format(/,' - using SVD to solve for parameter change ',
     +   'vector...')
C         allocate(x_keep(nxrow,nespar),stat=ierr)      !debug
C         if(ierr.ne.0) go to 9200                      !debug
C         do j=1,nespar                               !debug
C           do i=1,nxrow                              !debug
C             x_keep(i,j)=x(i,j)   ! arrays           !debug
C           end do                                    !debug
C         end do                                      !debug
         ldu=nxrow
         allocate(uu(ldu,min(nespar,nxrow)),stat=ierr)
         if(ierr.ne.0) go to 9200
         ldvt=min(nespar,nxrow)
         allocate(vvt(ldvt,nespar),stat=ierr)
         if(ierr.ne.0) go to 9200
         lwork=8*max(nespar,nxrow)
         allocate(wwork(lwork),stat=ierr)
         if(ierr.ne.0) go to 9200
         allocate(ss(min(nespar,nxrow)),stat=ierr)
         if(ierr.ne.0) go to 9200
         call dgesvd('S','S',nxrow,nespar,x,nxrow1,ss,uu,
     +   ldu,vvt,ldvt,wwork,lwork,info)
         if(info.ne.0)then
           write(errmsg,446)
446        format('Singular value decomposition process encountered '
     +     'an error.')
           go to 9890
         end if
         do i=1,ntrunc
           sum=0.0d0
           do j=1,nxrow
             sum=sum+uu(j,i)*rhs(j)
           end do
           wwork(i)=sum
         end do
         icount=0
         highest_s=ss(1)
         thresh=ss(1)*5.0e-7
         do i=1,ntrunc
           if(ss(i).gt.thresh)then
             icount=icount+1
             wwork(i)=wwork(i)/ss(i)
           else
             ntrunc=icount
             go to 447
           end if
         end do
447      continue
         do i=1,nespar
           sum=0.0d0
           do j=1,ntrunc
             sum=sum+vvt(j,i)*wwork(j)
           end do
           soln(i)=sum
         end do
       end if

C       open(unit=30,file='debug.dat')              !debug
C       if(asoln.eq.'s')then                        !debug
C         do j=1,nespar                               !debug
C           do i=1,nxrow                              !debug
C             x(i,j)=x_keep(i,j)   ! arrays           !debug
C           end do                                    !debug
C         end do                                      !debug
C       end if                                      !debug
C       do irow=1,nxrow                             !debug
C         sum=0.0d0                                 !debug
C         do ies=1,nespar                           !debug
C           sum=sum+x(irow,ies)*soln(ies)           !debug
C         end do                                    !debug
C         write(30,*) sum,temp_rhs(irow)            !debug
C       end do                                      !debug

C       write(30,*)                                 !debug
C       write(30,*) ' solution'                     !debug
C       do ies=1,nespar                             !debug
C         write(30,*) soln(ies)                     !debug
C       end do                                      !debug

C       if(asoln.eq.'s')then                        !debug
C         write(30,*)                               !debug
C         write(30,*) ' singular values'            !debug
C         do i=1,ntrunc                             !debug
C           write(30,*) ss(i)                       !debug
C         end do                                    !debug

C         write(30,*) ' uu'                          !debug
C         do i=1,nxrow                               !debug
C           write(30,555) (uu(i,j),j=1,ntrunc)       !debug
C555        format(8(1pg14.7,1x))                    !debug
C         end do                                     !debug

C         write(30,*) ' vvt'                         !debug
C         do i=1,ntrunc                              !debug
C           write(30,555) (vvt(i,j),j=1,nespar)      !debug
C         end do                                     !debug
C       end if                                       !debug

C -- We now build the output parameter set.

       iess=0
       do ipar=1,npar
         if(itrans(ipar).ge.0)then
           if(radjpar(ipar).lt.-1.0d300) then
             iess=iess+1
             if(itrans(ipar).eq.0)then
               pval(ipar)=pval(ipar)+fract*soln(iess)
             else
               pval(ipar)=log10(pval(ipar))+fract*soln(iess)
               pval(ipar)=10**pval(ipar)
             end if
           else
             if(itrans(ipar).eq.0)then
               pval(ipar)=pval(ipar)+radjpar(ipar)
             else
               pval(ipar)=log10(pval(ipar))+radjpar(ipar)
               pval(ipar)=10**pval(ipar)
             end if
           end if
         end if
       end do
       do ipar=1,npar
         if(itrans(ipar).lt.0)then
           if(itrans(ipar).le.-1000000)then
             pval(ipar)=pval_o(ipar)
           else
             itemp=-itrans(ipar)
             if(pval_o(itemp).eq.0.0d0)then
               write(errmsg,451) trim(apar(itemp))
451            format('Parameter "',a,'" is a parent parameter to ',
     +         'tied parameters in PEST control file. It must not have',
     +         ' a value of zero.')
               go to 9890
             end if
             pval(ipar)=pval(itemp)*pval_o(ipar)/pval_o(itemp)
           end if
         end if
       end do

C -- The parameter value file is written.

       call addquote(parvalfile,afile)
       write(6,509) trim(afile)
509    format(/,' - writing parameter value file ',a,'....')

       if(precis.eq.0)then
         aprecis='single'
       else
         aprecis='double'
       end if
       if(nopnt.eq.0)then
         adpoint='point'
       else
         adpoint='nopoint'
       end if
       write(20,510) trim(aprecis),trim(adpoint)
510    format(1x,a,2x,a)
       do ipar=1,npar
         write(20,520) trim(apar(ipar)),pval(ipar),scale(ipar),
     +   offset(ipar)
520      format(1x,a,t25,3(1pg14.7,3x))
       end do
       close(unit=20)
       write(6,530) trim(parvalfile)
530    format(' - file ',a,' written ok.')

C       write(30,*)                                 !debug
C       write(30,*)                                 !debug
C       do irow=1,nxrow                             !debug
C         ies=0                                     !debug
C         sum=0.0d0                                 !debug
C         do ipar=1,npar                            !debug
C           if(itrans(ipar).ge.0)then               !debug
C             ies=ies+1                             !debug
C             if(itrans(ipar).eq.0)then             !debug
C               dtemp=pval(ipar)-pval_o(ipar)       !debug
C             else                                  !debug
C               dtemp=log10(pval(ipar))-log10(pval_o(ipar))  !debug
C             end if                                !debug
C             sum=sum+xtemp(irow,ies)*dtemp         !debug
C           end if                                  !debug
C         end do                                    !debug
C         write(30,*) sum                           !debug
C       end do                                        !debug

       icount=0
       do ipar=1,npar
         if(itrans(ipar).ge.0)then
           if(pval(ipar).gt.parubnd(ipar)) icount=icount+1
           if(pval(ipar).lt.parlbnd(ipar)) icount=icount+1
         end if
       end do
       if(icount.gt.0)then
         write(6,550)
550      format(/,' Warning: at least one adjustable parameter ',
     +   'now exceeds its bounds ',/,' as defined in PEST ',
     +   'control file.')
       else
         icount=0
         do ipar=1,npar
           if(itrans(ipar).lt.0)then
             if(pval(ipar).gt.parubnd(ipar)) icount=icount+1
             if(pval(ipar).lt.parlbnd(ipar)) icount=icount+1
           end if
         end do
         if(icount.gt.0)then
           write(6,551)
551        format(/,' Warning: at least one fixed or tied parameter ',
     +     'now exceeds its bounds ',/,' as defined in PEST ',
     +     'control file.')
         end if
       end if

       go to 9900

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

9890   continue
       errmsg=' '//trim(errmsg)
       call writmess(6,errmsg)


9900   continue
       call pest_data_deallocate(ifail)
       deallocate(irgp,rownum,stat=ierr)
       deallocate(radjpar,workvec,stat=ierr)
       deallocate(tempcov,s,stat=ierr)
       deallocate(rhs,stat=ierr)
       deallocate(v,w,soln,stat=ierr)
       deallocate(uu,vvt,wwork,ss,stat=ierr)

C       deallocate(xtemp,stat=ierr)                      !debug
C       deallocate(vectemp,stat=ierr)                    !debug
C       deallocate(temp_rhs,stat=ierr)                   !debug
C       deallocate(x_keep,stat=ierr)                     !debug

       close(unit=10,iostat=ierr)
       close(unit=11,iostat=ierr)
       close(unit=20,iostat=ierr)
C       close(unit=30,iostat=ierr)                        !debug

9999   continue
       end



      subroutine aaprod(mode,m,n,xx,yy,leniw,lenrw,iw,rw)

      use pestdata, only : x
      implicit none

      integer mode,m,n,leniw,lenrw
      integer i,j
      integer iw(leniw)
      double precision rw(lenrw)
      double precision xx(n),yy(m)
      double precision sum

      if(mode.eq.1)then
        do i=1,m
          sum=yy(i)
          do j=1,n
            sum=sum+x(i,j)*xx(j)
          end do
          yy(i)=sum
        end do
      else
        do j=1,n
          sum=xx(j)
          do i=1,m
            sum=sum+x(i,j)*yy(i)
          end do
          xx(j)=sum
        end do
      end if

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


       logical function equals(r1,r2)

       double precision,  intent(in)      :: r1,r2
       double precision                   :: rtemp

       rtemp=abs(7.0*spacing(r1))
       if(abs(r1-r2).lt.rtemp)then
         equals=.true.
       else
         equals=.false.
       end if

       end

      SUBROUTINE  DSCAL(N,DA,DX,INCX)
C
C     SCALES A VECTOR BY A CONSTANT.
C     USES UNROLLED LOOPS FOR INCREMENT EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      DOUBLE PRECISION DA,DX(1)
      INTEGER I,INCX,M,MP1,N,NINCX
C
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1)GO TO 20
C
C        CODE FOR INCREMENT NOT EQUAL TO 1
C
      NINCX = N*INCX
      DO 10 I = 1,NINCX,INCX
        DX(I) = DA*DX(I)
   10 CONTINUE
      RETURN
C
C        CODE FOR INCREMENT EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        DX(I) = DA*DX(I)
   30 CONTINUE
      IF( N .LT. 5 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
        DX(I) = DA*DX(I)
        DX(I + 1) = DA*DX(I + 1)
        DX(I + 2) = DA*DX(I + 2)
        DX(I + 3) = DA*DX(I + 3)
        DX(I + 4) = DA*DX(I + 4)
   50 CONTINUE
      RETURN
      END



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

