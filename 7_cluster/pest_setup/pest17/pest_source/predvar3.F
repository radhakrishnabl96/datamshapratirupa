       program predvar3

C -- Program PREDVAR3 automates the calculation of contributions to predictive variance
C    by different parameter types.

       use pestdata, only  : maxcompdim,pestmode,npar,nobsgp,nxrow,
     +                       errmsg,apar,obgnme,aobs,itrans,numcov,
     +                       covind,owght,x,pval_o,modval_o,
     +                       parchglim,pval,parlbnd,parubnd,
     +                       ipargp,scale,offset,dercom,covgp,nespar,
     +                       nobs,nobgnm,covar,nobscov


       implicit none

       integer, parameter :: MAXSING=300
       logical lexist
       integer lwork,ibeg,iend,ifail,nb_pst,ierr,numsing,irestart,
     + iobsgp,itemp,irow,itemp1,itemp2,bn,icount,i,j,ies,nrow,ncol,
     + icode,icurrent,ipar,iunit,junit,itype,vardim,covdim,jpartype,
     + jes,jrow,icov,info,k,jsing,ii,jj,nb,igroup
       integer p1start,pfile
       integer ising(MAXSING)
       integer, allocatable :: iregobs(:),iregobsgp(:)
       double precision refvar,dtemp,first,second,rtemp
       double precision u(1,1),vt(1,1),parvar(1)
       double precision, allocatable :: workvec(:),w1(:),y1(:),y(:),
     + cp(:,:),lhs(:,:),v2tv2(:,:),w1x(:),g(:,:),yy(:),x_o(:,:)
       character*4 aext
       character*10 atype,asing,anum
       character*12 aversion
       character*12 aapar
       character*20 aaobs,apred
       character*200 afile,bfile,pestfile,jcofile,uncertfile,predfile,
     + outfile
       character*300 dline
       character*12, allocatable :: apar1(:),adjapar(:)

       integer, parameter :: MAXPARAM=1500
       integer nesparkeep,iline,nparam,endflag,singmin,iallocmat,
     + iloop,maxobs,job,iicount,iimin
       integer, allocatable :: leaveout(:)
       character*10 aline
       character*12 typename,newtypename
       character*12 listpar(MAXPARAM)
       character*100 cline
       character*200 svdlistfile,paramlistfile
       double precision thirdmin,rcond
       double precision, allocatable :: y_o(:),cp_o(:,:),z(:)
       double precision third(MAXSING),det(2),f1(MAXSING),s1(MAXSING)


C -- Initialization

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' PREDVAR3 Version ',a,'. Watermark Numerical ',
     + 'Computing.',/)

       iallocmat=0
       iloop=0

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
52     format(' Enter observation reference variance: ')
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
110    format(' Enter name of predictive sensitivity matrix file: ')
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

C -- The name of the singular value list file is read.

       write(6,*)
400    write(6,410,advance='no')
410    format(' Enter name of singular value list file: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 400
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,svdlistfile,ibeg,iend)
       if(ifail.ne.0) go to 400
#ifndef UNIX
       call lowcas(svdlistfile)
#endif
       open(unit=14,file=svdlistfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(svdlistfile,afile)
         write(6,95) trim(afile)
         go to 400
       end if

C -- The name of the parameter type list file is read.

500    write(6,510,advance='no')
510    format(' Enter name of parameter type list file: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 500
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,paramlistfile,ibeg,iend)
       if(ifail.ne.0) go to 500
#ifndef UNIX
       call lowcas(paramlistfile)
#endif
       open(unit=15,file=paramlistfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         call addquote(paramlistfile,afile)
         write(6,95) trim(afile)
         go to 500
       end if

C -- The name of the output file is read.

       write(6,*)
210    write(6,220,advance='no')
220    format(' Enter name for predictive error variance output ',
     + 'file: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 210
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,outfile,ibeg,iend)
       if(ifail.ne.0) go to 210
#ifndef UNIX
       call lowcas(outfile)
#endif
#ifdef LF90
       open(unit=23,file=outfile,action='readwrite,denynone',
     + blocksize=1,iostat=ierr)
#else
       open(unit=23,file=outfile,action='write',iostat=ierr)
#endif
       if(ierr.ne.0)then
         call addquote(outfile,afile)
         write(6,230) trim(afile)
230      format(/,' Cannot open file ',a,' for output - try again.',/)
         go to 210
       end if

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
240      format('PREDVAR3 requires that MAXCOMPDIM be set to 1 ',
     +   'or less in PEST control file ',a,'.')
         go to 9890
       end if
       if(pestmode.eq.2)then
         write(errmsg,250) trim(afile)
250      format('PREDVAR3 requires that PEST not be run in predictive ',
     +   'analysis mode in PEST control file ',a,'.')
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
       write(6,275) trim(afile)
275    format(' - file ',a,' read ok.')

C -- We can delete some unused PEST memory.

       deallocate(pval_o,modval_o,parchglim,pval,parlbnd,parubnd,
     + ipargp,scale,offset,dercom,stat=ierr)

C -- If PEST is running in regularisation mode, regularisation
C    observations and prior information equations are now identified.

       allocate(iregobs(nxrow),iregobsgp(nobsgp),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(x_o(nxrow,nespar),stat=ierr)
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

C -- Singular values are now read from singular value list file.

       call addquote(svdlistfile,afile)
       write(6,395) trim(afile)
395    format(/,' - reading singular value list file ',a,'....')
       numsing=0
129    numsing=numsing+1
       if(numsing.gt.MAXSING)then
         write(errmsg,130) trim(afile)
130      format('Too many singular values in file ',a,' - increase ',
     +   'MAXSING and re-compile program.')
         go to 9890
       end if
       read(14,*,end=450,err=9500) ising(numsing)
       if(ising(numsing).lt.0)then
         call writint(anum,numsing)
         write(errmsg,460) trim(anum),trim(afile)
460      format('Singular value number ',a,' from singular value ',
     +   'list file ',a,' is negative.')
         go to 9890
       end if
       go to 129
450    continue
       numsing=numsing-1
       if(numsing.eq.0)then
         write(errmsg,459) trim(afile)
459      format('No singular values were read from file ',a,'.')
         go to 9890
       end if
       close(unit=14)
       call writint(anum,numsing)
       write(6,470) trim(anum),trim(afile)
470    format(' - ',a,' singular values read from file ',a,'.')

C -- The predictive sensitivity file is now read. Note that it is not necessarily
C    assumed that the same parameters are cited in the same order.

       call addquote(predfile,afile)
       write(6,505) trim(afile)
505    format(/' - reading predictive sensitivity matrix file ',a,
     + '....')
       read(13,*,err=9100,end=9150) nrow,ncol,icode
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
       allocate(y1(nrow),apar1(nrow),y(nespar),adjapar(nespar),
     + cp(nespar,nespar),lhs(nespar,nespar),v2tv2(nespar,nespar),
     + g(nespar,nespar),yy(nespar),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(y_o(nespar),cp_o(nespar,nespar),leaveout(nespar),
     + stat=ierr)
       if(ierr.ne.0) go to 9200

       do irow=1,nrow
         read(13,*,err=9100,end=9150) y1(irow)
       end do
       read(13,'(a)',err=9100,end=9150) dline
       call lowcas(dline)
       dline=adjustl(dline)
       if(dline(1:1).ne.'*') go to 9250
       if(index(dline,'row').eq.0) go to 9250
       if(index(dline,'name').eq.0) go to 9250
       do irow=1,nrow
         read(13,*,err=9100,end=9150) apar1(irow)
         call lowcas(apar1(irow))
       end do
       read(13,'(a)',err=9100,end=9150) dline
       call lowcas(dline)
       dline=adjustl(dline)
       if(dline(1:1).ne.'*') go to 9300
       if(index(dline,'col').eq.0) go to 9300
       if(index(dline,'name').eq.0) go to 9300
       read(13,*,err=9100,end=9150) apred
       call lowcas(apred)
       close(unit=13)

C -- A new parameter name array is built comprised only of adjustable parameters.

       ies=0
       do ipar=1,npar
         if(itrans(ipar).ge.0) then
           ies=ies+1
           adjapar(ies)=apar(ipar)
         end if
       end do

C -- The predictive sensitivity vector is now re-arranged (and abridged if necessary).

       ies=0
       icurrent=1
       do ies=1,nespar
         call which1(ifail,nrow,icurrent,apar1,adjapar(ies))
         if(ifail.ne.0)then
           call addquote(pestfile,bfile)
           write(errmsg,550) trim(adjapar(ies)),trim(bfile),trim(afile)
550        format('Parameter "',a,'" is cited as adjustable in PEST ',
     +     'control file ',a,'; however it is not cited in ',
     +     'predictive sensitivity matrix file ',a,'.')
           go to 9890
         end if
         y(ies)=y1(icurrent)
       end do
       write(6,560) trim(afile)
560    format(' - file ',a,' read ok.')

C -- The parameter uncertainty file is read.

       call addquote(uncertfile,afile)
       write(6,570) trim(afile)
570    format(/,' - reading parameter uncertainty file ',a,'....')
       iunit=12
       junit=19
       itype=2
       vardim=1
       covdim=nespar
       atype='parameter'
       call read_uncert_data_file(ifail,iunit,junit,itype,jpartype,
     + nespar,vardim,covdim,parvar,cp,atype,uncertfile,errmsg,
     + dline,adjapar)
       if(ifail.ne.0) then
         errmsg=adjustl(errmsg)
         go to 9890
       end if
       write(6,580) trim(afile)
580    format(' - parameter uncertainty file ',a,' read ok.')

C --   If any covariance matrices were in the original PEST dataset these
C      are now inverted.

       if(numcov.ne.0)then
         maxobs=0
         iicount=0
         do icov=1,numcov
           if(nobscov(icov).gt.maxobs) maxobs=nobscov(icov)
         end do
         if(maxobs.eq.0) go to 584
         write(6,581)
581      format(/,' - inverting observation covariance matrixes...')
         allocate(z(maxobs),stat=ierr)
         if(ierr.ne.0) go to 9200
         do icov=1,numcov
           itemp=nobscov(icov)
           if(itemp.eq.0) go to 585
           iicount=iicount+1
           call dpoco(covar(icov)%dval,itemp,itemp,rcond,z,info)
           if(info.ne.0)then
             write(errmsg,582) trim(obgnme(covgp(icov)))
582          format('The observation covariance matrix supplied for ',
     +       'PEST observation group "',a,'" is not positive ',
     +       'definite.')
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
585        continue
         end do
         deallocate(z,stat=ierr)
         call writint(anum,iicount)
         write(6,583) trim(anum)
583      format(' - ',a,' covariance matrices inverted.')
       end if
584    continue

C -- A copy of various matrices is now made.

       x_o=x
       y_o=y
       cp_o=cp
       nesparkeep=nespar
       endflag=0

C -- This is the start of the big loop.

C -- A header is written to the output file.

       write(23,598) trim(apred)
598    format(' Name of prediction = "',a,'"',/)
       write(23,599)
599    format(' Fixed_params',t20,'First_term',t36,'Second_term',
     + t52,'Minimized_error_variance',t78,'Singular_values_at_minimum')
#ifdef FLUSHFILE
        call flush(23)
#endif


       iline=0
600    continue

C -- The next group of parameters is read.

       call addquote(paramlistfile,afile)
       if(iline.eq.0)then
         cline=' '
         iline=iline+1
         read(15,'(a)',end=618) cline
         call lowcas(cline)
         cline=adjustl(cline)
618      if(cline(1:17).ne.'* parameter type')then
           write(errmsg,610) trim(afile)
610        format('First line of file ',a,' should be "* parameter ',
     +     'type" plus type name.')
           go to 9890
         end if
         cline=cline(18:)
         call remchar(cline,'"')
         call remchar(cline,'''')
         cline=adjustl(cline)
         typename=cline
       end if
       nparam=0
       do
         nparam=nparam+1
         if(nparam.gt.MAXPARAM)then
           write(errmsg,620) trim(afile)
620        format('Too many parameters belong to an individual ',
     +     'parameter type in file ',a,'. Increase MAXPARAM and ',
     +     're-compile program.')
           go to 9890
         end if
621      iline=iline+1
         read(15,'(a)',end=650) cline
         if(cline.eq.' ') go to 621
         cline=adjustl(cline)
         call lowcas(cline)
         if(cline(1:1).eq.'*')then
           if(cline(1:17).ne.'* parameter type')then
             call writint(aline,iline)
             write(errmsg,630) trim(aline),trim(afile)
630          format('Line ',a,' of file ',a,' should be "* parameter ',
     +       'type" followed by type name.')
             go to 9890
           end if
           cline=cline(18:)
           call remchar(cline,'"')
           call remchar(cline,'''')
           cline=adjustl(cline)
           newtypename=cline
           go to 700
         else
           listpar(nparam)=cline
           cycle
         end if
650      endflag=1
         go to 700
       end do

! -- The x matrix and y vector are re-formulated with various rows set to zero.

700    continue
       nparam=nparam-1
       write(6,705) trim(typename)
705    format(/,' - computing pred error variance with param type "',a,
     + '" fixed...',/)

       leaveout=0                  ! an array
       if(nparam.eq.0)then
         x=x_o
         y=y_o
         cp=cp_o
         nespar=nesparkeep
       else
         nespar=0
         ies=0
         do i=1,npar
           if(itrans(i).lt.0) cycle
           ies=ies+1
           aapar=apar(i)
           do j=1,nparam
             if(aapar.eq.listpar(j)) then
               leaveout(ies)=1
               go to 670
             end if
           end do
           nespar=nespar+1
           do irow=1,nxrow
             x(irow,nespar)=x_o(irow,ies)
           end do
           y(nespar)=y_o(ies)
670        continue
         end do

C -- The cp matrix is also reformulated.
C -- But first we check whether we have fixed a parameter which is correlated
C    with a non-fixed parameter.

         do ies=1,nespar
           if(leaveout(ies).eq.1)then
             do jes=1,nespar
               if(leaveout(jes).eq.0)then
                 if(cp(ies,jes).ne.0.0d0)then
                   call addquote(paramlistfile,afile)
                   write(errmsg,671) trim(typename),trim(afile)
671                format('Parameter type "',a,'" from file ',a,
     +             ' instructs PREDVAR3 to freeze a parameter which ',
     +             'is corellated through the C(p) matrix with an ',
     +             'unfrozen parameter. This is not allowed, for ',
     +             'PREDVAR3 does not undertake ',
     +             'conditioning of the C(p) matrix to accomodate ',
     +             'this. Use PREDVAR4 instead.')
                   go to 9890
                 end if
               end if
             end do
           end if
         end do

         i=0
         do ies=1,nesparkeep
           if(leaveout(ies).eq.1) go to 710
           i=i+1
           j=0
           do jes=1,nesparkeep
             if(leaveout(jes).eq.1) go to 720
             j=j+1
             cp(j,i)=cp_o(jes,ies)
720          continue
           end do
710        continue
         end do

       end if


C -- The LHS matrix is zeroed.

       do ies=1,nespar
         do jes=1,nespar
           lhs(jes,ies)=0.0d0
         end do
       end do

C -- If there are any covariance matrixes in PEST control file, pertinent
C    weights are set to zero.

       if(iloop.eq.0)then
         if(numcov.gt.0)then
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
         iloop=1
       end if

C -- The component of XtQX is worked out for observation groups that do not
C    have a covariance matrix.


       write(6,295)
295    format(' - forming XtQX matrix....')
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

       if(numcov.gt.0)then
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
     +             x(irow,ies)*x(jrow,jes)*covar(icov)%dval(jj,ii)
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

C -- Singular value decomposition is now carried out.

       write(6,300)
300    format(' - carrying out singular value decomposition....')
       if(iallocmat.eq.0)then
         lwork=8*nesparkeep
         allocate(workvec(lwork),w1(nesparkeep),w1x(nesparkeep),
     +   stat=ierr)
         if(ierr.ne.0) go to 9200
         iallocmat=1
       end if
       call dgesvd('O','N',nespar,nespar,lhs,nesparkeep,w1,u,
     + nespar,vt,nespar,workvec,lwork,info)
       if(info.ne.0)then
         write(errmsg,310)
310      format('Singular value decomposition process did not ',
     +   'converge.')
         go to 9890
       end if

C -- We now start the singular value cycle.

       do jsing=1,numsing
         call writint(asing,ising(jsing))

C -- I-R is calculated.

         write(6,350) trim(asing)
350      format(' - computing V2tV2 for truncation at ',a,
     +   ' singular values  ....')

         if(ising(jsing).ge.nespar)then
            do i=1,nespar
              do j=1,nespar
                v2tv2(j,i)=0.0d0
              end do
            end do
         else
            if((nespar.le.20).or.(ising(jsing).ge.nespar/2))then
              do i=1,nespar
                do j=1,i
                  rtemp=0.0d0
                  do k=ising(jsing)+1,nespar
                    rtemp=rtemp+lhs(i,k)*lhs(j,k)
                  end do
                  v2tv2(j,i)=rtemp
                end do
              end do
              do i=1,nespar-1
                do j=i+1,nespar
                  v2tv2(j,i)=v2tv2(i,j)
                end do
              end do
            else
              do i=1,nespar
                do j=1,i
                  rtemp=0.0d0
                  do k=1,ising(jsing)
                    rtemp=rtemp+lhs(i,k)*lhs(j,k)
                  end do
                  v2tv2(j,i)=-rtemp
                end do
              end do
              do i=1,nespar
                v2tv2(i,i)=1.0+v2tv2(i,i)
              end do
              do i=1,nespar-1
                do j=i+1,nespar
                  v2tv2(j,i)=v2tv2(i,j)
                end do
              end do
            end if
         end if

C -- The second term matrix is evaluated.

         write(6,390) trim(asing)
390      format(' - computing V1tE(-1)V1 for truncation at ',a,
     +   ' singular values  ....')
         if(ising(jsing).eq.0)then
           do i=1,nespar
             do j=1,nespar
               g(j,i)=0.0d0
             end do
           end do
         else
           if(ising(jsing).gt.nespar)ising(jsing)=nespar
           do i=1,ising(jsing)
             if(w1(i).le.0.0d0)then
               write(errmsg,420)
420            format('Some singular values are too high; ',
     +         'singular values of zero have been encountered.')
               go to 9890
             end if
             w1x(i)=1.0/w1(i)
           end do
           do i=1,nespar
             do j=1,i
               rtemp=0.0d0
               do k=1,ising(jsing)
                 rtemp=rtemp+lhs(i,k)*lhs(j,k)*w1x(k)
               end do
               g(j,i)=rtemp
             end do
           end do
           do i=1,nespar-1
             do j=i+1,nespar
               g(j,i)=g(i,j)
             end do
           end do
         end if

C -- (I-R)y is formed.

         do i=1,nespar
           rtemp=0.0d0
           do j=1,nespar
             rtemp=rtemp+v2tv2(i,j)*y(j)
           end do
           yy(i)=rtemp
         end do

C -- The first term is calculated

         first=0.0d0
         if(jpartype.eq.1)then
           do i=1,nespar
             first=first+yy(i)*yy(i)*cp(i,i)
           end do
         else
           do i=1,nespar
             do j=1,nespar
               first=first+yy(i)*cp(i,j)*yy(j)
             end do
           end do
         end if

C -- The second term is calculated

         second=0.0d0
         do i=1,nespar
           do j=1,nespar
             second=second+y(i)*g(i,j)*y(j)
           end do
         end do
         second=second*refvar

C -- The third term is calculated

         third(jsing)=first+second
         f1(jsing)=first
         s1(jsing)=second

       end do

C -- We now see where the minimum lies.


       thirdmin=1.0e30
       do i=1,numsing
         if(third(i).lt.thirdmin)then
           thirdmin=third(i)
           singmin=ising(i)
           iimin=i
         end if
       end do
       write(23,840) trim(typename),f1(iimin),s1(iimin),thirdmin,
     + singmin
840    format(1x,a,t20,1pg14.7,t36,1pg14.7,t52,1pg14.7,t78,i5)
#ifdef FLUSHFILE
        call flush(23)
#endif

       if(endflag.eq.1) go to 880
       typename=newtypename
       go to 600

880    continue
       close(unit=23)
       call addquote(outfile,afile)
       write(6,800) trim(afile)
800    format(/,' - predictive variance contribution file ',a,
     + ' written ok.')

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
9500   continue
       call writint(anum,numsing)
       write(errmsg,9510) trim(anum),trim(afile)
9510   format('Error reading singular value number ',a,' from ',
     + 'singular value list file ',a,'.')
       go to 9890

9890   continue
       errmsg=' '//trim(errmsg)
       call writmess(6,errmsg)


9900   continue
       call pest_data_deallocate(ifail)
       deallocate(iregobs,iregobsgp,apar1,y1,adjapar,cp,lhs,workvec,
     + w1,v2tv2,w1x,g,yy,stat=ierr)
       deallocate(x_o,cp_o,y_o,leaveout,stat=ierr)

       close(unit=10,iostat=ierr)
       close(unit=11,iostat=ierr)
       close(unit=12,iostat=ierr)
       close(unit=13,iostat=ierr)
       close(unit=14,iostat=ierr)
       close(unit=15,iostat=ierr)
       close(unit=23,iostat=ierr)


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

