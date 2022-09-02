       program predunc4

C -- Program PREDUNC4 automates the calculation of contributions to predictive variance
C    by different parameter types.

       use pestdata, only  : maxcompdim,pestmode,npar,nobsgp,nxrow,
     +                       errmsg,apar,obgnme,aobs,itrans,numcov,
     +                       covind,owght,x,pval_o,modval_o,
     +                       parchglim,pval,parlbnd,parubnd,
     +                       ipargp,scale,offset,dercom,covgp,nespar,
     +                       nobs,nobgnm,covar,nobscov

       implicit none

       integer, parameter :: MAXPARAM=1500

       logical lexist
       integer ibeg,iend,nb_pst,ierr,ifail,itemp1,itemp2,i,j,icount,
     + icurrent,iunit,junit,itype,vardim,covdim,endflag,iline,
     + itemp,jcount,jtemp,k,info,l,i_weight_warn
       integer ialloc_cond,icondflag,ialloc_xcpxt,ialloc_z,ialloc_xcpy
       integer ialloc_zx
       integer irestart,bn,ies,irow,ncol,nrow,icode,ipar,jpartype,
     + nesparkeep,nparam,nespar2,iobsgp,jes,jrow,icov
       integer vchoice,nc,maxnc,ig,ir,ic,ialloc_xx,ialloc_xtqx,
     + iobscov_inv,refmult
       integer p1start,pfile
       integer zero_count,nxrow_s,irow_s,jrow_s

       double precision refvar,dtemp,sumk,suml,sum,precalvar,postcalvar
       double precision refvarinv

       character*5 aext
       character*10 atype,aline
       character*12 aversion,aapar,typename,newtypename
       character*20 aaobs,apred
       character*100 cline
       character*200 afile,pestfile,uncertfile,predfile,
     + paramlistfile,outfile,jcofile,bfile
       character*300 dline

       double precision det(2),parvar(1)
       character*12 listpar(MAXPARAM)

       integer, allocatable          :: leaveout(:)
       integer, allocatable          :: cp_first(:),cp_last(:)
       integer, allocatable          :: zx(:)
       double precision, allocatable :: c22(:,:),c21(:,:)
       double precision, allocatable :: y1(:),y(:),cp(:,:),y_o(:),
     +                                  cp_o(:,:),x_o(:,:)
       double precision, allocatable :: xcpxt(:,:),z(:),xcpy(:),u(:),
     +                                  xcp(:,:)
       double precision, allocatable :: xx(:,:),xtqx(:,:),xtq(:,:)
       character*12, allocatable     :: apar1(:),adjapar(:)

C -- Initialization

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' PREDUNC4 Version ',a,'. Watermark Numerical ',
     + 'Computing.',/)

       ialloc_cond=0
       ialloc_xcpxt=0
       ialloc_xtqx=0
       ialloc_z=0
       ialloc_xcpy=0
       i_weight_warn=0
       iobscov_inv=0
       refmult=0
       ialloc_xx=0
       ialloc_zx=0

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
220    format(' Enter name for predictive uncertainty output ',
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

       write(6,*)
112    write(6,113)
113    format(' Use which version of linear predictive ',
     + 'uncertainty equation:-')
       write(6,114)
114    format('     if version optimized for small number of ',
     + 'parameters   - enter 1')
       write(6,115)
115    format('     if version optimized for small number of ',
     + 'observations - enter 2')
116    write(6,117,advance='no')
117    format(' Enter your choice: ')
       read(5,*,err=116) vchoice
       if((vchoice.ne.1).and.(vchoice.ne.2)) go to 116

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
240      format('PREDUNC4 requires that MAXCOMPDIM be set to 1 ',
     +   'or less in PEST control file ',a,'.')
         go to 9890
       end if
       if(pestmode.eq.2)then
         write(errmsg,250) trim(afile)
250      format('PREDUNC4 requires that PEST not be run in predictive ',
     +   'analysis mode in PEST control file ',a,'.')
         go to 9890
       end if
       if(pestmode.eq.3)then
260      write(errmsg,270) trim(afile)
270      format(' File ',a,' tells PEST to run in ',
     +   'regularisation mode. PEST must be run in estimation mode ',
     +   'for PREDUNC4 to work. Alter the PEST control file and ',
     +   'then use JCO2JCO to build a new Jacobian matrix.')
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

       deallocate(pval_o,modval_o,parchglim,pval,parlbnd,parubnd,
     + ipargp,scale,offset,dercom,stat=ierr)

       allocate(x_o(nxrow,nespar),stat=ierr)
       if(ierr.ne.0) go to 9200

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
     + cp(nespar,nespar),stat=ierr)
       if(ierr.ne.0) go to 9200
       cp=0.0d0
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
599    format(' Fixed_params',t20,'Precal_variance',t40,
     + 'Postcal_variance',
     + t60,'Precal_uncertainty',t80,'Postcal_uncertainty')
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
           listpar(nparam)=trim(cline)
           do i=1,len(listpar(nparam))
             if(listpar(nparam)(i:i).eq.char(9))
     +       listpar(nparam)(i:i)=' '
           end do
           cycle
         end if
650      endflag=1
         go to 700
       end do

! -- The x matrix and y vector are re-formulated with various rows set to zero.

700    continue
       nparam=nparam-1
       write(6,705) trim(typename)
705    format(/,' - computing pred uncertainty with param type "',a,
     + '" fixed...')

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
         nespar2=nesparkeep-nespar
         if(nespar2.eq.0) then
           x=x_o
           y=y_o
           cp=cp_o
           go to 579
         end if

C -- The cp matrix is also reformulated.
C -- But first we check whether we have fixed a parameter which is correlated
C    with a non-fixed parameter.

         icondflag=0
         do ies=1,nespar
           if(leaveout(ies).eq.1)then
             do jes=1,nespar
               if(leaveout(jes).eq.0)then
                 if(cp_o(ies,jes).ne.0.0d0)then
                   icondflag=1
                   go to 672
                 end if
               end if
             end do
           end if
         end do

672      continue
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

         if(icondflag.eq.1)then

           if(ialloc_cond.eq.0)then
             allocate(c22(nesparkeep,nesparkeep),
     +       c21(nesparkeep,nesparkeep),stat=ierr)
             if(ierr.ne.0)then
               write(errmsg,712)
712            format('Cannot allocate sufficient memory for matrix ',
     +         'conditioning.')
               go to 9890
             end if
             ialloc_cond=1
           end if

           i=0
           do ies=1,nesparkeep
             if(leaveout(ies).eq.1) then
               i=i+1
               j=0
               k=0
               do jes=1,nesparkeep
                 if(leaveout(jes).eq.1) then
                   j=j+1
                   c22(j,i)=cp_o(jes,ies)
                 else
                   k=k+1
                   c21(i,k)=cp_o(jes,ies)
                 end if
               end do
             end if
           end do
           call dpofa(c22,nesparkeep,nespar2,info)
           if(info.ne.0)then
             write(errmsg,711)
711          format('The supplied parameter covariance matrix is not ',
     +       'positive definite. Cannot condition it on temporarily ',
     +       'frozen parameters.')
             go to 9890
           end if
           info=1
           call dpodi(c22,nesparkeep,nespar2,det,info)
           do i=1,nespar2-1
             do j=i+1,nespar2
               c22(j,i)=c22(i,j)
             end do
           end do

           do i=1,nespar
             do j=1,i
               sumk=0.0d0
               do k=1,nespar2
                 suml=0.0d0
                 do l=1,nespar2
                   suml=suml+c22(l,k)*c21(l,j)
                 end do
                 sumk=sumk+c21(k,i)*suml
               end do
               cp(j,i)=cp(j,i)-sumk
             end do
           end do

           do i=1,nespar-1
             do j=i+1,nespar
               cp(j,i)=cp(i,j)
             end do
           end do

         end if

       end if
579    continue

       if(vchoice.eq.1) go to 1000

C -- An array is declared for identifying observations with zero weight.

       if(ialloc_zx.eq.0)then
         allocate(zx(nxrow),stat=ierr)
         if(ierr.ne.0) go to 9200
         ialloc_zx=1
       end if

C -- The XC(p)X' matrix is now formed.

       write(6,581)
581    format(' - forming XC(p)X'' matrix....')
       if(ialloc_xcpxt.eq.0)then
         allocate(xcp(nxrow,nesparkeep),xcpxt(nxrow,nxrow),stat=ierr)
         if(ierr.ne.0) go to 9200
         allocate(cp_first(nesparkeep),cp_last(nesparkeep),stat=ierr)
         if(ierr.ne.0) go to 9200
         ialloc_xcpxt=1
       end if
       if(jpartype.eq.1)then
         do irow=1,nxrow
           do ies=1,nespar
             xcp(irow,ies)=x(irow,ies)*cp(ies,ies)
           end do
         end do
       else
         do ies=1,nespar
           do jes=1,nespar
             if(cp(jes,ies).ne.0.0d0)then
               cp_first(ies)=jes
               go to 5811
             end if
           end do
           cp_first(ies)=ies
5811       continue
           do jes=nespar,1,-1
             if(cp(jes,ies).ne.0.0d0)then
               cp_last(ies)=jes
               go to 5812
             end if
           end do
           cp_last(ies)=ies
5812       continue
         end do
         do irow=1,nxrow
           do ies=1,nespar
             sum=0.0d0
             do jes=cp_first(ies),cp_last(ies)
               sum=sum+x(irow,jes)*cp(jes,ies)
             end do
             xcp(irow,ies)=sum
           end do
         end do
       end if
       do irow=1,nxrow
         do jrow=1,irow
           sum=0.0d0
           do ies=1,nespar
             sum=sum+xcp(irow,ies)*x(jrow,ies)
           end do
           xcpxt(jrow,irow)=sum
         end do
       end do
       do irow=1,nxrow-1
         do jrow=irow+1,nxrow
           xcpxt(jrow,irow)=xcpxt(irow,jrow)
         end do
       end do

C -- The C(e) term is now added.

       zero_count=0
       zx=0                         ! an array
       write(6,582)
582    format(' - adding C(e) term....')
       if(numcov.eq.0)then
         do irow=1,nxrow
           dtemp=owght(irow)
           if(dtemp.le.0.0d0)then
             dtemp=1.0d35
             i_weight_warn=1
             zero_count=zero_count+1
             zx(irow)=1
           else
             dtemp=1.0d0/dtemp
           end if
           xcpxt(irow,irow)=xcpxt(irow,irow)+dtemp*refvar
         end do
       else
         do irow=1,nxrow
           itemp=nobgnm(irow)
           if(covind(itemp))then
             continue
           else
             dtemp=owght(irow)
             if(dtemp.le.0.0d0)then
               dtemp=1.0d35
               i_weight_warn=1
               zero_count=zero_count+1
               zx(irow)=1
             else
               dtemp=1.0d0/dtemp
             end if
             xcpxt(irow,irow)=xcpxt(irow,irow)+dtemp*refvar
           end if
         end do
         do icov=1,numcov
           icount=0
           iobsgp=covgp(icov)
           do irow=1,nxrow
             itemp=nobgnm(irow)
             if(itemp.eq.iobsgp)then
               icount=icount+1
               jcount=0
               do jrow=1,nxrow
                 jtemp=nobgnm(jrow)
                 if(jtemp.eq.iobsgp)then
                   jcount=jcount+1
                   xcpxt(jrow,irow)=xcpxt(jrow,irow)+
     +             covar(icov)%dval(jcount,icount)*refvar
                 end if
               end do
             end if
           end do
         end do
       end if

C -- If any weights are zero a new matrix is formulated.

       if(zero_count.ne.0)then
         nxrow_s=nxrow-zero_count
         irow_s=0
         do irow=1,nxrow
           if(zx(irow).ne.1) then
             irow_s=irow_s+1
             jrow_s=0
             do jrow=1,nxrow
               if(zx(jrow).ne.1)then
                 jrow_s=jrow_s+1
                 xcpxt(jrow_s,irow_s)=xcpxt(jrow,irow)
               end if
             end do
           end if
         end do
       else
         nxrow_s=nxrow
       end if

C -- The matrix is now inverted.

       write(6,583)
583    format(' - inverting [XC(p)X''+C(e)] matrix....')
       call dpofa(xcpxt,nxrow,nxrow_s,info)
       if(info.ne.0)then
         write(errmsg,601)
601      format(' Cannot invert [XC(p)X''+C(e)] matrix.')
         go to 9890
       end if

C -- y'C(p) is computed.

       write(6,584)
584    format(' - computing predictive uncertainty....')
       if(ialloc_z.eq.0)then
         allocate(z(nesparkeep),stat=ierr)
         if(ierr.ne.0) go to 9200
         ialloc_z=1
       end if
       do ies=1,nespar
         sum=0.0d0
         do jes=1,nespar
           sum=sum+y(jes)*cp(jes,ies)
         end do
         z(ies)=sum
       end do

C -- y'C(p)y (pre-calibration predictive uncertainty) is computed.

       sum=0.0d0
       do ies=1,nespar
         sum=sum+y(ies)*z(ies)
       end do
       precalvar=sum

C -- XC(p)y is formed.

       if(ialloc_xcpy.eq.0)then
         allocate(xcpy(nxrow),u(nxrow),stat=ierr)
         if(ierr.ne.0) go to 9200
         ialloc_xcpy=1
       end if

       irow_s=0
       do irow=1,nxrow
         if(zx(irow).ne.1)then
           irow_s=irow_s+1
           sum=0.0d0
           do ies=1,nespar
             sum=sum+x(irow,ies)*z(ies)
           end do
           xcpy(irow_s)=sum
         end if
       end do

C -- The second term is formed.

       u=xcpy                          !vectors
       call dposl(xcpxt,nxrow,nxrow_s,u)
       sum=0.0d0
       do irow_s=1,nxrow_s
         sum=sum+xcpy(irow_s)*u(irow_s)
       end do
       postcalvar=precalvar-sum
       go to 839

C -- The following code applies to the small parameter number option.

1000   continue

       write(6,1001)
1001   format(/,' - computing pre-calibration predictive ',
     + 'uncertainty....')
       if(ialloc_xtqx.eq.0)then
         allocate(xtqx(nesparkeep,nesparkeep),z(nesparkeep),stat=ierr)
         if(ierr.ne.0) go to 9200
         ialloc_xtqx=1
       end if
       do ies=1,nespar
         sum=0.0d0
         do jes=1,nespar
           sum=sum+y(jes)*cp(jes,ies)
         end do
         z(ies)=sum
       end do
       sum=0.0d0
       do ies=1,nespar
         sum=sum+y(ies)*z(ies)
       end do
       precalvar=sum

C -- If any observation covariance matrices are present they are inverted.

       refvarinv=1.0d0/refvar
       if(iobscov_inv.eq.0)then
       if(numcov.ne.0)then
         write(6,1010)
1010     format(' - inverting observation covariance matrices....')
         do i=1,numcov
           if(nobscov(i).gt.0)then
             nc=nobscov(i)
             call dpofa(covar(i)%dval,nc,nc,info)
             if(info.ne.0)then
               write(errmsg,1020) trim(obgnme(covgp(i))),trim(pestfile)
1020           format(' Cannot invert covariance matrix supplied for ',
     +         'observation group "',a,'" cited in PEST control ',
     +         'file ',a,'.')
               go to 9890
             end if
             info=1
             call dpodi(covar(i)%dval,nc,nc,det,info)
             do irow=1,nc-1
               do jrow=irow+1,nc
                 covar(i)%dval(jrow,irow)=covar(i)%dval(irow,jrow)
               end do
             end do
             covar(i)%dval=covar(i)%dval*refvarinv    ! arrays
           end if
           ig=covgp(i)
           do irow=1,nxrow
             if(nobgnm(irow).eq.ig)owght(irow)=0.0d0
           end do
         end do
         end if
         iobscov_inv=1
       end if

       write(6,1030)
1030   format(' - forming XtC-1(e)X matrix....')
       if(refmult.eq.0)then
         owght=owght*refvarinv       ! an array
         refmult=1
       end if
       do ies=1,nespar
         do jes=1,ies
           sum=0.0d0
           do irow=1,nxrow
             if(owght(irow).le.0.0d0)i_weight_warn=1
             sum=sum+x(irow,jes)*owght(irow)*x(irow,ies)
           end do
           xtqx(jes,ies)=sum
           xtqx(ies,jes)=sum
         end do
       end do
       if(numcov.ne.0)then
         maxnc=0
         do i=1,numcov
           if(maxnc.le.nobscov(i))maxnc=nobscov(i)
         end do
         if(maxnc.gt.0)then
           if(ialloc_xx.eq.0)then
           allocate(xx(maxnc,nesparkeep),xtq(nesparkeep,maxnc),
     +     stat=ierr)
           if(ierr.ne.0) go to 9200
           ialloc_xx=1
           end if
           do i=1,numcov
             ig=covgp(i)
             nc=nobscov(i)
             ir=0
             do irow=1,nxrow
               if(nobgnm(irow).eq.ig)then
                 ir=ir+1
                 do ies=1,nespar
                   xx(ir,ies)=x(irow,ies)
                 end do
                 if(ir.eq.nc) go to 1040
               end if
             end do
1040         continue
             if(ir.eq.0) go to 1041
             do ic=1,nc
               do ies=1,nespar
                 sum=0.0d0
                 do ir=1,nc
                   sum=sum+xx(ir,ies)*covar(i)%dval(ir,ic)
                 end do
                 xtq(ies,ic)=sum
               end do
             end do
             do jes=1,nespar
               do ies=1,nespar
                 sum=0.0d0
                 do ir=1,nc
                   sum=sum+xtq(ies,ir)*xx(ir,jes)
                 end do
                 xtqx(ies,jes)=xtqx(ies,jes)+sum
               end do
             end do
1041         continue
           end do
         end if
       end if

       write(6,1060)
1060   format(' - inverting C(p) matrix....')
       if(jpartype.eq.1)then
         do ies=1,nespar
           dtemp=cp(ies,ies)
           if(abs(dtemp).lt.1.0d-35)dtemp=1.0e-35
           xtqx(ies,ies)=xtqx(ies,ies)+1.0d0/dtemp
         end do
       else
         call dpofa(cp,nesparkeep,nespar,info)
         if(info.ne.0)then
           write(errmsg,1070) trim(uncertfile)
1070       format(' Cannot invert covariance matrix supplied in ',
     +     'parameter uncertainty file ',a,'.')
           go to 9890
         end if
         info=1
         call dpodi(cp,nesparkeep,nespar,det,info)
         do ies=1,nespar-1
           do jes=ies+1,nespar
             cp(jes,ies)=cp(ies,jes)
           end do
         end do
         do ies=1,nespar
           do jes=1,nespar
             xtqx(jes,ies)=xtqx(jes,ies)+cp(jes,ies)
           end do
         end do
       end if

       write(6,1080)
1080   format(' - inverting [XtC-1(e)X + C-1(p)] matrix....')
       call dpofa(xtqx,nesparkeep,nespar,info)
       if(info.ne.0)then
         write(errmsg,1090)
1090     format(' Cannot invert matrix.')
         go to 9890
       end if
       z=y                           ! Vectors
       call dposl(xtqx,nesparkeep,nespar,z)

       write(6,1100)
1100   format(' - calculating post-calibration predictive ',
     + 'uncertainty....')
       sum=0.0d0
       do ies=1,nespar
         sum=sum+y(ies)*z(ies)
       end do
       postcalvar=sum

839    continue
       if(precalvar.lt.0.0d0) precalvar=0.0d0
       if(postcalvar.lt.0.0d0) postcalvar=0.0d0
       write(23,840) trim(typename),precalvar,postcalvar,
     + sqrt(precalvar),sqrt(postcalvar)
840    format(1x,a,t20,1pg14.7,t40,1pg14.7,t60,1pg14.7,t80,1pg14.7)
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

C       if(i_weight_warn.eq.1)then
C         write(errmsg,810) trim(pestfile)
C810      format('Warning: at least one measurement weight in file ',
C     +   a,' is zero. This was assigned an artificial weight of ',
C     +   '1.0e-35 by PREDUNC4. ',
C     +   'Under some circumstances this may lead to numerical error. ',
C     +   'It is suggested that this observation be removed from the ',
C     +   'PEST control file. Then use JCO2JCO to generate a new ',
C     +   'Jacobian matrix for that file.')
C         go to 9890
C       end if


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

9890   continue
       errmsg=' '//trim(errmsg)
       call writmess(6,errmsg)


9900   continue
       call pest_data_deallocate(ifail)

       if(ialloc_cond.eq.1)then
         deallocate(c22,c21,stat=ierr)
       end if

       if(ialloc_xcpxt.eq.1)then
         deallocate(xcpxt,xcp,stat=ierr)
         deallocate(cp_first,cp_last,stat=ierr)
       end if

       if(ialloc_z.eq.1)then
         deallocate(z,stat=ierr)
       end if

       if(ialloc_xcpy.eq.1)then
         deallocate(xcpy,u,stat=ierr)
       end if

       if(ialloc_xtqx.eq.1)then
         deallocate(xtqx,z,stat=ierr)
       end if

       if(ialloc_xx.eq.1)then
         deallocate(xx,xtq,stat=ierr)
       end if

       if(ialloc_zx.eq.1)then
         deallocate(zx,stat=ierr)
       end if

       deallocate(leaveout,y1,y,cp,y_o,cp_o,x_o,apar1,adjapar,
     + stat=ierr)

       close(unit=11,iostat=ierr)
       close(unit=12,iostat=ierr)
       close(unit=13,iostat=ierr)
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

