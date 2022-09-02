       program parvar1

C -- Program PARVAR1 calculates parameter variance terms for a user-supplied number of
C    singular values using SVD of Q(1/2)X matrix. It is modified from PREDVAR1A

       use pestdata, only  : maxcompdim,pestmode,npar,nobsgp,nxrow,
     +                       errmsg,apar,obgnme,aobs,itrans,numcov,
     +                       covind,owght,x,pval_o,modval_o,
     +                       parchglim,pval,parlbnd,parubnd,
     +                       ipargp,scale,offset,dercom,covgp,nespar,
     +                       nobs,nobgnm,covar,nobscov

       implicit none

       logical lexist
       integer lwork,ibeg,iend,ifail,nb_pst,ierr,irestart,
     + iobsgp,itemp,irow,itemp1,itemp2,bn,icount,i,j,ies,
     + ipar,iunit,junit,itype,vardim,covdim,jpartype,
     + jrow,icov,info,k,nb,igroup
       integer maxobs
       integer p1start,pfile
       integer ising
       integer, allocatable :: iregobs(:),iregobsgp(:)
       double precision refvar,dtemp,first,second,third,rtemp
       double precision vt(1,1),parvar(1)
       double precision, allocatable :: workvec(:),w1(:),y(:),
     + cp(:,:),lhs(:,:),w1x(:),yy(:)
       character*4 aext
       character*10 atype,asing,anum
       character*12 aversion
       character*12 aapar
       character*20 aaobs
       character*200 afile,bfile,pestfile,jcofile,uncertfile,outfile
       character*300 dline
       character*12, allocatable :: adjapar(:)

       integer ldvt,iicount,ldu,nxrow1,nsing,icovrow
       integer, allocatable :: iindex(:)
       double precision, allocatable :: ytv2(:)
       double precision, allocatable :: u(:,:),xx(:,:)
       character*15 acov

C -- Initialization

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' PARVAR1 Version ',a,'. Watermark Numerical ',
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

       write(6,*)
140    write(6,150,advance='no')
150    format(' Enter no. of singular values before truncation: ')
       read(5,'(a)') anum
       if(anum.eq.' ') go to 140
       call intread(ifail,anum,ising)
       if(ifail.ne.0) then
         write(6,151)
151      format(/,' Illegal input - try again.',/)
         go to 140
       end if
       if(ising.lt.0)then
         write(6,151)
         go to 140
       end if

       continue
201    write(6,202,advance='no')
202    format(' Enter name for output file: ')
       read(5,'(a)') afile
       if(afile.eq.' ') go to 201
       ibeg=1
       iend=len_trim(afile)
       call getfile(ifail,afile,outfile,ibeg,iend)
       if(ifail.ne.0) go to 201
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

C -- A header is written to the output file.

       call writint(asing,ising)
       write(dline,344) trim(asing)
344    format(' Parameter error variances for ',a,' singular values')
       write(20,'(a)') trim(dline)
       nb=len_trim(dline)
       write(20,343)' ',('-',i=1,nb-1)
343    format(a,100(a))
       write(20,345)
345    format(1x,'parameter',t17,'variance_1',t37,'variance_2',
     + t56,'total_variance',t76,'standard_deviation')

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
240      format('PARVAR1 requires that MAXCOMPDIM be set to 1 ',
     +   'or less in PEST control file ',a,'.')
         go to 9890
       end if
       if(pestmode.eq.2)then
         write(errmsg,250) trim(afile)
250      format('PARVAR1 requires that PEST not be run in ',
     +   'predictive analysis mode in PEST control file ',a,'.')
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

C -- We check that the number of singular values makes sense

       if(ising.gt.nespar)then
         call writint(asing,ising)
         write(errmsg,351) trim(asing)
351      format('Cannot compute error variance for ',a,
     +   ' singular values; this number ',
     +   'of singular values exceeds the number of adjustable ',
     +   'parameters cited in the PEST control file.')
         go to 9890
       end if

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

C -- The number of singular values is worked out. This is the minimum of NESPAR and number of
C    non-regularisation observations for which the weight is nonzero.

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
         write(errmsg,479) trim(afile)
479      format('All observations and prior information cited in ',
     +   'file ',a,' are either zero-weighted or regularisation.')
         go to 9890
       end if
       nsing=min(nxrow1,nespar)

C --   If any covariance matrices were in the original PEST dataset these
C      are now inverted and the square root of them found.

       maxobs=0
       if(numcov.ne.0)then
         do icov=1,numcov
           if(nobscov(icov).gt.maxobs) then
             igroup=covgp(icov)
             if(iregobsgp(igroup).eq.0)then
               maxobs=nobscov(icov)
             end if
           end if
         end do
       end if
       itemp=max(maxobs,nespar)
       allocate(w1(itemp),stat=ierr)
       if(ierr.ne.0) go to 9200
       if(maxobs.ne.0)then
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
           igroup=covgp(icov)
           if(iregobsgp(igroup).ne.0) cycle
           itemp=nobscov(icov)
           if(itemp.eq.0) cycle
           iicount=iicount+1
           call dgesvd('A','N',itemp,itemp,covar(icov)%dval,itemp,
     +     w1,u,maxobs,vt,ldvt,workvec,lwork,info)
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
     +         'PARVAR1 cannot proceed.')
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

       if(maxobs.ne.0)then
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
               do ies=1,nespar
                 rtemp=0.0d0
                 do k=1,nobscov(icov)
                   rtemp=rtemp+
     +                   covar(icov)%dval(icovrow,k)*x(iindex(k),ies)
                 end do
                 xx(jrow,ies)=rtemp
               end do
               if(icovrow.eq.nobscov(icov)) go to 589
             end if
           end do
589        continue
         end do
         deallocate(iindex,stat=ierr)
       end if

       deallocate(x,stat=ierr)

C -- Now we do SVD on the xx matrix.

       write(6,300)
300    format(/,' - carrying out singular value decomposition ',
     + 'of Q(1/2)X....')
       if(allocated(workvec))deallocate(workvec)
       allocate(lhs(nespar,nespar),stat=ierr)
       if(ierr.ne.0) go to 9200
       lwork=max(5*min(nespar,nxrow1)+max(nespar,nxrow1),
     +         5*min(nespar,nxrow1))
       allocate(workvec(lwork),stat=ierr)
       if(ierr.ne.0) go to 9200
       ldu=1
       call dgesvd('N','A',nxrow1,nespar,xx,nxrow1,w1,vt,
     + ldu,lhs,nespar,workvec,lwork,info)
       if(info.ne.0)then
         write(errmsg,310)
310      format('Singular value decomposition process did not ',
     +   'converge.')
         go to 9890
       end if
       if(nsing.lt.nespar)then
         do i=nsing+1,nespar
           w1(i)=0.0d0
         end do
       end if
       do i=1,nsing
         w1(i)=w1(i)*w1(i)
       end do
       deallocate(xx,stat=ierr)

C -- A new parameter name array is built comprised only of adjustable parameters.

       allocate(adjapar(nespar),y(nespar),stat=ierr)
       if(ierr.ne.0) go to 9200
       ies=0
       do ipar=1,npar
         if(itrans(ipar).ge.0) then
           ies=ies+1
           adjapar(ies)=apar(ipar)
         end if
       end do

C -- The parameter uncertainty file is read.

       allocate(cp(nespar,nespar),stat=ierr)
       if(ierr.ne.0) go to 9200
       call addquote(uncertfile,afile)
       write(6,570) trim(afile)
570    format(/,' - reading parameter uncertainty file ',a,'....')
       iunit=12
       junit=15
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

C -- We now start the parameter cycle.

       allocate(w1x(nespar),ytv2(nespar),yy(nespar),stat=ierr)
       if(ierr.ne.0) go to 9200

       write(6,*)
       ies=0
       do ipar=1,npar
         if(itrans(ipar).lt.0) cycle
         ies=ies+1
         y=0.0d0            ! an array
         y(ies)=1.0d0

C         write(6,350) trim(apar(ies))
C350      format(' - computing error variance terms for parameter ',
C     +   a,'...')

C -- The first term is formed

         if(ising.ge.nespar)then
           first=0.0d0
         else
           j=0
           do i=ising+1,nespar
             j=j+1
             rtemp=0.0d0
             do k=1,nespar
               rtemp=rtemp+y(k)*lhs(i,k)
             end do
             ytv2(j)=rtemp
           end do
           do i=1,nespar
             j=0
             rtemp=0.0d0
             do k=ising+1,nespar
               j=j+1
               rtemp=rtemp+ytv2(j)*lhs(k,i)
             end do
             yy(i)=rtemp
           end do
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
         end if

C -- ytv1 is formed.

         if(ising.eq.0)then
           second=0.0d0
         else
           if(ising.gt.nespar)ising=nespar
           do i=1,ising
             if(w1(i).le.1.0e-35)then
               second=1.0e35
               go to 421
             end if
             w1x(i)=1.0/w1(i)
           end do
           do i=1,ising
             rtemp=0.0d0
             do k=1,nespar
               rtemp=rtemp+y(k)*lhs(i,k)
             end do
             ytv2(i)=rtemp
           end do
           second=0.0d0
           do i=1,ising
             second=second+ytv2(i)*w1x(i)*ytv2(i)
           end do
         end if
         second=second*refvar
421      continue

C -- The third term is calculated

         third=first+second

C -- Data is written to the output file.

         write(20,620) trim(apar(ipar)),first,second,third,sqrt(third)
620      format(1x,a,t16,1pg14.7,t36,1pg14.7,t56,1pg14.7,t76,1pg14.7)

       end do

       close(unit=20)
       call addquote(outfile,afile)
       write(6,800) trim(afile)
800    format(' - file ',a,' written ok.')

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
       deallocate(iregobs,iregobsgp,adjapar,cp,lhs,workvec,
     + w1,w1x,yy,y,stat=ierr)

       deallocate(ytv2,u,xx,iindex,stat=ierr)

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

