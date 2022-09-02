      module types
        implicit none

        type super_obs_struc
          integer num
          integer nobs_noreg
          double precision, allocatable :: s(:)
          double precision, allocatable :: u(:,:)
          double precision, allocatable :: vt(:,:)
          integer, allocatable :: map(:)
          character(len=200)   :: oldpestfile
          character(len=200)   :: newmatfile
          character(len=200)   :: newvtmatfile
        end type super_obs_struc
      end module types

      program supobspar1

C -- Program SUPOBSPAR1 writes super parameters and super observations to matrix files.
C    However, unlike SUPOBSPAR1 it undertakes an implicit KL transformation of parameters.

       use pestdata
       use types

       implicit none

       type(super_obs_struc) super_obs
       integer :: ierr, i, j, irestart,ifail
       integer :: lw(10),rw(10)
       integer :: n,n1
       integer jacdim
       integer :: iunit,junit,itype,vardim,covdim,jpartype
       integer :: ldu,ldvt,lwork,info
       integer :: ies,jes,kes,irow
       integer :: pfile
       double precision  :: dtemp,sum
       double precision  :: parvar(1),vt(1,1)
       character (len=4) :: aext
       character(len=12) :: aversion, atemp
       character(len=15) :: anum
       character(len=20) :: atype
       character(len=200) :: afile, bfile, pestfile, jcofile
       character(len=200) :: outfile,casename
       character(len=200) :: uncertfile
       character(len=600) :: dline

       integer, allocatable          :: istart(:),iend(:)
       double precision, allocatable :: xx(:,:),xxx(:,:)
       double precision, allocatable :: cp(:,:),cpi(:,:)
       double precision, allocatable :: cp1(:),cpi1(:)
       double precision, allocatable :: u(:,:),work(:),s(:)
       character*12, allocatable     :: bpar(:)

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' SUPOBSPAR1 Version ',a,'. Watermark Numerical ',
     + 'Computing.',/)

#ifdef NO_CMDLINE
       write(*,*)
1      write(*,'(a)') ' ENTER COMMAND LINE ARGUMENTS > '
       read(*,'(a)',ERR=1) dline
#else
       call pgetcl(dline)
#endif

C -- The casename is read from the SUPOBSPAR command line.

       if(dline.eq.' ') go to 8900
       call spacesub(dline)
       call linspl(ifail,6,lw,rw,dline)
       if(ifail.eq.0) go to 8900
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

       call intread(ifail,dline(lw(3):rw(3)),super_obs%num)
       if(ifail.ne.0) go to 8900
       if(super_obs%num.le.0) go to 8900

       uncertfile=dline(lw(2):rw(2))
       call remchar(uncertfile,char(211))
#ifndef UNIX
       call lowcas(uncertfile)
#endif

       super_obs%newmatfile=dline(lw(4):rw(4))
       call remchar(super_obs%newmatfile,char(211))
#ifndef UNIX
       call lowcas(super_obs%newmatfile)
#endif

       super_obs%newvtmatfile=dline(lw(5):rw(5))
       call remchar(super_obs%newvtmatfile,char(211))
#ifndef UNIX
       call lowcas(super_obs%newvtmatfile)
#endif

       pestfile=trim(casename)//'.pst'
       jcofile=trim(casename)//'.jco'

       open(unit=10,file=pestfile,status='old',iostat=ierr)
       close(unit=10)
       if(ierr.ne.0)then
         call addquote(pestfile,afile)
         write(errmsg,115) trim(afile)
115      format('Cannot open PEST control file ',a,'.')
         go to 9890
       end if
       super_obs%oldpestfile = pestfile

       outfile=super_obs%newmatfile
       open(unit=20,file=outfile,action='write',iostat=ierr)
       if(ierr.ne.0)then
          call addquote(outfile,afile)
          write(errmsg,311) trim(afile)
 311      format('Cannot open file ',a,' for output.')
          go to 9890
       end if
       close(unit=20)

       outfile=super_obs%newvtmatfile
       open(unit=20,file=outfile,action='write',iostat=ierr)
       if(ierr.ne.0)then
          call addquote(outfile,afile)
          write(errmsg,311) trim(afile)
          go to 9890
       end if
       close(unit=20)

C -- A check is made that the corresponding Jacobian matrix file is present.

       call open_unformatted_file(ierr,11,'read',jcofile,errmsg)
       close(unit=11)
       if(ierr.ne.0)then
         call addquote(jcofile,afile)
         call addquote(pestfile,bfile)
         write(errmsg,60) trim(afile),trim(bfile)
60       format('Cannot open Jacobian matrix file ',a,
     +   ' corresponding to PEST control file ',a,'.')
         go to 9890
       end if

C -- All input data has now been acquired. The complete PEST input
C    dataset is now read.

       call addquote(pestfile,afile)
       write(6,235) trim(afile)
235    format(' - reading PEST control file ',a,'....')
       irestart=0
       p1start=0
       pfile=0
       call read_pest_data(ierr,irestart,p1start,pfile,pestfile)
       if(ierr.ne.0) go to 9890
       if(maxcompdim.gt.1)then
         write(errmsg,240) trim(afile)
240      format('SUPOBSPAR1 requires that MAXCOMPDIM be set to 1 ',
     +   'or less in PEST control file ',a,'.')
         go to 9890
       end if
       if(pestmode.eq.2)then
         write(errmsg,239)
239      format('SUPOBSPAR1 does not allow PEST to be run in ',
     +   'predictive analysis mode.')
         go to 9890
       end if
       if(pestmode.ne.3)then
         do i=1,nxrow
           j=nobgnm(i)
           atemp=obgnme(j)(1:5)
           call lowcas(atemp)
           if(atemp.eq.'regul')then
             write(errmsg,241)
241          format('SUPOBSPAR1 does not allow observations or ',
     +       'prior information equations to belong to an observation ',
     +       'group whose name begins with "regul" unless PEST is ',
     +       'being run in regularisation mode.')
             go to 9890
           end if
         end do
       end if
       if(nobs.ne.nxrow)then
         do i=nobs+1,nxrow
           j=nobgnm(i)
           atemp=obgnme(j)(1:5)
           call lowcas(atemp)
           if(atemp.ne.'regul')then
             write(errmsg,242)
242          format('SUPOBSPAR1 does not allow prior information ',
     +       'to be present within a PEST control file unless it ',
     +       'belongs to a regularisation group.')
             go to 9890
           end if
         end do
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

C -- Additional error checks are made after reading the PEST control file.

       call check_files(super_obs, ierr, errmsg)
       if ( ierr.ne.0 )  goto 9890

C -- Read Jacobian file and compute weighted sensitivities

       allocate(xx(nxrow,nespar),stat=ierr)
       if(ierr.ne.0) go to 9200

       call calcWeightedSens(jcofile, xx, jacdim, ierr, errmsg)
       if ( ierr /=0 )  goto 9890
       deallocate(pval_o,parchglim,pval,parlbnd,parubnd,
     + ipargp,scale,offset,dercom,stat=ierr)

C -- The parameter uncertainty file is now read.

       allocate(cp(nespar,nespar),cpi(nespar,nespar),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(bpar(nespar),stat=ierr)
       if(ierr.ne.0) go to 9200
       ies=0
       do i=1,npar
         if(itrans(i).ge.0)then
           ies=ies+1
           bpar(ies)=apar(i)
         end if
       end do
       call addquote(uncertfile,afile)
       open(unit=10,file=uncertfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(errmsg,569) trim(afile)
569      format('Cannot open parameter uncertainty file ',a,'.')
         go to 9890
       end if
       write(6,570) trim(afile)
570    format(/,' - reading parameter uncertainty file ',a,'....')
       iunit=10
       junit=15
       itype=2
       vardim=1
       covdim=nespar
       atype='parameter'
       call read_uncert_data_file(ifail,iunit,junit,itype,jpartype,
     + nespar,vardim,covdim,parvar,cp,atype,uncertfile,errmsg,
     + cline,bpar)
       if(ifail.ne.0) then
         errmsg=adjustl(errmsg)
         go to 9890
       end if
       if(jpartype.eq.1)then
         allocate(cp1(nespar),cpi1(nespar))
         do ies=1,nespar
           cp1(ies)=cp(ies,ies)
         end do
       end if
       write(6,580) trim(afile)
580    format(' - parameter uncertainty file ',a,' read ok.')

       if(jpartype.eq.1)then
         do ies=1,nespar
           if(cp1(ies).le.0.0d0)then
             write(errmsg,581) trim(afile)
581          format('The covariance matrix supplied in file ',a,
     +       ' is not positive definite and is therefore not a ',
     +       'proper covariace matrix.')
             go to 9890
           end if
           cp1(ies)=sqrt(cp1(ies))
           cpi1(ies)=1.0d0/cp1(ies)
         end do
       else
         allocate(u(nespar,nespar),s(nespar),stat=ierr)
         if(ierr.ne.0) go to 9200
         ldu=nespar
         ldvt=1
         lwork = 20*nespar
         allocate(work(lwork))
         write(6,585)
585      format(/,' - undertaking SVD on C(k) matrix...')
         call dgesvd('A','N',nespar,nespar,cp,nespar,s,u,ldu,
     +   vt,ldvt,work,lwork,info)
C         write(6,586) info
C586      format(' - INFO =',i4)
         write(6,600)
600      format(/,' - formulating FE**(1/2) and E**(-1/2)Ft...')
         do ies=1,nespar
           dtemp=sqrt(s(ies))
           do jes=1,nespar
             cp(jes,ies)=u(jes,ies)*dtemp
           end do
         end do
         do ies=1,nespar
           dtemp=s(ies)
           if(dtemp.le.0.0)then
             write(errmsg,581) trim(afile)
             go to 9890
           end if
           dtemp=1.0d0/sqrt(dtemp)
           do jes=1,nespar
             cpi(ies,jes)=dtemp*u(jes,ies)
           end do
         end do
       end if
       deallocate(u,work,s,stat=ierr)

C -- The Y matrix is evaluated.

       allocate(xxx(nxrow,nespar),stat=ierr)
       if(ierr.ne.0) go to 9200
       write(6,700)
700    format(/,' - formulating ZFE**(1/2)...')

       if(jpartype.eq.1)then
         do ies=1,nespar
           dtemp=cp1(ies)
           do irow=1,nxrow
             xxx(irow,ies)=xx(irow,ies)*dtemp
           end do
         end do
       else
         allocate(istart(nespar),iend(nespar),stat=ierr)
         if(ierr.ne.0) go to 9200
         istart=0    ! an array
         iend=0      ! an array
         do ies=1,nespar
           do jes=1,nespar
             if(cp(jes,ies).ne.0.0)then
               istart(ies)=jes
               go to 1010
             end if
           end do
1010       continue
           if(istart(ies).ne.0)then
             do jes=nespar,1,-1
               if(cp(jes,ies).ne.0.0)then
                 iend(ies)=jes
                 go to 1020
               end if
             end do
1020         continue
           else
             istart(ies)=1
             iend(ies)=1
           end if
         end do

         do irow=1,nxrow
c           if((irow/100)*100.eq.irow) write(6,7011) irow
c7011       format('   matrix row number',i6)
           do ies=1,nespar
             sum=0.0d0
             do jes=istart(ies),iend(ies)
               sum=sum+xx(irow,jes)*cp(jes,ies)
             end do
             xxx(irow,ies)=sum
           end do
         end do
       end if

       deallocate(xx,stat=ierr)

       call CalcSuperObs(xxx, super_obs, ierr, errmsg)
       if ( ierr /=0 )  goto 9890
       deallocate(xxx,stat=ierr)

       write(6,810)
810    format(' - formulating VtE**(-1/2)Ft...')
       cp=super_obs%vt             ! arrays
       if(jpartype.eq.1)then
         do jes=1,nespar
           dtemp=cpi1(jes)
           do ies=1,super_obs%num
             super_obs%vt(ies,jes)=cp(ies,jes)*dtemp
           end do
         end do
       else
         do jes=1,nespar
           do kes=1,nespar
             if(cpi(kes,jes).ne.0.0d0)then
               istart(jes)=kes
               go to 811
             end if
           end do
           istart(jes)=1
           iend(jes)=1
           go to 813
811        continue
           do kes=nespar,1,-1
             if(cpi(kes,jes).ne.0.0d0)then
               iend(jes)=kes
               go to 812
             end if
           end do
812        continue
813        continue
         end do
         do jes=1,nespar
           do ies=1,super_obs%num
             sum=0.0d0
             do kes=istart(jes),iend(jes)
               sum=sum+cp(ies,kes)*cpi(kes,jes)
             end do
             super_obs%vt(ies,jes)=sum
           end do
         end do
       end if

! -- Super parameters are now re-normalized.

       do ies=1,super_obs%num
         sum=0.0d0
         do jes=1,nespar
           dtemp=super_obs%vt(ies,jes)
           dtemp=dtemp*dtemp
           sum=sum+dtemp
         end do
         if(sum.gt.0.0d0)then
           sum=sqrt(1.0d0/sum)
           do jes=1,nespar
             super_obs%vt(ies,jes)=super_obs%vt(ies,jes)*sum
           end do
         end if
       end do

       if(super_obs%newmatfile.ne.' ')then
         call writesupmat(super_obs, ierr, errmsg)
         if ( ierr /=0 )  goto 9890
       end if
       if(super_obs%newvtmatfile.ne.' ')then
         call writesupparmat(super_obs, ierr, errmsg)
         if ( ierr /=0 )  goto 9890
       end if

       go to 9900

8900   continue

       write(6,8910)
8910   format(/,' SUPOBSPAR1 is run using the command:',/)
       write(6,8920)
8920   format('     SUPOBSPAR1 casename uncertfile num obsmatfile ',
     + 'parmatfile ',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8935)
8935   format('     casename   is a PEST control file basename,')
       write(6,8936)
8936   format('     uncertfile is a parameter uncertainty file,')
       write(6,8950)
8950   format('     num        is the number of super obs & par sets ',
     + 'to calculate,')
       write(6,8980)
8980   format('     obsmatfile is the file to which super ',
     + 'observations will be written, and')
       write(6,8981)
8981   format('     parmatfile is the file to which super ',
     + 'parameters will be written.')
       go to 9999

C -- Process errors

9200   write(errmsg,9210)
9210   format('Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9890
9890   continue
       if(errmsg(1:1).ne.' ')errmsg=' '//trim(errmsg)
       call writmess(6,errmsg)
9900   continue
       call pest_data_deallocate(ierr)

       deallocate(super_obs%s, stat=ierr)
       deallocate(super_obs%u, stat=ierr)
       deallocate(super_obs%vt,stat=ierr)
       deallocate(super_obs%map, stat=ierr)


       if(allocated(xx))deallocate(xx, stat=ierr)
       if(allocated(xxx))deallocate(xxx,stat=ierr)
       if(allocated(istart)) deallocate(istart,stat=ierr)
       if(allocated(iend)) deallocate(iend,stat=ierr)
       if(allocated(cp)) deallocate(cp,stat=ierr)
       if(allocated(cp1)) deallocate(cp1,stat=ierr)
       if(allocated(cpi)) deallocate(cpi,stat=ierr)
       if(allocated(cpi1)) deallocate(cpi1,stat=ierr)
       if(allocated(u)) deallocate(u,stat=ierr)
       if(allocated(work)) deallocate(work,stat=ierr)
       if(allocated(s)) deallocate(s,stat=ierr)
       if(allocated(bpar)) deallocate(bpar,stat=ierr)

       close(unit=10,iostat=ierr)
       close(unit=11,iostat=ierr)
       close(unit=15,iostat=ierr)
       close(unit=20,iostat=ierr)

9999   continue

       end


      subroutine CalcSuperObs(xx, so, stat, stat_msg)
C######################################################################

C    This subroutine calculates the SVD of the PEST jacobian array xx, but
C    does not include regularisation and prior information in the
C    calculation.
C       variables:
C         xx(:,:)  = array containing the weighted jacobian from PEST
C         so       = structure of type(super_obs_struc).  The svd information
C                    is stored in this structure
C         stat     = integer flag that signals an error occured
C                    when the subroutine was called
C         stat_smg = character array that stores an error message
C                    describing any errors that occured when the subroutine
C                    was called
C
C    note: this subroutine also uses PEST information stored in the module
C          pestdata
C######################################################################
      use pestdata
      use types
      implicit none
      double precision,intent(inout)       ::  xx(nxrow,nespar)
      type(super_obs_struc), intent(inout) ::  so
      integer, intent(out) :: stat
      character(len=*), intent(inout) ::  stat_msg

      integer i,j, inew, ierr,itemp
      character(len=1) :: jobu, jobvt
      integer :: info, lda, ldu, ldvt, lwork, m, n
      double precision, allocatable :: work(:)
      character(len=500) emsg

      write(6,10)
10    format(' - undertaking SVD of weighted transformed Jacobian...')

C -- Initialize error handling varaibles

      emsg = ""
      ierr=0
      stat = 0
      stat_msg = ""

C --  Calculate the number of observations without regularisation
C     and allocate arrays for svd based on this number.

      m = 0
      do i = 1, nobs
         if(obgnme(nobgnm(i))(1:5) /= 'regul')then
            m = m+1
         end if
      end do
      so%nobs_noreg = m

C -- Initialize variable used by SVD

      jobu = 'S'
      jobvt = 'S'
      n =  nespar

      lda = nxrow
      ldu = m
      ldvt = min(m,n)
      lwork = max(3*min(m,n)+max(m,n),5*min(m,n))
      lwork=lwork+3*max(m,n)   ! for good measure

      if(allocated(so%u)) deallocate(so%u)
      if(allocated(so%vt)) deallocate(so%vt)
      if(allocated(so%s)) deallocate(so%s)
      if(allocated(so%map))deallocate(so%map)
      allocate(so%u(ldu, min(m,n)), stat=ierr)
      if (ierr /= 0) goto 9200
      allocate( so%map(m), stat=ierr)
      if (ierr /= 0) goto 9200
      allocate(so%vt(ldvt,n), stat=ierr)
      if (ierr /= 0) goto 9200

      x=0           ! an array
      inew = 0

C -- Populate the new jacobian matrix(a) which does not include regularisation
C -- and prior information.

      do i = 1, nobs
         if(obgnme(nobgnm(i))(1:5) /= 'regul') then
            inew = inew +1
            do j=1, n
               x(inew,j) = xx(i,j)
            end do
           so% map(inew) = i
         end if
      end do

      allocate(so%s(min(m,n)), stat=ierr)
      if (ierr /= 0) goto 9200
      allocate( work(lwork), stat=ierr)
      if (ierr /= 0) goto 9200

C --  Let SVD calculate to optimium size of the work vector
C     and resize it accordingly

      lwork = -1
      call dgesvd(jobu, jobvt, m, n, x, lda, so%s, so%u, ldu, so%vt,
     +     ldvt, work, lwork, info)
      itemp=nint(work(1))
      if(itemp.gt.lwork)then
        lwork = itemp
        deallocate(work, stat=ierr)
        allocate( work(lwork), stat=ierr)
        if (ierr /= 0) goto 9200
      end if

C --  Call SVD

      call dgesvd(jobu, jobvt, m, n, x, lda, so%s, so%u, ldu, so%vt,
     +     ldvt, work, lwork, info)

      if(info /= 0) goto 9220

c      write(6,20)
c20    format(' - calculation of super observations/parameters ',
c     +'complete.')
      goto 9900 ! finish with out any error

C -- Handle Errors

 9200 write(emsg,9205)
 9205 format(' Cannot allocate sufficient memory to continue ',
     +     'execution.')
      go to 9800
 9220  write(emsg,9225)
 9225  format(' Error: failed to compute SVD')
       go to 9800
 9800 continue
      stat = 1
      stat_msg = emsg

 9900 continue
      if(allocated(work)) deallocate(work, stat=ierr)

      end subroutine CalcSuperObs




      subroutine check_files(so, stat, stat_msg)
C######################################################################
C    This subroutine checks the PEST file to insure that all the features
C    and syntax are compatable with superobservations
C         so       = structure of type(super_obs_struc).   Super observation
C                    information is stored in this structure
C         stat     = integer flag that signals an error occured
C                    when the subroutine was called
C         stat_smg = character array that stores an error message
C                    describing any errors that occured when the subroutine
C                    was called
C
C    note: this subroutine also uses PEST information stored in the module
C          pestdata
C######################################################################
      use pestdata
      use types
      implicit none
      type(super_obs_struc), intent(inout) :: so
      integer, intent(out) :: stat
      character(len=*), intent(inout) ::  stat_msg

      integer i,j, m, ierr, itmp, itype, ifl
      logical:: found
      character(len=len(aobs(1))) tmpStr
      integer :: nobs_in_file
      character(len=20) :: aobgrp
      character(len=2000) :: emsg
      character(len=100) :: atemp
      character(len=200) :: afile

C -- Initialize error handling variables

      emsg = ""
      ierr=0
      stat = 0
      stat_msg = ""

C -- Calculate the number of observations without regularisation
C    and allocate arrays for svd based on this number

      m = 0
      do i = 1, nobs
         if(obgnme(nobgnm(i))(1:5) /= 'regul')then
            m = m+1
         end if
      end do
      so%nobs_noreg = m

C -- Check that number of super observation is less than number of observations
      if(so%num >=so%nobs_noreg) then
         ierr = 1
         emsg = 'Error: requested number of super observations must '
     +        // 'be less than the number of non-regularisation '
     +        // 'observations in the PEST control file.'
         goto 9800
      end if

C -- Check that the number of super observations is less than the number of adjustable parameters

      if(so%num > nespar) then
         ierr = 1
         emsg = 'Error: requested number of super observations must '
     +        // 'not exceed the number of adjustable parameters '
     +        // 'in the PEST control file.'
         goto 9800
      end if

      goto 9900  ! no errors

C     Process error messages
 9800 continue
      stat = 1
      stat_msg = emsg
 9900 continue
      end subroutine check_files




      subroutine writeSupMat(so, stat, stat_msg)
C######################################################################
C     This subroutine writes super observation eigenvectors in PEST matrix
C     file format.
C       variables:
C         so       = structure of type(super_obs_struc).   Super observation
C                    information is stored in this structure
C         stat     = integer flag that signals an error occured
C                    when the subroutine was called
C         stat_smg = character array that stores an error meesage
C                    describing any errors that occured when the subroutine
C                    was called
C
C    note: this subroutine also uses PEST information stored in the module
C          pestdata
C######################################################################
       use types
       use pestdata
       implicit none

       type(super_obs_struc), intent(inout)      :: so
       integer, intent(out)                      :: stat
       character(len=*), intent(inout)           :: stat_msg

       integer   :: j,ierr,i
       character(len=25)  :: anum
       character(len=500) :: emsg
       character(len=200) :: afile

C -- Initialize error handling varaibles

       emsg = ""
       ierr=0
       stat = 0
       stat_msg = ""

       write(6,10)
10     format(/,' - writing super observation matrix file...')

C -- The matrix file is written.

       open(unit=15,file=so%newmatfile,action='write',err=9400)
       write(15,'(3i12)',err=9400) so%nobs_noreg,so%num,2
       do i=1,so%nobs_noreg
         write(15,15,err=9400) (so%u(i,j) , j=1,so%num)
15       format(8(1x,1pg14.7))
       end do
       write(15,20,err=9400)
20     format('* row names')
       do j = 1, nobs
         if(obgnme(nobgnm(j))(1:5) /= 'regul')then
           write(15,'(1x,a)',err=9400) trim(aobs(j))
         end if
       end do
       write(15,30,err=9400)
30     format('* column names')
       do j=1,so%num
         call writint(anum,j)
         anum=adjustl(anum)
         anum='eig'//trim(anum)
         write(15,'(1x,a)',err=9400) trim(anum)
       end do

       close(unit=15)
       call addquote(so%newmatfile,afile)
       write(6,90) trim(afile)
90     format(' - superobservation matrix file ',a,' written ok.')

       go to 9900
9400   call addquote(so%newmatfile,afile)
       write(emsg,9410) trim(afile)
9410   format('Cannot write to file ',a,'.')
       go to 9800

9800  continue
      stat = 1
      stat_msg = emsg

9900   continue
      end subroutine writeSupMat


      subroutine writeSupParMat(so, stat, stat_msg)
C######################################################################
C     This subroutine writes super parameter eigenvectors in PEST matrix
C     file format.
C       variables:
C         so       = structure of type(super_obs_struc).   Super observation
C                    information is stored in this structure
C         stat     = integer flag that signals an error occured
C                    when the subroutine was called
C         stat_smg = character array that stores an error meesage
C                    describing any errors that occured when the subroutine
C                    was called
C
C    note: this subroutine also uses PEST information stored in the module
C          pestdata
C######################################################################
       use types
       use pestdata
       implicit none

       type(super_obs_struc), intent(inout)      :: so
       integer, intent(out)                      :: stat
       character(len=*), intent(inout)           :: stat_msg

       integer   :: j,ierr,i
       character(len=25)  :: anum
       character(len=500) :: emsg
       character(len=200) :: afile

C -- Initialize error handling varaibles

       emsg = ""
       ierr=0
       stat = 0
       stat_msg = ""

       write(6,10)
10     format(/,' - writing super parameter matrix file...')

C -- The matrix file is written.

       open(unit=15,file=so%newvtmatfile,action='write',err=9400)
       write(15,'(3i12)',err=9400) nespar,so%num,2
       do j=1,nespar
         write(15,15,err=9400) (so%vt(i,j) , i=1,so%num)
15       format(8(1x,1pg14.7))
       end do
       write(15,20,err=9400)
20     format('* row names')
       do j = 1, npar
         if(itrans(j).ge.0)then
           write(15,'(1x,a)',err=9400) trim(apar(j))
         end if
       end do
       write(15,30,err=9400)
30     format('* column names')
       do j=1,so%num
         call writint(anum,j)
         anum=adjustl(anum)
         anum='eig'//trim(anum)
         write(15,'(1x,a)',err=9400) trim(anum)
       end do

       close(unit=15)
       call addquote(so%newvtmatfile,afile)
       write(6,90) trim(afile)
90     format(' - superparameter matrix file ',a,' written ok.')

       go to 9900
9400   call addquote(so%newvtmatfile,afile)
       write(emsg,9410) trim(afile)
9410   format('Cannot write to file ',a,'.')
       go to 9800

9800  continue
      stat = 1
      stat_msg = emsg

9900   continue
      end subroutine writeSupParMat



      subroutine calcWeightedSens(jcofile, xx, jacdim, stat, stat_msg)
C######################################################################
C     This subroutine reads the PEST jacobian file and calculates
C     the weighted sesitivity matrix
C       varaibles:
C         jcofile = the name of the jacobian file to be read
C         xx      = an allocable array to which the weighted sensitivity
C                   values will be written
C         jacdim  = leading dimension of matrix in JCO file
C         stat    = integer flag that signals an error occured
C                    when the subroutine was called
C         stat_msg = character array that stores an error message
C                    describing any errors that occured when the subroutine
C                    was called
C
C    note: this subroutine also uses PEST information stored in the module
C          pestdata
C######################################################################
      use pestdata
      implicit none
      character(len=*), intent(in)    :: jcofile
      double precision, intent(inout) :: xx(nxrow,nespar)
      integer, intent(out) :: jacdim
      integer, intent(out) :: stat
      character(len=*), intent(inout) ::  stat_msg

      character(len=500) :: emsg
      character(len=200) :: afile
      character(len=20) :: aaobs
      character(len=12) :: aapar
      character(len=15) :: acov
      character(len=10):: anum
      integer :: ierr, itemp, itemp1, itemp2, bn, igroup
      integer :: icount, i, j, k, ies, irow
      integer :: maxobs, jcov, lwork, info
      integer :: ldvt,iicount,icovrow
      double precision :: dtemp, rtemp
      double precision, allocatable:: workvec(:), w1(:)
      double precision, allocatable::  u(:,:)
      double precision vt(1,1)
      integer, allocatable :: iindex(:)

C     Initialize error handling varaibles
      emsg = ""
      ierr=0
      stat = 0
      stat_msg = ""

      call addquote(jcofile,afile)
      write(6,5) trim(afile)
5     format(/,' - reading Jacobian matrix file ',a,'...')
      call open_unformatted_file(ierr,11,'read',jcofile,emsg)
      if(ierr.ne.0)then
          write(emsg,FMT='(2a)')
     +         'Cannot open Jacobian matrix file ',
     +         trim(afile)
         go to 9800
       end if

       read(11,err=9000,end=9000)itemp1,itemp2
       if(abs(itemp1).ne.nespar) go to 9250
       if(itemp1.lt.0)then
         continue
       else
         write(emsg,290) trim(afile)
290      format('Jacobian matrix file ',a,' was not produced by ',
     +   'version 8 or later of PEST; use program JCOTRANS to ',
     +   'convert this to new format.')
         go to 9800
       end if
       itemp2=abs(itemp2)
       bn=itemp2
       jacdim=bn
       if((itemp2.ne.nxrow).and.(itemp2.ne.nobs)) go to 9250
       do i=1,nespar
         do j=1,bn
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
         if(aapar.ne.apar(i)) go to 9250
       end do
       do i=1,bn
         read(11,err=9000,end=9000) aaobs
         call lowcas(aaobs)
         if(aaobs.ne.aobs(i)) go to 9250
       end do
       call addquote(jcofile,afile)
       write(6,291) trim(afile)
291    format(' - file ',a,' read ok.')

C --   If any covariance matrices were in the original PEST dataset these
C      are now inverted and the square root of them found.

       maxobs=0
       if(numcov.ne.0)then
         do jcov=1,numcov
           if(nobscov(jcov).gt.maxobs) maxobs=nobscov(jcov)
         end do
       end if
       if(maxobs.eq.0) go to 584
       itemp=max(maxobs,nespar)
       allocate(w1(itemp),stat=ierr)
       if(ierr.ne.0) go to 9200
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
         do jcov=1,numcov
           itemp=nobscov(jcov)
           if(itemp.eq.0) cycle
           iicount=iicount+1
           call dgesvd('A','N',itemp,itemp,covar(jcov)%dval,itemp,
     +     w1,u,maxobs,vt,ldvt,workvec,lwork,info)
           if(info.ne.0)then
             call writint(acov,jcov)
             write(emsg,587) trim(acov)
587          format('Cannot undertake SVD on covariance matrix ',
     +       'number ',a,' cited in PEST control file.')
             go to 9800
           end if
           do i=1,itemp
             if(w1(i).le.1.0d-200)then
               call writint(acov,jcov)
               write(emsg,582) trim(acov)
582            format('Observation covariance matrix number ',a,
     +         ' cited in PEST control file is singular. ',
     +         'SUPOBSPAR cannot proceed.')
               go to 9800
             end if
             w1(i)=1.0d0/(sqrt(w1(i)))
           end do
           do j=1,itemp
             do i=1,j
               rtemp=0.0d0
               do k=1,itemp
                 rtemp=rtemp+u(i,k)*u(j,k)*w1(k)
               end do
               covar(jcov)%dval(i,j)=rtemp
             end do
           end do
           do j=1,itemp-1
             do i=j+1,itemp
               covar(jcov)%dval(i,j)=covar(jcov)%dval(j,i)
             end do
           end do
         end do
         call writint(anum,iicount)
         write(6,583) trim(anum)
583      format(' - ',a,' covariance matrices decomposed.')
         deallocate(u,stat=ierr)
       end if
584    continue

C -- This new matrix is now filled - first where there is no covariance matrix.

       write(6,579)
579    format(/,' - calculating weighted sensitivity matrix...')
       do irow=1,nxrow
         itemp=nobgnm(irow)
         if(covind(itemp)) cycle
         rtemp=sqrt(owght(irow))
         do ies=1,nespar
           xx(irow,ies)=x(irow,ies)*rtemp
         end do
       end do

C -- Now where there are observation covariance matrices.

       if(numcov.ne.0)then
         allocate(iindex(maxobs),stat=ierr)
         if(ierr.ne.0) go to 9200
         do jcov=1,numcov
           if(nobscov(jcov).eq.0) cycle
           igroup=covgp(jcov)
           icovrow=0
           do irow=1,nxrow
             if(nobgnm(irow).eq.igroup)then
               icovrow=icovrow+1
               iindex(icovrow)=irow
               if(icovrow.eq.nobscov(jcov)) go to 585
             end if
           end do
585        continue
           icovrow=0
           do irow=1,nxrow
             itemp=nobgnm(irow)
             if(nobgnm(irow).eq.igroup)then
               icovrow=icovrow+1
               do ies=1,nespar
                 rtemp=0.0d0
                 do k=1,nobscov(jcov)
                   rtemp=rtemp+
     +                   covar(jcov)%dval(icovrow,k)*x(iindex(k),ies)
                 end do
                 xx(irow,ies)=rtemp
               end do
               if(icovrow.eq.nobscov(jcov)) go to 589
             end if
           end do
589        continue
         end do
       end if
       write(6,588)
588    format(' - weighted sensitivity matrix calculated ok.')

       goto 9900                ! no errors

C --  Process error messages

 9000  write(emsg,FMT='(3a)')
     +      'Error encountered in reading Jacobian matrix file "',
     +      trim(jcofile), '".'
       goto 9800
 9200  write(emsg,9210)
 9210  format('Cannot allocate sufficient memory to continue ',
     + 'execution.')
       go to 9800
 9250  write(emsg, FMT='(6a)')
     +      'Jacobian matrix file ',trim(jcofile),
     +      ' is not compatible ',
     +      'with current PEST control file; ',
     +      'at least one observation ',
     +      'or adjustable parameter is different between them.'
       go to 9800
 9800  continue
       stat = 1
       stat_msg = emsg
 9900  continue
       close(unit=11,iostat=ierr)
       if(allocated(w1))      deallocate(w1,stat=ierr)
       if(allocated(workvec)) deallocate(workvec,stat=ierr)
       if(allocated(iindex))   deallocate(iindex,stat=ierr)
       if(allocated(u)) deallocate(u,stat=ierr)

       end subroutine calcweightedsens


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

c       Subroutine WRITINT writes an integer to a character variable.

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
21      leadblank=i-1
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



        subroutine lowcase(astrng)

C -- Subroutine lowcase converts a string to lower case.

        integer i,j
        character*(*) astrng

        do 10 i=1,len_trim(astrng)
        j=ichar(astrng(i:i))
        if((j.ge.65).and.(j.le.90)) astrng(i:i)=char(j+32)
10      continue
        return
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


C -- The first subroutine in this file is also in certread.f in the c:\cma\programs directory.
C    However there are some slight alterations. Any changes made here should be made to
C    certread.f as well.


        subroutine read_uncert_data_file(ifail,iunit,junit,itype,jtype,
     +  nobs,vardim,covdim,var,covar,atype,covdatfile,amessage,cline,
     +  aobs)

C -- Subroutine READ_UNCERT_DATA_FILE reads an uncertainty data file.

        implicit none

        integer    :: ifail         ! return as zero if successful
        integer    :: iunit         ! unit number from which file will be read
        integer    :: junit         ! unit number from which another file can be read
        integer    :: itype         ! 1 if only variances permitted; 2 if also covariances
        integer    :: jtype         ! return as 1 if only variances supplied; 2 if also covars
        integer    :: nobs          ! number of items for which uncertainty required
        integer    :: vardim        ! dimension of variance vector
        integer    :: covdim        ! dimension of covariance matrix

        double precision  :: var(vardim)            ! variance vector
        double precision  :: covar(covdim,covdim)   ! covariance matrix

        character*(*)  :: atype      ! name of data type for which information is sought
        character*(*)  :: covdatfile ! name of the covariance data file
        character*(*)  :: amessage   ! string to write error message to
        character*(*)  :: cline      ! a general text string
        character*(*)  :: aobs(nobs) ! names of data elements

        integer          :: iline,iobs,j,jfail,nb,currentobs,i,icount,
     +                      priorflag,k,ient
        integer          :: lw(2),rw(2)
        double precision :: vmul,stdmul,dtemp

        character*4      :: aext
        character*10     :: aline
        character*20     :: atemp,ablock,akey
        character*200    :: afile,pestfile,matfile,bfile


C -- Initialisation

        ifail=0
        call addquote(covdatfile,afile)
        iline=0

        if(itype.eq.1)then
          do iobs=1,nobs
            var(iobs)=-1.1d35
          end do
        else
          covar=0.0d0
          do iobs=1,nobs
            covar(iobs,iobs)=-1.1d35
          end do
        end if
        jtype=1

        currentobs=0
        pestfile=' '

C -- We now look for a block.

100     iline=iline+1
        read(iunit,'(a)',err=9000,end=5000) cline
        if(cline.eq.' ') go to 100
        if(cline(1:1).eq.'#') go to 100
        call writint(aline,iline)
        cline=adjustl(cline)
        call linspl(jfail,2,lw,rw,cline)
        if(jfail.ne.0) go to 9100
        atemp=cline(lw(1):rw(1))
        call lowcas(atemp)
        if(atemp.ne.'start')then
          write(amessage,120) trim(aline),trim(afile)
120       format(' Block should commence with START at line ',
     +    a,' of file ',a,'.')
          go to 9800
        end if
        ablock=cline(lw(2):rw(2))
        call lowcas(ablock)
        if(ablock(1:12).eq.'pest_control')then
          if(atype.eq.'parameter')then
            call writint(aline,iline)
            write(amessage,1005) trim(aline),trim(afile)
1005        format(' A PEST_CONTROL_FILE block is cited on line ',a,
     +      ' of file ',a,'; this is only allowed for observations - ',
     +      'not parameters.')
            go to 9800
          end if
          vmul=1.0d0
1010      continue
          iline=iline+1
          call writint(aline,iline)
          read(iunit,'(a)',err=9000,end=9050) cline
          if(cline.eq.' ') go to 1010
          if(cline(1:1).eq.'#') go to 1010
          call spacesub(cline)
          call linspl(jfail,2,lw,rw,cline)
          if(jfail.ne.0) go to 9100
          akey=cline(lw(1):rw(1))
          call lowcas(akey)
          if(akey.eq.'file')then
            pestfile=cline(lw(2):)
            call remchar(pestfile,char(211))
#ifndef UNIX
            call lowcas(pestfile)
#endif
            nb=len_trim(pestfile)
            i=nb-3
            if(i.lt.1) i=1
            aext=pestfile(i:nb)
            call lowcas(aext)
            if(aext.ne.'.pst')then
              write(amessage,1015) trim(aline),trim(afile)
1015          format(' PEST control file must have an extension of ',
     +        '".pst" at line ',a,' of file ',a,'.')
              go to 9800
            end if
            go to 1010
          else if(akey.eq.'variance_multiplier')then
            atemp=cline(lw(2):rw(2))
            call drealrd(jfail,atemp,vmul)
            if(jfail.ne.0)then
              write(amessage,1020) trim(aline),trim(afile)
1020          format(' Error reading variance multiplier from line ',
     +        a,' of file ',a,'.')
              go to 9800
            end if
            if(vmul.le.0.0d0)then
              write(amessage,1030) trim(aline),trim(afile)
1030          format(' Variance multiplier must be greater than ',
     +        'zero at line ',a,' of file ',a,'.')
              go to 9800
            end if
            go to 1010
          else if(akey.eq.'end')then
            if(pestfile.eq.' ')then
              write(amessage,1035) trim(afile)
1035          format(' No FILE keyword has been provided in ',
     +        '"PEST_CONTROL_FILE" block of file ',a,'.')
              go to 9800
            end if
            priorflag=1
            call read_pest_control_file(jfail,junit,itype,jtype,
     +      priorflag,vardim,covdim,nobs,vmul,var,covar,pestfile,
     +      amessage,aobs,cline)
            if(jfail.gt.0) go to 9800
            if((jfail.eq.-1).or.(jfail.eq.-11))then
              call addquote(pestfile,bfile)
              write(amessage,1200) trim(bfile)
1200          format(' Warning: PEST control file ',a,' cites at ',
     +        'least one covariance matrix file in "observation ',
     +        'groups" section. For present version ',
     +        'of PARAMERR this covariance file is ignored and ',
     +        'observation weights are used instead for observation ',
     +        'variance computation.')
              call writmess(6,amessage)
              if(jfail.gt.-10) write(6,*)
            end if
            if(jfail.le.-10)then
              call addquote(pestfile,bfile)
              write(amessage,1210) trim(bfile)
1210          format(' Warning: at least one weight in PEST control ',
     +        'file ',a,' is supplied as zero. This was altered to ',
     +        '1.0E-30 for the purpose of observation variance ',
     +        'calculation.')
              call writmess(6,amessage)
              write(6,*)
            end if
            go to 100
          else
            call writint(aline,iline)
            write(amessage,1190) trim(akey),trim(aline),trim(afile)
1190        format(' Unknown keyword "',a,'" at line ',a,' of file ',
     +      a,'.')
            go to 9800
          end if

        else if(ablock(1:12).eq.'standard_dev')then
          stdmul=1.0d0
          ient=0
2010      continue
          iline=iline+1
          read(iunit,'(a)',err=9000,end=9250) cline
          if(cline.eq.' ') go to 2010
          if(cline(1:1).eq.'#') go to 2010
          ient=ient+1
          call linspl(ifail,2,lw,rw,cline)
          if(ifail.ne.0) go to 9100
          atemp=cline(lw(1):rw(1))
          call lowcas(atemp)
          if(atemp.eq.'end')then
            atemp=cline(lw(2):rw(2))
            call lowcas(atemp)
            if(atemp(1:12).eq.'standard_dev') go to 100
          end if
          if(atemp.eq.'std_multiplier')then
            if(ient.ne.1)then
              call writint(aline,iline)
              write(amessage,2015) trim(aline),trim(afile)
2015          format(' If a STD_MULTIPLIER is supplied, it must be ',
     +        'the first entry in a STANDARD_DEVIATION block. This ',
     +        'is violated on line ',a,' of file ',a,'.')
              go to 9800
            end if
            atemp=cline(lw(2):rw(2))
            call drealrd(jfail,atemp,stdmul)
            if(jfail.ne.0)then
              call writint(aline,iline)
              write(amessage,2020) trim(aline),trim(afile)
2020          format(' Cannot read standard deviation multiplier from ',
     +        'line ',a,' of file ',a,'.')
              go to 9800
            else
              if(stdmul.le.0.0d0)then
                call writint(aline,iline)
                write(amessage,2030) trim(aline),trim(afile)
2030            format(' Standard deviation multiplier must be ',
     +          'positive at line ',a,' of file ',a,'.')
                go to 9800
              end if
            end if
            stdmul=stdmul*stdmul
            go to 2010
          end if
          call whichone(jfail,nobs,currentobs,aobs,atemp)
          if(jfail.ne.0) go to 2010
          atemp=cline(lw(2):rw(2))
          call drealrd(jfail,atemp,dtemp)
          if(jfail.ne.0)then
            call writint(aline,iline)
            write(amessage,2050) trim(aline),trim(afile)
2050        format(' Cannot read standard deviation at line ',a,
     +      ' of file ',a,'.')
            go to 9800
          end if
          if(dtemp.le.0.0)then
            call writint(aline,iline)
            write(amessage,2060) trim(aline),trim(afile)
2060        format(' Standard deviation must be positive at ',
     +      'line ',a,' of file ',a,'.')
            go to 9800
          end if
          dtemp=dtemp*dtemp*stdmul
          if(itype.eq.1)then
            if(var(currentobs).gt.-1.0d35) go to 9300
            var(currentobs)=dtemp
          else
            if(covar(currentobs,currentobs).gt.-1.0d35) go to 9300
            covar(currentobs,currentobs)=dtemp
          end if
          go to 2010

        else if(ablock(1:14).eq.'covariance_mat')then
          if(itype.eq.1)then
            write(amessage,3010) trim(afile),trim(atype)
3010        format(' A "covariance_matrix_file" block exists in file ',
     +      a,'; this is not presently allowed for characterisation ',
     +      'of ',a,' data variance.')
            go to 9800
          end if
          jtype=2
          vmul=1.0d0
3020      iline=iline+1
          read(iunit,'(a)',err=9000,end=9050) cline
          if(cline.eq.' ') go to 3020
          if(cline(1:1).eq.'#') go to 3020
          call writint(aline,iline)
          call linspl(jfail,2,lw,rw,cline)
          if(jfail.ne.0) go to 9100
          akey=cline(lw(1):rw(1))
          call lowcas(akey)
          if(akey.eq.'end')then
            call read_covar_mat_file(jfail,junit,covdim,nobs,vmul,
     +      covar,matfile,amessage,aobs,cline)
            if(jfail.ne.0) go to 9800
            go to 100
          else if(akey.eq.'file')then
            matfile=cline(lw(2):)
            call remchar(matfile,'''')
            call remchar(matfile,'"')
            matfile=adjustl(matfile)
#ifndef UNIX
            call lowcas(matfile)
#endif
            go to 3020
          else if(akey.eq.'variance_multiplier')then
            atemp=cline(lw(2):rw(2))
            call drealrd(jfail,atemp,vmul)
            if(jfail.ne.0)then
              write(amessage,1020) trim(aline),trim(afile)
              go to 9800
            end if
            if(vmul.le.0.0d0)then
              write(amessage,1030) trim(aline),trim(afile)
              go to 9800
            end if
            go to 3020
          else
            write(amessage,3025) trim(aline),trim(afile)
3025        format(' Unrecognised keyword at line ',a,' of file ',a,'.')
            go to 9800
          end if

        else
          write(amessage,3100) trim(aline),trim(afile)
3100      format(' Unrecognised block name at line ',a,' of file ',
     +    a,'.')
          go to 9800

        end if

C -- Tidying up.

5000    continue

        icount=0
        if(itype.eq.1)then
          do iobs=1,nobs
            if(var(iobs).lt.-1.0d35) icount=icount+1
          end do
        else
          do iobs=1,nobs
            if(covar(iobs,iobs).lt.-1.0d35) icount=icount+1
          end do
        end if
        if(icount.ne.0)then
          if(atype.eq.'parameter')then
            write(amessage,5011) trim(atype),trim(afile),trim(atype)
5011        format(' Variances have not been provided for all ',
     +      'adjustable ',a,
     +      's in file ',a,' (and any files cited therein). Missing ',
     +      a,'s include:-')
          else
            write(amessage,5010) trim(atype),trim(afile),trim(atype)
5010        format(' Variances have not been provided for all ',a,
     +      's and/or prior information in file ',a,
     +      ' (and any files cited therein). Missing ',
     +      a,'s include:-')
          end if
          nb=len(amessage)
          do iobs=1,nobs
            if(itype.eq.1)then
              if(var(iobs).gt.-1.0d35) cycle
            else if(itype.eq.2)then
              if(covar(iobs,iobs).gt.-1.0d35) cycle
            end if
            k=len_trim(amessage)
            j=len_trim(aobs(iobs))+3
            if(k+j.gt.nb) go to 5050
            write(amessage(k+2:),5015) trim(aobs(iobs))
5015        format('"',a,'"')
          end do
5050      continue
          go to 9800
        end if

        return

9000    call writint(aline,iline)
        write(amessage,9010) trim(aline),trim(afile)
9010    format(' Error reading line ',a,' of file ',a,'.')
        go to 9800
9050    write(amessage,9060) trim(afile)
9060    format(' Unexpected end encountered to file ',a,'.')
        go to 9800
9100    call writint(aline,iline)
        write(amessage,9110) trim(aline),trim(afile)
9110    format(' Insufficient entries on line ',a,
     +  ' of file ',a,'.')
        go to 9800
9250    write(amessage,9260) trim(afile)
9260    format(' Cannot find "END STANDARD_DEVIATION" line in file ',
     +  a,'.')
        go to 9800
9300    write(amessage,9310) trim(atype),trim(aobs(currentobs)),
     +  trim(afile)
9310    format(' More than one variance has been supplied for ',a,' "',
     +  a,'" in file ',a,' (and any files cited therein).')
        go to 9800

9800    ifail=1

        return

        end


        subroutine read_covar_mat_file(ifail,iunit,covdim,
     +      nobs,vmul,covar,matfile,amessage,aobs,cline)

C -- Subroutine read_covar_mat_file reads a covariance matrix file.

        implicit none

        integer     :: ifail            ! return as non-zero if there is an error
        integer     :: iunit            ! unit number to read from
        integer     :: covdim           ! dimension of overall problem covariance matrix
        integer     :: nobs             ! number of observations

        double precision  :: vmul       ! variance multiplier

        double precision  :: covar(covdim,covdim)  ! covariance matrix

        character*(*) matfile           ! matrix file
        character*(*) amessage          ! error message string
        character*(*) cline             ! a working string
        character*(*) aobs(nobs)        ! observations

        integer          :: ierr,j,jfail,nrow,ncol,icode,
     +                      icount,nb,k,irow,icol,ir,ic,currentobs
        integer          :: lw(3),rw(3)
        double precision :: dtemp
        character*20     :: atemp
        character*200    :: afile

        integer, allocatable          :: iloc(:)
        double precision, allocatable :: mat(:,:)
        character*20, allocatable     :: bobs(:)


C -- Initialisation

        ifail=0
        currentobs=0
        call addquote(matfile,afile)
        write(6,10) trim(afile)
10      format(' - reading covariance matrix file ',a,'...')


C -- The matrix file is opened.

        open(unit=iunit,file=matfile,status='old',iostat=ierr)
        if(ierr.ne.0)then
          write(amessage,100) trim(afile)
100       format(' Cannot open covariance matrix file ',a,'.')
          go to 9800
        end if

        read(iunit,'(a)',err=9000,end=9050) cline
        call linspl(jfail,3,lw,rw,cline)
        if(jfail.ne.0) go to 9100
        atemp=cline(lw(1):rw(1))
        call intread(jfail,atemp,nrow)
        if(jfail.ne.0) go to 9000
        atemp=cline(lw(2):rw(2))
        call intread(jfail,atemp,ncol)
        if(jfail.ne.0) go to 9000
        atemp=cline(lw(3):rw(3))
        call intread(jfail,atemp,icode)
        if(jfail.ne.0) go to 9000
        if(nrow.ne.ncol)then
          write(amessage,120) trim(afile)
120       format(' Number of columns does not equal number of rows ',
     +    'according to integer header of covariance matrix file ',
     +    a,'.')
          go to 9800
        end if
        if((nrow.le.0).or.(ncol.le.0))then
          write(amessage,130) trim(afile)
130       format(' Improper integer header to matrix in file ',a,'.')
          go to 9800
        end if
        if((icode.ne.1).and.(icode.ne.-1))then
          write(amessage,140) trim(afile)
140       format(' Last integer entry on first line of covariance ',
     +    'matrix file ',a,' should be "1" or "-1".')
          go to 9800
        end if

C -- Memory is allocated.

        allocate(mat(nrow,ncol),bobs(nrow),iloc(nrow),stat=ierr)
        if(ierr.ne.0)then
          write(amessage,150)
150       format(' Cannot allocate sufficient memory to continue ',
     +    'execution.')
          go to 9800
        end if

C -- The rest of the file is read.

        if(icode.eq.-1)then
          mat=0.0d0           ! a matrix
          do irow=1,nrow
            read(iunit,*,err=9000,end=9050) mat(irow,irow)
          end do
        else
          do irow=1,nrow
            read(iunit,*,err=9000,end=9050) (mat(irow,icol),icol=1,ncol)
          end do
        end if
        read(iunit,'(a)',err=9000,end=9050) cline
        cline=adjustl(cline)
        if(cline(1:13).ne.'* row and col')then
          write(amessage,160) trim(afile)
160       format(' Line following matrix in file ',a,' should be ',
     +    '"* row and column names".')
          go to 9800
        end if
        do irow=1,nrow
          read(iunit,'(a)',err=9000,end=9050) bobs(irow)
          call lowcas(bobs(irow))
          bobs(irow)=adjustl(bobs(irow))
        end do

C -- Entities in the matrix file are linked to observation names.

        icount=0
        do irow=1,nrow
          call whichone(jfail,nobs,currentobs,aobs,bobs(irow))
          if(jfail.eq.0)then
            icount=icount+1
            iloc(irow)=currentobs
            if(covar(currentobs,currentobs).gt.-1.0d35)then
              write(amessage,165) trim(aobs(currentobs)),trim(afile)
165           format(' Parameter "',a,'" cited in matrix file ',a,
     +        ' has already been assigned a variance.')
              go to 9800
            end if
          else
            iloc(irow)=0
          end if
        end do
        if(icount.eq.0) go to 9900
        if(icount.ne.nrow)then
          write(amessage,170) trim(afile)
170       format(' All parameters, or no parameters, ',
     +    'cited in the covariance matrix ',
     +    'file ',a,' must be pertinent to the current parameter ',
     +    'estimation problem. ',
     +    'Some of them are, but some of them are not. The latter ',
     +    'include:-')
          nb=len(amessage)
          do irow=1,nrow
            if(iloc(irow).eq.0)then
              k=len_trim(amessage)
              j=len_trim(bobs(irow))+3
              if(k+j.gt.nb) go to 9800
              write(amessage(k+2:),180) trim(bobs(irow))
180           format('"',a,'"')
            end if
          end do
          go to 9800
        end if

C -- Pertinent elements of the global covariance matrix are now written.

        do icol=1,ncol
          do irow=1,nrow
            ir=iloc(irow)
            ic=iloc(icol)
            dtemp=mat(irow,icol)
            covar(ir,ic)=dtemp*vmul
          end do
        end do
        close(unit=iunit,iostat=ierr)
        write(6,185) trim(afile)
185     format(' - covariance matrix file ',a,' read ok.')

        go to 9900



9000    write(amessage,9010) trim(afile)
9010    format(' Error encountered in reading matrix file ',a,'.')
        go to 9800
9050    write(amessage,9060) trim(afile)
9060    format(' Unexpected end encountered to matrix file ',a,'.')
        go to 9800
9100    write(amessage,9110) trim(afile)
9110    format(' Three integers are expected on first line ',
     +  'of matrix file ',a,'.')
        go to 9800


9800    ifail=1

9900    continue
        deallocate(mat,bobs,iloc,stat=ierr)

        return
        end



        subroutine whichone(ifail,npar,ipar,apar,tpar)

C -- Subroutine whichone locates a string in an array. Note that both the
C    search string and the existing array of strings are assumed to be
C    in the same case.

        integer npar,ipar,i
        integer ifail
        character*(*) tpar
        character*(*) apar(npar)

        ifail=0
        if((ipar.lt.1).or.(ipar.gt.npar)) ipar=1
        if(tpar.eq.apar(ipar)) return
        if(ipar.ne.npar)then
          do 20 i=ipar+1,npar
          if(tpar.eq.apar(i))then
            ipar=i
            return
          end if
20        continue
        end if
        if(ipar.ne.1)then
          do 40 i=ipar-1,1,-1
          if(tpar.eq.apar(i)) then
            ipar=i
            return
          end if
40        continue
        end if
        ifail=1
        return
        end



        subroutine read_pest_control_file(jfail,iunit,itype,jtype,
     +  priorflag,vecdim,covdim, nobs,vmul,var,covar,pestfile,
     +  amessage,aobs,cline)

C -- Subroutine read_pest_control_file reads weights from a PEST control file
C    and converts them to variances.

        implicit none

        integer     :: jfail      ! returned as zero if no error
        integer     :: iunit      ! unit number to open PEST control file
        integer     :: itype      ! 1 if only variances permitted; 2 if also covariances
        integer     :: jtype      ! return as 1 if only variances supplied; 2 if also covars

        integer        :: priorflag  ! 0 if ignore prior info; 1 otherwise
        integer        :: nobs       ! dimensions of aobs

        integer        :: vecdim     ! dimension of var vector
        integer        :: covdim     ! dimension of covar array

        double precision  :: vmul                 ! variance multiplier
        double precision  :: var(vecdim)          ! variance vector
        double precision  :: covar(covdim,covdim) ! covariance matrix

        character*(*)    :: pestfile   ! name of pest control file
        character*(*)    :: amessage   ! text to write error message to
        character*(*)    :: cline      ! general text string
        character*(*)    :: aobs(nobs) ! names of observations/prior information

        integer       :: currentobs,ifail,numcov,ierr,iline,ireg,npar,
     +                   npargp,nprior,nobsgp,i,icountobs,iwarn,
     +                   iprior,ie,nobs1
        integer       :: lw(5),rw(5)

        double precision :: dtemp
        character*10  :: aline
        character*20  :: atemp,aaobs
        character*200 :: afile
        character*600 :: pline


C -- Initialisation.

        jfail=0
        ireg=0
        iwarn=0
        jtype=1     ! may change with future programming
        iline=0
        currentobs=0
        numcov=0
        call addquote(pestfile,afile)
        write(6,10) trim(afile)
10      format(' - reading PEST control file ',a,'...')

C -- The PEST control file is opened.

        open(unit=iunit,file=pestfile,status='old',iostat=ierr)
        if(ierr.ne.0)then
          write(amessage,50) trim(afile)
50        format(' Cannot open PEST control file ',a,'.')
          go to 9800
        end if

C -- The control data section of the file is found.

        do
          iline=iline+1
          read(iunit,'(a)',err=9000,end=200) cline
          cline=adjustl(cline)
          call lowcas(cline)
          if(cline(1:1).ne.'*') cycle
          if(index(cline,'control').ne.0) go to 230
          cycle
200       write(amessage,210) trim(afile)
210       format(' Cannot find "control data" section of ',
     +    'PEST control file ',a,'.')
          go to 9800
        end do
230     continue
        iline=iline+1
        read(iunit,'(a)',err=9000,end=9050) cline
        call lowcas(cline)
        call linspl(ifail,2,lw,rw,cline)
        if(ifail.ne.0) go to 9100
        atemp=cline(lw(2):rw(2))
        call lowcas(atemp)
        if(index(atemp,'regul').ne.0) ireg=1
        iline=iline+1
        read(iunit,'(a)',err=9000,end=9050) cline
        call linspl(ifail,5,lw,rw,cline)
        if(ifail.ne.0) go to 9100
        atemp=cline(lw(1):rw(1))
        call intread(ifail,atemp,npar)
        if(ifail.ne.0) go to 9000
        atemp=cline(lw(2):rw(2))
        call intread(ifail,atemp,nobs1)
        if(ifail.ne.0) go to 9000
        atemp=cline(lw(3):rw(3))
        call intread(ifail,atemp,npargp)
        if(ifail.ne.0) go to 9000
        atemp=cline(lw(4):rw(4))
        call intread(ifail,atemp,nprior)
        if(ifail.ne.0) go to 9000
        atemp=cline(lw(5):rw(5))
        call intread(ifail,atemp,nobsgp)
        if(ifail.ne.0) go to 9000

C -- We now find the "observation groups" section of this file.

        do
          iline=iline+1
          read(iunit,'(a)',err=9000,end=300) cline
          cline=adjustl(cline)
          if(cline(1:1).ne.'*') cycle
          call lowcas(cline)
          if(index(cline,'observation gr').ne.0) go to 330
          cycle
300       write(amessage,310) trim(afile)
310       format(' Cannot find "observation groups" section of PEST ',
     +    'control file ',a,'.')
          go to 9800
        end do
330     do i=1,nobsgp
          iline=iline+1
          read(iunit,'(a)',err=9000,end=9050) cline
          if(cline.eq.' ') go to 9300
          if(cline(1:1).eq.'#') go to 9300
          if(cline(1:1).eq.'*') go to 9300
          call linspl(ifail,2,lw,rw,cline)
          if(ifail.eq.0)then
            numcov=numcov+1
          end if
        end do

C -- We now find the "observation data" section of this file.

        do
          iline=iline+1
          read(iunit,'(a)',err=9000,end=400) cline
          cline=adjustl(cline)
          if(cline(1:1).ne.'*') cycle
          call lowcas(cline)
          if(index(cline,'observation data').ne.0) go to 430
          cycle
400       write(amessage,410) trim(afile)
410       format(' Cannot find "observation data" section of file ',
     +    a,'.')
          go to 9800
        end do
430     continue
        icountobs=0
        do i=1,nobs1
          iline=iline+1
          read(iunit,'(a)',err=9000,end=9050) cline
          if((cline.eq.' ').or.(cline(1:1).eq.'#')) go to 9300
          call linspl(ifail,4,lw,rw,cline)
          if(ifail.ne.0) go to 9100
          aaobs=cline(lw(1):rw(1))
          call lowcas(aaobs)
          call whichone(ifail,nobs,currentobs,aobs,aaobs)
          if(ifail.eq.0)then
            atemp=cline(lw(3):rw(3))
            call drealrd(ifail,atemp,dtemp)
            if(ifail.ne.0)then
              call writint(aline,iline)
              write(amessage,433) trim(aline),trim(afile)
433           format(' Cannot read observation weight from line ',a,
     +        ' of PEST control file ',a,'.')
              go to 9800
            end if
            if(dtemp.eq.0.0d0) then
              dtemp=1.0d-30
              iwarn=1
            else if(dtemp.lt.0.0d0)then
              call writint(aline,iline)
              write(amessage,435) trim(aline),trim(afile)
435           format(' Negative observation weight not permitted at ',
     +        'line ',a,' of PEST control file ',a,'.')
              go to 9800
            end if
            icountobs=icountobs+1
            dtemp=dtemp*dtemp
            if(itype.eq.1)then
              if(var(currentobs).gt.-1.0d35) go to 9400
              var(currentobs)=vmul/dtemp
            else
              if(covar(currentobs,currentobs).gt.-1.0d35) go to 9400
              covar(currentobs,currentobs)=vmul/dtemp
            end if
          end if
        end do
        if(nprior.eq.0) go to 1000
        if(icountobs.eq.nobs) go to 1000
        if(priorflag.eq.0) go to 1000

C -- We now look for the prior information section.

        do
          iline=iline+1
          read(iunit,'(a)',err=9000,end=500) cline
          if(cline(1:1).ne.'*') cycle
          call lowcas(cline)
          if(index(cline,'prior inf').ne.0) go to 530
          cycle
500       write(amessage,510) trim(afile)
510       format(' Cannot find "prior information" section of PEST ',
     +    'control file ',a,'.')
          go to 9800
        end do
530     continue
        read(iunit,'(a)',err=9000,end=9300) cline
        do iprior=1,nprior
          pline=cline
          cline=' '
540       iline=iline+1
          read(iunit,'(a)',err=9000,end=545) cline
          cline=adjustl(cline)
          if(cline(1:1).eq.'&')then
            cline(1:1)=' '
            pline=trim(pline)//trim(cline)
            go to 540
          end if
545       continue
          if(pline.eq.' ') go to 9300
          call linspl(ifail,1,lw,rw,pline)
          aaobs=pline(lw(1):rw(1))
          call lowcas(aaobs)
          call whichone(ifail,nobs,currentobs,aobs,aaobs)
          if(ifail.eq.0)then
            ie=index(pline,'=')
            if(ie.eq.0) go to 9300
            pline=pline(ie+1:)
            call linspl(ifail,2,lw,rw,pline)
            if(ifail.ne.0) go to 9300
            atemp=pline(lw(2):rw(2))
            call drealrd(ifail,atemp,dtemp)
            if(ifail.ne.0) go to 9300
            if(dtemp.le.0.0d0) then
              dtemp=1.0d-30
              iwarn=1
            end if
            icountobs=icountobs+1
            dtemp=dtemp*dtemp
            if(itype.eq.1)then
              if(var(currentobs).gt.-1.0d35) go to 9500
              var(currentobs)=vmul/dtemp
            else
              if(covar(currentobs,currentobs).gt.-1.0d35) go to 9500
              covar(currentobs,currentobs)=vmul/dtemp
            end if
          end if
        end do

1000    continue
        close(unit=iunit)
        write(6,1010) trim(afile)
1010    format(' - file ',a,' read ok.')


        go to 9900

9000    call writint(aline,iline)
        write(amessage,9010) trim(aline),trim(afile)
9010    format(' Error reading line ',a,' of PEST control file ',
     +  a,'.')
        go to 9800
9050    write(amessage,9060) trim(afile)
9060    format(' Unexpected end encountered to PEST control file ',
     +  a,'.')
        go to 9800
9100    call writint(aline,iline)
        write(amessage,9150) trim(aline),trim(afile)
9150    format(' Insufficient entries on line ',a,' of PEST control ',
     +  'file ',a,'.')
        go to 9800
9300    write(amessage,9310) trim(afile)
9310    format(' File ',a,' is an illegal PEST control file: check ',
     +  'it with PESTCHEK.')
        go to 9800
9400    call writint(aline,iline)
        write(amessage,9410) trim(aobs(currentobs)),
     +  trim(aline),trim(afile)
9410    format(' Observation "',a,'" cited at line ',a,' of PEST ',
     +  'control file ',a,' has already been assigned a variance.')
        go to 9800
9500    call writint(aline,iline)
        write(amessage,9510) trim(aobs(currentobs)),
     +  trim(aline),trim(afile)
9510    format(' Observation "',a,'" cited on or about line ',a,
     +  ' of PEST control file ',a,' has already been assigned a ',
     +  'variance.')
        go to 9800



9800    jfail=1

9900    continue
        if(jfail.eq.0)then
          if(numcov.ne.0) jfail=-1
          if(iwarn.ne.0) jfail=jfail-10
        end if

        return

        end



        SUBROUTINE DREALRD(IFAIL,CLINE,RTEMP)

C -- Subroutine DREALRD reads a real number from a string.

        INTEGER IFAIL
        DOUBLE PRECISION RTEMP
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

