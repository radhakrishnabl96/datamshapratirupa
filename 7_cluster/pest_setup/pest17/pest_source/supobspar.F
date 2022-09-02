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

      program supobspar

C -- Program SUPOBSPAR writes super parameters and super observations to matrix files.

       use pestdata
       use types

       implicit none

       type(super_obs_struc) super_obs
       integer :: ierr, i, j, irestart,ifail
       integer :: lw(10),rw(10)
       integer :: n,n1
       integer :: pfile
       character (len=4) aext
       character(len=12) :: aversion, aapar, atemp
       character(len=15) :: anum
       character(len=200) :: afile, bfile, pestfile, jcofile
       character(len=200) :: outfile,casename
       character(len=600) :: dline
       double precision, allocatable :: xx(:,:)
       integer jacdim

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' SUPOBSPAR Version ',a,'. Watermark Numerical ',
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
       call linspl(ifail,5,lw,rw,dline)
       if(ifail.eq.0) go to 8900
       call linspl(ifail,4,lw,rw,dline)
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

       call intread(ifail,dline(lw(2):rw(2)),super_obs%num)
       if(ifail.ne.0) go to 8900
       if(super_obs%num.le.0) go to 8900

       super_obs%newmatfile=dline(lw(3):rw(3))
       call remchar(super_obs%newmatfile,char(211))
#ifndef UNIX
       call lowcas(super_obs%newmatfile)
#endif

       super_obs%newvtmatfile=dline(lw(4):rw(4))
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
240      format('SUPOBSPAR requires that MAXCOMPDIM be set to 1 ',
     +   'or less in PEST control file ',a,'.')
         go to 9890
       end if
       if(pestmode.eq.2)then
         write(errmsg,239)
239      format('SUPOBSPAR does not allow PEST to be run in ',
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
241          format('SUPOBSPAR does not allow observations or ',
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
242          format('SUPOBSPAR does not allow prior information ',
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

C -- Additional error checks after reading PEST file

       call check_files(super_obs, ierr, errmsg)
       if ( ierr.ne.0 )  goto 9890

C -- Read Jacobian file and compute weighted sensitivities

       allocate(xx(nxrow,nespar),stat=ierr)
       if(ierr.ne.0) go to 9200

       call calcWeightedSens(jcofile, xx, jacdim, ierr, errmsg)
       if ( ierr /=0 )  goto 9890
       deallocate(pval_o,parchglim,pval,parlbnd,parubnd,
     + ipargp,scale,offset,dercom,stat=ierr)

       call CalcSuperObs(xx, super_obs, ierr, errmsg)
       if ( ierr /=0 )  goto 9890
       deallocate(xx,stat=ierr)

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
8910   format(/,' SUPOBSPAR is run using the command:',/)
       write(6,8920)
8920   format('     SUPOBSPAR casename num obsmatfile parmatfile ',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8935)
8935   format('     casename   is a PEST control file basename,')
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
       if(allocated(x))deallocate(x,stat=ierr)
       close(unit=10,iostat=ierr)
       close(unit=11,iostat=ierr)
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
C         stat_smg = character array that stores an error meesage
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
      character(len=200) emsg

      write(6,10)
10    format(/,' - undertaking SVD of weighted Jacobian...')

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

      write(6,20)
20    format(' - calculation of super observations/parameters ',
     +'complete.')
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

C -- Initialize error handling varaibles

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
C         stat_msg = character array that stores an error meesage
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

      character(len=200) :: emsg
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
C      are now inverted and the square root of them found. xxxx

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
