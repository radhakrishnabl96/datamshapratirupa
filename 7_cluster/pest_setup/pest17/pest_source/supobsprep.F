      module types
      use array_btree
      implicit none

      type super_obs_struc
        integer num
        integer nobs_noreg
        double precision, allocatable :: s(:)
        double precision, allocatable :: u(:,:)
        double precision, allocatable :: vt(:,:)
        integer, allocatable :: map(:)
        double precision :: comp_ratio
        double precision :: clip_ratio
        integer :: comp_type
        double precision, allocatable :: weights(:)
        double precision :: minweight
        character(len=200) :: oldpestfile
        character(len=200) :: newpestfile
        character(len=200) :: modelscript
        character(len=200) :: newmatfile
        character(len=200) :: newvtmatfile
        integer, allocatable :: insfletype(:) !insfile type 1==observation; 2=reularisation; 3==both
        type(array_btree_char_struc) :: aobs_btree
      end type super_obs_struc
      end module types

      program supobsprep

C -- Program SUPOBSPREP  builds a new PEST control file using SVD-based
C    "super observations" from an existing one

      use pestdata, only  : maxcompdim,pestmode,npar,nobsgp,nxrow,
     +     errmsg,apar,obgnme,aobs,itrans,numcov,
     +     covind,owght,x,pval_o,modval_o,
     +     parchglim,pval,parlbnd,parubnd,
     +     ipargp,scale,offset,dercom,covgp,nespar,
     +     nobs,nobgnm,covar,nobscov,oval,cline,
     +     ninsfle, insfle, outfle, comlin
       use types

       implicit none

       interface
         subroutine CalcSuperObs(xx, super_obs, stat, stat_msg)
         use types
         use string_utils
         implicit none
         double precision,intent(inout),dimension(:,:),allocatable::xx
         type(super_obs_struc), intent(inout) :: super_obs
         integer, intent(out), optional :: stat
         character(len=*), intent(inout), optional ::  stat_msg

         end subroutine CalcSuperObs
       end interface
       interface
          subroutine calcWeights(super_obs, stat, stat_msg)
          use types
          use string_utils
          implicit none
          type(super_obs_struc), intent(inout) :: super_obs
          integer, intent(out), optional :: stat
          character(len=*), intent(inout), optional ::  stat_msg
          end subroutine calcWeights
         end interface
         interface
            subroutine check_files(super_obs, stat, stat_msg)
            use types
            use string_utils
            implicit none
            type(super_obs_struc), intent(inout) :: super_obs
            integer, intent(out), optional :: stat
            character(len=*), intent(inout), optional ::  stat_msg
            end subroutine check_files
         end interface
         interface
            subroutine write_objcalc_file(super_obs, filename,
     +           stat, stat_msg)
              use types
              use pestdata
              use string_utils
              implicit none
              type(super_obs_struc), intent(inout) :: super_obs
              character(len=*), intent(in) :: filename
              integer, intent(out), optional :: stat
              character(len=*), intent(inout), optional ::  stat_msg
            end subroutine write_objcalc_file
         end interface
         interface
            subroutine write_new_pest_file(super_obs,stat,stat_msg)
            use types
            use pestdata
            use string_utils
            implicit none
            type(super_obs_struc), intent(inout) :: super_obs
            integer, intent(out), optional :: stat
            character(len=*), intent(inout), optional ::  stat_msg
            end subroutine  write_new_pest_file
         end interface
         interface
            subroutine writeModelBat(super_obs,oldfile,outfle,
     +           stat, stat_msg)
            use types
            implicit none
            type(super_obs_struc), intent(inout)      :: super_obs
            character(len=*), intent(in)              :: oldfile
            character(len=*), intent(in)              :: outfle(:)
            integer, intent(out), optional            :: stat
            character(len=*), intent(inout), optional ::  stat_msg
            end subroutine writeModelBat
         end interface
         interface
            subroutine calcWeightedSens(jcofile, xx, jacdim, stat,
     +      stat_msg)
            use pestdata, only  : maxcompdim,pestmode,npar,nobsgp,
     +           nxrow, errmsg,apar,obgnme,aobs,itrans,numcov,
     +           covind,owght,x,pval_o,modval_o,
     +           parchglim,pval,parlbnd,parubnd,
     +           ipargp,scale,offset,dercom,covgp,nespar,
     +           nobs,nobgnm,covar,nobscov,oval,cline,
     +           ninsfle, insfle, outfle
            implicit none
            character(len=*), intent(in) :: jcofile
            double precision, intent(inout), allocatable :: xx(:,:)
            integer, intent(out) :: jacdim
            integer, intent(out), optional :: stat
            character(len=*), intent(inout), optional ::  stat_msg
            end subroutine calcWeightedSens
         end interface
         interface
           subroutine read_insfile(insfle, aobs, maxobs_in, local_obs,
     +           stat, stat_msg)
           implicit none
           character(len=*), intent(in) :: insfle
           character(len=*),intent(inout),dimension(:),allocatable::aobs
           integer, intent(in) :: maxobs_in
           integer, intent(out) :: local_obs
           integer, intent(out), optional :: stat
           character(len=*), intent(inout), optional ::  stat_msg
           end subroutine read_insfile
        end interface
         interface
            subroutine writeSupoJco(jcofile_exist,so, stat, stat_msg)
            use pestdata
            use types
            use string_utils
            implicit none
            character(len=*),intent(in) :: jcofile_exist
            type(super_obs_struc), intent(inout) :: so
            integer, intent(out), optional :: stat
            character(len=*), intent(inout), optional ::  stat_msg
            end subroutine writeSupoJco
         end interface
         interface
            subroutine writeSupMat(so, stat, stat_msg)
            use types
            use pestdata
            use string_utils
            implicit none
            type(super_obs_struc), intent(inout)      :: so
            integer, intent(out), optional            :: stat
            character(len=*), intent(inout), optional ::  stat_msg
            end subroutine writeSupMat
         end interface
         interface
            subroutine writeSupParMat(so, stat, stat_msg)
            use types
            use pestdata
            use string_utils
            implicit none
            type(super_obs_struc), intent(inout)      :: so
            integer, intent(out), optional            :: stat
            character(len=*), intent(inout), optional ::  stat_msg
            end subroutine writeSupParMat
         end interface





       type(super_obs_struc) super_obs
       integer :: ierr, i, j, irestart,ifail
       integer           :: p1start,pfile
       character(len=12) :: aversion, aapar, atemp
       character(len=15) :: anum
       character(len=200) :: afile, bfile, pestfile, jcofile
       character(len=200) :: outfile
       double precision, allocatable :: xx(:,:)
       integer nsuperobs
       integer jacdim,flag_dealoc

C -- User input is acquired from the terminal.

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif
#endif

       include 'version.inc'
       write(6,10) trim(aversion)
10     format(/,' SUPOBSPREP Version ',a,'. Watermark Numerical ',
     + 'Computing.',/)

100    write(6,110,advance='no')
110    format(' Enter name of existing PEST control file: ')
       read(5,'(a)') pestfile
       if(pestfile.eq.' ') go to 100
       call remchar(pestfile,'"')
       call remchar(pestfile,'''')
       pestfile=adjustl(pestfile)
       i=len_trim(pestfile)
       j=i-4
       if(j.lt.1)j=1
       atemp=pestfile(j:i)
       if(index(atemp,'.').eq.0) then
         pestfile=trim(pestfile)//'.pst'
       end if
       i=len_trim(pestfile)
       atemp=pestfile(i-3:i)
       call lowcas(atemp)
       if(atemp(1:4).ne.'.pst')then
         write(6,109)
109      format(/,' PEST control file must have an extension of ',
     +   '".pst" - try again.',/)
         go to 100
       end if
       open(unit=10,file=pestfile,status='old',iostat=ierr)
       close(unit=10)
       if(ierr.ne.0)then
         call addquote(pestfile,afile)
         write(6,115) trim(afile)
115      format(/,' Cannot open file ',a,' - try again.',/)
         go to 100
       end if
       super_obs%oldpestfile = pestfile

C Read the number of super-observations
 210   write(6,220,advance='no')
 220   format(' Enter number of super observations ',
     +      'to build from this file: ')
       read(5,*,err=210) super_obs%num
       if(super_obs%num.le.0) go to 210

C Read information associated with super observation weight compression
C This in implimented by setting the variables super_obs%compress_type
C and super_obs%compress_ratio.
C   super_obs%compress_type = 0 no compression
C                           = 1 natural weights compression
C                           = 2 logarithmic weight compression
       write(6,*)
 222   write(6,221,advance='no')
 221   format(' Enter clipping-enforced pre-compression weights ',
     + 'range (<Enter> if 1E6): ')
       read(5,'(a)') anum
       if(anum.eq.' ')then
         super_obs%clip_ratio=1.0d6
       else
         call drealread(ifail,anum,super_obs%clip_ratio)
         if(ifail.ne.0) go to 222
         if(super_obs%clip_ratio.lt.1.0)go to 222
       end if
 230   write(6,245,advance='no')
 245   format(' Enable compression/expansion of super observation',
     +      'weights? (y/n): ')
        read(5,*,err=230) atemp
        if (len_trim(atemp) > 1) then
           goto 230
        else if( (atemp == 'n') .or. (atemp == 'N') ) then
          super_obs%comp_type = 0
       else if ( (atemp == 'y') .or. (atemp == 'Y') ) then
          super_obs%comp_type = 1
       else
          goto 230
       end if
       if (super_obs%comp_type /= 0) then
 250      write(6,260,advance='no')
 260      format(' Enter max/min super observation weight ratio: ')
          read(5,*,err=250) super_obs%comp_ratio
          if(super_obs%comp_ratio.lt.1.0) go to 250
 270      write(6,280,advance='no')
 280      format(' Undertake (n)atural or (l)ogarithmic '
     +         'compression to achieve this ratio: ')
          read(5,*,err=270) atemp
          if (len_trim(atemp) > 1) then
             goto 270
          else if( (atemp == 'n') .or. (atemp == 'N') ) then
             super_obs%comp_type = 1
          else if ( (atemp == 'l') .or. (atemp == 'L') ) then
             super_obs%comp_type = 2
          else
             goto 270
          end if
       end if
 510   write(6,520,advance='no')
 520   format(' Enter minimum super-observation weight: ')
       read(5,*,err=510) super_obs%minweight
       if(super_obs%minweight.le.0.0) go to 510

C Read name for new PEST control File
       write(6,*)
 300   write(6,310,advance='no')
 310   format(' Enter name for new super pest control file: ')
       read(5,'(a)') outfile
       if(outfile.eq.' ') go to 300
       call remchar(outfile,'"')
       call remchar(outfile,'''')
       i=len_trim(outfile)
       j=i-3
       if(j.lt.1)j=1
       atemp=outfile(j:i)
       call lowcas(atemp)
       if(atemp.ne.'.pst')then
          if(index(atemp,'.').eq.0)then
             outfile=trim(outfile)//'.pst'
          else
             write(6,309)
 309         format(/,' PEST control file must have an extension ',
     +            'of ".pst" - try again.',/)
             go to 300
          end if
       end if
       outfile=adjustl(outfile)
       open(unit=20,file=outfile,action='write',iostat=ierr)
       close(unit=20)
       if(ierr.ne.0)then
          call addquote(outfile,afile)
          write(6,311) trim(afile)
 311      format(/,' Cannot open file ',a,' for output - try again.',/)
          go to 300
       end if
       super_obs%newpestfile = outfile

       write(6,*)
 500   write(6,501,advance='no')
 501   format(' Enter name for super observation matrix file ',
     + '(<Enter> if none): ')
       read(5,'(a)') outfile
       if(outfile.ne.' ') then
         call remchar(outfile,'"')
         call remchar(outfile,'''')
         outfile=adjustl(outfile)
         open(unit=20,file=outfile,action='write',iostat=ierr)
         close(unit=20)
         if(ierr.ne.0)then
            call addquote(outfile,afile)
            write(6,311) trim(afile)
            go to 500
         end if
       end if
       super_obs%newmatfile = outfile


       write(6,*)
 600   write(6,601,advance='no')
 601   format(' Enter name for super parameter matrix file ',
     + '(<Enter> if none): ')
       read(5,'(a)') outfile
       if(outfile.ne.' ') then
         call remchar(outfile,'"')
         call remchar(outfile,'''')
         outfile=adjustl(outfile)
         open(unit=30,file=outfile,action='write',iostat=ierr)
         close(unit=30)
         if(ierr.ne.0)then
            call addquote(outfile,afile)
            write(6,311) trim(afile)
            go to 600
         end if
       end if
       super_obs%newvtmatfile = outfile

C Read name for new model batch file

  400   continue
C       write(6,410,advance='no')
C 410   format(' Enter name for new model batch file: ')
C       read(5,'(a)') outfile
C       if(outfile.eq.' ') go to 400
C       call remchar(outfile,'"')
C       call remchar(outfile,'''')
C       i=len_trim(outfile)
C       j=i-3
C       if(j.lt.1)j=1
C       atemp=outfile(j:i)
C       call lowcas(atemp)
C       if(atemp.ne.'.bat')then
C          write(6,409)
C 409      format(/,' New model batch file must have an extension ',
C     +           'of ".bat" - try again.',/)
C          go to 400
C       end if
C       outfile=adjustl(outfile)
       outfile='supobsbatch.bat'
C       open(unit=20,file=outfile,action='write',iostat=ierr)
C       close(unit=20)
C       if(ierr.ne.0)then
C          call addquote(outfile,afile)
C          write(6,411) trim(afile)
C 411      format(/,' Cannot open file ',a,' for output - try again.',/)
C          go to 400
C       end if
       super_obs%modelscript = outfile

C -- A check is made that the corresponding Jacobian matrix file is present.

       jcofile=trim(pestfile(1:len_trim(pestfile)-3)//'jco')
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

       write(6,*)
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
240      format('SUPOBSPREP requires that MAXCOMPDIM be set to 1 ',
     +   'or less in PEST control file ',a,'.')
         go to 9890
       end if
       if(pestmode.eq.2)then
         write(errmsg,239)
239      format('SUPOBSPREP does not allow construction of super ',
     +   'parameters when PEST is run in predictive analysis mode.')
         go to 9890
       end if
       if(pestmode.ne.3)then
         do i=1,nxrow
           j=nobgnm(i)
           atemp=obgnme(j)(1:5)
           call lowcas(atemp)
           if(atemp.eq.'regul')then
             write(errmsg,241)
241          format('SUPOBSPREP does not allow observations or ',
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
242          format('SUPOBSPREP does not allow prior information ',
     +       'to be present within a PEST control file unless it ',
     +       'belongs to a regularisation group.')
             go to 9890
           end if
         end do
       end if

cdew       if(pestmode.ne.1)then
cdew         write(errmsg,250) trim(afile)
cdew250      format('SUPOBSPREP requires that PEST be run in ',
cdew     +   'parameter estimation mode in PEST control file ',a,
cdew     +   '. If necessary, create such a file with optimised ',
cdew     +   'parameters using PARREP and use JCO2JCO to ',
cdew     +   'build a JCO matrix for this file. Then set NOPTMAX to ',
cdew     +   '-1 or -2 and run pest with the "/i" switch to create ',
cdew     +   'an output dataset without the necessity to re-calculate ',
cdew     +   'the Jacobian matrix.')
cdew         go to 9890
cdew       end if
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

C Additional error checks after reading PEST file
       call check_files(super_obs, stat=ierr, stat_msg=errmsg)
       if ( ierr /=0 )  goto 9890

C Read Jacobian file and compute weighted sensitivities
       call calcWeightedSens(jcofile, xx, jacdim, stat=ierr,
     + stat_msg=errmsg)
       if ( ierr /=0 )  goto 9890
C -- We can delete some unused PEST memory.
       if(jacdim.eq.nxrow)then
         deallocate(x)
         flag_dealoc=1
       else
         flag_dealoc=0
       end if
       deallocate(pval_o,parchglim,pval,parlbnd,parubnd,
     + ipargp,scale,offset,dercom,stat=ierr)

CDEW TEMP
       call CalcSuperObs(xx, super_obs, stat=ierr, stat_msg=errmsg)
       if ( ierr /=0 )  goto 9890
       call calcWeights(super_obs, stat=ierr, stat_msg=errmsg)
       if ( ierr /=0 )  goto 9890
       call write_objcalc_file(super_obs, "obscalc.in",
     + stat=ierr, stat_msg=errmsg)
       if ( ierr /=0 )  goto 9890
       call write_new_pest_file(super_obs, stat=ierr, stat_msg=errmsg)
       if ( ierr /=0 )  goto 9890
       call writeModelBat(super_obs,comlin(1),
     +      outfle, stat=ierr, stat_msg=errmsg)
       if ( ierr /=0 )  goto 9890
       if(flag_dealoc.eq.1)then
         allocate(x(nxrow,nespar),stat=ierr)
         if(ierr.ne.0) go to 9200
       else
         jcofile=' '
       end if
       call writeSupoJco(jcofile,super_obs, stat=ierr, stat_msg=errmsg)
       if ( ierr /=0 )  goto 9890
       if(super_obs%newmatfile.ne.' ')then
         call writesupmat(super_obs, stat=ierr, stat_msg=errmsg)
         if ( ierr /=0 )  goto 9890
       end if
       if(super_obs%newvtmatfile.ne.' ')then
         call writesupparmat(super_obs, stat=ierr, stat_msg=errmsg)
         if ( ierr /=0 )  goto 9890
       end if

       go to 9900

C process errors
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
       deallocate(super_obs%weights,stat=ierr)
       deallocate(super_obs%insfletype,stat=ierr)

       deallocate(xx, stat=ierr)
       close(unit=10,iostat=ierr)
       close(unit=11,iostat=ierr)
       close(unit=12,iostat=ierr)
       close(unit=20,iostat=ierr)
       close(unit=21,iostat=ierr)

9999   continue

       end


      subroutine CalcSuperObs(xx, so, stat, stat_msg)
C######################################################################
C  subroutine CalcSuperObs(xx, so, stat, stat_msg)
C    This subroutine calculates the SVD of the PEST jacobian array xx, but
C    does not include regularisation and prior information in the
C    calculation.
C       variables:
C         xx(:,:)  = array containing the jacobian from PEST
C         so       = structure of type(super_obs_struc).  The svd information
C                    is stored in this structure
C         stat    = optional integer flag that signals an error occured
C                    when the subroutine was called
C         stat_smg = optional character array that stores an error meesage
C                    describing any errors that occured when the subroutine
C                    was called
C
C    note: this subroutine also uses PEST information stored in the module
C          pestdata
C######################################################################
      use pestdata
      use string_utils
      use types
      implicit none
      double precision,intent(inout),dimension(:,:),allocatable::xx
      type(super_obs_struc), intent(inout) ::  so
      integer, intent(out), optional :: stat
      character(len=*), intent(inout), optional ::  stat_msg

      integer i,j, inew, ierr
      character(len=1) :: jobu, jobvt
      integer :: info, lda, ldu, ldvt, lwork, m, n
      double precision a(1,1)
      double precision, allocatable :: work(:)
      character(len=200) emsg

      write(6,10)
10    format(/,' - undertaking SVD of weighted Jacobian for super ',
     +'observation calcutaton...')

C Initialize error handling varaibles
      emsg = ""
      ierr=0
      if ( present(stat) ) stat = 0
      if (present(stat_msg)) stat_msg = ""

c     Calculate the number of observations without regularisation
C     and allocate arrays for svd based on this number
      m = 0
      do i = 1, nobs
         if( tolower( obgnme( nobgnm(i)),5 ) /= 'regul')then
            m = m+1
         end if
      end do
      so%nobs_noreg = m

C Initialize variable used by SVD
      jobu = 'O'
      jobvt = 'S'
      n =  size(xx, 2)

      if(m.lt.n)then
        write(emsg,5)
5       format(' Use of SUPOBSPREP requires that there be less ',
     +  'adjustable parameters than the number of non-',
     +  'regularization observations.')
        go to 9800
      end if

      lda = m
      ldu = m
      ldvt = min(m,n)
      lwork = max(3*min(m,n)+max(m,n),5*min(m,n))

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

      inew = 0
C Populate the new jacobian matrix(a) which does not include regularisation
C and prior information.
      do i = 1, nobs
         if( tolower( obgnme(nobgnm(i)),5 ) /= 'regul') then
            inew = inew +1
            do j=1, n
               so%u(inew,j) = xx(i,j)
            end do
           so% map(inew) = i
         end if
      end do

      deallocate(xx,stat=ierr)
      allocate(so%s(min(m,n)), stat=ierr)
      if (ierr /= 0) goto 9200
      allocate( work(lwork), stat=ierr)
      if (ierr /= 0) goto 9200

C     Let SVD calculate to optimium size of the work vector
C     and resize it accordingly
      lwork = -1
      call dgesvd(jobu, jobvt, m, n, so%u, ldu, so%s, a, lda, so%vt,
     +     ldvt, work, lwork, info)
      lwork = work(1)
      deallocate(work, stat=ierr)
      allocate( work(lwork), stat=ierr)
      if (ierr /= 0) goto 9200

C     Call SVD
      call dgesvd(jobu, jobvt, m, n, so%u, ldu, so%s, a, lda, so%vt,
     +     ldvt, work, lwork, info)

      deallocate(work, stat=ierr)
      if(info /= 0) goto 9220

      write(6,20)
20    format(' - calculation of super observations complete.')

      goto 9900 ! finish with out any error
C Handle Errors
 9200 write(emsg,9205)
 9205 format(' Cannot allocate sufficient memory to continue ',
     +     'execution.')
      go to 9800
 9220  write(emsg,9225)
 9225  format(' Error: failed to compute SVD')
       go to 9800
 9800 continue
      if ( present(stat) ) stat = 1
      if (present(stat_msg)) stat_msg = emsg
      if ( .not. (present(stat) .and. present(stat_msg) ) ) then
         write(6, FMT='(//," Error in subroutine CalcSuperObs:")')
         write(6,FMT='("  ",a)') emsg
       end if
 9900 continue
      end subroutine CalcSuperObs




      subroutine CalcWeights(so, stat, stat_msg)
C######################################################################
C    subroutine CalcWeights(so, stat, stat_msg)
C    This subroutine calculates the weights for the super observations.
C    The weights are based on singular values, although linear and logarithmic
C    options are available to compress the ranges
C       variables:
C         so       = structure of type(super_obs_struc).   Super observation
C                   information is stored in this structure some key
C                   variables are:
C                     so%num        = number of super observations
C                     so%clip_ratio = pre-compression weights ratio enforced by clipping
C                     so%comp_ratio = max/min super observation weight ratio
C                     so%comp_type  = compression type
C                                     (0=none; 1=linear, 2=logarithmic)
C                     so%weights    = weights array
C         stat    = optional integer flag that signals an error occured
C                    when the subroutine was called
C         stat_smg = optional character array that stores an error meesage
C                    describing any errors that occured when the subroutine
C                    was called
C
C    note: this subroutine also uses PEST information stored in the module
C          pestdata
C######################################################################
      use types
      use string_utils
      implicit none
      type(super_obs_struc), intent(inout) :: so
      integer, intent(out), optional :: stat
      character(len=*), intent(inout), optional ::  stat_msg

      integer ierr,i
      double precision maxw, minw, max_minus_min, slope, b
      double precision thresh,ratio
      character(len=200) emsg

C     Initialize error handling variables
      emsg = ""
      ierr=0
      if ( present(stat) ) stat = 0
      if (present(stat_msg)) stat_msg = ""

      write(6,10)
10    format(/,' - undertaking super observation weights ',
     +'calculation...')

C -- The pre-compression clipping ratio is enforced.

      thresh=so%s(1)/so%clip_ratio
      do i=1,so%num
        if(so%s(i).lt.thresh)so%s(i)=thresh
      end do

      maxw = so%s(1)
      minw = so%s(so%num)

      if(allocated(so%weights))deallocate(so%weights)
      allocate(so%weights(so%num), stat=ierr)
      if (ierr /= 0) goto 9200
      if(so%comp_type == 0) then ! no compression
         so%weights = so%s(1:so%num)

      else if(so%comp_type == 1) then ! linear compression
         slope = (so%comp_ratio-1)*minw / (maxw - minw)
         so%weights = (so%s(1:so%num)-minw) * slope + minw

      else if (so%comp_type == 2) then  ! logarithmic compression
         slope=(log10(so%comp_ratio*minw)-log10(minw))/
     +         (log10(maxw)-log10(minw))
         b = log10(minw)
         so%weights = 10**((log10(so%s(1:so%num))-b) * slope + b)
      else
         write(6, FMT='("Error processing compression information ",
     +        "no compression be be preformed on weights")')
         so%weights = so%s(1:so%num)
      end if

! -- The minimum weight is now enforced.

      ratio=so%minweight/so%s(so%num)
      so%weights=so%weights*ratio

      write(6,20)
20    format(' - super observation weights calculation complete.')

      goto 9900 ! finish with out any error
C     Handle Errors
 9200 write(emsg,9205)
 9205 format(' Cannot allocate sufficient memory to continue ',
     +     'execution.')
      go to 9800
 9800 continue
      if ( present(stat) ) stat = 1
      if (present(stat_msg)) stat_msg = emsg
      if ( .not. (present(stat) .and. present(stat_msg) ) ) then
         write(6, FMT='(//," Error in subroutine CalcWeights:")')
         write(6,FMT='("  ",a)') emsg
      end if
 9900 continue
      end subroutine calcWeights




      subroutine check_files(so, stat, stat_msg)
C######################################################################
C    subroutine check_files(so, stat, stat_msg)
C    This subroutine checks the PEST file to insure that all the features
C    and syntax are compatable with superobservations
C         so       = structure of type(super_obs_struc).   Super observation
C                   information is stored in this structure
C         stat    = optional integer flag that signals an error occured
C                    when the subroutine was called
C         stat_smg = optional character array that stores an error meesage
C                    describing any errors that occured when the subroutine
C                    was called
C
C    note: this subroutine also uses PEST information stored in the module
C          pestdata
C######################################################################
      use pestdata
      use types
      use string_utils
      implicit none
      type(super_obs_struc), intent(inout) :: so
      integer, intent(out), optional :: stat
      character(len=*), intent(inout), optional ::  stat_msg
      interface
         subroutine read_insfile(insfle, aobs, maxobs_in, local_obs,
     +        stat, stat_msg)
         implicit none
         character(len=*), intent(in) :: insfle
         character(len=*),intent(inout),dimension(:),allocatable::aobs
         integer, intent(in) :: maxobs_in
         integer, intent(out) :: local_obs
         integer, intent(out), optional :: stat
         character(len=*), intent(inout), optional ::  stat_msg
         end subroutine read_insfile
      end interface

      integer i,j, m, ierr, itmp, itype, ifl
      logical:: found
      character(len=len(aobs(1))) tmpStr
      integer :: nobs_in_file
      character(len=20), dimension(:), allocatable :: aobs_file
      character(len=20) :: aobgrp
      character(len=2000) :: emsg
      character(len=100) :: atemp
      character(len=200) :: afile

C     Initialize error handling varaibles
      emsg = ""
      ierr=0
      if ( present(stat) ) stat = 0
      if (present(stat_msg)) stat_msg = ""



c     Calculate the number of observations without regularisation
C     and allocate arrays for svd based on this number
      m = 0
      do i = 1, nobs
         if( tolower( obgnme( nobgnm(i)),5 ) /= 'regul')then
            m = m+1
         end if
      end do
      so%nobs_noreg = m

C check that number of super observation is less than number of observations
      if(so%num >=so%nobs_noreg) then
         ierr = 1
         emsg = 'Error: requested number of super observations must '
     +        // 'be less than the number of non-regularisation '
     +        // 'observations in the original PEST file.'
         goto 9800
      end if

C check that the number of super observations is less than the number of adjustable parameters

      if(so%num > nespar) then
         ierr = 1
         emsg = 'Error: requested number of super observations must '
     +        // 'not exceed the number of adjustable parameters '
     +        // 'in the original PEST file.'
         goto 9800
      end if

C check for multiple command lines
      if(numcom >1) then
         ierr = 1
         emsg = 'Error: SUPOBSPREP only supports PEST control '
     +        // 'files with a single model command line.'
         goto 9800
      end if

C check for external derivatives
      if(jacfile /= 0) then
         ierr = 1
         emsg = 'Error: PEST control file cannot contain'
     +        // ' a "derivatives command line" section '
     +        // 'if used with SUPOBSPREP.'
         goto 9800
      end if

C check that model command end with .bat
      i = len_trim(comlin(1))
      j = max(1, i-3)
      if (tolower(comlin(1)(j:i)) /= '.bat') then
         ierr = 1
         emsg = 'Error: command to run model in supplied PEST '
     +        //'control file must end with ".bat"'
         goto 9800
      end if
C check for names of regularisation observations of the form
C obsX, where X represents any number and can be of multiple
C digits.  This is not allowed as name of this form are
C reserved for the super observations.
      do i = 1, nobs
         if( tolower( obgnme( nobgnm(i)),5) == 'regul')then
            tmpStr = trim(aobs(i))
            call lowcas(tmpStr)
            if (tmpStr(1:3) == 'obs' ) then
               do j = 4, len_trim(tmpStr)
                  if ( .not.( iachar(tmpStr(j:j) ) >= iachar('0') .and.
     +                 iachar(tmpStr(j:j) ) <= iachar('9')) ) then
                     exit
                  end if
                  if (j==len_trim(tmpStr)) then
                    write(emsg,20)
20                  format('Regularisation observations in original ',
     +              'PEST control file must not be of the form obs1, ',
     +              'obs2, ... as ',
     +              'these names are reserved for super-observations.')
                    go to 9800
                  end if
               end do
            end if
         end if
      end do
      if (ierr > 0) goto 9800
C Determine which instruction files contain observations
C and which contain regularisation information.  Issue
C a waring if any files contain both
C
C Build a binary tree so we can quickly get the index
C of an observation name
      call array_btree_char_init (so%aobs_btree, aobs,
     +     stat=ierr, stat_msg=emsg)
      if(ierr /= 0) goto 9800
      allocate(aobs_file(nobs), stat=ierr)
      if(ierr /= 0) goto 9200
      if(allocated(so%insfletype)) deallocate(so%insfletype)
      allocate(so%insfletype(ninsfle), stat=ierr)
      if(ierr /= 0) goto 9200
      write(6,21)
21    format(/,' - checking instruction files cited in PEST ',
     +'control file...')
      fileloop:do ifl = 1, ninsfle
         call read_insfile(insfle(ifl), aobs_file, nobs, nobs_in_file,
     +        stat=ierr, stat_msg=emsg)
          if(ierr /= 0) then
            emsg=adjustl(emsg)
            goto 9800
          end if
         so%insfletype(ifl) = 0  !1==observation data; 2==regularisation data; 3==both
         obsloop: do i=1, nobs_in_file
           found = array_btree_char_getval(so%aobs_btree, aobs,
     +        aobs_file(i), j)
           if(.not.found)then
             call addquote(insfle(ifl),afile)
             write(emsg,23) trim(aobs_file(i)),trim(afile)
23           format(' Observation "',a,'" cited in instruction file ',
     +       a,' is not cited in PEST control file.')
             go to 9800
           end if
           if( tolower( obgnme( nobgnm(j)),5 ) == 'regul')then
              itmp = 2
           else
              itmp = 1
           end if
           if(so%insfletype(ifl)/=itmp)
     +          so%insfletype(ifl)=so%insfletype(ifl)+itmp
           if(so%insfletype(ifl)>2) then
              ierr = 1
              emsg=' Error: instruction file "'
     +             // trim(insfle(ifl)) //
     +             '" contains observations which '//
     +             'belong to both regularisation and '//
     +             'non-regularisation groups.'
              exit fileloop
           end if
        end do obsloop
      end do fileloop
      write(6,22)
22    format(' - instruction files checked ok.')

      if (allocated(aobs_file)) deallocate(aobs_file)
      if(ierr /= 0) goto 9800
      goto 9900  ! no errors
C     Process error messages
 9200 emsg = 'Cannot allocate sufficient memory to' //
     +     ' continue execution.'
      go to 9800
 9800 continue
      if ( present(stat) ) stat = 1
      if (present(stat_msg)) stat_msg = emsg
      if ( .not. (present(stat) .and. present(stat_msg) ) ) then
         write(6,FMT='(//," Error in subroutine check_files :")')
         write(6,FMT='("  ",a)') emsg
      end if
 9900 continue
      end subroutine check_files




      subroutine write_objcalc_file(so, filename, stat, stat_msg)
C######################################################################
C    write_objcalc_file(so, filename, stat, stat_msg)
C     This subroutine creates an objcalc input file
C         so       = structure of type(super_obs_struc).   Super observation
C                    information is stored in this structure
C         filename = name to be used to create the objcalc input file
C         stat     = optional integer flag that signals an error occured
C                    when the subroutine was called
C         stat_smg = optional character array that stores an error meesage
C                    describing any errors that occured when the subroutine
C                    was called
C
C    note: this subroutine also uses PEST information stored in the module
C          pestdata
C######################################################################
      use types
      use pestdata
      use string_utils
      implicit none
      type(super_obs_struc), intent(inout) :: so
      integer, intent(out), optional :: stat
      character(len=*), intent(inout), optional ::  stat_msg

      character(len=*), intent(in) :: filename
      integer i, j, ierr, ninsobs
      character(len=200) :: afile,bfile
      character(len=200) :: emsg

C     Initialize error handling varaibles
      emsg = ""
      ierr=0
      if ( present(stat) ) stat = 0
      if (present(stat_msg)) stat_msg = ""

      call addquote(filename,afile)
      write(6,1) trim(afile)
1     format(/,' - writing OBSCALC input file ',a,'...')

C     Compute number of observation insfiles
      ninsobs=0
      do i = 1, ninsfle
         if(so%insfletype(i) == 1) ninsobs = ninsobs +1
      end do

      open(unit=22,file=filename,status='unknown',action='write',
     +     iostat=ierr)
      if (ierr /= 0) then
         write(emsg,5) trim(afile)
5        format(' Cannot open OBSCALC input file ',a,' for ',
     +   'recording input data.')
         goto 9800
      end if

      write(22, FMT='("* control data")', iostat=ierr)
      if (ierr /= 0) goto 9220
      write(22, FMT='(3i12)', iostat=ierr) so%nobs_noreg,
     +     so%num, ninsobs
      if (ierr /= 0) goto 9220
      write(22, FMT='("* matrix")', iostat=ierr)
      if (ierr /= 0) goto 9220
      do j=1, so%num
#ifdef USE_D_FORMAT
         write(22, FMT='(8(1XD28.20))', iostat=ierr)
     +        (so%u(i,j) , i=1,so%nobs_noreg)
#else
         write(22, FMT='(8(1XE28.20))', iostat=ierr)
     +        (so%u(i,j) , i=1,so%nobs_noreg)
#endif
         if (ierr /= 0) goto 9220
      end do
      write(22, FMT='("* observation names")', iostat=ierr)
      if (ierr /= 0) goto 9220
      do i=1, so%nobs_noreg
         write(22, FMT='(1x,a)', iostat=ierr) trim(aobs(so%map(i)))
         if (ierr /= 0) goto 9220
      end do
      write(22, FMT='("* observation values")', iostat=ierr)
      if (ierr /= 0) goto 9220
        do i=1, so%nobs_noreg
#ifdef USE_D_FORMAT
         write(22, FMT='(1x,D28.20)', iostat=ierr) oval( so%map(i) )
#else
         write(22, FMT='(1x,E28.20)', iostat=ierr) oval( so%map(i) )
#endif
         if (ierr /= 0) goto 9220
      end do
      write(22, FMT='("* instruction files")')
      if (ierr /= 0) goto 9220
      do i=1, ninsfle
         if(so%insfletype(i) == 1) then
            call addquote(insfle(i),afile)
            call addquote(outfle(i),bfile)
            write(22, FMT='(1x,a,4X,a)', iostat=ierr)
     +           trim(afile), trim(bfile)
            if (ierr /= 0) goto 9220
         end if
      end do

      call addquote(filename,afile)
      write(6,50) trim(afile)
50    format(' - file ',a,' written ok.')

      goto 9900 ! no errors
C Process error messages
 9220 write(emsg, FMT='(3a)', iostat=ierr)
     +    ' Cannot write to OBSCALC input file ',
     +     trim(filename),','
      go to 9800
 9800 continue
      if ( present(stat) ) stat = 1
      if (present(stat_msg)) stat_msg = emsg
      if ( .not. (present(stat) .and. present(stat_msg) ) ) then
         write(6,FMT='(//," Error in subroutine  write_objcalc_file:")')
         write(6,FMT='("  ",a)') emsg
      end if
 9900 continue
      close(unit=22)
      end subroutine write_objcalc_file





      subroutine write_new_pest_file(so, stat, stat_msg)
C######################################################################
C   write_new_pest_file(so, stat, stat_msg)
C     This subroutine writes out the updated PEST control file based
C     on the super observations
C       variables:
C         so       = structure of type(super_obs_struc).   Super observation
C                    information is stored in this structure
C         stat    =  optional integer flag that signals an error occured
C                    when the subroutine was called
C         stat_smg = optional character array that stores an error meesage
C                    describing any errors that occured when the subroutine
C                    was called
C
C    note: this subroutine also uses PEST information stored in the module
C          pestdata
C######################################################################
      use types
      use pestdata
      use string_utils
      use containers
      implicit none
      type(super_obs_struc), intent(inout) :: so
      integer, intent(out), optional :: stat
      character(len=*), intent(inout), optional ::  stat_msg

      character(len=300):: afile
      character(len=8) :: atemp8
      character(len=20) :: atemp20
      character(len=20) :: tmp_obsname
      character(len=12) :: super_obsgp = "super_obs" ! observation group for super observations
      character(len=200) :: emsg

      integer :: i, ierr, item, ninsreg, nobs_sup,ii
      integer :: oldunit, newunit
      type(tokenizer_struc) :: tokenizer
      type(Array_struc) obsgrp_id

      oldunit = 23
      newunit = 24

C     Initialize error handling varaibles
      emsg = ""
      ierr=0
      if ( present(stat) ) stat = 0
      if (present(stat_msg)) stat_msg = ""

      call addquote(so%newpestfile,afile)
      write(6,1) trim(afile)
1     format(/,' - writing new PEST control file ',a,'...')

      nobs_sup =  so%num+(nobs-so%nobs_noreg)
C     Compute number of observation insfiles
      ninsreg=0
      do i = 1, ninsfle
         if(so%insfletype(i) == 2) ninsreg = ninsreg +1
      end do

c First compute observation groups in the new file
C First get regularisation groups
      do i = 1, nobsgp
         if( tolower(obgnme(i),5) == "regul" ) then
            call array_struc_add(obsgrp_id, i)
         end if
      end do
C Next add on groups with prior information
      if(nprior.ne.0)then
        do i = nobs+1, nobs+nprior
            call array_struc_add(obsgrp_id, nobgnm(i) )
        end do
        call array_struc_unique(obsgrp_id)
      end if
C open files
      call addquote(so%oldpestfile,afile)
      open(unit=oldunit,file=so%oldpestfile,status='old',action='read',
     +     iostat=ierr)
      if (ierr /= 0) then
         write(emsg, '(2a)') ' Cannot open file ', trim(afile)
         goto 9800
      end if
      call addquote(so%newpestfile,afile)
      open(unit=newunit,file=so%newpestfile,status='unknown',
     +     action='write',iostat=ierr)
            if (ierr /= 0) then
               write(emsg,5) trim(afile)
5              format(' Cannot open file ',a,' for output.')
               goto 9800
            end if

      do i=1,3
         read(unit=oldunit, FMT='(a)', iostat=ierr) cline
         if (ierr /= 0) goto 9210
         write(unit=newunit, FMT='(a)', iostat=ierr) trim(cline)
          if (ierr /= 0) goto 9220
      end do
      read(unit=oldunit, FMT='(a)', iostat=ierr) cline
      if (ierr /= 0) goto 9210
      call tokenizer_init(tokenizer, cline)
      do i=1,6
         atemp8 = trim(tokenizer_get(tokenizer))
      end do
C write out line with updated nobs and nobsgrp
      write(newunit,FMT='(5i7,2x,a)',iostat=ierr) npar,
     +     nobs_sup, npargp,
     +     nprior, array_struc_size(obsgrp_id)+1, atemp8
       if (ierr /= 0) goto 9220
C write out next line with update number of ins files
       read(unit=oldunit, FMT='(a)', iostat=ierr) cline
       if (ierr /= 0) goto 9210
       call tokenizer_init(tokenizer, cline)
       do i=1,10
          atemp20 = trim(tokenizer_get(tokenizer))
          if(trim(atemp20)=="") then
             exit
          else if(i==2) then
             write(newunit,FMT='(2X,i7)',advance='no',
     +            iostat=ierr) ninsreg  +1
          else
             write(newunit,FMT='(2X,a)',advance='no',
     +            iostat=ierr) trim(atemp20)
          end if
      end do
      write(newunit,FMT='(/)', advance='no')

C read and write the next three lines

      do i=1,3
         read(unit=oldunit, FMT='(a)', iostat=ierr) cline
         if (ierr /= 0) goto 9210
         write(unit=newunit, FMT='(a)', iostat=ierr) trim(cline)
         if (ierr /= 0) goto 9220
      end do

C read and write the next line by alter NOPTMAX to 50

       read(unit=oldunit, FMT='(a)', iostat=ierr) cline
       if (ierr /= 0) goto 9210
       cline=adjustl(cline)
       ii=index(cline,' ')
       write(newunit,40,err=9220) ' 50 ',trim(cline(ii:))
40     format(a,a)

C read and write everything up to observation group names
      do
         read(unit=oldunit, FMT='(a)', iostat=ierr) cline
         if (ierr /= 0) goto 9210
         write(unit=newunit, FMT='(a)', iostat=ierr) trim(cline)
          if (ierr /= 0) goto 9220
         if( tolower(cline) =='* observation groups') exit
      end do
C write updated observation group names
      write(unit=newunit, FMT='(a)', iostat=ierr)  trim(super_obsgp)
       if (ierr /= 0) goto 9220
      do i = 1, array_struc_size(obsgrp_id)
            write(unit=newunit, FMT='(a)', iostat=ierr)
     +        trim(obgnme( array_struc_getvalue(obsgrp_id, i)))
            if (ierr /= 0) goto 9220
      end do
C write updated observations
      do
         read(unit=oldunit, FMT='(a)', iostat=ierr) cline
         if (ierr /= 0) goto 9210
         if(tolower(cline, 18) == '* observation data') exit
      end do
      write(unit=newunit, FMT='(a)', iostat=ierr) trim(cline)
      if (ierr /= 0) goto 9220
C Write new super observations
      do i = 1, so%num
         call writint(atemp8, i)
         write(unit=newunit, FMT='(a,t23,2(1pg14.7), 3X,a)',
     +        iostat=ierr)
     +        trim('obs'//atemp8), 0.0, so%weights(i),
     +        trim(super_obsgp)
         if (ierr /= 0) goto 9220
      end do

      do i = 1, nobs
         read(unit=oldunit, FMT='(a)', iostat=ierr) cline
         if (ierr /= 0) goto 9210
         call tokenizer_init(tokenizer, trim(cline))
         do item=1,4
            tmp_obsname = trim(tokenizer_get(tokenizer))
         end do
         if( tolower(tmp_obsname ,5) == "regul" ) then
            write(unit=newunit, FMT='(a)', iostat=ierr) trim(cline)
            if (ierr /= 0) goto 9220
         end if
      end do
C write new model command line
      read(unit=oldunit, FMT='(a)', iostat=ierr) cline ! read line "* model command line"
      if (ierr /= 0) goto 9210
      write(unit=newunit, FMT='(a)', iostat=ierr) trim(cline)
      if (ierr /= 0) goto 9220
      write(unit=newunit, FMT='(a)', iostat=ierr) trim(so%modelscript)
      if (ierr /= 0) goto 9220
      read(unit=oldunit, FMT='(a)', iostat=ierr) cline ! read old model command line
      if (ierr /= 0) goto 9210
C  read and write out template files
C read and write "* model input/ouput"
      read(unit=oldunit, FMT='(a)', iostat=ierr) cline ! read line "* model input/output"
      if (ierr /= 0) goto 9210
      write(unit=newunit, FMT='(a)', iostat=ierr) trim(cline)
C read and write templates
      do i=1,NTPLFLE
         read(unit=oldunit, FMT='(a)', iostat=ierr, end = 9900) cline
         if (ierr /= 0) goto 9210
         write(unit=newunit, FMT='(a)', iostat=ierr) trim(cline)
          if (ierr /= 0) goto 9220
      end do
C  read and write out updated instruction files
      do i=1,ninsfle
         read(unit=oldunit, FMT='(a)', iostat=ierr, end = 9900) cline
         if (ierr /= 0) goto 9210
         if(so%insfletype(i) == 2) then
            write(unit=newunit, FMT='(a)', iostat=ierr) trim(cline)
            if (ierr /= 0) goto 9220
         end if
      end do
      write(newunit,'(a)') ' obscalc.ins   obscalc.out'

C read and write out rest of file

      do
         read(unit=oldunit, FMT='(a)', iostat=ierr, end = 100) cline
         if (ierr /= 0) goto 9210
         write(unit=newunit, FMT='(a)', iostat=ierr) trim(cline)
          if (ierr /= 0) goto 9220
      end do

100   continue
      call addquote(so%newpestfile,afile)
      write(6,2) trim(afile)
2     format(' - file ',a,' written ok.')

C -- The instruction file to read the OBSCALC output file is now written.

      close(unit=newunit)
200   write(6,210)
210   format(/,' - writing instruction file obscalc.ins')
      open(unit=newunit,file='obscalc.ins',action='write',err=9300)
      write(newunit,220,err=9300)
220   format('pif $')
      do i = 1, so%num
         call writint(atemp8, i)
         write(newunit,240,err=9300) trim('obs'//atemp8)
240      format('l1 [',a,']1:20')
      end do
      write(6,250)
250   format(' - file obscalc.ins written ok.')

      goto 9900
C Process error messages
 9200 write(emsg,FMT='(2a)')
     +     ' Cannot allocate sufficient memory to continue ',
     +     'execution.'
      go to 9800
 9210  write(emsg, FMT='(2a)') ' Error reading from file ',
     +     trim(so%oldpestfile)
        go to 9800
 9220   continue
        call addquote(so%newpestfile,afile)
        write(emsg,9221) trim(afile)
 9221   format(' Cannot write to new PEST control file ',a,'.')
        go to 9800

 9300   continue
        write(emsg,9310)
 9310   format(' Cannot write to new instruction file obscalc.ins.')
        go to 9800

 9800 continue
      if ( present(stat) ) stat = 1
      if (present(stat_msg)) stat_msg = emsg
      if ( .not. (present(stat) .and. present(stat_msg) ) ) then
         write(6,FMT='(//," Error in subroutine write_new_pestfile:")')
         write(6,FMT='("  ",a)') emsg
      end if
 9900 continue
      close(unit=oldunit,iostat=ierr)
      close(unit=newunit,iostat=ierr)
      end subroutine write_new_pest_file


      subroutine writeModelBat(so, oldfile, outfle,
     +     stat, stat_msg)
C######################################################################
C subroutine writeModelBat(oldfile, newfile, modOutFiles,stat, stat_msg)
C     This subroutine creates a new batch file to run the model
C     with super observations.  This new batch file calls OBJCALC after the
C     model is run to compute the residuals associates with the super
C     observations.
C         so       = structure of type(super_obs_struc).   Super observation
C                    information is stored in this structure
C         oldfile  = the name of the batch used to run the model in the
C                    original PEST control file
C         outfle   = array of model output file names.  Outfle from the
C                    pestadat module should be used
C         stat     = optional integer flag that signals an error occured
C                    when the subroutine was called
C         stat_smg = optional character array that stores an error meesage
C                    describing any errors that occured when the subroutine
C                    was called
C
C    note: this subroutine also uses PEST information stored in the module
C          pestdata
C######################################################################
      use types
      implicit none
      type(super_obs_struc), intent(inout) :: so
      character(len=*), intent(in)         :: oldfile
      character(len=*), intent(in)         :: outfle(:)
      integer, intent(out), optional       :: stat
      character(len=*), intent(inout), optional ::  stat_msg

      character(len=200) :: newfile
      character(len=200) :: afile
      character(len=200) :: emsg
      character(len=2000) :: cline
      integer :: oldunit, newunit, i, ierr

C     Initialize error handling varaibles
      emsg = ""
      ierr=0
      if ( present(stat) ) stat = 0
      if (present(stat_msg)) stat_msg = ""

      newfile=so%modelscript
      call addquote(newfile,afile)
      write(6,1) trim(afile)
1     format(/,' - writing new model batch file ',a,'...')

      oldunit = 23
      newunit = 24
C open files
      call addquote(oldfile,afile)
      open(unit=oldunit,file=oldfile,status='old',action='read',
     +     iostat=ierr)
      if (ierr /= 0) then
         write(emsg,10) trim(afile)
10       format(' Cannot open existing model batch file ',a,'.')
         go to 9800
      end if
      call addquote(newfile,afile)
      open(unit=newunit,file=afile,status='unknown',action='write',
     +     iostat=ierr)
      if (ierr /= 0) then
         write(emsg,15) trim(afile)
15       format(' Cannot open new model batch file ',a,' for output.')
         goto 9800
      end if

#ifdef UNIX
      write(newunit,20,err=9220)
20    format('# This portion of the model script file is added by ',
     +'SUPOBSPREP.')
      write(newunit,25,err=9220)
25    format('#')
      write(newunit,30,err=9220)
30    format('# Model output files are deleted')
      do i = 1, size(outfle)
        if(so%insfletype(i) == 1) then
          call addquote(outfle(i),afile)
          write(newunit,40,err=9220) trim(afile)
40        format('/bin/rm ',a,' > /dev/null')
        end if
      end do
      write(newunit,25,err=9220)
      write(newunit,60,err=9220)
60    format('# The next portion of the model script file is copied ',
     +'from the original.')
      write(newunit,25,err=9220)
#else
      write(newunit,19)
19    format('@echo off')
      write(newunit,21,err=9220)
21    format('REM This portion of the model batch file is added by ',
     +'SUPOBSPREP.')
      write(newunit,26,err=9220)
26    format('REM')
      write(newunit,31,err=9220)
31    format('REM Model output files are deleted')
      do i = 1, size(outfle)
        if(so%insfletype(i) == 1) then
          call addquote(outfle(i),afile)
          write(newunit,41,err=9220) trim(afile)
41        format('del ',a,' > nul')
        end if
      end do
      write(newunit,26,err=9220)
      write(newunit,60,err=9220)
60    format('REM The next portion of the model batch is copied ',
     +'from the original.')
      write(newunit,26,err=9220)
#endif

C read content from orginal batch file and write it to new file
      do
         read(unit=oldunit, FMT='(a)', iostat=ierr, end = 8000) cline
         if (ierr /= 0) goto 9210
         write(unit=newunit, FMT='(a)', iostat=ierr) trim(cline)
         if (ierr /= 0) goto 9220
      end do
 8000 continue

#ifdef UNIX
      write(newunit,25,err=9220)
      write(newunit,80,err=9220)
80    format('# The remainder of this script file is added by ',
     +'SUPOBSPREP.')
      write(newunit,25,err=9220)
      write(newunit,90,err=9220)
90    format('# Super observation differences are now calculated ',
     +'from native observation differences.')
      write(newunit,25,err=9220)
      write(newunit,100,err=9220)
100   format('obscalc > /dev/null')
#else
      write(newunit,26,err=9220)
      write(newunit,81,err=9220)
81    format('REM The remainder of this batch file is added by ',
     +'SUPOBSPREP.')
      write(newunit,26,err=9220)
      write(newunit,91,err=9220)
91    format('REM Super observation differences are now calculated ',
     +'from native observation differences.')
      write(newunit,26,err=9220)
      write(newunit,101,err=9220)
101   format('obscalc > nul')
#endif

      call addquote(newfile,afile)
      write(6,200) trim(afile)
200   format(' - file ',a,' written ok.')

      goto 9900

C Process error messages

 9210  continue
       call addquote(oldfile,afile)
       write(emsg,9211) trim(afile)
 9211  format(' Cannot read from existing model batch file ',a,'.')
       go to 9800
 9220  call addquote(newfile,afile)
       write(emsg,9221) trim(afile)
 9221  format(' Cannot write to new model batch file ',a,'.')
       go to 9800

 9800 continue
      if ( present(stat) ) stat = 1
      if (present(stat_msg)) stat_msg = emsg
      if ( .not. (present(stat) .and. present(stat_msg) ) ) then
         write(6,FMT='(//," Error in subroutine write_new_pestfile:")')
         write(6,FMT='("  ",a)') emsg
      end if
 9900 continue
      close(unit=oldunit)
      close(unit=newunit)
      end subroutine writeModelBat




      subroutine writeSupoJco(jcofile_exist,so, stat, stat_msg)
C######################################################################
C  subroutine writeSupoJco(so, stat, stat_msg)
C     This subroutine writes a jacbian file for the super parameter
C     pest simulation
C       variables:
C         so       = structure of type(super_obs_struc).   Super observation
C                    information is stored in this structure
C         stat    =  optional integer flag that signals an error occured
C                    when the subroutine was called
C         stat_smg = optional character array that stores an error meesage
C                    describing any errors that occured when the subroutine
C                    was called
C
C    note: this subroutine also uses PEST information stored in the module
C          pestdata
C######################################################################
      use pestdata
      use types
      use string_utils
      implicit none

      character (len=*), intent(in) :: jcofile_exist
      type(super_obs_struc), intent(inout) :: so
      integer, intent(out), optional :: stat
      character(len=*), intent(inout), optional ::  stat_msg


      double precision, allocatable :: xnew(:,:)
      character(len=20), allocatable :: saobs(:)
      character(len=8) :: atemp8
      character(len=200) :: emsg
      character(len=200) ::jcofile, afile
      character(len=12) :: apar2
      integer :: i, j, ierr, iob, ipar, isob
      integer icount, iobs,ipp
      integer :: snxrow, snreg
      integer :: itemp1,itemp2,bn,ies,irow
      double precision :: dtemp

C     Initialize error handling varaibles
      emsg = ""
      ierr=0
      if ( present(stat) ) stat = 0
      if (present(stat_msg)) stat_msg = ""

C -- Re-read the JCO file (we deallocated the x matrix to save memory).

      if(jcofile_exist.ne.' ')then
        call addquote(jcofile_exist,afile)
        write(6,5) trim(afile)
5       format(/,' - re-reading Jacobian matrix file ',a,'...')
        call open_unformatted_file(ierr,11,'read',jcofile_exist,emsg)
        if(ierr.ne.0)then
            write(emsg,FMT='(2a)')
     +           'Cannot open Jacobian matrix file ',
     +           trim(afile)
           go to 9800
        end if
        read(11,err=9000,end=9000)itemp1,itemp2
        itemp2=abs(itemp2)
        bn=itemp2
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
        write(6,291) trim(afile)
291     format(' - file ',a,' re-read ok.')
      end if

      write(6,10)
10    format(/,' - calculating super observation Jacobian matrix...')

c     Calculate the number of observations with regularisation
      snreg = 0
      do i = 1, nobs
         if( tolower( obgnme( nobgnm(i)),5 ) == 'regul')then
            snreg = snreg + 1
         end if
      end do

      snxrow = so%num + snreg + nprior

      allocate(saobs(snxrow), stat=ierr)
C     Fill array saobs with observation names for super parameter PEST case
C     First the super observations
      do isob = 1, so%num
         call writint(atemp8, isob)
         saobs(isob) = trim('obs'//atemp8)
      end do
C     Next the regularised opservations
      isob =  so%num
      do j = 1, nobs
         if( tolower( obgnme( nobgnm(j)),5 ) == 'regul')then
            isob = isob + 1
            saobs(isob) = aobs(j)
         end if
      end do
C     Finally add prior information
        isob =  so%num + snreg
      if(nprior.ne.0)then
        do j = nobs+1, nobs+nprior
           isob = isob + 1
           saobs(isob) = aobs(j)
        end do
      end if

C Allocate space for new jacobian
      allocate(xnew(snxrow, nespar))
C  first fill so%num rows of xnew with super observation sensitivities
      do isob = 1, so%num
         do ipar = 1, nespar
            xnew(isob, ipar) = 0
            do ieig = 1, nobs - snreg
               xnew(isob, ipar)=xnew(isob, ipar)
     +              +so%u(ieig, isob)*x(so%map(ieig), ipar)
            end do
         end do
      end do
C Now take the original sensitivities of the regularisation observations
      isob =  so%num
      do iob = 1, nobs
         if( tolower( obgnme( nobgnm(iob)),5 ) == 'regul')then
            isob = isob + 1
            do ipar = 1, nespar
               xnew(isob, ipar) = x(iob, ipar)
             end do
         end if
      end do
      isob =  so%num + snreg
      if(nprior.ne.0)then
        do iob = nobs+1, nobs+nprior
           isob = isob + 1
           do ipar = 1, nespar
              xnew(isob, ipar) = x(iob, ipar)
           end do
        end do
      end if
C -- The Jacobian matrix is written.
      jcofile=trim(so%newpestfile(1:len_trim(so%newpestfile)-3)
     +     //'jco')
#ifdef UNIX
      open(unit=15,file=jcofile,form='unformatted',
     +action='write',err=9400)
#else
#ifdef LAHEY
#ifdef LF90
      open(unit=15,file=jcofile,form='unformatted',
     +access='transparent',action='write',err=9400)
#else
      open(unit=15,file=jcofile,form='binary',
     +action='write',err=9400)
#endif
#else
      open(unit=15,file=jcofile,form='unformatted',
     +action='write',err=9400)
#endif
#endif
      write(15,err=9400) -nespar,-snxrow
      icount=0
       do i=1,nespar
          do j=1,snxrow
            if(xnew(j,i).ne.0.0d0) icount=icount+1
          end do
       end do
       write(15,err=9400) icount
       do i=1,nespar
         do j=1,snxrow
           if(xnew(j,i).ne.0.0d0)then
             icount=(i-1)*snxrow+j
             write(15,err=9400)icount,xnew(j,i)
           end if
         end do
       end do
       do ipp=1,npar
         if((itrans(ipp).eq.0).or.(itrans(ipp).eq.1))then
            write(15,err=9400) apar(ipp)
         end if
       end do
       do iobs=1,snxrow
         write(15,err=9400) saobs(iobs)
       end do
       close(unit=15)
       call addquote(jcofile,afile)
       write(6,490) trim(afile)
490    format(' - superobservation Jacobian matrix written to ',
     + 'file ',a,'.')

       go to 9900

 9000  write(emsg,FMT='(3a)')
     +      'Error encountered in re-reading Jacobian matrix file "',
     +      trim(jcofile_exist), '".'
       goto 9800

9400   call addquote(jcofile,afile)
       write(emsg,9410) trim(afile)
9410   format('Cannot write to file ',a,'.')
       go to 9800

9800  continue
      if ( present(stat) ) stat = 1
      if (present(stat_msg)) stat_msg = emsg
      if ( .not. (present(stat) .and. present(stat_msg) ) ) then
         write(6,FMT='(//," Error in subroutine write_new_pestfile:")')
         write(6,FMT='("  ",a)') emsg
      end if

9900   continue
       if(allocated(saobs)) deallocate(saobs)
       if(allocated(xnew)) deallocate(xnew)
      end subroutine writeSupoJco




      subroutine writeSupMat(so, stat, stat_msg)
C######################################################################
C  subroutine writeSupMat(so, stat, stat_msg)
C     This subroutine writes super observation eigenvectors in PEST matrix
C     file format.
C       variables:
C         so       = structure of type(super_obs_struc).   Super observation
C                    information is stored in this structure
C         stat    =  optional integer flag that signals an error occured
C                    when the subroutine was called
C         stat_smg = optional character array that stores an error meesage
C                    describing any errors that occured when the subroutine
C                    was called
C
C    note: this subroutine also uses PEST information stored in the module
C          pestdata
C######################################################################
       use types
       use pestdata
       use string_utils
       implicit none

       type(super_obs_struc), intent(inout)      :: so
       integer, intent(out), optional            :: stat
       character(len=*), intent(inout), optional :: stat_msg

       integer   :: j,ierr,i
       character(len=25)  :: anum
       character(len=200) :: emsg
       character(len=200) :: afile

C      Initialize error handling varaibles
       emsg = ""
       ierr=0
       if ( present(stat) ) stat = 0
       if (present(stat_msg)) stat_msg = ""

       write(6,10)
10     format(/,' - writing super observation matrix file...')

C -- The matrix file is written.

       open(unit=15,file=so%newmatfile,action='write',err=9400)
       write(15,'(3i12)',err=9400) so%nobs_noreg,so%num,2
       do i=1,so%nobs_noreg
         write(15,15,err=9400) (so%u(i,j) , j=1,so%num)
15       format(8(1x,1pg14.6E3))
       end do
       write(15,20,err=9400)
20     format('* row names')
       do j = 1, nobs
         if( tolower( obgnme( nobgnm(j)),5 ) /= 'regul')then
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
      if ( present(stat) ) stat = 1
      if (present(stat_msg)) stat_msg = emsg
      if ( .not. (present(stat) .and. present(stat_msg) ) ) then
         write(6,FMT='(//," Error in subroutine WriteSupMat:")')
         write(6,FMT='("  ",a)') emsg
      end if

9900   continue
      end subroutine writeSupMat


      subroutine writeSupParMat(so, stat, stat_msg)
C######################################################################
C  subroutine writeSupParMat(so, stat, stat_msg)
C     This subroutine writes super parameter eigenvectors in PEST matrix
C     file format.
C       variables:
C         so       = structure of type(super_obs_struc).   Super observation
C                    information is stored in this structure
C         stat    =  optional integer flag that signals an error occured
C                    when the subroutine was called
C         stat_smg = optional character array that stores an error meesage
C                    describing any errors that occured when the subroutine
C                    was called
C
C    note: this subroutine also uses PEST information stored in the module
C          pestdata
C######################################################################
       use types
       use pestdata
       use string_utils
       implicit none

       type(super_obs_struc), intent(inout)      :: so
       integer, intent(out), optional            :: stat
       character(len=*), intent(inout), optional :: stat_msg

       integer   :: j,ierr,i
       character(len=25)  :: anum
       character(len=200) :: emsg
       character(len=200) :: afile

C      Initialize error handling varaibles
       emsg = ""
       ierr=0
       if ( present(stat) ) stat = 0
       if (present(stat_msg)) stat_msg = ""

       write(6,10)
10     format(/,' - writing super parameter matrix file...')

C -- The matrix file is written.

       open(unit=15,file=so%newvtmatfile,action='write',err=9400)
       write(15,'(3i12)',err=9400) nespar,so%num,2
       do j=1,nespar
         write(15,15,err=9400) (so%vt(i,j) , i=1,so%num)
15       format(8(1x,1pg14.6E3))
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
      if ( present(stat) ) stat = 1
      if (present(stat_msg)) stat_msg = emsg
      if ( .not. (present(stat) .and. present(stat_msg) ) ) then
         write(6,FMT='(//," Error in subroutine WriteSupParMat:")')
         write(6,FMT='("  ",a)') emsg
      end if

9900   continue
      end subroutine writeSupParMat





      subroutine calcWeightedSens(jcofile, xx, jacdim, stat, stat_msg)
C######################################################################
C   calcWeightedSens(jcofile, xx, stat, stat_msg)
C     This subroutine reads the PEST jacobian file and calculates
C     the weighted sesitivity matrix
C       varaibles:
C         jcofile = the name of the jacobian file to be read
C         xx      = an allocable array to which the weighted sensitivity
C                   values will be read
C         jacdim  = leading dimension of matrix in JCO file
C         stat    = optional integer flag that signals an error occured
C                    when the subroutine was called
C         stat_smg = optional character array that stores an error meesage
C                    describing any errors that occured when the subroutine
C                    was called
C
C    note: this subroutine also uses PEST information stored in the module
C          pestdata
C######################################################################
      use pestdata, only  : maxcompdim,pestmode,npar,nobsgp,nxrow,
     +     errmsg,apar,obgnme,aobs,itrans,numcov,
     +     covind,owght,x,pval_o,modval_o,
     +     parchglim,pval,parlbnd,parubnd,
     +     ipargp,scale,offset,dercom,covgp,nespar,
     +     nobs,nobgnm,covar,nobscov,oval,cline,
     +     ninsfle, insfle, outfle
      implicit none
      character(len=*), intent(in) :: jcofile
      double precision, intent(inout), allocatable :: xx(:,:)
      integer, intent(out) :: jacdim
      integer, intent(out), optional :: stat
      character(len=*), intent(inout), optional ::  stat_msg

      character(len=200) :: emsg
      character(len=200) :: afile
      character(len=20) :: aaobs
      character(len=12) :: aapar
      character(len=15) :: acov
      character(len=10):: anum
      integer :: ierr, itemp, itemp1, itemp2, bn, igroup
      integer :: icount, i, j, k, ies, irow
      integer :: maxobs, icov, lwork, info
      integer :: ldvt,iicount,icovrow
      double precision :: dtemp, rtemp
      double precision, allocatable:: workvec(:), w1(:)
      double precision, allocatable::  u(:,:)
      double precision vt(1,1)
      integer, allocatable :: iindex(:)

C     Initialize error handling varaibles
      emsg = ""
      ierr=0
      if ( present(stat) ) stat = 0
      if (present(stat_msg)) stat_msg = ""

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
         do icov=1,numcov
           if(nobscov(icov).gt.maxobs) maxobs=nobscov(icov)
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
         do icov=1,numcov
           itemp=nobscov(icov)
           if(itemp.eq.0) cycle
           iicount=iicount+1
           call dgesvd('A','N',itemp,itemp,covar(icov)%dval,itemp,
     +     w1,u,maxobs,vt,ldvt,workvec,lwork,info)
           if(info.ne.0)then
             call writint(acov,icov)
             write(emsg,587) trim(acov)
587          format('Cannot undertake SVD on covariance matrix ',
     +       'number ',a,' cited in PEST control file.')
             go to 9800
           end if
           do i=1,itemp
             if(w1(i).le.1.0d-200)then
               call writint(acov,icov)
               write(emsg,582) trim(acov)
582            format('Observation covariance matrix number ',a,
     +         ' cited in PEST control file is singular. ',
     +         'SUPOBSPREP cannot proceed.')
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

C -- A new matrix is allocated.
       if (allocated(xx)) deallocate(xx)
       allocate(xx(nxrow,nespar),stat=ierr)
       if(ierr.ne.0) go to 9200

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
         do icov=1,numcov
           if(nobscov(icov).eq.0) cycle
           igroup=covgp(icov)
           icovrow=0
           do irow=1,nxrow
             if(nobgnm(irow).eq.igroup)then
               icovrow=icovrow+1
               iindex(icovrow)=irow
               if(icovrow.eq.nobscov(icov)) go to 585
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
                 do k=1,nobscov(icov)
                   rtemp=rtemp+
     +                   covar(icov)%dval(icovrow,k)*x(iindex(k),ies)
                 end do
                 xx(irow,ies)=rtemp
               end do
               if(icovrow.eq.nobscov(icov)) go to 589
             end if
           end do
589        continue
         end do
       end if
       write(6,588)
588    format(' - weighted sensitivity matrix calculated ok.')
       if(allocated(w1))      deallocate(w1,stat=ierr)
       if(allocated(workvec)) deallocate(workvec,stat=ierr)
       if(allocated(iindex))   deallocate(iindex,stat=ierr)

       goto 9900                ! no errors
C     Process error messages
 9000  write(emsg,FMT='(3a)')
     +      'Error encountered in reading Jacobian matrix file "',
     +      trim(jcofile), '".'
       goto 9800
 9200  write(errmsg,FMT='(2a)')
     +      'Cannot allocate sufficient memory to continue ',
     +      'execution.'
       go to 9800
 9250  write(emsg, FMT='(6a)')
     +      'Jacobian matrix file ',trim(jcofile),
     +      ' is not compatible ',
     +      'with current PEST control file; ',
     +      'at least one observation ',
     +      'or adjustable parameter is different between them.'
       go to 9800
 9800  continue
       if ( present(stat) ) stat = 1
       if (present(stat_msg)) stat_msg = emsg
       if ( .not. (present(stat) .and. present(stat_msg) ) ) then
          write(6,FMT='(//," Error in subroutine readJacobian:")')
          write(6,FMT='("  ",a)') emsg
       end if
 9900  continue
       close(unit=11,iostat=ierr)
       end subroutine calcweightedsens




      subroutine read_insfile(insfle, aobs, maxobs_in, nobs,
     +     stat, stat_msg)
C######################################################################
C   subroutine read_insfile(insfle, aobs, maxobs_in, local_obs, stat, stat_msg
C     This subroutine reads a PEST instruction file and returns the number of
C     observations in the file aand array containing the observation names
C       variables:
C         insfile  = character string containing the name of the instruction
C                    file to be processed
C         aobs(:)  = an allocateable array of character strings to which the
C                     names of the observations in the file will be written to
C         maxobs_in= the total number of observations in the PEST simulation
C                    This is the maximum number of observation the can possibly
C                    be contained in an instruction file
C         nobs     = The number of observations found in this instruction file
C         stat     = optional integer flag that signals an error occured
C                    when the subroutine was called
C         stat_smg = optional character array that stores an error message
C                    describing any errors that occured when the subroutine
C                    was called
C######################################################################

      implicit none
      character(len=*), intent(in) :: insfle
      character(len=*),intent(inout),dimension(:),allocatable::aobs
      integer, intent(in) :: maxobs_in
      integer, intent(out) :: nobs
      integer, intent(out), optional :: stat
      character(len=*), intent(inout), optional ::  stat_msg


        INTEGER I,J,NBLC,MAXWID,IERR,MCASE,LENIN,LENOUT,CHKTYP,II,
     +  J1,EXTLEN,LENMOD,MMLINE,MAXOBS,IS,IOBS,N1,
     +  N2,ITEMP,N3,NUM1,NUM2,BEGINS,NBLB,MMMLIN,INSNUM,MRKTYP,
     +  ALLMRK,J2,DUMFLG
        INTEGER JLINE,IFAIL,CIL,INS,NOL
        DOUBLE PRECISION RTEMP
        CHARACTER*10 AMXWID,ATEMP*30,TPAR*20,AMXOBS,PARDEL*1,FILADD*1
        CHARACTER*12 AVERSION
        CHARACTER*130 COMLIN, FRM*20,AA*1
        CHARACTER*200 AALINE,AAALIN
        CHARACTER*2120 ERRMSG
C -- File size
        CHARACTER*2010 CLINE,BUF

        INTEGER NBLNK

        COMMON /IERR/IERR,MCASE
        COMMON /MMLINE/MMLINE
        COMMON /ERRMSG/ERRMSG
        COMMON /AALINE/AALINE

C Initialize error handling varaibles
        stat = 0
        stat_msg = ""
        ierr=0
        if ( present(stat) ) stat = 0
        if (present(stat_msg)) stat_msg = ""


        maxobs = maxobs_in
        J1=0
        FILADD=' '
        NOBS=0
        IERR=0
        MAXWID=2000
        CALL WRTINT(AMXWID,MAXWID)
        CALL WRTINT(AMXOBS,MAXOBS)

        IF(ALLOCATED(AOBS) ) then
           IF(SIZE(AOBS)<MAXOBS) THEN
              DEALLOCATE(AOBS)
           END IF
        END IF
        IF ( .NOT.(ALLOCATED(AOBS)) ) THEN
           ALLOCATE(AOBS(MAXOBS),STAT=IFAIL)
           IF(IFAIL.NE.0) THEN
              WRITE(STAT_MSG,5)
 5            FORMAT(' Cannot allocate sufficient memory to run ',
     +             'INSCHEK.')
              GO TO 9800
           END IF
        END IF

C -- THE INSTRUCTION FILE IS OPENED

#ifdef UNICOS
        OPEN(UNIT=40,FILE=INSFLE,STATUS='OLD',RECL=1000,ERR=230)
#else
        OPEN(UNIT=40,FILE=INSFLE,STATUS='OLD',ERR=230)
#endif
        GO TO 250
230     WRITE(STAT_MSG,240) INSFLE(:NBLNK(INSFLE))
240     FORMAT(' Cannot open instruction file ',A,'.')
c        CALL WRTERR
        GO TO 9800
250     CONTINUE

C -- THE INSTRUCTION FILE IS READ AND CHECKED

2640    JLINE=1
        CALL WRTINT(ATEMP,JLINE)
        AALINE=' Line '//ATEMP(1:INDEX(ATEMP,' ')-1)//' of instruction '
     +  //'file '//trim(INSFLE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(40,'(A)',ERR=2900,END=2950) CLINE
        CALL TABREM(CLINE)
        CALL LOWCAS(CLINE)
        IF((CLINE(1:3).EQ.'ptf').or.(CLINE(1:3).eq.'jtf'))THEN
          WRITE(STAT_MSG,2645) trim(INSFLE)
2645      FORMAT(' File ',A,' is a template file: an instruction ',
     +    'file is expected.')
c          CALL WRTERR
          GO TO 9800
        END IF
        IF(((CLINE(1:4).NE.'pif ').AND.
     +      (CLINE(1:4).NE.'jif ')).OR.(NBLNK(CLINE).LT.5))THEN
          WRITE(STAT_MSG,2650) AALINE(1:MMLINE)
2650      FORMAT(A,'first line must be "pif" or "jif" + ',
     +    'marker delimiter.')
c          CALL WRTERR
          GO TO 9800
        END IF
        PARDEL=CLINE(5:5)
        II=ICHAR(PARDEL)
        IF(PARDEL.EQ.' ')THEN
          WRITE(STAT_MSG,2660) AALINE(1:MMLINE)
2660      FORMAT(A,'marker delimiter not found in fifth character ',
     +    'position.')
c          CALL WRTERR
          GO TO 9800
        ELSE IF((PARDEL.EQ.'(').OR.(PARDEL.EQ.')').OR.(PARDEL.EQ.'[')
     +    .OR.(PARDEL.EQ.']').OR.(PARDEL.EQ.'!').OR.
     +    ((II.GE.48).AND.(II.LE.57)).OR.
     +    ((II.GE.97).AND.(II.LE.122)).OR.
     +    (PARDEL.EQ.'&').OR.(PARDEL.EQ.':'))THEN
          WRITE(STAT_MSG,2670) AALINE(1:MMLINE)
2670      FORMAT(A,'illegal marker delimiter.')
c          CALL WRTERR
          PARDEL=' '
          GO TO 9800
        END IF
        IS=0
2680    JLINE=JLINE+1
        CALL WRTINT(ATEMP,JLINE)
        AALINE=' Line '//ATEMP(1:INDEX(ATEMP,' ')-1)//
     +  ' of instruction file '//trim(INSFLE)//': '
        MMLINE=NBLNK(AALINE)+1
        READ(40,'(A)',ERR=2900,END=2950) CLINE
        IF(INDEX(CLINE,CHAR(9)).NE.0)THEN
          IF(INDEX(CLINE,PARDEL).EQ.0) CALL TABREM(CLINE)
        ENDIF
        IF(CLINE.EQ.' ') GO TO 2680
        NBLC=NBLNK(CLINE)
        IF(NBLC.GT.MAXWID)THEN
          WRITE(STAT_MSG,2705) AALINE(1:MMLINE),AMXWID(:NBLNK(AMXWID))
2705      FORMAT(A,'line exceeds ',A,' characters.')
c          CALL WRTERR
          GO TO 9800
        END IF
        CALL SHIFTL(CLINE)
        IF((IS.EQ.0).AND.(CLINE(1:1).EQ.'&'))THEN
          WRITE(STAT_MSG,2706) AALINE(1:MMLINE)
2706      FORMAT(A,'first instruction line in file cannot begin ',
     +    'with continuation character.')
c          CALL WRTERR
           GO TO 9800
        END IF
        IS=1
        N2=0
        IF((CLINE(1:1).NE.'l').AND.(CLINE(1:1).NE.'L').AND.
     +  (CLINE(1:1).NE.'&').AND.(CLINE(1:1).NE.PARDEL))THEN
          WRITE(STAT_MSG,2685) AALINE(1:MMLINE)
2685      FORMAT(A,'instruction line must begin with "l", a ',
     +    'marker delimiter or "&" (for continuation).')
c          CALL WRTERR
           GO TO 9800
        END IF
        IF(CLINE(1:1).NE.'&') J1=0
2690    NBLC=NBLNK(CLINE)
2700    CALL GETINT(IFAIL,CLINE,N1,N2,NBLC,PARDEL)
        IF(IFAIL.NE.0)THEN
          WRITE(STAT_MSG,2710) AALINE(1:MMLINE)
2710      FORMAT(A,'unbalanced marker delimiters.')
c          CALL WRTERR
          GO TO 9800
        END IF
        IF(N1.EQ.0) GO TO 2680
        IF((CLINE(N1:N1).EQ.'l').OR.(CLINE(N1:N1).EQ.'L'))THEN
          IF(N1.EQ.N2)THEN
            CALL ILINT1(CLINE(N1:N2))
            STAT_MSG=ERRMSG
            GO TO 9800
          END IF
          WRITE(FRM,2730) N2-N1
2730      FORMAT('(I',I4,')')
          READ(CLINE(N1+1:N2),FRM,ERR=2740) NOL
          GO TO 2750
2740      CALL ILINT1(CLINE(N1:N2))
          STAT_MSG=ERRMSG
          GO TO 9800
2750      IF(N1.NE.1)THEN
            WRITE(STAT_MSG,2720) AALINE(1:MMLINE)
2720        FORMAT(A,'line advance instruction can only occur at the ',
     +      'beginning of an instruction line.')
c            CALL WRTERR
             GO TO 9800
          END IF
          IF(NOL.LE.0) THEN
            CALL ILINT1(CLINE(N1:N2))
            WRITE(STAT_MSG,2755) trim(errmsg)
2755        FORMAT(a,'  (Integer following "l" must be positive.)')
c            CALL WRTERR
             GO TO 9800
          END IF
        ELSE IF(CLINE(N1:N1).EQ.PARDEL)THEN
          IF(INDEX(CLINE(N1:N2),CHAR(9)).NE.0)THEN
            WRITE(STAT_MSG,2686) AALINE(1:MMLINE)
2686        FORMAT(A,'tab character embedded in marker; space ',
     +      'characters must be used.')
c            CALL WRTERR
             GO TO 9800
          END IF
          IF(N2-N1.EQ.1)THEN
            WRITE(STAT_MSG,2760)AALINE(1:MMLINE)
2760        FORMAT(A,'marker has zero length.')
c            CALL WRTERR
             GO TO 9800
          ELSE
            J1=J1+N2-N1-1
            IF(J1.GT.MAXWID)THEN
              WRITE(STAT_MSG,2765) AALINE(1:MMLINE),
     +        AMXWID(:NBLNK(AMXWID))
2765          FORMAT(A,'model output file width cannot exceed ',A,
     +        ' characters.')
c              CALL WRTERR
              GO TO 9800
            END IF
          ENDIF
        ELSE IF(CLINE(N1:N1).EQ.'&')THEN
          IF(N1.NE.N2)THEN
            CALL ILINT1(CLINE(N1:N2))
            STAT_MSG=ERRMSG
            GO TO 9800
          ELSE
            IF(N1.GT.1)THEN
              WRITE(STAT_MSG,2770) AALINE(1:MMLINE)
2770          FORMAT(A,'"&" is a continuation character and can only ',
     +        'occur at the beginning of an instruction line.')
c              CALL WRTERR
               GO TO 9800
            END IF
          END IF
        ELSE IF((CLINE(N1:N1).EQ.'w').OR.(CLINE(N1:N1).EQ.'W'))THEN
          IF(N1.NE.N2)THEN
            CALL ILINT1(CLINE(N1:N2))
            STAT_MSG=ERRMSG
            GO TO 9800
          ELSE
            J1=J1+1
            IF(J1.GT.MAXWID) THEN
              WRITE(STAT_MSG,2765) AALINE(1:MMLINE),
     +        AMXWID(:NBLNK(AMXWID))
c              CALL WRTERR
              GO TO 9800
            END IF
          ENDIF
        ELSE IF((CLINE(N1:N1).EQ.'t').OR.(CLINE(N1:N1).EQ.'T'))THEN
          IF(N1.EQ.N2)THEN
            CALL ILINT1(CLINE(N1:N2))
            STAT_MSG=ERRMSG
            GO TO 9800
          END IF
          WRITE(FRM,2730)N2-N1
          READ(CLINE(N1+1:N2),FRM,ERR=2780) ITEMP
          GO TO 2790
2780      CALL ILINT1(CLINE(N1:N2))
          STAT_MSG=ERRMSG
          GO TO 9800
2790      IF(ITEMP.LE.0)THEN
            CALL ILINT1(CLINE(N1:N2))
            WRITE(STAT_MSG,2848) trim(errmsg)
2848        FORMAT(a,'  (Number following "t" must be greater than ',
     +      'zero.)')
c            CALL WRTERR
             GO TO 9800
          ELSE IF(ITEMP.LT.J1)THEN
            CALL ILINT1(CLINE(N1:N2))
            WRITE(STAT_MSG,2800) trim(errmsg)
2800        FORMAT(a,'  (Tab position results in backward move - ',
     +      'model output file must be read from left to right.)')
c            CALL WRTERR
             GO TO 9800
          ELSE IF(ITEMP.GT.MAXWID)THEN
            CALL ILINT1(CLINE(N1:N2))
            WRITE(STAT_MSG,2847) TRIM(ERRMSG),AMXWID(:NBLNK(AMXWID))
2847        FORMAT(A,'  (Model output file width cannot exceed ',A,
     +      ' characters.)')
c            CALL WRTERR
            GO TO 9800
          ELSE
            J1=ITEMP
          END IF
        ELSE IF((CLINE(N1:N1).EQ.'[').OR.(CLINE(N1:N1).EQ.'('))THEN
          AA=CLINE(N1:N1)
          IF(AA.EQ.'[')THEN
            N3=INDEX(CLINE(N1:N2),']')
          ELSE
            N3=INDEX(CLINE(N1:N2),')')
          END IF
          IF(N3.EQ.0)THEN
            CALL ILINT1(CLINE(N1:N2))
            WRITE(STAT_MSG,2810) TRIM(ERRMSG)
2810        FORMAT(A,'  (Instruction contains unbalanced parentheses.)')
c            CALL WRTERR
            GO TO 9800
          END IF
          N3=N3+N1-1
          IF((N3-1.GT.N1).AND.(N3-1-(N1+1)+1.LE.20))TPAR=
     +    CLINE(N1+1:N3-1)
          CALL LOWCAS(TPAR)
          IF(N3-1.EQ.N1)THEN
            CALL ILINT1(CLINE(N1:N2))
            WRITE(STAT_MSG,2815) TRIM(ERRMSG)
2815        FORMAT(A,'  (Instruction missing observation name.)')
c            CALL WRTERR
             GO TO 9800
          ELSE IF(N3-1-(N1+1)+1.GT.20)THEN
            CALL ILINT1(CLINE(N1:N2))
            WRITE(STAT_MSG,2816) TRIM(ERRMSG)
2816        FORMAT(A,'  (Observation name greater than 20 characters ',
     +      'long.)')
c            CALL WRTERR
             GO TO 9800
          ELSE IF((N3-1-(N1+1)+1.EQ.3).AND.(TPAR.EQ.'dum'))
     +      THEN
            CALL ILINT1(CLINE(N1:N2))
            WRITE(STAT_MSG,2817) TRIM(ERRMSG)
2817        FORMAT(A,'  (Dummy observation name "dum" only allowed ',
     +      'for non-fixed observations.)')
c            CALL WRTERR
             GO TO 9800
          ELSE
            IF(NOBS.EQ.0) THEN
              NOBS=1
              AOBS(1)=TPAR
              IOBS=1
            ELSE
              CALL WHICH1(IFAIL,NOBS,IOBS,AOBS,TPAR)
              IF(IFAIL.NE.0)THEN
                NOBS=NOBS+1
                IF(NOBS.GT.MAXOBS)THEN
                  WRITE(STAT_MSG,2825) AMXOBS(:NBLNK(AMXOBS)),
     +            trim(INSFLE)
2825              FORMAT(' Subroutine read_insfle can handle only ',A,
     +            ' different observations: more than this are cited ',
     +            'in file ',A,'.')
c                  CALL WRTERR
                  GO TO 9800
                END IF
                AOBS(NOBS)=TPAR
                IOBS=NOBS
              ELSE
                WRITE(STAT_MSG,2830)AALINE(1:MMLINE),TPAR(:NBLNK(TPAR))
2830            FORMAT(A,'observation "',A,'" already cited.')
c                CALL WRTERR
                 GO TO 9800
              END IF
            END IF
            IF(N2.EQ.N3)THEN
              CALL ILINT1(CLINE(N1:N2))
              WRITE(STAT_MSG,2840) TRIM(ERRMSG)
c              CALL WRTERR
              GO TO 9800
            END IF
            CALL GETNUM(IFAIL,CLINE,N3,N2,NUM1,NUM2,FRM)
            IF(IFAIL.NE.0)THEN
              CALL ILINT1(CLINE(N1:N2))
              WRITE(STAT_MSG,2840) TRIM(ERRMSG)
2840          FORMAT(A,'  (Cannot read numbers in format "N1:N2" ',
     +        'after observation name.)')
c              CALL WRTERR
               GO TO 9800
            ELSE IF(NUM2.LE.NUM1)THEN
              CALL ILINT1(CLINE(N1:N2))
              WRITE(STAT_MSG,2845) TRIM(ERRMSG)
2845          FORMAT(A,'  (Numbers in format "N1:N2" following ',
     +        'observation name must be in increasing order.)')
c              CALL WRTERR
               GO TO 9800
            ELSE IF (NUM2.GT.MAXWID) THEN
              CALL ILINT1(CLINE(N1:N2))
              WRITE(STAT_MSG,2847) TRIM(ERRMSG),AMXWID(:NBLNK(AMXWID))
c              CALL WRTERR
              GO TO 9800
            ELSE
              IF(NUM1.LT.J1)THEN
                WRITE(STAT_MSG,2850) AALINE(1:MMLINE)
2850            FORMAT(A,
     +          'model output file must be read from left to right.')
c                CALL WRTERR
                 GO TO 9800
              END IF
              J1=NUM2
            END IF
          END IF
        ELSE IF(CLINE(N1:N1).EQ.'!')THEN
          IF(N1.EQ.N2)THEN
            CALL ILINT1(CLINE(N1:N2))
            STAT_MSG=ERRMSG
            GO TO 9800
          END IF
          ITEMP=INDEX(CLINE(N1+1:N2),'!')
          IF(ITEMP.EQ.0)THEN
            CALL ILINT1(CLINE(N1:N2))
            WRITE(STAT_MSG,2860) TRIM(ERRMSG)
2860        FORMAT(A,'  ("!"s not balanced.)')
c            CALL WRTERR
            GO TO 9800
          END IF
          IF(CLINE(N2:N2).NE.'!')THEN
            CALL ILINT1(CLINE(N1:N2))
            WRITE(STAT_MSG,2870) TRIM(ERRMSG)
2870        FORMAT(A,'  (Instruction must end in "!".)')
c            CALL WRTERR
            GO TO 9800
          END IF
          IF(N1+1.EQ.N2)THEN
            CALL ILINT1(CLINE(N1:N2))
            WRITE(STAT_MSG,2815) TRIM(ERRMSG)
c            CALL WRTERR
             GO TO 9800
          ELSE IF(N2-1-(N1+1)+1.GT.20)THEN
            CALL ILINT1(CLINE(N1:N2))
            WRITE(STAT_MSG,2816) TRIM(ERRMSG)
c            CALL WRTERR
             GO TO 9800
          ELSE
            TPAR=CLINE(N1+1:N2-1)
            CALL LOWCAS(TPAR)
            IF(TPAR.EQ.'dum') GO TO 2700
            IF(NOBS.EQ.0) THEN
              NOBS=1
              AOBS(1)=TPAR
              IOBS=1
            ELSE
              CALL WHICH1(IFAIL,NOBS,IOBS,AOBS,TPAR)
              IF(IFAIL.NE.0)THEN
                NOBS=NOBS+1
                IF(NOBS.GT.MAXOBS)THEN
                  WRITE(STAT_MSG,2825) AMXOBS(:NBLNK(AMXOBS)),
     +            trim(INSFLE)
c                  CALL WRTERR
                  GO TO 9800
                END IF
                AOBS(NOBS)=TPAR
                IOBS=NOBS
              ELSE
                WRITE(STAT_MSG,2830) AALINE(1:MMLINE),TPAR(:NBLNK(TPAR))
c                CALL WRTERR
                 GO TO 9800
              END IF
            END IF
          END IF
        ELSE
          CALL ILINT1(CLINE(N1:N2))
          STAT_MSG=ERRMSG
          GO TO 9800
        END IF
        GO TO 2700

2900    WRITE(STAT_MSG,2910) AALINE(1:MMLINE)
2910    FORMAT(A,'cannot read line.')
c        CALL WRTERR
        GO TO 9800
2950    IF(JLINE.EQ.1)THEN
          WRITE(STAT_MSG,2960) AALINE(1:MMLINE)
2960      FORMAT(A,'unexpected end to file.')
c          CALL WRTERR
          GO TO 9800
        END IF
        IF(IERR.NE.0) GO TO 9800

        IF(NOBS.EQ.0)THEN
          WRITE(STAT_MSG,295) INSFLE(:NBLNK(INSFLE))
295       FORMAT(' No observations identified in file ',A,'.')
c          CALL WRTERR
          GO TO 9800
        END IF
        IF(IERR.NE.0) GO TO 9800
C        WRITE(6,300,ERR=9000)
C300     FORMAT(' No errors encountered.')

C -- SOME ERROR MESSAGES .....

9000    goto 9999
9050    WRITE(STAT_MSG,9060) AMXWID(:NBLNK(AMXWID))
9060    FORMAT(' Model output file width exceeds ',A,' characters.')
c        CALL WRTERR
        GO TO 9820
9070    WRITE(STAT_MSG,9080) AALINE(1:MMLINE)
9080    FORMAT(' Inexplicable error while reading model output file ',
     +  A,'.')
        GO TO 9820
9100    WRITE(STAT_MSG,9110) AALINE(1:MMLINE)
9110    FORMAT(' Line advance moves cursor beyond end of file.',A)
c        CALL WRTERR
        GO TO 9820
9150    WRITE(STAT_MSG,9160) AALINE(1:MMLINE)
9160    FORMAT(' End of file encountered while searching for ',
     +  'marker.',A)
c        CALL WRTERR
        GO TO 9820
9200    WRITE(STAT_MSG,9210) AAALIN(1:MMMLIN),AALINE(1:MMLINE)
9210    FORMAT(A,'unable to find secondary marker.',A)
c        CALL WRTERR
        GO TO 9820
9250    WRITE(STAT_MSG,9260) AAALIN(1:MMMLIN),AALINE(1:MMLINE)
9260    FORMAT(A,'unable to find requested whitespace or ',
     +  'whitespace preceeds end of line.',A)
c        CALL WRTERR
        GO TO 9820
9300    WRITE(STAT_MSG,9310) AAALIN(1:MMMLIN),TPAR(:NBLNK(TPAR)),
     +  AALINE(1:MMLINE)
9310    FORMAT(A,'cannot locate observation "',A,'".',A)
c        CALL WRTERR
        GO TO 9820
9350    WRITE(STAT_MSG,9360) AAALIN(1:MMMLIN),TPAR(:NBLNK(TPAR)),
     +  AALINE(1:MMLINE)
9360    FORMAT(A,'error reading observation "',A,'".',A)
c        CALL WRTERR
        GO TO 9820
9500    WRITE(STAT_MSG,9510) INSFLE(:NBLNK(INSFLE))
9510    FORMAT(' Error re-reading instruction file ',A,'.')
c        CALL WRTERR
        GO TO 9820
9550    WRITE(STAT_MSG,9560) INSFLE(:NBLNK(INSFLE))
9560    FORMAT(' Unable to backspace in file ',A,'.')
c        CALL WRTERR
        GO TO 9820
9600    WRITE(STAT_MSG,9610) INSFLE(:NBLNK(INSFLE))
9610    FORMAT(' Cannot rewind instruction file ',A,'.')
c        CALL WRTERR
        GO TO 9820
9800    CONTINUE
C        OPEN(UNIT=36,FILE='pest.tmp',STATUS='UNKNOWN',ERR=9000)
C        WRITE(36,'(a)',ERR=9000)'1'
C        CLOSE(UNIT=36,ERR=9000)
 9820   continue
        if(trim(STAT_MSG) /= "") stat = 1
 9999    continue
        END

C######################################################################
C######################################################################
C
C  Low Level Supporting Subroutines
C
C######################################################################
C######################################################################

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

        SUBROUTINE WRTINT(ATEMP,IVAL)

C -- SUBROUTINE WRITINT WRITES AN INTEGER TO A STRING

        INTEGER IVAL
        CHARACTER*(*) ATEMP

        WRITE(ATEMP,'(i10)')IVAL
        CALL SHIFTL(ATEMP)
        RETURN
        END


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

        SUBROUTINE WRTERR

C -- SUBROUTINE WRTERR FORMATS AND WRITES AN ERROR MESSAGE

        INTEGER NBLNK
        INTEGER IERR,J,JEND,I,NBLC,ITAKE,MCASE
        CHARACTER*2120 ERRMSG

        COMMON /IERR/IERR,MCASE
        COMMON /ERRMSG/ERRMSG

        IERR=IERR+1
        IF(IERR.GT.100)THEN
          WRITE(6,*,ERR=200)
C          OPEN(UNIT=36,FILE='pest.tmp',STATUS='unknown',ERR=200)
C          WRITE(36,'(a)',ERR=200)'1'
C          CLOSE(UNIT=36,ERR=200)
          CALL EXIT(50)
        END IF
        ITAKE=0
        J=0
        NBLC=NBLNK(ERRMSG)
5       JEND=J+78-ITAKE
        IF(JEND.GE.NBLC) GO TO 100
        DO 10 I=JEND,J+1,-1
        IF(ERRMSG(I:I).EQ.' ') THEN
          IF(ITAKE.EQ.0) THEN
            WRITE(6,'(a)',ERR=200) ERRMSG(J+1:I)
            ITAKE=3
          ELSE
            WRITE(6,'(a)',ERR=200) '   '//ERRMSG(J+1:I)
          END IF
          J=I
          GO TO 5
        END IF
10      CONTINUE
        IF(ITAKE.EQ.0)THEN
          WRITE(6,'(a)',ERR=200) ERRMSG(J+1:JEND)
          ITAKE=3
        ELSE
          WRITE(6,'(a)',ERR=200) '   '//ERRMSG(J+1:JEND)
        END IF
        J=JEND
        GO TO 5
100     JEND=NBLC
        IF(ITAKE.EQ.0)THEN
          WRITE(6,'(a)',ERR=200) ERRMSG(J+1:JEND)
        ELSE
          WRITE(6,'(a)',ERR=200) '   '//ERRMSG(J+1:JEND)
        END IF
        RETURN

200     CALL EXIT(100)
        END


        INTEGER FUNCTION NBLNK(ASTRNG)
        CHARACTER*(*) ASTRNG

        NBLNK=LEN_TRIM(ASTRNG)
        RETURN
        END FUNCTION NBLNK

        SUBROUTINE TABREM(CLINE)

C -- SUBROUTINE TABREM REMOVES TABS FROM A STRING

        INTEGER I
        CHARACTER*(*) CLINE

        DO 10 I=1,LEN(CLINE)
10      IF(ICHAR(CLINE(I:I)).EQ.9) CLINE(I:I)=' '

        RETURN
        END

        SUBROUTINE SHIFTL(AA)

C -- SUBROUTINE SHIFTL REMOVES LEADING BLANK CHARACTERS FROM A
C -- STRING

        INTEGER L,I,J,II
        CHARACTER*(*) AA

        L=LEN(AA)
        DO 10 I=1,L
        IF((AA(I:I).NE.' ').AND.(ICHAR(AA(I:I)).NE.9)) GO TO 50
10      CONTINUE
        RETURN
50      IF(I.EQ.1) RETURN
        II=I-1
        DO 100 J=I,L
100     AA(J-II:J-II)=AA(J:J)
        DO 110 J=1,II
110     AA(L-J+1:L-J+1)=' '
        RETURN
        END

        SUBROUTINE ILINT1(AINSTR)

C -- SUBROUTINE ILLINT1 ASSISTS IN WRITING ERROR MESSAGES

        INTEGER MMLINE
        CHARACTER*200 ERRMSG*2120,AALINE
        CHARACTER*(*) AINSTR

        COMMON /ERRMSG/ ERRMSG
        COMMON /AALINE/ AALINE
        COMMON /MMLINE/MMLINE

        WRITE(ERRMSG,10) AALINE(1:MMLINE),AINSTR
10      FORMAT(A,'illegal instruction - "',A,'".')
        CALL WRTERR

        RETURN
        END

        SUBROUTINE GETNUM(IFAIL,BUF,N3,N2,NUM1,NUM2,FMT)

C -- SUBROUTINE GETNUM DETERMINES THE CHARACTER POSITIONS OF FIXED
C -- AND SEMI-FIXED OBSERVATIONS

        INTEGER N3,NUM1,NUM2,I,N2
        INTEGER IFAIL
        CHARACTER*(*) BUF
        CHARACTER*(*) FMT

        IFAIL=0
        I=INDEX(BUF(N3+1:N2),':')
        IF(I.EQ.0) GO TO 100
        WRITE(FMT,20) I-1
20      FORMAT('(I',I3,')')
        READ(BUF(N3+1:N3+I-1),FMT,ERR=100) NUM1
        N3=N3+I
        I=N2-N3
        IF(I.LT.1) GO TO 100
        WRITE(FMT,20) I
        READ(BUF(N3+1:N2),FMT,ERR=100) NUM2
        RETURN
100     IFAIL=1
        RETURN
        END


        SUBROUTINE DREALREAD(IFAIL,CLINE,RTEMP)

C -- Subroutine DREALREAD reads a double precision number from a string.

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
