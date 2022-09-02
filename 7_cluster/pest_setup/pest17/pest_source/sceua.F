
C -------------------------------------------------------------------------------------
C !!!!!!!!!! MODULE SCEUA_WORK !!!!!!!!!!
C -------------------------------------------------------------------------------------

      module sceua_work

      private

      integer           :: iset=0         ! Used by random number generator
      double precision  :: gset           ! Used by random number generator
      integer           :: outunit
      integer           :: outunit1
      integer           :: stop_reason=0
      integer           :: irestart=1
      integer           :: randcall=0
      integer           :: randcall_slave=0
      integer           :: ialter=0
      character*200     :: restartfile=' '

      real,allocatable  :: sxx(:,:)       ! A single precision "holder" for model run parameter values.
      real,allocatable  :: sobjfn(:)      ! The objective function calculated through a model run.
      real              :: phi_lowest

      integer                   :: ncontrib=1        ! This is only used for reporting purposes
      real, allocatable         :: phi_contrib(:)    ! This is only used for reporting purposes
      character*12, allocatable :: agp_contrib(:)    ! This is only used for reporting purposes

C -- Parallelisation variables.

      integer           :: ipll=0           ! 1 if master SCE in parallelisation; -1 if slave.
      integer           :: iopt=0           ! Used in template files
      integer           :: iunit_slave      ! Unit from which slave control data is read.
      real,allocatable  :: ctl_pll(:,:)
      character*200     :: sce_slave_tplfile=' '
      character*200     :: sce_slave_outfile=' '

C -------------------------------------------------------------------------------------
C -- The following are arrays required by the original SCE program.

C -- The name of each variable as it originally occurs in the subroutine in which
C    it is introduced is followed by the name of the subroutine.

      double precision, allocatable    :: x_main(:,:)
      double precision, allocatable    :: xx_main(:)
      double precision, allocatable    :: bestx_main(:)
      double precision, allocatable    :: worstx_main(:)
      double precision, allocatable    :: xf_main(:)
      double precision, allocatable    :: s_main(:,:)
      double precision, allocatable    :: sf_main(:)
      integer, allocatable             :: lcs_main(:)
      double precision, allocatable    :: cx_main(:,:)
      double precision, allocatable    :: cf_main(:)
      double precision, allocatable    :: xnstd_main(:)
      double precision, allocatable    :: bound_main(:)
      double precision, allocatable    :: criter_main(:)
      double precision, allocatable    :: unit_main(:)

      double precision, allocatable    :: sw_cce(:)
      double precision, allocatable    :: sb_cce(:)
      double precision, allocatable    :: ce_cce(:)
      double precision, allocatable    :: snew_cce(:)

      double precision, allocatable    :: xmax_parstt(:)
      double precision, allocatable    :: xmin_parstt(:)
      double precision, allocatable    :: xmean_parstt(:)

      double precision, allocatable    :: wk_sort(:,:)
      integer, allocatable             :: iwk_sort(:)

C -------------------------------------------------------------------------------------

C -- INTERFACES

       interface

       subroutine feval(ifail,xx,yy)
          integer, intent(out)  :: ifail
          real, intent(in)      :: xx(:)
          real, intent(out)     :: yy
      end subroutine feval

      subroutine mul_feval(ifail,n,xx,yy)
          integer, intent(out)  :: ifail
          integer, intent(in)   :: n
          real, intent(inout)   :: xx(:,:)
          real, intent(inout)   :: yy(:)
      end subroutine mul_feval


      end interface

C -------------------------------------------------------------------------------------

C -- SUBROUTINES

C -- Public subroutines are declared.

       public
     +     sceua,
     +     set_outunits,
     +     allocate_sce_arrays,
     +     deallocate_sce_arrays,
     +     get_stop_reason,
     +     set_restart_data,
     +     set_parallel_data

C -- Code for all subroutines follows.

      contains

      subroutine set_parallel_data(ipll_in,iopt_in,iunit_slave_in,
     +  sce_slave_tplfile_in,sce_slave_outfile_in)

        integer, intent(in)        :: ipll_in
        integer, intent(in)        :: iopt_in
        integer, intent(in)        :: iunit_slave_in
        character*(*), intent(in)  :: sce_slave_tplfile_in
        character*(*), intent(in)  :: sce_slave_outfile_in

        ipll=ipll_in
        iopt=iopt_in
        iunit_slave=iunit_slave_in
        sce_slave_tplfile=sce_slave_tplfile_in
        sce_slave_outfile=sce_slave_outfile_in

        return
      end subroutine set_parallel_data


      subroutine set_restart_data(irestart_in,restartfile_in)
        integer, intent(in)       :: irestart_in
        character*(*), intent(in) :: restartfile_in

        irestart=irestart_in
        restartfile=restartfile_in

        return
      end subroutine set_restart_data


      subroutine set_outunits(outunit1_in,outunit_in,ncontrib_in)
        integer, intent(in)  :: outunit1_in
        integer, intent(in)  :: outunit_in
        integer, intent(in)  :: ncontrib_in
        outunit1=outunit1_in
        outunit=outunit_in
        ncontrib=ncontrib_in
        return
      end subroutine set_outunits


      subroutine allocate_sce_arrays(ifail,nopt,nps,ngs,npg)

C -- Subroutine ALLOCATE_SCE_ARRAYS is used for the allocation of SCE local arrays.

        implicit none
        integer, intent(out)   :: ifail
        integer, intent(in)    :: nopt
        integer, intent(in)    :: nps
        integer, intent(in)    :: ngs
        integer, intent(in)    :: npg

        integer                :: ierr,npt

        ifail=0

        npt=ngs*npg

        allocate(
     +      x_main(npt,nopt),
     +      xx_main(nopt),
     +      bestx_main(nopt),
     +      worstx_main(nopt),
     +      xf_main(npt),
     +      s_main(nps,nopt),
     +      sf_main(nps),
     +      lcs_main(nps),
     +      cx_main(npt,nopt),
     +      stat=ierr)
        if(ierr.ne.0) go to 9890

        allocate(
     +      cf_main(npt),
     +      xnstd_main(nopt),
     +      bound_main(nopt),
     +      criter_main(20),
     +      unit_main(nopt),
     +      sw_cce(nopt),
     +      sb_cce(nopt),
     +      ce_cce(nopt),
     +      snew_cce(nopt),
     +      xmax_parstt(nopt),
     +      xmin_parstt(nopt),
     +      xmean_parstt(nopt),
     +      wk_sort(npt,nopt),
     +      iwk_sort(npt),
     +      stat=ierr)
        if(ierr.ne.0) go to 9890

        if(ipll.le.0)then
          allocate(sxx(nopt,1),sobjfn(1),stat=ierr)
          if(ierr.ne.0) go to 9890
        else
          allocate(sxx(nopt,npt),sobjfn(npt),ctl_pll(2,ngs),stat=ierr)
          if(ierr.ne.0) go to 9890
        end if

        allocate(phi_contrib(ncontrib),agp_contrib(ncontrib),stat=ierr)
        if(ierr.ne.0) go to 9890
        phi_contrib=0.0
        agp_contrib=' '

C -- The following line prevents an error in the original code.
        criter_main=0.0

        return

9890    ifail=1
        return

      end subroutine allocate_sce_arrays




      subroutine deallocate_sce_arrays(ifail)

C -- Subroutine DEALLOCATE_SCE_ARRAYS is used for the deallocation of SCE local arrays.

        implicit none
        integer, intent(out)   :: ifail

        integer                :: ierr,icount

        ifail=0
        icount=0

        deallocate(
     +      x_main,
     +      xx_main,
     +      bestx_main,
     +      worstx_main,
     +      xf_main,
     +      s_main,
     +      sf_main,
     +      lcs_main,
     +      cx_main,
     +      stat=ierr)
        if(ierr.ne.0) icount=icount+1

        deallocate(
     +      cf_main,
     +      xnstd_main,
     +      bound_main,
     +      criter_main,
     +      unit_main,
     +      sw_cce,
     +      sb_cce,
     +      ce_cce,
     +      snew_cce,
     +      xmax_parstt,
     +      xmin_parstt,
     +      xmean_parstt,
     +      wk_sort,
     +      iwk_sort,
     +      stat=ierr)
        if(ierr.ne.0) icount=icount+1

        deallocate(sxx,sobjfn,stat=ierr)
        if(ierr.ne.0) icount=icount+1

        deallocate(phi_contrib,agp_contrib,stat=ierr)
        if(ierr.ne.0) icount=icount+1

        if(allocated(ctl_pll)) deallocate(ctl_pll,stat=ierr)
        if(ierr.ne.0) icount=icount+1

        if(icount.ne.0) go to 9890

        return

9890    ifail=1
        return

      end subroutine deallocate_sce_arrays



      subroutine get_stop_reason(ireason)

        implicit none
        integer, intent(out)  :: ireason

        ireason = stop_reason

        return
      end subroutine get_stop_reason



      subroutine sceua(ifail,a,bl,bu,nopt,maxn,kstop,pcento,iseed,
     &                 ngs,npg,nps,nspl,mings,iniflg,iprint)

c
c
c  SHUFFLED COMPLEX EVOLUTION METHOD FOR GLOBAL OPTIMIZATION
c     -- Version 2.1
c
c  by QINGYUN DUAN
c  DEPARTMENT OF HYDROLOGY & WATER RESOURCES
c  UNIVERSITY OF ARIZONA, TUCSON, AZ 85721
c  (602) 621-9360, email: duan@hwr.arizona.edu
c
c  WRITTEN IN OCTOBER 1990.
c  REVISED IN AUGUST 1991
c  REVISED IN APRIL 1992
c
c  STATEMENT BY AUTHOR:
c  --------------------
c
c     This general purpose global optimization program is developed at
c     the Department of Hydrology & Water Resources of the University
c     of Arizona.  Further information regarding the SCE-UA method can
c     be obtained from Dr. Q. Duan, Dr. S. Sorooshian or Dr. V.K. Gupta
c     at the address and phone number listed above.  We request all
c     users of this program make proper reference to the paper entitled
c     'Effective and Efficient Global Optimization for Conceptual
c     Rainfall-runoff Models' by Duan, Q., S. Sorooshian, and V.K. Gupta,
c     Water Resources Research, Vol 28(4), pp.1015-1031, 1992.
c
c
c  LIST OF INPUT ARGUEMENT VARIABLES
c
c     a(.) = initial parameter set
c     bl(.) = lower bound on parameters
c     bu(.) = upper bound on parameters
c     nopt = number of parameters to be optimized
c
c
c  LIST OF SCE ALGORITHMIC CONTROL PARAMETERS:
c
c     ngs = number of complexes in the initial population
c     npg = number of points in each complex
c     npt = total number of points in initial population (npt=ngs*npg)
c     nps = number of points in a sub-complex
c     nspl = number of evolution steps allowed for each complex before
c         complex shuffling
c     mings = minimum number of complexes required, if the number of
c         complexes is allowed to reduce as the optimization proceeds
c     iseed = initial random seed
c     iniflg = flag on whether to include the initial point in population
c         = 0, not included
c         = 1, included
c     iprint = flag for controlling print-out after each shuffling loop
c         = 0, print information on the best point of the population
c         = 1, print information on every point of the population
c
c
c  CONVERGENCE CHECK PARAMETERS
c
c     maxn = max no. of trials allowed before optimization is terminated
c     kstop = number of shuffling loops in which the criterion value must
c         chang by the given percentage before optimization is terminated
c     pcento = percentage by which the criterion value must change in
c         given number of shuffling loops
c     ipcnvg = flag indicating whether parameter convergence is reached
c         (i.e., check if gnrng is less than 0.001)
c         = 0, parameter convergence not satisfied
c         = 1, parameter convergence satisfied
c
c
c  LIST OF LOCAL VARIABLES
c     x(.,.) = coordinates of points in the population
c     xf(.) = function values of x(.,.)
c     xx(.) = coordinates of a single point in x
c     cx(.,.) = coordinates of points in a complex
c     cf(.) = function values of cx(.,.)
c     s(.,.) = coordinates of points in the current simplex
c     sf(.) = function values of s(.,.)
c     bestx(.) = best point at current shuffling loop
c     bestf = function value of bestx(.)
c     worstx(.) = worst point at current shuffling loop
c     worstf = function value of worstx(.)
c     xnstd(.) = standard deviation of parameters in the population
c     gnrng = normalized geometric mean of parameter ranges
c     lcs(.) = indices locating position of s(.,.) in x(.,.)
c     bound(.) = bound on ith variable being optimized
c     ngs1 = number of complexes in current population
c     ngs2 = number of complexes in last population
c     iseed1 = current random seed
c     criter(.) = vector containing the best criterion values of the last
c         10 shuffling loops
c
      implicit real*8 (a-h,o-z)
c
c  ARRAYS FROM THE INPUT DATA
      dimension a(nopt),bl(nopt),bu(nopt)
c
      character*5 xname(1000)
      character*10 acall,atemp
      integer resunit
c
!!!      write (*,*) ' ENTER THE SCEUA SUBROUTINE --- '
c
c  INITIALIZE VARIABLES

      ifail=0
      do i=1,nopt
        write(xname(i),'(i5)') i
        xname(i)=adjustl(xname(i))
        xname(i)='X'//trim(xname(i))
      end do

C -- The following are initialised so that the compiler doesn't complain
C    when they are stored in a restart file and have not been assigned a
C    value.

      nps1=0
      lpos=0
      ngs2=0
      denomi=0.0
      timeou=0.0
      s_main=0.0
      sf_main=0.0
      lcs_main=0
      cx_main=0.0
      cf_main=0.0
      nloop = 0
      npt = ngs * npg

      if(ipll.lt.0)then
        read(iunit_slave,*,err=7120,end=7120) rtemp
        igs_slave=nint(rtemp)
        read(iunit_slave,*,err=7120,end=7120) rtemp
        randcall_slave=nint(rtemp)
        read(iunit_slave,*,err=7120,end=7120) nloop
        nloop=nloop-1
        go to 1000
7120    ifail=120
        return
      end if
      if(irestart.eq.1)then
        resunit=sce_nextunit()
        open(unit=resunit,file=restartfile,form='unformatted')
        write(resunit) nloop
        close(unit=resunit)
        call write_restart_data_main(nopt,ngs,npg,nps,nspl,mings,npt)
      else if(irestart.eq.2)then
        resunit=sce_nextunit()
        itemp=8*max(20,npt,nps,nopt)
        open(unit=resunit,file=restartfile,form='unformatted',
     +  status='old',recl=itemp,iostat=ierr)
        if(ierr.ne.0)then
          ifail=110
          return
        end if
        read(resunit,iostat=ierr) nloop
        if(ierr.ne.0) then
          ifail=111
          return
        end if
        if(nloop.gt.0) then
          nloop=nloop-1
          go to 1000
        end if
        close(unit=resunit)
        irestart=1
      end if

      loop = 0
      igs = 0
      nopt1 = 8
      if (nopt.lt.8) nopt1 = nopt
      nopt2 = 12
      if (nopt.lt.12) nopt2 = nopt
c
c  INITIALIZE RANDOM SEED TO A NEGATIVE INTEGER
      iseed1 = -abs(iseed)
c
c  COMPUTE THE TOTAL NUMBER OF POINTS IN INITIAL POPUALTION
      ngs1 = ngs
      npt1 = npt
c
      if(ipll.ge.0)then
        write(outunit,400)
      end if
!!!      write (*,*) ' ***  Evolution Loop Number ',nloop
c
c  COMPUTE THE BOUND FOR PARAMETERS BEING OPTIMIZED
      do j = 1, nopt
        bound_main(j) = bu(j) - bl(j)
        unit_main(j) = 1.0
      end do
c
c  COMPUTE THE FUNCTION VALUE OF THE INITIAL POINT

      if((iniflg.eq.0).or.(ipll.gt.0))then
        call sce_writint(acall,npt1)
        write(6,8040) trim(acall)
8040    format(/,' - running model ',a,' times with random ',
     +  'parameter sets...')
      end if
c
c  GENERATE npt1 RANDOM POINTS DISTRIBUTED UNIFORMLY IN THE PARAMETER
c  SPACE, AND COMPUTE THE CORRESPONDING FUNCTION VALUES
      icall=0

      ipass_pll=0
8041  continue
      if(ipll.gt.0) ipass_pll=ipass_pll+1
      do i = 1, npt1
        if((i.eq.1).and.(iniflg.eq.1))then
          if(ipll.le.0)then
            write(6,8005)
8005        format(/,' - running model using initial parameter ',
     +      'set...')
          end if
          if((ipass_pll.eq.0).or.(ipass_pll.eq.1))then
            sxx(:,1)=a
          end if
          if(ipass_pll.eq.0)then
            call feval(ifail,sxx(:,1),sobjfn(1))
            if(ifail.ne.0) go to 9890
          end if
          if((ipass_pll.eq.0).or.(ipass_pll.eq.2))then
            fa=sobjfn(1)
c
c  PRINT THE INITIAL POINT AND ITS CRITERION VALUE
            if(ipll.ge.0)then
              write(outunit,500)
              write(outunit,510) (xname(j),j=1,nopt2)
              write(outunit,520) fa,(a(j),j=1,nopt2)
              if (nopt.lt.13) go to 101
              write(outunit,530) (xname(j),j=13,nopt)
              write(outunit,540) (a(j),j=13,nopt)
  101         continue
#ifdef FLUSHFILE
              call flush(outunit)
#endif
            end if
            if(ncontrib.gt.1)then
              call get_contribs(phi_lowest,phi_contrib,agp_contrib)
            end if
            if(ipll.ge.0)then
              if(ipll.eq.0) write(6,8006)
8006          format(' - model run completed.')
              write(6,8010)
              write(outunit1,8010)
8010          format(/,/,' INITIAL PARAMETER SET')
              call get_number_model_runs(iicall)
              call sce_writint(acall,iicall)
              if(ipll.eq.0)then
                write(6,8015) trim(acall)
                write(outunit1,8015) trim(acall)
8015            format(t5,'Number of model runs so far',t50,'=      ',a)
              end if
              write(6,8020) sobjfn(1)
              write(outunit1,8020) sobjfn(1)
8020          format(t5,'Objective function',t50,'= ',1pg13.6)
              if(ipll.eq.0)then
                if(ncontrib.gt.1)then
                  do j=1,ncontrib
                    write(6,8030) trim(agp_contrib(j)),phi_contrib(j)
                    write(outunit1,8030) trim(agp_contrib(j)),
     +              phi_contrib(j)
8030                format(t5,'Contribution from group "',a,'"',t50,'= ',
     +              1pg13.6)
                  end do
                end if
              end if

#ifdef FLUSHFILE
              call flush(outunit1)
#endif
              if(maxn.eq.0) go to 9870
              if(iicall.gt.maxn) go to 9000
            end if

            icall=icall+1
            do j = 1, nopt
              x_main(1,j) = a(j)
            end do
            xf_main(1) = fa
            call sce_writint(acall,npt1-1)
            if(ipll.eq.0)then
              write(6,8040) trim(acall)
            end if
          end if
        else
          if((ipass_pll.eq.0).or.(ipass_pll.eq.1))then
            call getpnt(nopt,1,iseed1,xx_main,bl,bu,unit_main,bl)
            do j = 1, nopt
              x_main(i,j) = xx_main(j)
            end do
          end if
          if(ipass_pll.eq.0)then
            sxx(:,1)=xx_main
            call feval(ifail,sxx(:,1),sobjfn(1))
            if(ifail.ne.0) go to 9890
            xf_main(i)=sobjfn(1)
            if(ipll.eq.0)then
              if(((iniflg.eq.0).and.(i.eq.1)).or.
     +           ((iniflg.eq.1).and.(i.eq.2)))then
                write(6,8031)
8031            format(' - number of runs completed...')
                write(6,'(a)',advance='no') '   '
              end if
              if(iniflg.eq.1)then
                write(6,8032,advance='no') i-1
              else
                write(6,8032,advance='no') i
              end if
8032          format(i6)
              if(iniflg.eq.1)then
                itemp=i-1
              else
                itemp=i
              end if
              if((i.eq.npt1).or.(mod(itemp,12).eq.0))then
                write(6,*)
                write(6,'(a)',advance='no') '   '
              end if
            end if
          else if(ipass_pll.eq.1)then
            sxx(:,i)=xx_main
          else if(ipass_pll.eq.2)then
            xf_main(i)=sobjfn(i)
          end if
          if((ipass_pll.eq.0).or.(ipass_pll.eq.2))then
            icall = icall + 1
          end if
        end if
      end do
      if(ipass_pll.eq.1)then
        call mul_feval(ifail,npt,sxx,sobjfn)
        if(ifail.lt.0)then
          ifail=-1
          go to 9890
        else if(ifail.gt.0) then
          ifail=11
          go to 9890
        end if
        go to 8041
      end if
c
c  ARRANGE THE POINTS IN ORDER OF INCREASING FUNCTION VALUE
   45 call sort(npt1,nopt,x_main,xf_main)
c
c  RECORD THE BEST AND WORST POINTS
      do j = 1, nopt
        bestx_main(j) = x_main(1,j)
        worstx_main(j) = x_main(npt1,j)
      end do
      bestf = xf_main(1)
      worstf = xf_main(npt1)
c
c  COMPUTE THE PARAMETER RANGE FOR THE INITIAL POPULATION
      call parstt(npt1,nopt,x_main,xnstd_main,bound_main,gnrng,ipcnvg)
c
c  PRINT THE RESULTS FOR THE INITIAL POPULATION
      if(ipll.ge.0)then
        write(outunit,600)
        write(outunit,610) (xname(j),j=1,nopt1)
        if (nopt .lt. 9) go to 201
        write(outunit,620) (xname(j),j=9,nopt)
  201   continue
        write(outunit,630) nloop,icall,ngs1,bestf,worstf,gnrng,
     &                 (bestx_main(j),j=1,nopt1)
        if (nopt .lt. 9) go to 301
        write(outunit,640) (bestx_main(j),j=9,nopt)
  301   continue
        if (iprint .eq. 1) then
          write(outunit,650) nloop
          do i = 1, npt1
            write(outunit,660) xf_main(i),(x_main(i,j),j=1,nopt1)
            if (nopt .lt. 9) go to 401
            write(outunit,640) (x_main(i,j),j=9,nopt)
  401     end do
        end if
#ifdef FLUSHFILE
        call flush(outunit)
#endif
      end if
c
C      if (icall .ge. maxn) go to 9000
      if (ipcnvg .eq. 1) go to 9200
c
c  BEGIN THE MAIN LOOP ----------------
 1000 continue

C -- If running as the master of a parallelised run, the main program must be
C    instructed to alter its definitions of parameters and observations.

C -- Restart data is saved or retreived.

      nloop = nloop + 1

      if(ipll.lt.0)then
        read(iunit_slave,*,err=7130,end=7130) randcall,iset,gset
        read(iunit_slave,*,err=7130,end=7130) loop,igs,nopt1,nopt2,
     +  iseed,npt,nps1,npt1,icall,lpos,ngs2,ngs1,iprint
        read(iunit_slave,*,err=7130,end=7130) gnrng,ipcnvg
        read(iunit_slave,*,err=7130,end=7130) bestf,worstf
        read(iunit_slave,*,err=7130,end=7130) denomi,timeou
        read(iunit_slave,*,err=7130,end=7130)
     +  ((x_main(i,j),i=1,npt),j=1,nopt)
        read(iunit_slave,*,err=7130,end=7130) (xx_main(i),i=1,nopt)
        read(iunit_slave,*,err=7130,end=7130) (bestx_main(i),i=1,nopt)
        read(iunit_slave,*,err=7130,end=7130) (worstx_main(i),i=1,nopt)
        read(iunit_slave,*,err=7130,end=7130) (xf_main(i),i=1,npt)
        read(iunit_slave,*,err=7130,end=7130)
     +  ((s_main(i,j),i=1,nps),j=1,nopt)
        read(iunit_slave,*,err=7130,end=7130) (sf_main(i),i=1,nps)
        read(iunit_slave,*,err=7130,end=7130) (lcs_main(i),i=1,nps)
        read(iunit_slave,*,err=7130,end=7130) (xnstd_main(i),i=1,nopt)
        read(iunit_slave,*,err=7130,end=7130) (bound_main(i),i=1,nopt)
        read(iunit_slave,*,err=7130,end=7130) (criter_main(i),i=1,20)
        read(iunit_slave,*,err=7130,end=7130) (unit_main(i),i=1,nopt)
        close(unit=iunit_slave)
        go to 7140
7130    ifail=120
        return
7140    continue
        iseed1=-abs(iseed)
        itemp=randcall_slave
        randcall=0
        do i=1,itemp
          dtemp=ran1(iseed1)
        end do
        go to 7150
      end if
      if(irestart.eq.1)then
        resunit=sce_nextunit()
        itemp=8*max(20,npt,nps,nopt)
        open(unit=resunit,file=restartfile,form='unformatted',
     +  recl=itemp)
        write(resunit) nloop
        write(resunit) randcall,iset,gset
        write(resunit) loop,igs,nopt1,nopt2,iseed,npt,nps1,npt1,icall,
     +  lpos,ngs2,ngs1,iprint
        write(resunit) gnrng,ipcnvg
        write(resunit) bestf,worstf
        write(resunit) denomi,timeou
        do j=1,nopt
          write(resunit) (x_main(i,j),i=1,npt)
        end do
        write(resunit) (xx_main(i),i=1,nopt)
        write(resunit) (bestx_main(i),i=1,nopt)
        write(resunit) (worstx_main(i),i=1,nopt)
        write(resunit) (xf_main(i),i=1,npt)
        do j=1,nopt
          write(resunit) (s_main(i,j),i=1,nps)
        end do
        write(resunit) (sf_main(i),i=1,nps)
        write(resunit) (lcs_main(i),i=1,nps)
        write(resunit) (xnstd_main(i),i=1,nopt)
        write(resunit) (bound_main(i),i=1,nopt)
        write(resunit) (criter_main(i),i=1,20)
        write(resunit) (unit_main(i),i=1,nopt)
        close(unit=resunit)
        call write_restart_data_main(nopt,ngs,npg,nps,nspl,mings,npt)
      else if(irestart.eq.2)then
        read(resunit,err=8200,end=8200) randcall,iset,gset
        read(resunit,err=8200,end=8200)
     +  loop,igs,nopt1,nopt2,iseed,npt,nps1,npt1,icall,
     +  lpos,ngs2,ngs1,iprint
        read(resunit,err=8200,end=8200) gnrng,ipcnvg
        read(resunit,err=8200,end=8200) bestf,worstf
        read(resunit,err=8200,end=8200) denomi,timeou
        do j=1,nopt
          read(resunit,err=8200,end=8200) (x_main(i,j),i=1,npt)
        end do
        read(resunit,err=8200,end=8200) (xx_main(i),i=1,nopt)
        read(resunit,err=8200,end=8200) (bestx_main(i),i=1,nopt)
        read(resunit,err=8200,end=8200) (worstx_main(i),i=1,nopt)
        read(resunit,err=8200,end=8200) (xf_main(i),i=1,npt)
        do j=1,nopt
          read(resunit,err=8200,end=8200) (s_main(i,j),i=1,nps)
        end do
        read(resunit,err=8200,end=8200) (sf_main(i),i=1,nps)
        read(resunit,err=8200,end=8200) (lcs_main(i),i=1,nps)
        read(resunit,err=8200,end=8200) (xnstd_main(i),i=1,nopt)
        read(resunit,err=8200,end=8200) (bound_main(i),i=1,nopt)
        read(resunit,err=8200,end=8200) (criter_main(i),i=1,20)
        read(resunit,err=8200,end=8200) (unit_main(i),i=1,nopt)
        close(unit=resunit)
        go to 8230
8200    ifail=111
        return
8230    continue
        irestart=1
        iseed1=-abs(iseed)
        itemp=randcall
        randcall=0
        do i=1,itemp
          dtemp=ran1(iseed1)
        end do
      end if
      if(ipll.gt.0)then
        resunit=sce_nextunit()
        open(unit=resunit,file=sce_slave_tplfile)
        write(resunit,7010)
7010    format('ptf $')
        write(resunit,7020) 2                 ! Control variable
7020    format(i5)
        write(resunit,7020) iopt
        write(resunit,7030)
7030    format('$ctl1         $')
        write(resunit,7040)
7040    format('$ctl2         $')
        write(resunit,7020) nloop
        write(resunit,*) randcall,iset,gset
        write(resunit,*) loop,igs,nopt1,nopt2,iseed,npt,nps1,
     +  npt1,icall,lpos,ngs2,ngs1,iprint
        write(resunit,7043) gnrng,ipcnvg
7043    format(1x,1pg14.7,1x,i5)
        write(resunit,*) bestf,worstf
        write(resunit,*) denomi,timeou
        write(resunit,7044) ((x_main(i,j),i=1,npt),j=1,nopt)
        write(resunit,7044) (xx_main(i),i=1,nopt)
        write(resunit,7044) (bestx_main(i),i=1,nopt)
        write(resunit,7044) (worstx_main(i),i=1,nopt)
        write(resunit,7044) (xf_main(i),i=1,npt)
        write(resunit,7044) ((s_main(i,j),i=1,nps),j=1,nopt)
        write(resunit,7044) (sf_main(i),i=1,nps)
        write(resunit,7045) (lcs_main(i),i=1,nps)
        write(resunit,7044) (xnstd_main(i),i=1,nopt)
        write(resunit,7044) (bound_main(i),i=1,nopt)
        write(resunit,7044) (criter_main(i),i=1,20)
        write(resunit,7044) (unit_main(i),i=1,nopt)
7044    format(10(1x,1pg14.7))
7045    format(20(1x,i6))
        close(unit=resunit)
        if(ialter.eq.0)then
          call alter_mio_data(npg,nopt,ngs)
          ialter=1
        end if
      end if

      call writint(acall,nloop)
      write(6,8100) trim(acall)
      write(outunit1,8100) trim(acall)
8100  format(/,/,' EVOLUTION LOOP NUMBER ',a)
      call get_number_model_runs(iicall)
      call sce_writint(acall,iicall)
      write(6,8015) trim(acall)
      write(outunit1,8015) trim(acall)
      call get_contribs(phi_lowest,phi_contrib,agp_contrib)
      write(6,8120) phi_lowest
      write(outunit1,8120) phi_lowest
8120  format(t5,'Best objective function so far',t50,'= ',1pg13.6)
      if(ncontrib.gt.1)then
        do i=1,ncontrib
          write(6,8030) trim(agp_contrib(i)),phi_contrib(i)
          write(outunit1,8030) trim(agp_contrib(i)),phi_contrib(i)
        end do
      end if

#ifdef FLUSHFILE
      call flush(outunit1)
#endif

      if(iicall.gt.maxn) go to 9000

      call sce_writint(atemp,ngs1)
      write(6,8130) trim(atemp)
8130  format(/,' - choosing and evolving ',a,' complexes...')

c
!!!      write (*,*) ' ***  Evolution Loop Number ',nloop
c
c  BEGIN LOOP ON COMPLEXES

7150  continue
      do 3000 igs = 1, ngs1
        if(ipll.gt.0)then
          ctl_pll(1,igs)=igs
          randcall=randcall+(igs-1)*nspl*3
          ctl_pll(2,igs)=randcall
          go to 3000
        else if(ipll.lt.0)then
          if(igs.ne.igs_slave) cycle
        end if
c
c  ASSIGN POINTS INTO COMPLEXES
      do k1 = 1, npg
        k2 = (k1-1) * ngs1 + igs
        do j = 1, nopt
          cx_main(k1,j) = x_main(k2,j)
        end do
        cf_main(k1) = xf_main(k2)
      end do
c
c  BEGIN INNER LOOP - RANDOM SELECTION OF SUB-COMPLEXES ---------------
      do 2000 loop = 1, nspl
c
c  CHOOSE A SUB-COMPLEX (nps points) ACCORDING TO A LINEAR
c  PROBABILITY DISTRIBUTION
      if (nps .eq. npg) then
        do k = 1, nps
          lcs_main(k) = k
        end do
        go to 85
      end if
c
      rand = ran1(iseed1)
      lcs_main(1) = 1 + dint(npg + 0.5 - dsqrt( (npg+.5)**2 -
     &         npg * (npg+1) * rand ))
      do k = 2, nps
   60   rand = ran1(iseed1)
        lpos = 1 + dint(npg + 0.5 - dsqrt((npg+.5)**2 -
     &         npg * (npg+1) * rand ))
        do k1 = 1, k-1
          if (lpos .eq. lcs_main(k1)) go to 60
        end do
        lcs_main(k) = lpos
      end do
c
c  ARRANGE THE SUB-COMPLEX IN ORDER OF INCEASING FUNCTION VALUE
      call sort1(nps,lcs_main)
c
c  CREATE THE SUB-COMPLEX ARRAYS
   85 do k = 1, nps
        do j = 1, nopt
          s_main(k,j) = cx_main(lcs_main(k),j)
        end do
        sf_main(k) = cf_main(lcs_main(k))
      end do
c
c  USE THE SUB-COMPLEX TO GENERATE NEW POINT(S)
      call cce(ifail,nopt,nps,s_main,sf_main,bl,bu,xnstd_main,icall,
     +maxn,iseed1)
      if(ifail.ne.0) go to 9890
c
c  IF THE SUB-COMPLEX IS ACCEPTED, REPLACE THE NEW SUB-COMPLEX
c  INTO THE COMPLEX
      do k = 1, nps
        do j = 1, nopt
          cx_main(lcs_main(k),j) = s_main(k,j)
        end do
        cf_main(lcs_main(k)) = sf_main(k)
      end do
c
c  SORT THE POINTS
      call sort(npg,nopt,cx_main,cf_main)
c
c  IF MAXIMUM NUMBER OF RUNS EXCEEDED, BREAK OUT OF THE LOOP
      if (icall .ge. maxn) go to 2222
c
c  END OF INNER LOOP ------------
 2000 continue
 2222 continue
c
c  REPLACE THE NEW COMPLEX INTO ORIGINAL ARRAY x(.,.)

      if(ipll.lt.0)then
        resunit=sce_nextunit()
        open(unit=resunit,file=sce_slave_outfile)
      end if

      do k1 = 1, npg
        k2 = (k1-1) * ngs1 + igs
        do j = 1, nopt
          x_main(k2,j) = cx_main(k1,j)
          if(ipll.lt.0) write(resunit,*) x_main(k2,j)
        end do
        xf_main(k2) = cf_main(k1)
        if(ipll.lt.0) write(resunit,*) xf_main(k2)
      end do

      if(ipll.eq.0)then
        if(igs.eq.1)then
          write(6,8300)
8300      format(' - number of complexes completed...')
          write(6,'(a)',advance='no') '   '
        end if
        write(6,8310,advance='no') igs
8310    format(i6)
        if((igs.eq.ngs1).or.(mod(igs,12).eq.0))then
          write(6,*)
          write(6,'(a)',advance='no') '   '
        end if
      end if

      if (icall .ge. maxn) go to 3333
c
c  END LOOP ON COMPLEXES
 3000 continue

      if(ipll.gt.0)then
        call mul_feval(ifail,ngs1,ctl_pll,sobjfn)
        if(ifail.lt.0) then
          go to 9890
        else if(ifail.gt.0)then
          ifail=11
          go to 9890
        end if
        call get_x_data(npt,nopt,ngs1,npg,x_main,
     +  xf_main,jcall)
       icall=icall+jcall
      else if(ipll.lt.0)then
        call get_contribs(phi_lowest,phi_contrib,agp_contrib)
        write(resunit,*) phi_lowest
        do i=1,ncontrib
          write(resunit,*)phi_contrib(i)
        end do
        call get_lowest_x_vals(nopt,sxx(:,1))
        do i=1,nopt
          write(resunit,*) sxx(i,1)
        end do
        call get_number_model_runs(iicall)
        write(resunit,*) float(iicall)
        close(unit=resunit)
        return
      end if

      if (icall .ge. maxn) go to 3333
c
c  RE-SORT THE POINTS
 3333 call sort(npt1,nopt,x_main,xf_main)
c
c  RECORD THE BEST AND WORST POINTS
      do j = 1, nopt
        bestx_main(j) = x_main(1,j)
        worstx_main(j) = x_main(npt1,j)
      end do
      bestf = xf_main(1)
      worstf = xf_main(npt1)
c
c  TEST THE POPULATION FOR PARAMETER CONVERGENCE
      call parstt(npt1,nopt,x_main,xnstd_main,bound_main,gnrng,ipcnvg)
c
c  PRINT THE RESULTS FOR CURRENT POPULATION
      if (mod(nloop,5) .ne. 0) go to 501
      write(outunit,610) (xname(j),j=1,nopt1)
      if (nopt .lt. 9) go to 501
      write(outunit,620) (xname(j),j=9,nopt)
  501 continue
      write(outunit,630) nloop,icall,ngs1,bestf,worstf,gnrng,
     &               (bestx_main(j),j=1,nopt1)
      if (nopt.lt.9) go to 601
      write(outunit,640) (bestx_main(j),j=9,nopt)
  601 continue
      if (iprint .eq. 1) then
        write(outunit,650) nloop
        do i = 1, npt1
          write(outunit,660) xf_main(i),(x_main(i,j),j=1,nopt1)
          if (nopt .lt. 9) go to 701
          write(outunit,640) (x_main(i,j),j=9,nopt)
  701   end do
      end if
#ifdef FLUSHFILE
      call flush(outunit)
#endif
c
c  TEST IF MAXIMUM NUMBER OF FUNCTION EVALUATIONS EXCEEDED
      if(ipll.eq.0)then
        if (icall .ge. maxn) go to 9000
      end if
c
c  COMPUTE THE COUNT ON SUCCESSIVE LOOPS W/O FUNCTION IMPROVEMENT
      criter_main(20) = bestf
      if (nloop .lt. (kstop+1)) go to 132
      denomi = dabs(criter_main(20-kstop) + criter_main(20)) / 2.
      timeou = dabs(criter_main(20-kstop) - criter_main(20)) / denomi
      if (timeou .lt. pcento) go to 9100
  132 continue
      do l = 1, 19
        criter_main(l) = criter_main(l+1)
      end do
c
c  IF POPULATION IS CONVERGED INTO A SUFFICIENTLY SMALL SPACE
      if (ipcnvg .eq. 1) go to 9200
c
c  NONE OF THE STOPPING CRITERIA IS SATISFIED, CONTINUE SEARCH
c
c  CHECK FOR COMPLEX NUMBER REDUCTION
      if (ngs1 .gt .mings) then
        ngs2 = ngs1
        ngs1 = ngs1 - 1
        npt1 = ngs1 * npg
        call comp(nopt,npt1,ngs1,ngs2,npg,x_main,xf_main,cx_main,
     +  cf_main)
      end if
c
c  END OF MAIN LOOP -----------
      go to 1000

C -- An error condition was encountered in function evaluations.


9870  continue
      stop_reason=4
      return

9890  continue
      if((ipll.lt.0).and.(ifail.lt.0))ifail=-1
      if(ifail.gt.0)then
        return
      else
        if(ifail.eq.-1)then
          return
        else
          ifail=0
          stop_reason=-2
          return
        end if
      end if
c
c  SEARCH TERMINATED
 9000 continue
      write(outunit,800) maxn,loop,igs,nloop
      stop_reason=1
      go to 9999
 9100 continue
      write(outunit,810) pcento*100.,kstop
      stop_reason=2
      go to 9999
 9200 write(outunit,820) gnrng*100.
      stop_reason=3
 9999 continue
      if(icall.lt.2) return
c
c  PRINT THE FINAL PARAMETER ESTIMATE AND ITS FUNCTION VALUE
      write(outunit,830)
      write(outunit,510) (xname(j),j=1,nopt2)
      write(outunit,520) bestf,(bestx_main(j),j=1,nopt2)
      if (nopt.lt.13) go to 801
      write(outunit,530) (xname(j),j=13,nopt)
      write(outunit,540) (bestx_main(j),j=13,nopt)
  801 continue
#ifdef FLUSHFILE
      call flush(outunit)
#endif

c
c  END OF SUBROUTINE SCEUA
      return
  400 format(//,2x,50(1h=),/,2x,'ENTER THE SHUFFLED COMPLEX EVOLUTION',
     &       ' GLOBAL SEARCH',/,2x,50(1h=))
  500 format(//,'*** PRINT THE INITIAL POINT AND ITS CRITERION ',
     &       'VALUE ***')
  510 format(/,' CRITERION',12(5x,a5),/1x,60(1h-))
  520 format(g10.3,12f10.3)
  530 format(10x,12(5x,a5))
  540 format(10x,12f10.3)
  600 format(//,1x,'*** PRINT THE RESULTS OF THE SCE SEARCH ***')
  610 format(/,1x,'LOOP',1x,'TRIALS',1x,'COMPLXS',2x,'BEST F',3x,
     &       'WORST F',3x,'PAR RNG',1x,8(5x,a5))
  620 format(49x,8(5x,a5))
  630 format(i5,1x,i5,3x,i5,3g10.3,8(f10.3))
  640 format(49x,8(f10.3))
  650 format(/,1x,'POPULATION AT LOOP ',i3,/,1x,22(1h-))
  660 format(15x,g10.3,20x,8(f10.3))
  800 format(//,1x,'*** OPTIMIZATION SEARCH TERMINATED BECAUSE THE',
     &       ' LIMIT ON THE MAXIMUM',/,5x,'NUMBER OF TRIALS ',i5,
     &       ' EXCEEDED.  SEARCH WAS STOPPED AT',/,5x,'SUB-COMPLEX ',
     &       i3,' OF COMPLEX ',i3,' IN SHUFFLING LOOP ',i3,' ***')
  810 format(//,1x,'*** OPTIMIZATION TERMINATED BECAUSE THE CRITERION',
     &       ' VALUE HAS NOT CHANGED ',/,5x,f5.2,' PERCENT IN',i3,
     &       ' SHUFFLING LOOPS ***')
  820 format(//,1x,'*** OPTIMIZATION TERMINATED BECAUSE THE POPULATION',
     &       ' HAS CONVERGED INTO ',/,4x,f5.2,' PERCENT OF THE',
     &       ' FEASIBLE SPACE ***')
  830 format(//,'*** PRINT THE FINAL PARAMETER ESTIMATE AND ITS',
     &       ' CRITERION VALUE ***')

      end subroutine sceua



c====================================================================
      subroutine cce(ifail,nopt,nps,s,sf,bl,bu,xnstd,icall,maxn,iseed)

c
c  ALGORITHM GENERATE A NEW POINT(S) FROM A SUB-COMPLEX
c
c  SUB-COMPLEX VARIABLES
      implicit real*8 (a-h,o-z)
      parameter (c1=0.8,c2=0.4)
      dimension s(:,:),sf(:),bu(:),bl(:),xnstd(:)
c
c  LIST OF LOCAL VARIABLES
c    sb(.) = the best point of the simplex
c    sw(.) = the worst point of the simplex
c    w2(.) = the second worst point of the simplex
c    fw = function value of the worst point
c    ce(.) = the centroid of the simplex excluding wo
c    snew(.) = new point generated from the simplex
c    iviol = flag indicating if constraints are violated
c          = 1 , yes
c          = 0 , no
c
c  EQUIVALENCE OF VARIABLES FOR READABILTY OF CODE

      n = nps
      m = nopt
      alpha = 1.0
      beta = 0.5
c
c  IDENTIFY THE WORST POINT wo OF THE SUB-COMPLEX s
c  COMPUTE THE CENTROID ce OF THE REMAINING POINTS
c  COMPUTE step, THE VECTOR BETWEEN wo AND ce
c  IDENTIFY THE WORST FUNCTION VALUE fw
      do j = 1, m
        sb_cce(j) = s(1,j)
        sw_cce(j) = s(n,j)
        ce_cce(j) = 0.0
        do i = 1, n-1
          ce_cce(j) = ce_cce(j) + s(i,j)
        end do
        ce_cce(j) = ce_cce(j)/dble(n-1)
      end do
      fw = sf(n)
c
c  COMPUTE THE NEW POINT snew
c
c  FIRST TRY A REFLECTION STEP
      do j = 1, m
        snew_cce(j) = ce_cce(j) + alpha * (ce_cce(j) - sw_cce(j))
      end do
c
c  CHECK IF snew SATISFIES ALL CONSTRAINTS
      call chkcst(nopt,snew_cce,bl,bu,ibound)
c
c
c  snew IS OUTSIDE THE BOUND,
c  CHOOSE A POINT AT RANDOM WITHIN FEASIBLE REGION ACCORDING TO
c  A NORMAL DISTRIBUTION WITH BEST POINT OF THE SUB-COMPLEX
c  AS MEAN AND STANDARD DEVIATION OF THE POPULATION AS STD
      if (ibound .ge. 1) call getpnt(nopt,2,iseed,snew_cce,bl,bu,
     +xnstd,sb_cce)
c
c
c  COMPUTE THE FUNCTION VALUE AT snew
      sxx(:,1)=snew_cce
      call feval(ifail,sxx(:,1),sobjfn(1))
      if(ifail.ne.0) go to 9890
      fnew=sobjfn(1)
      icall = icall + 1
c
c  COMPARE fnew WITH THE WORST FUNCTION VALUE fw
c
c  fnew IS LESS THAN fw, ACCEPT THE NEW POINT snew AND RETURN
      if (fnew .le. fw) go to 2000
      if (icall .ge. maxn) go to 3000
c
c
c  fnew IS GREATER THAN fw, SO TRY A CONTRACTION STEP
      do j = 1, m
        snew_cce(j) = ce_cce(j) - beta * (ce_cce(j) - sw_cce(j))
      end do
c
c  COMPUTE THE FUNCTION VALUE OF THE CONTRACTED POINT
      sxx(:,1)=snew_cce
      call feval(ifail,sxx(:,1),sobjfn(1))
      if(ifail.ne.0) go to 9890
      fnew=sobjfn(1)
      icall = icall + 1
c
c  COMPARE fnew TO THE WORST VALUE fw
c  IF fnew IS LESS THAN OR EQUAL TO fw, THEN ACCEPT THE POINT AND RETURN
      if (fnew .le. fw) go to 2000
      if (icall .ge. maxn) go to 3000
c
c
c  IF BOTH REFLECTION AND CONTRACTION FAIL, CHOOSE ANOTHER POINT
c  ACCORDING TO A NORMAL DISTRIBUTION WITH BEST POINT OF THE SUB-COMPLEX
c  AS MEAN AND STANDARD DEVIATION OF THE POPULATION AS STD
 1000 call getpnt(nopt,2,iseed,snew_cce,bl,bu,xnstd,sb_cce)
c
c  COMPUTE THE FUNCTION VALUE AT THE RANDOM POINT
      sxx(:,1)=snew_cce
      call feval(ifail,sxx(:,1),sobjfn(1))
      if(ifail.ne.0) go to 9890
      fnew=sobjfn(1)
      icall = icall + 1
c
c
c  REPLACE THE WORST POINT BY THE NEW POINT
 2000 continue
      do j = 1, m
        s(n,j) = snew_cce(j)
      end do
      sf(n) = fnew
 3000 continue
c
c  END OF SUBROUTINE CCE

      return

9890  continue
      return

      end subroutine cce



c===================================================================
      subroutine getpnt(nopt,idist,iseed,x,bl,bu,std,xi)

c
c     This subroutine generates a new point within feasible region
c
c     x(.) = new point
c     xi(.) = focal point
c     bl(.) = lower bound
c     bu(.) = upper bound
c     std(.) = standard deviation of probability distribution
c     idist = probability flag
c           = 1 - uniform distribution
c           = 2 - Gaussian distribution
c
      implicit real*8 (a-h,o-z)
      dimension x(:),bl(:),bu(:),std(:),xi(:)
c
    1 do j=1, nopt
    2   if (idist .eq. 1) rand = ran1(iseed)
        if (idist .eq. 2) rand = gasdev(iseed)
        x(j) = xi(j) + std(j) * rand * (bu(j) - bl(j))
c
c     Check explicit constraints
c
        call chkcst(1,x(j),bl(j),bu(j),ibound)
        if (ibound .ge. 1) go to 2
      end do
c
c     Check implicit constraints
c
      call chkcst(nopt,x,bl,bu,ibound)
      if (ibound .ge. 1) go to 1
c
      return
      end subroutine getpnt



c===================================================================
      subroutine parstt(npt,nopt,x,xnstd,bound,gnrng,ipcnvg)

c
c  SUBROUTINE CHECKING FOR PARAMETER CONVERGENCE
      implicit real*8 (a-h,o-z)
      dimension x(:,:),xnstd(:),bound(:)

cjd      parameter (delta = 1.0d-20,peps=1.0d-3)
      parameter (delta = 1.0d-20,peps=1.0d-5)
c
c  COMPUTE MAXIMUM, MINIMUM AND STANDARD DEVIATION OF PARAMETER VALUES
      gsum = 0.d0
      do k = 1, nopt
        xmax_parstt(k) = -1.0d+20
        xmin_parstt(k) = 1.0d+20
        xsum1 = 0.d0
        xsum2 = 0.d0
        do i = 1, npt
          xmax_parstt(k) = dmax1(x(i,k), xmax_parstt(k))
          xmin_parstt(k) = dmin1(x(i,k), xmin_parstt(k))
          xsum1 = xsum1 + x(i,k)
          xsum2 = xsum2 + x(i,k)*x(i,k)
        end do
        xmean_parstt(k) = xsum1 / dble(npt)
        xnstd(k) = (xsum2 / dble(npt) - xmean_parstt(k)*
     +  xmean_parstt(k))
        if (xnstd(k) .le. delta) xnstd(k) = delta
        xnstd(k) = dsqrt(xnstd(k))
        xnstd(k) = xnstd(k) / bound(k)
        gsum = gsum + dlog( delta + (xmax_parstt(k)-xmin_parstt(k))/
     +  bound(k) )
      end do
      gnrng = dexp(gsum/dble(nopt))
c
c  CHECK IF NORMALIZED STANDARD DEVIATION OF PARAMETER IS <= eps
      ipcnvg = 0
      if (gnrng .le. peps) then
        ipcnvg = 1
      end if
c
c  END OF SUBROUTINE PARSTT
      return

      end subroutine parstt


c====================================================================
      subroutine comp(n,npt,ngs1,ngs2,npg,a,af,b,bf)

c
c
c  THIS SUBROUTINE REDUCE INPUT MATRIX a(n,ngs2*npg) TO MATRIX
c  b(n,ngs1*npg) AND VECTOR af(ngs2*npg) TO VECTOR bf(ngs1*npg)
      implicit real*8 (a-h,o-z)
      dimension a(:,:),af(:),b(:,:),bf(:)
      do igs=1, ngs1
        do ipg=1, npg
          k1=(ipg-1)*ngs2 + igs
          k2=(ipg-1)*ngs1 + igs
          do i=1, n
            b(k2,i) = a(k1,i)
          end do
          bf(k2) = af(k1)
        end do
      end do
c
      do j=1, npt
        do i=1, n
          a(j,i) = b(j,i)
        end do
        af(j) = bf(j)
      end do
c
c  END OF SUBROUTINE COMP
      return
      end subroutine comp



c===================================================================
      subroutine sort(n,m,rb,ra)

c
c
c  SORTING SUBROUTINE ADAPTED FROM "NUMERICAL RECIPES"
c  BY W.H. PRESS ET AL., pp. 233-234
c
c  LIST OF VARIABLES
c     ra(.) = array to be sorted
c     rb(.,.) = arrays ordered corresponding to rearrangement of ra(.)
c     wk(.,.), iwk(.) = local varibles
c
      implicit real*8 (a-h,o-z)
      dimension ra(:),rb(:,:)
c
      call indexx(n, ra, iwk_sort)
      do 11 i = 1, n
      wk_sort(i,1) = ra(i)
   11 continue
      do 12 i = 1, n
      ra(i) = wk_sort(iwk_sort(i),1)
   12 continue
      do 14 j = 1, m
      do 13 i = 1, n
      wk_sort(i,j) = rb(i,j)
   13 continue
   14 continue
      do 16 j = 1, m
      do 15 i = 1, n
      rb(i,j) = wk_sort(iwk_sort(i),j)
   15 continue
   16 continue
c
c  END OF SUBROUTINE SORT
      return
      end subroutine sort



c===========================================================
      subroutine sort1(n,ra)

c
c
c  SORTING SUBROUTINE ADAPTED FROM "NUMERICAL RECIPES"
c  BY W.H. PRESS ET AL., pp. 231
c
c  LIST OF VARIABLES
c     ra(.) = integer array to be sorted
c
      implicit real*8 (a-h,o-z)
      dimension ra(n)
c
      integer ra, rra
c
      l = (n / 2) + 1
      ir = n
   10 continue
      if (l .gt. 1) then
      l = l - 1
      rra = ra(l)
      else
      rra = ra(ir)
      ra(ir) = ra(1)
      ir = ir - 1
      if (ir .eq. 1) then
      ra(1) = rra
      return
      end if
      end if
      i = l
      j = l + l
   20 if (j .le. ir) then
      if (j .lt. ir) then
      if (ra(j) .lt. ra(j + 1)) j = j + 1
      end if
      if (rra .lt. ra(j)) then
      ra(i) = ra(j)
      i = j
      j = j + j
      else
      j = ir + 1
      end if
      goto 20
      end if
      ra(i) = rra
      goto 10
c
c  END OF SUBROUTINE SORT1

      end subroutine sort1



c=======================================================
      subroutine indexx(n, arrin, indx)

c
c
c  THIS SUBROUTINE IS FROM "NUMERICAL RECIPES" BY PRESS ET AL.
      implicit real*8 (a-h,o-z)
      dimension arrin(n), indx(n)
c
      do 11 j = 1, n
      indx(j) = j
   11 continue
      l = (n / 2) + 1
      ir = n
   10 continue
      if (l .gt. 1) then
      l = l - 1
      indxt = indx(l)
      q = arrin(indxt)
      else
      indxt = indx(ir)
      q = arrin(indxt)
      indx(ir) = indx(1)
      ir = ir - 1
      if (ir .eq. 1) then
      indx(1) = indxt
      return
      end if
      end if
      i = l
      j = l + l
   20 if (j .le. ir) then
      if (j .lt. ir) then
      if (arrin(indx(j)) .lt. arrin(indx(j + 1))) j = j + 1
      end if
      if (q .lt. arrin(indx(j))) then
      indx(i) = indx(j)
      i = j
      j = j + j
      else
      j = ir + 1
      end if
      goto 20
      end if
      indx(i) = indxt
      goto 10
c
c  END OF SUBROUTINE INDEXX

      end subroutine indexx



c==============================================================
      real*8 function ran1(idum)

c
c
c  THIS SUBROUTINE IS FROM "NUMERICAL RECIPES" BY PRESS ET AL.
      implicit real*8 (a-h,o-z)
      dimension r(97)
      parameter (m1 = 259200, ia1 = 7141, ic1 = 54773, rm1 =
     &3.8580247e-6)
      parameter (m2 = 134456, ia2 = 8121, ic2 = 28411, rm2 =
     &7.4373773e-6)
      parameter (m3 = 243000, ia3 = 4561, ic3 = 51349)
      save
      data iff / 0 /

      randcall=randcall+1

      if ((idum .lt. 0) .or. (iff .eq. 0)) then
      iff = 1
      ix1 = mod(ic1 - idum,m1)
      ix1 = mod((ia1 * ix1) + ic1,m1)
      ix2 = mod(ix1,m2)
      ix1 = mod((ia1 * ix1) + ic1,m1)
      ix3 = mod(ix1,m3)
      do 11 j = 1, 97
      ix1 = mod((ia1 * ix1) + ic1,m1)
      ix2 = mod((ia2 * ix2) + ic2,m2)
      r(j) = (dble(ix1) + (dble(ix2) * rm2)) * rm1
   11 continue
      idum = 1
      end if
      ix1 = mod((ia1 * ix1) + ic1,m1)
      ix2 = mod((ia2 * ix2) + ic2,m2)
      ix3 = mod((ia3 * ix3) + ic3,m3)
      j = 1 + ((97 * ix3) / m3)
C      if ((j .gt. 97) .or. (j .lt. 1)) pause      ! Defunct function; so replaced by following two lines.
      if(j.gt.97)j=97
      if(j.lt.1)j=1
      ran1 = r(j)
      r(j) = (dble(ix1) + (dble(ix2) * rm2)) * rm1
c
c  END OF SUBROUTINE RAN1
      return

      end function ran1



c===============================================================
      real*8 function gasdev(idum)

c
c
c  THIS SUBROUTINE IS FROM "NUMERICAL RECIPES" BY PRESS ET AL.
      implicit real*8 (a-h,o-z)
cjd      common /gasblk/ iset
cjd      data iset / 0 /
      if (iset .eq. 0) then
    1 v1 = (2. * ran1(idum)) - 1.
      v2 = (2. * ran1(idum)) - 1.
      r = (v1 ** 2) + (v2 ** 2)
      if (r .ge. 1.) goto 1
      fac = sqrt(- ((2. * log(r)) / r))
      gset = v1 * fac
      gasdev = v2 * fac
      iset = 1
      else
      gasdev = gset
      iset = 0
      end if
c
c  END OF SUBROUTINE GASDEV
      return

      end function gasdev


        subroutine sce_writint(atemp,ival)

! -- Subroutine SCE_WRITINT writes an integer to a character variable.

        integer*4 ival
        character*6 afmt
        character*(*) atemp

        afmt='(i   )'
        write(afmt(3:5),'(i3)') len(atemp)
        write(atemp,afmt)ival
        atemp=adjustl(atemp)
        return

        end subroutine sce_writint



       integer function sce_nextunit()

C -- Function sce_nextunit determines the lowest unit number available for
C -- opening.

       logical::lopen

       do sce_nextunit=10,100
         inquire(unit=sce_nextunit,opened=lopen)
         if(.not.lopen) return
       end do
       write(6,10)
10     format(' *** No more unit numbers to open files ***')
       stop

      end function sce_nextunit




      end module sceua_work



C -------------------------------------------------------------------------------------
C !!!!!!!!!! MODULE SCEUA_MODULE !!!!!!!!!!
C -------------------------------------------------------------------------------------


      module sceua_module


      private

C -- VARIABLES

C -------------------------------------------------------------------------------------

         integer                       :: nopt      ! Number of parameters to be optimised
         integer                       :: ngs       ! Number of complexes in the initial population
         integer                       :: npg       ! Number of points in each complex
         integer                       :: nps       ! Number of points in a sub-complex
         integer                       :: nspl      ! Number of evolution steps allowed for each complex before
                                                    !   complex shuffling
         integer                       :: mings     ! Minimum number of complexes required, if the number of
                                                    !   complexes is allowed to reduce as the optimization proceeds
         integer                       :: iseed     ! Initial random seed
         integer                       :: iniflg    ! Flag on whether to include the initial point in population
                                                    !    = 0, not included
                                                    !    = 1, included
         integer                       :: iprint    ! Flag for controlling print-out after each shuffling loop
                                                    !    = 0, print information on the best point of the population
                                                    !    = 1, print information on every point of the population
         integer                       :: maxn      ! Max no. of trials allowed before optimization is terminated
         integer                       :: kstop     ! Number of shuffling loops in which the criterion value must
                                                    !    change by the given percentage before optimization is terminated
         double precision              :: pcento    ! Percentage by which the criterion value must change in
                                                    !    given number of shuffling loops

         double precision, allocatable :: a(:)      ! Initial parameter set
         double precision, allocatable :: bl(:)     ! Lower bound on parameters
         double precision, allocatable :: bu(:)     ! Upper bound on parameters

         integer                       :: ipll=0
         integer                       :: iopt=0
         integer                       :: outunit1,outunit2
         integer                       :: ncontrib=1
         integer                       :: stop_reason
         integer                       :: irestart=0
         integer                       :: iunit_slave
         character*200                 :: restartfile=' '
         character*200                 :: sce_slave_tplfile=' '
         character*200                 :: sce_slave_outfile=' '

         character*500                 :: amessage

         integer                       :: icount_sceua_init
         integer                       :: icount_set_problem_definition
         integer                       :: icount_set_integer_options
         integer                       :: icount_get_integer_options
         integer                       :: icount_set_real_options
         integer                       :: icount_get_real_options
         integer                       :: icount_set_character_options
         integer                       :: icount_get_character_options
         integer                       :: icount_sceua_run
         integer                       :: icount_sceua_fin

        public
     +     sceua_init,
     +     sceua_run,
     +     sceua_set_problem_definition,
     +     sceua_set_integer_options,
     +     sceua_get_integer_options,
     +     sceua_set_real_options,
     +     sceua_get_real_options,
     +     sceua_set_character_options,
     +     sceua_get_character_options,
     +     sceua_fin,
     +     sceua_get_message_string,
     +     sceua_get_stop_info

        contains

C -- SUBROUTINE SCEUA_INIT ---------------------------------------------------

        subroutine sceua_init(ifail,nopt_in,outunit1_in,outunit2_in)

        implicit none

        integer, intent(out)  :: ifail
        integer, intent(in)   :: nopt_in
        integer, intent(in)   :: outunit1_in
        integer, intent(in)   :: outunit2_in

        integer               :: ierr

! -- Initialisation

        ifail=0
        if(icount_sceua_init.ne.0)then
          write(amessage,5)
5         format('Error: subroutine SCEUA_INIT must not be called ',
     +    'more than once.')
          go to 9890
        end if
        icount_sceua_init=icount_sceua_init+1

! -- A check is made for illegal input.

        if(nopt_in.le.0)then
          write(amessage,10)
10        format('Error in subroutine SCEUA_INIT. ',
     +    'Problem dimensions must be a positive number.')
          go to 9890
        end if
        nopt=nopt_in
        if(outunit1_in.le.0)then
          write(amessage,11)
11        format('Error in subroutine SCEUA_INIT. ',
     +    'Zero or negative unit number supplied.')
          go to 9890
        end if
        outunit1=outunit1_in
        if(outunit2_in.le.0)then
          write(amessage,11)
          go to 9890
        end if
        outunit2=outunit2_in

! -- Arrays are allocated.

        allocate(a(nopt),bl(nopt),bu(nopt),stat=ierr)
        if(ierr.ne.0) go to 9200

        return

9200    write(amessage,9210)
9210    format('Cannot allocate sufficient memory to continue ',
     +  'execution.')
        go to 9890

9890    ifail=1
        return

        end subroutine sceua_init


C -- SUBROUTINE SCEUA_SET_PROBLEM_DEFINITION ---------------------------------------------------

       subroutine sceua_set_problem_definition(ifail,xstart,lbounds,
     + ubounds)

! -- Subroutine SCEUA_SET_PROBLEM_DEFINITION receives initial parameter values and bounds.

       implicit none

       integer, intent(out)           :: ifail
       real, intent(in)               :: xstart(:)
       real, intent(in)               :: lbounds(:)
       real, intent(in)               :: ubounds(:)

       integer        :: i,itemp
       character*15   :: atemp

       ifail=0
       if(icount_sceua_init.eq.0)then
          write(amessage,5)
5         format('Subroutine SCEUA_SET_PROBLEM_DEFINITION must not ',
     +    'be called before subroutine SCEUA_INIT.')
          go to 9890
       end if
       if(icount_set_problem_definition.ne.0)then
         write(amessage,6)
6        format('Subroutine SCEUA_SET_PROBLEM_DEFINITION must not be ',
     +   'called more than once.')
         go to 9890
       end if
       icount_set_problem_definition=icount_set_problem_definition+1

       itemp=size(xstart)
       if(itemp.lt.nopt)then
         call sceua_writint(atemp,nopt)
         write(amessage,10) trim(atemp)
10       format('Error in subroutine SCEUA_SET_PROBLEM_DEFINITION: ',
     +   'dimension of XSTART must be at least ',a,'.')
         go to 9890
       end if
       itemp=size(lbounds)
       if(itemp.lt.nopt)then
         call sceua_writint(atemp,nopt)
         write(amessage,16) trim(atemp)
16       format('Error in subroutine SCEUA_SET_PROBLEM_DEFINITION: ',
     +   'dimension of LBOUNDS must be at least ',a,'.')
         go to 9890
       end if
       itemp=size(ubounds)
       if(itemp.lt.nopt)then
         call sceua_writint(atemp,nopt)
         write(amessage,17) trim(atemp)
17       format('Error in subroutine SCEUA_SET_PROBLEM_DEFINITION: ',
     +   'dimension of UBOUNDS must be at least ',a,'.')
         go to 9890
       end if

       do i=1,nopt
         a(i)=xstart(i)
       end do
       do i=1,nopt
         bl(i)=lbounds(i)
       end do
       do i=1,nopt
         bu(i)=ubounds(i)
       end do

       call set_defaults_internal()

       return

9890   continue
       ifail=1
       return

       end subroutine sceua_set_problem_definition



C -- SUBROUTINE GET_STOP_INFO -----------------------------------------------------

       subroutine sceua_get_stop_info(istop)

       implicit none
       integer, intent(out) :: istop

       istop=stop_reason
       return
       end subroutine sceua_get_stop_info



C -- SUBROUTINE SET_DEFAULTS_INTERNAL ---------------------------------------------

       subroutine set_defaults_internal()

       implicit none

       ngs = 5
       mings = ngs
       npg = 2*nopt + 1
       nps = nopt + 1
       nspl = npg
       iniflg = 0
       iprint = 0
       maxn=50000
       kstop=5
       pcento=0.01
       iseed=555

       return
       end subroutine set_defaults_internal


C -- SUBROUTINE CHECK_INPUT_DATA ---------------------------------------------------

       subroutine check_input_data(ifail)

C    Subroutine CHECK_INPUT_DATA checks the input data. It also starts writing to the SCE output file.

       implicit real*8 (a-h,o-z)

       integer, intent(out)   :: ifail

       character*10 pcntrl,deflt,usrsp
       character*4 reduc,initl,ysflg,noflg
       character*5 xname(1000)
       character*10 atemp

       data deflt/' DEFAULT  '/
       data usrsp/'USER SPEC.'/
       data ysflg/'YES '/
       data noflg/'NO  '/

       pcntrl='  na'
       do i=1,1000
         write(xname(i),'(i5)') i
         xname(i)=adjustl(xname(i))
         xname(i)='X'//trim(xname(i))
       end do
       if(irestart.eq.2)then
         open(unit=outunit2,file='sceout.dat',action='write',
     +   position='append')
         write(outunit2,131)
131      format(/,/,t35,'RESTART',/)
       else
         open(unit=outunit2,file='sceout.dat',action='write',
     +   status='unknown')
       end if

       ifail=0
       ierror = 0
       iwarn = 0
       if(ipll.ge.0)then
         if(irestart.ne.2) write(outunit2,700)
  700    format(10x,'SHUFFLED COMPLEX EVOLUTION GLOBAL OPTIMIZATION',
     &         /,10x,46(1h=))
       end if


C -- The following tests were in the original SCE code but have been commented out
C    either because they are not relevant or because they are checked on input.

C       if (ngs .lt. 1 .or. ngs .ge. 1320) then
C         call sceua_writint(atemp,ngs)
C         write(amessage,900) trim(atemp)
C900      format('Number of comlexes in initial population ',a,
C     +   ' is not a valid choice.')
C         go to 9890
C       end if

C       if (kstop .lt. 0 .or. kstop .ge. 20) then
C         if(irestart.ne.2) write(outunit2,901) kstop
C  901    format(//,1x,'**WARNING** THE NUMBER OF SHUFFLING LOOPS IN',
C     *   ' WHICH THE CRITERION VALUE MUST CHANGE ',/,13x,'SHOULD BE',
C     *   ' GREATER THAN 0 AND LESS THAN 10.  ','kstop = ',i2,
C     *   ' WAS SPECIFIED.'/,13x,'BUT kstop = 5 WILL BE USED INSTEAD.')
C         iwarn = iwarn + 1
C         kstop=5
C       end if

C       if (mings .lt. 1 .or. mings .gt. ngs) then
C         if(irestart.ne.2) write(outunit2,902) mings
C  902    format(//,1x,'**WARNING** THE MINIMUM NUMBER OF COMPLEXES ',
C     *          i2,' IS NOT A VALID CHOICE. SET IT TO DEFAULT')
C         iwarn = iwarn + 1
C         mings = ngs
C       end if

C       if (npg .lt. 2 .or. npg .gt. 1320/max(ngs,1)) then
C         if(irestart.ne.2) write(outunit2,903) npg
C  903    format(//,1x,'**WARNING** THE NUMBER OF POINTS IN A COMPLEX ',
C     *          I4,' IS NOT A VALID CHOICE, SET IT TO DEFAULT')
C         iwarn = iwarn + 1
C         npg = 2*nopt+1
C       end if

C       if (nps.lt.2 .or. nps.gt.npg ) then
Ccjd       if (nps.lt.2 .or. nps.gt.npg .or. nps.gt.50) then
C         if(irestart.ne.2) write(outunit2,904) nps
C  904    format(//,1x,'**WARNING** THE NUMBER OF POINTS IN A SUB-',
C     *   'COMPLEX ',i4,' IS NOT A VALID CHOICE, SET IT TO DEFAULT')
C         iwarn = iwarn + 1
C         nps = nopt + 1
C       end if

C       if (nspl .lt. 1) then
C         if(irestart.ne.2) write(outunit2,905) nspl
C  905    format(//,1x,'**WARNING** THE NUMBER OF EVOLUTION STEPS ',
C     *          'TAKEN IN EACH COMPLEX BEFORE SHUFFLING ',I4,/,13x,
C     *          'IS NOT A VALID CHOICE, SET IT TO DEFAULT')
C         iwarn = iwarn + 1
C         nspl = npg
C       end if

c  COMPUTE THE TOTAL NUMBER OF POINTS IN INITIAL POPULATION
       npt = ngs * npg

C       if (npt .gt. 1320) then
C         if(irestart.ne.2) write(outunit2,906) npt
C  906    format(//,1x,'**WARNING** THE NUMBER OF POINTS IN INITIAL ',
C     *          'POPULATION ',i5,' EXCEED THE POPULATION LIMIT,',/,13x,
C     *          'SET NGS TO 2, AND NPG, NPS AND NSPL TO DEFAULTS')
C         iwarn = iwarn + 1
C         ngs = 2
C         npg = 2*nopt + 1
C         nps = nopt + 1
C         nspl = npg
C       end if

c  PRINT OUT THE TOTAL NUMBER OF ERROR AND WARNING MESSAGES

       if (ierror .ge. 1) then
         if(irestart.ne.2) write(outunit2,907) ierror
  907    format(//,1x,'*** TOTAL NUMBER OF ERROR MESSAGES IS ',i2)
       end if
       if (iwarn .ge. 1) then
         if(irestart.ne.2) write(outunit2,908) iwarn
  908    format(//,1x,'*** TOTAL NUMBER OF WARNING MESSAGES IS ',i2)
       end if
       if (mings .lt. ngs) then
         reduc = ysflg
       else
         reduc = noflg
       end if
c
       if (iniflg .ne. 0) then
         initl = ysflg
       else
         initl = noflg
       end if

c  PRINT SHUFFLED COMPLEX EVOLUTION OPTIMIZATION OPTIONS

      if(irestart.ne.2)then
      if(ipll.ge.0)then
  104 write(outunit2,910)
  910 format(//,2x,'SCE CONTROL',5x,'MAX TRIALS',5x,
     &'REQUIRED IMPROVEMENT',5x,'RANDOM',/,3x,'PARAMETER',8x,
     &'ALLOWED',6x,'PERCENT',4x,'NO. LOOPS',6x,'SEED',/,
     &2x,11(1h-),5x,10(1H-),5x,7(1h-),4x,9(1h-),5x,6(1h-))

      pcenta=pcento*100.
      write(outunit2,912) pcntrl,maxn,pcenta,kstop,iseed
  912 format(3x,a10,7x,i5,10x,f3.1,9x,i2,9x,i5)
      write(outunit2,914) ngs,npg,npt,nps,nspl
  914 format(//,18x,'SCE ALGORITHM CONTROL PARAMETERS',/,18x,32(1H=),
     &//,2x,'NUMBER OF',5x,'POINTS PER',5x,'POINTS IN',6x,'POINTS PER',
     &4x,'EVOL. STEPS',/,2x,'COMPLEXES',6X,'COMPLEX',6x,'INI. POPUL.',
     &5x,'SUB-COMPLX',4x,'PER COMPLEX',/,2x,9(1h-),5x,10(1h-),4x,
     &11(1h-),5x,10(1h-),4x,11(1h-),5x,/,2x,5(i5,10x))
      write(outunit2,915) reduc,mings,initl
  915 format(//,15x,'COMPLX NO.',5x,'MIN COMPLEX',5x,'INI. POINT',/,
     &15x,'REDUCTION',6x,'NO. ALLOWED',6x,'INCLUDED',/,
     &15x,10(1h-),5x,11(1h-),5x,10(1h-),/,17x,a5,6x,i8,12x,a5)
      write(outunit2,916)
  916 format(//,8x,'INITIAL PARAMETER VALUES AND PARAMETER BOUNDS',/,
     &       8x,45(1h=),//,2x,'PARAMETER',5x,'INITIAL VALUE',5x,
     &       'LOWER BOUND',5x,'UPPER BOUND',/,2x,9(1h-),5x,13(1h-),5x,
     &       11(1h-),5x,11(1h-))
      do 920 i = 1, nopt
        write(outunit2,918) xname(i),a(i),bl(i),bu(i)
  918   format(3x,a5,4x,3(6x,f10.3))
  920 continue

#ifdef FLUSHFILE
       call flush(outunit2)
#endif
      end if
      end if

      return

9890  continue
      ifail=1
      return

      end subroutine check_input_data



C -- SUBROUTINE SCEUA_SET_INTEGER_OPTIONS -------------------------------------------------

       subroutine sceua_set_integer_options(ifail,option,ivar)

! -- Subroutine SCEUA_SET_INTEGER_OPTIONS allows a calling program to set integer options.

       implicit none

       integer, intent(out)           :: ifail
       character (len=*), intent(in)  :: option
       integer, intent(in)            :: ivar

       character (len=50)             :: aoption

       ifail=0
       if(icount_set_problem_definition.eq.0)then
         write(amessage,5)
5        format('Subroutine SCEUA_SET_INTEGER_OPTIONS must only be ',
     +   'called after subroutine SCEUA_SET_PROBLEM_DEFINITION has ',
     +   'been called.')
         ifail=1
         return
       end if
       icount_set_integer_options=icount_set_integer_options+1
       aoption=option
       call sceua_lowcase(aoption)

       select case(aoption)
         case('maxn')
           maxn=ivar
         case('kstop')
           kstop=ivar
         case('iseed')
           iseed=ivar
         case('ngs')
           ngs=ivar
         case('npg')
           npg=ivar
         case('nps')
           nps=ivar
         case('nspl')
           nspl=ivar
         case('mings')
           mings=ivar
         case('iniflg')
           iniflg=ivar
         case('iprint')
           iprint=ivar
         case('ncontrib')
           ncontrib=ivar
         case('irestart')
           irestart=ivar
         case('ipll')
           ipll=ivar
         case('iopt')
           iopt=ivar
         case('iunit_slave')
           iunit_slave=ivar
         case default
           write(amessage,10) trim(option)
10         format('Unknown or inappropriate variable name "',a,
     +     '" in call to subroutine SCEUA_SET_INTEGER_OPTIONS.')
           ifail=1
           return
       end select

       return

       end subroutine sceua_set_integer_options


C -- SUBROUTINE SCEUA_GET_INTEGER_OPTIONS -------------------------------------------------

       subroutine sceua_get_integer_options(ifail,option,ivar)

! -- Subroutine SCEUA_GET_INTEGER_OPTIONS allows a calling program to retreive integer options.

       implicit none

       integer, intent(out)           :: ifail
       character (len=*), intent(in)  :: option
       integer, intent(out)           :: ivar

       character (len=50)             :: aoption

       ifail=0
       if(icount_set_problem_definition.eq.0)then
         write(amessage,5)
5        format('Subroutine SCEUA_GET_INTEGER_OPTIONS must only be ',
     +   'called after subroutine SCEUA_SET_PROBLEM_DEFINITION has ',
     +   'been called.')
         ifail=1
         return
       end if
       icount_get_integer_options=icount_get_integer_options+1
       aoption=option
       call sceua_lowcase(aoption)

       select case(aoption)
         case('maxn')
           ivar=maxn
         case('kstop')
           ivar=kstop
         case('iseed')
           ivar=iseed
         case('ngs')
           ivar=ngs
         case('npg')
           ivar=npg
         case('nps')
           ivar=nps
         case('nspl')
           ivar=nspl
         case('mings')
           ivar=mings
         case('iniflg')
           ivar=iniflg
         case('iprint')
           ivar=iprint
         case('ncontrib')
           ivar=ncontrib
         case('irestart')
           ivar=irestart
         case('ipll')
           ivar=ipll
         case('iopt')
           ivar=iopt
         case('iunit_slave')
           ivar=iunit_slave
         case default
           write(amessage,10) trim(option)
10         format('Unknown or inappropriate variable name "',a,
     +     '" in call to subroutine SCEUA_GET_INTEGER_OPTIONS.')
           ifail=1
           return
       end select

       return

       end subroutine sceua_get_integer_options


C -- SUBROUTINE SCEUA_GET_REAL_OPTIONS -------------------------------------------------

       subroutine sceua_set_real_options(ifail,option,rvar)

! -- Subroutine SCEUA_SET_REAL_OPTIONS allows a calling program to set real options.

       implicit none

       integer, intent(out)           :: ifail
       character (len=*), intent(in)  :: option
       real, intent(in)               :: rvar

       character (len=50)             :: aoption

       ifail=0
       if(icount_set_problem_definition.eq.0)then
         write(amessage,5)
5        format('Subroutine SCEUA_SET_REAL_OPTIONS must only be ',
     +   'called after subroutine SCEUA_SET_PROBLEM_DEFINITION has ',
     +   'been called.')
         ifail=1
         return
       end if
       icount_set_real_options=icount_set_real_options+1
       aoption=option
       call sceua_lowcase(aoption)

       select case(aoption)
         case('pcento')
           pcento=rvar
         case default
           write(amessage,10) trim(option)
10         format('Unknown or inappropriate variable name "',a,
     +     '" in call to subroutine SCEUA_SET_REAL_OPTIONS.')
           ifail=1
           return
       end select

       return

       end subroutine sceua_set_real_options


C -- SUBROUTINE SCEUA_GET_REAL_OPTIONS -------------------------------------------------

       subroutine sceua_get_real_options(ifail,option,rvar)

! -- Subroutine SCEUA_GET_REAL_OPTIONS allows a calling program to retreive real options.

       implicit none

       integer, intent(out)           :: ifail
       character (len=*), intent(in)  :: option
       real, intent(out)              :: rvar

       character (len=50)             :: aoption

       ifail=0
       if(icount_set_problem_definition.eq.0)then
         write(amessage,5)
5        format('Subroutine SCEUA_GET_REAL_OPTIONS must only be ',
     +   'called after subroutine SCEUA_SET_PROBLEM_DEFINITION has ',
     +   'been called.')
         ifail=1
         return
       end if
       icount_get_real_options=icount_get_real_options+1
       aoption=option
       call sceua_lowcase(aoption)

       select case(aoption)
         case('pcento')
           rvar=pcento
         case default
           write(amessage,10) trim(option)
10         format('Unknown or inappropriate variable name "',a,
     +     '" in call to subroutine SCEUA_GET_REAL_OPTIONS.')
           ifail=1
           return
       end select

       return

       end subroutine sceua_get_real_options



C -- SUBROUTINE SCEUA_SET_CHARACTER_OPTIONS ---------------------------------------------

       subroutine sceua_set_character_options(ifail,option,bvar)

! -- Subroutine SCEUA_SET_CHARACTER_OPTIONS allows a calling program to set character options.

       implicit none

       integer, intent(out)           :: ifail
       character (len=*), intent(in)  :: option
       character (len=*), intent(in)  :: bvar

       character (len=50)             :: aoption
       character (len=200)            :: avar

       ifail=0
       if(icount_set_problem_definition.eq.0)then
         write(amessage,5)
5        format('Subroutine SCEUA_SET_CHARACTER_OPTIONS must only ',
     +   'be called after subroutine SCEUA_SET_PROBLEM_DEFINITION ',
     +   'has been called.')
         ifail=1
         return
       end if
       icount_set_character_options=icount_set_character_options+1

       aoption=option
       call sceua_lowcase(aoption)
       avar=bvar
       call sceua_lowcase(avar)
       avar=adjustl(avar)

       select case(aoption)
         case('restartfile')
           restartfile=avar
         case('sce_slave_tplfile')
           sce_slave_tplfile=avar
         case('sce_slave_outfile')
           sce_slave_outfile=avar
         case default
           write(amessage,10) trim(option)
10         format('Unknown or inappropriate variable name "',a,
     +     '" in call to subroutine SCEUA_SET_CHARACTER_OPTIONS.')
           ifail=1
           return
       end select

       return

       end subroutine sceua_set_character_options


C -- SUBROUTINE SCEUA_GET_CHARACTER_OPTIONS ---------------------------------------------


       subroutine sceua_get_character_options(ifail,option,avar)

C -- Subroutine SCEUA_GET_CHARACTER_OPTIONS allows a calling program to retreive character options.

       implicit none

       integer, intent(out)           :: ifail
       character (len=*), intent(in)  :: option
       character (len=*), intent(out) :: avar

       character (len=50)             :: aoption

       ifail=0
       if(icount_set_problem_definition.eq.0)then
         write(amessage,5)
5        format('Subroutine SCEUA_GET_CHARACTER_OPTIONS must only be ',
     +   'called after subroutine SCEUA_SET_PROBLEM_DEFINITION has ',
     +   'been called.')
         ifail=1
         return
       end if
       icount_get_character_options=icount_get_character_options+1

       aoption=option
       call sceua_lowcase(aoption)

       select case(aoption)
         case('restartfile')
           avar=restartfile
         case('sce_slave_tplfile')
           avar=sce_slave_tplfile
         case('sce_slave_outfile')
           avar=sce_slave_outfile
         case default
           write(amessage,10) trim(option)
10         format('Unknown or inappropriate variable name "',a,
     +     '" in call to subroutine SCEUA_GET_CHARACTER_OPTIONS.')
           ifail=1
           return
       end select

       return

       end subroutine sceua_get_character_options



C -- SUBROUTINE SCEUA_RUN ------------------------------------------------------

       subroutine sceua_run(ifail)

       use sceua_work
       implicit none

       integer, intent(out)   :: ifail

       ifail=0

C -- Check that other necessary subroutines have been called.

       if(icount_set_problem_definition.eq.0)then
         write(amessage,5)
5        format('Subroutine SCEUA_RUN must only be called after ',
     +   'subroutine SCEUA_SET_PROBLEM_DEFINITION has been called.')
         ifail=1
         return
       end if
       icount_sceua_run=icount_sceua_run+1

C -- Check all input data and adjust if necessary.

       call check_input_data(ifail)
       if(ifail.ne.0) return

C -- Inform SCEUA of its output unit numbers and number of observation groups.

       call set_outunits(outunit1,outunit2,ncontrib)

C -- If necessary, provide parallelisation information.

       if(ipll.ne.0)then
         call set_parallel_data(ipll,iopt,iunit_slave,
     +   sce_slave_tplfile,sce_slave_outfile)
       end if

C -- Allocate memory in SCEUA_WORK arrays.

       call allocate_sce_arrays(ifail,nopt,nps,ngs,npg)
       if(ifail.ne.0)then
         write(amessage,20)
20       format('Cannot allocate SCE internal arrays.')
         ifail=1
         return
       end if

C -- Provide restart information.

       if(restartfile.eq.' ')then
         if(irestart.ne.0)then
           write(amessage,15)
15         format('Error in subroutine SCEUA_RUN: no name has been ',
     +     'provided for restart file.')
           ifail=1
           return
         end if
       end if
       call set_restart_data(irestart,restartfile)

C -- Run sceua

       call sceua(ifail,a,bl,bu,nopt,maxn,kstop,pcento,iseed,
     &                 ngs,npg,nps,nspl,mings,iniflg,iprint)
       if(ifail.ne.0) then
         if(ifail.eq.110)then
           write(amessage,22) trim(restartfile)
22         format('Cannot open restart file ',a,'.')
           ifail=1
         else if(ifail.eq.111)then
           write(amessage,23) trim(restartfile)
23         format('Error encountered when reading re-start data ',
     +     'from file ',a,'.')
           ifail=1
         else if(ifail.eq.120)then
           write(amessage,24)
24         format('Error encountered when reading data sent ',
     +     'from master program.')
           ifail=1
         end if
         return
       end if

C -- Deallocate SCE arrays.

       call deallocate_sce_arrays(ifail)
       if(ifail.ne.0)then
         write(amessage,30)
30       format('Problem encountered in deallocating SCE internal ',
     +   'arrays.')
         ifail=1
         return
       end if

       call get_stop_reason(stop_reason)

       return

       end subroutine sceua_run


C -- SUBROUTINE SCEUA_FIN ----------------------------------------------------

       subroutine sceua_fin(ifail)

       implicit none

       integer, intent(out)  :: ifail
       integer               :: ierr

       ifail=0
       icount_sceua_fin=icount_sceua_fin+1

! -- Arrays are deallocated.

       if(allocated(a))        deallocate(a,stat=ierr)
       if(allocated(bl))       deallocate(bl,stat=ierr)
       if(allocated(bu))       deallocate(bu,stat=ierr)

       return

       end subroutine sceua_fin


C -- SUBROUTINE SCEUA_GET_MESSAGE_STRING ------------------------------------------

       subroutine sceua_get_message_string(ifail,amessage_out)

       implicit none

       integer, intent(out)       :: ifail
       character*(*), intent(out) :: amessage_out

       ifail=0
       amessage_out=amessage

       return

       end subroutine sceua_get_message_string


C -- SUBROUTINE SCEUA_LOWCASE ----------------------------------------------------

        subroutine sceua_lowcase(ASTRNG)

! -- Subroutine SCEUA_LOWCASE converts a string to lower case.

        INTEGER I,J
        CHARACTER*(*) ASTRNG

        DO 10 I=1,len_trim(ASTRNG)
        J=ICHAR(ASTRNG(I:I))
        IF((J.GE.65).AND.(J.LE.90)) ASTRNG(I:I)=CHAR(J+32)
10      CONTINUE
        RETURN

        end subroutine sceua_lowcase


C -- SUBROUTINE SCEUA_WRITINT ----------------------------------------------------

        subroutine sceua_writint(atemp,ival)

! -- Subroutine SCEUA_WRITINT writes an integer to a character variable.

        integer*4 ival
        character*6 afmt
        character*(*) atemp

        afmt='(i   )'
        write(afmt(3:5),'(i3)') len(atemp)
        write(atemp,afmt)ival
        atemp=adjustl(atemp)
        return

        end subroutine sceua_writint


       end module sceua_module


