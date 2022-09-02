
module cmaes_module

   private

! -- Control variables.

   integer               :: outunit1                 ! Unit number for run record file.
   integer               :: irestart                 ! Restart control.
   integer               :: resunit                  ! Unit number of restart file.
   integer               :: iopt                     ! Actually used by calling program.
                                                     ! (supplied to the module for restart storage).
! Options defaults: Stopping criteria % (value of stop flag)

   real                  :: StopFitness              ! stop if f(xmin) < stopfitness, minimization
   integer               :: MaxFunEvals              ! maximal number of fevals
   integer               :: MaxIter                  ! maximal number of iterations
   integer               :: StopFunEvals             ! stop after resp. evaluation to resume later
   integer               :: StopIter                 ! stop after resp. iteration to resume later
   real, allocatable     :: TolX(:)                  ! stop if x-change smaller TolX
   real, allocatable     :: TolUpX(:)                ! stop if x-changes larger TolUpX
   real                  :: TolFun                   ! stop if fun-changes smaller TolFun
   logical               :: StopOnWarnings           ! ''no''==''off''==0, ''on''==''yes''==1
   logical               :: SupplyInitialCov=.false. ! Initial c is supplied if true
   real                  :: phiredstp                ! PEST phi stopping criterion
   integer               :: nphistp                  ! PEST phi stopping criterion
   integer               :: nphinored                ! PEST phi stopping criterion
   real                  :: relparstp                ! PEST parameter stopping criterion
   integer               :: nrelpar                  ! PEST parameter stopping criterion
   integer               :: phi_index                ! current index in phimaxminreldiff array
   integer               :: nphimaxmin               ! number of most recent elements to calc rel diff
   real, allocatable     :: phimaxminreldiff(:)      ! relative phi differences
   real                  :: thresh_phimaxmin         ! allowed max difference over nphimaxmin itns

! Options defaults: Other

   real, allocatable     :: DiffMaxChange(:)          ! maximal variable change(s)
   real, allocatable     :: DiffMinChange(:)          ! minimal variable change(s)
   logical               :: WarnOnEqualFunctionValues = .true.
   real, allocatable     :: LBounds(:)                ! lower bounds
   real, allocatable     :: UBounds(:)                ! upper bounds
   logical               :: EvalInitialX = .true.     ! evaluation of initial solution

   integer               :: PopSize                   !% population size, lambda
   integer               :: ParentNumber              !% number of parents, mu
   character*30          :: RecombinationWeights = 'superlinear decrease'   !% or linear, or equal
   logical               :: Science = .false.         !% off==do some additional (minor) problem capturing
   integer               :: Seed = 1111               ! I don't know what to do about this

! -- Problem variables

   integer               :: N                         ! Dimensions of problem.
   real, allocatable     :: xstart(:)                 ! Starting values of adjustable variables.
   real, allocatable     :: insigma(:)                ! Starting standard deviations of adjustable variables.

! -- Subroutines that have been called.

   integer               :: icount_cmaes_init              = 0
   integer               :: icount_set_problem_definition  = 0
   integer               :: icount_set_logical_options     = 0
   integer               :: icount_set_integer_options     = 0
   integer               :: icount_set_real_options        = 0
   integer               :: icount_set_real_array_options  = 0
   integer               :: icount_set_real_matrix_options = 0
   integer               :: icount_set_character_options   = 0
   integer               :: icount_get_logical_options     = 0
   integer               :: icount_get_integer_options     = 0
   integer               :: icount_get_real_options        = 0
   integer               :: icount_get_real_array_options  = 0
   integer               :: icount_get_real_matrix_options = 0
   integer               :: icount_get_character_options   = 0
   integer               :: icount_get_best_parameters     = 0
   integer               :: icount_cmaes_run               = 0
   integer               :: icount_cmaes_fin               = 0

! -- Miscellaneous

   character (len=10)    :: cmaVersion = '2.50'
   character*500         :: amessage                  ! Error message string.

! -- Variables used in computation.

   logical               :: flgscience
   logical               :: flgsaving
   logical               :: flgsavingfinal
   logical               :: flgwarnonequalfunctionvalues

   integer               :: countnorm=0
   integer               :: counteval
   integer               :: numberofvariables
   integer               :: lambda
   integer               :: mu
   integer               :: countiter
   integer               :: outiter
   integer               :: i,j,k,m
   integer               :: iflag
   integer               :: jfail
   integer               :: ierr
   integer               :: lwork
   integer               :: ldvt=1
   integer               :: itemp,itemp1,itemp2
   integer               :: info
   integer               :: iterlo=0
   integer               :: adjust_flag
   integer               :: num_extra
   integer               :: INANVALUE=-HUGE(INANVALUE)


   real                  :: RNANVALUE=-1.1e35
   real                  :: RNANTHRESH=-1.0e35
   real                  :: fmean
   real                  :: mueff
   real                  :: sum1,sum2
   real                  :: cc,cs
   real                  :: mucov,ccov
   real                  :: damps
   real                  :: rtemp,rtemp1,rtemp2,rtemp3,rtemp4
   real                  :: sigma
   real                  :: fac
   real                  :: hsig
   real                  :: chin
   real                  :: val
   real                  :: vt(1,1)
   real                  :: philo=1.1e35

   character*15          :: atemp1,atemp2
   character*15          :: atemp

   integer               :: nstopflag=0
   integer, parameter    :: MAXSTOPFLAG=10
   character*30          :: stopflag(MAXSTOPFLAG)

   real, allocatable     :: xmean(:)
   real, allocatable     :: xlo(:)
   real, allocatable     :: xold(:)
   real, allocatable     :: zmean(:)
   real, allocatable     :: maxdx(:)           ! we should be able to get rid of this or diffmaxchange
   real, allocatable     :: mindx(:)           ! we should be able to get rid of this or diffminchange
   real, allocatable     :: weights(:)
   real, allocatable     :: pc(:)
   real, allocatable     :: ps(:)
   real, allocatable     :: b(:,:),d(:),bd(:,:),c(:,:) ! Note that I am defining d as a vector whereas it is a matrix in matlab

   real, allocatable         :: fitness_hist(:)
   real, allocatable         :: fitness_histsel(:)
   real, allocatable         :: fitness_raw(:)
   real, allocatable         :: fitness_sel(:)
   integer, allocatable      :: fitness_idx(:)
   integer, allocatable      :: fitness_idxsel(:)
   real, allocatable         :: xhist(:,:)
   integer                   :: ncontrib          ! This is only used for reporting purposes
   real, allocatable         :: phi_contrib(:)    ! This is only used for reporting purposes
   character*12, allocatable :: agp_contrib(:)    ! This is only used for reporting purposes

! -- Bounds

   logical               :: bnd_validfitval
   logical               :: bnd_iniphase
   logical               :: bnd_isactive
   real, allocatable     :: bnd_weights(:),bnd_scale(:)
   logical, allocatable  :: bnd_isbounded(:)
   real, allocatable     :: bnd_dfithist(:)               !!!!! check that this should be real and not integer

! -- Random numbers

   integer               :: seed_size
   integer, allocatable  :: seed1(:),startseed(:)

   real, allocatable     :: arz(:,:)                    !!!!!! I think we can dispense with some of these arrays.
   real, allocatable     :: arx(:,:)
   real, allocatable     :: arxvalid(:,:)

   integer, allocatable  :: iwork(:)
   real, allocatable     :: rwork(:)
   real, allocatable     :: rwork1(:)
   real, allocatable     :: cwork1(:,:),cwork2(:,:)
   real, allocatable     :: tx(:)
   integer, allocatable  :: ti(:)

   character*200         :: restartfile

! -- Variables used in handling MIN/MAX problems with some UNIX compilers.

   integer               :: itempm
   real                  :: rtempm,rtempm1,rtempm2


! -- Interfaces

   interface

   subroutine mul_feval(ifail,n,xx,yy,adjust_flag,num_extra)
          integer, intent(out)  :: ifail
          integer, intent(in)   :: n
          real, intent(inout)   :: xx(:,:)
          real, intent(inout)   :: yy(:)
          integer, intent(out)  :: adjust_flag
          integer, intent(out)  :: num_extra
   end subroutine mul_feval


   subroutine record_parameters(ifail,xx)
          integer, intent(out)  :: ifail
          real, intent(in)      :: xx(:)
   end subroutine record_parameters

   end interface

! -- The random number funcion

   real, external :: cmaes_gennor

! -- Variables used in input/output.


   character*10          :: anum


! -- SUBROUTINES

! -- Visible subroutines

      public cmaes_init,                    &
             cmaes_run,                     &
             cmaes_fin,                     &
             cmaes_set_problem_definition,  &
             cmaes_set_logical_options,     &
             cmaes_set_integer_options,     &
             cmaes_set_real_options,        &
             cmaes_set_real_array_options,  &
             cmaes_set_real_matrix_options, &
             cmaes_set_character_options,   &
             cmaes_get_logical_options,     &
             cmaes_get_integer_options,     &
             cmaes_get_real_options,        &
             cmaes_get_real_array_options,  &
             cmaes_get_real_matrix_options, &
             cmaes_get_character_options,   &
             cmaes_get_best_parameters,     &
             cmaes_get_message_string,      &
             cmaes_read_restart_data_1,     &
             cmaes_get_stop_info

contains

   subroutine cmaes_init(ifail,ndim,outunit1_in)

       implicit none

       integer, intent(out)  :: ifail
       integer, intent(in)   :: ndim
       integer, intent(in)   :: outunit1_in

! -- Initialisation

       ifail=0
       if(icount_cmaes_init.ne.0)then
         write(amessage,5)
5        format('Error: subroutine CMAES_INIT must not be called more than once.')
         go to 9890
       end if
       icount_cmaes_init=icount_cmaes_init+1

! -- A check is made for illegal input.

       if(ndim.le.0)then
         write(amessage,10)
10       format('Error in subroutine CMAES_INIT. Problem dimensions must be a positive number.')
         go to 9890
       end if
       n=ndim
       if(outunit1_in.le.0)then
         write(amessage,11)
11       format('Error in subroutine CMAES_INIT. Zero or negative unit number supplied.')
         go to 9890
       end if
       outunit1=outunit1_in

! -- Arrays are allocated.

       allocate(xstart(n),insigma(n),c(n,n),xlo(n),stat=ierr)
       if(ierr.ne.0) go to 9200
       allocate(diffmaxchange(n),diffminchange(n),lbounds(n),ubounds(n),   &
       tolx(n),tolupx(n),stat=ierr)
       if(ierr.ne.0) go to 9200

! -- Arrays are given a dummy initial value.

       xstart=RNANVALUE
       insigma=RNANVALUE
       c=RNANVALUE
       diffmaxchange=RNANVALUE
       diffminchange=RNANVALUE
       lbounds=RNANVALUE
       ubounds=RNANVALUE

       return

9200   write(amessage,9210)
9210   format('Cannot allocate sufficient memory to continue execution.')
       go to 9890

9890   ifail=1
       return

   end subroutine cmaes_init



   subroutine cmaes_set_problem_definition(ifail,xstart_in,lbounds_in,ubounds_in,insigma_in)

! -- Subroutine CMAES_SET_PROBLEM_DEFINITION receives initial parameter values and variable standard deviations.

       integer, intent(out)           :: ifail
       real, intent(in)               :: xstart_in(:)
       real, intent(in)               :: lbounds_in(:)
       real, intent(in)               :: ubounds_in(:)
       real, intent(in)               :: insigma_in(:)

! -- Initialisation

       ifail=0
       if(icount_cmaes_init.eq.0)then
          write(amessage,5)
5         format('Subroutine CMAES_SET_PROBLEM_DEFINITION must not be called before ', &
          'subroutine CMAES_INIT.')
          go to 9890
       end if
       if(icount_set_problem_definition.ne.0)then
         write(amessage,6)
6        format('Subroutine CMAES_SET_PROBLEM_DEFINITION must not be called more than once.')
         go to 9890
       end if
       icount_set_problem_definition=icount_set_problem_definition+1

       itemp=size(xstart_in)
       if(itemp.lt.n)then
         call cmaes_writint(atemp,n)
         write(amessage,10) trim(atemp)
10       format('Error in subroutine CMAES_SET_PROBLEM_DEFINITION: dimension of XSTART ', &
         'must be at least ',a,'.')
         go to 9890
       end if
       itemp=size(insigma_in)
       if(itemp.lt.n)then
         call cmaes_writint(atemp,n)
         write(amessage,15) trim(atemp)
15       format('Error in subroutine CMAES_SET_PROBLEM_DEFINITION: dimension of ', &
         'INSIGMA must be at least ',a,'.')
         go to 9890
       end if
       itemp=size(lbounds_in)
       if(itemp.lt.n)then
         call cmaes_writint(atemp,n)
         write(amessage,16) trim(atemp)
16       format('Error in subroutine CMAES_SET_PROBLEM_DEFINITION: dimension of ', &
         'LBOUNDS must be at least ',a,'.')
         go to 9890
       end if
       itemp=size(ubounds_in)
       if(itemp.lt.n)then
         call cmaes_writint(atemp,n)
         write(amessage,17) trim(atemp)
17       format('Error in subroutine CMAES_SET_PROBLEM_DEFINITION: dimension of ', &
         'UBOUNDS mustbe at least ',a,'.')
         go to 9890
       end if

       do i=1,n
         xstart(i)=xstart_in(i)
       end do
       do i=1,n
         lbounds(i)=lbounds_in(i)
       end do
       do i=1,n
         ubounds(i)=ubounds_in(i)
       end do
       do i=1,n
         if(insigma_in(i).le.0.0d0)then
           call cmaes_writint(atemp,i)
           write(amessage,20) trim(atemp)
20         format('Error in subroutine CMAES_SET_PROBLEM_DEFINITION: zero or negative INSIGMA ', &
           'for adjustable variable number ',a,'.')
           go to 9890
         end if
         insigma(i)=insigma_in(i)
       end do

! -- Default values for control variables are set.

       call set_defaults_internal()

       return

9890   continue
       ifail=1
       return

   end subroutine cmaes_set_problem_definition


   subroutine set_defaults_internal()

! -- Subroutine SET_DEFAULTS_INTERNAL sets defaults.

       implicit none

       real rn,rpopsize

       iopt=0

       rn=float(n)
       popsize = 4 + ceiling(3.0*log(rn))
       rpopsize=float(popsize)
       lambda=popsize

       bnd_isactive       = .true.
       SupplyInitialCov   = .false.
       StopFitness        = -huge(stopfitness)
       MaxIter            =  nint(1.0e3*(rn+5)*(rn+5)/sqrt(rpopsize))
       MaxFunEvals        =  huge(MaxFunEvals)
       StopFunEvals       =  huge(stopfunevals)
       StopIter           =  huge(stopiter)
       TolX               =  1.0e-12*insigma     ! an array
       TolUpX             =  1.0e8*insigma       ! an array
       TolFun             =  1.0e-12
       StopOnWarnings     = .false.

       phiredstp          = 1.0e-3
       nphistp            = 40
       nphinored          = 40
       relparstp          = 1.0e-3
       nrelpar            = 40
       nphimaxmin         = 10
       thresh_phimaxmin   = 0.01

       DiffMaxChange  = huge(rtemp)      ! an array
       DiffMinChange  = 0.0              ! an array
       WarnOnEqualFunctionValues = .true.
       EvalInitialX   = .true.

!       ParentNumber         = max(ceiling(rpopsize/2.0),1)
       rtempm=rpopsize*0.5
!       itempm=ceiling(rtempm)
       itempm=floor(rtempm)
       ParentNumber         = max(itempm,1)
       RecombinationWeights = 'superlinear decrease'
       Science              = .false.
       Seed                 = 1111                   ! I don't know what to do about this
       ncontrib             = 1

       irestart             = 0
       restartfile          ='cmaes.rst'
       amessage             = ' '

       return

   end subroutine set_defaults_internal



   subroutine cmaes_set_logical_options(ifail,option,lvar)

! -- Subroutine CMAES_SET_LOGICAL_OPTIONS allows a calling program to set logical options.

       implicit none

       integer, intent(out)           :: ifail
       character (len=*), intent(in)  :: option
       logical, intent(in)            :: lvar

       character (len=50)             :: aoption

       ifail=0
       if(icount_set_problem_definition.eq.0)then
         write(amessage,5)
5        format('Subroutine CMAES_SET_LOGICAL_OPTIONS must only be called after ',  &
         'subroutine CMAES_SET_PROBLEM_DEFINITION has been called.')
         ifail=1
         return
       end if
       icount_set_logical_options=icount_set_logical_options+1
       aoption=option
       call cmaes_lowcase(aoption)

       select case(aoption)
         case('stoponwarnings')
           stoponwarnings=lvar
         case('warnonequalfunctionvalues')
           warnonequalfunctionvalues=lvar
         case('evalinitialx')
           evalinitialx=lvar
         case('science')
           science=lvar
         case('bnd_isactive')
           bnd_isactive=lvar
         case('supplyinitialcov')
           supplyinitialcov=lvar
         case default
           write(amessage,10) trim(option)
10         format('Unknown or inappropriate variable name "',a,'" in call to ',  &
           'subroutine CMAES_SET_LOGICAL_OPTIONS.')
           ifail=1
           return
         end select

         return

   end subroutine cmaes_set_logical_options



   subroutine cmaes_get_logical_options(ifail,option,lvar)

! -- Subroutine CMAES_GET_LOGICAL_OPTIONS allows a calling program to retreive logical options.

       implicit none

       integer, intent(out)           :: ifail
       character (len=*), intent(in)  :: option
       logical, intent(out)           :: lvar

       character (len=50)             :: aoption

       ifail=0
       if(icount_set_problem_definition.eq.0)then
         write(amessage,5)
5        format('Subroutine CMAES_GET_LOGICAL_OPTIONS must only be called after ',  &
         'subroutine CMAES_SET_PROBLEM_DEFINITION has been called.')
         ifail=1
         return
       end if
       icount_get_logical_options=icount_get_logical_options+1
       aoption=option
       call cmaes_lowcase(aoption)

       select case(aoption)
         case('stoponwarnings')
           lvar=stoponwarnings
         case('warnonequalfunctionvalues')
           lvar=warnonequalfunctionvalues
         case('evalinitialx')
           lvar=evalinitialx
         case('science')
           lvar=science
         case('bnd_isactive')
           lvar=bnd_isactive
         case('supplyinitialcov')
           lvar=supplyinitialcov
         case default
           write(amessage,10) trim(option)
10         format('Unknown or inappropriate variable name "',a,'" in call to ',  &
           'subroutine CMAES_GET_LOGICAL_OPTIONS.')
           ifail=1
           return
         end select

         return

   end subroutine cmaes_get_logical_options


   subroutine cmaes_set_integer_options(ifail,option,ivar)

! -- Subroutine CMAES_SET_INTEGER_OPTIONS allows a calling program to set integer options.

       implicit none

       integer, intent(out)           :: ifail
       character (len=*), intent(in)  :: option
       integer, intent(in)            :: ivar

       character (len=50)             :: aoption

       ifail=0
       if(icount_set_problem_definition.eq.0)then
         write(amessage,5)
5        format('Subroutine CMAES_SET_INTEGER_OPTIONS must only be called after ',  &
         'subroutine CMAES_SET_PROBLEM_DEFINITION has been called.')
         ifail=1
         return
       end if
       icount_set_integer_options=icount_set_integer_options+1
       aoption=option
       call cmaes_lowcase(aoption)

       select case(aoption)
         case('maxfunevals')
           MaxFunEvals=ivar
         case('maxiter')
           MaxIter=ivar
         case('stopfunevals')
           StopFunEvals=ivar
         case('stopiter')
           stopiter=ivar
         case('popsize')
           PopSize=ivar
           lambda=popsize
         case('parentnumber')
           ParentNumber=ivar
         case('seed')
           Seed=ivar
         case('nphistp')
           nphistp=ivar
         case('nphinored')
           nphinored=ivar
         case('nrelpar')
           nrelpar=ivar
         case('ncontrib')
           ncontrib=ivar
         case('irestart')
           irestart=ivar
         case('resunit')
           resunit=ivar
         case('iopt')
           iopt=ivar
         case('nphimaxmin')
           nphimaxmin=ivar
         case default
           write(amessage,10) trim(option)
10         format('Unknown or inappropriate variable name "',a,'" in call to ',  &
           'subroutine CMAES_SET_INTEGER_OPTIONS.')
           ifail=1
           return
         end select

         return

   end subroutine cmaes_set_integer_options


   subroutine cmaes_get_integer_options(ifail,option,ivar)

! -- Subroutine CMAES_GET_INTEGER_OPTIONS allows a calling program to retreive integer options.

       implicit none

       integer, intent(out)           :: ifail
       character (len=*), intent(in)  :: option
       integer, intent(out)           :: ivar

       character (len=50)             :: aoption

       ifail=0
       if(icount_set_problem_definition.eq.0)then
         write(amessage,5)
5        format('Subroutine CMAES_GET_INTEGER_OPTIONS must only be called after ',  &
         'subroutine CMAES_SET_PROBLEM_DEFINITION has been called.')
         ifail=1
         return
       end if
       icount_get_integer_options=icount_get_integer_options+1
       aoption=option
       call cmaes_lowcase(aoption)

       select case(aoption)
         case('maxfunevals')
           ivar=MaxFunEvals
         case('maxiter')
           ivar=MaxIter
         case('stopfunevals')
           ivar=StopFunEvals
         case('stopiter')
           ivar=stopiter
         case('popsize')
           ivar=PopSize
         case('parentnumber')
           ivar=ParentNumber
         case('seed')
           ivar=Seed
         case('nphistp')
           ivar=nphistp
         case('nphinored')
           ivar=nphinored
         case('nrelpar')
           ivar=nrelpar
         case('ncontrib')
           ivar=ncontrib
         case('irestart')
           ivar=irestart
         case('resunit')
           ivar=resunit
         case('iopt')
           ivar=iopt
         case('nphimaxmin')
           ivar=nphimaxmin
         case default
           write(amessage,10) trim(option)
10         format('Unknown or inappropriate variable name "',a,'" in call to ',  &
           'subroutine CMAES_GET_INTEGER_OPTIONS.')
           ifail=1
           return
         end select

         return

   end subroutine cmaes_get_integer_options



   subroutine cmaes_set_real_options(ifail,option,rvar)

! -- Subroutine CMAES_SET_REAL_OPTIONS allows a calling program to set real options.

       implicit none

       integer, intent(out)           :: ifail
       character (len=*), intent(in)  :: option
       real, intent(in)               :: rvar

       character (len=50)             :: aoption

       ifail=0
       if(icount_set_problem_definition.eq.0)then
         write(amessage,5)
5        format('Subroutine CMAES_SET_REAL_OPTIONS must only be called after ',  &
         'subroutine CMAES_SET_PROBLEM_DEFINITION has been called.')
         ifail=1
         return
       end if
       icount_set_real_options=icount_set_real_options+1
       aoption=option
       call cmaes_lowcase(aoption)

       select case(aoption)
         case('stopfitness')
           StopFitness=rvar
         case('tolfun')
           TolFun=rvar
         case('phiredstp')
           phiredstp=rvar
         case('relparstp')
           relparstp=rvar
         case('sigma')
           sigma=rvar
         case('thresh_phimaxmin')
           thresh_phimaxmin=rvar
         case default
           write(amessage,10) trim(option)
10         format('Unknown or inappropriate variable name "',a,'" in call to ',  &
           'subroutine CMAES_SET_REAL_OPTIONS.')
           ifail=1
           return
         end select

         return

   end subroutine cmaes_set_real_options


   subroutine cmaes_get_real_options(ifail,option,rvar)

! -- Subroutine CMAES_GET_REAL_OPTIONS allows a calling program to retreive real options.

       implicit none

       integer, intent(out)           :: ifail
       character (len=*), intent(in)  :: option
       real, intent(out)              :: rvar

       character (len=50)             :: aoption

       ifail=0
       if(icount_set_problem_definition.eq.0)then
         write(amessage,5)
5        format('Subroutine CMAES_GET_REAL_OPTIONS must only be called after ',  &
         'subroutine CMAES_SET_PROBLEM_DEFINITION has been called.')
         ifail=1
         return
       end if
       icount_get_real_options=icount_get_real_options+1
       aoption=option
       call cmaes_lowcase(aoption)

       select case(aoption)
         case('stopfitness')
           rvar=StopFitness
         case('tolfun')
           rvar=TolFun
         case('phiredstp')
           rvar=phiredstp
         case('relparstp')
           rvar=relparstp
         case('sigma')
           rvar=sigma
         case('thresh_phimaxmin')
           rvar=thresh_phimaxmin
         case default
           write(amessage,10) trim(option)
10         format('Unknown or inappropriate variable name "',a,'" in call to ',  &
           'subroutine CMAES_GET_REAL_OPTIONS.')
           ifail=1
           return
         end select

         return

   end subroutine cmaes_get_real_options




   subroutine cmaes_set_real_array_options(ifail,option,rvar)

! -- Subroutine CMAES_SET_REAL_ARRAY_OPTIONS allows a calling program to set options in real arrays.

       implicit none

       integer, intent(out)           :: ifail
       character (len=*), intent(in)  :: option
       real, intent(in)               :: rvar(:)

       character (len=50)             :: aoption

       ifail=0
       if(icount_set_problem_definition.eq.0)then
         write(amessage,5)
5        format('Subroutine CMAES_SET_REAL_ARRAY_OPTIONS must only be called after ',  &
         'subroutine CMAES_SET_PROBLEM_DEFINITION has been called.')
         ifail=1
         return
       end if
       icount_set_real_array_options=icount_set_real_array_options+1

       if(size(rvar).lt.n)then
         call cmaes_writint(atemp,n)
         write(amessage,6) trim(atemp)
6        format('Error in subroutine CMAES_SET_REAL_ARRAY_OPTIONS: SIZE of RVAR argument must ', &
         ' be ',a,' or greater.')
         ifail=1
         return
       end if

       aoption=option
       call cmaes_lowcase(aoption)

       select case(aoption)
         case('diffmaxchange')
           do i=1,n
             DiffMaxChange(i)=rvar(i)
           end do
         case('diffminchange')
           do i=1,n
             DiffMinChange(i)=rvar(i)
           end do
         case('tolx')
           do i=1,n
             TolX(i)=rvar(i)
           end do
         case('tolupx')
           do i=1,n
             TolUpX(i)=rvar(i)
           end do
         case('sigma')
           do i=1,n
             insigma(i)=rvar(i)
           end do
         case default
           write(amessage,10) trim(option)
10         format('Unknown or inappropriate variable name "',a,'" in call to subroutine ', &
           'CMAES_SET_REAL_ARRAY_OPTIONS.')
           ifail=1
           return
         end select

         return

   end subroutine cmaes_set_real_array_options


   subroutine cmaes_get_real_array_options(ifail,option,rvar)

! -- Subroutine CMAES_GET_REAL_ARRAY_OPTIONS allows a calling program to retreive options in real arrays.

       implicit none

       integer, intent(out)           :: ifail
       character (len=*), intent(in)  :: option
       real, intent(out)              :: rvar(:)

       character (len=50)             :: aoption

       ifail=0
       if(icount_set_problem_definition.eq.0)then
         write(amessage,5)
5        format('Subroutine CMAES_GET_REAL_ARRAY_OPTIONS must only be called after ',  &
         'subroutine CMAES_SET_PROBLEM_DEFINITION has been called.')
         ifail=1
         return
       end if
       icount_get_real_array_options=icount_get_real_array_options+1

       if(size(rvar).lt.n)then
         call cmaes_writint(atemp,n)
         write(amessage,6) trim(atemp)
6        format('Error in subroutine CMAES_GET_REAL_ARRAY_OPTIONS: SIZE of RVAR argument must ', &
         ' be ',a,' or greater.')
         ifail=1
         return
       end if

       aoption=option
       call cmaes_lowcase(aoption)

       select case(aoption)
         case('diffmaxchange')
           do i=1,n
             rvar(i)=DiffMaxChange(i)
           end do
         case('diffminchange')
           do i=1,n
             rvar(i)=DiffMinChange(i)
           end do
         case('tolx')
           do i=1,n
             rvar(i)=tolx(i)
           end do
         case('tolupx')
           do i=1,n
             rvar(i)=tolupx(i)
           end do
         case('sigma')
           do i=1,n
             rvar(i)=insigma(i)
           end do
         case default
           write(amessage,10) trim(option)
10         format('Unknown or inappropriate variable name "',a,'" in call to subroutine ', &
           'CMAES_GET_REAL_ARRAY_OPTIONS.')
           ifail=1
           return
         end select

         return

   end subroutine cmaes_get_real_array_options


   subroutine cmaes_set_real_matrix_options(ifail,option,rvar)

! -- Subroutine CMAES_SET_REAL_MATRIX_OPTIONS allows a calling program to set
!    real matrices.

       implicit none

       integer, intent(out)           :: ifail
       character (len=*), intent(in)  :: option
       real, intent(in)               :: rvar(:,:)

       character (len=50)             :: aoption

       ifail=0
       if(icount_set_problem_definition.eq.0)then
         write(amessage,5)
5        format('Subroutine CMAES_SET_REAL_MATRIX_OPTIONS must only be called after ',  &
         'subroutine CMAES_SET_PROBLEM_DEFINITION has been called.')
         ifail=1
         return
       end if
       icount_set_real_matrix_options=icount_set_real_matrix_options+1

       if((size(rvar,1).lt.n).or.(size(rvar,2).lt.n))then
         call cmaes_writint(atemp,n)
         write(amessage,6) trim(atemp),trim(atemp)
6        format('Error in subroutine CMAES_SET_REAL_MATRIX_OPTIONS: dimensions ',  &
         'of RVAR matrix argument must be at least ',a,' * ',a,'.')
         ifail=1
         return
       end if

       aoption=option
       call cmaes_lowcase(aoption)

       select case(aoption)
         case('c')
           do i=1,n
             do j=1,n
               c(j,i)=rvar(j,i)
             end do
           end do
         case default
           write(amessage,10) trim(option)
10         format('Unknown or inappropriate variable name "',a,'" in call to subroutine ', &
           'CMAES_SET_REAL_MATRIX_OPTIONS.')
           ifail=1
           return
         end select

         return

   end subroutine cmaes_set_real_matrix_options


   subroutine cmaes_get_real_matrix_options(ifail,option,rvar)

! -- Subroutine CMAES_GET_REAL_MATRIX_OPTIONS allows a calling program to retreive
!    real matrices.

       implicit none

       integer, intent(out)           :: ifail
       character (len=*), intent(in)  :: option
       real, intent(out)              :: rvar(:,:)

       character (len=50)             :: aoption

       ifail=0
       if(icount_set_problem_definition.eq.0)then
         write(amessage,5)
5        format('Subroutine CMAES_GET_REAL_MATRIX_OPTIONS must only be called after ',  &
         'subroutine CMAES_SET_PROBLEM_DEFINITION has been called.')
         ifail=1
         return
       end if
       icount_get_real_matrix_options=icount_get_real_matrix_options+1

       if((size(rvar,1).lt.n).or.(size(rvar,2).lt.n))then
         call cmaes_writint(atemp,n)
         write(amessage,6) trim(atemp),trim(atemp)
6        format('Error in subroutine CMAES_GET_REAL_MATRIX_OPTIONS: dimensions ',  &
         'of RVAR matrix argument must be at least ',a,' * ',a,'.')
         ifail=1
         return
       end if

       aoption=option
       call cmaes_lowcase(aoption)

       select case(aoption)
         case('c')
           do i=1,n
             do j=1,n
               rvar(j,i)=c(j,i)
             end do
           end do
         case default
           write(amessage,10) trim(option)
10         format('Unknown or inappropriate variable name "',a,'" in call to subroutine ', &
           'CMAES_GET_REAL_MATRIX_OPTIONS.')
           ifail=1
           return
         end select

         return

   end subroutine cmaes_get_real_matrix_options



   subroutine cmaes_set_character_options(ifail,option,bvar)

! -- Subroutine CMAES_SET_CHARACTER_OPTIONS allows a calling program to set character options.

       implicit none

       integer, intent(out)           :: ifail
       character (len=*), intent(in)  :: option
       character (len=*), intent(in)  :: bvar

       character (len=50)             :: aoption
       character (len=200)            :: avar

       ifail=0
       if(icount_set_problem_definition.eq.0)then
         write(amessage,5)
5        format('Subroutine CMAES_SET_CHARACTER_OPTIONS must only be called after ',  &
         'subroutine CMAES_SET_PROBLEM_DEFINITION has been called.')
         ifail=1
         return
       end if
       icount_set_character_options=icount_set_character_options+1

       aoption=option
       call cmaes_lowcase(aoption)
       avar=bvar
       call cmaes_lowcase(avar)
       avar=adjustl(avar)

       select case(aoption)
         case('recombinationweights')
           RecombinationWeights=avar
         case('restartfile')
           restartfile=avar
         case default
           write(amessage,10) trim(option)
10         format('Unknown or inappropriate variable name "',a,'" in call to subroutine ',  &
           'CMAES_SET_CHARACTER_OPTIONS.')
           ifail=1
           return
         end select

         return

   end subroutine cmaes_set_character_options


   subroutine cmaes_get_character_options(ifail,option,avar)

! -- Subroutine CMAES_GET_CHARACTER_OPTIONS allows a calling program to retreive character options.

       implicit none

       integer, intent(out)           :: ifail
       character (len=*), intent(in)  :: option
       character (len=*), intent(out) :: avar

       character (len=50)             :: aoption

       ifail=0
       if(icount_set_problem_definition.eq.0)then
         write(amessage,5)
5        format('Subroutine CMAES_GET_CHARACTER_OPTIONS must only be called after ',  &
         'subroutine CMAES_SET_PROBLEM_DEFINITION has been called.')
         ifail=1
         return
       end if
       icount_get_character_options=icount_get_character_options+1

       aoption=option
       call cmaes_lowcase(aoption)

       select case(aoption)
         case('recombinationweights')
           avar=RecombinationWeights
         case('restartfile')
           avar=restartfile
         case default
           write(amessage,10) trim(option)
10         format('Unknown or inappropriate variable name "',a,'" in call to subroutine ',  &
           'CMAES_GET_CHARACTER_OPTIONS.')
           ifail=1
           return
         end select

         return

   end subroutine cmaes_get_character_options


   subroutine cmaes_get_best_parameters(ifail,xx)

       implicit none

       integer, intent(out)           :: ifail
       real   , intent(out)           :: xx(:)

       ifail=0
       if(icount_cmaes_run.eq.0)then
         write(amessage,5)
5        format('Subroutine CMAES_GET_BEST_PARAMETERS must only be called after ',  &
         'subroutine CMAES_RUN has been called.')
         ifail=1
         return
       end if
       if(size(xx).lt.n)then
         call cmaes_writint(atemp,n)
         write(amessage,10) trim(atemp)
10       format('Error in subroutine CMAES_GET_BEST_PARAMETERS: second argument ', &
         'must be an array with at least 10 elements.')
         ifail=1
         return
       end if
       do i=1,n
         xx(i)=xlo(i)
       end do

       return

   end subroutine cmaes_get_best_parameters



   subroutine cmaes_run(ifail)

     implicit none

     integer, intent(out)   :: ifail

     ifail=0
     icount_cmaes_run = icount_cmaes_run+1

! -- Some memory is allocated (don't forget to de-allocate later.)

! -- Arrays are allocated.
!    As they are allocated they are given a dummy initial value.

      allocate(maxdx(n),mindx(n),stat=ierr)
      if(ierr.ne.0) go to 9200
      maxdx=RNANVALUE
      mindx=RNANVALUE

      allocate(xmean(n),xold(n),zmean(n),stat=ierr)
      if(ierr.ne.0) go to 9200
      xmean=RNANVALUE
      xold=RNANVALUE
      zmean=RNANVALUE
      xlo=RNANVALUE

      allocate(pc(n),ps(n),stat=ierr)
      if(ierr.ne.0) go to 9200
      pc=RNANVALUE
      ps=RNANVALUE

      allocate(b(n,n),d(n),bd(n,n),stat=ierr)
      if(ierr.ne.0) go to 9200
      b=RNANVALUE
      d=RNANVALUE
      bd=RNANVALUE

      allocate(bnd_weights(n),bnd_scale(n),bnd_isbounded(n),stat=ierr)
      if(ierr.ne.0) go to 9200
      bnd_weights=RNANVALUE
      bnd_scale=RNANVALUE
      bnd_isbounded=.false.

      allocate(tx(n),ti(n),stat=ierr)
      if(ierr.ne.0) go to 9200
      tx=RNANVALUE
      ti=INANVALUE

      itemp=ceiling(20+float(3*N)/float(popsize))
      allocate(bnd_dfithist(itemp),stat=ierr)
      if(ierr.ne.0) go to 9200
      bnd_dfithist=RNANVALUE

      itemp=10+ceiling(float(3*10*N)/float(lambda))
      if(itemp.lt.nphinored+1)itemp=nphinored+1
      if(itemp.lt.nphistp+1)itemp=nphistp+1
      allocate(fitness_hist(itemp),fitness_histsel(itemp),fitness_raw(popsize),    &
      fitness_sel(popsize),fitness_idx(popsize),fitness_idxsel(popsize),stat=ierr)
      if(ierr.ne.0) go to 9200
      fitness_hist=RNANVALUE
      fitness_histsel=RNANVALUE
      fitness_raw=RNANVALUE
      fitness_sel=RNANVALUE
      fitness_idx=INANVALUE
      fitness_idxsel=INANVALUE

      allocate(arz(n,popsize),arx(n,popsize),arxvalid(n,popsize),stat=ierr)
      if(ierr.ne.0) go to 9200
      arz=RNANVALUE
      arx=RNANVALUE
      arxvalid=RNANVALUE

      lwork=8*n
!      itemp1=max(n,popsize,20+float(3*N)/float(popsize)+1)
      rtempm=float(3*N)/float(popsize)
      itempm=20+ceiling(rtempm)+1
      itemp1=max(n,popsize,itempm)
      itemp2=max(n,popsize,lwork)
      allocate(iwork(itemp1),rwork(itemp2),stat=ierr)           ! check later that it doesn't need to be allocated greater
      if(ierr.ne.0) go to 9200
      iwork=INANVALUE
      rwork=RNANVALUE

      allocate(rwork1(popsize),stat=ierr)
      if(ierr.ne.0) go to 9200
      rwork1=RNANVALUE

      allocate(cwork1(n,parentnumber),cwork2(n,n),stat=ierr)
      if(ierr.ne.0) go to 9200
      cwork1=RNANVALUE
      cwork2=RNANVALUE

      allocate(weights(parentnumber),stat=ierr)
      if(ierr.ne.0) go to 9200
      weights=RNANVALUE

      allocate(xhist(n,nrelpar+1),stat=ierr)
      if(ierr.ne.0) go to 9200
      xhist=RNANVALUE

      allocate(phi_contrib(ncontrib),agp_contrib(ncontrib),stat=ierr)
      if(ierr.ne.0) go to 9200
      phi_contrib=RNANVALUE
      agp_contrib=' '

      allocate(phimaxminreldiff(nphimaxmin),stat=ierr)
      if(ierr.ne.0) go to 9200
      phimaxminreldiff=RNANVALUE

! -- Initialisation

      nstopflag=0
      stopflag=' '      ! an array
      counteval=0
      phi_index=0

      if(irestart.eq.2)then
        call cmaes_read_restart_data_2(ifail)
        if(ifail.ne.0) go to 9890
        go to 325
      end if

!!!% ------------------------ Initialization -------------------------------

      xmean=xstart           ! arrays
      xlo=xmean              ! arrays
      numberofvariables = N

      flgWarnOnEqualFunctionValues=WarnOnEqualFunctionValues
      flgscience = Science
      flgsaving=.true.
      flgsavingfinal=.true.
      xmean=xstart          ! arrays
      maxdx=DiffMaxChange   ! arrays
      mindx=DiffMinChange   ! arrays

      mu=ParentNumber
      if(index(RecombinationWeights,'equal').ne.0)then
        weights=1.0       ! an array
      else if(index(RecombinationWeights,'superlinear').ne.0)then
        do i=1,mu
          weights(i)=log(float(mu+1))-log(float(i))
        end do
      else if(index(RecombinationWeights,'linear').ne.0)then
        do i=1,mu
          weights(i)=mu+1-i
        end do
      else
        write(amessage,210) trim(RecombinationWeights)
210     format('Error in subroutine CMAES_RUN: recombination weights strategy ',  &
        '"',a,'" is not yet implemented.')
        go to 9890
      end if
!!!!!!CHECK THE ABOVE AGAINST THE THEORY.

      sum1=0.0
      sum2=0.0
      do i=1,mu
        sum1=sum1+weights(i)
        sum2=sum2+weights(i)*weights(i)
      end do
      mueff=sum1*sum1/sum2
!!!!!!CHECK THE ABOVE AGAINST THE THEORY


!!!  if mueff == lambda
!!!    error(['Combination of values for PopSize, ParentNumber and ' ...
!!!	  ' and RecombinationWeights is not reasonable']);
!!!  end

      if(nint(mueff).ge.lambda)then
        write(amessage,220)
220     format('Error in subroutine CMAES_RUN: combination of values ',  &
        'for PopSize, ParentNumber and RecombinationWeights is not reasonable.')
        go to 9890
      end if

!!!  % Strategy internal parameter setting: Adaptation
!!!  cc = 4/(N+4);         % time constant for cumulation for covariance matrix
!!!  cs = (mueff+2)/(N+mueff+3); % t-const for cumulation for step size control
!!!  mucov = mueff;   % size of mu used for calculating learning rate ccov
!!!  ccov = (1/mucov) * 2/(N+1.41)^2 ... % learning rate for covariance matrix
!!!	 + (1-1/mucov) * min(1,(2*mueff-1)/((N+2)^2+mueff));

      cc = 4.0/(N+4.0)
      cs = (mueff+2.0)/(N+mueff+3.0)
      mucov = mueff
!      ccov = (1.0/mucov) * 2.0/(N+1.41)/(N+1.41)                     &
!      + (1.0-1.0/mucov) * min(1,(2.0*mueff-1.0)/((N+2.0)*(N+2.0)+mueff))
      rtempm=(2.0*mueff-1.0)/((N+2.0)*(N+2.0)+mueff)
      rtempm=min(1.0,rtempm)
      ccov = (1.0/mucov) * 2.0/(N+1.41)/(N+1.41)                     &
      + (1.0-1.0/mucov) * rtempm

!!!  % ||ps|| is close to sqrt(mueff/N) for mueff large on linear fitness
!!!  damps = ... % damping for step size control, usually close to one
!!!      (1 + 2*max(0,sqrt((mueff-1)/(N+1))-1)) ... % limit sigma increase
!!!      * max(0.3, ... % reduce damps, if max. iteration number is small
!!!	  1 - N/min(stopMaxIter,stopMaxFunEvals/lambda)) + cs;

      rtemp=(mueff-1.0)/(N+1)
      if(rtemp.lt.0.0)rtemp=0.0
      itemp=max(maxiter,1000)
!      damps =(1.0 + 2.0*max(0,sqrt(rtemp)-1.0))    &
!             * max(0.3,1.0 - float(N)/min(float(itemp),float(MaxFunEvals)/float(lambda))) + cs
      rtempm1=float(itemp)
      rtempm2=float(MaxFunEvals)/float(lambda)
      rtempm2=min(rtempm1,rtempm2)
      rtempm1=float(N)
      rtempm=1.0-rtempm1/rtempm2
      rtempm=max(0.3,rtempm)
      damps =(1.0 + 2.0*max(0.0,sqrt(rtemp)-1.0))*rtempm+cs

! -- The following is what the paper says. Maybe it would be better.

!!!!      damps =(1.0 + 2.0*max(0,sqrt(rtemp)-1.0)) + cs

      if(any(insigma.le.0.0))then
        write(amessage,250)
250     format('Error in subroutine CMAES_RUN: ',                  &
        'Initial search volume (SIGMA) must be greater than zero.')
        go to 9890
      end if

!!!  if max(insigma)/min(insigma) > 1e6
!!!    error(['Initial search volume (SIGMA) badly conditioned']);
!!!  end

      if(maxval(insigma)/minval(insigma).gt.1.0e6)then
        write(amessage,260)
260     format('Error in subroutine CMAES_RUN: initial search volume (SIGMA) badly conditioned.')
        go to 9890
      end if

!!!  sigma = max(insigma);              % overall standard deviation
!!!  pc = zeros(N,1); ps = zeros(N,1);  % evolution paths for C and sigma

      sigma = maxval(insigma)
      pc=0.0                   ! an array
      ps=0.0                   ! an array

!!!  B = eye(N,N);                      % B defines the coordinate system
!!!  D = diag(insigma/max(insigma));    % diagonal matrix D defines the scaling
!!!  BD = B*D;                          % for speed up only
!!!  C = BD*(BD)';                      % covariance matrix

      if(.not.supplyinitialCov)then
        b = 0.0           ! an array
        d = 0.0           ! an array
        c = 0.0           ! an array
        bd=0.0            ! an array
        do i=1,n
          b(i,i)=1.0
        end do
        rtemp=maxval(insigma)
        do i=1,n
          d(i)=insigma(i)/rtemp
        end do
        do i=1,n
          bd(i,i)=b(i,i)*d(i)
        end do
        do i=1,n
          c(i,i)=bd(i,i)*bd(i,i)
        end do
      else
!        do i=1,n
!          d(i)=sqrt(c(i,i))
!          rtemp=1.0/(d(i))
!          do j=1,n
!            c(i,j)=c(i,j)*rtemp
!          end do
!          do j=1,n
!            c(j,i)=c(j,i)*rtemp
!          end do
!        end do
        rtemp=-huge(rtemp)
        do i=1,n
          if(c(i,i).gt.rtemp)rtemp=c(i,i)
        end do
        if(rtemp.gt.0.0)then
          sigma=sqrt(rtemp)
          rtemp=1.0/rtemp
          c=c*rtemp
        else
          sigma=1.0
        end if
        cwork2=c                ! arrays
        call sgesvd('A','N',n,n,cwork2,n,d,b,n,vt,ldvt,rwork,lwork,info)
        if(info.ne.0)then
          write(amessage,261)
261       format('Error in subroutine CSAEM_RUN: cannot undertake SVD ',  &
          'on user-provided on C matrix.')
          go to 9890
        end if
        do i=1,n
          d(i)=sqrt(d(i))
        end do
        do i=1,n
          do j=1,n
            bd(j,i)=b(j,i)*d(i)
          end do
        end do
      end if

!!!  fitness.hist=NaN*ones(1,10+ceil(3*10*N/lambda)); % history of fitness values
!!!  fitness.histsel=NaN*ones(1,10+ceil(3*10*N/lambda)); % history of fitness values

      fitness_hist=RNANVALUE
      fitness_histsel=RNANVALUE

!!!!!! MAKE SURE TO DEALLOCATE THESE ARRAYS
!!!!!! NOTE THAT THE ORIGINAL CODE HAS THESE AS "HORIZONTAL" RATHER THAN "VERTICAL" VECTORS.
!!!!!! CHECK HOW APPLICABLE THE HUGE IS. SHOULD IT BE POSITIVE OR NEGATIVE HUGE

!!!  % Initialize boundary handling

!xxx      bnd_isactive=.true.                  ! Maybe re-instate later
!xxx      if(bnd_isactive)then                 ! Maybe re-instate later

        if(any(lbounds.ge.ubounds))then
          write(amessage,300)
300       format('Error in subroutine CMAES_RUN: ',                &
          'lower bound is greater than or equal to upper bound ',  &
          'for at least one parameter.')
          go to 9890
        end if

        do i=1,n
          if(xmean(i).lt.lbounds(i))then
            xmean(i)=lbounds(i)
          end if
        end do
        do i=1,n
          if(xmean(i).gt.ubounds(i))then
            xmean(i)=ubounds(i)
          end if
        end do

!!!    bnd.weights = zeros(N,1);         % weights for bound penalty

       bnd_weights=0.0       ! an array

!!!    bnd.scale = diag(C)/mean(diag(C));

        rtemp=0.0
        do i=1,n
          rtemp=rtemp+c(i,i)
        end do
        rtemp=rtemp/n
        do i=1,n
          bnd_scale(i)=c(i,i)/rtemp
        end do

!!!    idx = (lbounds > -Inf) | (ubounds < Inf);
!!!    if length(idx) == 1
!!!      idx = idx * ones(N,1);
!!!    end
!!!    bnd.isbounded = zeros(N,1);
!!!    bnd.isbounded(find(idx)) = 1;

        bnd_isbounded=.true.                   ! all parameters will be bounded (but the bound may not matter).

!!!    maxdx = min(maxdx, (ubounds - lbounds)/2);

        maxdx = min(maxdx, (ubounds - lbounds)*0.5)       ! arrays

!!!    if any(sigma*sqrt(diag(C)) > maxdx)
!!!      fac = min(maxdx ./ sqrt(diag(C)))/sigma;
!!!      sigma = min(maxdx ./ sqrt(diag(C)));
!!!      warning(['Initial SIGMA multiplied by the factor ' num2str(fac) ...
!!!	       ', because it was larger than half' ...
!!!	       ' of one of the boundary intervals']);
!!!    end

        iflag=0
        do i=1,n
          if(sigma*sqrt(c(i,i)).gt.maxdx(i))iflag=1
        end do
        if(SupplyInitialCov)iflag=0  !The code below does not seem to help if there
                                      !is a lot of parameter correlation.
        if(iflag.eq.1)then
          fac=huge(fac)
          do i=1,n
            rtemp=maxdx(i)/sqrt(c(i,i))
            if(rtemp.lt.fac)fac=rtemp
          end do
          fac=fac/sigma
          sigma=huge(sigma)
          do i=1,n
            rtemp=maxdx(i)/sqrt(c(i,i))
            if(rtemp.lt.sigma)sigma=rtemp
          end do
          write(atemp,'(1pg10.3)') fac
          atemp=adjustl(atemp)
          write(amessage,330) trim(atemp)
330       format('Warning: initial SIGMA multiplied by factor of ',a,' because ',  &
          'it was larger than half of one of the boundary intervals.')
          call cmaes_write_warning(6)
          call cmaes_write_warning(outunit1)
        end if

!!!!!! DO WE REALLY NEED TO DO THIS IF BOUNDS ARE VERY WIDE?

!!!    idx = (lbounds > -Inf) & (ubounds < Inf);
!!!    dd = diag(C);
!!!    if any(5*sigma*sqrt(dd(idx)) < ubounds(idx) - lbounds(idx))
!!!      warning(['Initial SIGMA is, in at least one coordinate, ' ...
!!!	       'much smaller than the '...
!!!	       'given boundary intervals. For reasonable ' ...
!!!	       'global search performance SIGMA should be ' ...
!!!	       'between 0.2 and 0.5 of the bounded interval in ' ...
!!!	       'each coordinate. If all coordinates have ' ...
!!!	       'lower and upper bounds SIGMA can be empty']);
!!!    end

        iflag=0
        do i=1,n
          if(5.0*sigma*sqrt(c(i,i)).lt.ubounds(i)-lbounds(i)) iflag=1
        end do
        if(iflag.eq.1)then
          write(amessage,240)
240       format('Warning: initial SIGMA is, in at least one coordinate, much smaller than the ', &
          'given boundary intervals. For reasonable global search performance SIGMA should be ',  &
          'between 0.2 and 0.5 of the bounded interval in each coordinate.')
          call cmaes_write_warning(6)
          call cmaes_write_warning(outunit1)
        end if


!!!    bnd.dfithist = 1;              % delta fit for setting weights
!!!    bnd.aridxpoints = [];          % remember complete outside points
!!!    bnd.arfitness = [];            % and their fitness
!!!    bnd.validfitval = 0;
!!!    bnd.iniphase = 1;
!!!  end

        bnd_dfithist = 1               ! an array
        bnd_validfitval = .false.
        bnd_iniphase = .true.

!xxx      end if                      ! Maybe re-instate later.

!!!  if myevalbool(opts.EvalInitialX)
!!!    fitness.hist(1)=feval(fitfun, xmean, varargin{:});
!!!    fitness.histsel(1)=fitness.hist(1);
!!!    counteval = counteval + 1;
!!!  else
!!!    fitness.hist(1)=NaN;
!!!    fitness.histsel(1)=NaN;
!!!  end

      if((EvalInitialX).or.(maxiter.eq.0))then
        do i=1,n
          arx(i,1)=xmean(i)
        end do
        write(6,241)
241     format(/,' - running model with initial parameter values...')
        call mul_feval(jfail,1,arx,fitness_hist,adjust_flag,num_extra)
        if(jfail.ne.0) go to 9891
        counteval = counteval + 1
        if(fitness_hist(1).le.philo)then
          philo=fitness_hist(1)
          iterlo=countiter
          call record_parameters(jfail,xmean)
          if(jfail.ne.0) go to 9300
          xlo=xmean     ! arrays
          if(ncontrib.gt.1)then
            call get_contribs(phi_contrib,agp_contrib)
          end if
        end if
!        write(outunit1,8010) fitness_hist(1)            !debug
!8010    format(' Function value = ',1pg14.7)            !debug
                                                          ! program to return a NAN value if it fails
        fitness_histsel(1)=fitness_hist(1)
        do i=1,n
          xhist(i,1)=xmean(i)
        end do
      else
        fitness_hist(1)=RNANVALUE
        fitness_histsel(1)=RNANVALUE
      end if
      if(maxiter.eq.0)then
        write(6,*)
        write(6,8160) philo
        write(outunit1,8160) philo
8160    format(t5,'Objective function',t50,'= ',1pg13.6)
        if(ncontrib.gt.1)then
          do i=1,ncontrib
            write(6,8170) trim(agp_contrib(i)),phi_contrib(i)
            write(outunit1,8170) trim(agp_contrib(i)),phi_contrib(i)
8170        format(t5,'Contribution from group "',a,'"',t50,'= ',1pg13.6)
          end do
        end if
        nstopflag=1
        stopflag(1)='OneRunOnly'
        write(amessage,324)
324     format('Maximum number of iterations was supplied as zero. So model run only ', &
        'once using initial parameters.')
        go to 9900
      end if

!!!!!! NOTE: SPECIAL PRECAUTIONS MAY HAVE TO BE TAKEN ON A RESTART THAT WE REGENERATE
!!!!!!       THE PRECEDING SEQUENCE OF RANDOM NUMBERS. SO THESE MAY NEED TO BE COUNTED.

325   continue

! -- The random number generator is now initialized.

      call random_seed(size=seed_size)
      allocate(seed1(seed_size),startseed(seed_size),stat=ierr)
      if(ierr.ne.0)then
        write(amessage,331)
331     format('Error in allocating random number generator seed array in subroutine CMAES_RUN.')
        go to 9890
      end if
      seed1=seed                  ! seed1 is an array
      call random_seed(put=seed1)
      startseed=seed1
      deallocate(seed1)

      if(irestart.ne.2)then

!!!  % Initialize further constants
!!!  chiN=N^0.5*(1-1/(4*N)+1/(21*N^2));  % expectation of
!!!				      %   ||N(0,I)|| == norm(randn(N,1))
!!!  weights = weights/sum(weights);     % normalize recombination weights array

        chin=sqrt(float(N))*(1.0-1.0/(4.0*float(N))+1.0/(21.0*float(N)*float(N)))
        weights = weights/sum(weights)

        countiter = 0
        outiter=0
        iterlo=0

      end if

!!!% -------------------- Generation Loop --------------------------------

     if(irestart.eq.2)then
       countiter=countiter-1
       do i=1,countnorm
         rtemp=cmaes_gennor(0.0,1.0)
       end do
       irestart=1
     end if

!!!while isempty(stopflag)

     do while (all(stopflag.eq.' '))

!!!  countiter = countiter + 1;
!!!  flush;
!!!  % Generate and evaluate lambda offspring

       countiter=countiter+1

! -- Restart data is stored.

       if(irestart.eq.1)then
         call cmaes_dump_all_data(ifail)
         if(ifail.ne.0) go to 9890
       end if

! -- Data is recorded to the screen and run record file.

       call cmaes_writint(anum,countiter)
       write(6,8120) trim(anum)
       write(outunit1,8120) trim(anum)
8120   format(/,/,' ITERATION NUMBER ',A)
       call cmaes_writint(anum,counteval)
       write(6,8121) trim(anum)
       write(outunit1,8121) trim(anum)
8121   format(t5,'Number of function evaluations so far ',t50,'=   ',a)
       if((countiter.gt.1).or.                                &
         ((countiter.eq.1).and.(evalinitialx)))then
         if((philo.lt.1.233e30).or.(philo.gt.1.235e30))then
           write(6,8122) philo
           write(outunit1,8122) philo
8122       format(t5,'Lowest objective function so far',t50,'= ',1pg13.6)
         else
           write(6,8119)
           write(outunit1,8119)
8119       format(t5,'Lowest objective function so far',t50,'= model_run_failure')
         end if
         if(ncontrib.gt.1)then
           do i=1,ncontrib
           if((phi_contrib(i).lt.1.233e30).or.(phi_contrib(i).gt.1.235e30))then
             write(6,8124) trim(agp_contrib(i)),phi_contrib(i)
             write(outunit1,8124) trim(agp_contrib(i)),phi_contrib(i)
8124         format(t5,'Contribution from group "',a,'"',t50,'= ',1pg13.6)
           else
             write(6,8118) trim(agp_contrib(i))
             write(outunit1,8118) trim(agp_contrib(i))
8118         format(t5,'Contribution from group "',a,'"',t50,'= model_run_failure')
           end if
           end do
         end if
         call cmaes_writint(anum,iterlo)
         write(6,8123) trim(anum)
         write(outunit1,8123) trim(anum)
8123     format(t5,'Iteration number of lowest phi so far',t50,'=   ',a)
       end if

#ifdef FLUSHFILE
       call flush(outunit1)
#endif

!!!  fitness.raw = repmat(NaN, 1, lambda);

       fitness_raw=RNANVALUE                     ! an array

!!!  % non-parallel evaluation and remaining NaN-values
!!!  for k=find(isnan(fitness.raw)),
!!!    % fitness.raw(k) = NaN;
!!!    tries = 0;
!!!    % Resample, until fitness is not NaN
!!!    while isnan(fitness.raw(k))
!!!      arz(:,k) = randn(N,1);
!!!      arx(:,k) = xmean + sigma * (BD * arz(:,k));                % Eq. (1)

!!!      % You may handle constraints here. You may either resample
!!!      % arz(:,k) and/or multiply it with a factor between -1 and 1
!!!      % (the latter will decrease the overall step size) and
!!!      % recalculate arx accordingly. Do not change arx or arz in any
!!!      % other way.

!!!      if ~bnd.isactive
!!!        arxvalid(:,k) = arx(:,k);
!!!      else
!!!        arxvalid(:,k) = xintobounds(arx(:,k), lbounds, ubounds);
!!!      end
!!!      % You may handle constraints here.  You may copy and alter
!!!      % (columns of) arxvalid(:,k) only for the evaluation of the
!!!      % fitness function. arx and arxvalid should not be changed.
!!!      fitness.raw(k) = feval(fitfun, arxvalid(:,k), varargin{:});
!!!      tries = tries + 1;
!!!      if isnan(fitness.raw(k))
!!!	countevalNaN = countevalNaN + 1;
!!!      end
!!!      if mod(tries, 10) == 0
!!!	warning(['(Another) 10 NaN objective function values at evaluation ' ...
!!!		num2str(counteval)]);
!!!      end
!!!    end
!!!    counteval = counteval + 1; % retries due to NaN are not counted
!!!  end

       do k=1,lambda
         do i=1,n
           arz(i,k)=cmaes_gennor(0.0,1.0)
           countnorm=countnorm+1
         end do
         do i=1,n
           rtemp=0.0
           do j=1,n
             rtemp=rtemp+bd(i,j)*arz(j,k)
           end do
           arx(i,k)=rtemp*sigma+xmean(i)
         end do
         do i=1,n
           arxvalid(i,k)=arx(i,k)
           if(arxvalid(i,k).gt.ubounds(i)) arxvalid(i,k)=ubounds(i)
           if(arxvalid(i,k).lt.lbounds(i)) arxvalid(i,k)=lbounds(i)
         end do
       end do
       call mul_feval(jfail,lambda,arxvalid,fitness_raw,adjust_flag,num_extra)
       if((jfail.eq.-2).or.(jfail.eq.-1))then
         nstopflag=nstopflag+1
         if(nstopflag.le.MAXSTOPFLAG)then
           stopflag(nstopflag)='userstp'
         else
           nstopflag=nstopflag-1
         end if
         if(nstopflag.eq.1)then
           if(jfail.eq.-2)then
             write(amessage,409)
409          format('User initiated termination with final model run using optimised parameters.')
           else
             write(amessage,408)
408          format('User initiated termination.')
           end if
         end if
         jfail=0
         go to 9900
       else if(jfail.ne.0) then
         go to 9891
       end if
       counteval=counteval+k+num_extra
       if(adjust_flag.ne.0)then
         do i=1,n
           arx(i,adjust_flag)=arxvalid(i,adjust_flag)
         end do
         do i=1,n
           rwork(i)=(arx(i,adjust_flag)-xmean(i))/sigma
         end do
         do i=1,n
           rtemp=0.0
           do j=1,n
             rtemp=rtemp+b(j,i)*rwork(j)
           end do
           if(d(i).ne.0.0)then
             rtemp=rtemp/d(i)
             if(rtemp.gt.3.0) rtemp=3.0
             if(rtemp.lt.-3.0) rtemp=-3.0
           else
             if(arx(i,adjust_flag).gt.xmean(i))then
               rtemp=3.0
             else
               rtemp=-3.0
             end if
           end if
           arz(i,adjust_flag)=rtemp
         end do
       end if

!       do k=1,lambda                               !debug
!         write(outunit1,8010) fitness_raw(k)       !debug
!       end do                                      !debug

       iflag=0
       do k=1,lambda
         if(fitness_raw(k).le.philo)then
           philo=fitness_raw(k)
           iterlo=countiter
           iflag=k
         end if
       end do
       if(iflag.ne.0)then
         call record_parameters(jfail,arxvalid(:,iflag))
         if(jfail.ne.0) go to 9300
         do i=1,n
           xlo(i)=arxvalid(i,iflag)
         end do
         if(ncontrib.gt.1)then
           call get_contribs(phi_contrib,agp_contrib)
         end if
       end if

!!!  fitness.sel = fitness.raw;

       fitness_sel = fitness_raw                    ! arrays
       do i=1,lambda
         rwork1(i)=fitness_raw(i)
       end do
       call cmaes_mysort(lambda,rwork1,iwork)

!!!  % ----- handle boundaries -----
!!!  if 1 < 3 && bnd.isactive
!!!    % Get delta fitness values
!!!    val = myprctile(fitness.raw, [25 75]);
!!!    val = (val(2) - val(1)) / N / mean(diag(C)) / sigma^2;
!!!    %val = (myprctile(fitness.raw, 75) - myprctile(fitness.raw, 25)) ...
!!!    %    / N / mean(diag(C)) / sigma^2;

       if(bnd_isactive)then
         rtemp1=myprctile_sorted(lambda,rwork1,25.0)
         rtemp2=myprctile_sorted(lambda,rwork1,75.0)
         val=(rtemp2-rtemp1)/float(n)/sigma/sigma
         rtemp=0.0
         do i=1,n
           rtemp=rtemp+c(i,i)
         end do
         rtemp=rtemp/float(n)
         val=val/rtemp

!         write(outunit1,*) '-------------start bounds----------'   !debug
!         write(outunit1,*) ' rwork1 ----->'                   !debug
!         do i=1,popsize                                      !debug
!           write(outunit1,*) rwork1(i)                        !debug
!         end do                                              !debug
!         write(outunit1,*) 'rtemp1 rtemp2'                   !debug
!         write(outunit1,*) rtemp1,rtemp2                     !debug

!!!    % Catch non-sensible values
!!!    if ~isfinite(val)
!!!      warning('Non-finite fitness range');
!!!      val = max(bnd.dfithist);
!!!    elseif val == 0 % happens if all points are out of bounds
!!!      val = min(bnd.dfithist(bnd.dfithist>0));
!!!    elseif bnd.validfitval == 0 % first sensible val
!!!      bnd.dfithist = [];
!!!      bnd.validfitval = 1;
!!!    end

         if((val.gt.1.0e35).or.(val.lt.-1.0e35))then
           write(amessage,410)
410        format('Warning: non-finite fitness range.')
           call cmaes_write_warning(6)
           call cmaes_write_warning(outunit1)
           val=maxval(bnd_dfithist)
         else if (val.eq.0.0)then
           val=minval(bnd_dfithist,(bnd_dfithist.gt.0.0))
         else if(.not.bnd_validfitval)then
           bnd_dfithist=RNANVALUE               !  I am using this as the sign of an 'empty matrix'
           bnd_validfitval=.true.
         end if

!!!    % Keep delta fitness values
!!!    if length(bnd.dfithist) < 20+(3*N)/lambda
!!!      bnd.dfithist = [bnd.dfithist val];
!!!    else
!!!      bnd.dfithist = [bnd.dfithist(2:end) val];
!!!    end

         i=size(bnd_dfithist)
         if(bnd_dfithist(i).lt.RNANTHRESH)then
           do j=1,i
             if(bnd_dfithist(j).lt.RNANTHRESH)then
               bnd_dfithist(j)=val
               go to 430
             end if
           end do
430        continue
         else
           do j=1,i-1
             bnd_dfithist(j)=bnd_dfithist(j+1)
           end do
           bnd_dfithist(i)=val
         end if

!!!    % Scale weights anew, bias scaling to unity
!!!    if 1 < 3
!!!      bnd.weights = bnd.scale .* bnd.weights;  % reset scaling

         do i=1,n
           bnd_weights(i)=bnd_scale(i)*bnd_weights(i)
         end do

!!!      bnd.scale = exp(0.1*mean(log(diag(C)))) * diag(C).^0.9;

         rtemp=0.0
         do i=1,n
           rtemp=rtemp+log(c(i,i))
         end do
         rtemp=rtemp/float(n)
         do i=1,n
           bnd_scale(i)=exp(0.1*rtemp)*c(i,i)**0.9
         end do


!!!      bnd.scale = bnd.scale / exp(mean(log(bnd.scale))); % prod is 1 initially

         rtemp=0.0
         do i=1,n
           rtemp=rtemp+log(bnd_scale(i))
         end do
         rtemp=rtemp/float(n)
         do i=1,n
           bnd_scale(i)=bnd_scale(i)/exp(rtemp)
         end do

!!!      bnd.weights = bnd.weights ./ bnd.scale;

         do i=1,n
           bnd_weights(i)=bnd_weights(i)/bnd_scale(i)
         end do

!!!    end

!!!    [tx ti]  = xintobounds(xmean, lbounds, ubounds);

         tx=xmean   ! arrays
         ti=0       ! an array
         do i=1,n
           if(tx(i).lt.lbounds(i)) then
             tx(i)=lbounds(i)
             ti(i)=1
           else if(tx(i).gt.ubounds(i))then
             tx(i)=ubounds(i)
             ti(i)=1
           end if
         end do



!!!    % Set initial weights
!!!    if bnd.iniphase
!!!      if any(ti)
!!!        bnd.weights(find(bnd.isbounded)) = ...
!!!          2.0002 * median(bnd.dfithist) ./ bnd.scale(find(bnd.isbounded));
!!!	if bnd.validfitval && countiter > 2
!!!          bnd.iniphase = 0;
!!!	end
!!!      end
!!!    end


         if(bnd_iniphase)then
           if(any(ti.ne.0))then
             rtemp=cmaes_median1(bnd_dfithist)
             do i=1,n
               bnd_weights(i)=2.0002*rtemp/bnd_scale(i)
             end do
             if((bnd_validfitval).and.(countiter.gt.2))then
               bnd_iniphase=.false.
             end if
           end if
         end if


!!!    % Increase/decrease weights
!!!    if  1 < 3 && any(ti) % any coordinate of xmean out of bounds
!!!      % judge distance of xmean to boundary
!!!      tx = xmean - tx;
!!!      idx = (ti ~= 0 & abs(tx) > 3*max(1,sqrt(N)/mueff) ...
!!!	     * sigma*sqrt(diag(C))) ;
!!!      if ~isempty(idx) % increase
!!!	bnd.weights(idx) = 1.1^(max(1, mueff/10/N)) * bnd.weights(idx);
!!!      end
!!!    end


         if(any(ti.ne.0))then
           tx=xmean-tx           ! arrays
           iwork=0               ! an array
           iflag=0
           do i=1,n
!             if((ti(i).ne.0).and.            &
!                (abs(tx(i)).gt.3.0*max(1.0,sqrt(float(n))/mueff)*sigma*sqrt(c(i,i)))) then
              rtempm=float(n)
              rtempm=sqrt(rtempm)
              rtempm=rtempm/mueff
              rtempm=rtempm*sigma*sqrt(c(i,i))
              rtempm=max(1.0,rtempm)
              if((ti(i).ne.0).and.(abs(tx(i)).gt.3.0*rtempm)) then
               iwork(i)=1
               iflag=1
             end if
           end do
           if(iflag.ne.0)then
             do i=1,n
               if(iwork(i).ne.0)then
!                 bnd_weights(i)=1.1**(max(1.0,mueff/10.0/float(n)))*bnd_weights(i)
                 rtempm=float(n)
                 rtempm=mueff/10.0/rtempm
                 rtempm=max(1.0,rtempm)
                 bnd_weights(i)=1.1**rtempm*bnd_weights(i)
               end if
             end do
           end if
         end if

!!!    % Assigned penalized fitness
!!!    bnd.arpenalty = bnd.weights' * (arxvalid - arx).^2;

!!!    fitness.sel = fitness.raw + bnd.arpenalty;

         do i=1,lambda      ! is this right
           rtemp=0.0
           do j=1,n
             rtemp=rtemp+bnd_weights(j)*(arxvalid(j,i)-arx(j,i))**2.0
           end do
           fitness_sel(i)=fitness_raw(i)+rtemp
         end do

!         write(outunit1,*) ' bnd_weights--->'   !debug
!         do i=1,n                               !debug
!           write(outunit1,*) bnd_weights(i)     !debug
!         end do                                 !debug
!         write(outunit1,*) ' fitness_raw--->'  !debug
!         do i=1,lambda                             !debug
!           write(outunit1,*) fitness_raw(i)         !debug
!         end do                                    !debug
!         write(outunit1,*) ' fitness-sel--->'      !debug
!         do i=1,lambda                              !debug
!           write(outunit1,*) fitness_sel(i)         !debug
!         end do                                     !debug

!         write(outunit1,*) '-------------end bounds---------------' !debug


!!!  end % handle boundaries

       end if
!!!  % ----- end handle boundaries -----

!!!  % Sort by fitness
!!!  [fitness.raw, fitness.idx] = sort(fitness.raw);

       call cmaes_mysort(lambda,fitness_raw,fitness_idx)

!!  [fitness.sel, fitness.idxsel] = sort(fitness.sel);   % minimization

       call cmaes_mysort(lambda,fitness_sel,fitness_idxsel)

       if((fitness_raw(1).gt.1.235e30).or.(fitness_raw(1).lt.1.233e30))then       
         write(6,8140) fitness_raw(1)
         write(outunit1,8140) fitness_raw(1)
8140     format(/,t5,'Lowest  phi this iteration',t50' = ',1pg13.6)
       else
         write(6,81401)        
         write(outunit1,81401) 
81401    format(/,t5,'Lowest  phi this iteration',t50,' = model_run_failure')         
       end if
       if((fitness_raw(popsize).gt.1.235e30).or.(fitness_raw(popsize).lt.1.233e30))then              
         write(6,8150) fitness_raw(popsize)
         write(outunit1,8150) fitness_raw(popsize)
8150     format(  t5,'Highest phi this iteration',t50,' = ',1pg13.6)
       else
         write(6,81501)
         write(outunit1,81501)         
81501    format(  t5,'Highest phi this iteration',t50,' = model_run_failure')
       end if
       if(adjust_flag.ne.0)then
         write(6,8151)
         write(outunit1,8151)
8151     format(t5,'Best random param set this iteration replaced by SVD-determined param set.')
       end if

       phi_index=phi_index+1
       if(phi_index.gt.nphimaxmin)phi_index=1
       if(fitness_raw(1).ne.0.0)then
         phimaxminreldiff(phi_index)=abs((fitness_raw(popsize)-fitness_raw(1))/fitness_raw(1))
       else
         rtemp=0.5*(fitness_raw(1)+fitness_raw(popsize))
         if(rtemp.eq.0.0)then
           phimaxminreldiff(phi_index)=0.0
         else
           phimaxminreldiff(phi_index)=abs((fitness_raw(popsize)-fitness_raw(1))/rtemp)
         end if
       end if

!       write(outunit1,*)      !debug
!       write(outunit1,*) ' fitness_idx----->'   !debug
!       do i=1,lambda                               !debug
!         write(outunit1,*) fitness_idx(i)       !debug
!       end do                                      !debug
!       write(outunit1,*) ' fitness_idxsel----->'   !debug
!       do i=1,lambda                               !debug
!         write(outunit1,*) fitness_idxsel(i)       !debug
!       end do                                      !debug

!!!  fitness.hist(2:end) = fitness.hist(1:end-1);    % record short history of
!!!  fitness.hist(1) = fitness.raw(1);               % best fitness values

       do i=size(fitness_hist),2,-1
         fitness_hist(i)=fitness_hist(i-1)
       end do
       fitness_hist(1)=fitness_raw(1)

!!!  fitness.histsel(2:end) = fitness.histsel(1:end-1);    % record short history of
!!!  fitness.histsel(1) = fitness.sel(1);               % best fitness values

       do i=size(fitness_histsel),2,-1
         fitness_histsel(i)=fitness_histsel(i-1)
       end do
       fitness_histsel(1)=fitness_sel(1)

!!!  % Calculate new xmean, this is selection and recombination

!!!  xold = xmean; % for speed up of Eq. (2) and (3)

       xold=xmean    ! arrays

!!!  xmean = arx(:,fitness.idxsel(1:mu))*weights;
!!!  zmean = arz(:,fitness.idxsel(1:mu))*weights;%==D^-1*B'*(xmean-xold)/sigma

       do i=1,n
         rtemp1=0.0
         rtemp2=0.0
         do j=1,mu
           k=fitness_idxsel(j)
           rtemp1=rtemp1+arx(i,k)*weights(j)
           rtemp2=rtemp2+arz(i,k)*weights(j)
         end do
         xmean(i)=rtemp1
         zmean(i)=rtemp2
       end do

       do j=nrelpar+1,2,-1
         do i=1,n
           xhist(i,j)=xhist(i,j-1)
         end do
       end do
       do i=1,n
         xhist(i,1)=xmean(i)
       end do

!       write(outunit1,*)                    !debug
!       write(outunit1,*) ' xmean --->'      !debug
!       do i=1,n                             !debug
!         write(outunit1,*) xmean(i)         !debug
!       end do                               !debug
!       write(outunit1,*)                    !debug
!       write(outunit1,*) ' zmean --->'      !debug
!       do i=1,n                             !debug
!         write(outunit1,*) zmean(i)         !debug
!       end do                               !debug

!!!  if mu == 1
!!!    fmean = fitness.sel(1);
!!!  else
!!!    fmean = NaN; % [] does not work in the latter assignment
!!!    % fmean = feval(fitfun, xintobounds(xmean, lbounds, ubounds), varargin{:});
!!!    % counteval = counteval + 1;
!!!  end

       if(mu.eq.1)then
         fmean=fitness_sel(1)
       else
         fmean=RNANVALUE
       end if

!!!  % Cumulation: update evolution paths
!!!  ps = (1-cs)*ps + (sqrt(cs*(2-cs)*mueff)) * (B*zmean);          % Eq. (4)

       do i=1,n
         rtemp=0.0
         do j=1,n
           rtemp=rtemp+b(i,j)*zmean(j)
         end do
         rwork(i)=rtemp
       end do
       rtemp=sqrt(cs*(2.0-cs)*mueff)
       do i=1,n
         ps(i)=(1.0-cs)*ps(i)+rtemp*rwork(i)
       end do


!!!  hsig = norm(ps)/sqrt(1-(1-cs)^(2*countiter))/chiN < 1.5 + 1/(N-0.5);
!!!%  hsig = norm(ps) < 1.5 * sqrt(N);
!!!%  hsig = 1;

       rtemp=0.0
       do i=1,n
         rtemp=rtemp+ps(i)*ps(i)
       end do
       rtemp=sqrt(rtemp)
       rtemp1=rtemp/sqrt(1.0-(1.0-cs)**(2.0*float(countiter)))/chin
       rtemp2=1.5+1.0/(float(n)-0.5)
       if(rtemp1.lt.rtemp2)then
         hsig=1.0
       else
         hsig=0.0
       end if

!!!  pc = (1-cc)*pc ...
!!!        + hsig*(sqrt(cc*(2-cc)*mueff)/sigma) * (xmean-xold);     % Eq. (2)
!!!  if hsig == 0
!!!    %disp([num2str(countiter) ' ' num2str(counteval) ' pc update stalled']);
!!!  end

       rtemp=hsig*(sqrt(cc*(2.0-cc)*mueff)/sigma)
       do i=1,n
         pc(i)=(1.0-cc)*pc(i)+rtemp*(xmean(i)-xold(i))
       end do

!!!  % Adapt covariance matrix
!!!  if ccov > 0                                                    % Eq. (3)
!!!    C = (1-ccov+(1-hsig)*ccov*cc*(2-cc)/mucov) * C ... % regard old matrix
!!!        + ccov * (1/mucov) * pc*pc' ...                % plus rank one update
!!!        + ccov * (1-1/mucov) ...                       % plus rank mu update
!!!          * sigma^-2 * (arx(:,fitness.idxsel(1:mu))-repmat(xold,1,mu)) ...
!!!          * diag(weights) * (arx(:,fitness.idxsel(1:mu))-repmat(xold,1,mu))';
!!!  end

       if(ccov.gt.0.0)then

!         write(outunit1,*)                           !debug
!         write(outunit1,*) ' Computing new C...'     !debug
!         write(outunit1,*) ' IDXSEL'                 !debug
!         do i=1,mu                                   !debug
!           write(outunit1,*) fitness_idxsel(i)                !debug
!         end do                                      !debug
         do i=1,n
           do j=1,mu
             k=fitness_idxsel(j)
             cwork1(i,j)=arx(i,k)-xold(i)
           end do
         end do
!         write(outunit1,*)                  !debug
!         write(outunit1,*) 'cwork1 ---->'   !debug
!         do i=1,n                           !debug
!           write(outunit1,8111) (cwork1(i,j),j=1,mu)   !debug
!8111       format(8(1x,1pg12.5))                       !debug
!         end do                             !debug

         do i=1,n
           do j=1,n
             rtemp=0.0
             do k=1,mu
               rtemp=rtemp+cwork1(i,k)*weights(k)*cwork1(j,k)
             end do
             cwork2(i,j)=rtemp
           end do
         end do

!         write(outunit1,*)                  !debug
!         write(outunit1,*) 'cwork2 ---->'   !debug
!         do i=1,n                           !debug
!           write(outunit1,8111) (cwork2(i,j),j=1,n)   !debug
!         end do                             !debug

         rtemp1=(1.0-ccov+(1.0-hsig)*ccov*cc*(2.0-cc)/mucov)
         rtemp2=ccov*(1.0/mucov)
         rtemp3=ccov*(1.0-1.0/mucov)/sigma/sigma

!         write(outunit1,*)     !debug
!         write(outunit1,*)  'rtemp1 = ',rtemp1   !debug
!         write(outunit1,*)  'rtemp2 = ',rtemp2   !debug
!         write(outunit1,*)  'rtemp3 = ',rtemp3   !debug
!         write(outunit1,*)                  !debug
!         write(outunit1,*) 'c before adjustment ---->'   !debug
!         do i=1,n                           !debug
!           write(outunit1,8111) (c(i,j),j=1,n)   !debug
!         end do                             !debug

         do i=1,n
           do j=1,n
             c(i,j)=rtemp1*c(i,j)+rtemp2*pc(i)*pc(j)+rtemp3*cwork2(i,j)
           end do
         end do

!         write(outunit1,*) 'c after adjustment ---->'   !debug
!         do i=1,n                           !debug
!           write(outunit1,8111) (c(i,j),j=1,n)   !debug
!         end do                             !debug

       end if
!!!  if 1 < 2 && ~flgscience
!!!    % remove momentum in ps, if ps is large and fitness is getting worse.
!!!    % this should rarely happen.
!!!    % this might be questionable in dynamic environments
!!!    if sum(ps.^2)/N > 1.5 + 10*(2/N)^.5 && ...
!!!        fitness.histsel(1) > max(fitness.histsel(2:3))
!!!      ps = ps * sqrt(N*(1+max(0,log(sum(ps.^2)/N))) / sum(ps.^2));
!!!      if flgdisplay
!!!        disp(['Momentum in ps removed at [niter neval]=' ...
!!!              num2str([countiter counteval]) ']']);
!!!      end
!!!    end
!!!  end

       if(.not.flgscience)then
         rtemp=0.0
         do i=1,n
           rtemp=rtemp+ps(i)*ps(i)
         end do
         rtemp=rtemp/float(n)
         if((fitness_histsel(1).gt.RNANTHRESH).and.  &
            (fitness_histsel(2).gt.RNANTHRESH).and.  &
            (fitness_histsel(3).gt.RNANTHRESH))then
           if((rtemp.gt.1.5+10*sqrt(2.0/float(n))).and.  &
              (fitness_histsel(1).gt.max(fitness_histsel(2),fitness_histsel(3))))then
               rtemp1=sqrt(float(n)*(1.0+max(0.0,log(rtemp)))/(rtemp*float(n)))
              do i=1,n
                ps(i)=ps(i)*rtemp1
              end do
              call cmaes_writint(atemp1,countiter)
              call cmaes_writint(atemp2,counteval)
              write(amessage,510) trim(atemp1),trim(atemp2)
510           format('Momentum in ps removed at [niter neval] = [',a,',',a,'].')
              call cmaes_write_warning(6)
              call cmaes_write_warning(outunit1)
           end if
         end if
       end if

!!!  % Adapt sigma
!!!  sigma = sigma * exp((norm(ps)/chiN - 1)*cs/damps);             % Eq. (5)

       rtemp=0.0
       do i=1,n
         rtemp=rtemp+ps(i)*ps(i)
       end do
       rtemp=sqrt(rtemp)
       sigma=sigma * exp((rtemp/chin-1.0)*cs/damps)

!!!  % Update B and D from C
!!!  if ccov > 0 && mod(countiter, 1/ccov/N/10) < 1

       if(ccov.gt.0.0)then
         rtemp=float(countiter)
         if(mod(rtemp,(1.0/ccov/float(n)/10.0)).lt.1.0)then    ! Should this be done every iteration for big models?

!!!    C=triu(C)+triu(C,1)'; % enforce symmetry

           if(n.gt.1)then
             do i=1,n-1
               do j=i+1,n
                 rtemp=(c(i,j)+c(j,i))*0.5
                 c(i,j)=rtemp
                 c(j,i)=rtemp
               end do
             end do
           end if

!!!    [B,D] = eig(C);       % eigen decomposition, B==normalized eigenvectors


!           write(outunit1,8030)                          !debug
!8030       format(' - undertaking SVD')                  !debug

           cwork2=c                ! arrays
           call sgesvd('A','N',n,n,cwork2,n,d,b,n,vt,ldvt,rwork,lwork,info)
           if(info.ne.0)then
             write(amessage,520)
520          format('Error in subroutine CSAEM_RUN: cannot undertake SVD on C matrix.')
             go to 9890
           end if

           if(any(abs(d).gt.1.0e35))then
             write(amessage,530)
530          format('Error in function CMAES_RUN: function SGESVD returned ',   &
             'non-finite eigenvalues.')
!!!!!!           call cmaes_save_run_data(jfail,'tmp')
             go to 9890
           end if

!!!    if any(any(~isfinite(diag(B))))
!!!      save(['tmp' opts.SaveFileName]);
!!!      error(['function eig returned non-finited eigenvectors, cond(C)=' ...
!!!	     num2str(cond(C)) ]);
!!!    end

!!!    % limit condition of C to 1e14 + 1
!!!    if min(diag(D)) <= 0
!!!	if stopOnWarnings
!!!	  stopflag(end+1) = {'warnconditioncov'};
!!!	else
!!!	  warning(['Iteration ' num2str(countiter) ...
!!!		   ': Eigenvalue (smaller) zero']);
!!!	  D(D<0) = 0;
!!!	  tmp = max(diag(D))/1e14;
!!!	  C = C + tmp*eye(N,N); D = D + tmp*eye(N,N);
!!!	end
!!!    end


           if(minval(d).le.0.0)then
             if(stoponwarnings)then
               nstopflag=nstopflag+1
               if(nstopflag.le.MAXSTOPFLAG) then
                 stopflag(nstopflag)='warnconditioncov'
               else
                 nstopflag=nstopflag-1
               end if
               if(nstopflag.eq.1)then
                 write(amessage,539)
539              format('At least one eigenvalue of CMAES "C" matrix is less than ', &
                 'or equal to zero.')
               end if
             else
               call cmaes_writint(atemp,countiter)
               write(amessage,540) trim(atemp)
540            format('Warning: on iteration ',a,'at least one eigenvalue is less than ',  &
               'or equal to zero.')
               call cmaes_write_warning(6)
               call cmaes_write_warning(outunit1)
               rtemp=maxval(d)/1.0e14
               do i=1,n
                 if(d(i).le.0.0) d(i)=0.0
                 c(i,i)=c(i,i)+rtemp
                 d(i)=d(i)+rtemp
               end do
             end if
           end if

!!!    if max(diag(D)) > 1e14*min(diag(D))
!!!	if stopOnWarnings
!!!	  stopflag(end+1) = {'warnconditioncov'};
!!!	else
!!!	  warning(['Iteration ' num2str(countiter) ': condition of C ' ...
!!!		   'at upper limit' ]);
!!!	  tmp = max(diag(D))/1e14 - min(diag(D));
!!!	  C = C + tmp*eye(N,N); D = D + tmp*eye(N,N);
!!!	end
!!!    end

           if(maxval(d).gt.1.0e14*minval(d))then
             if(stoponwarnings)then
               nstopflag=nstopflag+1
               if(nstopflag.le.MAXSTOPFLAG)then
                 stopflag(nstopflag)='warnconditioncov'
               else
                 nstopflag=nstopflag-1
               end if
               if(nstopflag.eq.1)then
                 write(amessage,549)
549              format('Condition number of CMAES "C" matrix at upper limit.')
               end if
             else
               call cmaes_writint(atemp,countiter)
               write(amessage,550) trim(atemp)
550            format('Iteration ',a,': condition number of C at upper limit.')
               call cmaes_write_warning(6)
               call cmaes_write_warning(outunit1)
               rtemp=maxval(d)/1.0e14-minval(d)
               do i=1,n
                 c(i,i)=c(i,i)+rtemp
                 d(i)=d(i)+rtemp
               end do
             end if
           end if

!!!    D = diag(sqrt(diag(D))); % D contains standard deviations now

           do i=1,n
             d(i)=sqrt(d(i))
           end do

!!!    % D = D / prod(diag(D))^(1/N);  C = C / prod(diag(D))^(2/N);
!!!    BD = B*D; % for speed up only

           do i=1,n
             do j=1,n
               bd(i,j)=b(i,j)*d(j)
             end do
           end do

!!!  end % if mod

         end if
       end if

!!!  % ----- numerical error management -----
!!!  % Adjust maximal coordinate axis deviations
!!!  if any(sigma*sqrt(diag(C)) > maxdx)
!!!    sigma = min(maxdx ./ sqrt(diag(C)));
!!!    %warning(['Iteration ' num2str(countiter) ': coordinate axis std ' ...
!!!    %         'deviation at upper limit of ' num2str(maxdx)]);
!!!    % stopflag(end+1) = {'maxcoorddev'};
!!!  end

       do i=1,n
         if(sqrt(c(i,i))*sigma.gt.maxdx(i)) then
           rtemp=huge(rtemp)
           do j=1,n
             rtemp1=maxdx(j)/sqrt(c(j,j))
             if(rtemp1.lt.rtemp)rtemp=rtemp1
           end do
           sigma=rtemp
           go to 560
         end if
       end do
560    continue

!!!  % Adjust minimal coordinate axis deviations
!!!  while any(sigma*sqrt(diag(C)) < mindx)
!!!    sigma = max(mindx ./ sqrt(diag(C))) * exp(0.05+cs/damps);
!!!    %warning(['Iteration ' num2str(countiter) ': coordinate axis std ' ...
!!!    %         'deviation at lower limit of ' num2str(mindx)]);
!!!    % stopflag(end+1) = {'mincoorddev'};;
!!!  end

569    continue
       do i=1,n
         if(sigma*sqrt(c(i,i)).lt.mindx(i))then
           rtemp=0.0
           do j=1,n
             rtemp1=mindx(j)/sqrt(c(j,j))
             if(rtemp1.gt.rtemp)rtemp=rtemp1
           end do
           sigma=rtemp*exp(0.05*cs/damps)
           go to 569
         end if
       end do
570    continue

!!!  % Adjust too low coordinate axis deviations
!!!  if any(xmean == xmean + 0.2*sigma*sqrt(diag(C)))
!!!    if stopOnWarnings
!!!	stopflag(end+1) = {'warnnoeffectcoord'};
!!!    else
!!!      warning(['Iteration ' num2str(countiter) ': coordinate axis std ' ...
!!!	       'deviation too low' ]);
!!!	C = C + ccov * diag(diag(C) .* ...
!!!			    (xmean == xmean + 0.2*sigma*sqrt(diag(C))));
!!!	sigma = sigma * exp(0.05+cs/damps);
!!!    end
!!!  end

       iflag=0
       do i=1,n
         if(xmean(i).eq.xmean(i)+0.2*sigma*sqrt(c(i,i)))then
           if(stoponwarnings) then
             nstopflag=nstopflag+1
             if(nstopflag.le.MAXSTOPFLAG)then
               stopflag(nstopflag)='warnnoeffectcoord'
             else
               nstopflag=nstopflag-1
             end if
             if(nstopflag.eq.1)then
               write(amessage,579)
579            format('The CMAES coordinate axis standard deviation is too low.')
             end if
           else
             iflag=iflag+1
             if(iflag.eq.1)then
               call cmaes_writint(atemp,countiter)
               write(amessage,580) trim(atemp)
580            format('Warning: on iteration ',a,'the coordinate axis standard ',  &
               'deviation is too low.')
               call cmaes_write_warning(6)
               call cmaes_write_warning(outunit1)
             end if
             c(i,i)=c(i,i)+ccov*c(i,i)
           end if
         end if
       end do
       if(iflag.ne.0)then
         sigma=sigma*exp(0.05*cs/damps)
         iflag=0
       end if

!!!  % Adjust step size in case of (numerical) precision problem
!!!  if all(xmean == xmean ...
!!!	    + 0.1*sigma*BD(:,1+floor(mod(countiter,N))))
!!!    i = 1+floor(mod(countiter,N));
!!!    if stopOnWarnings
!!!	stopflag(end+1) = {'warnnoeffectaxis'};
!!!    else
!!!      warning(['Iteration ' num2str(countiter) ...
!!!	       ': main axis standard deviation ' ...
!!!	       num2str(sigma*D(i,i)) ' has no effect' ]);
!!!	sigma = sigma * exp(0.2+cs/damps);
!!!    end
!!!  end


       j=1+mod(countiter,n)
       do i=1,n
         if(xmean(i).ne.xmean(i)+0.1*sigma*bd(i,j)) go to 600
       end do
       if(stoponwarnings) then
         nstopflag=nstopflag+1
         if(nstopflag.le.MAXSTOPFLAG)then
           stopflag(nstopflag)='warnnoeffectaxis'
         else
           nstopflag=nstopflag-1
         end if
         if(nstopflag.eq.1)then
           write(amessage,589)
589        format('The CMAES main axis standard deviation has no effect.')
         end if
       else
         call cmaes_writint(atemp1,countiter)
!         write(atemp2,'(1pg12.5)') sigma*d(i)
!         atemp2=adjustl(atemp2)
         write(amessage,590) trim(atemp1)
590      format('Warning on iteration ',a,': the main axis standard deviation ',   &
         ' has no effect.')
         call cmaes_write_warning(6)
         call cmaes_write_warning(outunit1)
         sigma=sigma*exp(0.2+cs/damps)
       end if
600    continue


!!!  % Adjust step size in case of equal function values (flat fitness)
!!!  if fitness.sel(1) == fitness.sel(1+ceil(0.1+lambda/4))
!!!    if flgWarnOnEqualFunctionValues && stopOnWarnings
!!!	stopflag(end+1) = {'warnequalfunvals'};
!!!    else
!!!      if flgWarnOnEqualFunctionValues
!!!	warning(['Iteration ' num2str(countiter) ...
!!!		 ': equal function values f=' num2str(fitness.sel(1)) ...
!!!		 ' at maximal main axis sigma ' ...
!!!		 num2str(sigma*max(diag(D)))]);
!!!      end
!!!      sigma = sigma * exp(0.2+cs/damps);
!!!    end
!!!  end

       j=ceiling(0.1+float(lambda)/4.0)
       if((fitness_sel(1).gt.RNANTHRESH).and.(fitness_sel(j).gt.RNANTHRESH))then
         if(fitness_sel(1).eq.fitness_sel(j))then
           if((flgWarnOnEqualFunctionValues).and.(stopOnWarnings))then
             nstopflag=nstopflag+1
             if(nstopflag.le.MAXSTOPFLAG)then
               stopflag(nstopflag)='warnequalfunvals'
             else
               nstopflag=nstopflag-1
             end if
             if(nstopflag.eq.1)then
               write(atemp2,'(1pg12.5)') fitness_sel(1)
               atemp2=adjustl(atemp2)
               write(atemp,'(1pg12.5)') sigma*maxval(d)
               atemp=adjustl(atemp)
               write(amessage,609) trim(atemp2),trim(atemp)
609            format('Equal function values f = ',a,' at main axis signma ',a,'.')
            end if
           else
             if(flgWarnOnEqualFunctionValues)then
               call cmaes_writint(atemp1,countiter)
               write(atemp2,'(1pg12.5)') fitness_sel(1)
               atemp2=adjustl(atemp2)
               write(atemp,'(1pg12.5)') sigma*maxval(d)
               atemp=adjustl(atemp)
               write(amessage,610)
610            format('Warning: equal function values.')
               call cmaes_write_warning(6)
               call cmaes_write_warning(outunit1)
             end if
           end if
           sigma=sigma*exp(0.2+cs/damps)
         end if
       end if

!!!  % Adjust step size in case of equal function values
!!!  if countiter > 2 && myrange([fitness.hist fitness.sel(1)]) == 0
!!!    if stopOnWarnings
!!!	stopflag(end+1) = {'warnequalfunvalhist'};
!!!    else
!!!      warning(['Iteration ' num2str(countiter) ...
!!!	       ': equal function values in history at maximal main ' ...
!!!	       'axis sigma ' num2str(sigma*max(diag(D)))]);
!!!	sigma = sigma * exp(0.2+cs/damps);
!!!    end
!!!  end

       rtemp1=maxval(fitness_hist,fitness_hist.gt.RNANTHRESH)
       rtemp2=minval(fitness_hist,fitness_hist.gt.RNANTHRESH)
       rtemp=fitness_sel(1)
       if(rtemp.gt.rtemp1)rtemp1=rtemp
       if(rtemp.lt.rtemp2)rtemp2=rtemp
       rtemp=rtemp1-rtemp2
       if((countiter.gt.2).and.(rtemp.eq.0.0))then
         if(stoponwarnings) then
           nstopflag=nstopflag+1
           if(nstopflag.le.MAXSTOPFLAG)then
             stopflag(nstopflag)='warnequalfunvalhist'
           else
             nstopflag=nstopflag-1
           end if
           if(nstopflag.eq.1)then
             write(atemp2,'(1pg12.5)') sigma*maxval(d)
             atemp2=adjustl(atemp2)
             write(amessage,619) trim(atemp2)
619          format('There are equal function values in history at maximal main ', &
             'axis sigma ',a,'.')
           end if
         else
           call cmaes_writint(atemp1,countiter)
           write(atemp2,'(1pg12.5)') sigma*maxval(d)
           atemp2=adjustl(atemp2)
           write(amessage,620) trim(atemp1),trim(atemp2)
620        format('Warning: on iteration ',a,'there are equal function values ',  &
           'in history at maximal main axis sigma ',a,'.')
           sigma=sigma*exp(0.2+cs/damps)
         end if
       end if

!!!  % Align scales of sigma and C for nicer output
!!!  if 11 < 2 && (sigma > 1e10*max(diag(D)) || 1e10*sigma < min(diag(D)))
!!!    fac = sqrt(sigma/median(diag(D)));
!!!    sigma = sigma/fac;
!!!    pc = fac * pc;
!!!    C = fac^2 * C;
!!!    D = fac * D;
!!!    BD = fac * BD;
!!!  end

       if((11.lt.2).and.((sigma.gt.1.0e10*maxval(d)).or.(1.0e10*sigma.lt.minval(d))))then
         fac=sqrt(sigma/cmaes_median1(d))
         sigma=sigma/fac
         pc = fac * pc
         C = fac*fac * C         ! C is an array
         D = fac * D             ! D is a vector
         BD = fac * BD           ! BD is an array
       end if

!!!  % ----- end numerical error management -----

!!!  % Set stop flag
!!!  if fitness.raw(1) <= stopFitness, stopflag(end+1) = {'fitness'}; end

       if(philo.eq.0.0)then
         nstopflag=nstopflag+1
         if(nstopflag.le.MAXSTOPFLAG)then
           stopflag(nstopflag)='zerophi'
         else
           nstopflag=nstopflag-1
         end if
         if(nstopflag.eq.1)then
           write(amessage,680)
680        format('Objective function zero.')
         end if
       end if

       if(fitness_raw(1).le.stopfitness) then
         nstopflag=nstopflag+1
         if(nstopflag.le.MAXSTOPFLAG)then
           stopflag(nstopflag)='fitness'
         else
           nstopflag=nstopflag-1
         end if
         if(nstopflag.eq.1)then
           write(amessage,685)
685        format('Objective function less than user-supplied threshold.')
         end if
       end if

!!!  if counteval >= stopMaxFunEvals, stopflag(end+1) = {'maxfunevals'}; end

       if(counteval.ge.MaxFunEvals) then
         nstopflag=nstopflag+1
         if(nstopflag.le.MAXSTOPFLAG)then
           stopflag(nstopflag)='maxfunevals'
         else
           nstopflag=nstopflag-1
         end if
         if(nstopflag.eq.1)then
           write(amessage,690)
690        format('Maximum allowed number of function evaluations reached.')
         end if
       end if

!!!  if countiter >= stopMaxIter, stopflag(end+1) = {'maxiter'}; end

       if(countiter.ge.MaxIter)then
         nstopflag=nstopflag+1
         if(nstopflag.le.MAXSTOPFLAG)then
           stopflag(nstopflag)='maxiter'
         else
           nstopflag=nstopflag-1
         end if
         if(nstopflag.eq.1)then
           write(amessage,695)
695        format('Maximum allowed number of CMAES iterations reached.')
         end if
       end if

!!!  if all(sigma*(max(abs(pc), sqrt(diag(C)))) < stopTolX)
!!!    stopflag(end+1) = {'tolx'};
!!!  end

       do i=1,n
         if(sigma*max(abs(pc(i)),sqrt(c(i,i))).ge.Tolx(i)) go to 750
       end do
       nstopflag=nstopflag+1
       if(nstopflag.le.MAXSTOPFLAG)then
         stopflag(nstopflag)='tolx'
       else
         nstopflag=nstopflag-1
       end if
       if(nstopflag.eq.1)then
         write(amessage,698)
698      format('CMAES "tolx" threshold criterion achieved.')
       end if
750    continue


!!!  if any(sigma*sqrt(diag(C)) > stopTolUpX)
!!!    stopflag(end+1) = {'tolupx'};
!!!  end

       do i=1,n
         if(sigma*sqrt(c(i,i)).gt.TolUpX(i))then
           nstopflag=nstopflag+1
           if(nstopflag.le.MAXSTOPFLAG)then
             stopflag(nstopflag)='tolupx'
           else
             nstopflag=nstopflag-1
           end if
           if(nstopflag.eq.1)then
             write(amessage,755)
755          format('CMAES "tolupx" criterion achieved.')
           end if
           go to 760
         end if
       end do
760    continue

!!!  if sigma*max(diag(D)) == 0  % should never happen
!!!    stopflag(end+1) = {'bug'};
!!!  end

       if(sigma*maxval(d).eq.0.0) then
         nstopflag=nstopflag+1
         if(nstopflag.le.MAXSTOPFLAG)then
           stopflag(nstopflag)='bug'
         else
           nstopflag=nstopflag-1
         end if
         if(nstopflag.eq.1)then
           write(amessage,765)
765        format('CMAES "bug" condition encountered.')
         end if
       end if

!!!  if countiter > 2 && myrange([fitness.sel fitness.hist]) < stopTolFun
!!!    stopflag(end+1) = {'tolfun'};
!!!  end

!       write(outunit1,*)                 !debug
!       write(outunit1,*) ' fitness_sel --->'   !debug
!       do i=1,lambda                           !debug
!         write(outunit1,*) fitness_sel(i)      !debug
!       end do                                  !debug
!       write(outunit1,*) ' fitness_hist --->'   !debug
!       do i=1,lambda                           !debug
!         write(outunit1,*) fitness_hist(i)      !debug
!       end do                                  !debug

       if(countiter.gt.2)then
         rtemp1=minval(fitness_sel,fitness_sel.gt.RNANTHRESH)   ! do this above somewhere as well
         rtemp2=maxval(fitness_sel,fitness_sel.gt.RNANTHRESH)
         rtemp3=minval(fitness_hist,fitness_hist.gt.RNANTHRESH)
         rtemp4=maxval(fitness_hist,fitness_hist.gt.RNANTHRESH)
         rtemp1=min(rtemp1,rtemp3)
         rtemp2=max(rtemp2,rtemp4)
         if(rtemp2-rtemp1.lt.TolFun)then
           nstopflag=nstopflag+1
           if(nstopflag.le.MAXSTOPFLAG)then
             stopflag(nstopflag)='tolfun'
           else
             nstopflag=nstopflag-1
           end if
           if(nstopflag.eq.1)then
             write(amessage,767)
767          format('CMAES "tolfun" criterion achieved - objective function ' &
             'change between iterations too small.')
           end if
         end if
       end if

       rtemp=huge(rtemp)
       do i=1,nphinored+1
         if(fitness_hist(i).gt.RNANTHRESH)then
           if(fitness_hist(i).lt.rtemp)then
             rtemp=fitness_hist(i)
             itemp=i
           end if
         end if
       end do
       if(itemp.eq.nphinored+1)then
         nstopflag=nstopflag+1
         if(nstopflag.le.MAXSTOPFLAG)then
           stopflag(nstopflag)='nphinored'
         else
           nstopflag=nstopflag-1
         end if
         if(nstopflag.eq.1)then
           call cmaes_writint(atemp,nphinored)
           write(amessage,782) trim(atemp)
782        format(a,' iterations have elapsed since an improvement in the ',  &
           'objective function was achieved.')
         end if
       end if

       if(countiter.ge.nphimaxmin)then
         do i=1,nphimaxmin
           if(phimaxminreldiff(i).gt.thresh_phimaxmin) go to 830
         end do
         nstopflag=nstopflag+1
         if(nstopflag.le.MAXSTOPFLAG)then
           stopflag(nstopflag)='phimaxminreldiff'
         else
           nstopflag=nstopflag-1
         end if
         if(nstopflag.eq.1)then
           call cmaes_writint(atemp,nphimaxmin)
           write(amessage,840) trim(atemp)
840        format('Highest-to-lowest relative objective function difference for generated parameter sets below ',  &
           'threshold over ',a,' successive iterations.')
         end if
       end if
830    continue

       if(countiter.ge.nphistp+1)then
         rtemp1=huge(rtemp1)
         rtemp2=-huge(rtemp2)
         do i=1,nphistp+1
           if(fitness_hist(i).lt.rtemp1) rtemp1=fitness_hist(i)
           if(fitness_hist(i).gt.rtemp2) rtemp2=fitness_hist(i)
         end do
         if(rtemp1.ne.0.0)then
           if(abs((rtemp2-rtemp1)/rtemp1).lt.phiredstp)then
             nstopflag=nstopflag+1
             if(nstopflag.le.MAXSTOPFLAG)then
               stopflag(nstopflag)='phiredstp'
             else
               nstopflag=nstopflag-1
             end if
             if(nstopflag.eq.1)then
               write(amessage,783)
783            format('Rate of objective function reduction is below user-supplied threshold.')
             end if
           end if
         end if
       end if

       if(countiter.ge.nrelpar+1)then
         do i=1,n
           rtemp1=huge(rtemp1)
           rtemp2=-huge(rtemp2)
           do j=1,nrelpar+1
             if(xhist(i,j).lt.rtemp1) rtemp1=xhist(i,j)
             if(xhist(i,j).gt.rtemp2) rtemp2=xhist(i,j)
           end do
           if(rtemp1.eq.0.0)then
             if(rtemp2.ne.0.0) go to 763
           else
             if(abs((rtemp2-rtemp1)/rtemp1).gt.relparstp)go to 763
           end if
         end do
         nstopflag=nstopflag+1
         if(nstopflag.le.MAXSTOPFLAG)then
           stopflag(nstopflag)='relparstp'
         else
           nstopflag=nstopflag-1
         end if
         if(nstopflag.eq.1)then
           write(amessage,787)
787        format('Rate of parameter change less than user-supplied threshold.')
         end if
       end if
763    continue

!!!  if counteval >= stopFunEvals || countiter >= stopIter
!!!    stopflag(end+1) = {'stoptoresume'};
!!!    if length(stopflag) == 1 && flgsaving == 0
!!!      error('To resume later the saving option needs to be set');
!!!    end
!!!  end

       if((counteval.ge.stopfunevals).or.(countiter.ge.stopiter))then
         nstopflag=nstopflag+1
         if(nstopflag.le.MAXSTOPFLAG)then
           stopflag(nstopflag)='stoptoresume'
         else
           nstopflag=nstopflag-1
         end if
         if((nstopflag.eq.1).and.(.not.flgsaving))then
           write(amessage,721)
721        format('Error: to resume later the saving options needs to be set.')
           go to 9890
         end if
       end if

       if(mod(countiter,10).eq.0)then        ! Use a variable that allows us to say what out save interval is instead of 10
         outiter=countiter

       end if

!!!  % ----- end output generation -----

!!!end % while, end generation loop

     end do

     go to 9900

9200  write(amessage,9210)
9210  format('Cannot allocate sufficient memory to continue execution.')
      go to 9890

9300  write(amessage,9310)
9310  format('Error in subroutine CMAES_RUN: cannot write best parameter ', &
      'values to parameter value file.')
      go to 9890

9890  ifail=1
      go to 9900

9891  continue
      if(jfail.gt.0)then
        ifail=11                ! indicates that error was in MIO/PRM
      else
        ifail=jfail
      endif
      go to 9900


9900  continue

      if(allocated(maxdx))  deallocate(maxdx,stat=ierr)
      if(allocated(mindx))  deallocate(mindx,stat=ierr)
      if(allocated(xmean))  deallocate(xmean,stat=ierr)
      if(allocated(xold))   deallocate(xold,stat=ierr)
      if(allocated(tolx))   deallocate(tolx,stat=ierr)
      if(allocated(tolupx)) deallocate(tolupx,stat=ierr)
      if(allocated(zmean))  deallocate(zmean,stat=ierr)
      if(allocated(pc))     deallocate(pc,stat=ierr)
      if(allocated(ps))     deallocate(ps,stat=ierr)
      if(allocated(b))      deallocate(b,stat=ierr)
      if(allocated(d))      deallocate(d,stat=ierr)
      if(allocated(bd))     deallocate(bd,stat=ierr)
      if(allocated(bnd_weights)) deallocate(bnd_weights,stat=ierr)
      if(allocated(bnd_scale))   deallocate(bnd_scale,stat=ierr)
      if(allocated(bnd_isbounded)) deallocate(bnd_isbounded,stat=ierr)
      if(allocated(tx))     deallocate(tx,stat=ierr)
      if(allocated(ti))     deallocate(ti,stat=ierr)
      if(allocated(bnd_dfithist)) deallocate(bnd_dfithist,stat=ierr)
      if(allocated(fitness_hist)) deallocate(fitness_hist,stat=ierr)
      if(allocated(fitness_histsel)) deallocate(fitness_histsel,stat=ierr)
      if(allocated(fitness_raw)) deallocate(fitness_raw,stat=ierr)
      if(allocated(fitness_sel)) deallocate(fitness_sel,stat=ierr)
      if(allocated(fitness_idx)) deallocate(fitness_idx,stat=ierr)
      if(allocated(fitness_idxsel)) deallocate(fitness_idxsel,stat=ierr)
      if(allocated(arz))      deallocate(arz,stat=ierr)
      if(allocated(arx))      deallocate(arx,stat=ierr)
      if(allocated(arxvalid)) deallocate(arxvalid,stat=ierr)
      if(allocated(iwork))    deallocate(iwork,stat=ierr)
      if(allocated(rwork))    deallocate(rwork,stat=ierr)
      if(allocated(cwork1))   deallocate(cwork1,stat=ierr)
      if(allocated(cwork2))   deallocate(cwork2,stat=ierr)
      if(allocated(weights))  deallocate(weights,stat=ierr)
      if(allocated(rwork1))   deallocate(rwork1,stat=ierr)
      if(allocated(xhist))    deallocate(xhist,stat=ierr)
      if(allocated(phi_contrib)) deallocate(phi_contrib,stat=ierr)
      if(allocated(agp_contrib)) deallocate(agp_contrib,stat=ierr)
      if(allocated(phimaxminreldiff)) deallocate(phimaxminreldiff,stat=ierr)

      return

   end subroutine cmaes_run



   subroutine cmaes_fin(ifail)

       implicit none

       integer, intent(out)  :: ifail

       ifail=0
       icount_cmaes_fin=icount_cmaes_fin+1

! -- Arrays are deallocated.

       if(allocated(xstart))        deallocate(xstart,stat=ierr)
       if(allocated(insigma))       deallocate(insigma,stat=ierr)
       if(allocated(diffmaxchange)) deallocate(diffmaxchange,stat=ierr)
       if(allocated(diffminchange)) deallocate(diffminchange,stat=ierr)
       if(allocated(lbounds))       deallocate(lbounds,stat=ierr)
       if(allocated(ubounds))       deallocate(ubounds,stat=ierr)
       if(allocated(startseed))     deallocate(startseed,stat=ierr)
       if(allocated(xlo))           deallocate(xlo,stat=ierr)
       if(allocated(c))             deallocate(c,stat=ierr)

       return


   end subroutine cmaes_fin



      real function myprctile_sorted(nn,inar,perc)

! -- Returns the value in vector INAR corresponding to percentage PERC. It is assumed that INAR is sorted.

         implicit none

         integer, intent(in) :: nn
         real, intent(in)    :: inar(:)
         real, intent(in)    :: perc

         real    :: perc1,perc2

         if(perc.le.100.0*(0.5/float(nn)))then
           myprctile_sorted=inar(1)
         else if(perc.ge.100.0*((float(nn)-0.5)/float(nn)))then
           myprctile_sorted=inar(nn)
         else
           do i=1,nn
             perc2=100.0*(float(i)-0.5)/float(nn)
             if(perc2.ge.perc) exit
           end do
           perc1=100*(float(i-1)-0.5)/float(nn)
           myprctile_sorted=inar(i-1)+(perc-perc1)/(perc2-perc1)*(inar(i)-inar(i-1))
         end if

        return

      end function myprctile_sorted




  subroutine cmaes_write_warning(iunit)

        implicit none

	integer iunit,jend,nblc,junit,leadblank,itake
	character (len=20) ablank

        amessage=' '//amessage

	ablank=' '
	itake=0
	j=0
	junit=iunit

        if(amessage.eq.' ')then
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
	     write(junit,'(a)') ablank(1:leadblank+2)//amessage(j+1:i)
	  end if
	  j=i
	  go to 5
	end if
	end do
	if(itake.eq.0)then
	  write(junit,'(a)') amessage(j+1:jend)
	  itake=2+leadblank
	else
	  write(junit,'(a)') ablank(1:leadblank+2)//amessage(j+1:jend)
	end if
	j=jend
	go to 5
100     jend=nblc
	if(itake.eq.0)then
	  write(junit,'(a)') amessage(j+1:jend)
	else
	  write(junit,'(a)') ablank(1:leadblank+2)//amessage(j+1:jend)
	end if

! -- The message is now cleared.

        amessage=' '
	return

  end subroutine cmaes_write_warning


  subroutine cmaes_mysort(num,vector,idx)

! -- Subroutine MYSORT sorts the contents of a vector.
!    NUM is thenumber of active elements in the vector.

        implicit none

        integer, intent(in)     :: num
        real, intent(inout)     :: vector(:)
        integer, intent(inout)  :: idx(:)

        integer                 :: jj
        real                    :: rtemp

        do i=1,num
          rwork(i)=vector(i)
        end do
        do i=1,num
          rtemp=huge(rtemp)
          do j=1,num
            if(rwork(j).lt.rtemp)then
              jj=j
              rtemp=rwork(j)
            end if
          end do
          vector(i)=rtemp
          idx(i)=jj
          rwork(jj)=huge(rtemp)
        end do

        return

  end subroutine cmaes_mysort



  real function cmaes_median1(rarray)

! -- CMAES_MEDIAN1 calculates a median, given the fact that nul values may be present.

        implicit none
        real, intent(in)   :: rarray(:)

        integer            :: mm,nn

        nn=size(rarray)

        do i=1,nn
          if(rarray(i).lt.RNANTHRESH)then
            iwork(i)=0
          else
            iwork(i)=1
          end if
        end do

        m=0
        do
          rtemp=-huge(rtemp)
          j=0
          do i=1,nn
            if(iwork(i).ne.0)then
              if(rarray(i).gt.rtemp)then
                rtemp=rarray(i)
                j=i
              end if
            end if
          end do
          if(j.ne.0)then
            m=m+1
            rwork(m)=rarray(j)
            iwork(j)=0
          else
            go to 20
          end if
        end do
20      continue
        if(m.eq.0)then
          cmaes_median1=RNANVALUE
        else if(m.eq.1)then
          cmaes_median1=rwork(1)
        else
          mm=m/2
          if(2*mm.eq.m)then
            cmaes_median1=(rwork(mm)+rwork(mm+1))*0.5
          else
            cmaes_median1=rwork(mm+1)
          end if
        endif

        return

  end function cmaes_median1



  subroutine cmaes_get_message_string(ifail,amessage_out)

      implicit none

      integer, intent(out)       :: ifail
      character*(*), intent(out) :: amessage_out

      ifail=0
      amessage_out=amessage

      return

  end subroutine cmaes_get_message_string



  subroutine cmaes_get_stop_info(ifail,jj,astop)

        implicit none
        integer, intent(out)             :: ifail
        integer, intent(out)             :: jj
        character*(*),intent(out)        :: astop(:)

        integer :: nn

        ifail=0
        nn=size(astop)
        if(nn.lt.nstopflag)then
          write(amessage,10)
10        format('Error in subroutine CMAES_GET_MESSAGE_STRING: ',    &
          'character array is dimensioned smaller than number of ',   &
          'stoppage strings.')
          go to 9890
        end if

        jj = nstopflag
        do i=1,nstopflag
          astop(i)=stopflag(i)
        end do

        return

9890    continue
        ifail=1
        return

  end subroutine cmaes_get_stop_info




  subroutine cmaes_lowcase(ASTRNG)

! -- Subroutine CMAES_LOWCASE converts a string to lower case.

        INTEGER I,J
        CHARACTER*(*) ASTRNG

        DO 10 I=1,len_trim(ASTRNG)
        J=ICHAR(ASTRNG(I:I))
        IF((J.GE.65).AND.(J.LE.90)) ASTRNG(I:I)=CHAR(J+32)
10      CONTINUE
        RETURN

  end subroutine cmaes_lowcase


  subroutine cmaes_writint(atemp,ival)

! -- Subroutine CMAES_WRITINT writes an integer to a character variable.

        integer*4 ival
        character*6 afmt
        character*(*) atemp

        afmt='(i   )'
        write(afmt(3:5),'(i3)') len(atemp)
        write(atemp,afmt)ival
        atemp=adjustl(atemp)
        return

  end subroutine cmaes_writint



  subroutine cmaes_read_restart_data_1(ifail)

  ! -- Subroutine CMAES_READ_RESTART_DATA_1 reads restart data from the binary restart file.

        implicit none
        integer, intent(out)  :: ifail

        integer               :: itemp1,itemp2

        ifail=0

        read(resunit,err=9000,end=9000) itemp1,itemp2,iopt
        if(itemp1.ne.n)then
          write(amessage,10) trim(restartfile)
10        format('Problem specifications as read from file ',a,' do not match ',  &
          'those in previous model run.')
          go to 9890
        end if

        read(resunit,err=9000,end=9000) bnd_isactive,SupplyInitialCov,StopOnWarnings,WarnOnEqualFunctionValues,  &
                                        EvalInitialX,Science
        read(resunit,err=9000,end=9000) popsize,lambda,MaxIter,MaxFunEvals,StopFunEvals,StopIter,nphistp,  &
                                        nphinored,nrelpar,ParentNumber,seed,ncontrib
        read(resunit,err=9000,end=9000) stopfitness,tolfun,phiredstp,relparstp
        read(resunit,err=9000,end=9000) RecombinationWeights
        read(resunit,err=9000,end=9000) tolx
        read(resunit,err=9000,end=9000) tolupx
        read(resunit,err=9000,end=9000) diffmaxchange
        read(resunit,err=9000,end=9000) diffminchange

        return

9000    write(amessage,9010) trim(restartfile)
9010    format('Error encountered while reading restart data from file ',a,'.')
        go to 9890

9890    ifail=1
         close(unit=resunit,iostat=ierr)

9910    continue
        return

        end subroutine cmaes_read_restart_data_1


  subroutine cmaes_read_restart_data_2(ifail)

! -- Subroutine CMAES_READ_RESTART_DATA_2 continues to read restart data from the binary restart file.

        implicit none
        integer, intent(out)  :: ifail

        ifail=0

        read(resunit,err=9000,end=9000) flgscience,flgsaving,flgsavingfinal,flgwarnonequalfunctionvalues,   &
                     bnd_validfitval,bnd_iniphase,bnd_isactive
        read(resunit,err=9000,end=9000) bnd_isbounded
        read(resunit,err=9000,end=9000) icount_cmaes_init,icount_set_problem_definition,icount_set_logical_options, &
                     icount_set_integer_options,icount_set_real_options,icount_set_real_array_options, &
                     icount_set_real_matrix_options,icount_set_character_options,icount_get_logical_options, &
                     icount_get_integer_options,icount_get_real_options,icount_get_real_array_options, &
                     icount_get_real_matrix_options,icount_get_character_options,icount_get_best_parameters, &
                     icount_cmaes_run,icount_cmaes_fin,seed_size,countnorm
        read(resunit,err=9000,end=9000) counteval,numberofvariables,lambda,mu,countiter,outiter,lwork,ldvt,iterlo, &
                                        nphimaxmin,phi_index
        read(resunit,err=9000,end=9000) fmean,mueff,cc,cs,mucov,ccov,damps,sigma,fac,hsig,chin,val,philo, &
                                        thresh_phimaxmin
        read(resunit,err=9000,end=9000) fitness_idx
        read(resunit,err=9000,end=9000) fitness_idxsel
        read(resunit,err=9000,end=9000) lbounds
        read(resunit,err=9000,end=9000) ubounds
        read(resunit,err=9000,end=9000) xstart
        read(resunit,err=9000,end=9000) insigma
        read(resunit,err=9000,end=9000) xmean
        read(resunit,err=9000,end=9000) xlo
        read(resunit,err=9000,end=9000) xold
        read(resunit,err=9000,end=9000) zmean
        read(resunit,err=9000,end=9000) maxdx
        read(resunit,err=9000,end=9000) mindx
        read(resunit,err=9000,end=9000) weights
        read(resunit,err=9000,end=9000) pc
        read(resunit,err=9000,end=9000) ps
        read(resunit,err=9000,end=9000) b
        read(resunit,err=9000,end=9000) d
        read(resunit,err=9000,end=9000) bd
        read(resunit,err=9000,end=9000) c
        read(resunit,err=9000,end=9000) fitness_hist
        read(resunit,err=9000,end=9000) fitness_histsel
        read(resunit,err=9000,end=9000) fitness_raw
        read(resunit,err=9000,end=9000) fitness_sel
        read(resunit,err=9000,end=9000) xhist
        read(resunit,err=9000,end=9000) bnd_weights
        read(resunit,err=9000,end=9000) bnd_scale
        read(resunit,err=9000,end=9000) bnd_dfithist
        read(resunit,err=9000,end=9000) arz
        read(resunit,err=9000,end=9000) arx
        read(resunit,err=9000,end=9000) arxvalid
        read(resunit,err=9000,end=9000) phi_contrib
        read(resunit,err=9000,end=9000) agp_contrib
        read(resunit,err=9000,end=9000) phimaxminreldiff

        go to 9900

9000    write(amessage,9010) trim(restartfile)
9010    format('Error encountered while reading restart data from file ',a,'.')
        go to 9890

9890    ifail=1

9900    continue
        close(unit=resunit,iostat=ierr)
        return

        end subroutine cmaes_read_restart_data_2




  subroutine cmaes_dump_all_data(ifail)

  ! -- Subroutine CMAES_DUMP_ALL_DATA writes all data to a binary file so that a subsequent
  !    restart may take place.

        implicit none
        integer, intent(out) :: ifail

        integer              :: iunit

        ifail=0

        iunit=cmaes_nextunit()
        open(unit=iunit,file=restartfile,form='unformatted',    &
        action='write',iostat=ierr)
        if(ifail.ne.0)then
          write(amessage,10) trim(restartfile)
10        format('Cannot open file ',a,' for storage of restart data.')
          ifail=1
          return
        end if

! -- Variables that are assigned values in CMAES_INIT are stored.

        write(iunit) n,outunit1,iopt                       ! first two required by CMAES_INIT

!-- Variables that are assigned values in CMAES_DEFAULT (or later) are stored.

! --    Logical variables

        write(iunit) bnd_isactive,SupplyInitialCov,StopOnWarnings,WarnOnEqualFunctionValues,  &
                     EvalInitialX,Science

! --    Integers

        write(iunit) popsize,lambda,MaxIter,MaxFunEvals,StopFunEvals,StopIter,nphistp,  &
                     nphinored,nrelpar,ParentNumber,seed,ncontrib

! --    Real numbers

        write(iunit) stopfitness,tolfun,phiredstp,relparstp

! --    Character variables

        write(iunit) RecombinationWeights

! --    Arrays

        write(iunit) tolx
        write(iunit) tolupx
        write(iunit) diffmaxchange
        write(iunit) diffminchange

! -- Other variables are stored.

! --    Logical variables

        write(iunit) flgscience,flgsaving,flgsavingfinal,flgwarnonequalfunctionvalues,   &
                     bnd_validfitval,bnd_iniphase,bnd_isactive

! --    Logical arrays

        write(iunit) bnd_isbounded

! --    Integer variables

        write(iunit) icount_cmaes_init,icount_set_problem_definition,icount_set_logical_options, &
                     icount_set_integer_options,icount_set_real_options,icount_set_real_array_options, &
                     icount_set_real_matrix_options,icount_set_character_options,icount_get_logical_options, &
                     icount_get_integer_options,icount_get_real_options,icount_get_real_array_options, &
                     icount_get_real_matrix_options,icount_get_character_options,icount_get_best_parameters, &
                     icount_cmaes_run,icount_cmaes_fin,seed_size,countnorm

        write(iunit) counteval,numberofvariables,lambda,mu,countiter,outiter,lwork,ldvt,iterlo,nphimaxmin,phi_index

! --    Real variables

        write(iunit) fmean,mueff,cc,cs,mucov,ccov,damps,sigma,fac,hsig,chin,val,philo,thresh_phimaxmin

! --    Integer arrays

        write(iunit) fitness_idx
        write(iunit) fitness_idxsel

! --    Real arrays

        write(iunit) lbounds
        write(iunit) ubounds
        write(iunit) xstart
        write(iunit) insigma
        write(iunit) xmean
        write(iunit) xlo
        write(iunit) xold
        write(iunit) zmean
        write(iunit) maxdx
        write(iunit) mindx
        write(iunit) weights
        write(iunit) pc
        write(iunit) ps
        write(iunit) b
        write(iunit) d
        write(iunit) bd
        write(iunit) c
        write(iunit) fitness_hist
        write(iunit) fitness_histsel
        write(iunit) fitness_raw
        write(iunit) fitness_sel
        write(iunit) xhist
        write(iunit) bnd_weights
        write(iunit) bnd_scale
        write(iunit) bnd_dfithist
        write(iunit) arz
        write(iunit) arx
        write(iunit) arxvalid
        write(iunit) phi_contrib
        write(iunit) agp_contrib
        write(iunit) phimaxminreldiff

        close(unit=iunit)

  end subroutine cmaes_dump_all_data


  integer function cmaes_nextunit()

! -- Function CMAES_NEXTUNIT determines the lowest unit number available for
! -- opening.

       logical::lopen

       do cmaes_nextunit=10,100
         inquire(unit=cmaes_nextunit,opened=lopen)
         if(.not.lopen) return
       end do
       write(6,10)
10     format(' *** No more unit numbers to open files ***')
       stop

  end function cmaes_nextunit



end module cmaes_module





      REAL FUNCTION cmaes_gennor(av,sd)

!     Generates a single random deviate from a normal distribution
!     with mean, AV, and standard deviation, SD.

      REAL av,sd
      cmaes_gennor = sd*cmaes_snorm() + av
      RETURN

      END FUNCTION cmaes_gennor



      REAL FUNCTION cmaes_snorm()

      real a(32),d(31),t(31),h(31)

      DATA a/0.0,.3917609E-1,.7841241E-1,.1177699,.1573107,.1970991,   &
           .2372021,.2776904,.3186394,.3601299,.4022501,.4450965,      &
           .4887764,.5334097,.5791322,.6260990,.6744898,.7245144,      &
           .7764218,.8305109,.8871466,.9467818,1.009990,1.077516,      &
           1.150349,1.229859,1.318011,1.417797,1.534121,1.675940,      &
           1.862732,2.153875/
      DATA d/5*0.0,.2636843,.2425085,.2255674,.2116342,.1999243,       &
           .1899108,.1812252,.1736014,.1668419,.1607967,.1553497,      &
           .1504094,.1459026,.1417700,.1379632,.1344418,.1311722,      &
           .1281260,.1252791,.1226109,.1201036,.1177417,.1155119,      &
           .1134023,.1114027,.1095039/
      DATA t/.7673828E-3,.2306870E-2,.3860618E-2,.5438454E-2,             &
           .7050699E-2,.8708396E-2,.1042357E-1,.1220953E-1,.1408125E-1,   &
           .1605579E-1,.1815290E-1,.2039573E-1,.2281177E-1,.2543407E-1,   &
           .2830296E-1,.3146822E-1,.3499233E-1,.3895483E-1,.4345878E-1,   &
           .4864035E-1,.5468334E-1,.6184222E-1,.7047983E-1,.8113195E-1,   &
           .9462444E-1,.1123001,.1364980,.1716886,.2276241,.3304980,      &
           .5847031/
      DATA h/.3920617E-1,.3932705E-1,.3950999E-1,.3975703E-1,             &
           .4007093E-1,.4045533E-1,.4091481E-1,.4145507E-1,.4208311E-1,   &
           .4280748E-1,.4363863E-1,.4458932E-1,.4567523E-1,.4691571E-1,   &
           .4833487E-1,.4996298E-1,.5183859E-1,.5401138E-1,.5654656E-1,   &
           .5953130E-1,.6308489E-1,.6737503E-1,.7264544E-1,.7926471E-1,   &
           .8781922E-1,.9930398E-1,.1155599,.1404344,.1836142,.2790016,   &
           .7010474/

!!   10 u = ranf()
   10 call random_number(u)
      s = 0.0
      IF (u.GT.0.5) s = 1.0
      u = u + u - s
   20 u = 32.0*u
      i = int(u)
      IF (i.EQ.32) i = 31
      IF (i.EQ.0) GO TO 100

!                                START CENTER

   30 ustar = u - float(i)
      aa = a(i)
   40 IF (ustar.LE.t(i)) GO TO 60
      w = (ustar-t(i))*h(i)

!                                EXIT   (BOTH CASES)

   50 y = aa + w
      cmaes_snorm = y
      IF (s.EQ.1.0) cmaes_snorm = -y
      RETURN

!                                CENTER CONTINUED

!!   60 u = ranf()
   60 call random_number(u)
      w = u* (a(i+1)-aa)
      tt = (0.5*w+aa)*w
      GO TO 80

   70 tt = u
!!      ustar = ranf()
      call random_number(ustar)
   80 IF (ustar.GT.tt) GO TO 50
!!   90 u = ranf()
   90 call random_number(u)
      IF (ustar.GE.u) GO TO 70
!!      ustar = ranf()
      call random_number(ustar)
      GO TO 40

!                                START TAIL

  100 i = 6
      aa = a(32)
      GO TO 120

  110 aa = aa + d(i)
      i = i + 1
  120 u = u + u
      IF (u.LT.1.0) GO TO 110
  130 u = u - 1.0
  140 w = u*d(i)
      tt = (0.5*w+aa)*w
      GO TO 160

  150 tt = u
!!  160 ustar = ranf()
  160 call random_number(ustar)
      IF (ustar.GT.tt) GO TO 50
!!  170 u = ranf()
  170 call random_number(u)
      IF (ustar.GE.u) GO TO 150
!!      u = ranf()
      call random_number(u)
      GO TO 140

      END function cmaes_snorm



! -- What do we do about the seed?
! -- In a CMAES finalise routine, restart all of the subroutine call counts.
!    Point out that if it CMAES is initialised again, default values will have
!    been provided to all control variables.
! -- I have disabled the INCPOPSIZE thing. This must be done on the outside. So
!    we must have a restart capability that allows the population size to be
!    increased when the restart is done.
! -- We need to deallocate memory on exiting CMAES. Or do we need to retrieve results.
!    Anyway, we need to deallocate memory somewhere.
