      MODULE PESTDATA

C -- Module PESTDATA holds the values of PEST variables.

        IMPLICIT NONE

C -- Data types.

        TYPE :: SQUARE_DMATRIX
          INTEGER :: NDIM
          DOUBLE PRECISION, POINTER, DIMENSION(:,:) :: DVAL
        END TYPE SQUARE_DMATRIX

C -- Utility Parameters

        INTEGER, PARAMETER           :: LENOBSNAM=20 ! Max characters allowed in observation name.
        INTEGER, PARAMETER           :: UNASSIGNED_I = HUGE(0)
        DOUBLE PRECISION, PARAMETER  :: UNASSIGNED_D = HUGE(0.0D0)
        CHARACTER (LEN=1), PARAMETER :: UNASSIGNED_C = ' '

C -- File unit numbers

        INTEGER                 :: ISDU=10      ! The JCO file used by SVDA (input).
        INTEGER                 :: IPPP=11      ! Direct access binary file for parallel parameter register (i/o)
        INTEGER                 :: IPPO=12      ! Direct access binary file for parallel observation register (i/o)
        INTEGER                 :: ISDA=14      ! Used for reading/writing SVDA data (i/o).
        INTEGER                 :: IDFL=31      ! Used for file deletion and copying.
        INTEGER                 :: INST=35      ! Template, instruction and deriv files (input).
        INTEGER                 :: IMMF=36      ! PEST-to-model message file.
        INTEGER                 :: ISTP=39      ! Pest stopping file pest.stp (input).
        INTEGER                 :: IMIF=40      ! Model input file (output).
        INTEGER                 :: IRSF=50      ! Binary restart files (i/o); also pll comm file.
        INTEGER                 :: IPFL=51      ! Parameter value file (output - also input when NOPTMAX=-3).
        INTEGER                 :: ISNS=52      ! PEST sensitivity file (output).
        INTEGER                 :: IGNF=53      ! Residuals, obs sen and matrix files (output);
                                                ! Parameter hold file (input).
        INTEGER                 :: IRSJ=54      ! Jacobian binary file read for sensitivity re-use restart.
        INTEGER                 :: ISDF=75      ! SVD-related data (output).
        INTEGER                 :: ICND=75      ! LINPACK solver condition number file (output).
        INTEGER                 :: ILSQ=75      ! LSQR-related data (output).
        INTEGER                 :: IPCM=79      ! Parallel message file communication (i/o).
        INTEGER                 :: IRMR=80      ! Run management record file (output).
        INTEGER                 :: IRMF=81      ! Run management file (input).
        INTEGER                 :: IJRF=82      ! File jacob.runs (unix only).
        INTEGER                 :: IPPD=83      ! Binary file for storage of Pareto parameter data
        INTEGER                 :: IPOD=84      ! ASCII file for storage of Pareto obj fn data
        INTEGER                 :: IREC=90      ! Run record file (output only).
        INTEGER                 :: IRRF=97      ! Run results file (output only).
        INTEGER                 :: IPTF=96      ! Restart file used for "/s" and"/d" restart.

C -- Problem size.

        INTEGER             :: NOBSGP       ! Number of observation groups.
        INTEGER             :: NPARGP       ! Number of parameter groups.
        INTEGER             :: NPAR         ! Number of "non-derived" parameters.
        INTEGER             :: NOBS         ! Number of observations.
        INTEGER             :: NPRIOR       ! Number of linear prior information eqns
        INTEGER             :: NXROW        ! Number of obs plus prior info
        INTEGER             :: NUMCOM=1     ! No. of model command lines for fd derivs calc.
        INTEGER             :: NESPAR       ! No. of adjustable parameters.
        INTEGER             :: NTPLFLE      ! No. of model input files.
        INTEGER             :: NINSFLE      ! No. of model output files

C -- General variables

        INTEGER            :: RSTFLE=1                ! RSTFLE       : 0="norestart";1="restart"
        INTEGER            :: PESTMODE=UNASSIGNED_I   ! Mode         : 1='estimation'; 2='prediction'; 3='regularisation'; 4='pareto'
        INTEGER            :: RESTART=0    ! Restart flag : 0=beginning; 1="/r"; 2="/j"; 3="/s"; 4="/d";5="/i"
        INTEGER            :: P1START=0    ! Flag for doing 1st model run and 1st Jacobian as 1 parallel parcel
        INTEGER            :: HPSTART      ! Set to 1 to leave a PEST_HP starter file.
        CHARACTER(LEN=5)   :: DOAUI='noaui'           ! DOAUI: 'aui' or 'noaui'
        CHARACTER (LEN=10) :: DOSENREUSE='nosenreuse' ! DOSENREUSE: 'senreuse' or 'nosenreuse'
        CHARACTER (LEN=11) :: RUNREPEAT='runrepeat'   ! RUNREPEAT: runrepeat' or 'norunrepeat'

C -- Variables which say if a particular section is present.

        INTEGER            :: HAS_SIMUL_SECTION=0      ! Tells whether a "simultaneous increment" section is present.
        INTEGER            :: HAS_RANDOM_SECTION=0     ! Tells whether a "randomized Jacobian" section is present.
        INTEGER            :: HAS_RSI_SECTION=0        ! Tells whether a "rsi" section is present.

C -- Writing to a model input file

        INTEGER  :: PRECIS=0     ! PRECIS  : 0="single";1="double"
        INTEGER  :: NOPNT=0      ! DPOINT  : 0="point";1="nopoint"

C -- Derivatives calculation and model messaging

        INTEGER  :: JACFILE=0    ! Flag for external derivatives
        INTEGER  :: BINJACFILE=0 ! Indicates whether external Jacobian matrix file is binary.
        INTEGER  :: MESSFILE=0   ! Flag for writing of PEST message file
        INTEGER  :: MS_STATUS=0  ! Employ a subroutine for model runs and/or derivatives calculation

C -- Marquardt lamda manipulation

        INTEGER           :: NUMLAM=10           ! Max no. of lambdas per iteration
        INTEGER           :: RLAMFAC_ADJUST=0    ! Whether to adjust LAMFAC each iteration depending on LAMDA
        INTEGER           :: LAMFORGIVE=0        ! Treat an error condition on a lambda run as a high objective fn if 1.
        INTEGER           :: UPTESTLIM=-1        ! For PEST_HP: user-supplied limit on number of parameter upgrades
        INTEGER           :: UPTESTMIN=-1        ! For PEST_HP: user-supplied limit on number of parameter upgrades
        DOUBLE PRECISION  :: RLAMBDA1=10.0D0     ! Initial Marquardt lambda
        DOUBLE PRECISION  :: RLAMFAC=2.0D0       ! Lambda adjustment ratio
        DOUBLE PRECISION  :: PHIRATSUF=0.3D0     ! Sufficient objective function reduction
        DOUBLE PRECISION  :: PHIREDLAM=0.03D0    ! Indicator of no further lambda gains
        DOUBLE PRECISION  :: RLAMFACEXP=10.0D0   ! Number of LAMFAC multiplications between current value and 1.0

C -- Parameter change controls

        INTEGER, PARAMETER :: MAXABSPARMAX =10         ! Maximum number of absolute change limits
        INTEGER            :: IBOUNDSTICK=0            ! Flag for parameter bounds sticking
        INTEGER            :: UPVECBEND=0              ! Flag for parameter upgrade vector bending
        INTEGER            :: IBOUNDSCALE=0            ! Scale parameters wrt their bounds
        DOUBLE PRECISION   :: RELPARMAX=5.0D0          ! Relative change limit
        DOUBLE PRECISION   :: FACPARMAX=5.0D0          ! Factor change limit
        DOUBLE PRECISION   :: FACORIG=0.01D0           ! Use in denominator when parameter low
        DOUBLE PRECISION   :: ABSPARMAX(MAXABSPARMAX)  ! Absolute parameter change limits

C -- Derivatives control

        INTEGER           :: NOPTSWITCH=0        ! Itn at which to consider switching
        DOUBLE PRECISION  :: PHIREDSWH=0.1D0     ! Rel phi improvement needed to avoid switching
        DOUBLE PRECISION  :: SPLITSWH=0.0D0      ! Rel phi change for onset of split derivatives analysis
        INTEGER           :: DERFORGIVE=0
        DOUBLE PRECISION  :: JCOWARNTHRESH=0.0D0   ! Level at which to warn user about high JCO elements (PEST_HP only)
        DOUBLE PRECISION  :: JCOZEROTHRESH=0.0D0   ! Level at which to warn user and set JCO elements to zero (PEST_HP only)

C -- Termination criteria

        INTEGER           :: NOPTMAX=40             ! Maximum number of optimisation iterations.
        INTEGER           :: NPHISTP=4              ! Iterations since significant phi reduction
        INTEGER           :: NPHINORED=4            ! Iterations of no phi reduction
        INTEGER           :: NRELPAR=4              ! Iterations since parameter change
        DOUBLE PRECISION  :: PHIREDSTP=0.001D0      ! Obj. Fn. reduction threshold
        DOUBLE PRECISION  :: RELPARSTP=0.001D0      ! Rel. parameter change threshold
        DOUBLE PRECISION  :: PHISTOPTHRESH=-1.1D100 ! Obj. Fn. stoppage threshold
        INTEGER           :: LASTRUN=1              ! Do last model run
        DOUBLE PRECISION, ALLOCATABLE :: PHIABANDON(:)  ! Obj. Fn. abandonmnent threshold; allocate (0:noptmax)
        INTEGER           :: IFINSTOPFLAG=0         ! Set to 1 if stop after 1st model run ready for jacobian restart.

C -- Numerical output

        INTEGER           :: ICOV=1       ! Covariance matrix flag
        INTEGER           :: ICOR=1       ! Corellation matrix flag
        INTEGER           :: IEIG=1       ! Eigenstuff flag
        INTEGER           :: IRES=-9999   ! Resolution matrix flag
        INTEGER           :: IJCO=0       ! JCO save per iteration flag
        INTEGER           :: IREI=0       ! REI save per iteration flag
        INTEGER           :: JCOSAVE=1    ! Whether to save JCO file.
        INTEGER           :: VERBOSEREC=1 ! Verbose run record file
        INTEGER           :: PARSAVEITN=0 ! Save iteration-specific parameter value file.
        INTEGER           :: PARSAVERUN=0 ! Save run-specific parameter value file.
        INTEGER           :: PARSAVEOBS=0 ! Save reference model outputs in best par file.
        INTEGER           :: WRITE_RRF=0  ! Save run results file.

C -- Predictive analysis control

        INTEGER           :: NPREDMAXMIN=UNASSIGNED_I  ! Maximize or minimize prediction
        INTEGER           :: PREDNOISE=0        ! Take account of predictive noise
        DOUBLE PRECISION  :: PD0=UNASSIGNED_D   ! Objective function threshold
        DOUBLE PRECISION  :: PD1=UNASSIGNED_D   ! Acceptable phi threshold
        DOUBLE PRECISION  :: PD2=UNASSIGNED_D   ! Phi threshold for change in testing phi behavior
        DOUBLE PRECISION  :: ABSPREDLAM=0.0D0   ! Abs phi reduction threshold for new lambda
        DOUBLE PRECISION  :: RELPREDLAM=0.005D0 ! Rel phi reduction threshold for new lambda
        DOUBLE PRECISION  :: INITSCHFAC=1.0D0   ! Factor for start of line search
        DOUBLE PRECISION  :: MULSCHFAC=2.0D0    ! Line search multiplier
        INTEGER           :: NSEARCH=8          ! Max number of models runs per line search
        DOUBLE PRECISION  :: ABSPREDSWH=0.0D0   ! Abs phi improvement for 3 pt derivs calc
        DOUBLE PRECISION  :: RELPREDSWH=0.05D0  ! Rel phi improvement for 3 pt derivs calc
        INTEGER           :: NPREDNORED=4       ! Itns since prediction improvement for termination
        DOUBLE PRECISION  :: ABSPREDSTP=0.0D0   ! Abs prediction improvement threshold
        DOUBLE PRECISION  :: RELPREDSTP=0.005D0 ! Rel prediction improvement threshold
        INTEGER           :: NPREDSTP=4         ! Itns since insufficient rel or abs improvement

C -- Regularisation control

        DOUBLE PRECISION  :: PHIMLIM=UNASSIGNED_D    ! Target meas. obj fn
        DOUBLE PRECISION  :: PHIMACCEPT=UNASSIGNED_D ! Acceptable meas. obj fn
        DOUBLE PRECISION  :: FRACPHIM=-1.0D30        ! PHIMLIM as fraction of current phi
        DOUBLE PRECISION  :: REG2MEASRAT=0.0D0       ! Makes phir a fraction of current phim
        DOUBLE PRECISION  :: WFINIT=1.0D0            ! Initial weight factor
        DOUBLE PRECISION  :: WFMIN=1.0D-10           ! Minimum weight factor
        DOUBLE PRECISION  :: WFMAX=1.0E10            ! Maximum weight factor
        DOUBLE PRECISION  :: WFFAC=1.3D0             ! Weight factor adjustment ratio
        DOUBLE PRECISION  :: WFTOL=1.0D-2       ! Closure criterion in wt factor calculation
        INTEGER           :: IREGADJ=1          ! Automatic inter-group weights adjustment
        INTEGER           :: LINREG=0           ! Flag indicating linear prior info
        INTEGER           :: MEMSAVE=-1         ! MEMSAVE: 1="memsave";.lt.1="nomemsave"
        INTEGER           :: CONJGRAD=0         ! CONJGRAD: 1="cg";0="nocg"
        INTEGER           :: REGCONTINUE=0      ! Set to 1 to prevent termination when PHIM less than PHIMLIM.
        INTEGER           :: NOPTREGADJ=1       ! Optim interval at which wts recalc applied when IREGADJ=4 or 5
        DOUBLE PRECISION  :: REGWEIGHTRAT=100.0 ! Max regularisation weight ratio when IREGADJ=4 or 5
        DOUBLE PRECISION  :: REGSINGTHRESH=1E-4 ! Singular value ratio threshold when IREGADJ=5

C -- Pareto control

        INTEGER           :: PAR_NOBGNM       ! Number of observation group for Pareto weights adjustment
        DOUBLE PRECISION  :: PAR_WTFAC_START  ! Pareto starting weights adjustment factor
        DOUBLE PRECISION  :: PAR_WTFAC_FIN    ! Pareto final weights adjustement factor
        INTEGER           :: NUM_WTFAC_INC    ! Number of Pareto weight factor increments
        INTEGER           :: NUM_ITER_START   ! Number of iterations at starting weight
        INTEGER           :: NUM_ITER_GEN     ! General number of iterations
        INTEGER           :: NUM_ITER_FIN     ! Number of iterations for final weight
        INTEGER           :: ALT_TERM	      ! 0 if no alternative stopping criterion; 1 if yes
        INTEGER           :: NOBS_TERM        ! Observation number to monitor
        INTEGER           :: N_ABOVE_BELOW    ! -1 if below 1 if above for observation
        DOUBLE PRECISION  :: OBS_THRESH       ! Observation threshold for termination
        INTEGER           :: NUM_ITER_THRESH  ! Number of iterations above/below this threshold
        INTEGER           :: NOBS_REPORT      ! Number of observations to report on
        INTEGER, ALLOCATABLE :: IOBS_REPORT(:) ! Observation numbers whose values to report

C -- Automatic user intervention control

        INTEGER           :: AUISECT=0           ! AUI data supplied;0="no";1="yes"
        INTEGER           :: MAXAUI=UNASSIGNED_I ! Max AUI trials per iteration
        INTEGER           :: AUISTARTOPT=1       ! Itn to commence AUI
        DOUBLE PRECISION  :: NOAUIPHIRAT=0.9D0   ! Rel phi redn threshold for using AUI
        INTEGER           :: AUIRESTITN=0        ! AUI rest iteration interval
        DOUBLE PRECISION  :: AUISENSRAT=5.0D0    ! Max/min sensitivity ratio for AUI
        INTEGER           :: AUIHOLDMAXCHG=0     ! Flag for targetting big parameter movers
        INTEGER           :: AUINUMFREE=3        ! Unheld parameter threshold to stop AUI
        DOUBLE PRECISION  :: AUIPHIRATSUF=0.80D0 ! Rel phi for termination of AUI
        DOUBLE PRECISION  :: AUIPHIRATACCEPT=0.99D0  ! Rel phi for accepting AUI results
        INTEGER           :: NAUINOACCEPT=UNASSIGNED_I! No of failures before AUI termination

C -- Sensitivity re-use.

        DOUBLE PRECISION  :: SENRELTHRESH=0.15      ! Relative threshold for sensitivity reuse.
        DOUBLE PRECISION  :: SENPREDWEIGHT=-1.0D35 ! Weight to apply to predictive sensitivities.
        INTEGER           :: SENMAXREUSE=-999      ! Maximum number of sensitivities to keep
        INTEGER           :: SENALLCALCINT=3       ! Iteration interval for re-computing all sensitivities.
        CHARACTER(LEN=3)  :: SENPIEXCLUDE='yes'    ! "yes" if exclude prior inf from sensitiivty calculations; "no" otherwise

C -- User-declared zero sensitivity

        INTEGER           :: IZEROSENVAL=0         ! Set to 1 to activate user-declared zero sensitivity.
        DOUBLE PRECISION  :: ZEROSENVAL=-1.0D200   ! Model output value identifying zero sensitivity.

C -- Broyden's Jacobian update control

        INTEGER           :: JACUPDATE=0         ! Set to 1 for Broyden's Jacobian update
        INTEGER           :: JACUPDATE_RETAIN=0  ! Set to 1 to retain between iterations if /ii switch used.

C -- Singular Value decomposition control

        INTEGER           :: SVDMODE=0            ! SVD mode flag
        INTEGER           :: MAXSING=UNASSIGNED_I ! Max number of sing. values to use
        DOUBLE PRECISION  :: EIGTHRESH=1.0D-7 ! Max/min eigenvaue threshold
        INTEGER           :: EIGWRITE=0       ! Flag for writing of eigenvalue matrix

C -- LSQR control

        INTEGER           :: LSQRMODE=0           ! LSQR mode flag
        DOUBLE PRECISION  :: LSQR_ATOL=0.0D0      ! LSQR ATOL convergence criterion
        DOUBLE PRECISION  :: LSQR_BTOL=0.0D0      ! LSQR BTOL convergence criterion
        DOUBLE PRECISION  :: LSQR_CONLIM=0.0D0    ! LSQR CONLIM convergence criterion
        INTEGER           :: LSQR_ITNLIM          ! LSQR iteration limit
        INTEGER           :: LSQRWRITE=0          ! Whether to write an LSQR-specific output file

C -- SVD-Assist control

        INTEGER           :: SVDA=0                     ! Set to 1 to implement SVD-Assist
        INTEGER           :: SVDA_MULBPA=0              ! Set to 1 for multiple BPA files.
        INTEGER           :: SVDA_SCALADJ=0             ! Set to 1 for non-log-trans par gp adj.
        INTEGER           :: SVDA_EXTSUPER=0            ! Set to 1 for external super parameters.
        INTEGER           :: SVDA_SUPDERCALC=0          ! Set to 1 to calc super param derivs for first itn.
        INTEGER           :: SVDA_PAR_EXCL=0            ! Set to 1 to exclude fixed-weight-groups from super param defn
        CHARACTER*200     :: BASEPESTFILE=UNASSIGNED_C  ! Base PEST control file
        CHARACTER*200     :: BASEJACFILE=UNASSIGNED_C   ! Base Jacobian matrix file

C -- Model command line information

        CHARACTER(LEN=200)              :: COMJAC=UNASSIGNED_C    ! Model command for deriv calc
        CHARACTER(LEN=200)              :: EXTDERFLE=UNASSIGNED_C ! Exter derivs file.
        CHARACTER(LEN=200), ALLOCATABLE :: COMLIN(:) ! Model commands for fd derivs calc

C -- Model input/output data

        CHARACTER(LEN=200), ALLOCATABLE :: TEMPFLE(:)   ! Name of template files
        CHARACTER(LEN=200), ALLOCATABLE :: INFLE(:)     ! Name of model input files
        CHARACTER(LEN=200), ALLOCATABLE :: INSFLE(:)    ! Name of instruction files
        CHARACTER(LEN=200), ALLOCATABLE :: OUTFLE(:)    ! Name of model output files

C -- Parameter group arrays

        CHARACTER (LEN=12), ALLOCATABLE :: PARGNME(:)      ! Parameter group names
        INTEGER, ALLOCATABLE            :: INCTYP(:)       ! 1="relative";2="absolute";3="rel_to_max"
        DOUBLE PRECISION, ALLOCATABLE   :: DERINC(:)       ! Derivative increment
        DOUBLE PRECISION, ALLOCATABLE   :: DERINCLB(:)     ! Derivative increment lower bound
        DOUBLE PRECISION, ALLOCATABLE   :: DERINCMUL(:)    ! Derivative increment multiplier
        INTEGER, ALLOCATABLE            :: FORCEN(:)       ! 1="switch";2="always_2";3="always_3"
        INTEGER, ALLOCATABLE            :: DERMTHD(:)      ! 1="parabolic";2="best_fit";3="outside_pts"
        DOUBLE PRECISION, ALLOCATABLE   :: SPLITTHRESH(:)  ! Threshold for derivative split testing
        DOUBLE PRECISION, ALLOCATABLE   :: SPLITRELDIFF(:) ! Relative difference threshold for action
        INTEGER, ALLOCATABLE            :: SPLITACTION(:)  ! Nature of action taken

C -- Parameter data arrays

        CHARACTER (LEN=12), ALLOCATABLE   :: APAR(:)     ! Parameter names
        INTEGER, ALLOCATABLE              :: ITRANS(:)   ! 0="none";1="log";(-x)="tied_to_x";
        INTEGER, ALLOCATABLE              :: PARCHGLIM(:) ! 0="relative";1="factor"
        DOUBLE PRECISION, ALLOCATABLE     :: PVAL(:)     ! Initial parameter values
        DOUBLE PRECISION, ALLOCATABLE     :: PARLBND(:)  ! Parameter lower bounds
        DOUBLE PRECISION, ALLOCATABLE     :: PARUBND(:)  ! Parameter upper bounds
        INTEGER, ALLOCATABLE              :: IPARGP(:)   ! Parameter group index
        DOUBLE PRECISION, ALLOCATABLE     :: SCALE(:)    ! Parameter scale
        DOUBLE PRECISION, ALLOCATABLE     :: OFFSET(:)   ! Parameter offset
        INTEGER, ALLOCATABLE              :: DERCOM(:)   ! Command line index for fd derivs

C -- Special parameters

        INTEGER                           :: NPARSEC=0           ! Number of secondary parameters (PEST_HP only)
        INTEGER                           :: NPARFILE=0          ! Number of file-parameters (PEST_HP only)
        INTEGER                           :: NEQUATION=0         ! Number of equations (PEST_HP only)
        CHARACTER (LEN=256)               :: FILEPARFILE=' '     ! File listing parameter value files holding file-parameter values (PEST_HP only)

C -- Observation group data

        CHARACTER (LEN=12), ALLOCATABLE    :: OBGNME(:)  ! Observation group names
        LOGICAL, ALLOCATABLE               :: COVIND(:)  ! Covariance matrix identifer

C -- Covariance matrix data

        INTEGER                            :: NUMCOV=0   ! Number of covariance matrices
        INTEGER, ALLOCATABLE               :: COVGP(:)   ! Group number of covariance matrix
        INTEGER, ALLOCATABLE               :: NOBSCOV(:) ! Number of elements in cov matrix
        DOUBLE PRECISION, ALLOCATABLE      :: GTARG(:)   ! Observation group phi targets
        TYPE (SQUARE_DMATRIX), ALLOCATABLE :: COVAR(:)   ! Observation covariance matrices

C -- Observation data

        CHARACTER (LEN=LENOBSNAM), ALLOCATABLE  :: AOBS(:)    ! Observation names
        DOUBLE PRECISION, ALLOCATABLE           :: OVAL(:)    ! Observation values
        DOUBLE PRECISION, ALLOCATABLE           :: OWGHT(:)   ! Observation weights (squared)
        INTEGER, ALLOCATABLE                    :: NOBGNM(:)  ! Observation group indices

C -- Jacobian matrix

#ifdef INTEL
        INTEGER (8)                    :: MAXCOMPDIM=1 ! elements for compressed X storage
        INTEGER (8)                    :: NCOMPDIM=0   ! elements stored in XC vector
        INTEGER (8)                    :: ISTART_C=0   ! used for expediting compressed vector search
        INTEGER (8)                    :: IFOUND_C=0   ! used for expediting compressed vector search
        INTEGER (8)                    :: IXC_DIM=0    ! dimensions of IXC vector (can change)
#else
        INTEGER                        :: MAXCOMPDIM=1 ! elements for compressed X storage
        INTEGER                        :: NCOMPDIM=0   ! elements stored in XC vector
        INTEGER                        :: ISTART_C=0   ! used for expediting compressed vector search
        INTEGER                        :: IFOUND_C=0   ! used for expediting compressed vector search
        INTEGER                        :: IXC_DIM=0    ! dimensions of IXC vector (can change)
#endif
        INTEGER                        :: XDIM1,XDIM2  ! allocated dimensions of X matrix
        DOUBLE PRECISION               :: DERZEROLIM=0.0D0 ! Zero thresh. (finite-diff derivs)
        DOUBLE PRECISION, ALLOCATABLE  :: X(:,:)       ! The Jacobian matrix
#ifdef INTEL
        INTEGER (8), ALLOCATABLE       :: IXC(:)       ! Element locations in XC vector
#else
        INTEGER, ALLOCATABLE           :: IXC(:)       ! Element locations in XC vector
#endif
#ifdef SINGLE_XC
        REAL, ALLOCATABLE              :: XC(:)        ! Compressed Jacobian matrix X
#else
        DOUBLE PRECISION, ALLOCATABLE  :: XC(:)        ! Compressed Jacobian matrix X
#endif
        INTEGER       :: EXTJAC_CALL=0  ! number of calls to subroutine EXTJAC
        INTEGER       :: COMPRESS_EASY  ! whether easy "compressed" jco can take place
                                        ! (hence only compressed for prior information)
        INTEGER, ALLOCATABLE           :: PI_PAR(:)    ! used with COMPRESS_EASY to allow easy access to JCO
        INTEGER, ALLOCATABLE           :: IPAR2IES(:)  ! provides an IES for every IPAR
        INTEGER, ALLOCATABLE           :: IES2IPAR(:)  ! provides an IPAR for every IES

C -- Run results file data

        CHARACTER (LEN=256)            :: PARFILEBASE=' '      ! Filename base of parameter value files
        INTEGER                        :: PARFILE_START        ! Starting index of parameter value files
        INTEGER                        :: PARFILE_END          ! Ending index of parameter value files
        INTEGER                        :: PACKETSIZE=0         ! Parallel packet size
        CHARACTER (LEN=256)            :: RUN_RESULTS_FILE=' ' ! Name of run results file

C -- Prior information compression

        INTEGER                       :: PR_INDEX=0        ! Use prior information indexing
        INTEGER                       :: NPR_NONZERO       ! Number of non-zero prior information elements
        INTEGER, ALLOCATABLE          :: IXPR(:)           ! Row indices of prior information
        INTEGER, ALLOCATABLE          :: IXPC(:)           ! Column indices of prior information
        INTEGER, ALLOCATABLE          :: IXP_RIGHT(:)      ! Index of next element in same row
        INTEGER, ALLOCATABLE          :: IXP_FIRST(:)      ! The index of the first prior info item in a row
        DOUBLE PRECISION, ALLOCATABLE :: XPVAL(:)          ! Value of prior information
        INTEGER, ALLOCATABLE          :: IESTRANS(:)       ! The "IESS index" of a Jacobian column

C -- Error message strings

        CHARACTER (LEN=1500)             :: ERRMSG=' '
        CHARACTER (LEN=1500)             :: ERRMSG_ADD=' '

C -- JUPITER derivatives interface file

        CHARACTER (LEN=200)             :: DI_DERFILE ! Derivatives file
        INTEGER                         :: DI_NSKIP   ! Number of lines to skip
        INTEGER                         :: DI_NDEP    ! Number of dependents in file
        INTEGER                         :: DI_NPAR    ! Number of parameters in file
        CHARACTER (LEN=10)              :: DI_ORIENTATION ! Matrix specs
        CHARACTER (LEN=100)             :: DI_DERFORMAT   ! Matrix format
        INTEGER, ALLOCATABLE            :: DI_PAR(:)  ! Index of parameters in file
        INTEGER, ALLOCATABLE            :: DI_IES(:)  ! Jacobian index of params in file
        INTEGER, ALLOCATABLE            :: DI_OBS(:)  ! Index of observations in file
        INTEGER, ALLOCATABLE            :: DI_PARENT(:) ! Whether param is parent or not

C -- Variables required for JUPITER awareness

        INTEGER             :: CALLER=0         ! Calling program: 0=PEST;1=J_PEST
        INTEGER             :: JUP_PAR_FILES=0  ! Record JUPITER parameter files: 0="no";1="yes"
        INTEGER             :: J_UNIT=91        ! Unit number for "_pc", "_mv", "_mc" files
        INTEGER             :: J_PE=92          ! Unit number for "_pe" file
        INTEGER             :: J_SS=93          ! Unit number for "_ss" file
        CHARACTER (LEN=200) :: J_OUTNAM=' '     ! JUPITER OUTNAM variable.

C -- Other input variables.

        CHARACTER(LEN=200)            :: CTLFILE=' '      ! Name of PEST control file
        CHARACTER(LEN=200)            :: JCOFIRSTFILE=' ' ! JCO file for first Jacobian matrix
        CHARACTER(LEN=200)            :: RRFFIRSTFILE=' ' ! RRF file for first model results
        CHARACTER(LEN=200)            :: PARREGFILE='pest####.dap'  ! Direct access parallel parameter register file
        CHARACTER(LEN=200)            :: OBSREGFILE='pest####.dao'  ! Direct access parallel observation register file
        CHARACTER(LEN=2000)           :: CLINE=' '        ! Work string
        CHARACTER(LEN=200)            :: REFERENCE_TEXT   ! User supplied text to characterize run
        INTEGER                       :: MAXRECLNUM=1000  ! Number of variables in record of direct acces files
        INTEGER                       :: IREFOBS=0        ! 1 to do model run to recalculate reference observations
        INTEGER                       :: ORR_NOT_FIRST=0  ! Set to 1 to turn off observation re-rerefencering in first iteration.
        INTEGER                       :: IREFOBS_PAUSE=0  ! 1 to pause after re-referencing runs during JCO calculation

C -- Some parameter estimation outcomes

        INTEGER                         :: SENFLAG=0     ! 1 if sensitivities available
        DOUBLE PRECISION, ALLOCATABLE   :: MODVAL_O(:)   ! Best-fit observation-equivalents
        DOUBLE PRECISION, ALLOCATABLE   :: PVAL_O(:)     ! Optimised parameter values.

C -- Parallelisation control

        DOUBLE PRECISION                :: RUN_SLOW_FAC_1=-1.1D100  ! The run time factor indicating slowness from PST file.
        DOUBLE PRECISION                :: RUN_ABANDON_FAC=0.0      ! The run time factor to abandon runs during lambda search.
                                                                    ! (Only used by PEST_HP.)
        DOUBLE PRECISION                :: HARDSTOPHOURS=0.0D0      ! Used only by PEST_HP
        DOUBLE PRECISION                :: SOFTSTOPHOURS=0.0D0      ! Used only by PEST_HP
        DOUBLE PRECISION                :: WIN_MRUN_HOURS=-1.0D0    ! Used only by PEST_HP

      END MODULE PESTDATA




