MODULE EQUATION

      PRIVATE

! -- Defined types

      TYPE COMPRESSED_EQUATION
         CHARACTER (LEN=12)          :: NAME        ! Name of equation
         INTEGER                     :: NUMTERM     ! No. of terms in equation
         INTEGER                     :: LASTINDEX   ! Highest used index of astring
         CHARACTER (LEN=1), POINTER  :: ASTRING(:)  ! Character array to hold equation
         INTEGER, POINTER            :: PTERM(:)    ! Location of terms in astring array
      END TYPE COMPRESSED_EQUATION

! -- Variables that are GLOBAL VARIABLES in JUPITER.

      character*400 amessage,errsub
      integer, parameter :: max_string_len=2000

! -- Variables

      INTEGER, PARAMETER             :: MAXTERM=500    ! Maximum elements in an equation
      INTEGER, PARAMETER             :: MAXEXP=10      ! Maximum exponents in an equation
      INTEGER                        :: NUMEQN=0       ! Number of equations
      INTEGER                        :: IORDER(MAXTERM)! Integer work array
      INTEGER                        :: MTYPE(MAXTERM) ! Integer work array
      INTEGER                        :: MTYPE1(MAXTERM)! Integer work array
      INTEGER                        :: MAX_EQN_LENGTH=MAX_STRING_LEN ! Maximum length of eqn text
      INTEGER                        :: LASTJPAR=1     ! Last parameter found in an equation
      CHARACTER (LEN=MAX_STRING_LEN) :: BTEXT          ! Temporary text storage
      CHARACTER (LEN=25)             :: BTERM(MAXTERM) ! Temporary equation storage
      CHARACTER (LEN=25)             :: ATERM(MAXTERM) ! Temporary equation storage
      CHARACTER (LEN=25)             :: CTERM(MAXTERM) ! Temporary equation storage
      DOUBLE PRECISION               :: DTERM(MAXTERM) ! Storage of numbers in equation
      DOUBLE PRECISION               :: ETERM(MAXTERM) ! Storage of numbers in equation
      LOGICAL                        :: LTERM(MAXTERM) ! Storage of logical terms in equation
      LOGICAL                        :: OTERM(MAXTERM) ! Storage of logical terms in equation
      TYPE (COMPRESSED_EQUATION),ALLOCATABLE  :: COMPEQN(:)     ! Compressed storage for equations

      INTEGER                              :: NTERM       ! number active terms in aterm()
      INTEGER, PARAMETER                   :: NOPER=15    ! number of operators
      INTEGER, PARAMETER                   :: LOPER=8     ! number of logical operators
      CHARACTER, DIMENSION(NOPER)          :: OPERAT      ! operator array
      CHARACTER, DIMENSION(LOPER)          :: LOPERAT     ! logical operators
      CHARACTER (LEN=5), DIMENSION(LOPER)  :: LOPERATFULL ! unabbreviated logical operators
      INTEGER, PARAMETER                   :: NFUNCT=19   ! number of functions
      CHARACTER (LEN=6), DIMENSION(NFUNCT) :: FUNCT       ! function array

      DATA FUNCT /'abs   ','acos  ','asin  ','atan  ','cos   ','cosh  ',   &
         'exp   ','log   ','log10 ','sin   ','sinh  ','sqrt  ','tan   ',   &
         'tanh  ','neg   ','pos   ','min   ','max   ','mod   '/


! -- Visible subprograms

      PUBLIC EQN_INI,                           &
             EQN_INSTALL,                       &
             EQN_EVALUATE,                      &
             EQN_LINEAR_COEFFS,                 &
             EQN_GETNAME,                       &
             EQN_CLN,                           &
             EQN_GET_MESSAGE_STRING

! -- Subroutines

CONTAINS


SUBROUTINE EQN_GET_MESSAGE_STRING(IFAIL,ASTRING)

       IMPLICIT NONE

       INTEGER, INTENT(OUT)            :: IFAIL
       CHARACTER*(*), INTENT(OUT)      :: ASTRING

       IFAIL=0
       ASTRING=AMESSAGE

       RETURN

END SUBROUTINE EQN_GET_MESSAGE_STRING


SUBROUTINE EQN_INI(IFAIL,NUM_EQN)

! -- Subroutine EQN_INI initializes the equation module.

       IMPLICIT NONE

       INTEGER, INTENT(OUT)           :: IFAIL    ! indicates error condition
       INTEGER, INTENT(IN)            :: NUM_EQN  ! number of evaluatable equations

       INTEGER                        :: IERR,I

       ERRSUB='Error in subroutine EQN_INI:'

       IFAIL=0

! -- Allocate memory

       IF(NUM_EQN.LT.0)THEN
         WRITE(AMESSAGE,20) TRIM(ERRSUB)
20       FORMAT(A,' attempt to initialise with negative number of equations.')
         IFAIL=1
         RETURN
       END IF
       NUMEQN=NUM_EQN
       IF(NUMEQN.GT.0)THEN
         ALLOCATE (COMPEQN(NUMEQN),STAT=IERR)
         IF(IERR.NE.0)THEN
           WRITE(AMESSAGE,10) TRIM(ERRSUB)
10         FORMAT(A,' cannot allocate sufficient memory to continue.')
           IFAIL=1
           RETURN
         END IF
         DO I=1,NUMEQN
           COMPEQN(I)%NUMTERM=0
         END DO
       END IF

       MTYPE=0            ! mtype is an array
       DTERM=0.0D0        ! dterm is an array
       LTERM=.TRUE.       ! lterm is an array
       ATERM=' '

! -- Initialise operator arrays

       OPERAT(1)='^'
       OPERAT(2)='/'
       OPERAT(3)='*'
       OPERAT(4)='-'
       OPERAT(5)='+'
       OPERAT(6)='('
       OPERAT(7)=')'


       LOPERATFULL(1) = '.lt.'
       LOPERAT(1)     = CHAR(200)    ! .lt.
       LOPERATFULL(2) = '.le.'
       LOPERAT(2)     = CHAR(201)    ! .le.
       LOPERATFULL(3) = '.eq.'
       LOPERAT(3)     = CHAR(202)    ! .eq.
       LOPERATFULL(4) = '.gt.'
       LOPERAT(4)     = CHAR(203)    ! .gt.
       LOPERATFULL(5) = '.ge.'
       LOPERAT(5)     = CHAR(204)    ! .ge.
       LOPERATFULL(6) = '.ne.'
       LOPERAT(6)     = CHAR(205)    ! .ne.
       LOPERATFULL(7) = '.and.'
       LOPERAT(7)     = CHAR(206)    ! .and.
       LOPERATFULL(8) = '.or.'
       LOPERAT(8)     = CHAR(207)    ! .or.
       DO I=1,8
         OPERAT(I+7)=LOPERAT(I)
       END DO

       RETURN

END SUBROUTINE EQN_INI



SUBROUTINE EQN_CLN()

! -- Subroutine EQN_CLN de-allocates equation memory.

       IMPLICIT NONE
       INTEGER       :: IERR,N,I

       IF(NUMEQN.GT.0)THEN
         DO I=1,NUMEQN
           N=COMPEQN(I)%NUMTERM
           IF(N.GT.0)THEN
             DEALLOCATE(COMPEQN(I)%ASTRING,COMPEQN(I)%PTERM, STAT=IERR)
             NULLIFY(COMPEQN(I)%ASTRING,COMPEQN(I)%PTERM)
           END IF
         END DO
         DEALLOCATE (COMPEQN,STAT=IERR)
       END IF

       RETURN

END SUBROUTINE EQN_CLN



SUBROUTINE EQN_COUNT_TERMS(IFAIL,IEQN,ISTERM,ATEXT)

! -- Subroutine EQN_COUNT_TERMS counts the number of times a term is repeated in an
!    equation. This subroutine is mainly used for detecting the presence of a parameter;
!    however it will detect certain operators, but no functions.

      IMPLICIT NONE

      INTEGER, INTENT(OUT)           :: IFAIL    ! error condition indicator
      INTEGER, INTENT(IN)            :: IEQN     ! equation number
      INTEGER, INTENT(OUT)           :: ISTERM   ! number of times the term occurs
      CHARACTER (LEN=*), INTENT(IN)  :: ATEXT    ! text containing term

      INTEGER                 :: I
      CHARACTER (LEN=12)      :: EQUATION_NAME
      CHARACTER (LEN=25)      :: ATEMP

      IFAIL=0
      ISTERM=0
      IF((IEQN.GT.NUMEQN).OR.(IEQN.LE.0))THEN
        WRITE(AMESSAGE,1)
1       FORMAT('Error in subroutine EQN_COUNT_TERMS: equation number is ',   &
        'out of range.')
        IFAIL=1
        RETURN
      END IF

      ATEMP=ATEXT
      CALL UTL_CASETRANS(ATEMP,'lo')
      CALL EQN_EXTRACT(IEQN,EQUATION_NAME)
      DO I=1,NTERM
        IF(ATEMP.EQ.ATERM(I))ISTERM=ISTERM+1
      END DO

      RETURN

END SUBROUTINE EQN_COUNT_TERMS


SUBROUTINE EQN_COUNT_TERMS_CURRENT(IFAIL,ISTERM,ATEXT)

! -- Subroutine EQN_COUNT_TERMS_CURRENT counts the number of times a term is repeated in an
!    equation. The current equation is assumed to be stored in the aterm array.
!    This subroutine is mainly used for detecting the presence of a particular parameter;
!    however it will detect certain operators, but no functions.

      IMPLICIT NONE

      INTEGER, INTENT(OUT)           :: IFAIL    ! error condition indicator
      INTEGER, INTENT(OUT)           :: ISTERM   ! number of times the term occurs
      CHARACTER (LEN=*), INTENT(IN)  :: ATEXT    ! text containing term

      INTEGER                 :: I
      CHARACTER (LEN=25)      :: ATEMP

      IFAIL=0
      ISTERM=0

      ATEMP=ATEXT
      CALL UTL_CASETRANS(ATEMP,'lo')
      DO I=1,NTERM
        IF(ATEMP.EQ.ATERM(I))ISTERM=ISTERM+1
      END DO

      RETURN

END SUBROUTINE EQN_COUNT_TERMS_CURRENT




SUBROUTINE EQN_PARSE(IFAIL,EQUATION_NAME,ATEXT)

! -- Subroutine EQN_PARSE creates an equation by parsing a text string.

       IMPLICIT NONE

       INTEGER, INTENT(OUT)           :: IFAIL    ! indicates error condition
       CHARACTER (LEN=*), INTENT(IN)  :: EQUATION_NAME  ! name of equation
       CHARACTER (LEN=*), INTENT(IN)  :: ATEXT    ! text containing the equation

       INTEGER                 :: ICOUNT,NB,I,JFAIL,IIFUN,ITERM,NEG,J
       CHARACTER (LEN=1)       :: AA
       CHARACTER (LEN=6)       :: AATERM
       CHARACTER (LEN=10)      :: BB
       CHARACTER (LEN=25)      :: ATEMP

      ERRSUB='Error in equation "'//TRIM(EQUATION_NAME)//'":'

      IFAIL=0

      NTERM=0
      DO I=1,MAXTERM
        ATERM(I)=' '
      END DO

      IF(ATEXT.EQ.' ')GO TO 9200              ! correct in par2par?
      IF(LEN_TRIM(ATEXT).GT.MAX_EQN_LENGTH)THEN
        WRITE(AMESSAGE,1) TRIM(ERRSUB)
1       FORMAT(A,' equation is too long.')
        GO TO 9020
      END IF

! --  A copy is made of the equation string.

      BTEXT=ADJUSTL(ATEXT)
      CALL UTL_CASETRANS(BTEXT,'lo')

! -- A check is made to see if brackets are balanced.

      ICOUNT=0
      NB=LEN_TRIM(BTEXT)
      DO 3 I=1,NB
        IF(BTEXT(I:I).EQ.'(')THEN
          ICOUNT=ICOUNT+1
        ELSE IF(BTEXT(I:I).EQ.')')THEN
          ICOUNT=ICOUNT-1
        END IF
3     CONTINUE
      IF(ICOUNT.NE.0)THEN
        WRITE(AMESSAGE,4) TRIM(ERRSUB)
4       FORMAT(A,' unbalanced parantheses.')
        GO TO 9020
      END IF

! -- Logical operators are replaced with their single-digit representation.

      CALL EQN_LOGICAL_REPLACE()

! -- Items of the equation are individually extracted and stored in the equation array.

      IF(INDEX(BTEXT,'=').NE.0)GO TO 9000
5     CONTINUE
      CALL EQN_NEXT_TERM(JFAIL,ATEMP)
      IF(JFAIL.LT.0)THEN
        GO TO 50
      ELSE
        NTERM=NTERM+1
        IF(NTERM.GT.MAXTERM)GO TO 9100
        ATERM(NTERM)=ATEMP
        GO TO 5
      END IF
50    CONTINUE

! -- Functions are now dealt with.

      IIFUN=0
      IF(NTERM.LE.2) GO TO 400
      DO 200 ITERM=2,NTERM
        IF(ATERM(ITERM)(1:1).EQ.'(')THEN
          AA=ATERM(ITERM-1)(1:1)
          IF((AA.EQ.'+').OR.(AA.EQ.'-').OR.(AA.EQ.'*').OR.         &
             (AA.EQ.'/').OR.(AA.EQ.'^').OR.(AA.EQ.'(')) GO TO 200
          DO J=1,LOPER
            IF(AA.EQ.LOPERAT(J)) GO TO 200
          END DO
          AATERM=ATERM(ITERM-1)(1:6)
          DO 70 J=1,NFUNCT
            IF(AATERM.EQ.FUNCT(J)) GO TO 80
70        CONTINUE
          WRITE(AMESSAGE,75) TRIM(ERRSUB),TRIM(ATERM(ITERM-1))
75        FORMAT(A,' illegal function name  "',A,'".')
          GO TO 9020
80        CALL UTL_i2a(J,BB)
          ATERM(ITERM-1)='~#str_'//TRIM(BB)
          IIFUN=IIFUN+1
        END IF
200   CONTINUE
      IF(IIFUN.EQ.0) GO TO 400

      DO 300 ITERM=1,NTERM
        IF(ATERM(ITERM)(1:6).EQ.'~#str_') THEN
          ATERM(ITERM+1)(1:1)=CHAR(220)
          ICOUNT=1
          DO 280 J=ITERM+1,NTERM
            IF(ATERM(J)(1:1).EQ.'(')THEN
              ICOUNT=ICOUNT+1
            ELSE IF(ATERM(J)(1:1).EQ.')')THEN
              ICOUNT=ICOUNT-1
              IF(ICOUNT.EQ.0)THEN
                ATERM(J)='~#fin_'
                GO TO 300
              END IF
            END IF
280       CONTINUE
        END IF
300   CONTINUE

      CALL EQN_COMPRESS()

400   CONTINUE

! -- If the last item is an operator then the expression is invalid.

      AA=ATERM(NTERM)(1:1)
      IF((AA.EQ.'+').OR.(AA.EQ.'-').OR.(AA.EQ.'/').OR.(AA.EQ.'*').OR.   &
      (AA.EQ.'^')) GO TO 9000
      DO J=1,LOPER
        IF(AA.EQ.LOPERAT(J)) GO TO 9000
      END DO

! -- The "-" and the "+" signs are expanded as a function if appropriate.

490   CONTINUE
      DO 500 ITERM=1,NTERM
        IF((ATERM(ITERM)(1:1).EQ.'-').OR.     &
           (ATERM(ITERM)(1:1).EQ.'+'))THEN
          IF(ATERM(ITERM)(1:1).EQ.'+')THEN
            NEG=0
          ELSE
            NEG=1
          END IF
          IF(ITERM.EQ.1) THEN
            IF(NTERM.EQ.MAXTERM) GO TO 9100
            CALL EQN_EXPAND_NEG(IFAIL,ITERM,NEG)
            IF(IFAIL.NE.0) GO TO 9000
            GO TO 490
          ELSE IF(ATERM(ITERM-1)(1:6).EQ.'~#str_')THEN
            IF(NTERM.EQ.MAXTERM) GO TO 9100
            CALL EQN_EXPAND_NEG(IFAIL,ITERM,NEG)
            IF(IFAIL.NE.0) GO TO 9000
            GO TO 490
          ELSE
            AA=ATERM(ITERM-1)(1:1)
            IF((AA.EQ.'(').OR.(AA.EQ.'+').OR.(AA.EQ.'-').OR.     &
               (AA.EQ.'*').OR.(AA.EQ.'/').OR.(AA.EQ.'^').or.(aa.eq.','))THEN
               IF(NTERM.EQ.MAXTERM) GO TO 9100
               CALL EQN_EXPAND_NEG(IFAIL,ITERM,NEG)
               IF(IFAIL.NE.0) GO TO 9000
               GO TO 490
            ELSE
              DO J=1,LOPER
                IF(AA.EQ.LOPERAT(J))THEN
                  IF(NTERM.EQ.MAXTERM) GO TO 9100
                  CALL EQN_EXPAND_NEG(IFAIL,ITERM,NEG)
                  IF(IFAIL.NE.0) GO TO 9000
                  GO TO 490
                END IF
              END DO
            END IF
          END IF
        END IF
500   CONTINUE

      RETURN

9000  WRITE(AMESSAGE,9010) TRIM(ERRSUB)
9010  FORMAT(A,' illegal equation syntax.')
      GO TO 9020
9100  WRITE(AMESSAGE,9110) TRIM(ERRSUB)
9110  FORMAT(A,' to many items in expression.')
      GO TO 9020
9200  WRITE(AMESSAGE,9210) TRIM(ERRSUB)
9210  FORMAT(A,' equation is empty.')
      GO TO 9020

9020  IFAIL=1

      RETURN

END SUBROUTINE EQN_PARSE



SUBROUTINE EQN_INSTALL(IFAIL,IEQN,EQUATION_NAME,ATEXT)

! -- Subroutine EQN_INSTALL installs an equation and stores it in compressed form.

      IMPLICIT NONE

      INTEGER, INTENT(OUT)           :: IFAIL    ! indicates error condition
      INTEGER, INTENT(IN)            :: IEQN     ! equation number
      CHARACTER (LEN=*), INTENT(IN)  :: EQUATION_NAME  ! name of equation
      CHARACTER (LEN=*), INTENT(IN)  :: ATEXT    ! text containing the equation

      CHARACTER (LEN=5)              :: ANUM

      IFAIL=0
      ERRSUB='Error in subroutine EQN_INSTALL:'

      IF(IEQN.GT.NUMEQN)THEN
        WRITE(AMESSAGE,1) TRIM(ERRSUB)
1       FORMAT(A,' equation number is greater than the number of allocated equations.')
        IFAIL=1
        RETURN
      ELSE IF(IEQN.LT.1)THEN
        WRITE(AMESSAGE,2) TRIM(ERRSUB)
2       FORMAT(A,' the provided equation number is less than 1.')
        IFAIL=1
        RETURN
      END IF
      IF(COMPEQN(IEQN)%NUMTERM.NE.0)THEN
        CALL UTL_i2a(IEQN,ANUM)
        WRITE(AMESSAGE,3) TRIM(ERRSUB),TRIM(ANUM)
3       FORMAT(A,' the number "',A,'" has already been allocated to another equation.')
        IFAIL=-1
        RETURN
      END IF

      CALL EQN_PARSE(IFAIL,EQUATION_NAME,ATEXT)
      IF(IFAIL.NE.0)RETURN

      CALL EQN_STORE(IFAIL,EQUATION_NAME,IEQN)
      IF(IFAIL.NE.0) THEN
        WRITE(AMESSAGE,10) TRIM(ERRSUB)
10      FORMAT(A,' cannot allocate sufficient memory to store equations.')
        RETURN
      END IF

      RETURN

END SUBROUTINE EQN_INSTALL



SUBROUTINE EQN_LINEAR_COEFFS(IFAIL,EQUATION_NAME,ATEXT,NVAR,NPE,AVAR,ITRANS,RVAR,  &
      CONST_TERM,WORK,XROW)

! -- Subroutine EQN_LINEAR_COEFFS parses a linear equation, and returns coefficients
!    and constant term.

      IMPLICIT NONE

      INTEGER, INTENT(OUT)           :: IFAIL        ! indicates error condition
      CHARACTER (LEN=*), INTENT(IN)  :: EQUATION_NAME  ! name of equation
      CHARACTER (LEN=*), INTENT(IN)  :: ATEXT        ! text containing the equation
      INTEGER, INTENT(IN)            :: NVAR         ! number of parameters
      INTEGER, INTENT(IN)            :: NPE          ! number of adjustable params
      CHARACTER (LEN=*), INTENT(IN)  :: AVAR(NVAR)   ! names of parameters
      INTEGER, INTENT(IN)            :: ITRANS(NVAR) ! param transformation state
      DOUBLE PRECISION, INTENT(IN)   :: RVAR(NVAR)   ! param values
      DOUBLE PRECISION, INTENT(OUT)  :: CONST_TERM   ! constant term in eqn
      DOUBLE PRECISION, INTENT(OUT)  :: WORK(NVAR)   ! work array
      DOUBLE PRECISION, INTENT(OUT)  :: XROW(NPE)    ! row of jacobian


      LOGICAL                     :: LVAL
      INTEGER                     :: I,ICOUNT,IPAR,ITERM,IES,ITYPE,IEQN,NCTERM,ICOUNT1
      DOUBLE PRECISION            :: SLOPE1,SLOPE2,RDEN,RVAL1,RVAL2
      CHARACTER (LEN=1)           :: AA
      CHARACTER (LEN=10)          :: ANUM
      CHARACTER (LEN=12)          :: CTEXT
      CHARACTER (LEN=80)          :: LOCAL_ERRSUB


      IFAIL=0

      CALL EQN_PARSE(IFAIL,EQUATION_NAME,ATEXT)
      IF(IFAIL.NE.0)RETURN

      LOCAL_ERRSUB='Cannot evaluate parameter coefficients ' &
      //'for equation "'//TRIM(EQUATION_NAME)//'":'

! -- The linear equation is checked for illegal terms.

      AA='('
      CALL EQN_COUNT_TERMS_CURRENT(IFAIL,ICOUNT,AA)
      IF(ICOUNT.NE.0) GO TO 9400
      AA=')'
      CALL EQN_COUNT_TERMS_CURRENT(IFAIL,ICOUNT,AA)
      IF(ICOUNT.NE.0) GO TO 9400
      AA='/'
      CALL EQN_COUNT_TERMS_CURRENT(IFAIL,ICOUNT,AA)
      IF(ICOUNT.NE.0) GO TO 9400
      AA='^'
      CALL EQN_COUNT_TERMS_CURRENT(IFAIL,ICOUNT,AA)
      IF(ICOUNT.NE.0) GO TO 9400
      AA=','
      CALL EQN_COUNT_TERMS_CURRENT(IFAIL,ICOUNT,AA)
      IF(ICOUNT.NE.0) GO TO 9400
      DO I=1,NFUNCT
        IF((I.EQ.8).OR.(I.EQ.9).OR.(I.EQ.15).OR.(I.EQ.16))CYCLE
        CALL UTL_i2a(I,ANUM)
        CTEXT='~#str_'//TRIM(ANUM)
        CALL EQN_COUNT_TERMS_CURRENT(IFAIL,ICOUNT,CTEXT)
        IF(ICOUNT.NE.0) GO TO 9400
      END DO

! -- If any tied parameters appear in the equation, an error condition arises.

      DO IPAR=1,NVAR
        IF(ITRANS(IPAR).GE.0) CYCLE
        IF(ITRANS(IPAR).EQ.-10000) CYCLE
        CTEXT=AVAR(IPAR)
        CALL EQN_COUNT_TERMS_CURRENT(IFAIL,ICOUNT,CTEXT)
        IF(ICOUNT.NE.0) GO TO 9450
      END DO

! -- A copy is kept of the equation.

      NCTERM=NTERM
      DO ITERM=1,NTERM
        CTERM(ITERM)=ATERM(ITERM)
      END DO

! -- The equation is evaluated with all parameters set to zero (except fixed parameters).

      DO IPAR=1,NVAR
        IF(ITRANS(IPAR).EQ.-10000)THEN
          WORK(IPAR)=RVAR(IPAR)
        ELSE IF(ITRANS(IPAR).EQ.1)THEN
          WORK(IPAR)=1.0D0
        ELSE
          WORK(IPAR)=0.0D0
        END IF
      END DO

      IEQN=0
      CALL EQN_EVALUATE(IFAIL,IEQN,NVAR,AVAR,WORK,ITYPE,CONST_TERM,LVAL,-1.0D35,EQUATION_NAME)
      IF(IFAIL.NE.0) GO TO 9500
      IF(ITYPE.EQ.1) THEN
        WRITE(AMESSAGE,510) TRIM(LOCAL_ERRSUB)
510     FORMAT(A,' linear prior information equation must not have logical outcome.')
        GO TO 9020
      END IF
      NTERM=NCTERM
      DO ITERM=1,NTERM
        ATERM(ITERM)=CTERM(ITERM)
      END DO

! -- Now for each parameter cited in the equation we supply values of 1 and -1.

      ICOUNT1=0
      IES=0
      DO IPAR=1,NVAR
        IF(ITRANS(IPAR).LT.0) CYCLE
        IES=IES+1
        CTEXT=AVAR(IPAR)
        CALL EQN_COUNT_TERMS_CURRENT(IFAIL,ICOUNT,CTEXT)
        IF(ICOUNT.EQ.0) THEN
          XROW(IES)=0.0D0
          CYCLE
        END IF
        ICOUNT1=ICOUNT1+1
        IF(ITRANS(IPAR).EQ.1)THEN
          WORK(IPAR)=10.0D0
        ELSE
          WORK(IPAR)=1.0D0
        END IF
        CALL EQN_EVALUATE(IFAIL,IEQN,NVAR,AVAR,WORK,ITYPE,RVAL1,LVAL,-1.0D35,EQUATION_NAME)
        IF(IFAIL.NE.0) GO TO 9500
        NTERM=NCTERM
        DO ITERM=1,NTERM
          ATERM(ITERM)=CTERM(ITERM)
        END DO
        IF(ITRANS(IPAR).EQ.1)THEN
          WORK(IPAR)=0.1D0
        ELSE
          WORK(IPAR)=-1.0D0
        END IF
        CALL EQN_EVALUATE(IFAIL,IEQN,NVAR,AVAR,WORK,ITYPE,RVAL2,LVAL,-1.0D35,EQUATION_NAME)
        IF(IFAIL.NE.0) GO TO 9500
        NTERM=NCTERM
        DO ITERM=1,NTERM
          ATERM(ITERM)=CTERM(ITERM)
        END DO
        SLOPE1=RVAL1-CONST_TERM
        SLOPE2=CONST_TERM-RVAL2
        IF((SLOPE1.EQ.0.0D0).OR.(SLOPE2.EQ.0.0D0)) GO TO 9550
        RDEN=SLOPE1+SLOPE2
        IF(RDEN.EQ.0.0D0) GO TO 9550
        IF(ABS(SLOPE1-SLOPE2).GT.ABS(1.0D-10*RDEN)) GO TO 9600
        XROW(IES)=(RVAL1-RVAL2)*0.5D0
        IF(ITRANS(IPAR).EQ.1)THEN
          WORK(IPAR)=1.0D0
        ELSE
          WORK(IPAR)=0.0D0
        END IF
      END DO
      IF(ICOUNT1.EQ.0)THEN
        WRITE(AMESSAGE,520) TRIM(LOCAL_ERRSUB)
520     FORMAT(A,' equation cites no adjustable parameters.')
        GO TO 9020
      END IF

      RETURN

9400  WRITE(AMESSAGE,9410) TRIM(LOCAL_ERRSUB)
9410  FORMAT(A,' equation does not respect protocol for linear prior information.')
      GO TO 9020
9450  WRITE(AMESSAGE,9460) TRIM(LOCAL_ERRSUB)
9460  FORMAT(A,' tied parameter cannot appear in linear prior information equation.')
      GO TO 9020
9500  WRITE(AMESSAGE,9510) TRIM(LOCAL_ERRSUB)
9510  FORMAT(A,' equation does not respect protocol for linear prior information or ', &
      'does not respect the fact that at least some cited parameters are (or are not) ', &
      'log-transformed in inversion process.')
      GO TO 9020
9550  WRITE(AMESSAGE,9560) TRIM(LOCAL_ERRSUB)
9560  FORMAT(A,' equation is not a valid linear prior information equation.')
      GO TO 9020
9600  CONTINUE
      IF(ITRANS(IPAR).EQ.0)THEN
        WRITE(AMESSAGE,9610) TRIM(LOCAL_ERRSUB), TRIM(AVAR(IPAR))
9610    FORMAT(A,' equation is not linear with respect to parameter "',A,'".')
      ELSE
        WRITE(AMESSAGE,9620) TRIM(LOCAL_ERRSUB), TRIM(AVAR(IPAR))
9620    FORMAT(A,' equation is not linear with respect to the log of parameter "',A,'".')
      END IF
      GO TO 9020

9020  IFAIL=1

      RETURN

END SUBROUTINE EQN_LINEAR_COEFFS




SUBROUTINE EQN_EVALUATE(IFAIL,IEQN,NVAR,AVAR,RVAR,ITYPE,RVAL,LVAL,UNASSIGNED,ANAME)

! -- Subroutine EQN_EVALUATE evaluates an equation.

       IMPLICIT NONE

       INTEGER, INTENT(OUT)                 :: IFAIL ! signifies error conditionn
       INTEGER, INTENT(IN)                  :: IEQN  ! equation number
       INTEGER, INTENT(IN)                  :: NVAR  ! number of variables
       CHARACTER (LEN=*), INTENT(IN), DIMENSION(NVAR)   :: AVAR ! names of variables
       DOUBLE PRECISION, INTENT(IN), DIMENSION(NVAR)    :: RVAR ! values of variables
       INTEGER, INTENT(OUT)                 :: ITYPE ! whether logical or numerical output
       DOUBLE PRECISION, INTENT(OUT)        :: RVAL  ! real number produced by equation
       LOGICAL, INTENT(OUT)                 :: LVAL  ! logical output of equation
       DOUBLE PRECISION, OPTIONAL, INTENT(IN)  :: UNASSIGNED !value for which param unassigned
       CHARACTER (LEN=*), OPTIONAL, INTENT(IN) :: ANAME  ! name of equation

! -- Note: it is assumed that variable names are supplied in lower case.

       LOGICAL                 :: LVAL1,LTEMP1,LTEMP2
       INTEGER                 :: JFAIL,JERR,ITERM,MAXORD,ICOUNT,IOPER,I,K,J,  &
                                  NVEC,IM,M,IERR,ICHECK
       DOUBLE PRECISION        :: DTEMP1,DTEMP2,DVAL,DDVAL,DTEMP,DDTEMP1,DDTEMP2
       CHARACTER (LEN=1)       :: AA
       CHARACTER (LEN=6)       :: AFUNCT
       CHARACTER (LEN=12)      :: EQUATION_NAME

       IFAIL=0
       ITYPE=0

! -- The equation vector is copied to a temporary equation vector for evaluation,
!    as this is a destructive activity.

       IF(IEQN.EQ.0)THEN
         IF(.NOT.PRESENT(ANAME))THEN
           WRITE(AMESSAGE,10)
10         FORMAT('Error in call to subroutine EQN_EVALUATE: if IEQN is zero ', &
           'optional argument ANAME must be supplied.')
           IFAIL=1
           RETURN
         END IF
       ELSE IF((IEQN.LT.0).OR.(IEQN.GT.NUMEQN))THEN
         WRITE(AMESSAGE,15)
15       FORMAT('Error in call to subroutine EQN_EVALUATE: IEQN argument is out of range.')
         IFAIL=1
         RETURN
       ELSE IF(COMPEQN(IEQN)%NUMTERM.EQ.0)THEN
         WRITE(AMESSAGE,16)
16       FORMAT('Error in call to subroutine EQN_EVALUATE: IEQN argument references ', &
         'an uninstalled equation.')
         IFAIL=1
         RETURN
       END IF

       ICHECK=0
       IF(PRESENT(UNASSIGNED)) ICHECK=1

       IF(IEQN.GT.0)THEN
         CALL EQN_EXTRACT(IEQN,EQUATION_NAME)
       ELSE IF(IEQN.EQ.0)THEN
         EQUATION_NAME=ANAME
       END IF

       ERRSUB='Error evaluating equation "'//TRIM(EQUATION_NAME)//'":'
       DO ITERM=1,NTERM
         MTYPE(ITERM)=0
       END DO

 ! -- Variable values are substituted for their names.

      IF(NVAR.NE.0)THEN
        CALL EQN_PARTONUM(JFAIL,NVAR,AVAR,RVAR,EQUATION_NAME,ICHECK,UNASSIGNED)
        IF(JFAIL.NE.0) THEN
          IFAIL=1
          RETURN
        END IF
      END IF

! -- If there is only one term left, the expression has been evaluated.

100   CONTINUE

      IF(NTERM.EQ.1)THEN
        IF(MTYPE(1).EQ.0)THEN
          RVAL=DTERM(1)
        ELSE IF(MTYPE(1).EQ.1)THEN
          LVAL=LTERM(1)
          ITYPE=1
        END IF
        RETURN
      END IF

! -- If there are any numbers surrounded by brackets, then the brackets are
!    removed

      IF(NTERM.GE.3)THEN
        DO 150 ITERM=1,NTERM-2
          IF(ATERM(ITERM)(1:1).EQ.'(') THEN
            IF(ATERM(ITERM+2)(1:1).EQ.')')THEN
              ATERM(ITERM)(1:1)=CHAR(220)
              ATERM(ITERM+2)(1:1)=CHAR(220)
              CALL EQN_COMPRESS()
              GO TO 100
            END IF
          END IF
150     CONTINUE
      END IF

! -- Can any function evaluations now be done?

      IF(NTERM.GE.3)THEN
        DO 300 ITERM=1,NTERM-2
          IF(ATERM(ITERM)(1:6).EQ.'~#str_')THEN
            IF(ATERM(ITERM+2)(1:6).EQ.'~#fin_')THEN
              IF(MTYPE(ITERM+1).EQ.1) GO TO 9000
              CALL EQN_FUNCTION(JERR,ATERM(ITERM),DTERM(ITERM+1),DVAL)
              IF(JERR.NE.0)THEN
                AFUNCT=FUNCT(JERR)
                WRITE(AMESSAGE,170) TRIM(ERRSUB), TRIM(AFUNCT)
170             FORMAT(A,' cannot evaluate "',A,'" function in ',               &
                'expression because function argument is illegal or out of range.')
                GO TO 9999
              END IF
              ATERM(ITERM)(1:1)=CHAR(220)
              ATERM(ITERM+1)=' '
              DTERM(ITERM+1)=DVAL
              MTYPE(ITERM+1)=0
              ATERM(ITERM+2)(1:1)=CHAR(220)
              CALL EQN_COMPRESS()
              GO TO 100
            END IF
          END IF
300     CONTINUE
      END IF

! -- We see if a min/max/mod function evaluation can be done.

      IF(NTERM.GE.3)THEN
        DO 305 ITERM=1,NTERM
          IF((ATERM(ITERM)(1:8).EQ.'~#str_17').OR.        &
             (ATERM(ITERM)(1:8).EQ.'~#str_18').OR.        &
             (ATERM(ITERM)(1:8).EQ.'~#str_19'))THEN
             DO J=ITERM+1,NTERM
               AA=ATERM(J)(1:1)
               DO K=1,NOPER
                 IF(AA.EQ.OPERAT(K)) GO TO 305
               END DO
               IF(ATERM(J)(1:5).EQ.'~#str') GO TO 305
               IF(ATERM(J)(1:5).EQ.'~#fin')THEN
                 IF(J-ITERM.EQ.1) GO TO 9300
                 NVEC=0
                 IF(ATERM(ITERM)(7:8).EQ.'17')THEN
                   IM=1
                   DDVAL=1.0D100
                 ELSE IF(ATERM(ITERM)(7:8).EQ.'18')THEN
                   IM=2
                   DDVAL=-1.0D100
                 ELSE IF(ATERM(ITERM)(7:8).EQ.'19')THEN
                   IM=3
                 END IF
                 DO M=ITERM+1,J-1
                   IF(ATERM(M)(1:1).EQ.',') CYCLE
                   NVEC=NVEC+1
                   DTEMP=DTERM(M)
                   IF(IM.EQ.1)THEN
                     IF(DTEMP.LT.DDVAL)DDVAL=DTEMP
                   ELSE IF(IM.EQ.2)THEN
                     IF(DTEMP.GT.DDVAL)DDVAL=DTEMP
                   ELSE IF(IM.EQ.3)THEN
                     IF(NVEC.EQ.1)THEN
                       DDTEMP1=DTEMP
                     ELSE IF(NVEC.EQ.2)THEN
                       DDTEMP2=DTEMP
                       IF(DDTEMP2.EQ.0.0D0)THEN
                         WRITE(AMESSAGE,304) TRIM(ERRSUB)
304                      FORMAT(A,' second argument of mod function must not be zero.')
                         GO TO 9999
                       END IF
                     ELSE
                       GO TO 9400
                     END IF
                   END IF
                 END DO
                 IF(NVEC.EQ.0) GO TO 9300
                 IF(IM.EQ.3)THEN
                   IF(NVEC.EQ.1) GO TO 9300
                   DDVAL=MOD(DDTEMP1,DDTEMP2)
                 END IF
                 ATERM(ITERM)=' '
                 DTERM(ITERM)=DDVAL
                 DO M=ITERM+1,J
                   ATERM(M)(1:1)=CHAR(220)
                 END DO
                 CALL EQN_COMPRESS()
                 GO TO 100
               END IF
             END DO
          END IF
305     CONTINUE
      END IF

! -- Operators are now ranked by their level of nesting.

      MAXORD=0
      DO 320 ITERM=1,NTERM
        IORDER(ITERM)=0
320   CONTINUE
      ICOUNT=1
      DO 350 ITERM=1,NTERM
        AA=ATERM(ITERM)(1:1)
        IF(AA.EQ.'(')THEN
          ICOUNT=ICOUNT+1
        ELSE IF(AA.EQ.')')THEN
          ICOUNT=ICOUNT-1
        ELSE IF(ATERM(ITERM)(1:6).EQ.'~#str_')THEN
          ICOUNT=ICOUNT+1
        ELSE IF(ATERM(ITERM)(1:6).EQ.'~#fin_')THEN
          ICOUNT=ICOUNT-1
        ELSE IF((AA.EQ.'+').OR.(AA.EQ.'-').OR.(AA.EQ.'*').OR.    &
        (AA.EQ.'/').OR.(AA.EQ.'^'))THEN
          IORDER(ITERM)=ICOUNT
          IF(ICOUNT.GT.MAXORD)MAXORD=ICOUNT
        ELSE
          DO J=1,LOPER
            IF(AA.EQ.LOPERAT(J))THEN
              IORDER(ITERM)=ICOUNT
              IF(ICOUNT.GT.MAXORD)MAXORD=ICOUNT
              GO TO 330
            END IF
          END DO
          IF(MTYPE(ITERM).EQ.0)THEN
            IORDER(ITERM)=-1            ! It must be a number.
          ELSE IF(MTYPE(ITERM).EQ.1)THEN
            IORDER(ITERM)=-2            ! Must be a logical value.
          END IF
330       CONTINUE
        END IF
350   CONTINUE

! -- We now look for a calculation to do, starting at the highest level.

      IF(NTERM.GE.3)THEN
        DO 400 I=MAXORD,1,-1
          DO 390 IOPER=1,NOPER
            IF((IOPER.EQ.6).OR.(IOPER.EQ.7)) GO TO 390
            DO 380 ITERM=NTERM-1,2,-1   ! Reverse order for multiple exponentiation
              IF(IORDER(ITERM).EQ.I)THEN   !It is an operator
                IF(ATERM(ITERM)(1:1).EQ.OPERAT(IOPER))THEN
                  IF((IORDER(ITERM-1).EQ.-1).AND.                         &
                     (IORDER(ITERM+1).EQ.-1))THEN    !numbers either side
                    DTEMP1=DTERM(ITERM-1)
                    DTEMP2=DTERM(ITERM+1)
                    IF(IOPER.EQ.1)THEN
                      IF(DTEMP1.LT.0.0)THEN
                        IF(DTEMP2.NE.FLOAT(NINT(DTEMP2)))THEN
                          WRITE(AMESSAGE,384) TRIM(ERRSUB)
384                       FORMAT(A,' negative number raised to ',        &
                          'fractional power.')
                          GO TO 9999
                        END IF
                      END IF
                      DVAL=DTEMP1**DTEMP2
                      MTYPE(ITERM)=0
                    ELSE IF(IOPER.EQ.3)THEN
                      DVAL=DTEMP1*DTEMP2
                      MTYPE(ITERM)=0
                    ELSE IF(IOPER.EQ.2)THEN
                      IF(DTEMP2.EQ.0.0D0) THEN
                        WRITE(AMESSAGE,385) TRIM(ERRSUB)
385                     FORMAT(A,' divide by zero.')
                        GO TO 9999
                      END IF
                      DVAL=DTEMP1/DTEMP2
                      MTYPE(ITERM)=0
                    ELSE IF(IOPER.EQ.5)THEN
                      DVAL=DTEMP1+DTEMP2
                      MTYPE(ITERM)=0
                    ELSE IF(IOPER.EQ.4)THEN
                      DVAL=DTEMP1-DTEMP2
                      MTYPE(ITERM)=0
                    ELSE IF(IOPER.EQ.8)THEN
                      LVAL1=(DTEMP1.LT.DTEMP2)
                      MTYPE(ITERM)=1
                    ELSE IF(IOPER.EQ.9)THEN
                      LVAL1=(DTEMP1.LE.DTEMP2)
                      MTYPE(ITERM)=1
                    ELSE IF(IOPER.EQ.10)THEN
                      LVAL1=(DTEMP1.EQ.DTEMP2)
                      MTYPE(ITERM)=1
                    ELSE IF(IOPER.EQ.11)THEN
                      LVAL1=(DTEMP1.GT.DTEMP2)
                      MTYPE(ITERM)=1
                    ELSE IF(IOPER.EQ.12)THEN
                      LVAL1=(DTEMP1.GE.DTEMP2)
                      MTYPE(ITERM)=1
                    ELSE IF(IOPER.EQ.13)THEN
                      LVAL1=(DTEMP1.NE.DTEMP2)
                      MTYPE(ITERM)=1
                    ELSE IF((IOPER.EQ.14).OR.(IOPER.EQ.15))THEN
                      GO TO 9200
                    END IF
                    IF(MTYPE(ITERM).EQ.0)THEN
                      ATERM(ITERM)=' '
                      DTERM(ITERM)=DVAL
                    ELSE
                      LTERM(ITERM)=LVAL1
                      ATERM(ITERM)=' '
                    END IF
                    ATERM(ITERM-1)(1:1)=CHAR(220)
                    ATERM(ITERM+1)(1:1)=CHAR(220)
                    CALL EQN_COMPRESS()
                    GO TO 100
                  ELSE IF((IORDER(ITERM-1).EQ.-2).AND.   &
                          (IORDER(ITERM+1).EQ.-2))THEN  ! logical values either side
                    LTEMP1=LTERM(ITERM-1)
                    LTEMP2=LTERM(ITERM+1)
                    IF(IOPER.EQ.14)THEN
                      LVAL1=(LTEMP1.AND.LTEMP2)
                    ELSE IF(IOPER.EQ.15)THEN
                      LVAL1=(LTEMP1.OR.LTEMP2)
                    ELSE
                      GO TO 9000
                    END IF
                    MTYPE(ITERM)=1
                    ATERM(ITERM)=' '
                    LTERM(ITERM)=LVAL1
                    ATERM(ITERM-1)(1:1)=CHAR(220)
                    ATERM(ITERM+1)(1:1)=CHAR(220)
                    CALL EQN_COMPRESS()
                    GO TO 100
                  ELSE
                    GO TO 9100
                  END IF
                END IF
              END IF
380         CONTINUE
390       CONTINUE
400     CONTINUE
      END IF
      WRITE(AMESSAGE,410) TRIM(ERRSUB)
410   FORMAT(A,' illegal expression.')
      GO TO 9999

9000  WRITE(AMESSAGE,9010) TRIM(ERRSUB)
9010  FORMAT(A,' logical value occurs where number expected.')
      GO TO 9999
9100  WRITE(AMESSAGE,9110) TRIM(ERRSUB)
9110  FORMAT(A,' number and/or logical value in unexpected location.')
      GO TO 9999
9200  WRITE(AMESSAGE,9210) TRIM(ERRSUB)
9210  FORMAT(A,' number occurs where logical value expected.')
      GO TO 9999
9300  WRITE(AMESSAGE,9310) TRIM(ERRSUB)
9310  FORMAT(A,' illegal argument to max, min or mod function.')
      GO TO 9999
9400  WRITE(AMESSAGE,9410) TRIM(ERRSUB)
9410  FORMAT(A,' illegal argument to mod function.')
      GO TO 9999
9999  IFAIL=1
      RETURN

END SUBROUTINE EQN_EVALUATE


SUBROUTINE EQN_GETNAME(IFAIL,IEQN,EQUATION_NAME)

! -- Subroutine EQN_GETNAME retrieves the name of an equation, given its number.

       IMPLICIT NONE

       INTEGER, INTENT(OUT)                 :: IFAIL         ! signifies error conditionn
       INTEGER, INTENT(IN)                  :: IEQN          ! equation number
       CHARACTER (LEN=*), INTENT(OUT)       :: EQUATION_NAME ! name of equation

       IFAIL=0

       IF((IEQN.LT.1).OR.(IEQN.GT.NUMEQN))THEN
         WRITE(AMESSAGE,15)
15       FORMAT('Error in call to subroutine EQN_GETNAME: IEQN argument is out of range.')
         IFAIL=1
         RETURN
       ELSE IF(COMPEQN(IEQN)%NUMTERM.EQ.0)THEN
         WRITE(AMESSAGE,16)
16       FORMAT('Error in call to subroutine EQN_GETNAME: IEQN argument references ', &
         'an uninstalled equation.')
         IFAIL=1
         RETURN
       END IF

       EQUATION_NAME=COMPEQN(IEQN)%NAME

       RETURN

END SUBROUTINE EQN_GETNAME



SUBROUTINE EQN_NEXT_TERM(IFAIL,ATEMP)

! -- Subroutine EQN_NEXT_TERM splits off the next term of an expression.

      IMPLICIT NONE
      INTEGER, INTENT(OUT)             :: IFAIL   ! finish indicator
      CHARACTER (LEN=*), INTENT(OUT)   :: ATEMP   ! the next equation item

      INTEGER              :: I,J,NB,K,L,IERR
      DOUBLE PRECISION     :: DVAL
      CHARACTER (LEN=10)   :: AFMT

      ATEMP=' '
      IFAIL=0
      IF(BTEXT.EQ.' ')THEN
        IFAIL=-1
        RETURN
      END IF
      DO 10 I=1,NOPER
        IF(BTEXT(1:1).EQ.OPERAT(I))THEN            ! check in par2par
          ATEMP(1:1)=OPERAT(I)
          BTEXT=BTEXT(2:)
          BTEXT=ADJUSTL(BTEXT)
          GO TO 20
        END IF
10    CONTINUE
      GO TO 50

20    IF(ATEMP(1:1).EQ.'*')THEN
        IF(BTEXT(1:1).EQ.'*')THEN
          ATEMP(1:1)='^'
          BTEXT=BTEXT(2:)
          BTEXT=ADJUSTL(BTEXT)
        END IF
      END IF
      RETURN

50    CONTINUE

! -- Commas are extracted.

      IF(BTEXT(1:1).EQ.',')THEN
        ATEMP(1:1)=','
        BTEXT=BTEXT(2:)
        BTEXT=ADJUSTL(BTEXT)
        RETURN
      END IF

! -- Now operators.

      NB=LEN_TRIM(BTEXT)
      DO 100 I=2,NB
        IF(BTEXT(I:I).EQ.',') GO TO 120
        DO 90 J=1,NOPER
          IF(BTEXT(I:I).EQ.OPERAT(J)) THEN
            IF(I.LE.2) GO TO 120
            IF(I.EQ.NB) GO TO 120
            IF((J.NE.4).AND.(J.NE.5))GO TO 120
            IF((BTEXT(I-1:I-1).NE.'E').AND.(BTEXT(I-1:I-1).NE.'e').AND.   &
               (BTEXT(I-1:I-1).NE.'D').AND.(BTEXT(I-1:I-1).NE.'d'))       &
               GO TO 120
            DO 190 K=I+1,NB
              DO 180 L=1,NOPER
                IF(BTEXT(K:K).EQ.OPERAT(L))GO TO 200
180           CONTINUE
190         CONTINUE
            K=NB+1
200         K=K-1
            AFMT='(f    .0)'
            WRITE(AFMT(3:6),'(i4)')K
            READ(BTEXT(1:K),AFMT,IOSTAT=IERR) DVAL
            if(IERR.NE.0) go to 120
            ATEMP=BTEXT(1:K)              ! correct par2par
            BTEXT=BTEXT(K+1:)             ! correct par2par
            IF(BTEXT.NE.' ')BTEXT=ADJUSTL(BTEXT)      ! correct par2par
            RETURN                        ! correct par2par
!!            GO TO 100                   ! correct par2par
          END IF
!!          IF(BTEXT(I:I).EQ.OPERAT(J)) GO TO 120     ! correct par2par
90      CONTINUE
100   CONTINUE
      ATEMP=BTEXT(1:MIN(25,NB))
      BTEXT=' '
      RETURN

120   ATEMP=BTEXT(1:I-1)
      BTEXT=BTEXT(I:)
      BTEXT=ADJUSTL(BTEXT)                ! correct par2par
      RETURN

END SUBROUTINE EQN_NEXT_TERM


SUBROUTINE EQN_COMPRESS()

! -- Subroutine EQN_COMPRESS removes "dead terms" from an expression.

      IMPLICIT NONE

      INTEGER                     :: I,JTERM

      DO 100 I=1,NTERM
        BTERM(I)=ATERM(I)
        MTYPE1(I)=MTYPE(I)
        ETERM(I)=DTERM(I)
        OTERM(I)=LTERM(I)
100   CONTINUE
      JTERM=0
      DO 200 I=1,NTERM
        IF(BTERM(I)(1:1).NE.CHAR(220))THEN
          JTERM=JTERM+1
          ATERM(JTERM)=BTERM(I)
          MTYPE(JTERM)=MTYPE1(I)
          DTERM(JTERM)=ETERM(I)
          LTERM(JTERM)=OTERM(I)
        END IF
200   CONTINUE
      NTERM=JTERM

      RETURN

END SUBROUTINE EQN_COMPRESS



SUBROUTINE EQN_EXPAND_NEG(IFAIL,ITERM,NEG)

! -- Subroutine EQN_EXPAND_NEG expands a "-" or "+" sign into a function.

      IMPLICIT NONE

      INTEGER, INTENT(OUT)                :: IFAIL  ! error indicator
      INTEGER, INTENT(IN)                 :: ITERM  ! current term number
      INTEGER, INTENT(IN)                 :: NEG    ! negative/positive indicator

      INTEGER                             :: ICOUNT,JTERM,I,JCOUNT,LASTEXP
      INTEGER                             :: LEVEL(MAXEXP)

      IFAIL=0
      IF(NEG.EQ.1)THEN
        ATERM(ITERM)='~#str_15'
      ELSE
        ATERM(ITERM)='~#str_16'
      END IF
      ICOUNT=0
      JCOUNT=0
      JTERM=ITERM+1
      DO
        IF((ATERM(JTERM)(1:1).EQ.'-').OR.            &
           (ATERM(JTERM)(1:1).EQ.'+'))GO TO 100
        IF(ATERM(JTERM)(1:1).EQ.'(')THEN
          ICOUNT=ICOUNT+1
        ELSE IF(ATERM(JTERM)(1:1).EQ.')')THEN
          ICOUNT=ICOUNT-1
        ELSE IF(ATERM(JTERM)(1:6).EQ.'~#str_')THEN
          ICOUNT=ICOUNT+1
        ELSE IF(ATERM(JTERM)(1:6).EQ.'~#fin_')THEN
          ICOUNT=ICOUNT-1
        END IF
        IF(JTERM.NE.NTERM)THEN
          IF(ATERM(JTERM+1)(1:1).EQ.'^')THEN
            JCOUNT=JCOUNT+1
            ICOUNT=ICOUNT+1
            LASTEXP=JTERM+1
            LEVEL(JCOUNT)=ICOUNT
            GO TO 100
          END IF
        END IF
39      CONTINUE
        IF(JCOUNT.NE.0)THEN
          IF(ICOUNT.EQ.LEVEL(JCOUNT))THEN
            IF(JTERM.NE.LASTEXP)THEN
              ICOUNT=ICOUNT-1
              JCOUNT=JCOUNT-1
              GO TO 39
            END IF
          END IF
        END IF
        IF(ICOUNT.LT.0)THEN                ! Is this correct? Test ...+(-4)+...
          IFAIL=1
          RETURN
        END IF
        IF(ICOUNT.EQ.0)THEN
          IF(JTERM.LT.NTERM)THEN
            DO 40 I=NTERM,JTERM+1,-1
              ATERM(I+1)=ATERM(I)
40          CONTINUE
          END IF
          ATERM(JTERM+1)='~#fin_'
          NTERM=NTERM+1
          RETURN
        END IF
100     CONTINUE
        JTERM=JTERM+1
        IF(JTERM.GT.NTERM) EXIT
      END DO

      RETURN

END SUBROUTINE EQN_EXPAND_NEG



SUBROUTINE EQN_PARTONUM(IFAIL,NVAR,AVAR,RVAR,EQUATION_NAME,ICHECK,UNASSIGNED)

! -- Subroutine EQN_PARTONUM replaces variable names with their values.

      IMPLICIT NONE

      INTEGER, INTENT(OUT)            :: IFAIL  ! error indicator
      INTEGER, INTENT(IN)             :: NVAR   ! number of parameters
      CHARACTER (LEN=*), INTENT(IN), DIMENSION(NVAR)  :: AVAR   ! variable names
      DOUBLE PRECISION, INTENT(IN), DIMENSION(NVAR)   :: RVAR   ! variable values
      CHARACTER (LEN=*), INTENT(IN)   :: EQUATION_NAME
      INTEGER, INTENT(IN)             :: ICHECK ! set to 1 to check previous assignment
      DOUBLE PRECISION, INTENT(IN)    :: UNASSIGNED ! value deemed to be unassigned

      INTEGER                         :: ITERM,J,NB,JERR
      DOUBLE PRECISION                :: DTEMP
      CHARACTER (LEN=25)              :: AAVAR

      ERRSUB='Cannot evaluate equation "'//TRIM(EQUATION_NAME)//'":'

      IFAIL=0
      DO 200 ITERM=1,NTERM
         IF(ATERM(ITERM)(1:2).EQ.'~#') GO TO 200
         IF(ATERM(ITERM)(1:1).EQ.',') go to 200
         DO 20 J=1,NOPER
           IF(ATERM(ITERM)(1:1).EQ.OPERAT(J)) GO TO 200
20       CONTINUE
         AAVAR=ATERM(ITERM)
         NB=LEN_TRIM(AAVAR)
         IF(INDEX(AAVAR(1:NB),' ').NE.0)THEN
           WRITE(AMESSAGE,30) TRIM(ERRSUB),TRIM(AAVAR)
30         FORMAT(A,' variable name "',A,'" cannot include blank character.')
           IFAIL=1
           RETURN
         END IF
         CALL UTL_a2d(JERR,AAVAR,DTEMP)
         IF(JERR.EQ.0) THEN
           if((aavar(1:1).eq.'d').or.(aavar(1:1).eq.'e').or.    &
              (aavar(1:1).eq.'g').or.(aavar(1:1).eq.'q')) go to 31
           DTERM(ITERM)=DTEMP
           GO TO 200
         END IF
31       continue
         IF(NVAR.GT.0)THEN               ! alter in other modules.
           DO 40 J=LASTJPAR,NVAR
             IF(AAVAR.EQ.AVAR(J))GO TO 50
40         CONTINUE
           IF(LASTJPAR.NE.1)THEN
             DO 41 J=LASTJPAR-1,1,-1
               IF(AAVAR.EQ.AVAR(J)) GO TO 50
41           CONTINUE
           END IF
           WRITE(AMESSAGE,45) TRIM(ERRSUB),TRIM(AAVAR)
45         FORMAT(A,' unknown variable "',A,'".')
           IFAIL=1
           RETURN
         END IF
50       LASTJPAR=J
         DTERM(ITERM)=RVAR(J)
         IF(ICHECK.EQ.1)THEN
           IF(DTERM(ITERM).EQ.UNASSIGNED)THEN
             WRITE(AMESSAGE,46) TRIM(ERRSUB),TRIM(AAVAR)
46           FORMAT(A,' a value has not been assigned to the variable "',A,'".')
             IFAIL=1
             RETURN
           END IF
         END IF
         ATERM(ITERM)=' '
200   CONTINUE

      RETURN

END SUBROUTINE EQN_PARTONUM



SUBROUTINE EQN_FUNCTION(JERR,ATERM1,DTERM,DVAL)

! -- Subroutine EQN_FUNCTION evaluates a function.

      IMPLICIT NONE

      INTEGER, INTENT(OUT)                 :: JERR     ! error signifier
      CHARACTER (LEN=*), INTENT(INOUT)     :: ATERM1   ! eqn term containing function name
      DOUBLE PRECISION, INTENT(IN)         :: DTERM    ! function argument
      DOUBLE PRECISION, INTENT(OUT)        :: DVAL     ! function output

      INTEGER                              :: IFN,IFAIL
      DOUBLE PRECISION                     :: DTEMP

! -- First we find out which function we are evaluating.

      JERR=0
      ATERM1(1:6)=' '
      ATERM1=ADJUSTL(ATERM1)
      CALL UTL_a2i(IFAIL,ATERM1,IFN)
      IF(IFAIL.NE.0) GO TO 9000
      DTEMP=DTERM
      IF(IFN.EQ.1)THEN
        DVAL=ABS(DTEMP)
      ELSE IF(IFN.EQ.2)THEN
        IF((DTEMP.GT.1.0D0).OR.(DTEMP.LT.-1.0D0))GO TO 9000
        DVAL=ACOS(DTEMP)
      ELSE IF(IFN.EQ.3)THEN
        IF((DTEMP.GT.1.0D0).OR.(DTEMP.LT.-1.0D0))GO TO 9000
        DVAL=ASIN(DTEMP)
      ELSE IF(IFN.EQ.4)THEN
        DVAL=ATAN(DTEMP)
      ELSE IF(IFN.EQ.5)THEN
        IF((DTEMP.GT.1.0D10).OR.(DTEMP.LT.-1.0D10))GO TO 9000
        DVAL=COS(DTEMP)
      ELSE IF(IFN.EQ.6)THEN
        DVAL=COSH(DTEMP)
      ELSE IF(IFN.EQ.7)THEN
        IF(DTEMP.GT.500.0D0) GO TO 9000
        DVAL=EXP(DTEMP)
      ELSE IF(IFN.EQ.8)THEN
        IF(DTEMP.LE.0.0D0) GO TO 9000
        DVAL=LOG(DTEMP)
      ELSE IF(IFN.EQ.9)THEN
        IF(DTEMP.LE.0.0D0) GO TO 9000
        DVAL=LOG10(DTEMP)
      ELSE IF(IFN.EQ.10)THEN
        IF((DTEMP.GT.1.0D10).OR.(DTEMP.LT.-1.0D10))GO TO 9000
        DVAL=SIN(DTEMP)
      ELSE IF(IFN.EQ.11)THEN
        DVAL=SINH(DTEMP)
      ELSE IF(IFN.EQ.12)THEN
        IF(DTEMP.LT.0.0D0) GO TO 9000
        DVAL=SQRT(DTEMP)
      ELSE IF(IFN.EQ.13)THEN
        IF((DTEMP.GT.1.0E10).OR.(DTEMP.LT.-1.0E10))GO TO 9000
        DVAL=TAN(DTEMP)
      ELSE IF(IFN.EQ.14)THEN
        DVAL=TANH(DTEMP)
      ELSE IF(IFN.EQ.15)THEN
        DVAL=-DTEMP
      ELSE IF(IFN.EQ.16)THEN
        DVAL=DTEMP
      ELSE IF(IFN.EQ.17)THEN
        GO TO 9000
      ELSE IF(IFN.EQ.18)THEN
        GO TO 9000
      ELSE IF(IFN.EQ.19)THEN
        GO TO 9000
      END IF

      RETURN

! -- An error condition has occurred.

9000  JERR=IFN
      RETURN
END SUBROUTINE EQN_FUNCTION



SUBROUTINE EQN_LOGICAL_REPLACE()

! -- Subroutine EQN_LOGICAL_REPLACE replaces logical operators with their single
!    character equivalents.

      IMPLICIT NONE
      INTEGER                             :: NB,I,J,K
      CHARACTER (LEN=5)                   :: AA

      IF(BTEXT.EQ.' ') RETURN
      DO I=1,LOPER
        AA=LOPERATFULL(I)
        NB=LEN_TRIM(AA)
10      J=INDEX(BTEXT,AA(1:NB))
        IF(J.NE.0)THEN
          BTEXT(J:J)=LOPERAT(I)
          DO K=1,NB-1
            BTEXT(J+K:J+K)=' '
          END DO
          GO TO 10
        END IF
      END DO

      RETURN

END SUBROUTINE EQN_LOGICAL_REPLACE


SUBROUTINE EQN_STORE(IFAIL,EQUATION_NAME,IEQN)

! -- Subroutine EQN_STORE stores an equation in a compressed string.

     IMPLICIT NONE

     INTEGER, INTENT(OUT)          :: IFAIL          ! error flag
     CHARACTER (LEN=*), INTENT(IN) :: EQUATION_NAME  ! name of equation
     INTEGER, INTENT(IN)           :: IEQN           ! equation number

     INTEGER                       :: I,J,NB,K,IERR

     IFAIL=0
     COMPEQN(IEQN)%NUMTERM=NTERM
     COMPEQN(IEQN)%NAME=EQUATION_NAME
     J=1
     DO I=1,NTERM
       NB=LEN_TRIM(ATERM(I))
       J=J+NB
     END DO
     COMPEQN(IEQN)%LASTINDEX=J-1
     ALLOCATE(COMPEQN(IEQN)%PTERM(NTERM),COMPEQN(IEQN)%ASTRING(J-1),STAT=IERR)
     IF(IERR.NE.0)THEN
       IFAIL=1
       RETURN
     END IF

     J=1
     DO I=1,NTERM
       COMPEQN(IEQN)%PTERM(I)=J
       NB=LEN_TRIM(ATERM(I))
       DO K=1,NB
         COMPEQN(IEQN)%ASTRING(J+K-1)=ATERM(I)(K:K)
       END DO
       J=J+NB
     END DO

END SUBROUTINE EQN_STORE



SUBROUTINE EQN_EXTRACT(IEQN,EQUATION_NAME)

! -- Subroutine EQN_EXTRACT extracts an equation from a compressed string.

     IMPLICIT NONE

     INTEGER, INTENT(IN)              :: IEQN           ! equation number
     CHARACTER (LEN=*), INTENT(OUT)   :: EQUATION_NAME  ! equation name
     INTEGER I,K,J,L

     NTERM=COMPEQN(IEQN)%NUMTERM
     EQUATION_NAME=COMPEQN(IEQN)%NAME
     DO I=1,NTERM
       ATERM(I)=' '
     END DO
     K=COMPEQN(IEQN)%PTERM(1)
     IF(NTERM.GT.1)THEN
       DO I=1,NTERM-1
         J=K
         K=COMPEQN(IEQN)%PTERM(I+1)
         DO L=1,K-J
           ATERM(I)(L:L)=COMPEQN(IEQN)%ASTRING(J+L-1)
         END DO
       END DO
     END IF
     DO L=1,COMPEQN(IEQN)%LASTINDEX-K+1
       ATERM(NTERM)(L:L)=COMPEQN(IEQN)%ASTRING(K+L-1)
     END DO

     RETURN

END SUBROUTINE EQN_EXTRACT


     subroutine UTL_a2i(ifail,string,num)
     IMPLICIT NONE
     integer, intent(out)            :: ifail
     character (len=*), intent(in)   :: string
     integer, intent(out)            :: num
     character (len=10)              :: afmt

       if(string.eq.' ') go to 10
     ifail=0
     afmt='(i    )'
     WRITE(afmt(3:6),'(i4)')len(string)
     read(string,afmt,err=10) num
     return

10    ifail=1
      return

      end subroutine UTL_a2i


      subroutine UTL_a2l(ifail,string,num)
      IMPLICIT NONE
      integer, intent(out)            :: ifail
      character (len=*), intent(in)   :: string
      logical, intent(out)            :: num
      character (len=10)              :: afmt

      if(string.eq.' ') go to 10
      ifail=0
      afmt='(l    )'
      WRITE(afmt(3:6),'(i4)')len(string)
      read(string,afmt,err=10) num
      return

10    ifail=1
      return

      end subroutine UTL_a2l


      subroutine UTL_a2r(ifail,string,num)
      IMPLICIT NONE
      integer, intent(out)            :: ifail
      character (len=*), intent(in)   :: string
      real, intent(out)               :: num
      character (len=10)              :: afmt

      if(string.eq.' ') go to 10
      ifail=0
      afmt='(f    .0)'
      WRITE(afmt(3:6),'(i4)')len(string)
      read(string,afmt,err=10) num
      return

10    ifail=1
      return

      end subroutine UTL_a2r


      subroutine UTL_a2d(ifail,string,num)
      IMPLICIT NONE
      integer, intent(out)            :: ifail
      character (len=*), intent(in)   :: string
      double precision, intent(out)   :: num
      character (len=10)              :: afmt

      if(string.eq.' ') go to 10
      ifail=0
      afmt='(f    .0)'
      WRITE(afmt(3:6),'(i4)')len(string)
      read(string,afmt,err=10) num
      return

10    ifail=1
      return

      end subroutine UTL_a2d



      subroutine UTL_i2a(value,string,nchar)
      IMPLICIT NONE
      integer, intent(in)             :: value
      character (len=*), intent(out)  :: string
      integer, intent(in), optional   :: nchar
      character (len=12)              :: afmt
      integer                         :: llen

      string=' '
      afmt='(i    )'
      llen=min(30,len(string))
      if(present(nchar)) llen=min(llen,nchar)
      WRITE(afmt(3:6),'(i4)') llen
      WRITE(string(1:llen),afmt,err=100) value
      string=adjustl(string)
      if(string(1:1).eq.'*') go to 100
      return

100   string(1:llen)=repeat('#',llen)
      return

      end subroutine UTL_i2a


      SUBROUTINE UTL_CASETRANS(STRING,HI_OR_LO)
    !
    ! -- Subroutine UTL_CASETRANS converts a string to upper or lower case.
    !
    ! -- Arguments are as follows:-
    !      string:      contains the string whose case must be changed
    !      hi_or_lo:  must be either 'lo' or 'hi' to indicate
    !                 change of case direction.
    !
    ! -- Revision history:-
    !       June-November, 1995: version 1.
    !
      IMPLICIT NONE
    !
      CHARACTER (LEN=*), INTENT(INOUT)        :: STRING
      CHARACTER (LEN=*), INTENT(IN)           :: HI_OR_LO
      CHARACTER                               :: ALO, AHI
      INTEGER                                 :: INC,I
    !
      IF(HI_OR_LO.EQ.'lo') THEN
        ALO='A'; AHI='Z'; INC=IACHAR('a')-IACHAR('A')
      ELSE IF(HI_OR_LO.eq.'hi') THEN
        ALO='a'; AHI='z'; INC=IACHAR('A')-IACHAR('a')
      ELSE
        CALL UTL_SUBERROR('UTL_CASETRANS')
      ENDIF
    !
      DO I=1,LEN_TRIM(STRING)
        IF((STRING(I:I).GE.ALO).AND.(STRING(I:I).LE.AHI)) &
        STRING(I:I)=ACHAR(IACHAR(STRING(I:I))+INC)
      END DO
    !
      RETURN
    !
      END SUBROUTINE UTL_CASETRANS


      subroutine UTL_SUBERROR(subname)

    ! -- Subroutine UTL_SUBERROR names the subroutine causing a run-time error.

    ! -- Arguments are as follows:-
    !       subname:  name of offending subroutine

    ! -- Revision history:-
    !       June-November, 1995: version 1.

      IMPLICIT NONE
      character (len=*)               ::subname

      WRITE(*,10) trim(subname)
10      format(/,' *** PROGRAMMING ERROR CALLING SUBROUTINE ',a,' ***')
      stop

      end subroutine UTL_SUBERROR



END MODULE EQUATION



