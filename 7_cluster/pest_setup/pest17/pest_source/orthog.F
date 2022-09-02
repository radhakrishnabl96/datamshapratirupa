

      SUBROUTINE ORTCOL(M,N,Q,IRQ,V,SMALLR,RHO,WRK1,WRK2,IFAIL)
C
C======================== D E S C R I P T I O N ========================
C
C     ASSUMING THE M BY N MATRIX  Q  HAS (NEARLY) ORTHONORMAL COLUMNS,
C     THIS SUBROUTINE ORTHOGONALIZES THE M-VECTOR  V  TO THE COLUMNS
C     OF  Q.  IT NORMALIZES THE RESULT IF  M > N.  THE N-VECTOR
C     SMALLR  IS THE ARRAY OF "FOURIER COEFFICIENTS", AND RHO
C     IS THE DISTANCE FROM  V  TO THE RANGE OF Q.  SMALLR  AND
C     ITS CORRECTIONS ARE COMPUTED IN DOUBLE PRECISION.  FOR
C     MORE DETAIL, SEE SECTIONS 2 AND 4 OF THE PAPER BY DANIEL ET AL.
C
C     NOTES : 1. INNER PRODUCTS ARE DONE USING THE ROUTINE SDOT
C                FROM THE BLAS (DDOT IN DOUBLE PRECISION) AND ARE
C                ACCUMULATED IN DOUBLE PRECISION.
C
C             2. WE DO NOT CHECK THAT M > 0. THE USER MUST ENSURE THIS.
C                N MAY BE 0. IF N < 0, IT IS TREATED AS 0.
C
C             3. THE VECTORS  U  AND  S  FROM THE ALGOL PROGRAM ARE
C                PASSED TO THE ROUTINE AS WORK VECTORS WRK1 AND WRK2.
C
C             4. THE GLOBAL VARIABLES THETA, OMEGA AND SIGMA ARE
C                EXPLAINED IN DESCRB. NORMALLY SIGMA SHOULD BE OF THE
C                ORDER OF ONE TENTH OF THE RELATIVE MACHINE PRECISION,
C                OMEGA MAY BE SET TO 0 AND THETA MAY BE 1.4.  THESE
C                SPECIFIC RECOMMENDATIONS ARE BASED ON THE PRESENTATION
C                OF EXPERIMENT 1 IN THE LAST SECTION OF THE DANIEL
C                ET AL PAPER.  FOR COMPLETE INFORMATION, SEE THE PAPER.
C
C             5. EXIT TO THE GLOBAL EXIT "FAIL" IN ALGOL IS
C                IMPLEMENTED BY SETTING  IFAIL = 1  ON EXIT.
C                OTHERWISE,  IFAIL = 0 .
C
C             6. SEE "QRFACT" FOR A DESCRIPTION OF  IRQ.
C
C======================= D E C L A R A T I O N S =======================
C
CIIII      REAL              SDOT, SNRM2, OMEGA, ONE, ONENEG, Q, RHO
#ifdef INTEL
      DOUBLE PRECISION  DDOT, DNRM2, ONE, ONENEG, Q, RHO
#else
      DOUBLE PRECISION  DDOT, DNRM2, OMEGA, ONE, ONENEG, Q, RHO
#endif
CIIII      REAL               RHO0,   RHO1,  SIGMA, SMALLR
#ifdef INTEL
      DOUBLE PRECISION   RHO0,   RHO1,  SMALLR
#else
      DOUBLE PRECISION   RHO0,   RHO1,  SIGMA, SMALLR
#endif
CIIII      REAL                  T,  THETA,    TWO,      V,   WRK1
#ifdef INTEL
      DOUBLE PRECISION      T,  TWO,      V,   WRK1
#else
      DOUBLE PRECISION      T,  THETA,    TWO,      V,   WRK1
#endif
CIIII      REAL               WRK2,   ZERO
      DOUBLE PRECISION   WRK2,   ZERO
C
      INTEGER
     -      I,  IFAIL,    IRQ,      J,      K,
     -      M,      N
C
      DIMENSION Q(IRQ, 1), V(M), SMALLR(1)
      DIMENSION WRK1(M), WRK2(1)
C
      LOGICAL RESTAR, NULL
C
#ifndef INTEL
      COMMON /MGREEK/ THETA,OMEGA,SIGMA
#endif
C
CIIII      DATA ZERO /0.E0/, ONE /1.E0/, TWO /2.E0/, ONENEG /-1.E0/
      DATA ZERO /0.D0/, ONE /1.D0/, TWO /2.D0/, ONENEG /-1.D0/
C
C========================= E X E C U T I O N ===========================
C
CIIII      THETA = 1.4E0
      THETA = 1.4D0
C
CIIII      OMEGA = 0.0E0
      OMEGA = 0.0D0
C
CIIII      SIGMA = 3.55E-16
      SIGMA = 1.11D-17
C
      RESTAR = .FALSE.
      NULL   = .FALSE.
      IFAIL  = 0
C
      IF ( N .LE. 0 ) GOTO 2000
C
         DO 1000 J = 1, N
         SMALLR(J)   = ZERO
 1000    CONTINUE
C
 2000 CONTINUE
CIIII      RHO    = SNRM2(M,V,1)
      RHO    = DNRM2(M,V,1)
      RHO0   = RHO
      K      = 0
C
C=======================================================================
C-----TAKE A GRAM-SCHMIDT ITERATION, IGNORING R ON LATER STEPS
C-----IF PREVIOUS V WAS NULL.
C=======================================================================
C
 3000    DO 3100 I = 1, M
         WRK1(I)   = ZERO
 3100    CONTINUE
C
      IF ( N .LE. 0 ) GOTO 3400
C
         DO 3300 J = 1, N
CIIII         T = SDOT(M,Q(1,J),1,V,1)
         T = DDOT(M,Q(1,J),1,V,1)
         WRK2(J)   = T
CIIII         CALL SAXPY(M,T,Q(1,J),1,WRK1,1)
         CALL DAXPY(M,T,Q(1,J),1,WRK1,1)
 3300    CONTINUE
C
 3400 CONTINUE
CIIII      IF (.NOT. NULL .AND. N .GT. 0 ) CALL SAXPY(N,ONE,WRK2,1,SMALLR,1)
      IF (.NOT. NULL .AND. N .GT. 0 ) CALL DAXPY(N,ONE,WRK2,1,SMALLR,1)
C
CIIII      CALL SAXPY(M,ONENEG,WRK1,1,V,1)
      CALL DAXPY(M,ONENEG,WRK1,1,V,1)
CIIII      RHO1   = SNRM2(M,V,1)
      RHO1   = DNRM2(M,V,1)
CIIII      T      = SNRM2(N,WRK2,1)
      T      = DNRM2(N,WRK2,1)
      K      = K + 1
C
      IF ( M .NE. N ) GOTO 5000
C
C=======================================================================
C-----A SPECIAL CASE WHEN M = N.
C=======================================================================
C
         DO 4100 I = 1, M
         V(I)   = ZERO
 4100    CONTINUE
C
      RHO    = ZERO
      GOTO 90000
C
C=======================================================================
C----TEST FOR NONTERMINATION.
C=======================================================================
C
 5000 IF ( RHO0 + OMEGA * T .LT. THETA * RHO1 ) GOTO 6000
C
C-----EXIT IF TOO MANY ITERATIONS.
C
      IF ( K .LE. 4 ) GOTO 5100
      IFAIL  = 1
      GOTO 90000
C
C-----RESTART IF NECESSARY.
C
 5100 IF ( RESTAR .OR. RHO1 .GT. RHO * SIGMA ) GOTO 5900
      RESTAR = .TRUE.
C
C-----FIND FIRST ROW OF MINIMAL LENGTH OF Q.
C
         DO 5300 I = 1, M
CIIII         WRK1(I) = SDOT(N,Q(I,1),IRQ,Q(I,1),IRQ)
         WRK1(I) = DDOT(N,Q(I,1),IRQ,Q(I,1),IRQ)
 5300    CONTINUE
C
      T   = TWO
C
         DO 5500 I = 1, M
         IF ( WRK1(I) .GE. T ) GOTO 5500
         K      = I
         T      = WRK1(K)
 5500    CONTINUE
C
C-----TAKE CORRECT ACTION IF V IS NULL.
C
      IF ( RHO1 .NE. ZERO ) GOTO 5700
      NULL   = .TRUE.
      RHO1   = ONE
C
C-----REINITIALIZE V AND K.
C
 5700    DO 5800 I = 1, M
 5800    V(I)   = ZERO
C
      V(K)   = RHO1
      K      = 0
C
C-----TAKE ANOTHER ITERATION.
C
 5900 RHO0   = RHO1
      GOTO 3000
C
C======================================================================
C-----NORMALIZE V AND TAKE THE STANDARD EXIT
C======================================================================
C
 6000    DO 6100 I = 1, M
 6100    V(I)   = V(I) / RHO1
C
      RHO    = ZERO
      IF ( .NOT. NULL ) RHO = RHO1
C
C=============================== E X I T ===============================
C
90000 RETURN
C
      END
      SUBROUTINE CRFLCT(X, Y, C, S)
C
C======================== D E S C R I P T I O N ========================
C
C     THIS SUBROUTINE COMPUTES PARAMETERS FOR THE GIVENS MATRIX G FOR
C     WHICH (X,Y)G = (Z,0). IT REPLACES (X,Y) BY (Z,0).
C
C======================= D E C L A R A T I O N S =======================
C
CIIII      REAL                ARG,     C,    ONE,      S,      T
      DOUBLE PRECISION    ARG,     C,    ONE,      S,      T
CIIII      REAL                  U,  UDUM,     UM,      V,   VDUM
      DOUBLE PRECISION      U,  UDUM,     UM,      V,   VDUM
CIIII      REAL                  X,     Y,   ZERO
      DOUBLE PRECISION      X,     Y,   ZERO
C
CIIII      DATA ZERO /0.E0/, ONE /1.E0/
      DATA ZERO /0.D0/, ONE /1.D0/
C
C========================= E X E C U T I O N ===========================
C
      U      = X
      V      = Y
C
      IF ( V .NE. ZERO ) GOTO 1000
      C      = ONE
      S      = ZERO
      GOTO 90000
C
 1000 CONTINUE
CIIII      UM     =  AMAX1( ABS(U),  ABS(V))
      UM     =  DMAX1(DABS(U), DABS(V))
      UDUM   = U / UM
      VDUM   = V / UM
      ARG    = UDUM * UDUM + VDUM * VDUM
CIIII      T      = UM *  SQRT(ARG)
      T      = UM * DSQRT(ARG)
C
      IF ( U .LT. ZERO ) T = -T
C
      C      = U / T
      S      = V / T
      X      = T
      Y      = ZERO
C
C=============================== E X I T ===============================
C
90000 RETURN
C
      END
      SUBROUTINE ARFLCT (C,S,IP,X,INCX,IDISX,Y,INCY,IDISY)
C
C======================== D E S C R I P T I O N ========================
C
C     THIS IS A FORTRAN IMPLEMENTATION OF THE ALGOL ROUTINE
C          "APPLYREFLECTOR" WRITTEN BY DANIEL ET AL.
C
C     THE CALLING SEQUENCE IS DIFFERENT, BUT THAT IS UNAVOIDABLE DUE
C          TO FUNDAMENTAL DIFFERENCES IN THE HANDLING OF PARAMETER
C          LISTS IN FORTRAN AND ALGOL. (SEE THE FOLLOWING PARAGRAPHS.)
C
C     THIS ROUTINE TAKES 2 VECTORS, CALLED X AND Y, AND REPLACES
C          THEM BY LINEAR COMBINATIONS
C                      C * X + S * Y
C                      S * X - C * Y.
C          THAT IS, IT APPLIES A GIVEN'S REFLECTION TO VECTORS X
C          AND Y.  C AND S  ARE COMPUTED IN "CRFLCT".  THE NUMBER
C          OF ELEMENTS IN EACH OF  X  AND  Y  IS  IP.
C
C     THE JENSEN DEVICE USED IN THE ALGOL PROCEDURE IS NO LONGER
C     RELEVANT. INSTEAD IT IS ASSUMED THAT ANY CALL WITH AN ACTUAL
C     PARAMETER WHICH IS AN ARRAY OR ARRAY ELEMENT WILL BE DONE BY
C     PASSING THE ADDRESS OF THE FIRST ELEMENT OF THE ARRAY OR
C     THE ADDRESS OF THE ARRAY ELEMENT.
C
C     IN "APPLYREFLECTOR" X AND Y WERE IN EFFECT ROWS OR COLUMNS
C          OF A SQUARE MATRIX.  THE SAME WILL BE TRUE HERE, BUT THEY
C          MAY BE FROM THE TRIANGULAR MATRIX  R  AS DISCUSSED
C          IN THE ROUTINE "DESCRB".
C
C     THE PARAMETERS  INCX  AND  IDISX  ARE USED IN THE FOLLOWING WAY
C        (WITH SIMILAR USAGE FOR INCY AND IDISY):
C
C        THE PARAMETER X IS ASSUMED TO BE EQUIVALENT TO X(1).
C        THE SUBSCRIPT REFERENCE IS INITIALIZED TO I = 1 AND THE FIRST
C             SUBSCRIPT REFERENCE IS TO X(I) = X(1) .
C        THE NEXT LOCATION REFERENCED IN THE ARRAY X IS X(I + IDISX).
C             THUS IDISX IS THE DISTANCE TO THE NEXT SUBSCRIPT NEEDED.
C             THEN I IS REPLACED BY  I + IDISX.
C        THEN  IDISX IS INCREMENTED BY  INCX SO THAT THE DISTANCE TO
C             THE NEXT SUBSCRIPT NEEDED MAY BE DIFFERENT.
C        THE CYCLE THEN REPEATS, SO THAT  THE CALL "...X,1,1,..." WILL
C             GET  X(1),X(2),X(4),X(7),X(11),...    AND THE CALL WITH
C             "...X,0,2,..."  WILL GET X(1),X(3),X(5),...  .
C        THIS IS EXACTLY WHAT IS NEEDED TO HANDLE THE TRIANGULAR ARRAYS.
C
C======================= D E C L A R A T I O N S =======================
C
CIIII      REAL                  C,    ONE,      S,      T,      U
      DOUBLE PRECISION      C,    ONE,      S,      T,      U
CIIII      REAL                 UN,      V,      X,      Y
      DOUBLE PRECISION     UN,      V,      X,      Y
C
      INTEGER
     -  IDISX,  IDISY, INCVXT, INCVYT,   INCX,
     -   INCY,     IP,     JX,     JY,      K
C
      DIMENSION X(1), Y(1)
C
CIIII      DATA ONE /1.E0/
      DATA ONE /1.D0/
C
C========================= E X E C U T I O N ===========================
C
      IF ( IP .LE. 0 ) GOTO 90000
      UN     = S / ( ONE + C )
      JX     = 1
      JY     = 1
      INCVXT = IDISX
      INCVYT = IDISY
C
        DO 1000 K = 1, IP
        U      = X(JX)
        V      = Y(JY)
        T      = U * C + V * S
        X(JX)  = T
        Y(JY)  = ( T + U ) * UN - V
        JX     = JX + INCVXT
        JY     = JY + INCVYT
        INCVXT = INCVXT + INCX
        INCVYT = INCVYT + INCY
 1000   CONTINUE
C
C=============================== E X I T ===============================
C
90000 RETURN
C
      END
      SUBROUTINE RANK1(M,N,Q,IRQ,R,U,V,WRK1,WRK2,WRK3,WRK4,IFAIL)
C
C======================== D E S C R I P T I O N ========================
C
C     THIS SUBROUTINE UPDATES THE FACTORIZATION  A  =  Q R  WHEN THE
C     OUTER PRODUCT OF THE M-VECTOR  V  AND  THE N-VECTOR  U  IS
C     ADDED TO A.  ON ENTRY  Q IS  M  BY  N   AND  R  IS  N  BY  N.
C     THE USER SHOULD ENSURE THAT  M >= N > 0.
C
C     IRQ IS DESCRIBED IN "QRFACT".
C
C     WRK1 AND WRK2 ARE TEMPORARY VECTORS PASSED AS WORKING STORAGE
C     TO THE ROUTINE  "ORTCOL".
C
C     WRK3 IS A TEMPORARY WORK VECTOR OF LENGTH N CORRESPONDING TO
C     THE VECTOR  T  DECLARED IN THE ALGOL PROCEDURE.
C
C     NOTICE ALSO THAT, AS MENTIONED IN  "DESCRB" , THE TRIANGULAR
C     MATRIX R IS NOT STORED IN FULL, BUT ONLY ITS NONZERO
C     UPPER HALF IS AVAILABLE.  THUS THERE IS NO STORAGE AVAILABLE
C     FOR THE ZERO ELEMENTS IN THE LOWER PART.  HOWEVER, THE ALGOL
C     PROCEDURE USES THE STORAGE SPACE ALONG THE FIRST SUBDIAGONAL OF
C     R.  THUS WE NEED TO PROVIDE SOME TEMPORARY STORAGE TO ALLOW
C     FOR THE INFORMATION STORED THERE.  THIS IS THE USE OF THE
C     WORKING VECTOR  WRK4.
C
C
C======================= D E C L A R A T I O N S =======================
C
CIIII      REAL                  C,    ONE,      Q,      R,    RHO
      DOUBLE PRECISION      C,    ONE,      Q,      R,    RHO
CIIII      REAL               RHOV,      S,     T1,      U,      V
      DOUBLE PRECISION   RHOV,      S,     T1,      U,      V
CIIII      REAL               WRK1,   WRK2,   WRK3,   WRK4,   ZERO
      DOUBLE PRECISION   WRK1,   WRK2,   WRK3,   WRK4,   ZERO
C
      INTEGER
     -      I,  IFAIL,    IRQ, ITEMP1,      K,
     -    KP1,      M,      N,    NM1,    NP1
C
      DIMENSION Q(IRQ,N), R(1), U(N), V(M), RHOV(1)
      DIMENSION WRK1(M), WRK2(N), WRK3(N), WRK4(N)
C
      EQUIVALENCE  (RHO,RHOV(1))
C
CIIII      DATA  ZERO/0.E0/, ONE/1.E0/
      DATA  ZERO/0.D0/, ONE/1.D0/
C
C======================== E X E C U T I O N ============================
C
      NM1 = N - 1
      NP1 = N + 1
C
      CALL ORTCOL(M,N,Q,IRQ,V,WRK3,RHO,WRK1,WRK2,IFAIL)
      IF (IFAIL .EQ. 1) GOTO 90000
      CALL CRFLCT(WRK3(N),RHO,C,S)
      ITEMP1 = ( N*NP1) / 2
      CALL ARFLCT(C,S,1,R(ITEMP1),0,1,RHOV,0,1)
      CALL ARFLCT(C,S,M,Q(1,N),0,1,V,0,1)
C
      IF ( N .LE. 1) GOTO 2000
         DO 1000 I = 1,NM1
         K = N-I
         KP1 = K + 1
         CALL CRFLCT(WRK3(K),WRK3(KP1), C,S)
         CALL ARFLCT(C,S,I,R(ITEMP1-1),1,KP1,R(ITEMP1),1,KP1)
         WRK4(KP1) = ZERO
         ITEMP1 = ITEMP1 - KP1
         CALL ARFLCT(C,S,1,R(ITEMP1),0,1,WRK4(KP1),0,1)
         CALL ARFLCT(C,S,M,Q(1,K),0,1,Q(1,KP1),0,1)
 1000    CONTINUE
C
 2000 K = 1
      T1 = WRK3(1)
         DO 2500 I = 1,N
         R(K) = ONE * R(K)  +  T1 * U(I)
         K = K + I
 2500    CONTINUE
      ITEMP1 = 1
      IF ( N .LE. 1) GOTO 4000
         DO 3000 K = 1,NM1
         KP1 = K + 1
         CALL CRFLCT(R(ITEMP1), WRK4(KP1), C,S)
         ITEMP1 = ITEMP1 + KP1
         CALL ARFLCT(C,S,N-K,R(ITEMP1-1),1,KP1,R(ITEMP1),1,KP1)
         CALL ARFLCT(C,S,M,Q(1,K),0,1,Q(1,KP1),0,1)
 3000    CONTINUE
C
 4000 CALL CRFLCT(R(ITEMP1),RHO,C,S)
      CALL ARFLCT(C,S,M,Q(1,N),0,1,V,0,1)
C
C========================= E X I T =====================================
C
90000 RETURN
C
      END
      SUBROUTINE DELCOL(M, N, Q, IRQ, R, K, V )
C
C======================== D E S C R I P T I O N ========================
C
C     THIS SUBROUTINE UPDATES THE FACTORIZATION OF THE  M  BY  N
C     MATRIX  A  = Q R  WHEN THE K-TH COLUMN OF A IS DELETED.  IT
C     RETURNS THE DELETED COLUMN OF THE ORIGINAL A IN V.
C
C     NOTICE THAT NO  ACTUAL DELETION TAKES PLACE. IN FACT, ON ENTRY,
C     A IS DEFINED ONLY BY THE DECOMPOSITION A = Q R. THEN, ON EXIT,
C     Q AND R DEFINE A NEW DECOMPOSITION OF A MATRIX GIVEN BY DELETING
C     THE COLUMN OF THE ORIGINAL(IMPLICITLY DEFINED) A. NOTE THAT Q AND
C     R ARE NOT ACTUALLY CHANGED IN DIMENSION, BUT OBSERVE THAT THE
C     SUBROUTINE WILL COMPUTE IN Q AND R THE DECOMPOSITION OF THE
C     SMALLER MATRIX A. THE NEW DECOMPOSITION APPEARS IN THE FIRST N-1
C     COLUMNS OF Q AND IN WHAT IS LOGICALLY THE UPPER LEFT N-1 BY N-1
C     SUBMATRIX OF R.
C
C     SEE "QRFACT" FOR A DESCRIPTION OF IRQ.
C
C     THE METHOD OF STORAGE OF  R  IS DESCRIBED IN "DESCRB".
C
C     IF K < 1  OR IF  K > N , NO OPERATION IS DONE.
C     THE USER IS RESPONSIBLE FOR ENSURING THAT  M >= N > 0.
C
C======================= D E C L A R A T I O N S =======================
C
CIIII      REAL                  C,    ONE,      Q,      R,      S
      DOUBLE PRECISION      C,    ONE,      Q,      R,      S
CIIII      REAL                  T,      V,   ZERO
      DOUBLE PRECISION      T,      V,   ZERO
C
      INTEGER
     -      I,    IRQ, ITEMP1, ITEMP2,    IT1,
     -    IT2,      J,      K,      L,    LP1,
     -    LP2,      M,      N,  NMIS1
C
      DIMENSION Q(IRQ,N), R(1), V(M)
C
CIIII      DATA ZERO /0.E0/, ONE /1.E0/
      DATA ZERO /0.D0/, ONE /1.D0/
C
C========================= E X E C U T I O N ===========================
C
      IF ( K .LE. 0   .OR.   K .GT. N ) GOTO 90000
C
         DO 1000 I = 1, M
 1000    V(I)   = ZERO
C
      ITEMP1 = (K*(K-1)) / 2
C
         DO 3000 L = 1, K
         IT2    = ITEMP1 + L
         T      = R(IT2)
CIIII         CALL SAXPY(M,T,Q(1,L),1,V,1)
         CALL DAXPY(M,T,Q(1,L),1,V,1)
 3000    CONTINUE
C
      NMIS1  = N - 1
      IF ( K .GT. NMIS1 ) GOTO 7000
      LP2 = K + 1
C
         DO 5000 L = K, NMIS1
         LP1 = LP2
         LP2 = L + 2
         ITEMP1 = (LP1 * L) / 2 + L
         CALL CRFLCT(R(ITEMP1),R(ITEMP1+1), C, S)
         ITEMP1 = ITEMP1 + LP1
         IF (N .GT. LP1 )
     -   CALL ARFLCT(C, S, N-LP1, R(ITEMP1), 1,LP2, R(ITEMP1+1), 1,LP2)
         CALL ARFLCT(C, S, M, Q(1,L), 0, 1, Q(1,LP1), 0, 1)
 5000    CONTINUE
C
         DO 6500 J = K, NMIS1
         ITEMP1 = (J * (J - 1)) / 2
         ITEMP2 = ITEMP1 + J
C
            DO 6000 I = 1, J
            IT1  = ITEMP1 + I
            IT2  = ITEMP2 + I
 6000       R(IT1) = R(IT2)
C
 6500    CONTINUE
C
 7000 ITEMP1 = (N*NMIS1) / 2  + 1
      ITEMP2 = ITEMP1 + NMIS1
C
         DO 8000 I = ITEMP1, ITEMP2
 8000    R(I)   = ZERO
C
         DO 9000 I = 1, M
 9000    Q(I,N) = ZERO
C
C=============================== E X I T ===============================
C
90000 RETURN
C
      END
      SUBROUTINE INSCOL (M,N,Q,IRQ,R,K,V,WRK1,WRK2,WRK3,IFAIL)
C
C======================== D E S C R I P T I O N ========================
C
C     THIS SUBROUTINE UPDATES THE FACTORIZATION A = Q R WHEN THE M-
C     VECTOR V IS INSERTED BETWEEN COLUMNS K - 1 AND K OF A.
C
C     IT ASSUMES  Q  IS INITIALLY  M  BY  N-1
C     AND THAT    R  IS INITIALLY N-1 BY  N-1.
C
C     THE USER SHOULD ENSURE THAT  M >= N > 0  AND THAT  0 < K <= N.
C     NOTICE THAT A CALL WITH  K = N  JUST AMOUNTS TO A CALL
C     TO "ORTCOL".
C
C     WRK1 AND WRK2  ARE TEMPORARY VECTORS PASSED TO "ORTCOL".
C     WRK3 IS FOR TEMPORARY STORAGE OF THE WORK VECTOR U OF THE
C     ALGOL ROUTINE.
C
C     R IS STORED IN TRIANGULAR FORM, AS DESCRIBED IN "DESCRB".
C
C     IRQ IS EXPLAINED IN  "QRFACT".
C
C     IFAIL IS EXPLAINED IN "ORTCOL".
C
C
C======================= D E C L A R A T I O N S =======================
C
CIIII      REAL                  C,      Q,      R,    RHO,      S
      DOUBLE PRECISION      C,      Q,      R,    RHO,      S
CIIII      REAL                  V,   WRK1,   WRK2,   WRK3,   ZERO
      DOUBLE PRECISION      V,   WRK1,   WRK2,   WRK3,   ZERO
C
      INTEGER
     -      I,  IFAIL,    IRQ, ITEMP1, ITEMP2,
     -    IT1,    IT2,      J,     JJ,      K,
     -      L,     LL,    LP1,      M,      N,
     -     NK,     N1
C
      DIMENSION Q(IRQ,N), R(1),  V(M)
      DIMENSION WRK1(M), WRK2(N), WRK3(N)
C
CIIII      DATA ZERO /0.E0/
      DATA ZERO /0.D0/
C
C======================== E X E C U T I O N ============================
C
      N1 = N - 1
      IF ( K .GE. N) GOTO 3500
      NK = N1 + K
      ITEMP1 = (N*N1) / 2
      ITEMP2 = ITEMP1 + N
         DO 2000 JJ = K,N1
         R(ITEMP2) = ZERO
         ITEMP2 = ITEMP1
         J = NK - JJ
         ITEMP1 = ITEMP1 - J
            DO 1000 I = 1, J
            IT1 = ITEMP1 + I
            IT2 = ITEMP2 + I
 1000       R(IT2) = R(IT1)
 2000    CONTINUE
C
 3500 CALL ORTCOL(M,N1,Q,IRQ,V,WRK3,RHO,WRK1,WRK2,IFAIL)
      IF (IFAIL .EQ. 1) GOTO 90000
      WRK3(N) = RHO
C
         DO 4000 I = 1, M
 4000    Q(I,N) = V(I)
C
      IF ( K .GE. N) GOTO 5500
      ITEMP1 = (N*N1) /2 + N1
         DO 5000 LL = K, N1
         L = NK - LL
         LP1 = L + 1
         CALL CRFLCT(WRK3(L),WRK3(LP1),C,S)
         CALL ARFLCT(C,S,N-L,R(ITEMP1),1,LP1,R(ITEMP1+1),1,LP1)
         CALL ARFLCT(C,S,M,Q(1,L),0,1,Q(1,LP1),0,1)
         ITEMP1 = ITEMP1 - LP1
 5000    CONTINUE
C
 5500 ITEMP1 = (K*(K-1))  /  2
         DO 6000 I = 1, K
         IT1 = ITEMP1 + I
 6000    R(IT1) = WRK3(I)
C
C========================= E X I T =====================================
C
90000 RETURN
C
      END
      SUBROUTINE INSROW(M,N,Q,IRQ,R,K,U,WRK1)
C
C======================== D E S C R I P T I O N ========================
C
C     THIS SUBROUTINE UPDATES THE FACTORIZATION A = Q R WHEN THE
C     N-VECTOR U IS INSERTED BETWEEN ROWS K - 1 AND K OF A.
C
C     IT ASSUMES THAT  Q  IS INITIALLY  M-1 BY N
C            AND THAT  R  IS INITIALLY   N  BY N .
C
C     WRK1 IS A TEMPORARY VECTOR CORRESPONDING TO  V  IN
C          THE ALGOL ROUTINE.
C
C     R  IS STORED AS DESCRIBED IN  "DESCRB".
C
C     IRQ  IS DEFINED IN  "QRFACT".
C
C     IT IS ASSUMED THAT  M > N > 0  AND THAT  0 < K <= M.
C     NO OPERATION TAKES PLACE WHEN  K <=0  OR K > M.
C
C
C======================= D E C L A R A T I O N S =======================
C
CIIII      REAL                  C,    ONE,      Q,      R,      S
      DOUBLE PRECISION      C,    ONE,      Q,      R,      S
CIIII      REAL                  U,   WRK1,   ZERO
      DOUBLE PRECISION      U,   WRK1,   ZERO
C
      INTEGER
     -      I,     II,    IRQ, ITEMP1,    IT1,
     -      K,      L,    LP1,      M,     MK,
     -    MM1,      N
C
      DIMENSION Q(IRQ,N), R(1), U(N)
      DIMENSION WRK1(M)
C
CIIII      DATA ZERO /0.E0/, ONE /1.E0/
      DATA ZERO /0.D0/, ONE /1.D0/
C
C======================== E X E C U T I O N ============================
C
      IF ( K .LE. 0   .OR.   K .GT. M ) GOTO 90000
         DO 1000 I = 1,M
 1000    WRK1(I) = ZERO
      WRK1(K) = ONE
C
      MM1 = M - 1
      MK = MM1 + K
      ITEMP1 = 1
         DO 3000 L = 1,N
         LP1 = L + 1
         IF (K .GE. M) GOTO 2500
            DO 2000 II = K,MM1
            I = MK - II
 2000       Q(I+1,L) = Q(I,L)
 2500    Q(K,L) = ZERO
         CALL CRFLCT( R(ITEMP1),U(L),C,S)
         IT1 = ITEMP1 + L
         IF (N .GT. L) CALL ARFLCT( C,S,N-L,R(IT1),1,LP1,U(LP1),0,1)
         CALL ARFLCT(C,S,M,Q(1,L),0,1,WRK1,0,1)
         ITEMP1 = ITEMP1 + LP1
 3000    CONTINUE
C
C========================= E X I T =====================================
C
90000 RETURN
C
      END
      SUBROUTINE DELROW(M,N,Q,IRQ,R,K,U,WRK1,WRK2,WRK3,IFAIL)
C
C======================== D E S C R I P T I O N ========================
C
C     THIS SUBROUTINE UPDATES THE FACTORIZATION A = Q R WHEN THE K-TH
C     ROW OF A IS DELETED.  IT RETURNS THE DELETED ROW OF A IN U.
C
C     NOTE THAT, AS IN THE SUBROUTINE DELCOL, NO ACTUAL DELETION
C     TAKES PLACE. SEE DELCOL FOR FURTHER COMMENTS. THE REVISED
C     DECOMPOSITION APPEARS IN THE FIRST M-1 ROWS OF Q AND IN THE
C     SAME N BY N MATRIX R.
C
C     IT ASSUMES THAT  M > N , BUT IT DOES NOT CHECK THIS.
C     NO OPERATION IS PERFORMED IF  K < 1   OR  K > M.
C
C     WRK3 IS THE WORK VECTOR V OF LENGTH M OF THE ALGOL ROUTINE.
C     WRK1 AND WRK2 ARE TEMPORARY WORK VECTORS WHICH ARE USED
C     IN A CALL TO "ORTCOL" AND WHICH HAVE LENGTHS  M AND  N
C     RESPECTIVELY.
C
C     IRQ IS DESCRIBED IN "QRFACT".
C
C     R IS STORED AS DESCRIBED IN "DESCRB".
C
C     IFAIL IS DEFINED IN "ORTCOL".
C
C
C======================= D E C L A R A T I O N S =======================
C
CIIII      REAL                  C,    ONE,      Q,      R,      S
      DOUBLE PRECISION      C,    ONE,      Q,      R,      S
CIIII      REAL                  T,      U,   WRK1,   WRK2,   WRK3
      DOUBLE PRECISION      T,      U,   WRK1,   WRK2,   WRK3
CIIII      REAL               ZERO
      DOUBLE PRECISION   ZERO
C
      INTEGER
     -      I,  IFAIL,    IRQ, ITEMP1,      J,
     -      K,      L,     LL,      M,     M1,
     -      N,     N1
C
      DIMENSION Q(IRQ,N), R(1), U(N)
      DIMENSION WRK1(M), WRK2(N), WRK3(M)
C
CIIII      DATA ZERO /0.E0/, ONE /1.E0/
      DATA ZERO /0.D0/, ONE /1.D0/
C
C======================== E X E C U T I O N ============================
C
      IF ( K .LE. 0   .OR.  K .GT. M ) GOTO 90000
         DO 1000 I = 1, M
 1000    WRK3(I) = ZERO
      WRK3(K) = ONE
C
      CALL ORTCOL(M,N,Q,IRQ,WRK3,U,T,WRK1,WRK2,IFAIL)
      IF ( IFAIL .EQ. 1) GOTO 90000
C
      M1 = M - 1
      IF ( K .GE. M) GOTO 2500
         DO 2000 I = K, M1
 2000    WRK3(I) = WRK3(I + 1)
C
 2500 N1 = N + 1
         DO 4000 LL = 1,N
         L = N1 - LL
         IF ( K .GE. M) GOTO 3500
            DO 3000 I = K, M1
 3000       Q(I,L) = Q(I + 1,L)
 3500    CALL CRFLCT (T, U(L), C, S)
         ITEMP1 = (L*(L+1)) / 2
         CALL ARFLCT(C,S,LL,U(L),0,1,R(ITEMP1),1,L)
         CALL ARFLCT(C,S,M1,WRK3,0,1,Q(1,L),0,1)
         Q(M,L) = ZERO
 4000    CONTINUE
C
         DO 5000 J = 1,N
 5000    U(J) = T * U(J)
C
C========================= E X I T =====================================
C
90000 RETURN
C
      END
      SUBROUTINE QRFACT(M,N,A,IRA,Q,IRQ,R,WRK1,WRK2,WRK3,WRK4,IFAIL)
C
C======================== D E S C R I P T I O N ========================
C
C     THIS COMPUTES A GRAM-SCHMIDT QR FACTORIZATION OF A.
C
C     IT IS ASSUMED THAT  A  IS  M  BY  N  AND THAT  M >= N.
C     THUS  Q  MUST BE  M  BY  N  AND  R  WILL BE  N BY N,
C     ALTHOUGH  R  WILL BE STORED ONLY AS THE UPPER TRIANGULAR
C     HALF, STORED BY COLUMNS, AS DESCRIBED IN THE ROUTINE
C     "DESCRB".
C
C     WRK4 IS A TEMPORARY WORK VECTOR OF LENGTH  M,  NAMELY
C          V  OF THE ALGOL ROUTINE.
C     WRK1, WRK2 AND WRK3  ARE USED IN "INSCOL" AND ARE OF LENGTHS
C          M, N AND N RESPECTIVELY.
C
C     IRA AND IRQ ARE THE ACTUAL DECLARED FIRST DIMENSIONS OF THE
C          ARRAYS  A  AND  Q  RESPECTIVELY.
C
C     IFAIL IS DEFINED IN "ORTCOL", WHICH IS INDIRECTLY CALLED BY
C          "QRFACT".
C
C     FOR FURTHER DETAILS, PLEASE SEE THE ALGOL ROUTINE "QRFACTOR"
C          BY DANIEL ET AL.
C
C======================= D E C L A R A T I O N S =======================
C
CIIII      REAL                  A,      Q,      R,   WRK1,   WRK2
      DOUBLE PRECISION      A,      Q,      R,   WRK1,   WRK2
CIIII      REAL               WRK3,   WRK4
      DOUBLE PRECISION   WRK3,   WRK4
C
      INTEGER
     -      I,  IFAIL,    IRA,    IRQ,      K,
     -      M,      N
C
      DIMENSION   A(IRA,N), Q(IRQ,N), R(1)
      DIMENSION   WRK1(M), WRK2(N), WRK3(N), WRK4(M)
C
C======================== E X E C U T I O N ============================
C
         DO 2000 K = 1 , N
            DO 1000 I = 1 , M
 1000       WRK4(I) = A( I,K )
         CALL INSCOL(M,K,Q,IRQ,R,K,WRK4,WRK1,WRK2,WRK3,IFAIL)
         IF (IFAIL .EQ. 1) GOTO 90000
 2000    CONTINUE
C
C========================= E X I T =====================================
C
90000 RETURN
C
      END



