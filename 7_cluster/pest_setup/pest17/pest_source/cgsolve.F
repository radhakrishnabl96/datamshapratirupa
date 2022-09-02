c      SUBROUTINE cgsolve( N, B, R1, R2, V, W, X, Y,
c     1                   APROD, MSOLVE, PRECON, SHIFT,
c     2                   NOUT, ITNLIM, EPS, RTOL,
c     3                   ISTOP, ITN, ANORM, ACOND, RNORM, XNORM )
C
c      EXTERNAL           APROD, MSOLVE

      SUBROUTINE cgsolve( N, B, R1, R2, V, W, X, Y,
     1                   PRECON, SHIFT,
     2                   NOUT, ITNLIM, EPS, RTOL,
     3                   ISTOP, ITN, ANORM, ACOND, RNORM, XNORM,
     4                   LHSVDIM, LHSVEC)

      INTEGER            N, NOUT, ITNLIM, ISTOP, ITN
      LOGICAL            PRECON
      DOUBLE PRECISION   SHIFT, EPS, RTOL, ANORM, ACOND, RNORM, XNORM
      DOUBLE PRECISION   B(N), R1(N), R2(N), V(N), W(N), X(N), Y(N)
      INTEGER            LHSVDIM
      DOUBLE PRECISION   LHSVEC(LHSVDIM)
C     ------------------------------------------------------------------
C
C     SYMMLQ  is designed to solve the system of linear equations
C
C                A*x = b
C
C     where  A  is an  n*n  symmetric matrix and  b  is a given vector.
C     The matrix  A  is not required to be positive definite.
C     (If  A  is known to be definite, the method of conjugate gradients
C     may be used -- it will require about the same number of iterations
C     as  SYMMLQ  but slightly less work per iteration.)
C
C
C     The matrix  A  is intended to be large and sparse.  It is accessed
C     by means of a subroutine call of the form
C
C                CALL APROD( n,x,y)
C
C     which must return the product  y = A*x  for any given vector  x.
C
C
C     More generally,  SYMMLQ  is designed to solve the system
C
C                (A - shift*I)*x = b
C
C     where   shift  is a specified scalar value.  If  shift  and  b
C     are suitably chosen, the computed vector  x  may approximate an
C     (unnormalized) eigenvector of  A,  as in the methods of
C     inverse iteration and/or Rayleigh-quotient iteration.
C     Again, the matrix  (A - shift*I)  need not be positive definite.
C     The work per iteration is very slightly less if  shift = 0.
C
C
C     A further option is that of preconditioning, which may reduce
C     the number of iterations required.  If  M  is a positive
C     definite matrix that is known to approximate  (A - shift*I)
C     in some sense, and if systems of the form  M*y = x  can be
C     solved efficiently, the parameters  PRECON  and  MSOLVE  may be
C     used (see below).  When  PRECON = .true. ,  SYMMLQ  will
C     implicitly solve the system of equations
C
C                P*( A - shift*I )*P*y = P*b,   where   P = M**(-1/2),
C
C     and return the solution  x = P*y.
C
C
C     Parameters
C     ----------
C
C     N       input      n, the dimension of the matrix  A.
C
C     B(N)    input      The rhs vector  b.
C
C     R1(N)   output     Returns the final residual vector,
C                           r = b - (A - shift*I)*x.
C
C     R2(N)   workspace
C     V(N)    workspace
C     W(N)    workspace
C
C     X(N)    output     Returns the computed solution  x.
C
C     Y(N)    workspace
C
C     APROD   external   A subroutine defining the matrix  A.
C                        For a given vector  x,  the statement
C
C                              CALL APROD( n,x,y)
C
C                        must return the product  y = A*x
C                        without altering the vector  x.
C
C     MSOLVE  external   An optional subroutine defining a
C                        preconditioning matrix  M,  which should
C                        approximate  (A - shift*I)  in some sense ---
C                        typically having many similar eigenvalues.
C                        M  must be positive definite.
C                        For a given vector  x,  the statement
C
C                              CALL MSOLVE( n,x,y )
C
C                        must solve the linear system  M*y = x
C                        without altering the vector  x.
C
C                        NOTE.  The program calling  SYMMLQ  must
C                        declare  APROD  and  MSOLVE  to be external.
C
C     PRECON  input      If  PRECON = .true. ,  preconditioning will
C                        be invoked.  Otherwise, subroutine  MSOLVE
C                        will not be referenced; in this case the
C                        actual parameter corresponding to  MSOLVE  may
C                        be the same as that corresponding to  APROD.
C
C     SHIFT   input      Should be zero if the system  A*x = b  is to
C                        be solved.  Otherwise, it could be an
C                        approximation to an eigenvalue of  A,  such as
C                        the Rayleigh quotient  b(t)*A*b/(b(t)*b)
C                        corresponding to the vector  b.
C                        If  b  is sufficiently like an eigenvector
C                        corresponding to an eigenvalue near  SHIFT,
C                        then the computed  x  may have very large
C                        components.  When normalized,  x  may be
C                        closer to an eigenvector than  b.
C
C     NOUT    input      The output file number.  If positive,
C                        a summary will be printed on unit  NOUT.
C
C     ITNLIM  input      An upper limit on the number of iterations.
C
C     EPS     input      The relative machine precision.
C                        For example,
C                        Burroughs B6700      EPS = 2.0**(-37)
C                        CDC 6600, 7600       EPS = 2.0**(-47)
C                        IBM 370 (single)     EPS = 16.0**(-5)
C                        IBM 370 (double)     EPS = 16.0**(-13)
C
C     RTOL    input      A user-specified tolerance.  SYMMLQ terminates
C                        if it appears that  norm(Rbar)  is smaller than
C                        RTOL * norm(Abar) * norm(y),  where
C                        Abar  is the transformed matrix operator
C
C                          Abar = P * (A - shift*I) * P
C
C                        and  Rbar  is the transformed residual vector
C
C                          Rbar = P * ( b - (A - shift*I)*x ).
C
C                        If  shift = 0  and PRECON = .false., SYMMLQ
C                        terminates if  norm(b - A*x)  is smaller than
C                        RTOL * norm(A) * norm(x).
C
C     ISTOP   output     An integer giving the reason for termination...
C
C               0        b = 0,  so the exact solution is  x = 0.
C                        No iterations were performed.
C
C               1        Norm(Rbar)  appears to be less than
C                        the value  RTOL * norm(Abar) * norm(y).
C                        The solution in  X  should be acceptable.
C
C               2        Norm(Rbar)  appears to be less than
C                        the value   EPS * norm(Abar) * norm(y).
C                        This means that the residual is as small as
C                        seems reasonable on this machine.
C
C               3        Norm(Abar)*norm(y)  exceeds  norm(b)/EPS,
C                        which should indicate that  x  has essentially
C                        converged to an eigenvector of  A
C                        corresponding to the eigenvalue  shift.
C
C               4        ACOND  (see below)  has exceeded  0.1/EPS,  so
C                        the matrix  Abar  must be very ill-conditioned.
C                        X  may not contain an acceptable solution.
C
C               5        The iteration limit was reached before any of
C                        the previous criteria were satisfied.
C
C               6        An inner product of the form  x(t)*M**(-1)*x
C                        was not positive, so the preconditioning matrix
C                        M  does not appear to be positive definite.
C                        X  will not contain an acceptable solution.
C
C     ITN     output     The number of iterations performed.
C
C     ANORM   output     An estimate of the norm of the matrix operator
C                        Abar = P*(A - shift*I)*P,  where P = M**(-1/2).
C
C     ACOND   output     An estimate of the condition of  Abar  above.
C                        This will usually be a substantial
C                        under-estimate of the true condition.
C
C     RNORM   output     The norm of the final residual vector,
C                           b - (A - shift*I)*x.
C
C     XNORM   output     The norm of the final solution vector  x.
C
C
C
C     To change precision
C     -------------------
C
C     Alter the words
C            DABS, DMAX1, DMIN1, DOUBLE PRECISION,
C            DAXPY, DCOPY_CG, DDOT, DNRM2_CG, DSQRT
C     throughout.
C
C     Also make sure  EPS  is set correctly in the calling program.
C     ------------------------------------------------------------------
C
C
C     This routine is an implementation of the algorithm described in
C     the following references:
C
C     C.C. Paige and M.A. Saunders,  Solution of Sparse Indefinite
C          Systems of Linear Equations,
C          SIAM J. Numer. Anal. 12, 4, September 1975, pp. 617-629.
C
C     J.G. Lewis,  Algorithms for Sparse Matrix Eigenvalue Problems,
C          Report STAN-CS-77-595, Computer Science Department,
C          Stanford University, Stanford, California, March 1977.
C
C     Applications of SYMMLQ and the theory of preconditioning
C     are described in the following references:
C
C     D.B. Szyld and O.B. Widlund,  Applications of Conjugate Gradient
C          Type Methods to Eigenvalue Calculations,
C          in R. Vichnevetsky and R.S. Steplman (editors),
C          Advances in Computer Methods for Partial Differential
C          Equations -- III, IMACS, 1979, 167-173.
C
C     D.B. Szyld,  A Two-level Iterative Method for Large Sparse
C          Generalized Eigenvalue Calculations,
C          Ph. D. dissertation, Department of Mathematics,
C          New York University, New York, October 1983.
C     ------------------------------------------------------------------
C
C
C     SYMMLQ.    This version dated 15 Sept 1985.
C
C                Michael A. Saunders
C                Department of Operations Research
C                Stanford University
C     ------------------------------------------------------------------
C
C
C     Subroutines and functions
C
C     USER       APROD, MSOLVE
C     BLAS       DAXPY, DCOPY_CG, DDOT , DNRM2_CG
C     FORTRAN    DABS , DMAX1, DMIN1, DSQRT, MOD
C
C
C     Functions and local variables
C
#ifndef PESTMOD
      DOUBLE PRECISION   cgddot, DNRM2_CG
#endif      
      DOUBLE PRECISION   DABS, DMAX1, DMIN1, DSQRT
      INTEGER            MOD
      DOUBLE PRECISION   S,T,Z,B1,CS,SN,ONE,ALFA,BETA,DBAR,DIAG,
     1                   EPSA,EPSR,EPSX,GBAR,GMAX,GMIN,
     2                   OLDB,RHS1,RHS2,X1CG,X1LQ,ZBAR,ZERO,
     3                   BETA1,BSTEP,DELTA,DENOM,EPSLN,GAMMA,GPERT,
     4                   TNORM,YNORM,
     5                   CGNORM,ELQNRM,QRNORM,SNPROD,YNORM2
      INTEGER            I
C     ------------------------------------------------------------------
C
C
C     Print heading and initialize.
C
      IF (NOUT .GT. 0)
     1   WRITE(NOUT, 1000) N, PRECON, SHIFT, NOUT, ITNLIM, EPS, RTOL
      ZERO   = 0.0
      ONE    = 1.0
      ISTOP  = 0
      ITN    = 0
      ANORM  = ZERO
      ACOND  = ZERO
      RNORM  = ZERO
      XNORM  = ZERO
      YNORM  = ZERO
C
      DO 50 I = 1, N
         X(I) = ZERO
         W(I) = ZERO
   50 CONTINUE
C
C     Set up  V,  the first vector in the Lanczos sequence.
C
      CALL DCOPY_CG ( N,B,1,Y,1 )
      CALL DCOPY_CG ( N,B,1,R1,1 )
      IF (PRECON) CALL MSOLVE( N,R1,Y )
      B1     = Y(1)
      BETA1  = cgddot( N,R1,1,Y,1 )
      IF (BETA1 .LT. ZERO) ISTOP = 6
      IF (BETA1 .LE. ZERO) GO TO 900
      BETA1  = DSQRT( BETA1 )
      S      = ONE/BETA1
C
      DO 100 I = 1, N
         V(I)  = S*Y(I)
  100 CONTINUE
C
C     Set up  Y  for the second Lanczos vector.
C
      CALL APROD ( N,V,Y,lhsvdim,lhsvec)
      CALL cgdaxpy ( N,(-SHIFT),V,1,Y,1 )
      ALFA   = cgddot( N,V,1,Y,1 )
      CALL cgdaxpy ( N,(-ALFA/BETA1),R1,1,Y,1 )
C
C     Make sure  R2  will be orthogonal to the first  V.
C
      Z      = cgddot( N,V,1,Y,1 )
      S      = cgddot( N,V,1,V,1 )
      CALL cgdaxpy ( N,(-Z/S),V,1,Y,1 )
      CALL DCOPY_CG ( N,Y,1,R2,1 )
      IF (PRECON) CALL MSOLVE( N,R2,Y )
      OLDB   = BETA1
      BETA   = cgddot( N,R2,1,Y,1 )
      IF (BETA .LE. ZERO) ISTOP = 6
      IF (BETA .LE. ZERO) GO TO 900
      BETA   = DSQRT( BETA )
C
C     See if the local reorthogonalization achieved anything.
C
      DENOM  = DSQRT(S) * DNRM2_CG( N,R2,1 )
      S      = Z/DENOM
      T      = cgddot( N,V,1,R2,1 )/DENOM
      IF (NOUT .GT. 0) WRITE(NOUT, 1100) BETA1,ALFA
      IF (NOUT .GT. 0) WRITE(NOUT, 1120) S,T
C
C     Initialize other quantities.
C
      CGNORM = BETA1
      GBAR   = ALFA
      DBAR   = BETA
      RHS1   = BETA1
      RHS2   = ZERO
      BSTEP  = ZERO
      SNPROD = ONE
      TNORM  = ALFA**2
      YNORM2 = ZERO
      GMAX   = ZERO
      GMIN   = ONE/EPS
C
C     ------------------------------------------------------------------
C     Main iteration loop.
C     ------------------------------------------------------------------
C
C     Estimate various norms and test for convergence.
C
  300 ANORM  = DSQRT(TNORM)
      YNORM  = DSQRT(YNORM2)
      EPSA   = ANORM*EPS
      EPSX   = ANORM*YNORM*EPS
      EPSR   = ANORM*YNORM*RTOL
      DIAG   = GBAR
      IF (DIAG .EQ. ZERO) DIAG = EPSA
C
      ELQNRM = DSQRT(RHS1**2 + RHS2**2)
      QRNORM = SNPROD*BETA1
      CGNORM = QRNORM*BETA / DABS(DIAG)
C
C     Estimate  COND(A).
C     In this version we look at the diagonals of  L  in the
C     factorization of the tridiagonal matrix,  T = L*Q.
C
      DENOM  = DMIN1( GMIN, DABS(DIAG) )
      ACOND  = GMAX/DENOM
C
C     See if any of the stopping criteria are satisfied.
C
      IF (ITN    .GE. ITNLIM ) ISTOP = 5
      IF (ACOND  .GE. 0.1/EPS) ISTOP = 4
      IF (EPSX   .GE. BETA1  ) ISTOP = 3
      IF (CGNORM .LE. EPSX   ) ISTOP = 2
      IF (CGNORM .LE. EPSR   ) ISTOP = 1
C     ==================================================================
C
C     See if it is time to print something.
C
      IF (NOUT .LE.  0)          GO TO 600
      IF (N    .LE. 40)          GO TO 400
      IF (ITN  .LE. 10)          GO TO 400
      IF (ITN  .GE. ITNLIM - 10) GO TO 400
      IF (MOD(ITN,10)  .EQ.   0) GO TO 400
      IF (CGNORM .LE. 10.0*EPSX) GO TO 400
      IF (CGNORM .LE. 10.0*EPSR) GO TO 400
      IF (ACOND  .GE. 0.01/EPS ) GO TO 400
      IF (ISTOP  .NE. 0)         GO TO 400
      GO TO 600
C
C     Print a line for this iteration.
C
  400 ZBAR   = RHS1/DIAG
      Z      = (SNPROD*ZBAR + BSTEP)/BETA1
      X1LQ   = X(1) + B1*BSTEP/BETA1
      X1CG   = X(1) + W(1)*ZBAR + B1*Z
C
      IF (    ITN     .EQ. 0) WRITE(NOUT, 1200)
      WRITE(NOUT, 1300) ITN,X1CG,CGNORM,BSTEP,ANORM,ACOND
      IF (MOD(ITN,10) .EQ. 0) WRITE(NOUT, 1500)
C     ==================================================================
C
C
C     Obtain the current Lanczos vector  V = (1/BETA)*Y
C     and set up  Y  for the next iteration.
C
  600 IF (ISTOP .NE. 0) GO TO 800
      S      = ONE/BETA
C
      DO 620 I = 1, N
         V(I)  = S*Y(I)
  620 CONTINUE
C
      CALL APROD ( N,V,Y,lhsvdim,lhsvec)
      CALL cgdaxpy ( N,(-SHIFT),V,1,Y,1 )
      CALL cgdaxpy ( N,(-BETA/OLDB),R1,1,Y,1 )
      ALFA   = cgddot( N,V,1,Y,1 )
      TNORM  = TNORM + (ALFA**2) + 2.0*(BETA**2)
      CALL cgdaxpy ( N,(-ALFA/BETA),R2,1,Y,1 )
      CALL DCOPY_CG ( N,R2,1,R1,1 )
      CALL DCOPY_CG ( N,Y,1,R2,1 )
      IF (PRECON) CALL MSOLVE( N,R2,Y )
      OLDB   = BETA
      BETA   = cgddot( N,R2,1,Y,1 )
      IF (BETA .LE. ZERO) ISTOP = 6
      IF (BETA .LE. ZERO) GO TO 800
      BETA   = DSQRT( BETA )
C
C     Compute the next plane rotation for  Q.
C
      GAMMA  = DSQRT(GBAR**2 + OLDB**2)
      CS     = GBAR/GAMMA
      SN     = OLDB/GAMMA
      DELTA  = CS*DBAR + SN*ALFA
      GBAR   = SN*DBAR - CS*ALFA
      EPSLN  = SN*BETA
      DBAR   =         - CS*BETA
C
C     Update  X.
C
      Z      = RHS1/GAMMA
      S      = Z*CS
      T      = Z*SN
C
      DO 700 I = 1, N
         X(I)  = (W(I)*S  +  V(I)*T)  +  X(I)
         W(I)  =  W(I)*SN -  V(I)*CS
  700 CONTINUE
C
C     Accumulate the step along the direction  B,
C     and go round again.
C
      BSTEP  = SNPROD*CS*Z + BSTEP
      SNPROD = SNPROD*SN
      GMAX   = DMAX1( GMAX, GAMMA )
      GMIN   = DMIN1( GMIN, GAMMA )
      YNORM2 = Z**2 + YNORM2
      RHS1   = RHS2 - DELTA*Z
      RHS2   =      - EPSLN*Z
      ITN    = ITN  + 1
      GO TO 300
C
C     ------------------------------------------------------------------
C     End of main iteration loop.
C     ------------------------------------------------------------------
C
C     Move to the CG point.
C     (In this version of SYMMLQ, we never stop at an LQ point.)
C
  800 ZBAR   = RHS1/DIAG
      BSTEP  = SNPROD*ZBAR + BSTEP
      YNORM  = DSQRT(YNORM2 + ZBAR**2)
      CALL cgdaxpy ( N,ZBAR,W,1,X,1 )
C
C     Add the step along  B.
C
      BSTEP  = BSTEP/BETA1
      CALL DCOPY_CG ( N,B,1,Y,1 )
      IF (PRECON) CALL MSOLVE( N,B,Y )
      CALL cgdaxpy ( N,BSTEP,Y,1,X,1 )
C
C     Compute the final residual,  R1 = B - (A - SHIFT*I)*X.
C
      CALL APROD ( N,X,Y,lhsvdim,lhsvec)
      CALL cgdaxpy ( N,(-SHIFT),X,1,Y,1 )
C
      DO 850 I = 1, N
         R1(I) = B(I) - Y(I)
  850 CONTINUE
C
      RNORM  = DNRM2_CG( N,R1,1 )
      XNORM  = DNRM2_CG( N,X ,1 )
C
C     ==================================================================
C     Display final status.
C     ==================================================================
  900 IF (NOUT  .LE. 0) GO TO 950
      WRITE(NOUT, 2000) ISTOP, ITN, ANORM, ACOND, RNORM, XNORM
      IF (ISTOP .EQ. 0) WRITE(NOUT, 3000)
      IF (ISTOP .EQ. 1) WRITE(NOUT, 3100)
      IF (ISTOP .EQ. 2) WRITE(NOUT, 3200)
      IF (ISTOP .EQ. 3) WRITE(NOUT, 3300)
      IF (ISTOP .EQ. 4) WRITE(NOUT, 3400)
      IF (ISTOP .EQ. 5) WRITE(NOUT, 3500)
      IF (ISTOP .EQ. 6) WRITE(NOUT, 3600)
  950 RETURN
C
C     ------------------------------------------------------------------
 1000 FORMAT(// ' SYMMLQ.            Solution of symmetric  Ax = b'
     1 / ' N    =', I7, 6X, 'PRECON =', L6, 6X, 'SHIFT =', 1PE23.14
     2 / ' NOUT =', I7, 6X, 'ITNLIM =', I6, 6X, 'EPS   =', 1PE11.2, 5X,
     3   ' RTOL =', 1PE11.2)
 1100 FORMAT(/ ' beta1  =', 1PE12.2 / ' alpha1 =', 1PE12.2)
 1120 FORMAT(/ ' (V1,V2) before and after ', 1PE15.2
     1       / ' local reorthogonalization', 1PE15.2 )
 1200 FORMAT(// 5X, 'ITN', 7X, 'X1(CG)',
     1   10X, 'Norm(R)', 5X, 'BSTEP', 7X, 'Norm(A)', 3X, 'Cond(A)')
 1300 FORMAT(I8, 1PE19.10, 1PE11.2, 1PE14.5, 1P2E10.2)
 1500 FORMAT(1X)
 2000 FORMAT(/ ' EXIT SYMMLQ.',    14X, 'ISTOP =', I3, 18X, 'ITN =', I8
     1       / ' ANORM =', 1PE13.5, 6X, 'ACOND =', 1PE13.5, 5X,
     2         ' RNORM =', 1PE13.5, 6X, 'XNORM =', 1PE13.5)
 3000 FORMAT(/ ' The exact solution is  X = 0.')
 3100 FORMAT(/ ' Requested accuracy achieved, as determined by  RTOL.')
 3200 FORMAT(/ ' Reasonable accuracy achieved, given  EPS.')
 3300 FORMAT(/ ' X  has converged to an eigenvector.')
 3400 FORMAT(/ ' ACOND  has exceeded  0.1/EPS.')
 3500 FORMAT(/ ' The iteration limit was reached.')
 3600 FORMAT(/ ' MSOLVE  does not define a positive definite',
     1         ' preconditioner.')
C     ------------------------------------------------------------------
C     END OF SYMMLQ
#ifdef PESTMOD
      END SUBROUTINE cgsolve
#else      
      END
#endif      
C     ******************************************************
C
C     WARNING.  Delete the following imitation BLAS routines
C               if a genuine BLAS library is available.
C
C     ******************************************************
C
      SUBROUTINE cgdaxpy ( N,A,X,INCX,Y,INCY )
      DOUBLE PRECISION   A,X(N),Y(N)
C
C     This may be replaced by the corresponding  BLAS  routine.
C     The following is a simple version for use with  SYMMLQ.
C
      IF (A .EQ. 0.0) RETURN
      DO 10 I = 1, N
         Y(I) = A*X(I) + Y(I)
   10 CONTINUE
      RETURN
#ifdef PESTMOD
      END SUBROUTINE cgdaxpy
#else
      END
#endif


      SUBROUTINE DCOPY_CG ( N,X,INCX,Y,INCY )
      DOUBLE PRECISION   X(N),Y(N)
C
C     This may be replaced by the corresponding  BLAS  routine.
C     The following is a simple version for use with  SYMMLQ.
C
      DO 10 I = 1, N
         Y(I) = X(I)
   10 CONTINUE
      RETURN
#ifdef PESTMOD
      END SUBROUTINE DCOPY_CG
#else
      END
#endif


      FUNCTION   cgddot  ( N,X,INCX,Y,INCY )
      DOUBLE PRECISION   cgddot,X(N),Y(N),SUM
C
C     This may be replaced by the corresponding  BLAS  routine.
C     The following is a simple version for use with  SYMMLQ.
C
      SUM    = 0.0
      DO 10 I = 1, N
         SUM  = X(I)*Y(I) + SUM
   10 CONTINUE
      cgddot   = SUM
      RETURN
#ifdef PESTMOD
      END FUNCTION   cgddot
#else
      END
#endif      


      FUNCTION   DNRM2_CG ( N,X,INCX )
      DOUBLE PRECISION   DNRM2_CG,X(N),DSQRT,SUM
C
C     This may be replaced by the corresponding  BLAS  routine.
C     The following is a simple version for use with  SYMMLQ.
C
      SUM    = 0.0
      DO 10 I  = 1, N
         SUM   = X(I)**2 + SUM
   10 CONTINUE
      DNRM2_CG  = DSQRT(SUM)
      RETURN
#ifdef PESTMOD
      END FUNCTION   DNRM2_CG
#else      
      END
#endif      


      subroutine aprod(ncol,x,y,lhsvdim,lhsvec)

      implicit none

      integer ncol,icol,irow,lhsind
      integer lhsvdim
      double precision x(ncol),y(ncol)
      double precision sum
      double precision lhsvec(lhsvdim)

      lhsind=0
      do icol=1,ncol
        sum=0.0d0
        do irow=1,icol
          lhsind=lhsind+1
          sum=sum+lhsvec(lhsind)*x(irow)
        end do
        y(icol)=sum
      end do
      if(ncol.ne.1)then
        lhsind=0
        do icol=1,ncol
          do irow=1,icol
            lhsind=lhsind+1
            if(icol.eq.irow)cycle
            y(irow)=y(irow)+lhsvec(lhsind)*x(icol)
          end do
        end do
      end if

      return
#ifdef PESTMOD
      end subroutine aprod
#else
      end
#endif


      subroutine msolve(ncol,x,y)
      
      integer ncol
      double precision x(ncol),y(ncol)
      
      write(6,*)
      write(6,*) ' MSOLVE SHOULD NOT BE CALLED'
      write(6,*) ' IT HAS NOTHING IN IT AT THE MOMENT'
      stop
      
      return
#ifdef PESTMOD
      end subroutine msolve
#else      
      end
#endif
      
