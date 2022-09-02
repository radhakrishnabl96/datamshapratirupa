C**********************************************************************C
C***********************  MODULE TO REPLACE COMMON BLOCKS  ************C
C**********************************************************************C
      MODULE COMMON_MOD
      IMPLICIT NONE      
      INTEGER ISTOP,NCALL,MCALL,JCALL,IWAIT
      INTEGER M1,D1,Y1,H1,MIN1,IPERR,JPERR
      INTEGER ICOM
      INTEGER*4 REASON   
      REAL SS1 
      DOUBLE PRECISION THETA,OMEGA,SIGMA
      character*11 result
      CHARACTER*100 CASEFL          
      CHARACTER*200 FLENME
      END MODULE COMMON_MOD
