
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
#ifdef PESTMOD
        END SUBROUTINE DREALRD
#else        
        END
#endif        

        SUBROUTINE INTRD(IFAIL,CLINE,ITEMP)

C -- Subroutine INTRD reads an integer from a string.

        INTEGER IFAIL
        INTEGER ITEMP
        CHARACTER*8 AFMT
        CHARACTER*(*) CLINE

        IFAIL=0
        AFMT='(I   )'
        WRITE(AFMT(3:5),'(I3)') LEN(CLINE)
        READ(CLINE,AFMT,ERR=100) ITEMP
        RETURN

100     IFAIL=1
        RETURN
#ifdef PESTMOD
        END SUBROUTINE INTRD
#else        
        END
#endif

