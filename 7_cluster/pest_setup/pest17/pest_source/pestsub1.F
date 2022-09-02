        SUBROUTINE LINSPL(IFAIL,NUM,LW,RW,CLINE)

C -- SUBROUTINE LINSPL SPLITS A LINE INTO WHITESPACE-SEPARATED SUBSTRINGS

        INTEGER IFAIL,NW,NBLC,J,I
        INTEGER NUM
        INTEGER LW(NUM),RW(NUM)
        CHARACTER*(*) CLINE

        IFAIL=0
        NW=0
        NBLC=LEN_TRIM(CLINE)
        IF(NBLC.EQ.0) THEN
          IFAIL=1
          RETURN
        END IF
        J=0
5       IF(NW.EQ.NUM) RETURN
        DO 10 I=J+1,NBLC
        IF((CLINE(I:I).NE.' ').AND.(CLINE(I:I).NE.',')
     +  .AND.(ICHAR(CLINE(I:I)).NE.9)) GO TO 20
10      CONTINUE
        IFAIL=1
        RETURN
20      NW=NW+1
        LW(NW)=I
        DO 30 I=LW(NW)+1,NBLC
        IF((CLINE(I:I).EQ.' ').OR.(CLINE(I:I).EQ.',')
     +  .OR.(ICHAR(CLINE(I:I)).EQ.9)) GO TO 40
30      CONTINUE
        RW(NW)=NBLC
        IF(NW.LT.NUM) IFAIL=1
        RETURN
40      RW(NW)=I-1
        J=RW(NW)
        GO TO 5

#ifdef PESTMOD
        END SUBROUTINE LINSPL
#else
        END
#endif        



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
#ifdef PESTMOD        
        END SUBROUTINE NUMRD
#else        
        END
#endif        



        
        subroutine addquote(afile,aqfile)

C -- Subroutine ADDQUOTE adds quotes to a filename if it has a space in it.

C -- Arguments are as follows:-
C        afile:       the name of the file
C        aqfile:      the name of the file with quotes added

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
        
#ifdef PESTMOD        
        end subroutine addquote
#else        
        end
#endif        


        subroutine datestring(dd,mm,astring)
        
C -- Subroutine datestring supplies a day and 3 character month name if given 
C    a day and month.

        integer dd,mm
        character*(*) astring
        
        integer ierr
        character*2 aa
        
        select case (mm)
        case (1)
          astring='Jan'
        case (2)
          astring='Feb'
        case (3)
          astring='Mar'
        case (4)
          astring='Apr'
        case (5)
          astring='May'
        case (6)
          astring='Jun'
        case (7)
          astring='Jul'
        case (8)
          astring='Aug'
        case (9)
          astring='Sep'
        case (10)
          astring='Oct'
        case (11)
          astring='Nov'
        case (12)
          astring='Dec'
        case default
          astring='***'
        end select
        
        write(aa,'(i2.2)',iostat=ierr) dd
        if(ierr.ne.0)aa='**'
        astring=aa//' '//trim(astring)
        
        return
        end
         


        subroutine speed_test(rtemp3,rtemp1,time1,cpu_slave_time_index)
        
! -- This is put here so that the optimizer will not see its uselessness. 
! -- Settings may have to change with/without optimizer on.
        
        implicit none
        integer i,j
        real time1,time2
        real cpu_slave_time_index
        double precision rtemp3,rtemp1,rtemp2

        call cpu_time(time1)
        do j=1,20
          rtemp3=rtemp3+rtemp1
          do i=2,1000000
            rtemp1=log(rtemp3)
            rtemp2=log(rtemp1)
            if(rtemp1.lt.0.0) rtemp1=1.0e-5
            if(rtemp1.gt.10.0) rtemp1=10.0
            if(rtemp2.lt.0.0) rtemp1=1.0e-5
            if(rtemp2.gt.10.0) rtemp1=10.0            
            rtemp1=rtemp2/rtemp1+rtemp1/rtemp2+rtemp1
            if(rtemp1.lt.0.0) rtemp1=1.0e-5
            if(rtemp1.gt.10.0) rtemp1=10.0
            rtemp1=exp(rtemp1)
          end do                    
          rtemp3=rtemp1*1.0e-6
        end do
        call cpu_time(time2)
        cpu_slave_time_index=time2-time1
        if(cpu_slave_time_index.eq.0.0) then
          write(6,112) 1.0e30
        else            
          write(6,112) 10.0/cpu_slave_time_index
112       format(' Speed index for this machine = ',1pg12.5)        
        end if
        
        return
        end
        