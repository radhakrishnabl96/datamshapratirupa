	PROGRAM MAT2SRF

        IMPLICIT NONE

C -- Program MAT2SRF writes a matrix in SURFER grid file format.

        INTEGER           :: NROW,NCOL,IFAIL,THRESHFLAG,NB,I,J,K,IERR,
     +                       ICODE
        INTEGER           :: LW(4),RW(4)
        DOUBLE PRECISION  :: THRESH,ZMIN,ZMAX
        DOUBLE PRECISION, ALLOCATABLE :: MAT(:,:)
        CHARACTER*5       :: AEXT
        CHARACTER*12      :: AVERSION
        CHARACTER*200     :: INFILE,OUTFILE,AFILE
        CHARACTER*400     :: CLINE
        CHARACTER*500     :: AMESSAGE


#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif       
#endif

C -- The program is identified.

        INCLUDE 'version.inc'
	WRITE(6,1) TRIM(AVERSION)
1       FORMAT(/,' MAT2SRF Version ',A,'. ',
     +  'Watermark Numerical Computing.',/)

#ifdef NO_CMDLINE
        WRITE(*,*)
        WRITE(*,3) ' Enter command line arguments > '
        READ(*,3) CLINE
3       FORMAT(A)
#else
	CALL PGETCL(CLINE)
#endif

C -- The command line is read.

        CALL SPACESUB(CLINE)
        CLINE=ADJUSTL(CLINE)

        IF(CLINE.EQ.' ') GO TO 8900
        CALL LINSPL(IFAIL,4,LW,RW,CLINE)
        IF(IFAIL.EQ.0) GO TO 8900
        CALL LINSPL(IFAIL,3,LW,RW,CLINE)
        IF(IFAIL.EQ.0)THEN
          THRESHFLAG=1
        ELSE
          CALL LINSPL(IFAIL,2,LW,RW,CLINE)
          IF(IFAIL.NE.0) GO TO 8900
        END IF
        INFILE=CLINE(LW(1):RW(1))
        OUTFILE=CLINE(LW(2):RW(2))
        CALL REMCHAR(INFILE,CHAR(211))
        CALL REMCHAR(OUTFILE,CHAR(211))
#ifndef UNIX
        CALL LOWCAS(INFILE)
        CALL LOWCAS(OUTFILE)
#endif
        NB=LEN_TRIM(OUTFILE)
        I=NB-3
        IF(I.LT.1)I=1
        AEXT=OUTFILE(I:NB)
        CALL LOWCAS(AEXT)
        IF(AEXT.NE.'.grd')THEN
          OUTFILE=TRIM(OUTFILE)//'.grd'
        END IF
        IF(THRESHFLAG.NE.0)THEN
          CALL DREALRD(IFAIL,CLINE(LW(3):RW(3)),THRESH)
          IF(IFAIL.NE.0) GO TO 8900
          IF(THRESH.LE.0.0D0)THEN
            WRITE(AMESSAGE,50)
50          FORMAT(' If supplied, the blanking threshold must be ',
     +      'positive.')
            GO TO 9800
          END IF
        END IF

! -- The input file is opened.

        CALL ADDQUOTE(INFILE,AFILE)
        OPEN(UNIT=10,FILE=INFILE,STATUS='OLD',IOSTAT=IERR)
        IF(IERR.NE.0)THEN
          WRITE(AMESSAGE,70) TRIM(AFILE)
70        FORMAT(' Cannot open matrix file ',a,'.')
          GO TO 9800
        END IF

! -- The Matrix is read.

        READ(10,'(A)',ERR=9500,END=9500) CLINE
        CALL LINSPL(IFAIL,3,LW,RW,CLINE)
        IF(IFAIL.NE.0)THEN
          WRITE(AMESSAGE,71) TRIM(AFILE)
71        FORMAT(' Three integers are expected on first line of ',
     +    'matrix file ',a,'.')
          GO TO 9800
        END IF
        CALL INTREAD(IFAIL,CLINE(LW(1):RW(1)),NROW)
        IF(IFAIL.NE.0) GO TO 9500
        CALL INTREAD(IFAIL,CLINE(LW(2):RW(2)),NCOL)
        IF(IFAIL.NE.0) GO TO 9500
        CALL INTREAD(IFAIL,CLINE(LW(3):RW(3)),ICODE)
        IF(IFAIL.NE.0) GO TO 9500
        IF((NCOL.LE.0).OR.(NROW.LE.0))THEN
          WRITE(AMESSAGE,72) TRIM(AFILE)
72        FORMAT(' Zero or negative row or column number quoted on ',
     +    'first line of file ',A,'.')
          GO TO 9800
        END IF
        IF((ICODE.NE.-1).AND.(ICODE.NE.1).AND.(ICODE.NE.2))THEN
          WRITE(AMESSAGE,73) TRIM(AFILE)
73        FORMAT(' Illegal value for third integer on header line ',
     +    'of file ',A,'.')
          GO TO 9800
        END IF
        IF((NROW.NE.NCOL).AND.(ICODE.EQ.-1))THEN
          WRITE(AMESSAGE,74) TRIM(AFILE)
74        FORMAT(' Third integer on header line to matrix file ',a,
     +    ' must not be "-1" unless matrix is square.')
          GO TO 9800
        END IF
        ALLOCATE(MAT(NROW,NCOL),STAT=IERR)
        IF(IERR.NE.0) GO TO 9300
        IF(ICODE.NE.-1)THEN
          DO I=1,NROW
            READ(10,*,ERR=9000,END=9100) (MAT(I,J),J=1,NCOL)
          END DO
        ELSE
          MAT=0.0D0
          DO I=1,NROW
            READ(10,*,ERR=9000,END=9100) MAT(I,I)
          END DO
        END IF
        CLOSE(UNIT=10)
        WRITE(6,100) TRIM(AFILE)
100     FORMAT(' - file ',a,' read ok.')

! -- The grid file is written.

        AFILE=OUTFILE
        OPEN(UNIT=20,FILE=OUTFILE,ERR=9400)

        IF(THRESHFLAG.EQ.0) THEN
          THRESH=1.70141e38
        ELSE
           IF(THRESH.GT.1.70141e38)THRESH=1.70141e38
        END IF
        WHERE(ABS(MAT).GE.THRESH) MAT=1.70141e38

        K=COUNT(ABS(MAT).LT.THRESH)
        IF(K.EQ.0)THEN
          ZMIN=0.0
          ZMAX=0.0
        ELSE
          ZMIN=MINVAL(MAT,MASK=(ABS(MAT).LT.THRESH))
          ZMAX=MAXVAL(MAT,MASK=(ABS(MAT).LT.THRESH))
        END IF

        WRITE(20,'(A)')'DSAA'
        WRITE(20,'(1X,I5,1X,I5)') NCOL,NROW
        WRITE(20,140) 1.0, FLOAT(NCOL)
140     FORMAT(1X,1PE18.10,2X,1PE18.10)
        WRITE(20,140) 1.0, FLOAT(NROW)
        WRITE(20,140) ZMIN,ZMAX
        DO I=NROW,1,-1
          WRITE(20,165) (MAT(I,J),J=1,NCOL)
165       FORMAT(7(1PE14.6))
          WRITE(20,*)
        END DO

        CLOSE(UNIT=20)
        WRITE(6,170) TRIM(AFILE)
170     FORMAT(' - file ',A,' written ok.')
        GO TO 9900


8900    CONTINUE
        WRITE(6,8910)
8910    FORMAT(' MAT2SRF is run using the command:',/)
        WRITE(6,8920)
8920    FORMAT('    mat2srf matfile gridfile [threshold]',/,/,
     +  ' where',/)
        WRITE(6,8930)
8930    FORMAT('    resfile   is a matrix file, ')
        WRITE(6,8940)
8940    FORMAT('    gridfile  is a SURFER grid file, and')
        WRITE(6,8950)
8950    FORMAT('    threshold is an optional blanking threshold.')
        GO TO 9995

9000    WRITE(AMESSAGE,9010) TRIM(AFILE)
9010    FORMAT(' Error encountered in reading matrix file ',a,'.')
        GO TO 9800
9100    WRITE(AMESSAGE,9110) TRIM(AFILE)
9110    FORMAT(' Premature end encountered to matrix file ',a,'.')
        GO TO 9800
9300    WRITE(6,9310)
9310    FORMAT(' Cannot allocate sufficient memory to continue ',
     +  'execution.')
        GO TO 9800
9400    WRITE(6,9410) TRIM(OUTFILE)
9410    FORMAT(' Cannot write to SURFER grid file ',a,'.')
        GO TO 9800
9500    WRITE(AMESSAGE,9510) TRIM(AFILE)
9510    FORMAT(' Error encountered in reading ROWS/COLUMNS header on ',
     +  'first line of matrix file ',a,'.')
        GO TO 9800


9800    CONTINUE
        CALL WRITMESS(6,AMESSAGE)
9810    CONTINUE

9900    DEALLOCATE(MAT, STAT=IERR)

9995    END



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


