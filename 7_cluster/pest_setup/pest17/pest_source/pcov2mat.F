	PROGRAM PCOV2MAT

        IMPLICIT NONE

C -- Program PCOV2MAT extracts a parameter covariance matrix from a PEST REC or MTT file
C    and writes this file in matrix format.

        INTEGER, PARAMETER :: MAXPAR=5000
        INTEGER            :: NPAR,I,J,IFAIL,IERR,NUMPAR
        INTEGER            :: LW(8),RW(8)
        DOUBLE PRECISION, ALLOCATABLE :: MAT(:,:)
        CHARACTER*12       :: ATEMP
        CHARACTER*12       :: AVERSION
        CHARACTER*12       :: APAR(MAXPAR)
        CHARACTER*200      :: INFILE,OUTFILE,AFILE
        CHARACTER*400      :: CLINE
        CHARACTER*500      :: AMESSAGE


#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif       
#endif

C -- The program is identified.

        INCLUDE 'version.inc'
	WRITE(6,1) TRIM(AVERSION)
1       FORMAT(/,' PCOV2MAT Version ',A,'. ',
     +  'Watermark Numerical Computing.')

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
        CALL LINSPL(IFAIL,3,LW,RW,CLINE)
        IF(IFAIL.EQ.0) GO TO 8900
        CALL LINSPL(IFAIL,2,LW,RW,CLINE)
        IF(IFAIL.NE.0) GO TO 8900
        INFILE=CLINE(LW(1):RW(1))
        OUTFILE=CLINE(LW(2):RW(2))
        CALL REMCHAR(INFILE,CHAR(211))
        CALL REMCHAR(OUTFILE,CHAR(211))
#ifndef UNIX
        CALL LOWCAS(INFILE)
        CALL LOWCAS(OUTFILE)
#endif

! -- The input file is opened.

        CALL ADDQUOTE(INFILE,AFILE)
        OPEN(UNIT=10,FILE=INFILE,STATUS='OLD',IOSTAT=IERR)
        IF(IERR.NE.0)THEN
          WRITE(AMESSAGE,70) TRIM(AFILE)
70        FORMAT(' Cannot open file ',a,'.')
          GO TO 9800
        END IF

C -- A search is made for the covariance matrix.

        DO
          READ(10,'(A)',END=150) CLINE
          I=INDEX(CLINE,'->')
          IF(I.EQ.0) CYCLE
          CALL LOWCAS(CLINE)
          IF(INDEX(CLINE,'eigen').NE.0) CYCLE
          IF(INDEX(CLINE,'parameter covariance matrix').NE.0) GO TO 160
        END DO
150     WRITE(AMESSAGE,155) TRIM(AFILE)
155     FORMAT(' Cannot find a parameter covariance matrix in file ',
     +  a,'.')
        GO TO 9800
160     CONTINUE
        NPAR=0
        READ(10,'(A)',ERR=9000,END=9000) CLINE
        IF(CLINE.EQ.' ') GO TO 160
180     CALL LOWCAS(CLINE)
        IF(CLINE(1:4).NE.' ')THEN  ! No blank space under parameter names.
          BACKSPACE(UNIT=10)
          GO TO 250
        END IF
        NUMPAR=8
        CALL LINSPL(IFAIL,8,LW,RW,CLINE)
        IF(IFAIL.NE.0)THEN
          DO J=7,1,-1
            CALL LINSPL(IFAIL,J,LW,RW,CLINE)
            IF(IFAIL.EQ.0) THEN
              NUMPAR=J
              GO TO 200
            END IF
          END DO
        END IF
200     CONTINUE
        DO J=1,NUMPAR
          NPAR=NPAR+1
          IF(NPAR.GT.MAXPAR)THEN
            WRITE(AMESSAGE,220)
220         FORMAT(' Too many parameters - increase MAXPAR and ',
     +      're-compile program.')
            GO TO 9800
          END IF
          APAR(NPAR)=CLINE(LW(J):RW(J))
        END DO
        READ(10,'(A)',ERR=9000,END=9000) CLINE
        IF(CLINE.EQ.' ') GO TO 250
        GO TO 180

! -- Now that the names of all parameters have been acquired, the actual
!    matrix is first allocated, and then read.

250     CONTINUE
        IF(NPAR.EQ.0) GO TO 9000
        ALLOCATE(MAT(NPAR,NPAR),STAT=IERR)
        IF(IERR.NE.0) GO TO 9100
        DO I=1,NPAR
          READ(10,*,ERR=9000,END=9000) ATEMP,(MAT(I,J),J=1,NPAR)
        END DO
        CLOSE(UNIT=10)
        WRITE(6,260) TRIM(AFILE)
260     FORMAT(' - file ',A,' read ok.')

! -- The matrix is now written to another file.

        CALL ADDQUOTE(OUTFILE,AFILE)
        OPEN(UNIT=20,FILE=OUTFILE,ERR=9200)
        WRITE(20,280) NPAR,NPAR,1
280     FORMAT(3I6)
        DO I=1,NPAR
          WRITE(20,285,ERR=9200) (MAT(I,J),J=1,NPAR)
285       FORMAT(8(1x,1pg14.7))
        END DO
        WRITE(20,290,ERR=9200)
290     FORMAT('* row and column names')
        DO I=1,NPAR
          WRITE(20,300,ERR=9200) TRIM(APAR(I))
300       FORMAT(A)
        END DO
        CLOSE(UNIT=20)
        WRITE(6,320) TRIM(AFILE)
320     FORMAT(' - file ',a,' written ok.')

        GO TO 9900

8900    CONTINUE
        WRITE(6,8910)
8910    FORMAT(' PCOV2MAT is run using the command:',/)
        WRITE(6,8920)
8920    FORMAT('    pcov2mat pestfile matfile',/,/,
     +  ' where',/)
        WRITE(6,8930)
8930    FORMAT('    pestfile is a PEST run record (REC) or matrix ',
     +  '(MTT) file, and')
        WRITE(6,8940)
8940    FORMAT('    matfile  is a matrix file.')
        GO TO 9995

9000    WRITE(AMESSAGE,9010) TRIM(AFILE)
9010    FORMAT(' Error encountered while reading covariance matrix ',
     +  'from file ',a,'.')
        GO TO 9800
9100    WRITE(AMESSAGE,9110)
9110    FORMAT(' Cannot allocate sufficient memory to continue ',
     +  'execution.')
        GO TO 9800
9200    WRITE(AMESSAGE,9210) TRIM(AFILE)
9210    FORMAT(' Cannot write matrix to file ',a,'.')
        GO TO 9800

9800    CONTINUE
        WRITE(6,*)
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



