	PROGRAM RESWRIT

        IMPLICIT NONE

C -- Program RESWRIT translates a RESPROC output file to ASCII format.

        INTEGER NESPAR,NXROW,NBLC,J,I,IERR,IFAIL
        CHARACTER*12 AVERSION
        CHARACTER*200 RESFLE,OUTFLE1,OUTFLE2,AFILE
        CHARACTER*400 COMLIN
        CHARACTER*500 AMESSAGE

        INTEGER ICODE(10)
        DOUBLE PRECISION RCODE(10)
        CHARACTER*200 ACODE(10)

        DOUBLE PRECISION, ALLOCATABLE :: RES(:,:),G(:,:)
        CHARACTER*12, ALLOCATABLE     :: APAR(:)
        CHARACTER*20, ALLOCATABLE     :: AOBS(:)

#ifndef UNIX
#ifdef LAHEY
       open(unit=*,action='read',carriagecontrol='list')
#endif       
#endif

        INCLUDE 'version.inc'
	WRITE(6,1) TRIM(AVERSION)
1       FORMAT(/,' RESWRIT Version ',A,'. ',
     +  'Watermark Numerical Computing.',/)


#ifdef NO_CMDLINE
        WRITE(*,*)
        WRITE(*,3) ' Enter command line arguments > '
        READ(*,3) COMLIN
3       FORMAT(A)
#else
	CALL PGETCL(COMLIN)
#endif
        CALL SPACESUB(COMLIN)
        COMLIN=ADJUSTL(COMLIN)

        NBLC=LEN_TRIM(COMLIN)
        IF(NBLC.EQ.0) GO TO 9900
        DO J=1,NBLC
          IF(COMLIN(J:J).EQ.' ') GO TO 30
        END DO
        GO TO 9900
30      RESFLE=COMLIN(1:J-1)
        DO I=J+1,NBLC
          IF(COMLIN(I:I).NE.' ') GO TO 50
        END DO
50      DO J=I,NBLC
          IF(COMLIN(J:J).EQ.' ') GO TO 61
        END DO
        GO TO 9900
61      OUTFLE1=COMLIN(I:J-1)
        DO  I=J+1,NBLC
          IF(COMLIN(I:I).NE.' ') GO TO 67
        END DO
67      DO J=I,NBLC
          IF(COMLIN(J:J).EQ.' ') GO TO 9900
        END DO
        OUTFLE2=COMLIN(I:NBLC)


        CALL REMCHAR(RESFLE,CHAR(211))
        CALL REMCHAR(OUTFLE1,CHAR(211))
        CALL REMCHAR(OUTFLE2,CHAR(211))

#ifndef UNIX
        CALL LOWCAS(RESFLE)
        CALL LOWCAS(OUTFLE1)
        CALL LOWCAS(OUTFLE2)
#endif

C -- THE RESPROC OUTPUT FILE IS READ.

        CALL OPEN_UNFORMATTED_FILE(IFAIL,10,'read',RESFLE,AMESSAGE)
        IF(IFAIL.NE.0) GO TO 9800
        READ(10,ERR=9000,END=9100) (ICODE(I),I=1,10)
        READ(10,ERR=9000,END=9100) (RCODE(I),I=1,10)
        DO I=1,10
          READ(10,ERR=9000,END=9100) ACODE(I)
        END DO
        READ(10,ERR=9000,END=9100) NESPAR,NXROW
        ALLOCATE(RES(NESPAR,NESPAR),G(NESPAR,NXROW),APAR(NESPAR),
     +  AOBS(NXROW),STAT=IERR)
        IF(IERR.NE.0)THEN
          WRITE(AMESSAGE,35)
35        FORMAT(' Cannot allocate sufficient memory to continue ',
     +    'execution.')
          GO TO 9800
        END IF
        READ(10,ERR=9000,END=9100) ((RES(I,J),I=1,NESPAR),J=1,NESPAR)
        READ(10,ERR=9000,END=9100) ((G(I,J),I=1,NESPAR),J=1,NXROW)
        DO I=1,NESPAR
          READ(10,ERR=9000,END=9100) APAR(I)
        END DO
        DO I=1,NXROW
          READ(10,ERR=9000,END=9100) AOBS(I)
        END DO
        CLOSE(UNIT=10)
        CALL ADDQUOTE(RESFLE,AFILE)
        WRITE(6,55) TRIM(AFILE)
55      FORMAT(' - file ',A,' read ok.')

C -- THE RESOLUTION MATRIX FILE IS WRITTEN.

        CALL ADDQUOTE(OUTFLE1,AFILE)
        OPEN(UNIT=20,FILE=OUTFLE1,ERR=9300)
        WRITE(20,60,ERR=9300) NESPAR,NESPAR,1
60      FORMAT(3I6)
        DO I=1,NESPAR
          WRITE(20,70,ERR=9300) (RES(I,J),J=1,NESPAR)
70        FORMAT(8(1X,1PG13.6))
        END DO
        WRITE(20,75,ERR=9300)
75      FORMAT('* row and column names')
        DO I=1,NESPAR
          WRITE(20,'(A)') TRIM(APAR(I))
        END DO
        CLOSE(UNIT=20)
        WRITE(6,80) TRIM(AFILE)
80      FORMAT(' - file ',A,' written ok.')

C -- THE "G" MATRIX FILE IS WRITTEN.

        CALL ADDQUOTE(OUTFLE2,AFILE)
        OPEN(UNIT=20,FILE=OUTFLE2,ERR=9300)
        WRITE(20,60,ERR=9300) NESPAR,NXROW,2
        DO I=1,NESPAR
          WRITE(20,70,ERR=9300) (G(I,J),J=1,NXROW)
        END DO
        WRITE(20,85)
85      FORMAT('* row names')
        DO I=1,NESPAR
          WRITE(20,'(A)') TRIM(APAR(I))
        END DO
        WRITE(20,86)
86      FORMAT('* column names')
        DO I=1,NXROW
          WRITE(20,'(A)') TRIM(AOBS(I))
        END DO
        CLOSE(UNIT=20)
        WRITE(6,80) TRIM(AFILE)

        GO TO 9990


9000    CALL ADDQUOTE(RESFLE,AFILE)
        WRITE(AMESSAGE,9010) TRIM(AFILE)
9010    FORMAT(' Error encountered in reading unformatted ',
     +  'file ',A,'.')
        GO TO 9800
9100    CALL ADDQUOTE(RESFLE,AFILE)
        WRITE(AMESSAGE,9110) TRIM(AFILE)
9110    FORMAT(' Premature end encountered to unformatted ',
     +  'file ',A,'.')
        GO TO 9800
9300    WRITE(AMESSAGE,9310) TRIM(AFILE)
9310    FORMAT(' Cannot write to file ',A,'.')
        GO TO 9800

9900    WRITE(6,9910)
9910    FORMAT(' RESWRIT is run using the command:',/)
        WRITE(6,9920)
9920    FORMAT('    reswrit resfile matfile1 matfile2',/,/,
     +  ' where',/)
        WRITE(6,9930)
9930    FORMAT('    resfile  is an unformatted RESPROC output file ')
        WRITE(6,9940)
9940    FORMAT('    matfile1 will hold the resolution matrix, and')
        WRITE(6,9950)
9950    FORMAT('    matfile2 will hold the "G" matrix.')
        GO TO 9995

9800    WRITE(6,*)
        CALL WRITMESS(6,AMESSAGE)
9990    CONTINUE
        DEALLOCATE(APAR,AOBS,RES,G,STAT=IERR)
9995    CONTINUE

        END


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

