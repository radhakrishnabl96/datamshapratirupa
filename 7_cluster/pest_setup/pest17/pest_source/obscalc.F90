include 'inter.inc'
include 'utility.inc'
include 'modio.inc'

program obscalc

! -- Program OBSCALC calculates super observation differences from model outputs and
!    observed values.

        use model_input_output
        use utility
        implicit none

interface

	subroutine write_message(increment,iunit,error,leadspace,endspace)
	  integer, intent(in), optional           ::increment,iunit
	  character (len=*), intent(in), optional ::error,leadspace,endspace
	end subroutine write_message

end interface


        integer           :: npnt
        integer           :: nobs,nsup,iobs,isup,ninsfle
        integer           :: ierr,ifail,i,nn
        integer           :: lw(5),rw(5)
        double precision  :: rsum

        character*10      :: precision,decpoint
        character*12      :: aversion
        character*200     :: comline,infile,outfile
        character*400     :: dline
        character*500     :: instruction

        double precision, allocatable :: u1t(:,:),oval(:),obs(:),sup(:)
        character*20,     allocatable :: aobs(:)
        character*200,    allocatable :: insfle(:),outfle(:)

        

        include 'version.inc'
        write(6,10) trim(aversion)
10      format(' OBSCALC Version ',a,'. Watermark Numerical Computing.',/)

        call pgetcl(comline)
        if(comline.eq.' ')then
          infile='obscalc.in'
          outfile='obscalc.out'
        else
          comline=adjustl(comline)
          infile='obscalc.'//trim(comline)//'.in'
          outfile='obscalc.'//trim(comline)//'.out'
        end if
        write(6,80) trim(infile)
80      format(' - reading OBSCALC input file ',a,'....')
        open(unit=10,file=infile,status='old',iostat=ierr)
        if(ierr.ne.0)then
          write(amessage,90) trim(infile)
90        format('Cannot open OBSCALC input file ',a,'.')
          go to 9890
        end if

! -- The OBSCALC input file is now read.

        read(10,*,err=9000,end=9100)
        read(10,*,err=9000,end=9100) nobs,nsup,ninsfle
        allocate(u1t(nobs,nsup),aobs(nobs),oval(nobs),obs(nobs),sup(nsup),stat=ierr)
        if(ierr.ne.0) go to 9200
        allocate(insfle(ninsfle),outfle(ninsfle),stat=ierr)
        if(ierr.ne.0) go to 9200
        read(10,*)
        do isup=1,nsup
          read(10,*,err=9000,end=9100) (u1t(iobs,isup),iobs=1,nobs)
        end do
        read(10,*)
        do iobs=1,nobs
          read(10,*,err=9000,end=9100) aobs(iobs)
          call casetrans(aobs(iobs),'lo')
          aobs(iobs)=adjustl(aobs(iobs))
        end do
        read(10,*)
        do iobs=1,nobs
          read(10,*,err=9000,end=9100) oval(iobs)
        end do
        read(10,*,err=9000,end=9100)
        do i=1,ninsfle
91        read(10,'(a)',err=9000,end=9100) dline
          if(dline.eq.' ') go to 91
          call spacesub(dline)
          call linspl(ifail,2,lw,rw,dline)
          if(ifail.ne.0) go to 9000
          insfle(i)=dline(lw(1):rw(1))
          outfle(i)=dline(lw(2):rw(2))
          call remchar(insfle(i),char(211))
          call remchar(outfle(i),char(211))
          if(comline.ne.' ')then
            nn=npnt(outfle(i))
            if(nn.lt.0)then
              outfle(i)=trim(outfle(i))//'.'//trim(comline)
            else
              outfle(i)=outfle(i)(1:nn)//trim(comline)//outfle(i)(nn:)
            end if
          end if
        end do
        close(unit=10)
        write(6,120) trim(infile)
120     format(' - file ',a,' read ok.')

! Model outputs are read from model output files.

        write(6,*)
        write(6,124)
124     format(' - processing instruction files and reading model output files....')
        precision='single'
        decpoint='point'
        call initialise_mio(ifail,1,ninsfle,1,nobs,precision,decpoint)
        if(ifail.ne.0) go to 9400
        do i=1,ninsfle
          call put_mio_file(ifail,3,i,insfle(i))
          if(ifail.ne.0) go to 9400
          call put_mio_file(ifail,4,i,outfle(i))
          if(ifail.ne.0) go to 9400
        end do
        call store_instruction_set(ifail)
        if(ifail.ne.0)then
          go to 9890
        end if
        call read_model_output_files(ifail,nobs,aobs,obs,instruction)
        if(ifail.ne.0)then
          write(6,*)
          amessage='  '//trim(amessage)
          call write_message()
          if(instruction.ne.' ')then
            write(6,*)
            write(6,130)
130         format('  Instruction follows:-')
            write(6,140) trim(instruction)
140         format(3x,a)
          end if
          go to 9900
        end if
        call finalise_mio()
        write(6,150)
150     format(' - model output files read ok.')

! -- Differences are now taken with measured values.

        do iobs=1,nobs
          obs(iobs)=obs(iobs)-oval(iobs)
        end do

! -- These differences are now projected onto the directions of super observations.

        do isup=1,nsup
          rsum=0.0d0
          do iobs=1,nobs
            rsum=rsum+u1t(iobs,isup)*obs(iobs)
          end do
          sup(isup)=rsum
        end do

        write(6,*)
        write(6,350) trim(outfile)
350     format(' - writing OBSCALC output file ',a,'...')
        open(unit=20,file=outfile)
        do isup=1,nsup
          write(20,310) sup(isup)
310       format(1x,1pg14.7)
        end do
        close(unit=20)
        write(6,320) trim(outfile)
320     format(' - file ',a,' written ok.')

        go to 9900

9000    write(amessage,9010) trim(infile)
9010    format('*** Error reading OBSCALC input file ',a,' ***')
        go to 9890
9100    write(amessage,9110) trim(infile)
9110    format('*** Premature end to OBSCALC input file ',a,' ***')
        go to 9890
9200    write(amessage,9210)
9210    format('*** Cannot allocate sufficient memory for OBSCALC ',  &
        'execution ***')
        go to 9890
9400    write(amessage,9410)
9410    format(' *** Error condition encountered ***')
        go to 9890

9890    continue
        write(6,*)
        amessage='  '//trim(amessage)
        call write_message()

! -- Memory is de-allocated.

9900    continue

        deallocate(u1t,oval,aobs,insfle,outfle,obs,stat=ierr)

end program obscalc



subroutine casetrans(string,hi_or_lo)

! -- Subroutine casetrans converts a string to upper or lower case.

! -- Arguments are as follows:-
!      string:	  contains the string whose case must be changed
!      hi_or_lo:  must be either 'lo' or 'hi' to indicate
!                 change of case direction.

! -- Revision history:-
!       June-November, 1995: version 1.

	character (len=*), intent(inout)        :: string
	character (len=*), intent(in)           :: hi_or_lo
	character                               :: alo, ahi
	integer                                 :: inc,i

	if(hi_or_lo.eq.'lo') then
	  alo='A'; ahi='Z'; inc=iachar('a')-iachar('A')
	else if(hi_or_lo.eq.'hi') then
	  alo='a'; ahi='z'; inc=iachar('A')-iachar('a')
	else
	  call sub_error('CASETRANS')
	endif

	do i=1,len_trim(string)
	  if((string(i:i).ge.alo).and.(string(i:i).le.ahi)) &
	  string(i:i)=achar(iachar(string(i:i))+inc)
	end do

	return

end subroutine casetrans



subroutine addquote(afile,aqfile)

! -- Subroutine ADDQUOTE adds quotes to a filename if it has a space in it.

! -- Arguments are as follows:-
!        afile:       the name of the file
!        aqfile:      the name of the file with quotes added

        character (len=*), intent(in)   :: afile
        character (len=*), intent(out)  :: aqfile
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
end subroutine addquote


integer function nextunit()

! -- Function nextunit determines the lowest unit number available for
! -- opening.

! -- Revision history:-
!       June-November, 1995: version 1.

	logical::lopen

	do nextunit=10,100
	  inquire(unit=nextunit,opened=lopen)
	  if(.not.lopen) return
	end do
	write(6,10)
10      format(' *** No more unit numbers to open files ***')
	stop

end function nextunit



subroutine remchar(astring,ach)

       implicit none

       character*(*), intent(inout) :: astring
       character*(*), intent(in)    :: ach

       integer ll,ii,icount

       icount=0
       ll=len_trim(ach)

10     ii=index(astring,ach)
       if(ii.eq.0) then
         if(icount.eq.0)return
         go to 20
       end if
       icount=icount+1
       astring(ii:ii-1+ll)=' '
       go to 10

20     astring=adjustl(astring)
       return

end subroutine remchar


SUBROUTINE WHICH1(IFAIL,NPAR,IPAR,APAR,TPAR)

! -- SUBROUTINE WHICH1 FINDS A STRING IN AN ARRAY OF STRINGS

        implicit none

        integer, intent(out)                :: ifail    ! error indicator
        integer, intent(in)                 :: npar     ! number of parameters
        integer, intent(inout)              :: ipar     ! where to start the search
        character (len=*), intent(in), dimension(npar)  :: apar     ! parameter names
        character (len=*), intent(inout)    :: tpar     ! parameter name to look for

        integer                             :: i

        IFAIL=0
        IF((IPAR.LT.1).OR.(IPAR.GT.NPAR)) IPAR=1
        call casetrans(tpar,'lo')
        IF(TPAR.EQ.APAR(IPAR)) RETURN
        IF(IPAR.NE.NPAR)THEN
          DO 20 I=IPAR+1,NPAR
            IF(TPAR.EQ.APAR(I))THEN
              IPAR=I
              RETURN
            END IF
20        CONTINUE
        END IF
        IF(IPAR.NE.1)THEN
          DO 40 I=IPAR-1,1,-1
          IF(TPAR.EQ.APAR(I)) THEN
            IPAR=I
            RETURN
          END IF
40        CONTINUE
        END IF
        IFAIL=1
        RETURN
END subroutine which1


subroutine sub_error(subname)

! -- Subroutine sub_error names the subroutine causing a run-time error.

! -- Arguments are as follows:-
!       subname:  name of offending subroutine

! -- Revision history:-
!       June-November, 1995: version 1.

	character (len=*)               ::subname

	write(6,10) trim(subname)
10      format(/,' *** PROGRAMMING ERROR CALLING SUBROUTINE ',a,' ***')
	stop

end subroutine sub_error


!*****************************************************************************
! subroutines comprising the generic subroutine NUM2CHAR ------->
!*****************************************************************************

! -- Subroutine num2char writes the character equivalent of a number.

! -- Arguments are as follows:-
!       value:   the number to be expressed in character form
!       string:  the number expressed in character form
!       nchar:   the maximum number of characters in which to express number


subroutine i2a(value,string,nchar)

	integer, intent(in)             :: value
	character (len=*), intent(out)  :: string
	integer, intent(in), optional   :: nchar
	character (len=12)              :: afmt
	integer                         :: llen

	string=' '
	afmt='(i    )'
	llen=min(30,len(string))
	if(present(nchar)) llen=min(llen,nchar)
	write(afmt(3:6),'(i4)') llen
	write(string(1:llen),afmt,err=100) value
	string=adjustl(string)
	if(string(1:1).eq.'*') go to 100
	return

100     string(1:llen)=repeat('#',llen)
	return

end subroutine i2a


subroutine d2a(value,string,nchar)

	double precision, intent(in)    :: value
	character (len=*), intent(out)  :: string
	integer, intent(in), optional   :: nchar
	integer                         :: llen, ifail
	double precision                :: value_check
	character (len=32)              :: word

	string=' '
	llen=min(29,len(string))
	if(present(nchar)) llen=min(llen,nchar)
	call wrtsig(ifail,value,word,llen,1,value_check,0)
	if(ifail.lt.0) then
	  call sub_error('D2A')
	else if(ifail.gt.0) then
	  string(1:llen)=repeat('#',llen)
	else
	  string=adjustl(word)
	end if
	return

end subroutine d2a


subroutine r2a(value,string,nchar)

	real,intent(in)                 :: value
	character (len=*), intent(out)  :: string
	integer, intent(in), optional   :: nchar
	integer                         :: llen,ifail
	double precision                :: dvalue,dvalue_check
	character (len=32)              :: word

	string=' '
	llen=min(29,len(string))
	if(present(nchar)) llen=min(llen,nchar)
	dvalue=value
	call wrtsig(ifail,dvalue,word,llen,0,dvalue_check,0)
	if(ifail.lt.0) then
	  call sub_error('R2A')
	else if(ifail.gt.0) then
	  string(1:llen)=repeat('#',llen)
	else
	  string=adjustl(word)
	end if
	return

end subroutine r2a

!*****************************************************************************
! End of subroutines comprising the generic subroutine NUM2CHAR
!*****************************************************************************




subroutine write_message(increment,iunit,error,leadspace,endspace)

! -- Subroutine write_message formats and writes a message.

! -- Arguments are as follows:-
!       increment:  the increment to the message counter
!       iunit:      the unit number to which the message is written
!       error:      if "yes" precede message with "Error"
!       leadspace   if "yes" precede message with blank line
!       endspace    if "yes" follow message by blank line

! -- Revision history:-
!       June-November, 1995: version 1.

	use utility
        implicit none
	
	integer, intent(in), optional           ::increment,iunit
	integer                                 ::jend,i,nblc,junit,leadblank
	integer                                 ::itake,j
	character (len=*), intent(in), optional ::error,leadspace,endspace
	character (len=20) ablank
		
        if(amessage(1:1).ne.' ')amessage=' '//trim(amessage)
	ablank=' '
	itake=0
	j=0
	if(present(increment)) imessage=imessage+increment
	if(present(iunit))then
	  junit=iunit
	else
	  junit=6
	end if
	if(present(leadspace))then
	  if(leadspace.eq.'yes') write(junit,*)
	endif
	if(present(error))then
	  if(index(error,'yes').ne.0)then
	    nblc=len_trim(amessage)
	    amessage=adjustr(amessage(1:nblc+8))
	    if(nblc+8.lt.len(amessage)) amessage(nblc+9:)=' '
	    amessage(1:8)=' Error: '
	  end if
	end if

	do i=1,20
	  if(amessage(i:i).ne.' ')exit
20      end do
	leadblank=i-1
	nblc=len_trim(amessage)
5       jend=j+78-itake
	if(jend.ge.nblc) go to 100
	do i=jend,j+1,-1
	if(amessage(i:i).eq.' ') then
	  if(itake.eq.0) then
	     write(junit,'(a)',err=200) amessage(j+1:i)
	     itake=2+leadblank
	  else
	     write(junit,'(a)',err=200) ablank(1:leadblank+2)//amessage(j+1:i)
	  end if
	  j=i
	  go to 5
	end if
	end do
	if(itake.eq.0)then
	  write(junit,'(a)',err=200) amessage(j+1:jend)
	  itake=2+leadblank
	else
	  write(junit,'(a)',err=200) ablank(1:leadblank+2)//amessage(j+1:jend)
	end if
	j=jend
	go to 5
100     jend=nblc
	if(itake.eq.0)then
	write(junit,'(a)',err=200) amessage(j+1:jend)
	  else
	write(junit,'(a)',err=200) ablank(1:leadblank+2)//amessage(j+1:jend)
	  end if
	if(present(endspace))then
	  if(endspace.eq.'yes') write(junit,*)
	end if
	return

200     call exit(100)

end subroutine write_message



integer function npnt(astring)

        implicit none
        character*(*) astring
        integer i,j

        npnt=-99999999
        j=len_trim(astring)
        do i=1,j
          if(astring(i:i).eq.'.')then
            npnt=i
            return
          end if
        end do
        return
end function npnt


SUBROUTINE SPACESUB(ASTRING)

      INTEGER I,J,K,N
      CHARACTER*1 BB
      CHARACTER*(*) ASTRING

      BB=CHAR(211)
      N=LEN_TRIM(ASTRING)
      K=1
10    CONTINUE
      IF(K.GT.N) GO TO 100
      DO I=K,N
        IF((ASTRING(I:I).EQ.'''').OR.(ASTRING(I:I).EQ.'"'))THEN
          ASTRING(I:I)=' '
          DO J=I+1,N
            IF((ASTRING(J:J).EQ.'''').OR.(ASTRING(J:J).EQ.'"'))THEN
              ASTRING(J:J)=' '
              K=J+1
              GO TO 10
            END IF
            IF(ASTRING(J:J).EQ.' ')ASTRING(J:J)=BB
          END DO
          GO TO 100
        END IF
      END DO

100   CONTINUE
      RETURN

END SUBROUTINE SPACESUB


subroutine linspl(ifail,num,lw,rw,cline)

! -- Subroutine LINSPL splits a line into whitespace-separated substrings.

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
        if((cline(i:i).ne.' ').and.(cline(i:i).ne.',')   &
        .and.(ichar(cline(i:i)).ne.9)) go to 20
10      continue
        ifail=1
        return
20      nw=nw+1
        lw(nw)=i
        do 30 i=lw(nw)+1,nblc
        if((cline(i:i).eq.' ').or.(cline(i:i).eq.',')    &
        .or.(ichar(cline(i:i)).eq.9)) go to 40
30      continue
        rw(nw)=nblc
        if(nw.lt.num) ifail=1
        return
40      rw(nw)=i-1
        j=rw(nw)
        go to 5

end subroutine linspl



	SUBROUTINE WRTSIG(IFAIL,VAL,WORD,NW,PRECIS,TVAL,NOPNT)
! --
! -- SUBROUTINE WRTSIG WRITES A NUMBER INTO A CONFINED SPACE WITH MAXIMUM
! -- PRECISION
! --

! -- Revision history:-
!       July, 1993: version 1.
!       August 1994: modified for unix version (#ifdef's added)
!       August, 1995: #ifdefs commented out for inclusion in Groundwater
!                     Data Utilities

!       failure criteria:
!           ifail= 1 ...... number too large or small for single precision type
!           ifail= 2 ...... number too large or small for double precision type
!           ifail= 3 ...... field width too small to represent number
!           ifail=-1 ...... internal error type 1
!           ifail=-2 ...... internal error type 2
!           ifail=-3 ...... internal error type 3

	INTEGER PRECIS,LW,POS,INC,D,P,W,J,JJ,K,JEXP,N,JFAIL,NW, &
	EPOS,PP,NOPNT,KEXP,IFLAG,LEXP
	INTEGER IFAIL
	DOUBLE PRECISION VAL,TVAL
	CHARACTER*29 TWORD,TTWORD,FMT*14
	CHARACTER*(*) WORD

	LEXP=0
	IFLAG=0
	WORD=' '
	POS=1
	IF(VAL.LT.0.0D0)POS=0
!#ifdef USE_D_FORMAT
!        WRITE(TWORD,'(1PD23.15D3)') VAL
!#else
	WRITE(TWORD,'(1PE23.15E3)') VAL
!#endif
	READ(TWORD(20:23),'(I4)') JEXP
	EPOS=1
	IF(JEXP.LT.0)EPOS=0

	JFAIL=0
	IFAIL=0
	IF(PRECIS.EQ.0)THEN
	  LW=MIN(15,NW)
	ELSE
	  LW=MIN(23,NW)
	END IF

	N=0
	IF(NOPNT.EQ.1)N=N+1
	IF(POS.EQ.1)N=N+1
	IF(PRECIS.EQ.0)THEN
	  IF(ABS(JEXP).GT.38)THEN
	    IFAIL=1
	    RETURN
	  END IF
	  IF(POS.EQ.1) THEN
	    IF(LW.GE.13) THEN
	      WRITE(WORD,'(1PE13.7)',ERR=80) VAL
	      GO TO 200
	    END IF
	  ELSE
	    IF(LW.GE.14)THEN
	      WRITE(WORD,'(1PE14.7)',ERR=80) VAL
	      GO TO 200
	    END IF
	  END IF
	  IF(LW.GE.14-N) THEN
	    LW=14-N
	    GO TO 80
	  END IF
	ELSE
	  IF(ABS(JEXP).GT.275)THEN
	    IFAIL=2
	    RETURN
	  END IF
	  IF(POS.EQ.1) THEN
	    IF(LW.GE.22) THEN
!#ifdef USE_D_FORMAT
!              WRITE(WORD,'(1PD22.15D3)',ERR=80) VAL
!#else
	      WRITE(WORD,'(1PE22.15E3)',ERR=80) VAL
!#endif
	      GO TO 200
	    END IF
	  ELSE
	    IF(LW.GE.23) THEN
!#ifdef USE_D_FORMAT
!              WRITE(WORD,'(1PD23.15D3)',ERR=80) VAL
!#else
	      WRITE(WORD,'(1PE23.15E3)',ERR=80) VAL
!#endif
	      GO TO 200
	    END IF
	  END IF
	  IF(LW.GE.23-N)THEN
	    LW=23-N
	    GO TO 80
	  END IF
	END IF

	IF(NOPNT.EQ.1)THEN
	  IF((JEXP.EQ.LW-2+POS).OR.(JEXP.EQ.LW-3+POS))THEN
	    WRITE(FMT,15)LW+1
15          FORMAT('(F',I2,'.0)')
	    WRITE(WORD,FMT,ERR=19) VAL
	    IF(INDEX(WORD,'*').NE.0) GO TO 19
	    IF(WORD(1:1).EQ.' ') GO TO 19
	    WORD(LW+1:LW+1)=' '
	    GO TO 200
	  END IF
	END IF
19      D=MIN(LW-2+POS,LW-JEXP-3+POS)
20      IF(D.LT.0) GO TO 80
	WRITE(FMT,30) LW,D
30      FORMAT('(F',I2,'.',I2,')')
	WRITE(WORD,FMT,ERR=80) VAL
	IF(INDEX(WORD,'*').NE.0) THEN
	  D=D-1
	  GO TO 20
	END IF
	K=INDEX(WORD,'.')
	IF(K.EQ.0)THEN
	  IFAIL=-1
	  RETURN
	END IF
	IF((K.EQ.1).OR.((POS.EQ.0).AND.(K.EQ.2)))THEN
	  DO 70 J=1,3
	  IF(K+J.GT.LW) GO TO 75
	  IF(WORD(K+J:K+J).NE.'0') GO TO 200
70        CONTINUE
	  GO TO 80
75        IFAIL=3
	  RETURN
	END IF
	GO TO 200

80      WORD=' '
	IF(NOPNT.EQ.0)THEN
	  D=LW-7
	  IF(POS.EQ.1) D=D+1
	  IF(EPOS.EQ.1) D=D+1
	  IF(ABS(JEXP).LT.100) D=D+1
	  IF(ABS(JEXP).LT.10) D=D+1
	  IF((JEXP.GE.100).AND.(JEXP-(D-1).LT.100))THEN
	    P=1+(JEXP-99)
	    D=D+1
	    LEXP=99
	  ELSE IF((JEXP.GE.10).AND.(JEXP-(D-1).LT.10))THEN
	    P=1+(JEXP-9)
	    D=D+1
	    LEXP=9
	  ELSE IF((JEXP.EQ.-10).OR.(JEXP.EQ.-100)) THEN
	    IFLAG=1
	    D=D+1
	  ELSE
	    P=1
	  END IF
	  INC=0
85        IF(D.LE.0) GO TO 300
	  IF(IFLAG.EQ.0)THEN
	    WRITE(FMT,100,ERR=300) P,D+7,D-1
	  ELSE
	    WRITE(FMT,100,ERR=300) 0,D+8,D
	  END IF
	  WRITE(TWORD,FMT) VAL
	  IF(IFLAG.EQ.1) GO TO 87
	  READ(TWORD(D+4:D+7),'(I4)',ERR=500) KEXP
	  IF(((KEXP.EQ.10).AND.((JEXP.EQ.9).OR.(LEXP.EQ.9))).OR. &
	  ((KEXP.EQ.100).AND.((JEXP.EQ.99).OR.LEXP.EQ.99))) THEN
	    IF(INC.EQ.0)THEN
	      IF(LEXP.EQ.0)THEN
		IF(D-1.EQ.0) THEN
		  D=D-1
		ELSE
		  P=P+1
		END IF
	      ELSE IF(LEXP.EQ.9)THEN
		IF(JEXP-(D-2).LT.10) THEN
		  P=P+1
		ELSE
		  D=D-1
		END IF
	      ELSE IF(LEXP.EQ.99)THEN
		IF(JEXP-(D-2).LT.100)THEN
		  P=P+1
		ELSE
		  D=D-1
		END IF
	      END IF
	      INC=INC+1
	      GO TO 85
	    END IF
	  END IF
!#ifdef USE_D_FORMAT
!87        J=INDEX(TWORD,'D')
!#else
87        J=INDEX(TWORD,'E')
!#endif
	  GO TO 151
	END IF
	INC=0
	P=LW-2
	PP=JEXP-(P-1)
	IF(PP.GE.10)THEN
	  P=P-1
	  IF(PP.GE.100)P=P-1
	ELSE IF(PP.LT.0)THEN
	  P=P-1
	  IF(PP.LE.-10)THEN
	    P=P-1
	    IF(PP.LE.-100)P=P-1
	  END IF
	END IF
	IF(POS.EQ.0)P=P-1
90      CONTINUE
	D=P-1
	W=D+8
	WRITE(FMT,100) P,W,D
	IF(D.LT.0)THEN
	  IF(JFAIL.EQ.1) GO TO 300
	  JFAIL=1
	  P=P+1
	  GO TO 90
	END IF
!#ifdef USE_D_FORMAT
!100     FORMAT('(',I2,'pD',I2,'.',I2,'D3)')
!#else
100     FORMAT('(',I2,'pE',I2,'.',I2,'E3)')
!#endif
	WRITE(TWORD,FMT) VAL
!#ifdef USE_D_FORMAT
!        J=INDEX(TWORD,'D')
!#else
	J=INDEX(TWORD,'E')
!#endif
	IF(TWORD(J-1:J-1).NE.'.')THEN
	  IFAIL=-1
	  RETURN
	END IF
	N=1
	IF(TWORD(J+1:J+1).EQ.'-') N=N+1
	IF(TWORD(J+2:J+2).NE.'0') THEN
	  N=N+2
	  GO TO 120
	END IF
	IF(TWORD(J+3:J+3).NE.'0') N=N+1
120     N=N+1
	IF(J+N-2-POS.LT.LW)THEN
	  IF(INC.EQ.-1) GO TO 150
	  TTWORD=TWORD
	  P=P+1
	  INC=1
	  GO TO 90
	ELSE IF(J+N-2-POS.EQ.LW) THEN
	  GO TO 150
	ELSE
	  IF(INC.EQ.1)THEN
	    TWORD=TTWORD
	    GO TO 150
	  END IF
	  IF(JFAIL.EQ.1) GO TO 300
	  P=P-1
	  INC=-1
	  GO TO 90
	END IF

150     J=INDEX(TWORD,'.')
151     IF(POS.EQ.0)THEN
	  K=1
	ELSE
	 K=2
	END IF
	WORD(1:J-K)=TWORD(K:J-1)
	JJ=J
	J=J-K+1
	IF(PRECIS.EQ.0)THEN
	  WORD(J:J)='E'
	ELSE
	  WORD(J:J)='D'
	END IF
	JJ=JJ+2
	IF(NOPNT.EQ.0) JJ=JJ-1
	IF(TWORD(JJ:JJ).EQ.'-')THEN
	  J=J+1
	  WORD(J:J)='-'
	END IF
	IF(TWORD(JJ+1:JJ+1).NE.'0')THEN
	  J=J+2
	  WORD(J-1:J)=TWORD(JJ+1:JJ+2)
	  GO TO 180
	END IF
	IF(TWORD(JJ+2:JJ+2).NE.'0')THEN
	  J=J+1
	  WORD(J:J)=TWORD(JJ+2:JJ+2)
	END IF
180     J=J+1
	WORD(J:J)=TWORD(JJ+3:JJ+3)
	IF(IFLAG.EQ.1)THEN
	  IF(POS.EQ.1)THEN
	    JJ=1
	  ELSE
	    JJ=2
	  END IF
	  N=len_trim(WORD)
	  DO 190 J=JJ,N-1
190       WORD(J:J)=WORD(J+1:J+1)
	  WORD(N:N)=' '
	END IF

200     IF(len_trim(WORD).GT.LW)THEN
	  IFAIL=-2
	  RETURN
	END IF
	WRITE(FMT,30) LW,0
	READ(WORD,FMT,ERR=400) TVAL
	RETURN
300     IFAIL=3
	RETURN
400     IFAIL=-3
	RETURN
500     IFAIL=-2
	RETURN
	END

