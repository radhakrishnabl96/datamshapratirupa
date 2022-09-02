module model_input_output

      private
      
! -- Variables

      integer                            :: numinfile  ! Number of input files
      integer                            :: numoutfile ! Number of output files
      integer                            :: precis     ! Precision protocol
      integer                            :: nopnt      ! decimal point protocol
      integer                            :: asize      ! size of a array
      integer                            :: numl       ! size of ll array
      integer                            :: ninstr     ! size of lcins array
      integer                            :: mcall      ! number of model calls
      integer, allocatable, dimension(:) :: ll         ! holds line advance data
      integer, allocatable, dimension(:) :: lcins      ! pointer to instructions
      integer, allocatable, dimension(:) :: nw         ! minimum word length of a parameter
      integer, allocatable, dimension(:) :: obsn1(:)   ! stores part of observation instruction
      integer, allocatable, dimension(:) :: obsn2(:)   ! stores part of observation instruction
      integer, allocatable, dimension(:) :: iiobs(:)   ! stores part of observation instruction   
      character (len=180)                :: afile      ! temporary character storage
      character (len=1),   allocatable, dimension(:)  :: mrkdel     ! Marker delimiters
      character (len=1),   allocatable, dimension(:)  :: pardel     ! Parameter delimiters
      character (len=180), allocatable, dimension(:)  :: tempfile   ! Template files
      character (len=180), allocatable, dimension(:)  :: modinfile  ! Model input files
      character (len=180), allocatable, dimension(:)  :: insfile    ! Instruction files
      character (len=180), allocatable, dimension(:)  :: modoutfile ! Model output files
      character (len=23), allocatable,  dimension(:)  :: pword      ! Word for each parameter
      character (len=1), allocatable,   dimension(:)  :: a          ! holds compressed instruction set
      character (len=80)                 :: errsub     ! Character string for error header
      character (len=2000)               :: dline      ! Character string for text storage

! -- Visible programs

      public initialise_mio,                    &
             put_mio_file,                      &
             store_instruction_set,             &
             process_template_files,            &
             delete_output_files,               &
             write_model_input_files,           &
             read_model_output_files,           &
             finalise_mio
             

! -- Subroutines
      
      
      
contains


subroutine initialise_mio(ifail,numin,numout,npar,nobs,precision,decpoint)
      
! -- Subroutine MIO_INITIALISE initialises the model_input_output module.

      use utility
      implicit none      
      
      integer, intent(out)                     :: ifail   ! indicates failure condition
      integer, intent(in)                      :: numin   ! number of model input files
      integer, intent(in)                      :: numout  ! number of model output files
      integer, intent(in)                      :: npar    ! number of parameters
      integer, intent(in)                      :: nobs    ! number of observations
      character (len=*), intent(in), optional  :: precision
      character (len=*), intent(in), optional  :: decpoint
      
      integer                             :: ierr
      character (len=10)                  :: atemp
      
      errsub='Error in subroutine MIO_INITIALISE:'
      ifail=0

      if((numin.le.0).or.(numout.le.0))then
        write(amessage,10) trim(errsub)
10      format(a,' variables NUMIN and NUMOUT must both be supplied as positive.')
        ifail=1
        return
      end if
!      allocate(tempfile(numin),modinfile(numin),insfile(numout),modoutfile(numout), &
!      mrkdel(numout),pardel(numin),stat=ierr)
      allocate(tempfile(numin),modinfile(numin),pardel(numin),stat=ierr)
      if(ierr.ne.0)then
        write(amessage,20) trim(errsub)
        ifail=1
        return
      end if
      allocate(insfile(numout),modoutfile(numout),mrkdel(numout),stat=ierr)
      if(ierr.ne.0)then
        write(amessage,20) trim(errsub)
20      format(a,' cannot allocate sufficient memory to store model interface filenames.')
        ifail=1
        return
      end if
      tempfile=' '        ! an array
      modinfile=' '       ! an array
      insfile=' '         ! an array
      modoutfile=' '      ! an array
      
      numinfile=numin
      numoutfile=numout
      if(present(precision))then
        atemp=adjustl(precision)
        call casetrans(atemp,'lo')
        if(atemp(1:6).eq.'double')then
          precis=1
        else 
          precis=0
        end if
      else
        precis=0
      end if
      if(present(decpoint))then
        atemp=adjustl(decpoint)
        call casetrans(atemp,'lo')
        if(atemp(1:7).eq.'nopoint')then
          nopnt=1
        else
          nopnt=0
        end if
      else
        nopnt=0
      end if
      
      mcall=0

! -- Various work arrays are allocated.

      if((npar.le.0).or.(nobs.le.0))then
        write(amessage,30) trim(errsub)
30      format(a,' variables NPAR and NOBS must both be supplied as positive.')
        ifail=1
        return
      end if

      allocate(obsn1(nobs),obsn2(nobs),iiobs(nobs),stat=ierr)
      if(ierr.ne.0) go to 9200
      allocate(nw(npar),pword(npar),stat=ierr)
      if(ierr.ne.0) go to 9200
      
      return

9200  write(amessage,9210) trim(errsub)
9210  format(a,' cannot allocate sufficient memory to store model interface work arrays.')
      ifail=1
      return
      
end subroutine initialise_mio



subroutine put_mio_file(ifail,itype,inum,filename)

! -- Subroutine PUT_MIO_FILE provides a filename for storage in the model_input_output
!    module.

       use utility
       implicit none
       
       integer, intent(out)            :: ifail      ! indicates error condition
       integer, intent(in)             :: itype      ! type of file
       integer, intent(in)             :: inum       ! file number
       character (len=*), intent(in)   :: filename   ! name of file
       
       errsub = 'Error in subroutine PUT_MIO_FILE:'
       
       ifail=0
       if((itype.eq.1).or.(itype.eq.2))then
         if((inum.lt.1).or.(inum.gt.numinfile))go to 9000
         if(itype.eq.1)then
           tempfile(inum)=filename
         else
           modinfile(inum)=filename
         end if
       else if ((itype.eq.3).or.(itype.eq.4))then
         if((inum.lt.1).or.(inum.gt.numoutfile))go to 9000
         if(itype.eq.3)then
           insfile(inum)=filename
         else
           modoutfile(inum)=filename
         end if
       end if

       return

9000   write(amessage,9010) trim(errsub)
9010   format(' supplied INUM argument out of range.')
       ifail=1
       return

end subroutine put_mio_file
       

subroutine store_instruction_set(ifail)

! -- Subroutine STORE_INSTRUCTION_SET reads all instruction files, storing the 
!    instructions contained therein for more efficient later access.

        use utility
        implicit none
        
        integer, intent(out)          :: ifail
        
        integer                       :: i,nblbmx,iunit,ierr,nblc,j,ins,isum,numl
        integer                       :: nextunit

        ifail=0
        errsub='Error in user-supplied instruction set for reading model output files:'

        NBLBMX=0
        ASIZE=0
        NUML=0
        NINSTR=0
        DO 200 I=1,numoutfile
          if(insfile(i).eq.' ')then
            write(amessage,10)
10          format('Error in subroutine STORE_INSTRUCTION_SET: names have not ',  &
            'been installed for all instruction files.')
            ifail=1
            return
          end if
          if(modoutfile(i).eq.' ')then
            write(amessage,11)
11          format('Error in subroutine STORE_INSTRUCTION_SET: names have not ', &
            'been installed for all model output files.')
            ifail=1
            return
          end if
          call addquote(insfile(i),afile)
          iunit=nextunit()
          open(unit=iunit,file=insfile(i),status='old',iostat=ierr)
          if(ierr.ne.0)then
            write(amessage,20) trim(afile)
20          format('Cannot open instruction file ',a,'.')
            ifail=1
            return
          end if
          READ(iunit,'(A)',END=9400,err=9000) dline
          call remchar(dline,achar(9))
!          CALL TABREM(dline)
          CALL casetrans(dline,'lo')
          IF((dline(1:3).NE.'pif').AND.(DLINE(1:3).NE.'jif'))GO TO 9400
          MRKDEL(I)=dline(5:5)
          IF(MRKDEL(I).EQ.' ') GO TO 9400
50        READ(iunit,'(A)',END=180,err=9000) dline
          call remchar(dline,achar(9))
!          CALL TABREM(dline)
          IF(INDEX(dline,MRKDEL(I)).EQ.0) CALL CMPRSS()
          NBLC=len_trim(dline)
          IF(NBLC.EQ.0) GO TO 50
          IF(NBLC.GT.NBLBMX)NBLBMX=NBLC
          NINSTR=NINSTR+1
          DO 60 J=1,NBLC
            IF(dline(J:J).NE.' ') THEN
              IF((dline(J:J).EQ.'l').OR.(dline(J:J).EQ.'L')) NUML=NUML+1
              GO TO 100
            END IF
60        CONTINUE
100       ASIZE=ASIZE+NBLC
          GO TO 50
180       CLOSE(UNIT=iunit)
200     CONTINUE
        NBLBMX=NBLBMX+1
        DO 300 I=1,numoutfile
          ASIZE=ASIZE+2+len_trim(modoutfile(i))
300     continue
        NINSTR=NINSTR+numoutfile
        
! -- Memory is allocated for storage of instructions.

	ALLOCATE(A(ASIZE),LL(NUML),LCINS(NINSTR),STAT=IERR)
	if(ierr.ne.0)then
	  write(amessage,310)
310	  format('Cannot allocate sufficient memory to store instruction set.')
          ifail=1
          return
        end if
        a=' '              ! a is an array (do the same in PEST)
        
! -- The instruction set is now re-read and stored.

        INS=0
        ISUM=0
        DO 400 I=1,numoutfile
          call addquote(insfile(i),afile)
          iunit=nextunit()
          open(unit=iunit,file=insfile(i),status='old',iostat=ierr)
          if(ierr.ne.0)then
            write(amessage,20) trim(afile)
            ifail=1
            return
          end if
          READ(iunit,*,err=9000)
          INS=INS+1
          dline(1:1)=aCHAR(2)
          dline(2:2)=' '
          dline(3:)=modoutfile(I)
          LCINS(INS)=ISUM+1
          NBLC=len_trim(dline)
          DO 320 J=1,NBLC
            A(J+ISUM)=dline(J:J)
320       continue
          ISUM=ISUM+NBLC
350       READ(iunit,322,END=181,err=9000) dline
322       FORMAT(A)
          call remchar(dline,achar(9))
!          CALL TABREM(dline)
          IF(INDEX(dline,MRKDEL(I)).EQ.0) CALL CMPRSS()
          NBLC=len_trim(dline)
          IF(NBLC.EQ.0) GO TO 350
          INS=INS+1
          LCINS(INS)=ISUM+1
          DO 370 J=1,NBLC
            A(J+ISUM)=dline(J:J)
370       continue
          ISUM=ISUM+NBLC
          GO TO 350
181       CLOSE(UNIT=iunit)
400     CONTINUE
        
!        write(*,*) (a(i),i=1,asize)    !debug
        return

9000    write(amessage,9010) trim(afile)
9010    format('Unable to read instruction file ',a,'.')
        ifail=1
        return
9400    write(amessage,9410) trim(afile)
9410    format('Header of "pif" or "jif" expected on first line of instruction ',  &
        'file ',a,'.')
        ifail=1
        return
        
end subroutine store_instruction_set


subroutine process_template_files(ifail,npar,apar)

! -- Subroutine PROCESS_TEMPLATE_FILES does rudmentary checking of template files. 
!    However its main role is to find the smallest character width to which each
!    parameter will be written.


        use inter
        use utility
        implicit none
        
        integer, intent(out)                :: ifail ! indicates error condition
        integer, intent(in)                 :: npar  ! number of parameters
        character (len=*), dimension(npar)  :: apar  ! parameter names
!! Note that parameter names must be supplied in lower case.
        
        integer                             :: ipar,i,ierr,iline,nblc,j2,j1,jfail,nnw,iunit
        character (len=10)                  :: aline
        character (len=12)                  :: tpar
        
        errsub='Error in subroutine PROCESS_TEMPLATE_FILES:'
       
        ifail=0

        if(npar.le.0)then
          write(amessage,10) trim(errsub)
10        format(a,' value supplied for NPAR is zero or negative.')
          ifail=1
          return
        end if

        IPAR=1
        DO 400 I=1,NPAR
          NW(I)=1000
400     continue
        DO 500 I=1,numinfile
          if(tempfile(i).eq.' ')then
            write(amessage,401)
401         format('Error in subroutine PROCESS_TEMPLATE_FILES: names have not ',  &
            'been installed for all template files.')
            ifail=1
            return
          end if
          if(modinfile(i).eq.' ')then
            write(amessage,402)
402         format('Error in subroutine PROCESS_TEMPLATE_FILES: names have not ',  &
            'been installed for all model input files.')
            ifail=1
            return
          end if
          call addquote(tempfile(i),afile)
          iunit=nextunit()
          open(unit=iunit,file=tempfile(i),status='old',iostat=ierr)
          if(ierr.ne.0)then
            write(amessage,410) trim(afile)
410         format('Cannot open template file ',a,'.')
            ifail=1
            return
          end if
          READ(iunit,'(A)',err=9000,END=9200) dline
          call casetrans(dline(1:3),'lo')
          IF((dline(1:3).NE.'ptf').AND.   &
             (DLINE(1:3).NE.'jtf'))GO TO 9200
          PARDEL(I)=dline(5:5)
          IF(PARDEL(I).EQ.' ') GO TO 9200
          ILINE=1
520       ILINE=ILINE+1
          READ(iunit,'(A)',err=9000,END=680) dline
          NBLC=len_trim(dline)
          J2=0
550       IF(J2.GE.NBLC) GO TO 520
          J1=INDEX(dline(J2+1:NBLC),PARDEL(I))
          IF(J1.EQ.0) GO TO 520
          J1=J1+J2
          J2=INDEX(dline(J1+1:NBLC),PARDEL(I))
          IF(J2.EQ.0)then
            call num2char(iline,aline)
            write(amessage,555) trim(aline),trim(afile)
555         format('Unbalanced parameter delimiters at line ',a,' of template file ',a,'.')
            ifail=1
            return
          end if
          J2=J2+J1
          CALL PARNAM(jfail,J1,J2,TPAR)
          if(jfail.eq.1)then
            call num2char(iline,aline)
            write(amessage,556) trim(aline),trim(afile)
556         format('Parameter space less than three characters wide at line ',a,  &
            ' of file ',a,'.')
            ifail=1
            return
          else if (jfail.eq.2)then
            call num2char(iline,aline)
            write(amessage,557) trim(aline),trim(afile)
557         format('Blank parameter space at line ',a,' of file ',a,'.')
            ifail=1
            return
          end if 
          CALL WHICH1(jfail,NPAR,IPAR,APAR,TPAR)
          if(jfail.ne.0)then
            call num2char(iline,aline)
            write(amessage,558) trim(tpar),trim(aline),trim(afile)
558         format('Parameter "',a,'" cited on line ',a,' of template file ',a,   &
            ' has not been supplied with a value.')
            ifail=1
            return
          end if
          NNW=J2-J1+1
          IF(NNW.LT.NW(IPAR)) NW(IPAR)=NNW
          GO TO 550
680       CLOSE(UNIT=iunit)
500     CONTINUE
!!!        DO 800 I=1,NPAR
!!!          IF(NW(I).EQ.1000)then
!!!            write(amessage,690) trim(apar(i))
!!!690         format('Parameter "',a,'" is not cited on any template file.')
!!!            ifail=1
!!!            return
!!!          end if
800     CONTINUE

        RETURN

9000    write(amessage,9010) trim(afile)
9010    format('Unable to read template file ',a,'.')
        ifail=1
        return
9200    write(amessage,9210) trim(afile)
9210    format('"ptf" or "jtf" header expected on first line of template file ',a,'.')
        ifail=1
        return
        
        RETURN
END subroutine process_template_files


subroutine delete_output_files(ifail)

! -- Subroutine DELETE_OUTPUT_FILES deletes model output files.

        use utility
        implicit none
        
        integer, intent(out)                :: ifail     ! indicates error condition
        logical                             :: lexist
        integer                             :: ierr,jerr,iunit,i
        integer                             :: nextunit

        ifail=0
        do i=1,numoutfile
          inquire(file=modoutfile(i),exist=lexist)
          if(lexist)then
            iunit=nextunit()
            open(unit=iunit,file=modoutfile(i),status='old',iostat=ierr)
            if(ierr.eq.0)then
              close(unit=iunit,status='delete',iostat=jerr)
              if(jerr.ne.0)then
                call addquote(modoutfile(i),afile)
                write(amessage,10) trim(afile)
10              format('Cannot delete model output file ',a,' prior to running model.')
                ifail=1
                return
              end if
            else
              call addquote(modoutfile(i),afile)
              write(amessage,10) trim(afile)
              ifail=1
              return
            end if
          end if
        end do
        
        return
        
end subroutine delete_output_files 

! Note that there seems to be a bug in lf95 in that if there is an error deleting
! the file there is a compiler error rather than ierr being given a nonzero value.
        

subroutine write_model_input_files(ifail,npar,apar,pval,pvals,itrans)

! -- Subroutine WRITE_MODEL_INPUT_FILES writes model input files based on 
!    current parameter values and a set of model input template files.

        use utility
        implicit none
        
        integer, intent(out)                             :: ifail  ! error condition indicator
        integer, intent(in)                              :: npar   ! number of parameters
        character (len=*), intent(in), dimension(npar)   :: apar   ! parameter names
        double precision, intent(inout), dimension(npar) :: pval   ! parameter values
        double precision, intent(inout), dimension(npar) :: pvals  ! parameter values (unscaled)
        integer, intent(in), dimension(npar)             :: itrans ! transformation
        
        integer                          :: ipar,ipp,jfail,ifile,iunit,iunit1,iline, &
                                            lc,j1,j2,j,ierr
        integer                          :: nsp
        integer                          :: nextunit
        double precision                 :: tval
        character (len=12)               :: tpar

        errsub='Error writing parameters to model input file(s):'
        ifail=0

! -- EACH OF THE PARAMETER WORDS IS FILLED

        IPAR=1
        DO 100 IPP=1,NPAR
          CALL WRTSIG(jfail,PVAL(IPP),PWORD(IPP),NW(IPP),PRECIS,TVAL,NOPNT)
          if(jfail.lt.0)then
            write(amessage,10) trim(apar(ipp))
10          format('Internal error condition has arisen while attempting to write ', &
            'current value of parameter "',a,'" to model output file.')
            go to 9900
          else if (jfail.eq.1)then
            write(amessage,11) trim(errsub),trim (apar(ipp))
11          format(a,' exponent of parameter "',a,'" is too large or too small for ', &
            'single precision protocol.')
            go to 9900
          else if (jfail.eq.2)then
            write(amessage,12) trim(errsub),trim (apar(ipp))
12          format(a,' exponent of parameter "',a,'" is too large or too small for ', &
            'double precision protocol.')
            go to 9900
          else if (jfail.eq.3)then
            write(amessage,13) trim(errsub),trim (apar(ipp))
13          format(a,' field width of parameter "',a,'" on at least one template file ', &
            'is too small to represent current parameter value. The number is too large ', &
            'to fit, or too small to be represented with any precision.')
            go to 9900
          end if
          PVAL(IPP)=TVAL
100     CONTINUE

! -- NEXT THE SUBSTITUTIONS IN THE TEMPLATE FILES ARE MADE

        NSP=0
        DO IPP=1,NPAR
          IF((ITRANS(IPP).GE.0).OR.                      &
             (ITRANS(IPP).LT.-100002)) NSP=NSP+1
        END DO

        DO 500 IFILE=1,numinfile
          IF(tempfile(IFILE).EQ.'par###.tpl')THEN
            iunit1=nextunit()
            open(unit=iunit1,file=modinfile(ifile),iostat=ierr)
            if(ierr.ne.0)then
              call addquote(modinfile(ifile),afile)
              write(amessage,115) trim(errsub),trim(afile)
              go to 9900
            end if
            WRITE(iunit1,1030,ERR=320) NSP,1,2
1030        FORMAT(3I10)
            DO IPP=1,NPAR
              IF((ITRANS(IPP).GE.0).OR.                        &
                 (ITRANS(IPP).LT.-100002))THEN
                WRITE(iunit1,1050,ERR=320) PVALS(IPP)
1050            FORMAT(1X,1PG14.7)
              END IF
            END DO
            WRITE(iunit1,1060,ERR=320)
1060        FORMAT('* row names')
            DO IPP=1,NPAR
              IF((ITRANS(IPP).GE.0).OR.                        &
                 (ITRANS(IPP).LT.-100002))THEN
                WRITE(iunit1,1070,ERR=320) TRIM(APAR(IPP))
1070            FORMAT(A)
              END IF
            END DO
            WRITE(iunit1,1090,ERR=320)
1090        FORMAT('* column names')
            WRITE(iunit1,1190,ERR=320)
1190        FORMAT('col1')
            CLOSE(UNIT=iunit1)
            GO TO 500
          END IF
          call addquote(tempfile(ifile),afile)
          iunit=nextunit()
          open(unit=iunit,file=tempfile(ifile),status='old',iostat=ierr)
          if(ierr.ne.0)then
            write(amessage,110) trim(errsub),trim(afile)
110         format(a,' cannot open template file ',a,'.')
            go to 9900
          end if
          iunit1=nextunit()
          open(unit=iunit1,file=modinfile(ifile),iostat=ierr)
          if(ierr.ne.0)then
            call addquote(modinfile(ifile),afile)
            write(amessage,115) trim(errsub),trim(afile)
115         format(a,' cannot open model input file ',a,' to write updated parameter ',  &
            'values prior to running model.')
            go to 9900
          end if
          READ(iunit,*,err=9000)
          ILINE=1
120       ILINE=ILINE+1
          READ(iunit,22,END=400,err=9000) dline
22        FORMAT(A)
          LC=len_trim(dline)
          J2=0
150       IF(J2.GE.LC) GO TO 300
          J1=INDEX(dline(J2+1:LC),pardel(IFILE))
          IF(J1.EQ.0) GO TO 300
          J1=J1+J2
          J2=INDEX(dline(J1+1:LC),pardel(IFILE))
          J2=J2+J1
          CALL PARNAM(jfail,J1,J2,TPAR)
          CALL WHICH1(jfail,NPAR,IPAR,APAR,TPAR)
!       The following works when space bigger than pword(:nblnk(pword))
!       dline(j1:j2)=pword(ipar)(:nblnk(pword(ipar)))
          DO 160 J=J1,J2
            dline(J:J)=' '
160       continue
          J=len_trim(PWORD(IPAR))
          dline(J2-J+1:J2)=PWORD(IPAR)(1:J)
          GO TO 150
300       WRITE(iunit1,22,ERR=320) trim(dline)
          GO TO 120
320       call addquote(modinfile(ifile),afile)
          write(amessage,321) trim(errsub),trim(afile)
321       format(a,' cannot write to model input file ',a,'.')
          go to 9900
400       CLOSE(UNIT=iunit)
          CLOSE(UNIT=iunit1,iostat=ierr)
          if(ierr.ne.0)then
            call addquote(modinfile(ifile),afile)
            write(amessage,321) trim(errsub),trim(afile)
            go to 9900
          end if
500     CONTINUE
        RETURN

9000    write(amessage,9010) trim(afile)
9010    format('Unable to read template file ',a,'.')
        go to 9900
9900    ifail=1
        return        
        
END subroutine write_model_input_files



subroutine read_model_output_files(ifail,nobs,aobs,obs,instruction)

! -- Subroutine READ_MODEL_OUTPUT_FILES reads model output files using and instruction
!    set.

!! Important note: if an error condition occurs and the INSTRUCTION variable is not blank,
!! then this variable contains the offending instruction. This should be reproduced with 
!! the error message.

        use inter
        use utility
        implicit none
        
        integer, intent(out)                            :: ifail   ! error indicator
        integer, intent(in)                             :: nobs    ! number of observations
        character (len=*), intent(in), dimension(nobs)  :: aobs    ! observation names
        double precision, intent(out), dimension(nobs)  :: obs     ! observation values
        character (len=*), intent(out)                  :: instruction ! instruction of error
        
        integer             :: ifile,il,jobs,cil,iobs,begins,ins,nblb,i,    &
                               n1,n2,insnum,nblc,dumflg,marktyp,almark,iunit, &
                               nol,jfail,insfle,mrktyp,ierr,j2,j1,n3,num1,num2,j
        double precision    :: rtemp
        character (len=1)   :: aa,mkrdel
        character (len=12)  :: obsnam
        character (len=15)  :: fmt
        character (len=10)  :: anum
        character (len=200) :: flenme

        errsub='Error reading model output file(s):'
        ifail=0

        mcall=mcall+1
        IFILE=0
        IL=0
        JOBS=0
        MKRDEL=MRKDEL(1)
        CIL=0
        IOBS=1
        BEGINS=0

        INS=1
10      IF(INS.LT.NINSTR)THEN
          NBLB=LCINS(INS+1)-LCINS(INS)
        ELSE
          NBLB=ASIZE-LCINS(INS)+1
        END IF
        instruction=' '
        DO 20 I=1,NBLB
          instruction(I:I)=A(LCINS(INS)+I-1)
20      continue
25      N2=0
        INSNUM=0

50      CALL GETINT(jfail,instruction,N1,N2,NBLB,MKRDEL)
        if(jfail.ne.0)then
          write(amessage,49) trim(errsub)
49        format(a,' missing marker delimiter in user-supplied instruction.')
          go to 9995
        end if
51      IF(N1.EQ.0) GO TO 1000
        INSNUM=INSNUM+1
        IF(INSNUM.EQ.1)THEN
          IF(instruction(N1:N1).NE.'&') THEN
            MRKTYP=0
            ALMARK=1
            BEGINS=0
          ELSE
            IF(INS.EQ.INSFLE+1) then
              write(amessage,52) trim(errsub)
52            format(a,' first instruction line in instruction file cannot start ', &
              'with continuation character.')
              go to 9995
            end if
            IF(BEGINS.EQ.1)THEN
              INS=INS-1
              GO TO 10
            END IF
          END IF
        END IF
        IF(ICHAR(instruction(N1:N1)).EQ.2)THEN
          IF(IFILE.NE.0) CLOSE(UNIT=iunit)
          DO 60 I=N1+1,NBLB
            IF(instruction(I:I).NE.' ') GO TO 70
60        CONTINUE
70        FLENME=instruction(I:NBLB)
          iunit=nextunit()
          open(unit=iunit,file=flenme,status='old',iostat=ierr)
          call addquote(flenme,afile)
          if(ierr.ne.0)then
            write(amessage,71) trim (errsub),trim(afile)
71          format(a,' cannot open model output file ',a,'.')
            instruction=' '
            go to 9995
          end if
          IFILE=IFILE+1
          CIL=0
          MKRDEL=MRKDEL(IFILE)
          INSFLE=INS
          GO TO 1000
        ELSE IF((instruction(N1:N1).EQ.'l').OR.(instruction(N1:N1).EQ.'L'))THEN
          ALMARK=0
          IL=IL+1
          IF(MCALL.EQ.1)THEN
            if(n2.le.n1) go to 9050     ! put in pest
            WRITE(FMT,150) N2-N1
150         FORMAT('(I',I4,')')
            READ(instruction(N1+1:N2),FMT,ERR=9050) NOL
            LL(IL)=NOL
          ELSE
            NOL=LL(IL)
          END IF
          IF(NOL.GT.1) THEN
            DO 160 I=1,NOL-1
              READ(iunit,*,END=9100,err=9850)
              CIL=CIL+1
160         continue
          END IF
          READ(iunit,22,END=9100,err=9850) dline
22        FORMAT(A)
          IF(INDEX(dline,CHAR(9)).NE.0) CALL TABREP()
          CIL=CIL+1
          NBLC=len_trim(dline)
          MRKTYP=1
          J1=0
        ELSE IF(instruction(N1:N1).EQ.MKRDEL)THEN
          IF(MRKTYP.EQ.0)THEN
200         READ(iunit,22,END=9100,err=9850) dline
            IF(INDEX(dline,CHAR(9)).NE.0) CALL TABREP()
            CIL=CIL+1
            J1=INDEX(dline,instruction(N1+1:N2-1))
            IF(J1.EQ.0) GO TO 200
            NBLC=len_trim(dline)
            J1=J1+N2-N1-2
            MRKTYP=1
          ELSE
            IF(J1.GE.NBLC) THEN
              IF(ALMARK.EQ.1) THEN
                BEGINS=1
                GO TO 25
              END IF
              GO TO 9200
            END IF
            J2=INDEX(dline(J1+1:NBLC),instruction(N1+1:N2-1))
            IF(J2.EQ.0) THEN
              IF(ALMARK.EQ.1) THEN
                BEGINS=1
                GO TO 25
              END IF
              GO TO 9200
            END IF
            J1=J1+J2
            J1=J1+N2-N1-2
          END IF
        ELSE IF(instruction(N1:N1).EQ.'&')THEN
          IF(INSNUM.NE.1) then
            write(amessage,201) trim(errsub)
201         format(a,' if present, continuation character must be first instruction on ', &
            'an instruction line.')
            go to 9995
          end if
        ELSE IF((instruction(N1:N1).EQ.'w').OR.(instruction(N1:N1).EQ.'W'))THEN
          ALMARK=0
          IF(J1.GE.NBLC) GO TO 9400
          J2=INDEX(dline(J1+1:NBLC),' ')
          IF(J2.EQ.0) GO TO 9400
          J1=J1+J2
          DO 210 I=J1,NBLC
            IF(dline(I:I).NE.' ') GO TO 220
210       CONTINUE
          I=NBLC+1
220       J1=I-1
        ELSE IF((instruction(N1:N1).EQ.'t').OR.(instruction(N1:N1).EQ.'T'))THEN
          ALMARK=0
          if(n2.le.n1) go to 9000       ! put in PEST
          WRITE(FMT,150) N2-N1
          READ(instruction(N1+1:N2),FMT,ERR=9000) J2
          IF(J2.LT.J1) then
            call num2char(cil,anum)
            write(amessage,221) trim(errsub),trim(anum),trim(afile)
221         format(a,' backwards move to tab position not allowed on line ',a,  &
            ' of model output file ',a,'.')
            go to 9995
          end if
          J1=J2
          IF(J1.GT.NBLC) then
            call num2char(cil,anum)
            write(amessage,222) trim(errsub),trim(anum),trim(afile)
222         format(a,' tab position beyond end of line at line ',a,' of ', &
            'model output file ',a,'.')
            go to 9995
          end if
        ELSE IF((instruction(N1:N1).EQ.'[').OR.(instruction(N1:N1).EQ.'('))THEN
          ALMARK=0
          AA=instruction(N1:N1)
          JOBS=JOBS+1
          IF(MCALL.EQ.1)THEN
            IF(AA.EQ.'[')THEN
              N3=INDEX(instruction(N1:N2),']')
            ELSE
              N3=INDEX(instruction(N1:N2),')')
            END IF
            if(n3.eq.0)then
              call num2char(cil,anum)
              write(amessage,226) trim(errsub)
226           format(a,' missing "]" or ")" character in instruction.')
              go to 9995
            end if
            N3=N3+N1-1
            OBSNAM=instruction(N1+1:N3-1)
            CALL WHICH1(jfail,NOBS,IOBS,AOBS,OBSNAM)
            IF(jfail.NE.0) GO TO 9700
            CALL GETNUM(jfail,instruction,N3,N2,NUM1,NUM2,FMT)
            IF(jfail.NE.0) then
              write(amessage,223) trim(errsub)
223           format(a,' cannot interpret user-supplied instruction for reading model ', &
              'output file.')
              go to 9995
            end if
            OBSN1(JOBS)=NUM1
            OBSN2(JOBS)=NUM2
            IIOBS(JOBS)=IOBS
          ELSE
            NUM1=OBSN1(JOBS)
            NUM2=OBSN2(JOBS)
            IOBS=IIOBS(JOBS)
          END IF
          IF(AA.EQ.'(') THEN
            CALL GETTOT(jfail,NUM1,NUM2,NBLC)
            if(jfail.ne.0)then
              call num2char(cil,anum)
              write(amessage,224) trim (errsub),trim(aobs(iobs)),trim(anum),   &
              trim(afile)
224           format(a,' cannot find observation "',a,'" on line ',a,     &
              ' of model output file ',a,'.')
              go to 9995
            end if
          ELSE
            if(num1.gt.nblc)then
              call num2char(cil,anum)
              write(amessage,224) trim(errsub),trim(aobs(iobs)),trim(anum),trim(afile)
              go to 9995
            end if
            IF(NUM2.GT.NBLC) NUM2=NBLC
            IF(dline(NUM1:NUM2).EQ.' ')then
              call num2char(cil,anum)
              write(amessage,224) trim(errsub),trim(aobs(iobs)),trim(anum),trim(afile)
              go to 9995
            end if
          end if
          WRITE(FMT,250) NUM2-NUM1+1
250       FORMAT('(F',I4,'.0)')
          READ(dline(NUM1:NUM2),FMT,ERR=260) OBS(IOBS)
          J1=NUM2
          GO TO 50
260       continue
          call num2char(cil,anum)
          write(amessage,261) trim(errsub),trim(aobs(iobs)),trim(anum),trim(afile)
261       format(a,' cannot read observation "',a,'" from line ',a,     &
          ' of model output file ',a,'.')
          go to 9995
        ELSE IF(instruction(N1:N1).EQ.'!') THEN
          ALMARK=0
          CALL casetrans(instruction(N1+1:N2-1),'lo')
          IF((N2-N1.NE.4).OR.(instruction(N1+1:N2-1).NE.'dum'))THEN
            JOBS=JOBS+1
            IF(MCALL.EQ.1) THEN
              OBSNAM=instruction(N1+1:N2-1)
              CALL WHICH1(jfail,NOBS,IOBS,AOBS,OBSNAM)
              IF(jfail.NE.0) GO TO 9700
              IIOBS(JOBS)=IOBS
            ELSE
              IOBS=IIOBS(JOBS)
            END IF
            DUMFLG=0
          ELSE
            DUMFLG=1
          END IF
          CALL GETNXT(jfail,J1,NUM1,NUM2,NBLC)
          IF(jfail.NE.0) THEN
            IF(DUMFLG.EQ.0) THEN
              call num2char(cil,anum)
              write(amessage,224) trim(errsub),trim(aobs(iobs)),trim(anum),trim(afile)
              go to 9995
            ELSE
              call num2char(cil,anum)
              write(amessage,224) trim(errsub),'dum',trim(anum),trim(afile)
              go to 9995
            END IF
          END IF
          WRITE(FMT,250) NUM2-NUM1+1
          READ(dline(NUM1:NUM2),FMT,ERR=270) RTEMP
          IF(DUMFLG.EQ.0) OBS(IOBS)=RTEMP
          J1=NUM2
          GO TO 50
270       CALL GETINT(jfail,instruction,N1,N2,NBLB,MKRDEL)
          IF(jfail.NE.0) then
            write(amessage,271) trim(errsub)
271         format(a,' missing marker delimiter in user-supplied instruction set.')
            go to 9995
          end if  
          IF(N1.EQ.0)THEN
            IF(DUMFLG.EQ.1) GO TO 9900
            GO TO 9800
          END IF
          IF(instruction(N1:N1).NE.MKRDEL) THEN
            IF(DUMFLG.EQ.1) GO TO 9900
            GO TO 9800
          END IF
          J2=INDEX(dline(J1+1:NBLC),instruction(N1+1:N2-1))
          IF(J2.EQ.0) THEN
            IF(DUMFLG.EQ.1) GO TO 9900
            GO TO 9800
          END IF
          NUM2=J1+J2-1
          IF(NUM2.LT.NUM1)THEN
            IF(DUMFLG.EQ.1) GO TO 9900
            GO TO 9800
          END IF
          WRITE(FMT,250) NUM2-NUM1+1
          IF(DUMFLG.EQ.1)THEN
            READ(dline(NUM1:NUM2),FMT,ERR=9900) RTEMP
          ELSE
            READ(dline(NUM1:NUM2),FMT,ERR=9800) OBS(IOBS)
          END IF
          J1=NUM2
          GO TO 51
        ELSE
          write(amessage,272) trim(errsub)
272       format(a,' cannot interpret user-supplied instruction for reading model ',  &
          'output file.')
          go to 9995
        END IF
        GO TO 50
1000    INS=INS+1
        IF(INS.LE.NINSTR) GO TO 10

        IF(MCALL.EQ.1)THEN
          DO 1100 I=1,NOBS
          DO 1050 J=1,JOBS
          IF(IIOBS(J).EQ.I) GO TO 1100
1050      CONTINUE
          write(amessage,1051) trim(errsub),trim(aobs(i))
1051      format(a,' observation "',a,'" not referenced in the user-supplied instruction set.')
          instruction=' '
          go to 9995
1100      CONTINUE
        END IF

        CLOSE(UNIT=iunit)

        RETURN

9000    write(amessage,9010) trim(errsub)
9010    format(a,' cannot read tab position from user-supplied instruction.')
        go to 9995
9050    write(amessage,9060) trim(errsub)
9060    format(a,' cannot read line advance item from user-supplied instruction.')
        go to 9995
9100    write(amessage,9110) trim(errsub),trim(afile)
9110    format(a,' unexpected end to model output file ',a,'.')
        go to 9995
9200    call num2char(cil,anum)
        write(amessage,9210) trim(errsub),trim(anum),trim(afile)
9210    format(a,' unable to find secondary marker on line ',a,   &
        ' of model output file ',a,'.')
        go to 9995
9400    call num2char(cil,anum)
        write(amessage,9410) trim(errsub),trim(anum),trim(afile)
9410    format(a,' unable to find requested whitespace, or whitespace ',  &
        'precedes end of line at line ',a,' of model output file ',a,'.')
        go to 9995
9700    write(amessage,9710) trim(errsub),trim(obsnam)
9710    format(a,' observation name "',a,'" from user-supplied instruction set ',&
        'is not cited in main program input file.')
        go to 9995
9800    call num2char(cil,anum)
        write(amessage,9810) trim(errsub),trim(aobs(iobs)),trim(anum),trim(afile)
9810    format(a,' cannot read observation "',a,'" from line ',a,   &
        ' of model output file ',a,'.')
!        instruction=' '
        go to 9995
9850    write(amessage,9860) trim(afile)
9860    format('Unable to read model output file ',a,'.')
        instruction=' '
        go to 9995 
9900    call num2char(cil,anum)
        write(amessage,9810) trim(errsub),'dum',trim(anum),trim(afile)
!        instruction=' '
        go to 9995

9995    ifail=1
        return

END subroutine read_model_output_files


SUBROUTINE GETNXT(IFAIL,J1,NUM1,NUM2,NBLC)

! -- SUBROUTINE GETNXT GETS THE NEXT SPACE-DELIMITED WORD

        implicit none        

        INTEGER IFAIL
        INTEGER J1,NUM1,NUM2,NBLC,I

        IFAIL=0
        DO 20 I=J1+1,NBLC
        IF(dline(I:I).NE.' ') GO TO 50
20      CONTINUE
        IFAIL=1
        RETURN
50      NUM1=I
        I=INDEX(dline(NUM1:NBLC),' ')
        IF(I.EQ.0) THEN
          NUM2=NBLC
        ELSE
          NUM2=NUM1+I-2
        END IF

        RETURN

END subroutine getnxt


SUBROUTINE GETTOT(IFAIL,J1,J2,NBLC)

! -- SUBROUTINE GETTOT DETERMINES THE EXACT POSITION OCCUPIED BY A NUMBER

        implicit none
        INTEGER IFAIL
        INTEGER J1,J2,NBLC,I
 
        IFAIL=0
        IF(J1.GT.NBLC)THEN
          IFAIL=1
          RETURN
        END IF
        IF(J2.GT.NBLC)J2=NBLC
        IF(dline(J2:J2).EQ.' ') THEN
          DO 10 I=J2,J1,-1
          IF(dline(I:I).NE.' ')THEN
            J2=I
            GO TO 100
          END IF
10        CONTINUE
          IFAIL=1
          RETURN
        ELSE
          IF(J2.EQ.NBLC) GO TO 100
          DO 20 I=J2,NBLC
          IF(dline(I:I).EQ.' ') THEN
            J2=I-1
            GO TO 100
          END IF
20        CONTINUE
          J2=NBLC
        END IF
100     IF(J1.EQ.1) GO TO 200
        DO 120 I=J1,1,-1
        IF(dline(I:I).EQ.' ') THEN
          J1=I+1
          GO TO 200
        END IF
120     CONTINUE
        J1=1
200     RETURN

END subroutine gettot


SUBROUTINE GETINT(IFAIL,BUF,N1,N2,NBLB,MRKDEL)

! -- SUBROUTINE GETINT GETS THE NEXT STORED INSTRUCTION FOR PROCESSING

        INTEGER N1,N2,NBLB,I,II
        INTEGER IFAIL
        CHARACTER MRKDEL
        CHARACTER*(*) BUF

        IFAIL=0
        IF(N2.GE.NBLB) THEN
          N1=0
          RETURN
        END IF
        DO 10 I=N2+1,NBLB
        IF((BUF(I:I).NE.' ').AND.(ICHAR(BUF(I:I)).NE.9)) GO TO 50
10      CONTINUE
        N1=0
        RETURN
50      N1=I
        IF(BUF(N1:N1).NE.MRKDEL)THEN
          I=INDEX(BUF(N1:NBLB),' ')
          II=INDEX(BUF(N1:NBLB),CHAR(9))
          IF((I.EQ.0).AND.(II.EQ.0))THEN
            I=0
          ELSE IF(I.EQ.0)THEN
            I=II
          ELSE IF(II.EQ.0) THEN
            I=I
          ELSE
            I=MIN(I,II)
          END IF
          IF(I.NE.0) THEN
            N2=N1+I-2
          ELSE
            N2=NBLB
          END IF
        ELSE
          IF(N1.EQ.NBLB)THEN
            IFAIL=1
            RETURN
          END IF
          I=INDEX(BUF(N1+1:NBLB),MRKDEL)
          IF(I.EQ.0) THEN
            IFAIL=1
            RETURN
          END IF
          N2=N1+I
        END IF

        RETURN
 END subroutine getint


SUBROUTINE TABREP()

! -- SUBROUTINE TABREP REPLACES A TAB BY BLANK SPACE(S)

        INTEGER LLEN,I,J,K,NBLC

        LLEN=LEN(dline)
        DO 10 I=LLEN,1,-1
        IF(dline(I:I).NE.' ') GO TO 20
10      CONTINUE
        RETURN
20      NBLC=I

        I=0
30      I=I+1
        IF(I.GT.NBLC)RETURN
        IF(ICHAR(dline(I:I)).NE.9) GO TO 30
        J=((I-1)/8+1)*8-I
        IF(J.EQ.0) THEN
          dline(I:I)=' '
        ELSE
          dline(I:I)=' '
          NBLC=NBLC+J
          IF(NBLC.GT.LLEN) NBLC=LLEN
          DO 50 K=NBLC,((I-1)/8+1)*8,-1
          dline(K:K)=dline(K-J:K-J)
50        CONTINUE
          DO 60 K=I+1,MIN(NBLC,I+J)
          dline(K:K)=' '
60        CONTINUE
          I=I+J
        END IF
        GO TO 30

END subroutine tabrep



SUBROUTINE PARNAM(IFAIL,J1,J2,TPAR)

! -- SUBROUTINE PARNAM EXTRACTS A PARAMETER NAME FROM A STRING

        implicit none
        integer, intent(out)          :: ifail   ! reports error condition
        integer, intent(in)           :: j1,j2   ! beginning and end of word
        character (len=*), intent(out):: tpar    ! the extracted parameter
        
        integer             :: i,j

        IFAIL=0
        TPAR=' '
        IF(J2-J1.LE.1) THEN
          IFAIL=1
          RETURN
        END IF
        DO 10 I=J1+1,J2-1
        IF(dline(I:I).EQ.' ') GO TO 10
        GO TO 30
10      CONTINUE
        IFAIL=2
        RETURN
30      J=MIN(12,J2-I)
        TPAR(1:J)=dline(I:I+J-1)
        RETURN
END subroutine parnam

       

subroutine finalise_mio()
      
! -- Subroutine MIO_FINALISE de-allocates memory usage by the model_input_output
!    module.

      use utility
      implicit none      
      
      integer                             :: ierr
      
      if(allocated(tempfile))deallocate(tempfile,stat=ierr)
      if(allocated(modinfile))deallocate(modinfile,stat=ierr)
      if(allocated(pardel))deallocate(pardel,stat=ierr)
      if(allocated(insfile))deallocate(insfile,stat=ierr)
      if(allocated(modoutfile))deallocate(modoutfile,stat=ierr)
      if(allocated(mrkdel))deallocate(mrkdel,stat=ierr)
      if(allocated(nw))deallocate(nw,stat=ierr)
      if(allocated(pword))deallocate(pword,stat=ierr)
      if(allocated(obsn1))deallocate(obsn1,stat=ierr)
      if(allocated(obsn2))deallocate(obsn2,stat=ierr)
      if(allocated(iiobs))deallocate(iiobs,stat=ierr)
      if(allocated(a))deallocate(a,stat=ierr)
      if(allocated(ll))deallocate(ll,stat=ierr)
      if(allocated(lcins))deallocate(lcins,stat=ierr)

      return      

end subroutine finalise_mio



SUBROUTINE CMPRSS()

! -- SUBROUTINE CMPRSS COMPRESSES AN INSTRUCTION LINE BY REMOVING EXCESS
! -- BLANK CHARACTERS

        implicit none
        
        INTEGER NBLC,J

        IF(dline.EQ.' ') RETURN
10      NBLC=len_trim(dline)
        J=INDEX(dline(1:NBLC),'  ')
        IF(J.NE.0) THEN
          dline(j+1:)=adjustl(dline(j+1:))
!          CALL SHIFTL(dline(J+1:NBLC))
          GO TO 10
        END IF
        RETURN

END subroutine cmprss


SUBROUTINE GETNUM(IFAIL,BUF,N3,N2,NUM1,NUM2,FMT)

! -- SUBROUTINE GETNUM RETRIEVES CHARACTER POSITIONS FROM FIXED AND
! -- SEMI-FIXED OBSERVATION INSTRUCTIONS

        INTEGER N3,NUM1,NUM2,I,N2
        INTEGER IFAIL
        CHARACTER*(*) BUF
        CHARACTER*(*) FMT

        IFAIL=0
        I=INDEX(BUF(N3+1:N2),':')
        IF(I.EQ.0) GO TO 100
        WRITE(FMT,20) I-1
20      FORMAT('(I',I3,')')
        READ(BUF(N3+1:N3+I-1),FMT,ERR=100) NUM1
        N3=N3+I
        I=N2-N3
        IF(I.LT.1) GO TO 100
        WRITE(FMT,20) I
        READ(BUF(N3+1:N2),FMT,ERR=100) NUM2
        RETURN
100     IFAIL=1
        RETURN

END subroutine getnum


end module model_input_output 
