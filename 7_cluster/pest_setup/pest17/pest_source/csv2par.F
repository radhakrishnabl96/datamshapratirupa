       program csv2par

! -- CSV2PAR writes a parameter value file based on the contents of a CSV file.

       implicit none

       logical      :: lexist
       integer      :: ibeg,iend,ifail,i,j,k,ierr
       integer      :: nsize,last13,icount,icountlast
       integer      :: ipar,npar,nrow,ncol,irow,icol,jcol
       integer      :: icom

       character*1    :: aa,arc
       character*12   :: aversion
       character*20   :: anum,atype
       character*50   :: aname,aword
       character*256  :: csvfile,outfile
       character*256  :: cline
       character (len=1500) :: amessage

       double precision, allocatable   :: pval(:)
       character, allocatable          :: astring(:)
       character (len=20), allocatable :: apar(:)

! -- We write the header.

       include 'version.inc'
       write(6,50) trim(aversion)
50     format(/,' CSV2PAR Version ',a,'. Watermark Numerical Computing.',/)

! -- Information is acquired from the user.

!       open(unit=30,file='debug.dat')   !debug

100    write(6,110,advance='no')
110    format(' Enter name of CSV file: ')
       read(5,'(a)') cline
       if(cline.eq.' ') go to 100
       ibeg=1
       iend=len(cline)
       call getfile(ifail,cline,csvfile,ibeg,iend)
       inquire(file=csvfile,exist=lexist)
       if(.not.lexist) go to 100
       inquire(file=csvfile,size=nsize,iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,115) trim(csvfile)
115      format('Error encountered while inquiring into size of file ',a,'.')
         go to 9890
       end if
       allocate(astring(nsize+10),stat=ierr)
       if(ierr.ne.0) then
         write(amessage,120)
120      format('The contents of the CSV file are too big to fit into memory.')
         go to 9890
       end if
       astring=' '   ! an array
       open(unit=10,file=csvfile,status='old',access='stream',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,125) trim(csvfile)
125      format('Cannot open file ',a,'.')
         go to 9890
       end if

129    write(6,130,advance='no')
130    format(' Extract param vals from a row or column of this file? [r/c]: ')
       read(5,'(a)') arc
       if((arc.eq.'R').or.(arc.eq.'r'))then
         arc='r'
         atype='row'
       else if ((arc.eq.'C').or.(arc.eq.'c'))then
         arc='c'
         atype='column'
       else
         go to 129
       end if

139    write(6,140,advance='no') trim(atype)
140    format(' Enter name of ',a,': ')
       read(5,'(a)') cline
       if(cline.eq.' ') go to 139
       ibeg=1
       iend=len(cline)
       call getfile(ifail,cline,aname,ibeg,iend)
       if(ifail.ne.0) go to 139
       call lowcase(aname)

       write(6,*)
200    write(6,210,advance='no')
210    format(' Enter name for parameter value file: ')
       read(5,'(a)') cline
       if(cline.eq.' ') go to 200
       ibeg=1
       iend=len(cline)
       call getfile(ifail,cline,outfile,ibeg,iend)
       open(unit=20,file=outfile,action='write',err=200)

! -- Read the entire csv file.

       read(10,err=9300,end=215) astring       ! an array
215    continue
400    close(unit=10)

       nsize=size(astring)

! -- First get rid of carriage returns.

       do i=1,nsize
         if(astring(i).eq.char(10)) astring(i)=' '
       end do

! -- Now we look at the situation at the end of the file. We want the last row to end in CR
!    to make further processing easy. Also we don't want blank rows at the end of the file.

       do i=nsize,1,-1
         if(astring(i).ne.' ')then
           if(ichar(astring(i+1)).ne.13)astring(i+1)=char(13)
           go to 430
         end if
       end do
430    continue
       do i=nsize,1,-1
         if(astring(i).eq.char(13))then
           last13=i
         else
           if(astring(i).ne.' ') go to 440
         end if
       end do
440    continue
       do i=last13+1,nsize
         astring(i)=' '
       end do

! -- We now work out how many rows in the file.

       nrow=count(astring.eq.char(13))

! -- We now count how many columns are in each row, and make sure that this is the same from row to row.

       icount=0
       icountlast=-9999
       irow=1
       do i=1,nsize
         if(astring(i).eq.',')then
           icount=icount+1
         else if(astring(i).eq.char(13))then
           icount=icount+1
           if(icountlast.ne.-9999)then
             if(icount.ne.icountlast)then
               call writint(anum,irow)
               write(amessage,450) trim(csvfile),trim(anum)
450            format(' Rows in file ',a,' do not have the same number of columns. ',  &
               'First discrepancy is at row number ',a,'.')
               go to 9890
             end if
           end if
           icountlast=icount
           icount=0
           irow=irow+1
         end if
       end do
       ncol=icountlast

!       write(6,*) ' nrow = ',nrow    !debug
!       write(6,*) ' ncol = ',ncol    !debug

! -- We now acquire parameter names and values.

       if(arc.eq.'r')then
         npar=ncol-1
       else
         npar=nrow-1
       end if
       allocate(apar(npar),pval(npar),stat=ierr)
       if(ierr.ne.0) go to 9200
       apar=' '         ! an array

       if(arc.eq.'r')then
! -- First we read the first row to obtain parameter names.
         ipar=-1
         icom=0
220      continue
         do i=icom+1,nsize
           if((astring(i).eq.',').or.(astring(i).eq.char(13)))then
             ipar=ipar+1
             if(ipar.gt.0)then
               if(i-icom.eq.1) go to 9000
               k=0
               do j=icom+1,i-1
                 k=k+1
                 apar(ipar)(k:k)=astring(j)
               end do
               apar(ipar)=adjustl(apar(ipar))
               if(apar(ipar).eq.' ') go to 9000
               call tidyup(apar(ipar))
             end if
             icom=i
             if(ipar.eq.npar) go to 470
             go to 220
           end if
         end do

470      continue
! -- We now search for the desired row.
         do irow=2,nrow
           do i=icom,nsize
             if(astring(i).eq.char(13)) exit
           end do
           icom=i
           do i=icom+1,nsize
             if(astring(i).eq.',') exit
           end do
           if(i.eq.icom+1) go to 9050
           aword=' '
           k=0
           do j=icom+1,i-1
             k=k+1
             aword(k:k)=astring(j)
           end do
           if(aword.eq.' ') go to 9050
           icom=i
           call tidyup(aword)
           if(aword.eq.aname) go to 472
         end do
         write(amessage,460) trim(aname),trim(csvfile)
460      format('A row named "',a,'" was not found in file ',a,'.')
         go to 9890
! -- Now we extract the row of numbers.
472      continue
         ipar=0
451      continue
         do i=icom+1,nsize
           if((astring(i).eq.',').or.(astring(i).eq.char(13)))then
             ipar=ipar+1
             if(i-icom.eq.1) go to 9100
             aword=' '
             k=0
             do j=icom+1,i-1
               k=k+1
               aword(k:k)=astring(j)
             end do
             if(aword.eq.' ') go to 9100
             call drealread(ifail,aword,pval(ipar))
             if(ifail.ne.0) go to 9100
             icom=i
             if(ipar.eq.npar) go to 505
             go to 451
           endif
         end do
505      continue

       else if (arc.eq.'c')then
! -- We read the first row looking for the name of the column. This identifies the column number.
         icol=0
         icom=0
465      continue
         do i=icom+1,nsize
           if((astring(i).eq.',').or.(astring(i).eq.char(13)))then
             icol=icol+1
             if(icol.eq.1)then
               icom=i
               go to 465
             else
               if(i-icom.eq.1) go to 9150
               k=0
               aword=' '
               do j=icom+1,i-1
                 k=k+1
                 aword(k:k)=astring(j)
               end do
               if(aword.eq.' ') go to 9150
               call tidyup(aword)
               icom=i
               if(aword.eq.aname) go to 500
               if(icol.eq.ncol) then
                 write(amessage,480) trim(aname),trim(csvfile)
480              format('A column named "',a,'" was not found in file ',a,'.')
                 go to 9890
               end if
               go to 465
             end if
           endif
         end do
500      continue
! -- Now we extract a parameter name and value from each row.
         do irow=2,nrow
           do i=icom,nsize
             if(astring(i).eq.char(13)) exit
           end do
           icom=i
           do i=icom+1,nsize
             if(astring(i).eq.',') exit
           end do
           if(i.eq.icom+1) go to 9050
           ipar=irow-1
           apar(ipar)=' '
           k=0
           do j=icom+1,i-1
             k=k+1
             apar(ipar)(k:k)=astring(j)
           end do
           if(apar(ipar).eq.' ') go to 9050
           call tidyup(apar(ipar))
! -- Now we look for the value of the parameter.
           do jcol=2,icol
             icom=i
             do i=icom+1,nsize
               if((astring(i).eq.',').or.(astring(i).eq.char(13)))exit
             end do
           end do
           if(i.eq.icom+1) go to 9250
           aword=' '
           k=0
           do j=icom+1,i-1
             k=k+1
             aword(k:k)=astring(j)
           end do
           if(aword.eq.' ') go to 9250
           call drealread(ifail,aword,pval(ipar))
           if(ifail.ne.0) go to 9250
           icom=i
         end do
       end if

! -- The parameter value file is now written.

       write(20,550)
550    format('single point')
       do ipar=1,npar
         write(20,560) trim(apar(ipar)),pval(ipar)
560      format(a,t25,1pg16.9,t50,'1.0   0.0')
       end do
       close(unit=20)
       write(6,*)
       write(6,410) trim(csvfile)
410    format(' - file ',a,' read ok.')
       write(6,420) trim(outfile)
420    format(' - file ',a,' written ok.')
       go to 9900

9000   call writint(anum,ipar)
       write(amessage,9010) trim(anum),trim(csvfile)
9010   format('Parameter number ',a,' has no name in first row of file ',a,'.')
       go to 9890

9050   call writint(anum,irow)
       write(amessage,9060) trim(anum),trim(csvfile)
9060   format('Row number ',a,' has no name in file ',a,'.')
       go to 9890

9100   write(amessage,9110) trim(apar(ipar)),trim(aname),trim(csvfile)
9110   format('Value for parameter "',a,'" in row "',a,'" of file ',a,' is missing or illegal.')
       go to 9890

9150   call writint(anum,icol)
       write(amessage,9160) trim(anum),trim(csvfile)
9160   format('Column ',a,' does not have a name in the first row of file ',a,'.')
       go to 9890

9200   write(amessage,9210)
9210   format('Cannot allocate sufficient memory to continue execution.')
       go to 9890

9250   write(amessage,9260) trim(apar(ipar)),trim(aname),trim(csvfile)
9260   format('Cannot read value for parameter "',a,'" from column "',a,'" of CSV file ',a,'.')
       go to 9890

9300   write(amessage,9310) trim(csvfile)
9310   format('Error encountered in reading CSV file ',a,'.')
       go to 9890

9890   continue
       amessage=' '//trim(amessage)
       call writmess(6,amessage)

9900   continue
       deallocate(astring,stat=ierr)
       deallocate(pval,apar,stat=ierr)

       end


       subroutine tidyup(aword)

       implicit none

       integer        :: n
       character*(*)  :: aword

       call lowcase(aword)
       aword=adjustl(aword)
       if((aword(1:1).eq.'''').or.(aword(1:1).eq.'"'))then
         aword=aword(2:)
         aword=adjustl(aword)
       end if
       n=len_trim(aword)
       if((aword(n:n).eq.'''').or.(aword(n:n).eq.'"')) aword(n:n)=' '
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
        write(junit,*)
        do i=1,min(20,len(amessage))
          if(amessage(i:i).ne.' ')go to 21
20      end do
21      leadblank=i-1
        nblc=len_trim(amessage)
5       jend=j+78-itake
        if(jend.ge.nblc) go to 100
        do i=jend,j+1,-1
        if(amessage(i:i).eq.' ') then
          if(itake.eq.0) then
             write(junit,'(a)') amessage(j+1:i)
             itake=2+leadblank
          else
             write(junit,'(a)') ablank(1:leadblank+2)//amessage(j+1:i)
          end if
          j=i
          go to 5
        end if
        end do
        if(itake.eq.0)then
          write(junit,'(a)') amessage(j+1:jend)
          itake=2+leadblank
        else
          write(junit,'(a)') ablank(1:leadblank+2)//amessage(j+1:jend)
        end if
        j=jend
        go to 5
100     jend=nblc
        if(itake.eq.0)then
          write(junit,'(a)') amessage(j+1:jend)
        else
          write(junit,'(a)') ablank(1:leadblank+2)//amessage(j+1:jend)
        end if
        return

      end



subroutine getfile(ifail,cline,filename,ibeg,iend)

! Subroutine getfile extracts a filename from a string.

! -- Arguments are as follows:-
!       ifail: returned as zero if filename successfully read
!       cline: a character string containing the file name
!       filename: the name of the file read from the string
!       ibeg: character position at which to begin search for filename
!       iend: on input  - character position at which to end search for filename
!             on output - character postion at which filename ends


        integer, intent(out)               :: ifail
        integer, intent(in)                :: ibeg
        integer, intent(inout)             :: iend
        character (len=*), intent(in)      :: cline
        character (len=*), intent(out)     :: filename

        integer                            :: i,j,k
        character (len=1)                  :: aa

        ifail=0
        do i=ibeg,iend
          aa=cline(i:i)
          if((aa.ne.' ').and.(aa.ne.',').and.(aa.ne.char(9)))go to 50
        end do
        ifail=1
        return

50      if((aa.eq.'"').or.(aa.eq.''''))then
!          do j=i+1,iend
          do j=i+1,len_trim(cline)              !note
            if(cline(j:j).eq.aa) go to 60
          end do
          ifail=1
          return
60        iend=j
          if(i+1.gt.j-1)then
            ifail=1
            return
          else
            filename=cline(i+1:j-1)
          end if
        else
          do j=i+1,iend
            if((cline(j:j).eq.' ').or.(cline(j:j).eq.',').or.(cline(j:j).eq.char(9)))then
              k=j-1
              go to 100
            end if
          end do
          k=iend
100       filename=cline(i:k)
          if(cline(k:k).eq.'"')then
            ifail=1
            return
          else if(cline(k:k).eq.'''')then
            ifail=1
            return
          end if

          iend=k
        end if
        filename=adjustl(filename)
        return

end subroutine getfile




        subroutine writint(atemp,ival)

!       Subroutine WRITINT writes an integer to a character variable.

        integer*4 ival
        character*6 afmt
        character*(*) atemp

        afmt='(i   )'
        write(afmt(3:5),'(i3)') len(atemp)
        write(atemp,afmt)ival
        atemp=adjustl(atemp)
        return
        end


        SUBROUTINE LOWCASE(ASTRNG)

! -- Subroutine LOWCASE converts a string to lower case.

        INTEGER I,J
        CHARACTER*(*) ASTRNG

        DO 10 I=1,len_trim(ASTRNG)
        J=ICHAR(ASTRNG(I:I))
        IF((J.GE.65).AND.(J.LE.90)) ASTRNG(I:I)=CHAR(J+32)
10      CONTINUE
        RETURN
        END


        SUBROUTINE DREALREAD(IFAIL,CLINE,RTEMP)

! -- Subroutine DREALREAD reads a real number from a string.

        INTEGER IFAIL
        double precision RTEMP
        CHARACTER*8 AFMT
        CHARACTER*(*) CLINE

        IFAIL=0
        AFMT='(F   .0)'
        WRITE(AFMT(3:5),'(I3)') len_trim(CLINE)
        READ(CLINE,AFMT,ERR=100) RTEMP
        RETURN

100     IFAIL=1
        RETURN
        END
