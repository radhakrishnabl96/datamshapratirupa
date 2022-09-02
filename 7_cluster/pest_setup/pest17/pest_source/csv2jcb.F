       program csv2jcb

! -- CSV2JCB writes a JCB file based on the contents of a CSV file.

       implicit none

       logical      :: lexist
       integer      :: ifail,i,j,k,ierr
       integer      :: nsize,last13,icount,icountlast
       integer      :: ipar,npar,nrow,ncol,irow,icol,jcol
       integer      :: icom
       integer      :: nreal,ireal,narg,nb

       character*1    :: aa,arc
       character*10   :: aext
       character*12   :: aversion
       character*20   :: anum,atype
       character*50   :: aname,aword
       character*256  :: csvfile,outfile,acsvfile,aoutfile
       character (len=1500) :: amessage

       double precision, allocatable    :: pval(:)
       character, allocatable           :: astring(:)
       character (len=200), allocatable :: apar(:),areal(:)

! -- We write the header.

       include 'version.inc'
       write(6,50) trim(aversion)
50     format(/,' CSV2JCB Version ',a,'. Watermark Numerical Computing.',/)

! -- The command line is read.

       narg=iargc()
       if(narg.ne.2) go to 8900
       call getarg(1,csvfile)
       call getarg(2,outfile)

! -- Information is acquired from the user.

       call addquote(csvfile,acsvfile)
       nb=len_trim(csvfile)
       i=nb-3
       if(i.lt.1)i=1
       aext=csvfile(i:nb)
       call lowcase(aext)
       if(aext.ne.'.csv')then
         write(amessage,15) trim(acsvfile)
15       format(' File ',a,' does not have an extension of ".csv".')
         go to 9890
       end if
       inquire(file=csvfile,exist=lexist)
       if(.not.lexist) then
         write(amessage,30) trim(acsvfile)
30       format('File ',a,' does not exist.')
         go to 9890
       end if
       inquire(file=csvfile,size=nsize,iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,115) trim(acsvfile)
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
         write(amessage,125) trim(acsvfile)
125      format('Cannot open file ',a,'.')
         go to 9890
       end if

       call addquote(outfile,aoutfile)
       nb=len_trim(outfile)
       i=nb-3
       if(i.lt.1)i=1
       aext=outfile(i:nb)
       call lowcase(aext)
       if(aext.ne.'.jcb')then
         write(amessage,16) trim(aoutfile)
16       format(' File ',a,' does not have an extension of ".jcb".')
         go to 9890
       end if
       open(unit=20,file=outfile,action='write',access='stream',err=9400)

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
               write(amessage,450) trim(acsvfile),trim(anum)
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

! -- We now acquire parameter names and values, assuming that columns pertain to parameters. But in the end
!    these can be observations too.

       npar=ncol-1
       allocate(apar(npar),pval(npar),stat=ierr)
       if(ierr.ne.0) go to 9200
       apar=' '         ! an array
       nreal=nrow-1
       allocate(areal(nreal),stat=ierr)
       areal=' '        ! an array

! -- We write the header to the JCB file.

       write(20) npar,nreal
       write(20) npar*nreal

! -- First we read the first row to obtain parameter names.
       ipar=-1
       icom=0
220    continue
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
470    continue

! -- We now read and re-write the rows.
       ireal=0
       do irow=2,nrow
         ireal=ireal+1
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
         areal(ireal)=adjustl(aword)
         aname=areal(ireal)
! -- Now we extract the row of numbers.
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
         do ipar=1,npar
           write(20) ireal-1,ipar-1,pval(ipar)
         end do
       end do

! -- Now record names.

       do ipar=1,npar
         write(20) apar(ipar)
       end do
       do ireal=1,nreal
         write(20) areal(ireal)
       end do

! -- Tidy up

       close(unit=20,iostat=ierr)
       write(6,410) trim(csvfile)
410    format(' - file ',a,' read ok.')
       write(6,420) trim(outfile)
420    format(' - file ',a,' written ok.')
       go to 9900

8900   continue
       write(6,8910)
8910   format(/,' CSV2JCB is run using the command:',/)
       write(6,8920)
8920   format('     csv2jcb csvfile jcbfile',/)
       write(6,8930)
8930   format(' where',/)
       write(6,8940)
8940   format('     csvfile is the name of an existing CSV file, and')
       write(6,8950)
8950   format('     jcbfile is the name for a new JCB file.')
       go to 9900

9000   call writint(anum,ipar+1)
       write(amessage,9010) trim(anum),trim(csvfile)
9010   format('Column number ',a,' has no name in first row of file ',a,'.')
       go to 9890

9050   call writint(anum,irow)
       write(amessage,9060) trim(anum),trim(csvfile)
9060   format('Row number ',a,' has no name in file ',a,'.')
       go to 9890

9100   write(amessage,9110) trim(apar(ipar)),trim(aname),trim(acsvfile)
9110   format('Value for column "',a,'" in row with label "',a,'" of file ',a,' is missing or illegal.')
       go to 9890

9200   write(amessage,9210)
9210   format('Cannot allocate sufficient memory to continue execution.')
       go to 9890

9250   write(amessage,9260) trim(apar(ipar)),trim(aname),trim(acsvfile)
9260   format('Cannot read value for parameter "',a,'" from column "',a,'" of CSV file ',a,'.')
       go to 9890

9300   write(amessage,9310) trim(acsvfile)
9310   format('Error encountered in reading CSV file ',a,'.')
       go to 9890

9400   write(amessage,9410) trim(aoutfile)
9410   format('Cannot write to file ',a,'.')
       go to 9890

9890   continue
       amessage=' '//trim(amessage)
       call writmess(6,amessage)

9900   continue
       deallocate(astring,stat=ierr)
       deallocate(pval,apar,areal,stat=ierr)

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



        subroutine addquote(afile,aqfile)

! -- Subroutine ADDQUOTE adds quotes to a filename if it has a space in it.

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

