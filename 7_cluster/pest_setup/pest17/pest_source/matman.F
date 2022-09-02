       module matrix_manipulate

       implicit none
       private
       public     dmatrix
       public     mat_read,mat_write,mat_diag_expand,mat_icode_expand,
     +            mat_deallocate,mat_diag_compress,mat_icode_compress

       type dmatrix
         integer                         :: nrow,ncol,icode
         double precision, pointer       :: array(:,:)
         double precision, pointer       :: vector(:)
         character*20, pointer           :: arow(:)
         character*20, pointer           :: acol(:)
       end type dmatrix

       contains


       subroutine mat_read(ifail,iunit,mat,matfile,amessage,cline)

       implicit none

       integer               :: ifail       ! return as zero if an error
       integer               :: iunit       ! file to read matrix from
       character*(*)         :: matfile     ! the matrix file
       character*(*)         :: amessage    ! string to write error message to
       character*(*)         :: cline       ! a character work string
       type (dmatrix)        :: mat         ! the matrix to be read

       integer        :: ierr,ncol,nrow,icode,irow,icol
       integer        :: lw(3),rw(3)
       character*6 aarow
       character*200  :: afile

! -- Initialisation

       ifail=0
       call addquote(matfile,afile)

! -- The matrix file is opened.

       open(unit=iunit,file=matfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,10) trim(afile)
10       format(' Cannot open matrix file ',a,'.')
         go to 9800
       end if

! -- The header line is read.

       read(iunit,'(a)',err=9000,end=9000) cline
       call linspl(ifail,3,lw,rw,cline)
       if(ifail.ne.0)then
         write(amessage,40) trim(afile)
40       format(' Three integers are expected on first line of file ',
     +   a,'.')
         go to 9800
       end if
       call intread(ifail,cline(lw(1):rw(1)),nrow)
       if(ifail.ne.0) go to 9000
       call intread(ifail,cline(lw(2):rw(2)),ncol)
       if(ifail.ne.0) go to 9000
       call intread(ifail,cline(lw(3):rw(3)),icode)
       if(ifail.ne.0) go to 9000
       if((ncol.le.0).or.(nrow.le.0))then
         write(amessage,50) trim(afile)
50       format(' NCOL or NROW is less than or equal to zero at ',
     +   'first line of file ',a,'.')
         go to 9800
       end if
       if(abs(icode).eq.1)then
         if(ncol.ne.nrow)then
           write(amessage,60) trim(afile)
60         format(' ICODE cannot be "1" or "-1" on first line of ',
     +     'file ',a,' unless NCOL equals NROW.')
           go to 9800
         end if
       end if
       if((icode.ne.-1).and.(icode.ne.1).and.(icode.ne.2))then
         write(amessage,70) trim(afile)
70       format(' ICODE must be "1", "2" or "-1" on first line of ',
     +   'file ',a,'.')
         go to 9800
       end if
       if(icode.eq.-1)then
         if(ncol.ne.nrow)then
           write(amessage,80) trim(afile)
80         format(' According to the integer header line, the matrix ',
     +     'contained in file ',a,' is not square. Hence ICODE ',
     +     'must not be "-1".')
           go to 9800
         end if
       end if

! -- Arrays in the matrix structure are dimensioned.

       mat%nrow=nrow
       mat%ncol=ncol
       mat%icode=icode
       if(icode.ne.-1)then
         allocate(mat%array(nrow,ncol),stat=ierr)
       else
         allocate(mat%vector(nrow),stat=ierr)
       end if
       if(ierr.ne.0) go to 9400
       if(abs(icode).eq.1)then
         allocate(mat%arow(nrow),stat=ierr)
         if(ierr.ne.0) go to 9400
       else
         allocate(mat%arow(nrow),mat%acol(ncol),stat=ierr)
         if(ierr.ne.0) go to 9400
       end if

! -- The matrix is read.

       if(icode.ne.-1)then
         do irow=1,nrow
           read(iunit,*,err=9100,end=9200)
     +     (mat%array(irow,icol),icol=1,ncol)
         end do
       else
         do irow=1,nrow
           read(iunit,*,err=9100,end=9200) mat%vector(irow)
         end do
       end if


! -- The row and column labels are read.

       read(iunit,'(a)',err=9300,end=9300) cline
       call lowcas(cline)
       if(abs(icode).eq.1)then
         if (index(cline,'* row and column').eq.0)then
           write(amessage,120) trim(afile)
120        format(' "* row and column names" header expected ',
     +     'immediately following matrix in file ',a,'.')
           go to 9800
         end if
       else
         if(index(cline,'* row names').eq.0)then
           write(amessage,130) trim(afile)
130        format(' "* row names" header expected immediately ',
     +     'folowing matrix in file ',a,'.')
           go to 9800
         end if
       end if
       do irow=1,nrow
131      read(iunit,*,err=9300,end=9300) mat%arow(irow)
         if(mat%arow(irow).eq.' ') go to 131
         mat%arow(irow)=adjustl(mat%arow(irow))
         call lowcas(mat%arow(irow))
       end do

       if(icode.eq.2)then
         read(iunit,'(a)',err=9500,end=9500) cline
         call lowcas(cline)
         if(index(cline,'* column names').eq.0) go to 9500
         do icol=1,ncol
132        read(iunit,*,err=9300,end=9300) mat%acol(icol)
           if(mat%acol(icol).eq.' ') go to 132
           mat%acol(icol)=adjustl(mat%acol(icol))
           call lowcas(mat%acol(icol))
         end do
       end if

       close(unit=iunit)
       return


9000   write(amessage,9010) trim(afile)
9010   format(' Error reading integer matrix header line from first ',
     + 'line of file ',a,'.')
       go to 9800
9100   write(aarow,'(i6)') irow
       aarow=adjustl(aarow)
       write(amessage,9110) trim(aarow),trim(afile)
9110   format(' Error reading matrix row number ',a,' from file ',a,'.')
       go to 9800
9200   write(amessage,9210) trim(afile)
9210   format(' Unexpected end encountered to file ',a,' while ',
     + 'reading matrix.')
       go to 9800
9300   write(amessage,9310) trim(afile)
9310   format(' Error reading row and/or column names from matrix ',
     + 'file ',a,'.')
       go to 9800
9400   write(amessage,9410) trim(afile)
9410   format(' Cannot allocate sufficient memory to hold matrix ',
     + 'located in file ',a,'.')
       go to 9800
9500   write(amessage,9510) trim(afile)
9510   format(' "* column names" header expected immediately ',
     + 'following row names in file ',a,'.')
       go to 9800

9800   ifail=1

       close(unit=iunit,iostat=ierr)
       return

       end subroutine mat_read



       subroutine mat_write(ifail,iunit,mat,matfile,amessage,cline)

       implicit none

       integer               :: ifail       ! return as zero if an error
       integer               :: iunit       ! file to write matrix to
       character*(*)         :: matfile     ! the matrix file
       character*(*)         :: amessage    ! string to write error message to
       character*(*)         :: cline       ! a character work string
       type (dmatrix)        :: mat         ! the matrix to be written

       integer        :: ierr,irow,icol
       character*200  :: afile


C -- Initialisation

       ifail=0
       call addquote(matfile,afile)

C -- The output file is opened.

       open(unit=iunit,file=matfile,iostat=ierr)
       if(ierr.ne.0) go to 9000

C -- The matrix header line is written.

       write(iunit,20,err=9000) mat%nrow,mat%ncol,mat%icode
20     format(3i6)

C -- The matrix is written.

       if(mat%icode.ne.-1)then
         do irow=1,mat%nrow
           write(iunit,30,err=9000)
     +     (mat%array(irow,icol),icol=1,mat%ncol)
30         format(8(1x,1pg14.7))
         end do
       else
         do irow=1,mat%nrow
           write(iunit,40) mat%vector(irow)
40         format(1x,1pg14.7)
         end do
       end if

C -- Matrix row and column names are written.

       if(abs(mat%icode).eq.1)then
         write(iunit,50)
50       format('* row and column names')
       else
         write(iunit,60)
60       format('* row names')
       end if
       do irow=1,mat%nrow
         write(iunit,70) trim(mat%arow(irow))
70       format(a)
       end do
       if(mat%icode.eq.2)then
         write(iunit,75)
75       format('* column names')
         do icol=1,mat%ncol
           write(iunit,80) trim(mat%acol(icol))
80         format(a)
         end do
       end if
       close(unit=iunit)
       return


9000   write(amessage,9010) trim(afile)
9010   format(' Cannot write matrix to file ',a,'.')
       go to 9800

9800   ifail=1
       close(unit=iunit,iostat=ierr)

       end subroutine mat_write


       subroutine mat_diag_expand(ifail,mat,amessage)

C -- Subroutine MAT_DIAG_EXPAND changes icode from -1 to 1 and alters
C    the representation of the matrix accordingly.

       implicit none

       integer        :: ifail
       type(dmatrix)  :: mat
       character*(*)  :: amessage

       integer irow,nnrow,nncol,ierr

       ifail=0
       if(mat%icode.ne.-1) return

       if(associated(mat%array))then
         deallocate(mat%array,stat=ierr)
         nullify(mat%array)
       end if
       nnrow=mat%nrow
       nncol=mat%ncol
       allocate(mat%array(nnrow,nncol),stat=ierr)
       if(ierr.ne.0)then
         write(amessage,10)
10       format(' Cannot allocate sufficient memory to continue ',
     +   'execution.')
         ifail=1
         return
       end if
       mat%array=0.0d0
       do irow=1,nnrow
         mat%array(irow,irow)=mat%vector(irow)
       end do
       mat%icode=1

       deallocate(mat%vector,stat=ierr)
       nullify(mat%vector)

       return

       end subroutine mat_diag_expand


       subroutine mat_icode_expand(ifail,mat,amessage)

C -- Subroutine MAT_ICODE_EXPAND alters ICODE from 1 to 2 and changes
C    the representation of the matrix accordingly.

       implicit none

       integer        :: ifail
       type(dmatrix)  :: mat
       character*(*)  :: amessage

       integer        :: nncol,ierr,icol

       if(mat%icode.ne.1) return
       if(mat%ncol.ne.mat%nrow)then
         write(amessage,5)
5        format(' Illegal call to function MAT_ICODE_EXPAND.')
         ifail=1
         return
       end if
       nncol=mat%ncol
       allocate(mat%acol(nncol),stat=ierr)
       if(ierr.ne.0)then
         write(amessage,10)
10       format(' Cannot allocate sufficient memory to continue ',
     +   'execution.')
         ifail=1
         return
       end if
       do icol=1,nncol
         mat%acol(icol)=mat%arow(icol)
       end do
       mat%icode=2

       return

       end subroutine mat_icode_expand





       subroutine mat_deallocate(ifail,mat)

       implicit none
       integer         :: ifail
       type (dmatrix)  :: mat

       integer         :: ierr

       ifail=0
       if(associated(mat%array)) then
         deallocate(mat%array,stat=ierr)
         if(ierr.ne.0) ifail=1
       end if
       if(associated(mat%vector))then
         deallocate(mat%vector,stat=ierr)
         if(ierr.ne.0) ifail=1
       end if
       if(associated(mat%arow))then
         deallocate(mat%arow,stat=ierr)
         if(ierr.ne.0) ifail=1
       end if
       if(associated(mat%acol))then
         deallocate(mat%acol,stat=ierr)
         if(ierr.ne.0) ifail=1
       end if

       return

       end subroutine mat_deallocate


       subroutine mat_diag_compress(ifail,mat,amessage)

C -- Subroutine MAT_DIAG_COMPRESS tries to represent a matrix in compressed
C    diagonal form.

       implicit none

       integer        :: ifail
       type(dmatrix)  :: mat
       character*(*)  :: amessage

       integer irow,icol,nnrow,ierr

       ifail=0
       if(mat%icode.eq.-1) return

       if(mat%ncol.ne.mat%nrow)then
         write(amessage,10)
10       format(' Cannot represent as diagonal matrix - ',
     +   'matrix not square.')
         ifail=-1
         return
       end if
       if(mat%icode.gt.1)then
         do irow=1,mat%nrow
           if(mat%acol(irow).ne.mat%arow(irow))then
             write(amessage,20)
20           format(' Cannot represent as diagonal matrix - ',
     +       'row and column names are not the same.')
             ifail=-2
             return
           end if
         end do
       end if
       do icol=1,mat%ncol
         do irow=1,mat%nrow
           if(icol.ne.irow)then
             if(mat%array(irow,icol).ne.0.0d0)then
               write(amessage,30)
30             format(' Cannot represent as diagonal matrix - ',
     +         'matrix has at least one non-zero off-diagonal ',
     +         'element.')
               ifail=-3
               return
             end if
           end if
         end do
       end do

C -- The new matrix is formed.

       if(associated(mat%vector))then
         deallocate(mat%vector,stat=ierr)
         nullify(mat%vector)
       end if
       nnrow=mat%nrow
       allocate(mat%vector(nnrow),stat=ierr)
       if(ierr.ne.0)then
         write(amessage,50)
50       format(' Cannot allocate sufficient memory to continue ',
     +   'execution.')
         ifail=1
         return
       end if
       do irow=1,nnrow
         mat%vector(irow)=mat%array(irow,irow)
       end do
       mat%icode=-1
       deallocate(mat%array,stat=ierr)
       nullify(mat%array)
       deallocate(mat%acol,stat=ierr)
       nullify(mat%acol)

       return

       end subroutine mat_diag_compress



       subroutine mat_icode_compress(ifail,mat,amessage)

C -- Subroutine MAT_ICODE_COMPRESS tries to represent a matrix in compressed
C    icode form.

       implicit none

       integer        :: ifail
       type(dmatrix)  :: mat
       character*(*)  :: amessage

       integer irow,ierr

       ifail=0
       if(mat%icode.eq.-1) return
       if(mat%icode.eq.1)return

       if(mat%ncol.ne.mat%nrow)then
         write(amessage,10)
10       format(' Cannot provide matrix with ICODE of 1 as ',
     +   'matrix not square.')
         ifail=-1
         return
       end if

       do irow=1,mat%nrow
         if(mat%arow(irow).ne.mat%acol(irow)) then
           write(amessage,20)
20         format(' Cannot provide matrix with ICODE of 1 as ',
     +     'row and column names are not the same.')
           ifail=-2
           return
         end if
       end do

       mat%icode=1
       deallocate(mat%acol,stat=ierr)
       nullify(mat%acol)

       return

       end subroutine mat_icode_compress



       end module matrix_manipulate

