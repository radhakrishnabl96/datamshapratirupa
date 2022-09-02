module array_btree

! This module impliments a binary tree that is designed to work in
! conjunction with a fortran character array.  This module was designed
! to provide an effiecent means to search for elements in character arrays.
! An example of how this module is use follows
!
!  type(array_btree_char_struc) :: aobs_btree
!  call array_btree_char_init (aobs_btree, aobs,stat=ierr, stat_msg=emsg)
!  found = array_btree_char_getval(aobs_btree, aobs,"search_val"), j)
!  
! The first line creates a new instance of an array_b_tree
! The second line initializes the binary tree to work with character array aobs
! The third line calls the routine array_btree_char_getval which looks
! for the string "search_val" in the array of character strings aobs
! if it is not found false is returned hower if it is found true is returned
! and its index is return in the arguement j 


implicit none
private

public array_btree_char_struc, array_btree_char_init, array_btree_char_destroy
public array_btree_char_getval

type array_btree_char_struc
   integer, allocatable:: node(:)
end type array_btree_char_struc

contains

subroutine array_btree_char_init(self, array, stat, stat_msg)
  type (array_btree_char_struc), intent(inout) :: self
  character(len=*), intent(in) :: array(:)
  integer, intent(out), optional :: stat
  character(len=*), intent(inout), optional ::  stat_msg

  character(len=200) :: emsg
  integer :: i, j, w, isize, ierr

  !    Initialize error handling varaibles
      ierr=0
      emsg=""
      if ( present(stat) ) stat = 0
      if (present(stat_msg)) stat_msg = ""

  isize = size(array)
  if(allocated(self%node)) deallocate(self%node)
  allocate(self%node(isize), stat=ierr)
  if(ierr /=0) goto 9200

  do i=1, isize
     self%node(i) = i
  end do
  
  do i = 2, isize
     w = self%node(i)
     do j =i,2,-1
        if(array(w) >= array(self%node(j-1))) exit
        self%node(j) = self%node(j-1)
     end do
     self%node(j) = w
  end do
  goto 9900 ! finish with out any error

  ! Handle Errors
9200 emsg = 'Cannot allocate sufficient memory to continue execution.'
  goto 9800
9800 continue
  if ( present(stat) ) stat = 1
  if (present(stat_msg)) stat_msg = emsg
  if ( .not. (present(stat) .and. present(stat_msg) ) ) then
     write(6, FMT='(//," Error in subroutine array_btree_char_init:")')
     write(6,FMT='("  ",a)') emsg
  end if
9900 continue
end subroutine array_btree_char_init

subroutine array_btree_char_destroy(self)
  type (array_btree_char_struc), intent(inout) :: self
  if(allocated(self%node)) deallocate(self%node)
end subroutine array_btree_char_destroy

logical function array_btree_char_getval(self, array, value, index)
  type (array_btree_char_struc), intent(in) :: self
  character(len=*), intent(in) :: array(:)
  character(len=*), intent(in) :: value
  integer, intent(out) :: index

  logical :: found
  integer :: il, ic, ir
  integer :: ila, ica, ira

  array_btree_char_getval = .false.
  il = 1
  ir = size(self%node)
  
  do
     ic = (il+ir+1)/2
     ila = self%node(il)  !index of left or smaller element in array
     ica = self%node(ic)  !index of center element in array
     ira = self%node(ir)  !index of right or larger element in array
     if(value==array(ila) )then
        found = .true.
        index =  ila
        exit
     else if(value==array(ica) )then
        found = .true.
        index =  ica
        exit
     else if(value==array(ira) )then
        found = .true.
        index =  ira
        exit
     else if( (il==ic) .or. (ic==ir)) then
        found = .false.
        index = -999
        exit
     else if(value < array(ica)) then
        ir = ic
     else
        il = ic
     end if
  end do
  array_btree_char_getval = found
end function array_btree_char_getval

end module array_btree
     
     
