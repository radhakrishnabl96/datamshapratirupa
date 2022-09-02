module string_utils

! This module contains useveral utilities to a assist in string
! manipulations.  Included are the following 
!
! Tokenizer - This is a combination of the tokenizer_struc and the
! routines tokenizer_init and tokenizer_get.  Its purpose is to
! parse a string and return the space of comma deliminated items
! The usage to as follows:
!   call tokenizer_init(tokenizer, cline) 
!   atemp1 = trim(tokenizer_get(tokenizer))
!   atemp2 = trim(tokenizer_get(tokenizer))
! In this example the call to tokenizer_init will initialize the
! the structure with the string cline and the calls to 
! tokenizer_get() return first and second space or comma deliminated items.
! The empty string, "", is returned when no more items are left. 
!
!function tolower(cstring) and function tolower(cstring, length)
! This functions converts the input charater string to lower case
! and return it.  The second form will also truncate the string
! and only return the first (length) characters


implicit none
private


interface tolower
   module procedure tolower1, tolower2
end interface

public tokenizer_struc, tokenizer_init, tokenizer_get
public tolower

type tokenizer_struc
      private
      character(len=2000) :: cline
      integer ilast
end type tokenizer_struc


contains 

subroutine tokenizer_init(self, inp_line)
  implicit none
  type(tokenizer_struc), intent(inout) :: self
  character(len=*), intent(in) ::  inp_line
  self%cline = inp_line
  self%ilast = 0
end subroutine tokenizer_init

function tokenizer_get(self)
  implicit none
  type(tokenizer_struc), intent(inout) :: self
  character(len=300)  tokenizer_get
  integer i, istart, iend
  
  tokenizer_get = ""
  ! find begining of token
  do i = self%ilast+1, len(self%cline)
     if( .not.((self%cline(i:i)==' ') .or. (self%cline(i:i) == ',') &
          .or. (ichar(self%cline(i:i))== 9) ) ) exit
  end do
  
  istart = i
  if(istart>= len(self%cline)) then
     iend=istart
     self%ilast = istart
  else if(self%cline(istart:istart) == '"') then
     istart = istart + 1
     do i = istart, len(self%cline)
        if( (self%cline(i:i)=='"') ) exit
     end do
     self%ilast = i
     iend = i-1
  else
     do i = istart, len(self%cline)
        if( (self%cline(i:i)==' ') .or. (self%cline(i:i) == ',') &
             .or. (ichar(self%cline(i:i))== 9) ) exit
     end do
     self%ilast = i
     iend = min(i-1, len(self%cline))
  end if
  
  if (istart<=len(self%cline)) then


     tokenizer_get = self%cline(istart:iend)
  end if
end function tokenizer_get


function tolower1(cstring)
  implicit none
  character(len=*), intent(in) :: cstring
  character(len=len_trim(cstring)) tolower1
  
  integer i, j

  tolower1 = cstring(1:len_trim(cstring))
  do i=1,len_trim(tolower1)
     j=ichar(tolower1(i:i))
     if((j.ge.65).and.(j.le.90)) tolower1(i:i)=char(j+32)
  end do
  return
end function tolower1

function tolower2(cstring, length)
  implicit none
  integer, intent(in) ::  length
  character(len=*), intent(in) :: cstring
  character(len=min(len_trim(cstring), length)) tolower2
  
  integer i, j

  tolower2 = cstring(1:min(len_trim(cstring), length))
  do i=1,len_trim(tolower2)
     j=ichar(tolower2(i:i))
     if((j.ge.65).and.(j.le.90)) tolower2(i:i)=char(j+32)
  end do
  return
end function tolower2


end module string_utils
