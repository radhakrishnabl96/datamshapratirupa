module containers

implicit none
private

public Array_struc, array_struc_add, array_struc_print, array_struc_sort
public array_struc_unique, array_struc_size, array_struc_getvalue


integer, parameter :: resize_n = 100

type Array_struc
      private
      integer :: size = 0
      integer, dimension(:), pointer :: array => null()
end type Array_struc

contains

integer function array_struc_size(self)
  implicit none
  type(Array_struc), intent(inout) :: self
  
  if(.not.(associated(self%array))) then
     array_struc_size = 0
  else
     array_struc_size = self%size
  end if
end function array_struc_size


integer function array_struc_getvalue(self, i)
  implicit none
  type(Array_struc), intent(inout) :: self
  integer, intent(in) ::  i

  array_struc_getvalue = -9999
  if(i<=self%size) array_struc_getvalue = self%array(i)
end function array_struc_getvalue


subroutine array_struc_add(self, num)
  implicit none
  type(Array_struc), intent(inout) :: self
  integer, intent(in) ::  num
  integer :: i
  
  integer, dimension(:), pointer :: array_new=> null()
  
  if(.not.(associated(self%array))) then
     allocate(self%array(resize_n))
  else if (size(self%array) <= self%size+1) then
     allocate(array_new(size(self%array)+resize_n))
     array_new = self%array
     deallocate(self%array)
     self%array => array_new
     nullify(array_new)
  end if
  self%size = self%size+1
      self%array(self%size) = num
end subroutine array_struc_add

subroutine array_struc_sort(self)
  type(Array_struc), intent(inout) :: self

  integer :: i, j, w
  
  do i = 2, self%size
     w = self%array(i)
     do j =i,2,-1
        if(w >= self%array(j-1)) exit
        self%array(j) = self%array(j-1)
     end do
     self%array(j) = w
  end do
end subroutine array_struc_sort

subroutine array_struc_unique(self)
  type(Array_struc), intent(inout) :: self

  integer, dimension(:), pointer :: array_new=>null()
  integer i, j, nunique

  call array_struc_sort(self)

  nunique = 0
  do i = 1, self%size-1
     if(self%array(i) /= self%array(i+1)) then
        nunique = nunique + 1
     end if
  end do
  if(self%size-1 /= self%size)  nunique = nunique + 1 

  j = 0
  allocate(array_new(nunique))
  do i = 1, self%size-1
     if(self%array(i) /= self%array(i+1) ) then
        j = j+1
        array_new(j) = self%array(i)
     end if
  end do
  if(self%size-1 /= self%size) array_new(j+1) = self%array(self%size)

  deallocate(self%array)
  self%array => array_new
  self%size = nunique
  nullify(array_new)
end subroutine array_struc_unique
  

subroutine array_struc_print(self)
  type(Array_struc), intent(inout) :: self
  integer i

  do i=1, self%size
     print*, i, " = ",self%array(i)
  end do
end subroutine array_struc_print

end module containers
