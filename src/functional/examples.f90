module functional_examples
   implicit none(type, external)
   public

contains

   recursive pure function recursive_sum(integers, carry) result(res)
      integer, dimension(:), intent(in) :: integers
      integer, intent(in) :: carry
      integer :: res

      if (size(integers) == 0) then
         res = carry
         return
      end if

      res = integers(1) + recursive_sum(integers(2:), carry)
   end function recursive_sum

   pure function traditional_sum(integers) result(res)
      integer, dimension(:), intent(in) :: integers
      integer :: i
      integer :: res

      do i = 1, size(integers)
         res = res + integers(i)
      end do
   end function traditional_sum
end module functional_examples
