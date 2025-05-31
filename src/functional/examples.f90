module functional_examples
   use functional_lists, only: int_list_t, is_even

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

      res = recursive_sum(integers(2:), carry + integers(1))
   end function recursive_sum

   pure function traditional_sum(integers) result(res)
      integer, dimension(:), intent(in) :: integers
      integer :: i
      integer :: res

      do i = 1, size(integers)
         res = res + integers(i)
      end do
   end function traditional_sum

   pure function int_list_filter_even(integers) result(res)
      integer, dimension(:), intent(in) :: integers
      type(int_list_t), allocatable :: res

      res = int_list_t(integers)

      res = res%filter(is_even)
   end function int_list_filter_even

end module functional_examples
