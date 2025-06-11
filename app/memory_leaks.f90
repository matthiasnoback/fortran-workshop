
program memory_leaks
   use functional_lists, only: int_list_t

   implicit none(type, external)

   call run_all_experiments()

contains

   subroutine run_all_experiments()
      type(int_list_t), allocatable :: list

      character(len=:), allocatable :: int_string
      list = test_reassign_smaller_array()

      list = test_reassign_larger_array()
      ! Implicit: deallocate (list)

      int_string = trim(int_to_string(1))
   end subroutine run_all_experiments

   pure function int_to_string(value) result(res)
      integer, intent(in) :: value
      character(len=:), allocatable :: res

      character(len=32) :: temp

      write (temp, *) value
      res = trim(adjustl(temp))
   end function int_to_string

   function test_reassign_smaller_array() result(res)
      type(int_list_t) :: res
      integer :: i
      res%values = [(i, i=1, 40)]
      res%values = [(i, i=1, 20)]
   end function test_reassign_smaller_array

   function test_reassign_larger_array() result(res)
      type(int_list_t) :: res
      integer :: i
      res%values = [(i, i=1, 20)]
      res%values = [(i, i=1, 40)]
   end function test_reassign_larger_array

end program memory_leaks
