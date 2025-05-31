
program functional_performance
   use common_to_string, only: to_string
   use benchmark_facade, only: benchmark_repeated_procedure_calls, &
                               print_benchmark_results
   use functional_examples, only: traditional_sum, &
                                  recursive_sum, &
                                  int_list_filter_even
   use functional_lists, only: int_list_t, is_even

   implicit none(type, external)

   ! Beyond this number, we will get a runtime error:
   !   functional_perfor  0000000000405732  Unknown               Unknown
   !   Stack trace buffer overflow; further frames not shown.
   integer, parameter :: recursivity = 10000

   ! Run each benchmark several times, to get smoother results per iteration
   integer, parameter :: iterations = 1000

   ! Valgrind output: total heap usage: 1,026 allocs, 1,021 frees, 40,056,855 bytes allocated
   call benchmark_repeated_procedure_calls('Sum in do loop', &
                                           iterations, run_traditional_sum)

   ! Valgrind output: total heap usage: 1,026 allocs, 1,021 frees, 40,056,845 bytes allocated
   call benchmark_repeated_procedure_calls('Sum reduce recursive', &
                                           iterations, run_recursive_sum)

   ! Valgrind output: total heap usage: 4,029 allocs, 4,024 frees, 140,181,993 bytes allocated
   ! To be explained by the fact that we have to make a copy ourselves so we can have a result array
   call benchmark_repeated_procedure_calls('Do loop with modulo', &
                                           iterations, run_do_loop_is_even)
   ! Valgrind output: total heap usage: 3,029 allocs, 3,024 frees, 100,138,008 bytes allocated
   call benchmark_repeated_procedure_calls('Int array pack is_even', &
                                           iterations, run_integer_array_pack_is_even)
   ! Valgrind output: total heap usage: 7,029 allocs, 7,024 frees, 140,442,003 bytes allocated
   ! To be explained by the derived type, that holds a copy of the integers (which are about 40MB)
   call benchmark_repeated_procedure_calls('int_list_t is_even', &
                                           iterations, run_int_list_filter_even)

   call print_benchmark_results()

contains
   subroutine run_traditional_sum()
      integer :: sum
      sum = traditional_sum(create_integer_array())
   end subroutine run_traditional_sum

   subroutine run_recursive_sum()
      integer :: sum
      sum = recursive_sum(create_integer_array(), 0)
   end subroutine run_recursive_sum

   subroutine run_int_list_filter_even()
      type(int_list_t) :: even_integers
      even_integers = int_list_filter_even(create_integer_array())
   end subroutine run_int_list_filter_even

   subroutine run_integer_array_pack_is_even()
      integer, dimension(:), allocatable :: integers
      integer, dimension(:), allocatable :: res

      integer :: i

      integers = create_integer_array()

      res = pack(integers, [(is_even(integers(i)), i=1, size(integers))])
   end subroutine run_integer_array_pack_is_even

   subroutine run_do_loop_is_even()
      integer, dimension(:), allocatable :: integers
      integer, dimension(:), allocatable :: res

      integer :: i
      integer :: res_index

      integers = create_integer_array()
      allocate (res(size(integers)))

      res_index = 1

      do i = 1, size(integers)
         if (mod(integers(i), 2) == 0) then
            res(res_index) = integers(i)
            res_index = res_index + 1
         end if
      end do

      res = res(:res_index)
   end subroutine run_do_loop_is_even

   function create_integer_array() result(integers)
      integer, dimension(:), allocatable :: integers

      integer :: i

      integers = [(i, i=1, 10000)]
   end function create_integer_array

end program functional_performance
