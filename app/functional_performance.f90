
program functional_performance
   use common_to_string, only: to_string
   use benchmark_facade, only: benchmark_repeated_procedure_calls, &
                               print_benchmark_results, &
                               clear_benchmarks
   use functional_lists, only: int_list_t, is_even, sum
   use benchmark_memory, only: get_memory_usage_kb

   implicit none(type, external)

   ! Beyond this number, we will get a runtime error:
   !   functional_perfor  0000000000405732  Unknown               Unknown
   !   Stack trace buffer overflow; further frames not shown.
   integer, parameter :: recursivity = 10000

   ! Run each benchmark several times, to get smoother results per iteration
   integer, parameter :: iterations = 1000

   call run_all_benchmarks()

contains
   subroutine run_all_benchmarks()
      print *, get_memory_usage_kb()
      ! Valgrind output: total heap usage: 1,026 allocs, 1,021 frees, 40,056,855 bytes allocated
      call benchmark_repeated_procedure_calls('Sum in do loop', &
                                              iterations, run_traditional_sum)
      print *, get_memory_usage_kb()
      ! Valgrind output: total heap usage: 1,026 allocs, 1,021 frees, 40,056,874 bytes allocated
      ! call benchmark_repeated_procedure_calls('Sum with intrinsic reduce', &
      !                                         iterations, run_intrinsic_reduce_sum)

      ! Valgrind output: total heap usage: 1,026 allocs, 1,021 frees, 40,056,879 bytes allocated
      ! call benchmark_repeated_procedure_calls('Sum with int_list_t reduce', &
      !                                         iterations, run_int_list_reduce_sum)

      ! Valgrind output: total heap usage: 1,026 allocs, 1,021 frees, 40,056,845 bytes allocated
      ! call benchmark_repeated_procedure_calls('Sum reduce recursive', &
      !                                         iterations, run_recursive_sum)

      ! Valgrind output: total heap usage: 3,026 allocs, 3,021 frees, 100,140,844 bytes allocated
      ! 40MB for the original integers, 40MB for the temporary array, 20MB for the result array
      ! call benchmark_repeated_procedure_calls('Do loop with modulo', &
      !                                         iterations, run_do_loop_modulo)

      ! Valgrind output: total heap usage: 2,026 allocs, 2,021 frees, 60,096,869 bytes allocated
      ! call benchmark_repeated_procedure_calls('Do loop with less memory', &
      !                                         iterations, run_do_loop_less_memory)

      ! Valgrind output: total heap usage: 3,026 allocs, 3,021 frees, 100,140,849 bytes allocated
      ! Due to the fact that we have to make a copy ourselves so we can have a result array
      ! call benchmark_repeated_procedure_calls('Do loop with is_even', &
      !                                         iterations, run_do_loop_is_even)

      ! Valgrind output: otal heap usage: 2,026 allocs, 2,021 frees, 60,096,909 bytes allocated
      ! call benchmark_repeated_procedure_calls('Do loop with is_even less memory', &
      !                                         iterations, run_do_loop_is_even_less_memory)

      ! Valgrind output:total heap usage: 3,026 allocs, 3,021 frees, 100,136,914 bytes allocated
      ! The compiler can't optimize the mask argument of pack...
      ! call benchmark_repeated_procedure_calls('Int array pack is_even mask array', &
      !                                         iterations, run_integer_array_pack_is_even_mask_array)

      ! Valgrind output: total heap usage: 2,026 allocs, 2,021 frees, 60,096,859 bytes allocated
      ! 40MB for the original integers, 20MB for the result; *but what about the logical mask?*
      ! call benchmark_repeated_procedure_calls('Int array pack is_even', &
      !                                         iterations, run_integer_array_pack_is_even)

      ! Valgrind output: total heap usage: 4,026 allocs, 4,021 frees, 80,248,839 bytes allocated
      ! 40MB for the original integers, 20MB for the even ones, 20MB for copying the function result
      ! call benchmark_repeated_procedure_calls('int_list_t is_even', &
      !                                         iterations, run_int_list_filter_even)

      ! Valgrind output: total heap usage: 2,026 allocs, 2,021 frees, 60,096,839 bytes allocated
      ! call benchmark_repeated_procedure_calls('int_list_t mutable', &
      !                                         iterations, run_int_list_filter_even_mutable)

      ! Valgrind output: total heap usage: 2,026 allocs, 2,021 frees, 60,096,874 bytes allocated
      ! call benchmark_repeated_procedure_calls('int_list_t intent out arg', &
      !                                         iterations, run_int_list_filter_even_intent_out_argument)

      call print_benchmark_results()

      call clear_benchmarks()
   end subroutine run_all_benchmarks

   subroutine run_traditional_sum()
      integer, dimension(:), allocatable :: integers
      integer :: result
      integer :: i
      integers = [(i, i=1, recursivity)]

      do i = 1, size(integers)
         result = result + integers(i)
      end do
   end subroutine run_traditional_sum

   subroutine run_intrinsic_reduce_sum()
      integer :: result
      integer :: i
      result = reduce([(i, i=1, recursivity)], sum)
   end subroutine run_intrinsic_reduce_sum

   subroutine run_int_list_reduce_sum()
      type(int_list_t) :: integers
      integer :: i
      integer :: result

      integers%values = [(i, i=1, recursivity)]

      result = integers%reduce(sum, 0)
   end subroutine run_int_list_reduce_sum

   subroutine run_recursive_sum()
      integer :: i
      integer :: result
      result = recursive_sum([(i, i=1, recursivity)], 0)
   end subroutine run_recursive_sum

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

   subroutine run_int_list_filter_even()
      type(int_list_t) :: integers
      type(int_list_t), allocatable :: even_integers
      integer :: i

      integers%values = [(i, i=1, recursivity)]

      even_integers = integers%filter(is_even)
   end subroutine run_int_list_filter_even

   subroutine run_int_list_filter_even_mutable()
      type(int_list_t) :: integers
      integer :: i

      integers%values = [(i, i=1, recursivity)]

      call integers%int_list_filter_mutable(is_even)
   end subroutine run_int_list_filter_even_mutable

   subroutine run_int_list_filter_even_intent_out_argument()
      type(int_list_t) :: integers
      type(int_list_t) :: even_integers

      integer :: i

      integers%values = [(i, i=1, recursivity)]

      call integers%int_list_filter_out_argument(is_even, even_integers)
   end subroutine run_int_list_filter_even_intent_out_argument

   subroutine run_integer_array_pack_is_even()
      integer, dimension(:), allocatable :: integers
      integer, dimension(:), allocatable :: res

      integer :: i

      integers = [(i, i=1, recursivity)]

      res = pack(integers, [(is_even(integers(i)), i=1, size(integers))])
   end subroutine run_integer_array_pack_is_even

   subroutine run_integer_array_pack_is_even_mask_array()
      integer, dimension(:), allocatable :: integers
      integer, dimension(:), allocatable :: res
      logical, dimension(:), allocatable :: mask

      integer :: i

      integers = [(i, i=1, recursivity)]

      mask = [(is_even(integers(i)), i=1, size(integers))]

      res = pack(integers, mask)
   end subroutine run_integer_array_pack_is_even_mask_array

   subroutine run_do_loop_modulo()
      integer, dimension(:), allocatable :: integers
      integer, dimension(:), allocatable :: res

      integer :: i
      integer :: res_index

      integers = [(i, i=1, recursivity)]

      allocate (res(size(integers)))

      res_index = 1

      do i = 1, size(integers)
         if (mod(integers(i), 2) == 0) then
            res(res_index) = integers(i)
            res_index = res_index + 1
         end if
      end do

      res = res(:res_index)
   end subroutine run_do_loop_modulo

   subroutine run_do_loop_less_memory()
      integer, dimension(:), allocatable :: integers
      integer, dimension(:), allocatable :: res

      integer :: i
      integer :: res_index

      integers = [(i, i=1, recursivity)]

      res_index = 0

      do i = 1, size(integers)
         if (mod(integers(i), 2) == 0) then
            res_index = res_index + 1
            integers(res_index) = integers(i)
         end if
      end do

      res = integers(:res_index)
   end subroutine run_do_loop_less_memory

   subroutine run_do_loop_is_even()
      integer, dimension(:), allocatable :: integers
      integer, dimension(:), allocatable :: res

      integer :: i
      integer :: res_index

      integers = [(i, i=1, recursivity)]
      allocate (res(size(integers)))

      res_index = 1

      do i = 1, size(integers)
         if (is_even(integers(i))) then
            res(res_index) = integers(i)
            res_index = res_index + 1
         end if
      end do

      res = res(:res_index)
   end subroutine run_do_loop_is_even

   subroutine run_do_loop_is_even_less_memory()
      integer, dimension(:), allocatable :: integers
      integer, dimension(:), allocatable :: res

      integer :: i
      integer :: res_index

      integers = [(i, i=1, recursivity)]

      res_index = 0

      do i = 1, size(integers)
         if (is_even(integers(i))) then
            res_index = res_index + 1
            integers(res_index) = integers(i)
         end if
      end do

      res = integers(:res_index)
   end subroutine run_do_loop_is_even_less_memory
end program functional_performance
