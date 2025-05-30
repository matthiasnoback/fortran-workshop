
program functional_performance
   use benchmark_facade, only: benchmark_repeated_procedure_calls, &
                               print_benchmark_results
   use functional_examples, only: traditional_sum, &
                                  recursive_sum

   implicit none(type, external)

   call benchmark_repeated_procedure_calls('Traditional sum 1 to 10000', 1000, run_traditional_sum)
   call benchmark_repeated_procedure_calls('Recursive sum 1 to 10000', 1000, run_recursive_sum)

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

   function create_integer_array() result(integers)
      integer, dimension(:), allocatable :: integers

      integer :: i

      integers = [(i, i=1, 10000)]
   end function create_integer_array
end program functional_performance
