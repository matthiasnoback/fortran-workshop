module test_benchmark
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
   use benchmark_facade, only: start_benchmark, &
                               stop_benchmark, &
                               get_benchmarks, &
                               benchmark_ended_t, &
                               benchmark_result_t, &
                               benchmark_repeated_procedure_calls, &
                               print_benchmark_results, &
                               clear_benchmarks
   use benchmark_diagnostics, only: diagnostics_snapshot_t, &
                                    override_snapshot_for_testing, &
                                    override_clock_rate_for_testing

   use iso_fortran_env, only: real64, int64

   implicit none(type, external)

   private

   public :: collect_tests

   interface check
      procedure :: check_ended_benchmarks
      procedure :: check_benchmark_ended
      procedure :: check_diagnostic_snapshot
      procedure :: check_benchmark_result
   end interface check

   integer :: dummy_procedure_invoked = 0

contains

   subroutine collect_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_start_and_stop", test_start_and_stop), &
                  new_unittest("test_benchmark_repeated", test_benchmark_repeated), &
                  new_unittest("test_benchmark_result", test_benchmark_result) &
                  ]

   end subroutine collect_tests

   subroutine test_start_and_stop(error)
      type(error_type), allocatable, intent(out) :: error
      type(benchmark_ended_t), dimension(:), pointer :: benchmarks
      type(diagnostics_snapshot_t), target :: snapshot_start, snapshot_end

      call clear_benchmarks()

      call override_snapshot_for_testing(snapshot_start)

      snapshot_start%cpu_time = 7.0_real64
      snapshot_start%clock_time = 1000000_int64

      call start_benchmark('foo')

      call override_snapshot_for_testing(snapshot_end)
      snapshot_end%cpu_time = 10.0_real64
      snapshot_end%clock_time = 2000000_int64

      call stop_benchmark('foo')

      benchmarks => get_benchmarks()
      call check(error, benchmarks, [benchmark_ended_t('foo', snapshot_start, snapshot_end)])

   end subroutine test_start_and_stop

   subroutine test_benchmark_repeated(error)
      type(error_type), allocatable, intent(out) :: error

      call clear_benchmarks()

      call benchmark_repeated_procedure_calls('bar', 1000, dummy_procedure)

      call check(error, size(get_benchmarks()), 1)
      call check(error, dummy_procedure_invoked, 1000)
   end subroutine test_benchmark_repeated

   subroutine dummy_procedure()
      dummy_procedure_invoked = dummy_procedure_invoked + 1
   end subroutine dummy_procedure

   subroutine test_benchmark_result(error)
      type(error_type), allocatable, intent(out) :: error

      type(benchmark_ended_t), allocatable :: benchmark_ended

      benchmark_ended = benchmark_ended_t( &
                        'foo', &
                        diagnostics_snapshot_t(7.0_real64, 1_int64), &
                        diagnostics_snapshot_t(10.0_real64, 2_int64) &
                        )

      call override_clock_rate_for_testing(2.0_real64)
      call check(error, benchmark_ended%result(), benchmark_result_t(3.0_real64, 0.5_real64))
   end subroutine test_benchmark_result

   subroutine check_ended_benchmarks(error, actual, expected)
      type(error_type), allocatable, intent(out) :: error

      integer :: i

      type(benchmark_ended_t), dimension(:), intent(in) :: actual
      type(benchmark_ended_t), dimension(:), intent(in) :: expected

      call check(error, size(actual), size(expected), 'Expected two arrays of the same size')
      if (allocated(error)) then
         return
      end if

      do i = 1, size(expected)
         call check(error, actual(i), expected(i))
         if (allocated(error)) then
            return
         end if
      end do
   end subroutine check_ended_benchmarks

   subroutine check_benchmark_ended(error, actual, expected)
      type(error_type), allocatable, intent(out) :: error

      type(benchmark_ended_t), intent(in) :: actual
      type(benchmark_ended_t), intent(in) :: expected

      call check(error, actual%name, expected%name)
      if (allocated(error)) then
         return
      end if

      call check(error, actual%start, expected%start)
      if (allocated(error)) then
         return
      end if

      call check(error, actual%end, expected%end)
      if (allocated(error)) then
         return
      end if
   end subroutine check_benchmark_ended

   subroutine check_diagnostic_snapshot(error, actual, expected)
      type(error_type), allocatable, intent(out) :: error

      type(diagnostics_snapshot_t), intent(in) :: actual
      type(diagnostics_snapshot_t), intent(in) :: expected

      call check(error, actual%cpu_time, expected%cpu_time)
      if (allocated(error)) then
         return
      end if

      call check(error, actual%clock_time, expected%clock_time)
      if (allocated(error)) then
         return
      end if
   end subroutine check_diagnostic_snapshot

   subroutine check_benchmark_result(error, actual, expected)
      type(error_type), allocatable, intent(out) :: error

      type(benchmark_result_t), intent(in) :: actual
      type(benchmark_result_t), intent(in) :: expected

      call check(error, actual%cpu_time_diff, expected%cpu_time_diff)
      if (allocated(error)) then
         return
      end if

      call check(error, actual%wall_clock_time_diff, expected%wall_clock_time_diff)
      if (allocated(error)) then
         return
      end if
   end subroutine check_benchmark_result

end module test_benchmark
