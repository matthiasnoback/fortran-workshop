module test_stopwatch
   use testdrive, only: new_unittest, unittest_type, error_type, test_failed, skip_test
   use stopwatch_tdd_facade, only: stopwatch_start, &
                                   stopwatch_stop, &
                                   stopwatch_print
   use test_custom_checks, only: check

   implicit none(type, external)

   private

   public :: collect_tests

contains

   subroutine collect_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_start_stop_and_print", &
                               test_start_stop_and_print) &
                  ]
   end subroutine collect_tests

   subroutine test_start_stop_and_print(error)
      type(error_type), allocatable, intent(out) :: error

      call stopwatch_start()

      ! Should we do something intense here?

      call stopwatch_stop()

      call stopwatch_print()
      ! How can we test the output?

   end subroutine test_start_stop_and_print

end module test_stopwatch
