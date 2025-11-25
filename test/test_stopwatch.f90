module test_stopwatch
   use testdrive, only: new_unittest, unittest_type, error_type, test_failed, skip_test
   use stopwatch_tdd_facade, only: stopwatch_start, &
                                   stopwatch_stop, &
                                   stopwatch_result, &
                                   set_clock_count
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

      character(len=:), dimension(:), allocatable :: expected

      integer :: i, j

      call set_clock_count(1764081895648000)
      call stopwatch_start()

      do i = 1, 1000000
         j = i*i/(i + 1)
      end do
      ! Should we do something intense here?

      call set_clock_count(1764081895649000)

      call stopwatch_stop()

      expected = ['  wall clock time difference:        0.001000', &
                  '         cpu time difference:        0.000000']

      call check(error, &
                 stopwatch_result(), &
                 expected)

   end subroutine test_start_stop_and_print

end module test_stopwatch
