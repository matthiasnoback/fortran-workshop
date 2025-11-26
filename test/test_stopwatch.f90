module test_stopwatch
   use testdrive, only: new_unittest, unittest_type, error_type, test_failed, skip_test
   use stopwatch_tdd_facade, only: stopwatch_start, &
                                   stopwatch_stop, &
                                   stopwatch_result
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
      character(len=:), allocatable, dimension(:) :: actual
      character(len=45), allocatable, dimension(:) :: expected

      integer :: i
      integer :: j

      call skip_test(error, 'TODO make deterministic')
      return

      call stopwatch_start()

      ! Do something intense here to make time pass

      do i = 1, 1000000
         j = i*i/(i + 1)
      end do

      call stopwatch_stop()

      actual = stopwatch_result()
      expected = ['  wall clock time difference:        0.001000', &
                  '         cpu time difference:        0.000000']

      call check(error, actual, expected)

   end subroutine test_start_stop_and_print

end module test_stopwatch
