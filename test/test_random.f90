module test_random
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
   use common_random, only: initialize_rng, select_random_integer

   implicit none(type, external)

   private

   public :: collect_tests
contains

   subroutine collect_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_select_random_integer", &
                               test_select_random_integer) &
                  ]
   end subroutine collect_tests

   subroutine test_select_random_integer(error)
      type(error_type), allocatable, intent(out) :: error

      integer :: actual
      integer :: i

      call initialize_rng()

      actual = select_random_integer([(i, i=1, 100)])
   end subroutine test_select_random_integer

end module test_random
