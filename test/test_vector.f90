module test_vector
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
   use vector, only: dummy_function

   implicit none(type, external)

   private

   public :: collect_tests

contains

   subroutine collect_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("dummy_function", test_dummy_function) &
                  ]

   end subroutine collect_tests

   subroutine test_dummy_function(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, dummy_function())
   end subroutine test_dummy_function

end module test_vector
