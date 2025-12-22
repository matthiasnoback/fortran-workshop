module test_rainfall_runoff
   use iso_fortran_env, only: int64, real64
   use testdrive, only: new_unittest, unittest_type, error_type, test_failed, skip_test, check
   use hydrology_rainfall_runoff, only: run

   implicit none(type, external)

   private

   public :: collect_hydrology_demo_tests

contains

   subroutine collect_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_characterization_test", &
                               test_characterization_test) &
                  ]
   end subroutine collect_tests

   subroutine test_characterization_test(error)
      type(error_type), allocatable, intent(out) :: error

   end subroutine test_characterization_test

end module test_rainfall_runoff
