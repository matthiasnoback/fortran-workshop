module test_integration
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
   use iso_fortran_env, only: wp => real64
   use integration_trapezoid, only: integrate_trapezoid
   use integration_damped_oscillator, only: damped_oscillator_t

   implicit none(type, external)

   private

   public :: collect_tests

contains

   subroutine collect_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest( &
                  "test_integration_of_damped_oscillator", &
                  test_integration_of_damped_oscillator &
                  ) &
                  ]

   end subroutine collect_tests

   subroutine test_integration_of_damped_oscillator(error)
      type(error_type), allocatable, intent(out) :: error
      real(kind=wp) :: integral

      integral = integrate_trapezoid(damped_oscillator_t(1.0_wp, 2.0_wp), 0.0_wp, 10.0_wp, 100)

      call check(error, integral, 0.2508476515891153_wp)
   end subroutine test_integration_of_damped_oscillator

end module test_integration
