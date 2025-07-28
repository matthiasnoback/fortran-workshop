module test_interpolation
   use testdrive, only: new_unittest, unittest_type, error_type, test_failed
   use test_custom_checks, only: check
   use common_interpolation, only: interpolate_linearly
   use common_precision, only: dp
   use common_error_handling, only: error_t
   implicit none(type, external)
   private

   public :: collect_tests

contains

   subroutine collect_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_linear_interpolation", &
                               test_linear_interpolation), &
                  new_unittest("test_linear_interpolation_with_unevenly_spaced_values", &
                               test_linear_interpolation_with_unevenly_spaced_values) &
                  ]
   end subroutine collect_tests

   subroutine test_linear_interpolation(error)
      type(error_type), allocatable, intent(out) :: error

      real(kind=dp), dimension(:, :), allocatable :: table

      table = reshape([1.0_dp, 2.0_dp, 3.0_dp, &
                       10.0_dp, 20.0_dp, 30.0_dp], [3, 2])

      call check(error, interpolate_linearly(table, 2.5_dp), 25.0_dp)
   end subroutine test_linear_interpolation

   subroutine test_linear_interpolation_with_unevenly_spaced_values(error)
      type(error_type), allocatable, intent(out) :: error

      real(kind=dp), dimension(:, :), allocatable :: table

      table = reshape([1.0_dp, 2.0_dp, 4.0_dp, &
                       10.0_dp, 20.0_dp, 40.0_dp], [3, 2])

      call check(error, interpolate_linearly(table, 2.5_dp), 25.0_dp)
   end subroutine test_linear_interpolation_with_unevenly_spaced_values

end module test_interpolation
