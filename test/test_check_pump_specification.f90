module test_check_pump_specification
   use testdrive, only: new_unittest, unittest_type, error_type, test_failed, skip_test
   use test_custom_checks, only: check, assert_check_failed, assert_check_passed
   use hydraulic_structures_pump, only: pump_specification_t, &
                                        pump_specification_or_error_t, &
                                        next_pump_state, &
                                        pump_state_t, &
                                        pump_with_capacity
   use common_error_handling, only: error_t
   use common_to_string, only: to_string
   use common_precision, only: dp

   implicit none(type, external)

   private

   public :: collect_tests
   public :: check

   interface check
      procedure :: check_pump_specification
   end interface check

contains

   subroutine collect_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_same_capacity", &
                               test_same_capacity), &
                  new_unittest("test_different_capacity", &
                               test_different_capacity) &
                  ]
   end subroutine collect_tests

   subroutine test_same_capacity(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_t), allocatable :: actual
      type(pump_specification_t), allocatable :: expected
      type(error_type), allocatable :: check_error

      actual = pump_with_capacity(10.0_dp)
      expected = pump_with_capacity(10.0_dp)

      call check(check_error, actual, expected)

      call assert_check_passed(error, check_error)

   end subroutine test_same_capacity

   subroutine test_different_capacity(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_t), allocatable :: actual
      type(pump_specification_t), allocatable :: expected
      type(error_type), allocatable :: check_error

      actual = pump_with_capacity(10.0_dp)
      expected = pump_with_capacity(20.0_dp)

      call check(check_error, actual, expected)

      call assert_check_failed(error, check_error, 'Floating point value mismatch')
   end subroutine test_different_capacity

   subroutine check_pump_specification(error, actual, expected)
      type(error_type), allocatable, intent(out) :: error
      type(pump_specification_t), intent(in) :: actual
      type(pump_specification_t), intent(in) :: expected

      ! TODO as `pump_specification_t` gains more data components, add more checks here:

      call check(error, actual%capacity, expected%capacity)
      if (allocated(error)) then
         return
      end if
   end subroutine check_pump_specification
end module test_check_pump_specification
