module test_pump
   use testdrive, only: new_unittest, unittest_type, error_type, test_failed, skip_test
   use test_custom_checks, only: check, assert_check_passed, assert_check_failed
   use hydraulic_structures_pump, only: pump_specification_t, &
                                        pump_specification_or_error_t, &
                                        next_pump_state, &
                                        pump_state_t, &
                                        pump_with_capacity, &
                                        load_pump_specification
   use test_check_pump_specification, only: check
   use common_error_handling, only: error_t
   use common_to_string, only: to_string
   use iso_fortran_env, only: real64
   use config_loading, only: configuration_t, config_value_t

   implicit none(type, external)

   private

   public :: collect_tests

   interface check
      procedure :: check_pump_specification_or_error
      procedure :: check_pump_state
   end interface check

contains

   subroutine collect_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_load_specification", &
                               test_load_specification), &
                  new_unittest("test_capacity_is_missing", &
                               test_capacity_is_missing), &
                  new_unittest("test_capacity_is_not_a_real", &
                               test_capacity_is_not_a_real), &
                  new_unittest("test_check_same_error", &
                               test_check_same_error), &
                  new_unittest("test_check_different_error", &
                               test_check_different_error), &
                  new_unittest("test_check_same_capacity", &
                               test_check_same_specification), &
                  new_unittest("test_check_different_capacity", &
                               test_check_different_specification) &
                  ]
   end subroutine collect_tests

   subroutine test_load_specification(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_or_error_t) :: actual
      type(pump_specification_or_error_t) :: expected

      expected%pump = pump_specification_t(10.0_real64, 4.0_real64, 2.0_real64)

      actual = load_pump_specification( &
               configuration_t([ &
                               config_value_t('capacity', '10.0'), &
                               config_value_t('start_level', '4.0'), &
                               config_value_t('stop_level', '2.0') &
                               ]) &
               )

      call check(error, actual, expected)
   end subroutine test_load_specification

   subroutine test_capacity_is_missing(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_or_error_t) :: actual
      type(pump_specification_or_error_t) :: expected

      expected%error = error_t('"capacity" is required.')

      actual = load_pump_specification( &
               configuration_t([ &
                               config_value_t('no_capacity', '10.0') &
                               ]) &
               )

      call check(error, actual, expected)
   end subroutine test_capacity_is_missing

   subroutine test_capacity_is_not_a_real(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_or_error_t) :: actual
      type(pump_specification_or_error_t) :: expected

      expected%error = error_t('"capacity" should be a real.')

      actual = load_pump_specification( &
               configuration_t([ &
                               config_value_t('capacity', 'not a real') &
                               ]) &
               )

      call check(error, actual, expected)
   end subroutine test_capacity_is_not_a_real

   subroutine test_check_same_error(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_or_error_t) :: actual
      type(pump_specification_or_error_t) :: expected
      type(error_type), allocatable :: error_from_check

      actual%error = error_t('Same error')
      expected%error = error_t('Same error')

      call check(error_from_check, actual, expected)
      call assert_check_passed(error, error_from_check)
   end subroutine test_check_same_error

   subroutine test_check_different_error(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_or_error_t) :: actual
      type(pump_specification_or_error_t) :: expected
      type(error_type), allocatable :: error_from_check

      actual%error = error_t('Same error')
      expected%error = error_t('Different error')

      call check(error_from_check, actual, expected)
      call assert_check_failed(error, error_from_check)
   end subroutine test_check_different_error

   subroutine test_check_same_specification(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_or_error_t) :: actual
      type(pump_specification_or_error_t) :: expected
      type(error_type), allocatable :: error_from_check

      actual%pump = pump_with_capacity(10.0_real64)
      expected%pump = pump_with_capacity(10.0_real64)

      call check(error_from_check, actual, expected)
      call assert_check_passed(error, error_from_check)
   end subroutine test_check_same_specification

   subroutine test_check_different_specification(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_or_error_t) :: actual
      type(pump_specification_or_error_t) :: expected
      type(error_type), allocatable :: error_from_check

      actual%pump = pump_with_capacity(10.0_real64)
      expected%pump = pump_with_capacity(5.0_real64)

      call check(error_from_check, actual, expected)
      call assert_check_failed(error, error_from_check)
   end subroutine test_check_different_specification

   subroutine check_pump_specification_or_error(error, actual, expected)
      type(error_type), allocatable, intent(out) :: error
      type(pump_specification_or_error_t), intent(in) :: actual
      type(pump_specification_or_error_t), intent(in) :: expected

      if (allocated(expected%error)) then
         call check(error, allocated(actual%error), .true., 'Expected an error')
         if (allocated(error)) then
            return
         end if

         call check(error, actual%error%message, expected%error%message)
         return
      end if

      if (allocated(expected%pump)) then
         call check(error, allocated(actual%pump), .true., 'Expected a pump specification')
         if (allocated(error)) then
            return
         end if

         call check(error, actual%pump, expected%pump)
         return
      end if
   end subroutine check_pump_specification_or_error

   subroutine check_pump_state(error, actual, expected)
      type(error_type), allocatable, intent(out) :: error
      type(pump_state_t), intent(in) :: actual
      type(pump_state_t), intent(in) :: expected

      call check(error, actual%discharge, expected%discharge)
      if (allocated(error)) then
         return
      end if

      call check(error, actual%is_running, expected%is_running)
      if (allocated(error)) then
         return ! optional, if it's the last call to `check`
      end if
   end subroutine check_pump_state

   pure function switched_off() result(state)
      type(pump_state_t), allocatable :: state

      state = pump_state_t(.false., 0.0_real64)
   end function switched_off

   pure function running_at_capacity(capacity) result(state)
      real(kind=real64), intent(in) :: capacity
      type(pump_state_t), allocatable :: state

      state = pump_state_t(.true., capacity)
   end function running_at_capacity
end module test_pump
