module test_pump
   use testdrive, only: new_unittest, unittest_type, error_type, test_failed, skip_test
   use test_custom_checks, only: check
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

   interface check
      procedure :: check_pump_specification
      procedure :: check_pump_specification_or_error
      procedure :: check_pump_state
   end interface check

contains

   subroutine collect_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_suction_side_level_above_start", &
                               test_suction_side_level_above_start), &
                  new_unittest("test_suction_side_level_below_stop", &
                               test_suction_side_level_below_stop), &
                  new_unittest("test_suction_side_level_between_start_and_stop_and_pump_running", &
                               test_suction_side_level_between_start_and_stop_and_pump_running), &
                  new_unittest("test_suction_side_level_between_start_and_stop_and_pump_running", &
                               test_suction_side_level_between_start_and_stop_and_pump_running), &
                  new_unittest("test_check_pump_specification_or_error", &
                               test_check_pump_specification_or_error) &
                  ]
   end subroutine collect_tests

   subroutine test_suction_side_level_above_start(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_t), allocatable :: specification
      specification = pump_with_capacity(10.0_dp)
      call specification%control_suction_side(2.0_dp, 1.0_dp)

      call check(error, &
                 next_pump_state(specification, &
                                 running_at_capacity(10.0_dp), &
                                 2.5_dp), &
                 running_at_capacity(10.0_dp))
   end subroutine test_suction_side_level_above_start

   subroutine test_suction_side_level_below_stop(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_t), allocatable :: specification
      specification = pump_with_capacity(10.0_dp)
      call specification%control_suction_side(2.0_dp, 1.0_dp)

      call check(error, &
                 next_pump_state(specification, &
                                 running_at_capacity(10.0_dp), &
                                 0.5_dp), &
                 switched_off())
   end subroutine test_suction_side_level_below_stop

   subroutine test_suction_side_level_between_start_and_stop_and_pump_running(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_t), allocatable :: specification
      specification = pump_with_capacity(10.0_dp)
      call specification%control_suction_side(2.0_dp, 1.0_dp)

      call check(error, &
                 next_pump_state(specification, &
                                 running_at_capacity(10.0_dp), &
                                 1.5_dp), &
                 running_at_capacity(10.0_dp))
   end subroutine test_suction_side_level_between_start_and_stop_and_pump_running

   subroutine test_level_between_start_and_stop_and_pump_not_running(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_t), allocatable :: specification
      specification = pump_with_capacity(10.0_dp)
      call specification%control_suction_side(2.0_dp, 1.0_dp)

      call check(error, &
                 next_pump_state(specification, &
                                 switched_off(), &
                                 1.5_dp), &
                 running_at_capacity(10.0_dp))
   end subroutine test_level_between_start_and_stop_and_pump_not_running

   subroutine test_check_pump_specification_or_error(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_or_error_t), dimension(:), allocatable :: actual, expected
      logical, dimension(:), allocatable :: check_should_fail

      integer, save :: cases = 4
      integer :: case

      allocate (actual(cases), expected(cases), check_should_fail(cases))

      case = 1
      ! Both cases contain an error, with the same message
      actual(case)%error = error_t('Same error')
      expected(case)%error = error_t('Same error')
      check_should_fail(case) = .false.

      case = 2
      ! Both cases contain an error, but they have different messages
      actual(case)%error = error_t('Same error')
      expected(case)%error = error_t('Different error')
      check_should_fail(case) = .true.

      case = 3
      ! Both cases contain a pump, and they are equal
      actual(case)%pump = pump_with_capacity(10.0_dp)
      expected(case)%pump = pump_with_capacity(10.0_dp)
      check_should_fail(case) = .false.

      case = 4
      ! One case contains an error, the other a pump
      actual(case)%error = error_t('An error')
      expected(case)%pump = pump_with_capacity(10.0_dp)
      check_should_fail(case) = .true.

      do case = 1, cases
         call check(error, actual(case), expected(case))
         if (check_should_fail(case) .and. .not. allocated(error)) then
            call test_failed(error, 'Case '//to_string(case)//' was expected to fail')
            return
         end if

         if (.not. check_should_fail(case) .and. allocated(error)) then
            error%message = 'Case '//to_string(case)//' was not expected to fail, but failed: ' &
                            //error%message
            return
         end if

         if (allocated(error)) then
            ! fake success, or the final procedure of error_type will escalate the error as "uncaught"
            error%stat = 0

            ! start next iteration with a clean slate
            deallocate (error)
         end if
      end do
   end subroutine test_check_pump_specification_or_error

   subroutine check_pump_specification(error, actual, expected)
      type(error_type), allocatable, intent(out) :: error
      type(pump_specification_t), intent(in) :: actual
      type(pump_specification_t), intent(in) :: expected

      ! TODO as `pump_specification_t` gains more data components, add more checks here:

      ! call check(error, actual%capacity, expected%capacity)
      ! if (allocated(error)) then
      !    return
      ! end if
   end subroutine check_pump_specification

   subroutine check_pump_specification_or_error(error, actual, expected)
      type(error_type), allocatable, intent(out) :: error
      type(pump_specification_or_error_t), intent(in) :: actual
      type(pump_specification_or_error_t), intent(in) :: expected

      if (allocated(expected%error)) then
         call check(error, allocated(actual%error), .true.)
         if (allocated(error)) then
            return
         end if

         call check(error, actual%error%message, expected%error%message)
         return
      end if

      if (allocated(expected%pump)) then
         call check(error, allocated(actual%pump), .true.)
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

      state = pump_state_t(.false., 0.0_dp)
   end function switched_off

   pure function running_at_capacity(capacity) result(state)
      real(kind=dp), intent(in) :: capacity
      type(pump_state_t), allocatable :: state

      state = pump_state_t(.true., capacity)
   end function running_at_capacity
end module test_pump
