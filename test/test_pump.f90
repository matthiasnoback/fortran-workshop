module test_pump
   use testdrive, only: new_unittest, unittest_type, error_type, test_failed, skip_test
   use test_custom_checks, only: check
   use hydraulic_structures_pump, only: pump_specification_t, &
                                        pump_specification_or_error_t, &
                                        calculate_pump_discharge
   use common_error_handling, only: error_t
   use common_to_string, only: to_string
   use common_precision, only: dp

   implicit none(type, external)

   private

   public :: collect_tests

   interface check
      procedure :: check_pump_specification
      procedure :: check_pump_specification_or_error
   end interface check

contains

   subroutine collect_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_discharge_of_a_pump_that_is_turned_off", &
                               test_discharge_of_a_pump_that_is_turned_off), &
                  new_unittest("test_check_pump_specification_or_error", &
                               test_check_pump_specification_or_error) &
                  ]
   end subroutine collect_tests

   subroutine test_discharge_of_a_pump_that_is_turned_off(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, calculate_pump_discharge(), 0.0_dp)
   end subroutine test_discharge_of_a_pump_that_is_turned_off

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
      actual(case)%pump = pump_specification_t()
      expected(case)%pump = pump_specification_t()
      check_should_fail(case) = .false.

      case = 4
      ! One case contains an error, the other a pump
      actual(case)%error = error_t('An error')
      expected(case)%pump = pump_specification_t()
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
end module test_pump
