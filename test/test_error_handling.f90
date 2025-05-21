module test_error_handling
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
   use common_error_handling, only: error_t

   implicit none(type, external)

   private

   public :: collect_tests
contains

   subroutine collect_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_error_to_string", &
                               test_error_to_string), &
                  new_unittest("test_error_with_previous_error_to_string", &
                               test_error_with_previous_error_to_string) &
                  ]
   end subroutine collect_tests

   subroutine test_error_to_string(error)
      type(error_type), allocatable, intent(out) :: error

      type(error_t), allocatable :: current_error
      current_error = error_t('Something went wrong')

      call check(error, current_error%to_string(), 'Something went wrong')
   end subroutine test_error_to_string

   subroutine test_error_with_previous_error_to_string(error)
      type(error_type), allocatable, intent(out) :: error

      type(error_t), allocatable :: current
      type(error_t), allocatable :: previous_error

      previous_error = error_t('The previous error')

      current = error_t('The current error', previous_error)

      call check(error, current%to_string(), 'The current error The previous error')
   end subroutine test_error_with_previous_error_to_string

end module test_error_handling
