module test_custom_checks
   use testdrive, only: new_unittest, unittest_type, error_type, test_failed, skip_test, check
   use common_to_string, only: to_string

   implicit none(type, external)

   private

   public :: check
   public :: collect_tests
   public :: assert_check_passed
   public :: assert_check_failed

   interface check
      module procedure :: check_string_array
      module procedure :: check_integer_array
   end interface check

   type :: string_array_t
      character(len=:), dimension(:), allocatable :: strings
   end type string_array_t

   type :: integer_array_t
      integer, dimension(:), allocatable :: integers
   end type integer_array_t

contains

   subroutine collect_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_check_string_array", &
                               test_check_string_array), &
                  new_unittest("test_check_integer_array", &
                               test_check_integer_array) &
                  ]
   end subroutine collect_tests

   subroutine test_check_string_array(error)
      type(error_type), allocatable, intent(out) :: error

      type(string_array_t), dimension(:), allocatable :: actual, expected
      logical, dimension(:), allocatable :: check_should_fail

      integer, save :: cases = 3
      integer :: case

      allocate (actual(cases), expected(cases), check_should_fail(cases))

      case = 1
      ! Sizes are equal, values are equal
      actual(case)%strings = ['foo', 'bar']
      expected(case)%strings = ['foo', 'bar']
      check_should_fail(case) = .false.

      case = 2
      ! Sizes are not equal
      actual(case)%strings = ['foo', 'bar']
      expected(case)%strings = ['foo']
      check_should_fail(case) = .true.

      case = 3
      ! Sizes are equal, but values not
      actual(case)%strings = ['foo', 'bar']
      expected(case)%strings = ['foo', 'baz']
      check_should_fail(case) = .true.

      do case = 1, cases
         call check(error, actual(case)%strings, expected(case)%strings)
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
   end subroutine test_check_string_array

   subroutine check_string_array(error, actual, expected)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), dimension(:), intent(in) :: actual
      character(len=*), dimension(:), intent(in) :: expected

      integer :: index

      if (size(actual) /= size(expected)) then
         call test_failed(error, 'Expected size '//to_string(size(actual))// &
                          ' but got '//to_string(size(expected)))
         return
      end if

      do index = 1, size(expected)
         call check(error, actual(index), expected(index))
         if (allocated(error)) then
            error%message = 'Expected values at index '//to_string(index)// &
                            ' to be the same. '//error%message
            return
         end if
      end do

   end subroutine check_string_array

   subroutine test_check_integer_array(error)
      type(error_type), allocatable, intent(out) :: error

      type(integer_array_t), dimension(:), allocatable :: actual, expected
      logical, dimension(:), allocatable :: check_should_fail

      integer, save :: cases = 3
      integer :: case

      allocate (actual(cases), expected(cases), check_should_fail(cases))

      case = 1
      ! Sizes are equal, values are equal
      actual(case)%integers = [1, 2]
      expected(case)%integers = [1, 2]
      check_should_fail(case) = .false.

      case = 2
      ! Sizes are not equal
      actual(case)%integers = [1, 2]
      expected(case)%integers = [1]
      check_should_fail(case) = .true.

      case = 3
      ! Sizes are equal, but values not
      actual(case)%integers = [1, 2]
      expected(case)%integers = [1, 3]
      check_should_fail(case) = .true.

      do case = 1, cases
         call check(error, actual(case)%integers, expected(case)%integers)
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
   end subroutine test_check_integer_array

   subroutine check_integer_array(error, actual, expected)
      type(error_type), allocatable, intent(out) :: error
      integer, dimension(:), intent(in) :: actual, expected

      integer :: index

      if (size(actual) /= size(expected)) then
         call test_failed(error, "Array sizes do not match")
         return
      end if

      do index = 1, size(expected)
         call check(error, actual(index), expected(index))
         if (allocated(error)) then
            error%message = 'Expected values at index '//to_string(index)// &
                            ' to be the same. '//error%message
            return
         end if
      end do
   end subroutine check_integer_array

   subroutine assert_check_passed(error, error_from_check)
      type(error_type), allocatable, intent(out) :: error
      type(error_type), allocatable, intent(inout) :: error_from_check

      call check(error, allocated(error_from_check), .false.)

      call prevent_uncaught_error(error_from_check)
   end subroutine assert_check_passed

   subroutine assert_check_failed(error, error_from_check, message_should_contain)
      type(error_type), allocatable, intent(out) :: error
      type(error_type), allocatable, intent(inout) :: error_from_check
      character(len=*), intent(in), optional :: message_should_contain
      integer :: position_of_expected_message

      call check(error, allocated(error_from_check), .true., 'Expected the check to fail')
      call prevent_uncaught_error(error_from_check)

      if (allocated(error)) then
         return
      end if

      if (present(message_should_contain)) then
         position_of_expected_message = index(error_from_check%message, message_should_contain)
         call check(error, position_of_expected_message > 0, .true., &
                    'Expected the error message to contain "'//message_should_contain// &
                    '" but got "'//error_from_check%message//'"')
      end if
   end subroutine assert_check_failed

   subroutine prevent_uncaught_error(error)
      type(error_type), allocatable, intent(inout) :: error
      if (allocated(error)) then
         ! fake success, or the final procedure of error_type
         ! will escalate the error as "uncaught"
         error%stat = 0
      end if
   end subroutine prevent_uncaught_error
end module test_custom_checks
