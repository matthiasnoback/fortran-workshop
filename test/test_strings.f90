module test_strings
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
   use common_strings, only: string_t, string_list_t, string_length

   implicit none(type, external)

   private

   public :: collect_tests

   interface check
      module procedure :: check_integer_array
   end interface
contains

   subroutine collect_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_string_list_map_to_string_lengths", &
                               test_string_list_map_to_string_lengths) &
                  ]
   end subroutine collect_tests

   subroutine test_string_list_map_to_string_lengths(error)
      type(error_type), allocatable, intent(out) :: error

      type(string_list_t), allocatable :: list
      integer, dimension(:), allocatable :: lengths

      list = string_list_t([string_t('a'), string_t('abc'), string_t('ab'), string_t('abcd')])
      lengths = list%map(string_length)

      call check(error, lengths, [1, 3, 2, 4])
   end subroutine test_string_list_map_to_string_lengths

   subroutine check_integer_array(error, actual, expected)
      type(error_type), allocatable, intent(out) :: error
      integer, dimension(:), intent(in) :: actual, expected

      if (size(actual) /= size(expected)) then
         call test_failed(error, "Array sizes do not match")
         return
      end if

      if (any(actual /= expected)) then
         call test_failed(error, "Array contents do not match")
         return
      end if
   end subroutine check_integer_array

end module test_strings
