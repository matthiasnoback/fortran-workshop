module test_strings
   use testdrive, only: new_unittest, unittest_type, error_type, test_failed
   use common_strings, only: string_t, string_list_t, string_length, empty_string_list
   use common_to_string, only: to_string
   use test_custom_checks, only: check

   implicit none(type, external)

   private

   public :: collect_tests
   public :: check

   interface check
      module procedure :: check_string_list
   end interface
contains

   subroutine collect_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_string_list_add", &
                               test_string_list_add), &
                  new_unittest("test_string_list_map_to_string_lengths", &
                               test_string_list_map_to_string_lengths) &
                  ]
   end subroutine collect_tests

   subroutine test_string_list_add(error)
      type(error_type), allocatable, intent(out) :: error

      type(string_list_t) :: actual
      type(string_list_t), allocatable :: expected

      actual = empty_string_list()

      call actual%add('string 1')
      call actual%add('string 2')

      expected = string_list_t([string_t('string 1'), string_t('string 2')])

      call check(error, actual, expected)
   end subroutine test_string_list_add

   subroutine test_string_list_map_to_string_lengths(error)
      type(error_type), allocatable, intent(out) :: error

      type(string_list_t), allocatable :: list
      integer, dimension(:), allocatable :: lengths

      list = string_list_t([string_t('a'), string_t('abc'), string_t('ab'), string_t('abcd')])
      lengths = list%map(string_length)

      call check(error, lengths, [1, 3, 2, 4])
   end subroutine test_string_list_map_to_string_lengths

   subroutine check_string_list(error, actual, expected)
      type(error_type), allocatable, intent(out) :: error
      type(string_list_t), intent(in) :: actual
      type(string_list_t), intent(in) :: expected

      integer :: index

      call check(error, size(actual%strings), size(expected%strings), &
                 'Size of strings array does not match: ' &
                 //to_string(size(actual%strings))//' /= '//to_string(size(expected%strings)))
      if (allocated(error)) then
         return
      end if

      do index = 1, size(actual%strings)
         call check(error, actual%strings(index)%value, expected%strings(index)%value, &
                    'String value does not match: ' &
                    //actual%strings(index)%value//' /= '//expected%strings(index)%value)
         if (allocated(error)) then
            return
         end if
      end do

   end subroutine check_string_list

end module test_strings
