module test_file_reader
   use testdrive, only: new_unittest, unittest_type, error_type, test_failed, check
   use common_strings, only: string_t, string_list_t, string_or_error_t
   use file_reader, only: create_in_memory_reader, in_memory_reader_t

   implicit none(type, external)

   private

   public :: collect_tests

   interface check
      procedure :: check_string_and_string_or_error
   end interface check
contains

   subroutine collect_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_read_lines_from_memory", &
                               test_read_lines_from_memory) &
                  ]
   end subroutine collect_tests

   subroutine test_read_lines_from_memory(error)
      type(error_type), allocatable, intent(out) :: error

      type(in_memory_reader_t) :: reader
      type(string_or_error_t) :: line

      reader = create_in_memory_reader(string_list_t([ &
                                                     string_t('foo'), &
                                                     string_t('ba') &
                                                     ]))

      line = reader%read_line()
      call check(error, line, 'foo')
      if (allocated(error)) then
         return
      end if

      line = reader%read_line()
      call check(error, line, 'ba')
      if (allocated(error)) then
         return
      end if

      line = reader%read_line()
      call check(error, allocated(line%error), .true.)

   end subroutine test_read_lines_from_memory

   subroutine check_string_and_string_or_error(error, actual, expected)
      type(error_type), allocatable, intent(out) :: error

      type(string_or_error_t), intent(in) :: actual
      character(len=*), intent(in) :: expected

      call check(error, allocated(actual%error), .false., 'Did not expect an error')
      if (allocated(error)) then
         return
      end if

      call check(error, actual%string%value, expected)
   end subroutine check_string_and_string_or_error

end module test_file_reader
