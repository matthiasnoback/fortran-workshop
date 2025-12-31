module test_file_writer
   use testdrive, only: new_unittest, unittest_type, error_type, test_failed, check
   use common_strings, only: string_t, string_list_t, string_or_error_t
   use common_error_handling, only: optional_error_t, some_error_t
   use file_writer, only: create_in_memory_writer, in_memory_writer_t
   use test_strings, only: check

   implicit none(type, external)

   private

   public :: collect_tests

contains

   subroutine collect_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_write_lines_in_memory", &
                               test_write_lines_in_memory) &
                  ]
   end subroutine collect_tests

   subroutine test_write_lines_in_memory(error)
      type(error_type), allocatable, intent(out) :: error

      type(in_memory_writer_t) :: writer
      class(optional_error_t), allocatable :: optional_error

      writer = create_in_memory_writer()

      optional_error = writer%write_line('foo')
      select type (optional_error)
      type is (some_error_t)
         call test_failed(error, 'Expected no error')
         return
      end select

      optional_error = writer%write_line('ba')
      select type (optional_error)
      type is (some_error_t)
         call test_failed(error, 'Expected no error')
         return
      end select

      call check(error, writer%lines, string_list_t([ &
                                                    string_t('foo'), &
                                                    string_t('ba') &
                                                    ]))

   end subroutine test_write_lines_in_memory

end module test_file_writer
