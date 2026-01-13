module test_rainfall_runoff
   use iso_fortran_env, only: int64, real64
   use testdrive, only: new_unittest, unittest_type, error_type, test_failed, skip_test, check
   use hydrology_rainfall_runoff, only: run, inFile, outFile
   use file_operations, only: create_or_open_file, &
                              file_unit_or_error_t, &
                              write_lines_to_file, &
                              read_all_lines, &
                              delete_file_if_exists, &
                              open_existing_file_for_reading
   use common_strings, only: string_list_t, string_t, string_list_or_error_t
   use common_error_handling, only: optional_error_t, some_error_t, no_error_t
   use test_strings, only: check
   use test_error_handling, only: check_no_error

   implicit none(type, external)

   private

   public :: collect_tests

contains

   subroutine collect_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_characterization_test", &
                               test_characterization_test) &
                  ]
   end subroutine collect_tests

   subroutine test_characterization_test(error)
      type(error_type), allocatable, intent(out) :: error

      type(string_list_t), allocatable :: input
      type(string_list_t), allocatable :: expected

      class(optional_error_t), allocatable :: file_deleted

      file_deleted = delete_file_if_exists('output.csv')
      call check_no_error(error, file_deleted)

      ! Previously used input
      input = string_list_t([ &
                            string_t('date,precipitation,air temperature'), &
                            string_t('01-10-2025,5,20'), &
                            string_t('02-10-2025,30,21'), &
                            string_t('03-10-2025,40,10'), &
                            string_t('04-10-2025,50,5'), &
                            string_t('05-10-2025,60,-5'), &
                            string_t('06-10-2025,10,-10')])

      call write_input_csv(error, 'input.csv', input)
      if (allocated(error)) then
         return
      end if

      ! Previously recorded output that is correct
      expected = string_list_t([ &
                               string_t('date,P,T,PET,AET,Q,S'), &
                               string_t('01-10-2025,5.000,20.000,20.000,20.000,.000,75.000'), &
                               string_t('02-10-2025,30.000,21.000,20.800,20.800,.000,84.200'), &
                               string_t('03-10-2025,40.000,10.000,12.000,12.000,.000,112.200'), &
                               string_t('04-10-2025,50.000,5.000,8.000,8.000,12.200,142.000'), &
                               string_t('05-10-2025,60.000,-5.000,.000,.000,52.000,150.000'), &
                               string_t('06-10-2025,10.000,-10.000,.000,.000,10.000,150.000') &
                               ])

      ! TODO now run the rainfall_runoff simulation, passing 'input.csv' and 'output.csv' as arguments
      inFile = 'input.csv'
      outFile = 'output.csv'
      call run()

      call check_output(error, 'output.csv', expected)

   end subroutine test_characterization_test

   subroutine write_input_csv(error, path, lines)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), intent(in) :: path
      type(string_list_t), intent(in) :: lines

      type(file_unit_or_error_t) :: input_file
      class(optional_error_t), allocatable :: lines_written

      input_file = create_or_open_file(path)
      if (allocated(input_file%error)) then
         call test_failed(error, 'Expected no error, got '//input_file%error%to_string())
         return
      end if

      lines_written = write_lines_to_file(input_file%file_unit, &
                                          lines)
      close (input_file%file_unit)

      call check_no_error(error, lines_written)
      if (allocated(error)) then
         return
      end if
   end subroutine write_input_csv

   subroutine check_output(error, path, expected)
      type(error_type), allocatable, intent(out) :: error
      character(len=*), intent(in) :: path
      type(string_list_t), intent(in) :: expected

      type(file_unit_or_error_t) :: output_file
      type(string_list_or_error_t) :: actual

      output_file = open_existing_file_for_reading(path)
      if (allocated(output_file%error)) then
         call test_failed(error, 'Expected no error, got '//output_file%error%to_string())
         return
      end if

      actual = read_all_lines(output_file%file_unit)
      if (allocated(actual%error)) then
         call test_failed(error, 'Expected no error, got '//actual%error%to_string())
         return
      end if

      call check(error, actual%lines, expected)
   end subroutine check_output

end module test_rainfall_runoff
