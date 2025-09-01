module test_config
   use testdrive, only: new_unittest, unittest_type, error_type, test_failed, skip_test
   use config_loading, only: config_value_t, real_or_error_t, &
                             configuration_t, config_value_or_error_t, load_config_from_file, &
                             configuration_or_error_t
   use common_precision, only: dp
   use common_error_handling, only: error_t, optional_error_t, some_error_t, no_error_t
   use common_strings, only: string_t, string_list_t
   use file_operations, only: file_unit_or_error_t, write_lines_to_temp_file
   use test_custom_checks, only: check

   implicit none(type, external)

   private

   public :: collect_tests

   interface check
      procedure :: check_configuration
   end interface check

contains

   subroutine collect_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_get_config_values_from_file", &
                               test_get_config_values_from_file), &
                  new_unittest("test_get_config_value_as_real", &
                               test_get_config_value_as_real), &
                  new_unittest("test_get_config_value_as_string_removes_double_quotes", &
                               test_get_config_value_as_string_removes_double_quotes), &
                  new_unittest("test_no_config_values", &
                               test_get_config_value), &
                  new_unittest("test_get_non_existing_config_value", &
                               test_get_non_existing_config_value), &
                  new_unittest("test_get_config_value_as_real_failure", &
                               test_get_config_value_as_real_failure) &
                  ]
   end subroutine collect_tests

   subroutine test_get_config_value(error)
      type(error_type), allocatable, intent(out) :: error

      type(configuration_t), allocatable :: configuration
      type(config_value_t), allocatable :: expected
      type(config_value_or_error_t), allocatable :: actual

      expected = config_value_t('a key', 'a string value')

      configuration = configuration_t([expected])

      actual = configuration%get_config_value('a key')

      call check(error, allocated(actual%config_value))
      call check(error, actual%config_value%key, expected%key)
   end subroutine test_get_config_value

   subroutine test_get_non_existing_config_value(error)
      type(error_type), allocatable, intent(out) :: error

      type(configuration_t), allocatable :: configuration
      type(config_value_t), allocatable :: expected
      type(config_value_or_error_t), allocatable :: actual

      expected = config_value_t('a key', 'a string value')

      configuration = configuration_t([expected])

      actual = configuration%get_config_value('does not exist')

      call check(error, allocated(actual%error))
      call check(error, actual%error%message, &
                 'Could not find a configuration value with key "does not exist"')
   end subroutine test_get_non_existing_config_value

   subroutine test_get_config_value_as_real(error)
      type(error_type), allocatable, intent(out) :: error

      real(kind=dp) :: real_value
      type(config_value_t) :: config_value
      type(real_or_error_t), allocatable :: real_or_error

      config_value%string_value = '1.5'

      real_or_error = config_value%get_real()

      call check(error, allocated(real_or_error%value), .true.)
      call check(error, real_or_error%value, 1.5_dp)
   end subroutine test_get_config_value_as_real

   subroutine test_get_config_value_as_string_removes_double_quotes(error)
      type(error_type), allocatable, intent(out) :: error

      type(config_value_t) :: config_value

      config_value%string_value = '"Quoted string"'

      call check(error, config_value%get_string(), 'Quoted string')
   end subroutine test_get_config_value_as_string_removes_double_quotes

   subroutine test_get_config_value_as_real_failure(error)
      type(error_type), allocatable, intent(out) :: error

      type(config_value_t) :: config_value
      type(real_or_error_t), allocatable :: real_or_error

      config_value%string_value = 'abc'

      real_or_error = config_value%get_real()

      call check(error, allocated(real_or_error%error), .true.)
      call check(error, real_or_error%error%message, 'Could not parse "abc" as a real')
   end subroutine test_get_config_value_as_real_failure

   subroutine test_get_config_values_from_file(error)
      type(error_type), allocatable, intent(out) :: error

      type(file_unit_or_error_t) :: file_unit_or_error
      class(configuration_or_error_t), allocatable :: configuration_or_error
      type(configuration_t), allocatable :: expected
      type(string_list_t), allocatable :: lines

      lines = string_list_t([ &
                            string_t('name = "Nice pump"'), &
                            string_t('capacity = 15.0')])

      file_unit_or_error = write_lines_to_temp_file(lines)
      if (allocated(file_unit_or_error%error)) then
         call test_failed(error, file_unit_or_error%error%to_string())
         return
      end if

      configuration_or_error = load_config_from_file(file_unit_or_error%file_unit)

      call check(error, allocated(configuration_or_error%error), .false., 'Expected no error')
      if (allocated(error)) then
         return
      end if

      expected = configuration_t([config_value_t('name', '"Nice pump"'), &
                                  config_value_t('capacity', '15.0')])
      call check(error, configuration_or_error%configuration, expected)
   end subroutine test_get_config_values_from_file

   subroutine check_configuration(error, actual, expected)
      type(error_type), allocatable, intent(out) :: error
      type(configuration_t), intent(in) :: actual
      type(configuration_t), intent(in) :: expected

      integer :: index

      call check(error, size(actual%values), size(expected%values), &
                 'Number of values is not the same')
      if (allocated(error)) then
         return
      end if

      do index = 1, size(actual%values)
         call check(error, actual%values(index)%key, expected%values(index)%key, &
                    'Key is not the same: '//actual%values(index)%key// &
                    ' /= '//expected%values(index)%key)
         if (allocated(error)) then
            return
         end if

         call check(error, actual%values(index)%string_value, expected%values(index)%string_value, &
                    'Value is not the same: '//actual%values(index)%string_value// &
                    ' /= '//expected%values(index)%string_value)
         if (allocated(error)) then
            return
         end if
      end do
   end subroutine check_configuration

end module test_config
