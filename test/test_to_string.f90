module test_to_string
   use testdrive, only: new_unittest, unittest_type, error_type, test_failed
   use test_custom_checks, only: check
   use common_to_string, only: to_string, to_string_t
   use iso_fortran_env, only: real32

   implicit none(type, external)

   private

   public :: collect_tests

   type, extends(to_string_t) :: extends_to_string_t
      character(len=:), allocatable :: str1
      character(len=:), allocatable :: str2
   contains
      procedure, public :: to_string => to_string_implementation
   end type extends_to_string_t

contains

   subroutine collect_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_int_to_string", test_int_to_string), &
                  new_unittest("test_int_array_to_string", test_int_array_to_string), &
                  new_unittest("test_real32_to_string", test_real32_to_string), &
                  new_unittest("test_real32_array_to_string", test_real32_array_to_string), &
                  new_unittest("test_logical_to_string", test_logical_to_string), &
                  new_unittest("test_logical_array_to_string", test_logical_array_to_string), &
                  new_unittest("test_derived_type_to_string", test_derived_type_to_string), &
                  new_unittest("test_write_dt_to_string", test_write_dt_to_string) &
                  ]
   end subroutine collect_tests

   subroutine test_int_to_string(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, to_string(1), '1')
      call check(error, to_string(10000), '10000')
   end subroutine test_int_to_string

   subroutine test_int_array_to_string(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, to_string([1, 2]), '1, 2')
   end subroutine test_int_array_to_string

   subroutine test_real32_to_string(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, to_string(1.0_real32), '1.000000')
      if (allocated(error)) then
         return
      end if
      call check(error, to_string(12.123421_real32), '12.123421')
   end subroutine test_real32_to_string

   subroutine test_real32_array_to_string(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, to_string([1.0, 2.0]), '1.000000, 2.000000')
   end subroutine test_real32_array_to_string

   subroutine test_logical_to_string(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, to_string(.true.), 'T')
      call check(error, to_string(.false.), 'F')
   end subroutine test_logical_to_string

   subroutine test_logical_array_to_string(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, to_string([.true., .false.]), 'T, F')
   end subroutine test_logical_array_to_string

   subroutine test_derived_type_to_string(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, to_string(extends_to_string_t('foo', 'bar')), 'foo and bar')
   end subroutine test_derived_type_to_string

   subroutine test_write_dt_to_string(error)
      type(error_type), allocatable, intent(out) :: error

      character(len=100) :: str
      write (str, *) extends_to_string_t('foo', 'bar')

      call check(error, trim(adjustl(str)), 'foo and bar')
   end subroutine test_write_dt_to_string

   pure function to_string_implementation(self) result(res)
      class(extends_to_string_t), intent(in) :: self
      character(len=:), allocatable :: res

      res = self%str1//' and '//self%str2
   end function to_string_implementation

end module test_to_string
