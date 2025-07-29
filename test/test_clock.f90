module test_clock
   use iso_fortran_env, only: int64, real64
   use testdrive, only: new_unittest, unittest_type, error_type, test_failed, skip_test
   use common_clock, only: production_clock
   use test_custom_checks, only: check

   implicit none(type, external)

   private

   public :: collect_tests

contains

   subroutine collect_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_production_clock_get_cpu_time", &
                               test_production_clock_get_cpu_time), &
                  new_unittest("test_production_clock_get_count", &
                               test_production_clock_get_count) &
                  ]
   end subroutine collect_tests

   subroutine test_production_clock_get_count(error)
      type(error_type), allocatable, intent(out) :: error

      integer(kind=int64) :: count_1, count_2, res
      count_1 = production_clock%get_count()

      res = takes_some_time()

      count_2 = production_clock%get_count()

      call check(error, count_2 - count_1 > 0, .true.)
   end subroutine test_production_clock_get_count

   subroutine test_production_clock_get_cpu_time(error)
      type(error_type), allocatable, intent(out) :: error

      real(kind=real64) :: cpu_time_1, cpu_time_2, res

      cpu_time_1 = production_clock%get_cpu_time()

      res = takes_some_time()

      cpu_time_2 = production_clock%get_cpu_time()

      call check(error, cpu_time_2 - cpu_time_1 > 0.0_real64, .true.)
   end subroutine test_production_clock_get_cpu_time

   function takes_some_time() result(sum)
      integer :: i
      integer :: sum

      sum = 0
      do i = 1, 1000
         sum = sum + sqrt(real(i))
      end do
   end function takes_some_time
end module test_clock
