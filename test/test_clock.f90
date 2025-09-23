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
                               test_production_clock_get_cpu_time) &
                  ]
   end subroutine collect_tests

   subroutine test_production_clock_get_count(error)
      type(error_type), allocatable, intent(out) :: error

      integer(kind=int64) :: count_1, count_2, res
      count_1 = production_clock%get_count()

      call takes_some_time()

      count_2 = production_clock%get_count()

      call check(error, count_2 - count_1 > 0_int64, .true.)
   end subroutine test_production_clock_get_count

   subroutine test_production_clock_get_cpu_time(error)
      type(error_type), allocatable, intent(out) :: error

      real(kind=real64) :: cpu_time_1, cpu_time_2

      cpu_time_1 = production_clock%get_cpu_time()

      call takes_some_time()

      cpu_time_2 = production_clock%get_cpu_time()

      call check(error, (cpu_time_2 - cpu_time_1) > 0.0_real64, .true.)
   end subroutine test_production_clock_get_cpu_time

   subroutine takes_some_time()
      integer :: i, j, count, n
      integer :: start_clock, end_clock, rate
      logical :: is_prime

      n = 100000  ! Adjust for more/less CPU time

      count = 0
      do i = 2, n
         is_prime = .true.
         do j = 2, i - 1
            if (mod(i, j) == 0) then
               is_prime = .false.
               exit
            end if
         end do
         if (is_prime) count = count + 1
      end do

   end subroutine takes_some_time
end module test_clock
