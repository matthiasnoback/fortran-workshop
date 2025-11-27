module common_clock
   use iso_fortran_env, only: int64, real64

   implicit none(type, external)

   private

   public :: clock_t
   public :: clock
   public :: production_clock
   public :: use_test_clock

   type, abstract :: clock_t
   contains
      procedure(clock_get_count_interface), deferred :: get_count
      procedure(clock_get_rate_interface), deferred :: get_rate
      procedure(clock_get_cpu_time_interface), deferred :: get_cpu_time
   end type clock_t

   interface
      function clock_get_count_interface(self) result(count)
         import int64, clock_t

         implicit none(type, external)

         class(clock_t), intent(in) :: self
         integer(kind=int64) :: count
      end function clock_get_count_interface

      function clock_get_rate_interface(self) result(rate)
         import real64, clock_t

         implicit none(type, external)

         class(clock_t), intent(in) :: self
         integer(kind=real64) :: rate
      end function clock_get_rate_interface

      function clock_get_cpu_time_interface(self) result(time)
         import real64, clock_t

         implicit none(type, external)

         class(clock_t), intent(in) :: self
         real(kind=real64) :: time
      end function clock_get_cpu_time_interface
   end interface

   type, extends(clock_t) :: production_clock_t
   contains
      procedure :: get_count => production_clock_get_count
      procedure :: get_rate => production_clock_get_rate
      procedure :: get_cpu_time => production_clock_get_cpu_time
   end type production_clock_t

   type, extends(clock_t) :: test_clock_t
      integer(kind=int64) :: clock_count
      integer(kind=int64) :: clock_rate
      real(kind=real64) :: cpu_time
   contains
      procedure :: get_count => test_clock_get_count
      procedure :: get_rate => test_clock_get_rate
      procedure :: get_cpu_time => test_clock_get_cpu_time
   end type test_clock_t

   type(production_clock_t), target :: production_clock
   type(test_clock_t), target, allocatable :: test_clock
   class(clock_t), pointer :: clock => production_clock

contains

   subroutine use_test_clock(clock_count, clock_rate, cpu_time)
      integer(kind=int64), intent(in) :: clock_count
      integer(kind=int64), intent(in) :: clock_rate
      real(kind=real64), intent(in) :: cpu_time

      test_clock = test_clock_t(clock_count, clock_rate, cpu_time)

      clock => test_clock
   end subroutine use_test_clock

   function production_clock_get_count(self) result(count)
      class(production_clock_t), intent(in) :: self
      integer(kind=int64) :: count

      call system_clock(count=count)
   end function production_clock_get_count

   function production_clock_get_rate(self) result(rate)
      class(production_clock_t), intent(in) :: self
      integer(kind=real64) :: rate

      call system_clock(count_rate=rate)
   end function production_clock_get_rate

   function production_clock_get_cpu_time(self) result(time)
      class(production_clock_t), intent(in) :: self
      real(kind=real64) :: time

      call cpu_time(time)
   end function production_clock_get_cpu_time

   function test_clock_get_count(self) result(count)
      class(test_clock_t), intent(in) :: self
      integer(kind=int64) :: count

      count = self%clock_count
   end function test_clock_get_count

   function test_clock_get_rate(self) result(rate)
      class(test_clock_t), intent(in) :: self
      integer(kind=real64) :: rate

      rate = self%clock_rate
   end function test_clock_get_rate

   function test_clock_get_cpu_time(self) result(time)
      class(test_clock_t), intent(in) :: self
      real(kind=real64) :: time

      time = self%cpu_time
   end function test_clock_get_cpu_time

end module common_clock
