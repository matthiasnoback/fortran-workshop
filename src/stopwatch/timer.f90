module stopwatch_timer
   use iso_fortran_env, only: wp => real64, int64

   implicit none(type, external)

   private
   public :: timer_t

   type :: timer_t
      character(len=:), allocatable :: label
      integer(int64) :: system_clock_start_time
      integer(int64), allocatable :: system_clock_end_time
   contains
      procedure :: print => timer_print
      procedure :: stop => timer_stop
   end type timer_t

   interface timer_t
      module procedure :: timer_constructor
   end interface

contains
   function timer_constructor(label) result(timer)
      character(len=*), intent(in) :: label
      type(timer_t) :: timer

      integer(kind=int64) :: start_time

      call system_clock(start_time)

      timer = timer_t(label, start_time)
   end function timer_constructor

   subroutine timer_print(self)
      class(timer_t), intent(inout) :: self

      if (allocated(self%system_clock_end_time)) then
         print *, self%label, time_difference_in_seconds( &
            self%system_clock_start_time, &
            self%system_clock_end_time &
            )
      else
         print *, self%label//' (only started)'
      end if
   end subroutine timer_print

   subroutine timer_stop(self)
      class(timer_t), intent(inout) :: self

      integer(kind=int64) :: end_time

      call system_clock(end_time)

      self%system_clock_end_time = end_time
   end subroutine timer_stop

   function time_difference_in_seconds(start_time, end_time) result(difference)
      integer(kind=int64), intent(in) :: start_time
      integer(kind=int64), intent(in) :: end_time
      real(kind=wp) :: difference

      real(kind=int64) :: clock_rate

      call system_clock(count_rate=clock_rate)  ! # of clock ticks per second

      difference = (end_time - start_time)/real(clock_rate, kind=wp)
   end function time_difference_in_seconds

end module stopwatch_timer
