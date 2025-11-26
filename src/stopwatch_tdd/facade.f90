module stopwatch_tdd_facade
   use iso_fortran_env, only: int64, real64

   implicit none(type, external)

   private

   public :: stopwatch_start
   public :: stopwatch_stop
   public :: stopwatch_result
   public :: set_clock_count

   integer(kind=int64) :: time_start
   integer(kind=int64) :: time_stop
   integer(kind=int64), allocatable :: fixed_clock_count
   real(kind=real64) :: cpu_time_start
   real(kind=real64) :: cpu_time_stop

contains

   subroutine set_clock_count(clock_count)
      integer(kind=int64), intent(in), optional :: clock_count

      if (.not. present(clock_count)) then
         if (allocated(fixed_clock_count)) then
            deallocate (fixed_clock_count)
         end if
         return
      end if

      fixed_clock_count = clock_count
   end subroutine set_clock_count

   function get_clock_count() result(clock_count)
      integer(kind=int64) :: clock_count

      if (allocated(fixed_clock_count)) then
         clock_count = fixed_clock_count
      else
         call system_clock(count=clock_count)
      end if
   end function get_clock_count

   subroutine stopwatch_start()
      call cpu_time(cpu_time_start)
      time_start = get_clock_count()
   end subroutine stopwatch_start

   subroutine stopwatch_stop()
      call cpu_time(cpu_time_stop)
      time_stop = get_clock_count()
   end subroutine stopwatch_stop

   function stopwatch_result() result(timings)
      real(kind=int64) :: clock_rate
      real(kind=real64) :: wall_clock_time_difference
      real(kind=real64) :: cpu_time_difference
      real(kind=real64) :: time_difference
      character(len=45), dimension(2) :: timings

      call system_clock(count_rate=clock_rate)  ! # of clock ticks per second

      wall_clock_time_difference = (time_stop - time_start)/clock_rate
      write (timings(1), '(a30,f15.6)') 'wall clock time difference: ', wall_clock_time_difference

      cpu_time_difference = cpu_time_stop - cpu_time_start
      write (timings(2), '(a30,f15.6)') 'cpu time difference: ', cpu_time_difference
   end function stopwatch_result
end module stopwatch_tdd_facade
