module stopwatch_tdd_facade
   use iso_fortran_env, only: int64, real64

   implicit none(type, external)

   private

   public :: stopwatch_start
   public :: stopwatch_stop
   public :: stopwatch_print

   integer(kind=int64) :: time_start
   integer(kind=int64) :: time_stop
   real(kind=real64) :: cpu_time_start
   real(kind=real64) :: cpu_time_stop

contains

   subroutine stopwatch_start()
      call cpu_time(cpu_time_start)
      call system_clock(count=time_start)
   end subroutine stopwatch_start

   subroutine stopwatch_stop()
      call cpu_time(cpu_time_stop)
      call system_clock(count=time_stop)
   end subroutine stopwatch_stop

   subroutine stopwatch_print()
      real(kind=int64) :: clock_rate
      real(kind=real64) :: wall_clock_time_difference
      real(kind=real64) :: cpu_time_difference
      real(kind=real64) :: time_difference

      call system_clock(count_rate=clock_rate)  ! # of clock ticks per second

      wall_clock_time_difference = (time_stop - time_start)/clock_rate
      print '(a30,f15.8,a)', 'wall clock time difference: ', wall_clock_time_difference

      cpu_time_difference = cpu_time_stop - cpu_time_start
      print '(a30,f15.8)', 'cpu time difference: ', cpu_time_difference
   end subroutine stopwatch_print
end module stopwatch_tdd_facade
