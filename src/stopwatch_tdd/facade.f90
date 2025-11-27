module stopwatch_tdd_facade
   use iso_fortran_env, only: int64, real64
   use common_clock, only: clock

   implicit none(type, external)

   private

   public :: stopwatch_start
   public :: stopwatch_stop
   public :: stopwatch_result
   public :: time_snapshot_t

   type :: time_snapshot_t
      integer(kind=int64) :: clock_count
      real(kind=int64) :: clock_rate
      real(kind=real64) :: cpu_time
   end type time_snapshot_t

   type(time_snapshot_t), allocatable :: start_snapshot
   type(time_snapshot_t), allocatable :: stop_snapshot

contains
   subroutine stopwatch_start()
      start_snapshot = time_snapshot_t(clock%get_count(), clock%get_rate(), clock%get_cpu_time())
   end subroutine stopwatch_start

   subroutine stopwatch_stop()
      stop_snapshot = time_snapshot_t(clock%get_count(), clock%get_rate(), clock%get_cpu_time())
   end subroutine stopwatch_stop

   function stopwatch_result() result(timings)
      real(kind=real64) :: wall_clock_time_difference
      real(kind=real64) :: cpu_time_difference
      character(len=45), dimension(2) :: timings

      wall_clock_time_difference = (stop_snapshot%clock_count - start_snapshot%clock_count)/ &
                                   stop_snapshot%clock_rate
      write (timings(1), '(a30,f15.6)') 'wall clock time difference: ', wall_clock_time_difference

      cpu_time_difference = stop_snapshot%cpu_time - start_snapshot%cpu_time
      write (timings(2), '(a30,f15.6)') 'cpu time difference: ', cpu_time_difference
   end function stopwatch_result
end module stopwatch_tdd_facade
