module stopwatch_tdd_facade
   use iso_fortran_env, only: int64, real64

   implicit none(type, external)

   private

   public :: stopwatch_start
   public :: stopwatch_stop
   public :: stopwatch_result
   public :: make_actual_snapshots
   public :: make_fixed_snapshots
   public :: set_snapshot
   public :: time_snapshot_t

   type :: time_snapshot_t
      integer(kind=int64) :: clock_count
      real(kind=int64) :: clock_rate
      real(kind=real64) :: cpu_time
   end type time_snapshot_t

   type(time_snapshot_t), allocatable :: start_snapshot
   type(time_snapshot_t), allocatable :: stop_snapshot
   type(time_snapshot_t), allocatable :: fixed_snapshot

   interface
      function take_snapshot_proc() result(snapshot)
         import time_snapshot_t

         implicit none(type, external)

         type(time_snapshot_t) :: snapshot
      end function take_snapshot_proc
   end interface

   procedure(take_snapshot_proc), pointer :: take_snapshot
contains

   subroutine make_actual_snapshots()
      take_snapshot => take_actual_snapshot
   end subroutine make_actual_snapshots

   subroutine make_fixed_snapshots()
      take_snapshot => take_fixed_snapshot
   end subroutine make_fixed_snapshots

   subroutine stopwatch_start()
      start_snapshot = take_snapshot()
   end subroutine stopwatch_start

   function take_actual_snapshot() result(snapshot)
      type(time_snapshot_t) :: snapshot

      call system_clock(count=snapshot%clock_count, count_rate=snapshot%clock_rate)

      call cpu_time(snapshot%cpu_time)
   end function take_actual_snapshot

   function take_fixed_snapshot() result(snapshot)
      type(time_snapshot_t) :: snapshot

      snapshot = fixed_snapshot
   end function take_fixed_snapshot

   subroutine set_snapshot(snapshot)
      type(time_snapshot_t), intent(in) :: snapshot
      fixed_snapshot = snapshot
   end subroutine set_snapshot

   subroutine stopwatch_stop()
      stop_snapshot = take_snapshot()
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
