module benchmark_diagnostics
   use iso_fortran_env, only: real64, int64

   implicit none(type, external)

   private
   public :: get_snapshot
   public :: diagnostics_snapshot_t
   public :: get_clock_rate

   public :: override_snapshot_for_testing
   public :: override_clock_rate_for_testing

   type diagnostics_snapshot_t
      real(kind=real64) :: cpu_time
      integer(kind=int64) :: clock_time
   end type diagnostics_snapshot_t

   type(diagnostics_snapshot_t), pointer :: snapshot_for_testing
   real(kind=real64), allocatable :: fixed_clock_rate

contains

   function get_snapshot() result(snapshot)
      type(diagnostics_snapshot_t) :: snapshot

      if (associated(snapshot_for_testing)) then
         snapshot = snapshot_for_testing
         return
      end if

      snapshot = diagnostics_snapshot_t(get_cpu_time(), get_clock_time())
   end function get_snapshot

   subroutine override_snapshot_for_testing(snapshot)
      type(diagnostics_snapshot_t), pointer, intent(in) :: snapshot

      snapshot_for_testing => snapshot
   end subroutine override_snapshot_for_testing

   subroutine override_clock_rate_for_testing(clock_rate)
      real(kind=real64), intent(in) :: clock_rate

      fixed_clock_rate = clock_rate
   end subroutine override_clock_rate_for_testing

   function get_cpu_time() result(time)
      real(kind=real64) :: time

      ! Default implementation
      call cpu_time(time)
   end function get_cpu_time

   function get_clock_time() result(time)
      integer(kind=int64) :: time

      call system_clock(time)
   end function get_clock_time

   function get_clock_rate() result(clock_rate)
      real(kind=int64) :: clock_rate

      if (allocated(fixed_clock_rate)) then
         clock_rate = fixed_clock_rate
         return
      end if

      call system_clock(count_rate=clock_rate) ! # of clock ticks per second
   end function get_clock_rate

end module benchmark_diagnostics
