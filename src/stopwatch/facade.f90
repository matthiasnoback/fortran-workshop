
module stopwatch_facade
   implicit none(type, external)

   private
   public :: stopwatch_start, stopwatch_stop, stopwatch_print_timers

   interface
      module subroutine stopwatch_start(label)
         implicit none(type, external)

         character(len=*), intent(in) :: label
      end subroutine stopwatch_start

      module subroutine stopwatch_stop(label)
         implicit none(type, external)

         character(len=*), intent(in) :: label
      end subroutine stopwatch_stop

      module subroutine stopwatch_print_timers()
         implicit none(type, external)

      end subroutine stopwatch_print_timers
   end interface
end module stopwatch_facade
