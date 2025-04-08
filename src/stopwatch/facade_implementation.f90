
submodule(stopwatch_facade) stopwatch_facade_implementation
   use stopwatch_timer_list, only: timer_list_t
   use stopwatch_timer, only: timer_t

   implicit none(type, external)

   type(timer_list_t), allocatable :: timer_list

contains

   module subroutine stopwatch_start(label)
      character(len=*), intent(in) :: label

      ! TODO start a timer with the provided label
      ! TODO store the timer in the timer_list module variable
   end subroutine stopwatch_start

   module subroutine stopwatch_stop(label)
      character(len=*), intent(in) :: label

      ! TODO find the timer by its label and end it
   end subroutine stopwatch_stop

   module subroutine stopwatch_print_timers()
      ! TODO print timers
   end subroutine stopwatch_print_timers

end submodule stopwatch_facade_implementation
