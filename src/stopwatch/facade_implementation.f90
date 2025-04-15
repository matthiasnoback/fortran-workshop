
submodule(stopwatch_facade) stopwatch_facade_implementation
   use stopwatch_timer_list, only: timer_list_t
   use stopwatch_timer, only: timer_t

   implicit none(type, external)

   type(timer_list_t), allocatable :: timer_list

contains

   module subroutine stopwatch_start(label)
      character(len=*), intent(in) :: label

      if (.not. allocated(timer_list)) then
         allocate (timer_list)
      end if

      call timer_list%add(timer_t(label))
   end subroutine stopwatch_start

   module subroutine stopwatch_stop(label)
      character(len=*), intent(in) :: label


      type(timer_t), pointer :: found_timer

      found_timer => timer_list%get(label)
      if (associated(found_timer)) then
         call found_timer%stop()
      end if
   end subroutine stopwatch_stop

   module subroutine stopwatch_print_timers()
      if (allocated(timer_list)) then
         call timer_list%print_all()
      end if
   end subroutine stopwatch_print_timers

end submodule stopwatch_facade_implementation
