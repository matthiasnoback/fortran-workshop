program example
   use stopwatch_timer_list, only: timer_list_t
   use stopwatch_timer, only: timer_t

   implicit none(type, external)

   type(timer_list_t) :: timer_list
   type(timer_t) :: timer1
   type(timer_t) :: timer2
   type(timer_t), pointer :: found_timer

   timer1 = timer_t('Column-first')
   timer2 = timer_t('Row-first')

   call timer_list%add(timer1)
   call timer_list%add(timer2)

   found_timer => timer_list%get('Column-first')
   if (associated(found_timer)) then
      call found_timer%stop()
   end if

   call timer_list%print_all()

end program example
