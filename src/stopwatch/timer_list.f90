module stopwatch_timer_list
   use stopwatch_timer, only: timer_t

   implicit none(type, external)

   private
   public :: timer_list_t

   type :: timer_list_t
      type(timer_t), dimension(:), allocatable :: timers
   contains
      procedure :: add => timer_list_add
      procedure :: get => timer_list_get
      procedure :: print_all => timer_list_print_all
   end type timer_list_t

contains

   subroutine timer_list_add(self, timer)
      class(timer_list_t), intent(inout) :: self
      type(timer_t), intent(in) :: timer

      if (allocated(self%timers)) then
         self%timers = [self%timers, timer]
      else
         self%timers = [timer]
      endif
   end subroutine timer_list_add

   subroutine timer_list_print_all(self)
      class(timer_list_t), intent(inout) :: self

      integer :: i

      if (.not. allocated(self%timers)) then
         return
      end if

      do i = 1, size(self%timers)
         call self%timers(i)%print()
      end do
   end subroutine timer_list_print_all

   function timer_list_get(self, label) result(timer)
      class(timer_list_t), intent(inout), target :: self
      character(len=*), intent(in) :: label
      type(timer_t), pointer :: timer

      integer :: i

      if (allocated(self%timers)) then
         do i = 1, size(self%timers)
            if (self%timers(i)%label == label) then
               timer => self%timers(i)
               return
            end if
         end do
      end if

      timer => null()
   end function timer_list_get

end module stopwatch_timer_list
