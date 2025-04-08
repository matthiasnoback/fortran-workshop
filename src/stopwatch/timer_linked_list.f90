module stopwatch_timer_linked_list
   use stopwatch_timer, only: timer_t

   implicit none(type, external)

   private
   public :: timer_linked_list_t

   type :: timer_linked_list_node_t
      type(timer_t) :: timer
      type(timer_linked_list_node_t), pointer :: next => null()
   end type timer_linked_list_node_t

   type :: timer_linked_list_t
      type(timer_linked_list_node_t), pointer :: head => null()
   contains
      procedure :: add => timer_linked_list_add
      procedure :: get => timer_linked_list_get
      procedure :: print_all => timer_linked_list_print_all
   end type timer_linked_list_t

contains

   subroutine timer_linked_list_add(self, timer)
      class(timer_linked_list_t), intent(inout) :: self
      type(timer_linked_list_node_t), pointer :: new_node
      type(timer_t), intent(in) :: timer

      ! TODO if this would be the first node
      !    - allocate a `head` node
      !    - assign the timer to its timer component
      ! TODO else
      !    - create a new node
      !    - assign the timer to its timer component
      !    - let `next` point to the current `head`
      !    - let `head` point to the new node
   end subroutine timer_linked_list_add

   function timer_linked_list_get(self, label) result(timer)
      class(timer_linked_list_t), intent(inout), target :: self
      character(len=*), intent(in) :: label
      type(timer_t), pointer :: timer

      type(timer_linked_list_node_t), pointer :: current

      current => self%head

      do while (associated(current))
         ! TODO return the timer if its label matches the provided label

         current => current%next
      end do

      timer => null()
   end function timer_linked_list_get

   subroutine timer_linked_list_print_all(self)
      class(timer_linked_list_t), intent(inout) :: self

      type(timer_linked_list_node_t), pointer :: current

      ! TODO loop through the timer nodes and print each of them
   end subroutine timer_linked_list_print_all

end module stopwatch_timer_linked_list
