module logging_aggregation
   use logging_abstract, only: abstract_logger_t

   implicit none(type, external)

   private
   public :: multi_logger_t, logger_reference_t

   type :: logger_reference_t
      class(abstract_logger_t), pointer :: logger => null()
   end type logger_reference_t

   type, extends(abstract_logger_t) :: multi_logger_t
      type(logger_reference_t), allocatable, dimension(:) :: logger_references
   contains
      procedure :: log => multi_logger_log
   end type multi_logger_t

contains

   subroutine multi_logger_log(self, message)
      class(multi_logger_t), intent(inout) :: self
      character(len=*), intent(in) :: message

      integer :: i

      do i = 1, size(self%logger_references)
         call self%logger_references(i)%logger%log(message)
      end do
   end subroutine multi_logger_log

end module logging_aggregation
