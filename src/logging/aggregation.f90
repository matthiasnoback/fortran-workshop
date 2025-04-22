module logging_aggregation
   use logging_abstract, only: abstract_logger_t

   implicit none(type, external)

   private

   type :: logger_reference_t
      class(abstract_logger_t), pointer :: logger
   end type logger_reference_t

   type, extends(abstract_logger_t) :: multi_logger_t
      type(logger_reference_t), allocatable, dimension(:) :: logger_references
   contains
      procedure :: log => multi_logger_log
      procedure :: add_logger => multi_logger_add_logger
   end type multi_logger_t

contains

   subroutine multi_logger_add_logger(self, logger)
      class(multi_logger_t), intent(inout) :: self
      class(abstract_logger_t), pointer, intent(in) :: logger

      ! TODO add the logger to self%logger_references
   end subroutine multi_logger_add_logger

   subroutine multi_logger_log(self, message)
      class(multi_logger_t), intent(inout) :: self
      character(len=*), intent(in) :: message

      ! TODO loop over self%logger_references and call each logger's log() procedure
   end subroutine multi_logger_log

end module logging_aggregation
