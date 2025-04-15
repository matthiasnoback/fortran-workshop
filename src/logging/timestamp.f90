module logging_timestamp
   use common_time, only: current_time
   use logging_abstract, only: abstract_logger_t

   implicit none(type, external)

   private
   public :: timestamp_decorating_logger_t

   type, extends(abstract_logger_t) :: timestamp_decorating_logger_t
      class(abstract_logger_t), pointer :: decorated_logger => null()
   contains
      procedure :: log => timestamp_decorating_logger_log
   end type timestamp_decorating_logger_t

contains

   subroutine timestamp_decorating_logger_log(self, message)
      class(timestamp_decorating_logger_t), intent(inout) :: self
      character(len=*), intent(in) :: message

      call self%decorated_logger%log(current_time() // ' ' // message)
   end subroutine timestamp_decorating_logger_log

end module logging_timestamp
