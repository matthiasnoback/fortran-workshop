module logging_timestamp
   use common_time, only: current_time
   use logging_abstract, only: abstract_logger_t

   implicit none(type, external)

   private
   public :: timestamp_decorating_logger_t

   type, extends(abstract_logger_t) :: timestamp_decorating_logger_t
      class(abstract_logger_t), pointer :: the_original_logger
   contains
      procedure :: log => timestamp_decorating_logger_log
      final :: timestamp_decorating_logger_destruct
   end type timestamp_decorating_logger_t

contains

   subroutine timestamp_decorating_logger_log(self, message)
      class(timestamp_decorating_logger_t), intent(inout) :: self
      character(len=*), intent(in) :: message

      call self%the_original_logger%log(current_time()//' '//message)
   end subroutine timestamp_decorating_logger_log

   subroutine timestamp_decorating_logger_destruct(self)
      type(timestamp_decorating_logger_t), intent(inout) :: self
      print *, 'Destructing timestamp_decorating_logger_t'
      deallocate(self%the_original_logger)
   end subroutine timestamp_decorating_logger_destruct

end module logging_timestamp
