module logging_stdout
   use common_time, only: current_time
   use logging_abstract, only: t_abstract_logger

   implicit none(type, external)

   private
   public :: t_stdout_logger

   type, extends(t_abstract_logger) :: t_stdout_logger
   contains
      procedure :: log => stdout_logger_log
   end type t_stdout_logger

contains

   subroutine stdout_logger_log(self, message)
      class(t_stdout_logger), intent(inout) :: self
      character(len=*), intent(in) :: message

      print *, current_time()//' '//message
   end subroutine stdout_logger_log

end module logging_stdout
