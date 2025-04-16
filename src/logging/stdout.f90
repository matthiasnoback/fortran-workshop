module logging_stdout
   use logging_abstract, only: abstract_logger_t

   implicit none(type, external)

   private
   public :: stdout_logger_t

   type, extends(abstract_logger_t) :: stdout_logger_t
   contains
      procedure :: log => stdout_logger_log
   end type stdout_logger_t

   interface stdout_logger_t
      procedure :: stdout_logger_constructor
   end interface

contains

   function stdout_logger_constructor() result(stdout_logger)
      type(stdout_logger_t), pointer :: stdout_logger

      allocate (stdout_logger)
   end function stdout_logger_constructor

   subroutine stdout_logger_log(self, message)
      class(stdout_logger_t), intent(inout) :: self
      character(len=*), intent(in) :: message

      print *, message
   end subroutine stdout_logger_log

end module logging_stdout
