module logging_abstract

   implicit none(type, external)

   private
   public :: t_abstract_logger

   type, abstract :: t_abstract_logger
   contains
      procedure(log_function_interface), deferred :: log
   end type t_abstract_logger

   interface
      subroutine log_function_interface(self, message)
         import t_abstract_logger
         implicit none(type, external)

         class(t_abstract_logger), intent(inout) :: self
         character(len=*), intent(in) :: message
      end subroutine log_function_interface
   end interface

end module logging_abstract
