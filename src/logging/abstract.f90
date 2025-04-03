module logging_abstract

   implicit none(type, external)

   private
   public :: abstract_logger_t

   type, abstract :: abstract_logger_t
   contains
      procedure(log_function_interface), deferred :: log
   end type abstract_logger_t

   interface
      subroutine log_function_interface(self, message)
         import abstract_logger_t
         implicit none(type, external)

         class(abstract_logger_t), intent(inout) :: self
         character(len=*), intent(in) :: message
      end subroutine log_function_interface
   end interface

end module logging_abstract
