module logging_facade
   use logging_abstract, only: abstract_logger_t

   implicit none(type, external)

   private
   public :: get_logger

   interface
      module function get_logger() result(logger)
         import abstract_logger_t

         implicit none(type, external)

         class(abstract_logger_t), pointer :: logger
      end function get_logger
   end interface

end module logging_facade
