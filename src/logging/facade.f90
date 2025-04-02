module logging_facade
   use common_command_line, only: has_cli_argument
   use logging_abstract, only: t_abstract_logger
   use logging_file, only: t_file_logger
   use logging_stdout, only: t_stdout_logger
   use logging_everything, only: t_do_everything_logger

   implicit none(type, external)

   private
   public :: get_logger

   class(t_abstract_logger), allocatable, target :: the_logger
contains

   function get_logger() result(logger)
      class(t_abstract_logger), pointer :: logger

      logical :: debug
      logical :: quiet

      if (.not. allocated(the_logger)) then
         debug = .not. has_cli_argument('--no-debug')
         quiet = has_cli_argument('--quiet')

         the_logger = t_do_everything_logger(debug, quiet)
      end if

      logger => the_logger
   end function get_logger

end module logging_facade
