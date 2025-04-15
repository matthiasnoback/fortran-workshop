submodule(logging_facade) logging_facade_implementation
   use common_command_line, only: has_cli_argument
   use logging_abstract, only: abstract_logger_t
   use logging_file, only: file_logger_t
   use logging_stdout, only: stdout_logger_t
   use logging_everything, only: do_everything_logger_t
   use logging_timestamp, only: timestamp_decorating_logger_t

   implicit none(type, external)

   class(abstract_logger_t), allocatable, target :: the_logger

contains

   module function get_logger() result(logger)
      class(abstract_logger_t), pointer :: logger

      logical :: debug
      logical :: quiet
      type(timestamp_decorating_logger_t) :: timestamp_decorating_logger

      if (.not. allocated(the_logger)) then
         debug = .not. has_cli_argument('--no-debug')
         quiet = has_cli_argument('--quiet')

         timestamp_decorating_logger%decorated_logger => do_everything_logger_t(debug, quiet)
         the_logger = timestamp_decorating_logger
      end if

      logger => the_logger
   end function get_logger

end submodule logging_facade_implementation
