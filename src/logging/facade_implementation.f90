submodule(logging_facade) logging_facade_implementation
   use common_command_line, only: has_cli_argument
   use logging_abstract, only: abstract_logger_t
   use logging_file, only: file_logger_t
   use logging_stdout, only: stdout_logger_t
   use logging_everything, only: do_everything_logger_t
   use logging_timestamp, only: timestamp_decorating_logger_t
   use logging_aggregation, only: logger_reference_t, multi_logger_t

   implicit none(type, external)

   class(abstract_logger_t), allocatable, target :: the_logger

contains

   module function get_logger() result(logger)
      class(abstract_logger_t), pointer :: logger

      logical :: debug
      logical :: quiet
      type(timestamp_decorating_logger_t) :: timestamp_decorating_logger

      type(logger_reference_t), dimension(:), allocatable :: logger_references
      type(multi_logger_t), pointer :: multi_logger

      if (.not. allocated(the_logger)) then
         debug = .not. has_cli_argument('--no-debug')
         quiet = has_cli_argument('--quiet')

         if (debug) then
            logger_references = [logger_reference_t(file_logger_t('debug.log'))]
            if (.not. quiet) then
               logger_references = [logger_references, logger_reference_t(stdout_logger_t())]
            end if
         else
            allocate(logger_references(0))
         end if

         allocate(multi_logger)
         multi_logger%logger_references = logger_references
         timestamp_decorating_logger%decorated_logger => multi_logger
         the_logger = timestamp_decorating_logger
      end if

      logger => the_logger
   end function get_logger

end submodule logging_facade_implementation
