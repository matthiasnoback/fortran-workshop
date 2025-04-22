submodule(logging_facade) logging_facade_implementation
   use common_command_line, only: has_cli_argument
   use logging_abstract, only: abstract_logger_t
   use logging_file, only: file_logger_t
   use logging_stdout, only: stdout_logger_t
   use logging_everything, only: do_everything_logger_t
   use logging_aggregation, only: multi_logger_t

   implicit none(type, external)

   class(abstract_logger_t), allocatable, target :: the_logger

contains

   module function get_logger() result(logger)
      class(abstract_logger_t), pointer :: logger

      type(multi_logger_t), pointer :: multi_logger
      class(abstract_logger_t), pointer :: file_logger
      class(abstract_logger_t), pointer :: stdout_logger

      logical :: debug
      logical :: quiet

      if (.not. allocated(the_logger)) then
         allocate (multi_logger)

         debug = .not. has_cli_argument('--no-debug')
         quiet = has_cli_argument('--quiet')

         if (debug) then
            file_logger => file_logger_t('debug.log')
            call multi_logger%add_logger(file_logger)

            if (.not. quiet) then
               stdout_logger => stdout_logger_t()
               call multi_logger%add_logger(stdout_logger)
            end if
         end if

         the_logger = multi_logger
      end if

      logger => the_logger
   end function get_logger

end submodule logging_facade_implementation
