module logging_everything
   use logging_abstract, only: t_abstract_logger
   use logging_file, only: t_file_logger
   use logging_stdout, only: t_stdout_logger

   implicit none(type, external)

   private
   public :: t_do_everything_logger

   type, extends(t_abstract_logger) :: t_do_everything_logger
      private

      logical :: debug
      logical :: quiet

      class(t_file_logger), allocatable :: file_logger
      class(t_stdout_logger), allocatable :: stdout_logger
   contains
      procedure :: log => do_everything_logger_log
      final :: do_everything_logger_destructor
   end type t_do_everything_logger

   interface t_do_everything_logger
      procedure :: do_everything_logger_constructor
   end interface
contains
   function do_everything_logger_constructor(debug, quiet) result(logger)
      logical, intent(in) :: debug
      logical, intent(in) :: quiet
      type(t_do_everything_logger), pointer :: logger

      print *, 'Constructor of t_do_everything_logger'

      allocate (logger)

      logger%debug = debug
      logger%quiet = quiet
      logger%file_logger = t_file_logger('debug.log')
      logger%stdout_logger = t_stdout_logger()
   end function do_everything_logger_constructor

   subroutine do_everything_logger_destructor(self)
      type(t_do_everything_logger), intent(in) :: self

      print *, 'Destructor of t_do_everything_logger called'
   end subroutine do_everything_logger_destructor

   subroutine do_everything_logger_log(self, message)
      class(t_do_everything_logger), intent(inout) :: self
      character(len=*), intent(in) :: message

      if (self%debug) then
         call self%file_logger%log(message)

         if (.not. self%quiet) then
            call self%stdout_logger%log(message)
         end if
      end if
   end subroutine do_everything_logger_log
end module logging_everything
