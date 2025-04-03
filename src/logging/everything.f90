module logging_everything
   use logging_abstract, only: abstract_logger_t
   use logging_file, only: file_logger_t
   use logging_stdout, only: stdout_logger_t

   implicit none(type, external)

   private
   public :: do_everything_logger_t

   type, extends(abstract_logger_t) :: do_everything_logger_t
      private

      logical :: debug
      logical :: quiet

      class(file_logger_t), allocatable :: file_logger
      class(stdout_logger_t), allocatable :: stdout_logger
   contains
      procedure :: log => do_everything_logger_log
      final :: do_everything_logger_destructor
   end type do_everything_logger_t

   interface do_everything_logger_t
      procedure :: do_everything_logger_constructor
   end interface
contains
   function do_everything_logger_constructor(debug, quiet) result(logger)
      logical, intent(in) :: debug
      logical, intent(in) :: quiet
      type(do_everything_logger_t), pointer :: logger

      print *, 'Constructor of do_everything_logger_t'

      allocate (logger)

      logger%debug = debug
      logger%quiet = quiet
      logger%file_logger = file_logger_t('debug.log')
      logger%stdout_logger = stdout_logger_t()
   end function do_everything_logger_constructor

   subroutine do_everything_logger_destructor(self)
      type(do_everything_logger_t), intent(in) :: self

      print *, 'Destructor of do_everything_logger_t called'
   end subroutine do_everything_logger_destructor

   subroutine do_everything_logger_log(self, message)
      class(do_everything_logger_t), intent(inout) :: self
      character(len=*), intent(in) :: message

      if (self%debug) then
         call self%file_logger%log(message)

         if (.not. self%quiet) then
            call self%stdout_logger%log(message)
         end if
      end if
   end subroutine do_everything_logger_log
end module logging_everything
