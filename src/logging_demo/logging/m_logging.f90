
module m_logging
   use m_command_line, only: has_cli_argument

   implicit none(type, external)

   private
   public :: get_logger, t_abstract_logger

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

   type, extends(t_abstract_logger) :: t_file_logger
      integer :: log_file_unit
   contains
      procedure :: log => file_logger_log
      final :: file_logger_destructor
   end type t_file_logger

   interface t_file_logger
      procedure :: file_logger_constructor
   end interface

   type, extends(t_abstract_logger) :: t_stdout_logger
   contains
      procedure :: log => stdout_logger_log
   end type t_stdout_logger

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

   class(t_abstract_logger), allocatable, target :: the_logger
contains
   impure function do_everything_logger_constructor(debug, quiet) result(logger)
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
      type(t_do_everything_logger) :: self

      print *, 'Destructor of t_do_everything_logger called'
   end subroutine do_everything_logger_destructor

   function file_logger_constructor(log_file_path) result(file_logger)
      character(len=*), intent(in) :: log_file_path
      type(t_file_logger), pointer :: file_logger

      integer :: open_status
      integer :: log_file_unit

      print *, 'Constructing file logger'
      open (file=log_file_path, newunit=log_file_unit, status='unknown', &
            position='append', action='write', iostat=open_status)

      if (open_status /= 0) then
         error stop "Could not open log file for writing"
      end if

      allocate (file_logger)

      file_logger%log_file_unit = log_file_unit
   end function file_logger_constructor

   subroutine file_logger_destructor(self)
      type(t_file_logger) :: self
      print *, 'Destructor of file logger called'
   end subroutine file_logger_destructor

   subroutine file_logger_log(self, message)
      class(t_file_logger), intent(inout) :: self
      character(len=*), intent(in) :: message

      write (self%log_file_unit, fmt=*) current_time()//' '//message
   end subroutine file_logger_log

   subroutine stdout_logger_log(self, message)
      class(t_stdout_logger), intent(inout) :: self
      character(len=*), intent(in) :: message

      print *, current_time()//' '//message
   end subroutine stdout_logger_log

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

   function current_time() result(iso_time)
      character(len=8) :: date
      character(len=10) :: time
      character(len=5) :: zone
      character(len=24) :: iso_time

      call date_and_time(date, time, zone)

      iso_time = date(1:4)//'-'//date(5:6)//'-'//date(7:8)//'T'// &
                 time(1:2)//':'//time(3:4)//':'//time(5:6)//zone
   end function current_time
end module m_logging
