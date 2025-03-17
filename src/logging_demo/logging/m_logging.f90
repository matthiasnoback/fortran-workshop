module m_logging
   implicit none(type, external)

   type, abstract :: t_abstract_logger
   contains
      procedure(log_function_interface), deferred :: log
   end type t_abstract_logger

   interface
      subroutine log_function_interface(self, message)
         import t_abstract_logger
         implicit none(type, external)

         class(t_abstract_logger), intent(in) :: self
         character(len=*), intent(in) :: message
      end subroutine log_function_interface
   end interface

   type, extends(t_abstract_logger) :: t_do_everything_logger
      private

      logical :: debug
      logical :: quiet
      logical :: should_log_to_file
      integer :: log_file
   contains
      procedure :: log => do_everytying_logger_log
   end type t_do_everything_logger

   interface t_do_everything_logger
      procedure :: do_everything_logger_constructor
   end interface

   class(t_abstract_logger), allocatable :: shared_logger

contains
   function get_logger() result(logger_instance)
      ! Alternatively, we could add the `save` attribute to this variable.
      ! The downside is, we have no way to choose an alternative logger
      ! instance, e.g. in a situation where we want to use an "in-memory"
      ! logger when testing a function that calls the logger() function.
      class(t_abstract_logger), allocatable :: logger_instance

      if (.not. allocated(shared_logger)) then
         ! We could read values for debug and quiet from CLI arguments
         ! --debug, --quiet

         shared_logger = t_do_everything_logger(.true., .false.)
      end if

      logger_instance = shared_logger
   end function get_logger

   function do_everything_logger_constructor(debug, quiet) result(logger)
      logical, intent(in) :: debug
      logical, intent(in) :: quiet
      type(t_do_everything_logger) :: logger

      integer :: open_status
      integer :: log_file

      open (file='debug.log', newunit=log_file, status='unknown', &
            position='append', action='write', iostat=open_status)

      logger%debug = debug
      logger%quiet = quiet
      logger%log_file = log_file
      logger%should_log_to_file = open_status == 0
   end function do_everything_logger_constructor

   subroutine do_everytying_logger_log(self, message)
      class(t_do_everything_logger), intent(in) :: self
      character(len=*), intent(in) :: message

      if (self%debug) then
         if (self%should_log_to_file) then
            write (self%log_file, fmt=*) current_time()//' '//message
         end if

         if (.not. self%quiet) then
            print *, current_time()//' '//message
         end if
      end if
   end subroutine do_everytying_logger_log

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
