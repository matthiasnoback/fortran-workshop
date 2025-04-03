module logging_file
   use common_time, only: current_time
   use logging_abstract, only: abstract_logger_t

   implicit none(type, external)

   private
   public :: file_logger_t

   type, extends(abstract_logger_t) :: file_logger_t
      integer :: log_file_unit
   contains
      procedure :: log => file_logger_log
      final :: file_logger_destructor
   end type file_logger_t

   interface file_logger_t
      procedure :: file_logger_constructor
   end interface

contains

   function file_logger_constructor(log_file_path) result(file_logger)
      character(len=*), intent(in) :: log_file_path
      type(file_logger_t), pointer :: file_logger

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
      type(file_logger_t), intent(in) :: self
      print *, 'Destructor of file logger called'
   end subroutine file_logger_destructor

   subroutine file_logger_log(self, message)
      class(file_logger_t), intent(inout) :: self
      character(len=*), intent(in) :: message

      write (self%log_file_unit, fmt=*) current_time()//' '//message
   end subroutine file_logger_log

end module logging_file
