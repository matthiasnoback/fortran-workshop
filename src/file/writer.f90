module file_writer
   use common_error_handling, only: error_t, optional_error_t, some_error_t, no_error_t
   use common_strings, only: string_or_error_t, string_t, string_list_t
   use common_to_string, only: to_string
   use file_operations, only: open_or_create_file_for_writing, file_unit_or_error_t

   implicit none(type, external)

   private

   public :: writer_t
   public :: file_writer_t
   public :: file_writer_or_error_t
   public :: in_memory_writer_t
   public :: create_file_writer
   public :: create_in_memory_writer

   type, abstract :: writer_t
   contains
      procedure(write_line_proc), deferred :: write_line
      procedure(done_proc), deferred :: done
   end type writer_t

   abstract interface
      function write_line_proc(self, line) result(optional_error)
         import :: writer_t
         import :: optional_error_t

         implicit none(type, external)

         class(writer_t), intent(inout) :: self
         character(len=*), intent(in) :: line

         class(optional_error_t), allocatable :: optional_error
      end function write_line_proc

      subroutine done_proc(self)
         import :: writer_t

         implicit none(type, external)

         class(writer_t), intent(inout) :: self
      end subroutine done_proc
   end interface

   type, extends(writer_t) :: file_writer_t
      integer :: file_unit
   contains
      procedure :: write_line => file_writer_write_line
      procedure :: done => file_writer_done
   end type file_writer_t

   type :: file_writer_or_error_t
      type(file_writer_t), allocatable :: file_writer
      type(error_t), allocatable :: error
   end type file_writer_or_error_t

   type :: in_memory_writer_t
      type(string_list_t), allocatable :: lines
   contains
      procedure :: write_line => in_memory_writer_write_line
   end type in_memory_writer_t

contains

   function file_writer_write_line(self, line) result(optional_error)
      class(file_writer_t), intent(inout) :: self
      character(len=*), intent(in) :: line

      class(optional_error_t), allocatable :: optional_error

      integer :: io_status
      character(len=255) :: io_message

      write (self%file_unit, '(A)', iostat=io_status, iomsg=io_message) line

      if (io_status /= 0) then
         optional_error = some_error_t(error_t('Could not write line to file. iostat= '// &
                                               to_string(io_status)//' iomsg= '//io_message))
         return
      end if

      optional_error = no_error_t()
   end function file_writer_write_line

   subroutine file_writer_done(self)
      class(file_writer_t), intent(inout) :: self

      close (self%file_unit)
   end subroutine file_writer_done

   function in_memory_writer_write_line(self, line) result(optional_error)
      class(in_memory_writer_t), intent(inout) :: self
      character(len=*), intent(in) :: line

      class(optional_error_t), allocatable :: optional_error

      call self%lines%add(line)

      optional_error = no_error_t()
   end function in_memory_writer_write_line

   subroutine in_memory_writer_done(self)
      class(in_memory_writer_t), intent(inout) :: self

      ! Do nothing
   end subroutine in_memory_writer_done

   function create_file_writer(path) result(file_writer)
      character(len=*), intent(in) :: path

      type(file_writer_or_error_t) :: file_writer
      type(file_unit_or_error_t) :: file_unit

      file_unit = open_or_create_file_for_writing(path)
      if (allocated(file_unit%error)) then
         file_writer%error = file_unit%error
         return
      end if

      file_writer%file_writer = file_writer_t(file_unit%file_unit)
   end function create_file_writer

   function create_in_memory_writer() result(writer)
      type(in_memory_writer_t) :: writer

      writer%lines = string_list_t()
   end function create_in_memory_writer

end module file_writer
