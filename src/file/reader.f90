module file_reader
   use common_error_handling, only: error_t
   use common_strings, only: string_or_error_t, string_t, string_list_t
   use common_to_string, only: to_string
   use file_operations, only: open_existing_file_for_reading, file_unit_or_error_t

   implicit none(type, external)

   private

   public :: reader_t
   public :: file_reader_t
   public :: file_reader_or_error_t
   public :: in_memory_reader_t
   public :: create_file_reader
   public :: create_in_memory_reader

   type, abstract :: reader_t
   contains
      procedure(read_line_proc), deferred :: read_line
      procedure(done_proc), deferred :: done
   end type reader_t

   abstract interface
      function read_line_proc(self) result(line)
         import :: string_or_error_t
         import :: reader_t

         implicit none(type, external)

         class(reader_t), intent(inout) :: self
         type(string_or_error_t) :: line
      end function read_line_proc

      subroutine done_proc(self)
         import :: reader_t

         implicit none(type, external)

         class(reader_t), intent(inout) :: self
      end subroutine done_proc
   end interface

   type, extends(reader_t) :: file_reader_t
      integer :: file_unit
   contains
      procedure :: read_line => file_reader_read_line
      procedure :: done => file_reader_done
   end type file_reader_t

   type :: file_reader_or_error_t
      type(file_reader_t), allocatable :: file_reader
      type(error_t), allocatable :: error
   end type file_reader_or_error_t

   type :: in_memory_reader_t
      type(string_list_t), allocatable :: lines
      integer :: next_index = 1
   contains
      procedure :: read_line => in_memory_reader_read_line
      procedure :: done => in_memory_reader_done
   end type in_memory_reader_t

contains

   function file_reader_read_line(self) result(line)
      class(file_reader_t), intent(inout) :: self

      type(string_or_error_t) :: line

      character(len=512) :: line_buffer
      integer :: io_status
      character(len=255) :: io_message

      read (self%file_unit, '(A)', iostat=io_status, iomsg=io_message) line_buffer

      if (io_status /= 0) then
         line%error = error_t('Could not read line from file. iostat= '// &
                              to_string(io_status)//' iomsg= '//io_message)
         return
      end if

      line%string = string_t(trim(line_buffer))
   end function file_reader_read_line

   subroutine file_reader_done(self)
      class(file_reader_t), intent(inout) :: self
      close (self%file_unit)
   end subroutine file_reader_done

   function in_memory_reader_read_line(self) result(line)
      class(in_memory_reader_t), intent(inout) :: self

      type(string_or_error_t) :: line

      if (size(self%lines%strings) < self%next_index) then
         line%error = error_t('Reached the end of the list of strings')
         return
      end if

      line%string = self%lines%strings(self%next_index)
      self%next_index = self%next_index + 1
   end function in_memory_reader_read_line

   subroutine in_memory_reader_done(self)
      class(in_memory_reader_t), intent(inout) :: self

      ! Do nothing
   end subroutine in_memory_reader_done

   function create_file_reader(path) result(file_reader)
      character(len=*), intent(in) :: path

      type(file_reader_or_error_t) :: file_reader
      type(file_unit_or_error_t) :: file_unit

      file_unit = open_existing_file_for_reading(path)
      if (allocated(file_unit%error)) then
         file_reader%error = file_unit%error
         return
      end if

      file_reader%file_reader = file_reader_t(file_unit%file_unit)
   end function create_file_reader

   function create_in_memory_reader(lines) result(reader)
      type(string_list_t), intent(in) :: lines

      type(in_memory_reader_t) :: reader

      reader%lines = lines
   end function create_in_memory_reader

end module file_reader
