module file_operations
   use common_to_string, only: to_string
   use common_error_handling, only: error_t, optional_error_t, no_error_t, some_error_t
   use common_strings, only: string_list_t, string_list_or_error_t, empty_string_list

   implicit none(type, external)

   private

   public :: create_temp_file
   public :: file_unit_or_error_t
   public :: write_lines_to_file
   public :: write_lines_to_temp_file
   public :: read_all_lines

   type :: file_unit_or_error_t
      integer, allocatable :: file_unit
      type(error_t), allocatable :: error
   end type file_unit_or_error_t

contains

   function create_temp_file() result(file_unit_or_error)
      type(file_unit_or_error_t) :: file_unit_or_error

      integer :: file_unit
      integer :: io_status
      character(len=255) :: io_message

      open (newunit=file_unit, status='scratch', action='write', iostat=io_status, iomsg=io_message)
      if (io_status /= 0) then
         file_unit_or_error%error = error_t('Could not create temp file. iostat= '// &
                                            to_string(io_status)//' iomsg= '//io_message)
         return
      end if

      file_unit_or_error%file_unit = file_unit
   end function create_temp_file

   function write_lines_to_file(file_unit, lines) result(optional_error)
      integer, intent(in) :: file_unit
      type(string_list_t), intent(in) :: lines
      class(optional_error_t), allocatable :: optional_error

      integer :: line_number
      integer :: io_status
      character(len=255) :: io_message

      do line_number = 1, size(lines%strings)
         write (file_unit, '(A)', iostat=io_status, iomsg=io_message) &
            lines%strings(line_number)%value
         if (io_status /= 0) then
            optional_error = some_error_t( &
                             error_t('Failed to write some lines to file: '//io_message) &
                             )
            return
         end if
      end do

      optional_error = no_error_t()
   end function write_lines_to_file

   function write_lines_to_temp_file(lines) result(file_unit_or_error)
      type(string_list_t), intent(in) :: lines
      type(file_unit_or_error_t) :: file_unit_or_error
      class(optional_error_t), allocatable :: optional_error

      file_unit_or_error = create_temp_file()
      if (allocated(file_unit_or_error%error)) then
         return
      end if

      optional_error = write_lines_to_file(file_unit_or_error%file_unit, lines)
      select type (optional_error)
      type is (some_error_t)
         file_unit_or_error%error = optional_error%error
         return
      end select
   end function write_lines_to_temp_file

   function read_all_lines(file_unit) result(string_list_or_error)
      integer, intent(in) :: file_unit
      character(len=255) :: line
      integer :: io_status
      character(len=255) :: io_message
      type(string_list_t), allocatable :: lines
      type(string_list_or_error_t) :: string_list_or_error

      lines = empty_string_list()

      rewind (file_unit)

      do
         read (file_unit, '(A)', iostat=io_status, iomsg=io_message, end=100) line
         if (io_status /= 0) then
            string_list_or_error%error = error_t(io_message)
            return
         end if
         call lines%add(trim(line))
      end do

100   call move_alloc(lines, string_list_or_error%lines)
   end function read_all_lines

end module file_operations
