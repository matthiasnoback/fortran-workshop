module config_loading
   use common_precision, only: dp
   use common_error_handling, only: error_t, optional_error_t, no_error_t, some_error_t
   use common_strings, only: string_t, string_list_t, empty_string_list, &
                             string_list_or_error_t

   implicit none(type, external)

   private

   public :: config_value_t
   public :: real_or_error_t
   public :: configuration_t
   public :: config_value_or_error_t
   public :: load_config_from_file
   public :: configuration_or_error_t

   type :: config_value_t
      character(len=:), allocatable :: key
      character(len=:), allocatable :: string_value
   contains
      procedure :: get_real => config_value_get_real
      procedure :: get_string => config_value_get_string
   end type config_value_t

   type :: real_or_error_t
      real(kind=dp), allocatable :: value
      type(error_t), allocatable :: error
   end type real_or_error_t

   type :: configuration_t
      type(config_value_t), dimension(:), allocatable :: values
   contains
      procedure :: get_config_value => configuration_get_config_value
   end type configuration_t

   type :: configuration_or_error_t
      type(configuration_t), allocatable :: configuration
      type(error_t), allocatable :: error
   end type configuration_or_error_t

   type :: config_value_or_error_t
      type(config_value_t), allocatable :: config_value
      type(error_t), allocatable :: error
   end type config_value_or_error_t

contains

   pure function config_value_get_real(self) result(real_or_error)
      class(config_value_t), intent(in) :: self
      type(real_or_error_t) :: real_or_error
      real(kind=dp) :: real_value

      integer :: iostat

      read (self%string_value, *, iostat=iostat) real_value

      if (iostat /= 0) then
         real_or_error%error = error_t('Could not parse "'//self%string_value//'" as a real')
         return
      end if

      real_or_error%value = real_value
   end function config_value_get_real

   pure function config_value_get_string(self) result(res)
      class(config_value_t), intent(in) :: self
      character(len=:), allocatable :: res

      integer :: string_value_length
      string_value_length = len(self%string_value)

      if (self%string_value(1:1) == '"' .and. &
          self%string_value(string_value_length:string_value_length) == '"') then
         res = self%string_value(2:string_value_length - 1)
      else
         res = self%string_value
      end if
   end function config_value_get_string

   pure function configuration_get_config_value(self, key) result(config_value_or_error)
      class(configuration_t), intent(in) :: self
      character(len=*), intent(in) :: key
      type(config_value_or_error_t) :: config_value_or_error

      integer :: i

      do i = 1, size(self%values)
         if (self%values(i)%key == key) then
            config_value_or_error%config_value = self%values(i)
            return
         end if
      end do

      config_value_or_error%error = error_t( &
                                    'Could not find a configuration value with key "'//key//'"')
   end function configuration_get_config_value

   function load_config_from_file(file_unit) result(configuration_or_error)
      integer, intent(in) :: file_unit
      type(configuration_or_error_t) :: configuration_or_error

      type(string_list_or_error_t), allocatable :: lines_or_error
      type(config_value_or_error_t), allocatable :: config_value_or_error
      integer :: line_number

      character(len=:), allocatable :: key
      character(len=:), allocatable :: string_value

      type(configuration_t) :: configuration

      lines_or_error = read_all_lines(file_unit)
      if (allocated(lines_or_error%error)) then
         configuration_or_error%error = lines_or_error%error
      end if

      allocate (configuration%values(size(lines_or_error%string_list%strings)))

      ! TODO write function that parses `string_list_t` into config values
      do line_number = 1, size(lines_or_error%string_list%strings)
         config_value_or_error = create_config_value_from_line( &
                                 lines_or_error%string_list%strings(line_number))

         if (allocated(config_value_or_error%error)) then
            configuration_or_error%error = config_value_or_error%error
            return
         end if

         configuration%values(line_number) = config_value_or_error%config_value
      end do

      configuration_or_error%configuration = configuration
   end function load_config_from_file

   pure function create_config_value_from_line(line) result(config_value_or_error)
      type(string_t), intent(in) :: line
      type(config_value_or_error_t) :: config_value_or_error

      character(:), allocatable :: key, string_value
      integer :: split_at

      split_at = index(line%value, ' = ')

      if (split_at == 0) then
         config_value_or_error%error = error_t( &
                                       'Line does not match expected format key = value: ' &
                                       //line%value)
      end if

      key = adjustl(line%value(1:split_at - 1))
      string_value = adjustl(line%value(split_at + 3:))
      config_value_or_error%config_value = config_value_t(key, string_value)
   end function create_config_value_from_line

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

100   call move_alloc(lines, string_list_or_error%string_list)
   end function read_all_lines

end module config_loading
