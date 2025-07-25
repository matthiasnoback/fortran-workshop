module common_strings
   use common_error_handling, only: error_t

   implicit none(type, external)

   private
   public :: string_list_t
   public :: string_t
   public :: string_length
   public :: string_list_or_error_t
   public :: empty_string_list

   !> Wrapper for a variable-length character value
   type :: string_t
      character(len=:), allocatable :: value
   end type string_t

   !> List type for `string_t` objects (because in Fortran we can't have an array of
   !> variable-length character values)
   type :: string_list_t
      type(string_t), dimension(:), allocatable :: strings
   contains
      procedure :: add => string_list_add
      procedure, private :: map_to_integer_array
      generic :: map => map_to_integer_array
   end type string_list_t

   type :: string_list_or_error_t
      type(string_list_t), allocatable :: string_list
      type(error_t), allocatable :: error
   end type string_list_or_error_t

contains

   pure function empty_string_list() result(string_list)
      type(string_list_t) :: string_list
      allocate (string_list%strings(0))
   end function empty_string_list

   subroutine string_list_add(self, string)
      class(string_list_t), intent(inout) :: self
      character(len=*), intent(in) :: string
      type(string_t), dimension(:), allocatable :: new_strings

      integer :: current_size

      if (.not. allocated(self%strings)) then
         allocate (self%strings(0))
      end if

      current_size = size(self%strings)

      allocate (new_strings(current_size + 1))
      new_strings(1:current_size) = self%strings
      new_strings(current_size + 1) = string_t(string)
      call move_alloc(new_strings, self%strings)
   end subroutine string_list_add

   pure function map_to_integer_array(self, map_function) result(integers)
      class(string_list_t), intent(in) :: self
      interface
         pure function map_function(old_value) result(new_value)
            import string_t

            implicit none(type, external)

            class(string_t), intent(in) :: old_value
            integer :: new_value
         end function map_function
      end interface
      integer, dimension(size(self%strings)) :: integers

      integer :: i

      do i = 1, size(self%strings)
         integers(i) = map_function(self%strings(i))
      end do
   end function map_to_integer_array

   pure function string_length(string) result(res)
      class(string_t), intent(in) :: string
      integer :: res

      res = len(string%value)
   end function string_length

end module common_strings
