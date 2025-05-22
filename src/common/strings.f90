module common_strings
   implicit none(type, external)

   private
   public :: string_list_t
   public :: string_t
   public :: string_length

   !> Wrapper for a variable-length character value
   type :: string_t
      character(len=:), allocatable :: value
   end type string_t

   !> List type for `string_t` objects (because in Fortran we can't have an array of
   !> variable-length character values)
   type :: string_list_t
      type(string_t), dimension(:), allocatable :: strings
   contains
      procedure, private :: map_to_integer_array
      generic :: map => map_to_integer_array
   end type string_list_t

contains

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
