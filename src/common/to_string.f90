module common_to_string
   implicit none(type, external)

   private
   public :: to_string
   public :: to_string_t

   !> Generic interface `to_string()` for converting various types to strings
   !> E.g. to_string(1), to_string([1, 2]), to_string(1.0), to_string([1.0, 2.0]), etc.
   interface to_string
      module procedure &
         int_to_string, &
         int_array_to_string, &
         real_to_string, &
         real_array_to_string, &
         logical_to_string, &
         logical_array_to_string, &
         derived_type_to_string
   end interface

   !> Abstract type for derived types so they can represent themselves as strings
   type, abstract :: to_string_t
   contains
      !> A subtype should implement `to_string()` to present itself as a string
      procedure(derived_type_to_string_interface), public, deferred :: to_string

      !> A generic write(formatted) procedure adds support using this DT directly
      !> in `write` and `print` statements, e.g. `print *, my_dt`
      procedure, private :: to_string_write
      generic, public :: write (formatted) => to_string_write
   end type to_string_t

   interface
      pure function derived_type_to_string_interface(self) result(str)
         import :: to_string_t

         implicit none(type, external)

         class(to_string_t), intent(in) :: self
         character(len=:), allocatable :: str
      end function derived_type_to_string_interface
   end interface

contains

   !> Generic write(formatted) procedure for derived types
   !> This makes it possible to directly pass a derived type to `write` or `print` statements.
   !> The derived type must extend `to_string_t` and implement the `to_string` procedure.
   !> Note: officially, a generic write procedure  like this is meant to be used for serializing
   !> and deserializing derived types, not for simple debugging print statements.
   subroutine to_string_write(self, unit, iotype, v_list, iostat, iomsg)
      class(to_string_t), intent(in) :: self
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      ! Reference `iotype` to avoid unused variable warning; from "Modern Fortran, Explained":
      !   `iotype` is a scalar of intent `in` and type `character(*)`. Its value is 'LISTDIRECTED',
      !   'NAMELIST', or 'DT'//string, where string is the charcater string from the `dt` edit descriptor.
      associate (dummy => iotype)
      end associate

      ! Reference `v_list` to avoid unused variable warning; from "Modern Fortran, Explained":
      !   `v_list` is a rank-one assumed-shape array of intent `in` and type default `integer`.
      !   Its value comes from the parenthetical list of the edit descriptor.
      associate (dummy => v_list)
      end associate

      write (unit, fmt=*, iomsg=iomsg, iostat=iostat) self%to_string()
   end subroutine to_string_write

   pure function int_to_string(value) result(str)
      integer, intent(in) :: value
      character(len=32) :: temp
      character(len=:), allocatable :: str

      write (temp, *) value
      str = trim(adjustl(temp))
   end function int_to_string

   pure function int_array_to_string(value) result(str)
      integer, dimension(:), intent(in) :: value
      character(len=:), allocatable :: str

      integer :: i

      str = ''
      do i = 1, size(value)
         if (str /= '') then
            str = str//', '
         end if
         str = str//to_string(value(i))
      end do
   end function int_array_to_string

   pure function real_to_string(value) result(str)
      real, intent(in) :: value
      character(len=32) :: temp
      character(len=:), allocatable :: str

      write (temp, *) value
      str = trim(adjustl(temp))
   end function real_to_string

   pure function real_array_to_string(value) result(str)
      real, dimension(:), intent(in) :: value
      character(len=:), allocatable :: str

      integer :: i

      str = ''
      do i = 1, size(value)
         if (str /= '') then
            str = str//', '
         end if
         str = str//to_string(value(i))
      end do
   end function real_array_to_string

   pure function logical_to_string(value) result(str)
      logical, intent(in) :: value
      character(len=32) :: temp
      character(len=:), allocatable :: str

      write (temp, *) value
      str = trim(adjustl(temp))
   end function logical_to_string

   pure function logical_array_to_string(value) result(str)
      logical, dimension(:), intent(in) :: value
      character(len=:), allocatable :: str

      integer :: i

      str = ''
      do i = 1, size(value)
         if (str /= '') then
            str = str//', '
         end if
         str = str//to_string(value(i))
      end do
   end function logical_array_to_string

   pure function derived_type_to_string(value) result(str)
      class(to_string_t), intent(in) :: value
      character(len=:), allocatable :: str

      str = value%to_string()
   end function derived_type_to_string

end module common_to_string
