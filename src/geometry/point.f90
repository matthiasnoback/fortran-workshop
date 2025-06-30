module geometry_point
   use common_error_handling, only: error_t
   use common_strings, only: string_t

   implicit none(type, external)

   private
   public :: point_t
   public :: point_or_error_t
   public :: parse_point

   !> Represents a point in 2D space
   type :: point_t
      real :: x
      real :: y
   contains
      !> Calculate the distince to another point
      procedure :: distance_to => point_distance_to
   end type point_t

   type :: point_or_error_t
      type(error_t), allocatable :: error
      type(point_t), allocatable :: point
   end type point_or_error_t

contains

   pure function point_distance_to(self, other) result(res)
      class(point_t), intent(in) :: self
      class(point_t), intent(in) :: other
      real :: res

      res = sqrt((self%x - other%x)**2 + (self%y - other%y)**2)
   end function point_distance_to

   function parse_point(point_string) result(res)
      type(string_t), intent(in) :: point_string
      type(point_or_error_t) :: res

      integer :: iostat
      real :: x
      real :: y

      read (point_string%value, *, iostat=iostat) x, y

      if (iostat /= 0) then
         res%error = error_t('Failed to parse point from string: '//point_string%value)
         return
      end if

      res%point = point_t(x, y)
   end function parse_point

end module geometry_point
