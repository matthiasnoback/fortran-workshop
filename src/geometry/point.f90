module geometry_point
   use common_error_handling, only: error_t

   implicit none(type, external)

   private
   public :: point_t
   public :: point_or_error_t

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

end module geometry_point
