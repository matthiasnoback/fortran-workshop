module geometry_point
   use common_precision, only: dp
   use common_error_handling, only: error_t

   implicit none(type, external)

   private
   public :: point_t
   public :: create_point
   public :: point_or_error_t
   public :: point_wrapper_t

   !> Represents a point in 2D space
   type :: point_t
      private
      real(kind=dp) :: x
      real(kind=dp) :: y
   contains
      !> Calculate the distince to another point
      procedure :: distance_to => point_distance_to
      procedure :: get_x => point_get_x
      procedure :: get_y => point_get_y
   end type point_t

   type :: point_or_error_t
      type(error_t), allocatable :: error
      class(point_t), allocatable :: point
   end type point_or_error_t

   type :: point_wrapper_t
      class(point_t), allocatable :: point
   end type point_wrapper_t

contains

   pure function create_point(x, y) result(point)
      real(kind=dp), intent(in) :: x
      real(kind=dp), intent(in) :: y

      class(point_t), allocatable :: point

      point = point_t(x, y)
   end function create_point

   pure function point_get_x(self) result(x_value)
      class(point_t), intent(in) :: self

      real(kind=dp) :: x_value
      x_value = self%x
   end function point_get_x

   pure function point_get_y(self) result(y_value)
      class(point_t), intent(in) :: self

      real(kind=dp) :: y_value
      y_value = self%y
   end function point_get_y

   pure function point_distance_to(self, other) result(res)
      class(point_t), intent(in) :: self
      class(point_t), intent(in) :: other
      real(kind=dp) :: res

      res = sqrt((self%get_x() - other%get_x())**2 + (self%get_y() - other%get_y())**2)
   end function point_distance_to

end module geometry_point
