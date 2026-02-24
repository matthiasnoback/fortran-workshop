module geometry_polyline
   use common_precision, only: dp
   use common_error_handling, only: error_t
   use geometry_point, only: point_t, point_t, create_point, point_wrapper_t

   implicit none(type, external)

   private
   public :: polyline_t
   public :: create_polyline

   !> A polyline is a series of connected points
   type :: polyline_t
      type(point_wrapper_t), dimension(:), allocatable :: points
   contains
      !> Calculate the length of the polyline
      procedure :: length => polyline_length
   end type polyline_t

   type :: polyline_or_error_t
      type(error_t), allocatable :: error
      type(polyline_t), allocatable :: polyline
   end type polyline_or_error_t

contains

   function create_polyline(x_and_y_values) result(polyline)
      real(kind=dp), dimension(:), intent(in) :: x_and_y_values

      type(polyline_t) :: polyline

      real(kind=dp), dimension(:, :), allocatable :: points
      integer :: i

      points = reshape(x_and_y_values, [3, 2], order=[2, 1])

      polyline%points = [(point_wrapper_t(create_point(points(i, 1), points(i, 2))), &
                          i=1, size(points, 1))]
   end function create_polyline

   pure function polyline_length(self) result(res)
      class(polyline_t), intent(in) :: self
      real(kind=dp) :: res
      integer :: i

      res = 0.0
      do i = 1, size(self%points) - 1
         res = res + self%points(i)%point%distance_to(self%points(i + 1)%point)
      end do
   end function polyline_length

end module geometry_polyline
