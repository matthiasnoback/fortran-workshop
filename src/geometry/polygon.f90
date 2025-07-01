module geometry_polygon
   use geometry_polyline, only: polyline_t

   implicit none(type, external)

   private
   public :: polygon_t

   !> A polygon is a closed polyline
   !> The first and last points are the same
   type :: polygon_t
      type(polyline_t), allocatable :: polyline
   contains
      !> Calculate the perimeter of the polygon
      procedure :: perimeter => polygon_perimeter
   end type polygon_t

contains
   pure function polygon_perimeter(self) result(res)
      class(polygon_t), intent(in) :: self
      real :: res

      ! The perimeter is the same as the length of the polyline
      res = self%polyline%length()
   end function polygon_perimeter
end module geometry_polygon
