module geometry_polyline
   use geometry_point, only: point_t

   implicit none(type, external)

   private
   public :: polyline_t

   type :: polyline_t
      type(point_t), dimension(:), allocatable :: points
   contains
      procedure :: length => polyline_length
   end type polyline_t

contains

   pure function polyline_length(self) result(res)
      class(polyline_t), intent(in) :: self
      real :: res
      integer :: i

      res = 0.0
      do i = 1, size(self%points) - 1
         res = res + self%points(i)%distance_to(self%points(i + 1))
      end do
   end function polyline_length

end module geometry_polyline
