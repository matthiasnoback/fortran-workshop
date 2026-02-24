module geometry_polyline
   use common_precision, only: dp
   use common_error_handling, only: error_t
   use geometry_point, only: point_t, abstract_point_t

   implicit none(type, external)

   private
   public :: polyline_t

   !> A polyline is a series of connected points
   type :: polyline_t
      class(abstract_point_t), dimension(:), allocatable :: points
   contains
      !> Calculate the length of the polyline
      procedure :: length => polyline_length
   end type polyline_t

   type :: polyline_or_error_t
      type(error_t), allocatable :: error
      type(polyline_t), allocatable :: polyline
   end type polyline_or_error_t

contains

   pure function polyline_length(self) result(res)
      class(polyline_t), intent(in) :: self
      real(kind=dp) :: res
      integer :: i

      res = 0.0
      do i = 1, size(self%points) - 1
         res = res + self%points(i)%distance_to(self%points(i + 1))
      end do
   end function polyline_length

end module geometry_polyline
