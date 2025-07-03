module geometry_polyline
   use common_error_handling, only: error_t
   use geometry_point, only: point_t, point_or_error_t, parse_point
   use common_strings, only: string_t

   implicit none(type, external)

   private
   public :: polyline_t

   !> A polyline is a series of connected points
   type :: polyline_t
      type(point_t), dimension(:), allocatable :: points
   contains
      !> Calculate the length of the polyline
      procedure :: length => polyline_length
   end type polyline_t

   type :: polyline_or_error_t
      type(error_t), allocatable :: error
      type(polyline_t), allocatable :: polyline
   end type polyline_or_error_t

contains

   pure function parse_polyline(strings) result(polyline_or_error)
      type(string_t), dimension(:), intent(in) :: strings
      type(polyline_or_error_t) :: polyline_or_error

      type(point_or_error_t), dimension(:), allocatable :: point_or_errors
      integer :: index

      point_or_errors = parse_point(strings)

      do index = 1, size(point_or_errors)
         if (allocated(point_or_errors(index)%error)) then
            polyline_or_error%error = error_t('Could not parse polyline', &
                                              point_or_errors(index)%error)
            return
         end if
      end do

      polyline_or_error%polyline = polyline_t([(point_or_errors(index)%point, &
                                                index=1, size(point_or_errors))])
   end function parse_polyline

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
