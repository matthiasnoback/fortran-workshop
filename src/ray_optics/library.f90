module ray_optics_library
   use common_precision, only: dp

   implicit none(type, external)

   private
   public :: point_t
   public :: ray_t
   public :: line_segment_t
   public :: ray_line_segment_intersection

   real(kind=dp), parameter :: eps = 0.00000001_dp

   type point_t
      real(kind=dp) :: x, y
   end type point_t

   type :: ray_t
      type(point_t) :: origin
      real(kind=dp) :: dx, dy
   end type ray_t

   type :: line_segment_t
      type(point_t) :: start
      type(point_t) :: end
   end type line_segment_t

contains

   subroutine ray_line_segment_intersection(ray, line_segment, intersects, intersection)
      type(ray_t), intent(in) :: ray
      type(line_segment_t), intent(in) :: line_segment
      logical, intent(out) :: intersects
      type(point_t), allocatable, intent(out) :: intersection

      real(kind=dp) :: rx, ry, apx, apy, denom, t_candidate, u

      intersects = .false.

      ! Segment direction
      rx = line_segment%end%x - line_segment%start%x
      ry = line_segment%end%y - line_segment%start%y

      ! Vector from ray origin to segment start
      apx = line_segment%start%x - ray%origin%x
      apy = line_segment%start%y - ray%origin%y

      denom = cross(ray%dx, ray%dy, rx, ry)

      ! Parallel: no intersection
      if (abs(denom) < eps) then
         return
      end if

      ! Candidate ray parameter
      t_candidate = cross(apx, apy, rx, ry)/denom
      u = cross(apx, apy, ray%dx, ray%dy)/denom

      ! Valid only if ray hits within range and segment bounds
      if (u >= 0.0_dp .and. u <= 1.0_dp) then
         intersection = point_t(ray%origin%x + t_candidate*ray%dx, &
                                ray%origin%y + t_candidate*ray%dy)
         intersects = .true.
      end if
   end subroutine ray_line_segment_intersection

   pure function cross(ax, ay, bx, by) result(c)
      ! 2D "scalar cross product": a x b = ax*by - ay*bx
      real(dp), intent(in) :: ax, ay, bx, by
      real(dp) :: c
      c = ax*by - ay*bx
   end function cross
end module ray_optics_library
