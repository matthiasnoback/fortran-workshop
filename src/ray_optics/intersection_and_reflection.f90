module ray_optics_intersection_and_reflection
   use common_precision, only: dp
   use ray_optics_vector_2d, only: point_2d_t, vector_2d_t, dot, cross, norm

   implicit none(type, external)

   private
   public :: ray_segment_intersection
   public :: ray_t
   public :: make_ray
   public :: ray_circle_intersection
   public :: apply_surface_material

   type :: ray_t
      private
      type(point_2d_t) :: origin
      type(vector_2d_t) :: direction
   end type ray_t

contains

   pure function make_ray(x0, y0, dx, dy) result(ray)
      real(dp), intent(in) :: x0, y0, dx, dy

      type(ray_t) :: ray

      ray = ray_t(point_2d_t(x0, y0), vector_2d_t(dx, dy))
   end function make_ray

   ! Ray vs. line segment intersection
   ! Ray: P(t) = (x0, y0) + t*(dx, dy), t in [tmin, tmax]
   ! Segment: A(x1, y1) -> B(x2, y2)
   ! Returns hit parameter t; returns -1 if no hit.
   pure function ray_segment_intersection( &
      ray, x1, y1, x2, y2, tmin, tmax, eps) result(t)
      type(ray_t), intent(in) :: ray
      real(dp), intent(in) :: x1, y1, x2, y2
      real(dp), intent(in) :: tmin, tmax, eps
      real(dp) :: t

      real(dp) :: rx, ry, apx, apy, denom, t_candidate, u
      real(dp) :: x0, y0, dx, dy

      x0 = ray%origin%x
      y0 = ray%origin%y
      dx = ray%direction%dx
      dy = ray%direction%dy

      rx = x2 - x1
      ry = y2 - y1
      apx = x1 - x0
      apy = y1 - y0
      denom = ray%direction%cross(vector_2d_t(rx, ry))
      if (abs(denom) < eps) then
         t = -1.0_dp
         return
      end if
      associate (a => vector_2d_t(apx, apy))
         t_candidate = a%cross(vector_2d_t(rx, ry))/denom
         u = a%cross(ray%direction)/denom
      end associate
      if (t_candidate >= tmin .and. t_candidate <= tmax .and. u >= 0.0_dp .and. u <= 1.0_dp) then
         t = t_candidate
      else
         t = -1.0_dp
      end if
   end function ray_segment_intersection

   ! Ray vs. circle intersection
   ! Circle centered at (cx, cy) with radius r
   ! Returns smallest non-negative t in [tmin, tmax] or -1 if no hit.
   pure function ray_circle_intersection( &
      x0, y0, dx, dy, cx, cy, r, tmin, tmax, eps) result(t)
      real(dp), intent(in) :: x0, y0, dx, dy
      real(dp), intent(in) :: cx, cy, r, tmin, tmax, eps
      real(dp) :: t
      real(dp) :: ocx, ocy, a, b, c, disc, t1, t2, nrm

      ocx = x0 - cx
      ocy = y0 - cy
      a = dot(vector_2d_t(dx, dy), vector_2d_t(dx, dy))
      b = 2.0_dp*dot(vector_2d_t(ocx, ocy), vector_2d_t(dx, dy))
      c = dot(vector_2d_t(ocx, ocy), vector_2d_t(ocx, ocy)) - r*r
      disc = b*b - 4.0_dp*a*c

      if (disc < 0.0_dp) then
         t = -1.0_dp
         return
      end if

      ! Two roots; pick the smallest t in [tmin, tmax] and >= 0
      nrm = sqrt(disc)
      t1 = (-b - nrm)/(2.0_dp*a)
      t2 = (-b + nrm)/(2.0_dp*a)

      t = -1.0_dp
      if (t1 >= tmin .and. t1 <= tmax .and. t1 >= 0.0_dp) t = t1
      if (t == -1.0_dp) then
         if (t2 >= tmin .and. t2 <= tmax .and. t2 >= 0.0_dp) t = t2
      end if
   end function ray_circle_intersection

   ! Reflect a ray direction off a segment (using a naive normal)
   ! out direction = d - 2*(d·n̂)*n̂
   ! NOTE: Chooses a normal perpendicular to the segment without regard
   ! to which side the ray hits.
   pure subroutine reflect_on_segment(x1, y1, x2, y2, dx, dy, outdx, outdy)
      real(dp), intent(in)  :: x1, y1, x2, y2
      real(dp), intent(in)  :: dx, dy
      real(dp), intent(out) :: outdx, outdy
      real(dp) :: rx, ry, nx, ny, nlen, ndot

      rx = x2 - x1
      ry = y2 - y1
      nx = -ry
      ny = rx
      nlen = norm(vector_2d_t(nx, ny))
      if (nlen > 1.0e-12_dp) then
         nx = nx/nlen
         ny = ny/nlen
      else
         ! Degenerate segment; return original direction
         outdx = dx
         outdy = dy
         return
      end if

      ndot = dot(vector_2d_t(dx, dy), vector_2d_t(nx, ny))
      outdx = dx - 2.0_dp*ndot*nx
      outdy = dy - 2.0_dp*ndot*ny
   end subroutine reflect_on_segment

   subroutine apply_surface_material(material, dx, dy, x1, y1, x2, y2, outdx, outdy)
      character(len=*), intent(in) :: material
      real(dp), intent(in)  :: dx, dy
      real(dp), intent(in)  :: x1, y1, x2, y2
      real(dp), intent(out) :: outdx, outdy
      select case (material)
      case ('M') ! perfect mirror (reflect)
         call reflect_on_segment(x1, y1, x2, y2, dx, dy, outdx, outdy)
      case ('A') ! absorber (stop)
         outdx = 0.0_dp
         outdy = 0.0_dp
      case ('T') ! transmitter (keep direction)
         outdx = dx
         outdy = dy
      case default
         ! Unknown: pass-through
         outdx = dx
         outdy = dy
      end select
   end subroutine apply_surface_material

end module ray_optics_intersection_and_reflection
