module ray_optics_intersection_and_reflection
   use common_precision, only: dp
   use ray_optics_library, only: ray_line_segment_intersection, &
                                 ray_t, &
                                 point_t, &
                                 line_segment_t

   implicit none(type, external)

   private
   public :: ray_segment_intersection
   public :: ray_circle_intersection
   public :: apply_surface_material

   type, abstract :: material_t
   contains
      procedure(ray_direction_after_hit_proc), deferred :: ray_direction_after_hit
   end type material_t

   type, extends(material_t) :: mirror_t
   contains
      procedure :: ray_direction_after_hit => mirror_ray_direction_after_hit
   end type mirror_t

   type, extends(material_t) :: absorber_t
   contains
      procedure :: ray_direction_after_hit => absorber_ray_direction_after_hit
   end type absorber_t

   type, extends(material_t) :: transmitter_t
   contains
      procedure :: ray_direction_after_hit => transmitter_ray_direction_after_hit
   end type transmitter_t

   abstract interface
      subroutine ray_direction_after_hit_proc(self, dx, dy, x1, y1, x2, y2, outdx, outdy)
         import :: dp, material_t

         implicit none(type, external)

         class(material_t), intent(in) :: self
         real(dp), intent(in)  :: dx, dy
         real(dp), intent(in)  :: x1, y1, x2, y2
         real(dp), intent(out) :: outdx, outdy
      end subroutine ray_direction_after_hit_proc
   end interface

contains

   pure function dot(ax, ay, bx, by) result(d)
      real(dp), intent(in) :: ax, ay, bx, by
      real(dp) :: d
      d = ax*bx + ay*by
   end function dot

   pure function cross(ax, ay, bx, by) result(c)
      ! 2D "scalar cross product": a x b = ax*by - ay*bx
      real(dp), intent(in) :: ax, ay, bx, by
      real(dp) :: c
      c = ax*by - ay*bx
   end function cross

   pure function norm(ax, ay) result(n)
      real(dp), intent(in) :: ax, ay
      real(dp) :: n
      n = sqrt(ax*ax + ay*ay)
   end function norm

   ! Ray vs. line segment intersection
   ! Ray: P(t) = (x0, y0) + t*(dx, dy), t in [tmin, tmax]
   ! Segment: A(x1, y1) -> B(x2, y2)
   ! Returns hit parameter t; returns -1 if no hit.
   pure function ray_segment_intersection( &
      x0, y0, dx, dy, x1, y1, x2, y2, tmin, tmax, eps) result(t)
      real(dp), intent(in) :: x0, y0, dx, dy
      real(dp), intent(in) :: x1, y1, x2, y2
      real(dp), intent(in) :: tmin, tmax, eps
      real(dp) :: t
      real(dp) :: rx, ry, apx, apy, denom, t_candidate, u

      type(ray_t), allocatable :: ray
      type(line_segment_t), allocatable :: line_segment
      logical :: intersects
      type(point_t), allocatable :: intersection

      ray = ray_t(point_t(x0, y0), dx, dy)
      line_segment = line_segment_t(point_t(x1, y1), point_t(x2, y2))

      call ray_line_segment_intersection(ray, line_segment, intersects, intersection)

      if (.not. intersects) then
         t = -1.0_dp
         return
      end if

      t = sqrt((intersection%x - x0)**2 + (intersection%y - y0))
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
      a = dot(dx, dy, dx, dy)
      b = 2.0_dp*dot(ocx, ocy, dx, dy)
      c = dot(ocx, ocy, ocx, ocy) - r*r
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
      nlen = norm(nx, ny)
      if (nlen > 1.0e-12_dp) then
         nx = nx/nlen
         ny = ny/nlen
      else
         ! Degenerate segment; return original direction
         outdx = dx
         outdy = dy
         return
      end if

      ndot = dot(dx, dy, nx, ny)
      outdx = dx - 2.0_dp*ndot*nx
      outdy = dy - 2.0_dp*ndot*ny
   end subroutine reflect_on_segment

   subroutine apply_surface_material(material_name, dx, dy, x1, y1, x2, y2, outdx, outdy)
      character(len=*), intent(in) :: material_name
      real(dp), intent(in)  :: dx, dy
      real(dp), intent(in)  :: x1, y1, x2, y2
      real(dp), intent(out) :: outdx, outdy

      class(material_t), allocatable :: material

      material = create_material(material_name)

      call material%ray_direction_after_hit(dx, dy, x1, y1, x2, y2, outdx, outdy)
   end subroutine apply_surface_material

   pure function create_material(name) result(material)
      character(len=*), intent(in) :: name

      class(material_t), allocatable :: material
      select case (name)
      case ('M')
         material = mirror_t()
      case ('A')
         material = absorber_t()
      case default
      case ('T')
         material = transmitter_t()
      end select
   end function create_material

   subroutine mirror_ray_direction_after_hit(self, dx, dy, x1, y1, x2, y2, outdx, outdy)
      class(mirror_t), intent(in) :: self
      real(dp), intent(in)  :: dx, dy
      real(dp), intent(in)  :: x1, y1, x2, y2
      real(dp), intent(out) :: outdx, outdy

      call reflect_on_segment(x1, y1, x2, y2, dx, dy, outdx, outdy)
   end subroutine mirror_ray_direction_after_hit

   subroutine absorber_ray_direction_after_hit(self, dx, dy, x1, y1, x2, y2, outdx, outdy)
      class(absorber_t), intent(in) :: self
      real(dp), intent(in)  :: dx, dy
      real(dp), intent(in)  :: x1, y1, x2, y2
      real(dp), intent(out) :: outdx, outdy

      outdx = 0.0_dp
      outdy = 0.0_dp
   end subroutine absorber_ray_direction_after_hit

   subroutine transmitter_ray_direction_after_hit(self, dx, dy, x1, y1, x2, y2, outdx, outdy)
      class(transmitter_t), intent(in) :: self
      real(dp), intent(in)  :: dx, dy
      real(dp), intent(in)  :: x1, y1, x2, y2
      real(dp), intent(out) :: outdx, outdy

      outdx = dx
      outdy = dy
   end subroutine transmitter_ray_direction_after_hit

end module ray_optics_intersection_and_reflection
