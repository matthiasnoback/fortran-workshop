module test_ray_optics
   use common_precision, only: dp
   use testdrive, only: new_unittest, unittest_type, error_type, test_failed, skip_test, check
   use ray_optics_intersection_and_reflection, only: ray_segment_intersection, &
                                                     ray_circle_intersection, &
                                                     apply_surface_material
   use ray_optics_library, only: line_segment_t, &
                                 ray_t, &
                                 point_t, &
                                 ray_line_segment_intersection

   implicit none(type, external)

   private

   public :: collect_tests

contains

   subroutine collect_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_segment_intersection_legacy", &
                               test_segment_intersection_legacy), &
                  new_unittest("test_circle_intersection_legacy", &
                               test_circle_intersection_legacy), &
                  new_unittest("test_mirror_reflected_direction", &
                               test_mirror_reflected_direction) &
                  ]
   end subroutine collect_tests

   subroutine test_segment_intersection_legacy(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: x0, y0, dx, dy, tmin, tmax, eps
      real(dp) :: xs, ys, xe, ye, tseg
      real(dp) :: cx, cy, r
      real(dp) :: outdx, outdy
      type(line_segment_t), allocatable :: line_segment
      type(ray_t), allocatable :: ray
      type(point_t), allocatable :: intersection
      logical :: intersected

      ! Ray setup
      x0 = 0.0_dp; y0 = 0.0_dp
      dx = 1.0_dp; dy = 1.0_dp
      tmin = 0.0_dp; tmax = 100.0_dp
      eps = 1.0e-9_dp

      ! Segment
      xs = 0.0_dp; ys = 2.0_dp
      xe = 2.0_dp; ye = 0.0_dp

      tseg = ray_segment_intersection(x0, y0, dx, dy, xs, ys, xe, ye, tmin, tmax, eps)
      call check(error, tseg, sqrt(2.0_dp), thr=0.0001_dp)
      if (allocated(error)) then
         return
      end if

      ! Using library
      ray = ray_t(point_t(x0, y0), dx, dy)

      line_segment = line_segment_t(point_t(xs, ys), point_t(xe, ye))

      call ray_line_segment_intersection(ray, line_segment, intersected, intersection)
      call check(error, intersected, .true.)
      if (allocated(error)) then
         return
      end if

      call check(error, intersection%x, 1.0_dp, thr=0.0001_dp)
      if (allocated(error)) then
         return
      end if

      call check(error, intersection%y, 1.0_dp, thr=0.0001_dp)
      if (allocated(error)) then
         return
      end if

   end subroutine test_segment_intersection_legacy

   subroutine test_circle_intersection_legacy(error)
      type(error_type), allocatable, intent(out) :: error

      real(dp) :: x0, y0, dx, dy, tmin, tmax, eps
      real(dp) :: tcirc
      real(dp) :: cx, cy, r
      real(dp) :: outdx, outdy

      ! Ray
      x0 = 0.0_dp; y0 = 0.0_dp
      dx = 1.0_dp; dy = 0.2_dp
      tmin = 0.0_dp; tmax = 100.0_dp
      eps = 1.0e-9_dp

      ! Circle
      cx = 3.0_dp; cy = 0.6_dp; r = 1.0_dp

      tcirc = ray_circle_intersection(x0, y0, dx, dy, cx, cy, r, tmin, tmax, eps)

      call check(error, tcirc, 2.01941932_dp, thr=0.00001_dp)
   end subroutine test_circle_intersection_legacy

   subroutine test_mirror_reflected_direction(error)
      type(error_type), allocatable, intent(out) :: error
      real(dp) :: x0, y0, dx, dy
      real(dp) :: xs, ys, xe, ye
      real(dp) :: outdx, outdy

      ! Ray
      x0 = 0.0_dp; y0 = 0.0_dp
      dx = 1.0_dp; dy = 0.2_dp

      ! Segment
      xs = 5.0_dp; ys = -2.0_dp
      xe = 5.0_dp; ye = 2.0_dp

      call apply_surface_material('M', dx, dy, xs, ys, xe, ye, outdx, outdy)

      call check(error, outdx, -1.0_dp)
      if (allocated(error)) then
         return
      end if

      call check(error, outdy, 0.2_dp)
   end subroutine test_mirror_reflected_direction

end module test_ray_optics
