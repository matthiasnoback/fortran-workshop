
program ray_optics_demo
   use common_precision, only: dp
   use ray_optics_intersection_and_reflection, only: ray_segment_intersection, &
                                                     ray_circle_intersection, &
                                                     apply_surface_material, &
                                                     ray_t

   implicit none(type, external)

   real(dp) :: x0, y0, dx, dy, tmin, tmax, eps
   real(dp) :: xs, ys, xe, ye, tseg, tcirc
   real(dp) :: cx, cy, r
   real(dp) :: outdx, outdy
   real(dp) :: x, y, vx, vy, ax, ay, dt
   integer :: steps
   type(ray_t) :: ray

   ! Ray setup
   ray = ray_t(x0=0.0_dp, y0=0.0_dp, dx=1.0_dp, dy=0.2_dp)
   tmin = 0.0_dp; tmax = 100.0_dp
   eps = 1.0e-9_dp

   ! Segment (a "mirror")
   xs = 5.0_dp; ys = -2.0_dp
   xe = 5.0_dp; ye = 2.0_dp

   ! Circle (an obstacle)
   cx = 3.0_dp; cy = 0.6_dp; r = 1.0_dp

   tseg = ray_segment_intersection(ray, xs, ys, xe, ye, tmin, tmax, eps)
   tcirc = ray_circle_intersection(ray%x0, ray%y0, ray%dx, ray%dy, cx, cy, r, tmin, tmax, eps)

   print *, 't(hit segment) = ', tseg
   print *, 't(hit circle)  = ', tcirc

   call apply_surface_material('M', ray%dx, ray%dy, xs, ys, xe, ye, outdx, outdy)
   print *, 'reflected dir  = ', outdx, outdy
end program ray_optics_demo
