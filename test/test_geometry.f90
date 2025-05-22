module test_geometry
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
   use geometry_point, only: point_t
   use geometry_polyline, only: polyline_t
   use geometry_polygon, only: polygon_t

   implicit none(type, external)

   private

   public :: collect_tests
contains

   subroutine collect_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_point_distance_to_point", &
                               test_point_distance_to_point), &
                  new_unittest("test_polyline_length", &
                               test_polyline_length), &
                  new_unittest("test_polygon_perimeter", &
                               test_polygon_perimeter) &
                  ]
   end subroutine collect_tests

   subroutine test_point_distance_to_point(error)
      type(error_type), allocatable, intent(out) :: error

      type(point_t), allocatable :: point1, point2
      real :: distance

      point1 = point_t(3.0, 4.0)
      point2 = point_t(0.0, 0.0)

      distance = point1%distance_to(point2)

      call check(error, distance, 5.0)
   end subroutine test_point_distance_to_point

   subroutine test_polyline_length(error)
      type(error_type), allocatable, intent(out) :: error

      type(polyline_t), allocatable :: polyline

      polyline = polyline_t( &
                 [ &
                 point_t(0.0, 0.0), &
                 point_t(3.0, 4.0), &
                 point_t(6.0, 8.0) &
                 ] &
                 )

      call check(error, polyline%length(), 10.0)
   end subroutine test_polyline_length

   subroutine test_polygon_perimeter(error)
      type(error_type), allocatable, intent(out) :: error

      type(polygon_t), allocatable :: polygon

      polygon = polygon_t( &
                [ &
                point_t(0.0, 0.0), &
                point_t(3.0, 4.0), &
                point_t(0.0, 0.0) &
                ] &
                )

      call check(error, polygon%perimeter(), 10.0)
   end subroutine test_polygon_perimeter
end module test_geometry
