module test_geometry
   use common_precision, only: dp
   use testdrive, only: new_unittest, unittest_type, error_type, test_failed
   use test_custom_checks, only: check
   use geometry_point, only: point_t, create_point, point_or_error_t, translate, abstract_point_t
   use common_strings, only: string_t
   use common_error_handling, only: error_t
   use geometry_polyline, only: polyline_t
   use geometry_polygon, only: polygon_t

   implicit none(type, external)

   private

   public :: collect_tests

   interface check
      procedure :: check_point_or_error
   end interface check
contains

   subroutine collect_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_point_distance_to_point", &
                               test_point_distance_to_point), &
                  new_unittest("test_translate_point", &
                               test_translate_point), &
                  new_unittest("test_parse_valid_point", &
                               test_parse_valid_point), &
                  new_unittest("test_parse_invalid_point", &
                               test_parse_invalid_point), &
                  new_unittest("test_polyline_length", &
                               test_polyline_length), &
                  new_unittest("test_polygon_perimeter", &
                               test_polygon_perimeter) &
                  ]
   end subroutine collect_tests

   subroutine test_point_distance_to_point(error)
      type(error_type), allocatable, intent(out) :: error

      class(abstract_point_t), allocatable :: point1, point2
      real(kind=dp) :: distance

      point1 = create_point(3.0_dp, 4.0_dp)
      point2 = create_point(0.0_dp, 0.0_dp)

      distance = point1%distance_to(point2)

      call check(error, distance, 5.0_dp)
   end subroutine test_point_distance_to_point

   subroutine test_parse_valid_point(error)
      type(error_type), allocatable, intent(out) :: error

      type(point_or_error_t) :: actual
      type(point_or_error_t) :: expected

      ! TODO implement parse_point and replace the following line with
      ! actual = parse_point(string_t('1.0 2.0'))
      actual%point = create_point(1.0_dp, 2.0_dp)

      expected%point = create_point(1.0_dp, 2.0_dp)

      call check(error, actual, expected)
   end subroutine test_parse_valid_point

   subroutine test_parse_invalid_point(error)
      type(error_type), allocatable, intent(out) :: error

      type(point_or_error_t) :: actual
      type(point_or_error_t) :: expected

      ! TODO implement parse_point and replace the following line with
      ! actual = parse_point(string_t('1.0 abc'))
      expected%error = error_t('Failed to parse point string: 1.0 abc')

      call check(error, actual, expected)
   end subroutine test_parse_invalid_point

   subroutine test_polyline_length(error)
      type(error_type), allocatable, intent(out) :: error

      type(polyline_t), allocatable :: polyline

      ! polyline = polyline_t( &
      !            [ &
      !            create_point(0.0_dp, 0.0_dp), &
      !            create_point(3.0_dp, 4.0_dp), &
      !            create_point(6.0_dp, 8.0_dp) &
      !            ] &
      !            )

      ! call check(error, polyline%length(), 10.0_dp)
   end subroutine test_polyline_length

   subroutine test_polygon_perimeter(error)
      type(error_type), allocatable, intent(out) :: error

      type(polygon_t), allocatable :: polygon

      ! polygon = polygon_t(polyline_t( &
      !                     [ &
      !                     create_point(0.0_dp, 0.0_dp), &
      !                     create_point(3.0_dp, 4.0_dp), &
      !                     create_point(0.0_dp, 0.0_dp) &
      !                     ] &
      !                     ))

      ! call check(error, polygon%perimeter(), 10.0_dp)
   end subroutine test_polygon_perimeter

   subroutine check_point_or_error(error, actual, expected)
      type(error_type), allocatable, intent(out) :: error
      type(point_or_error_t), intent(in) :: actual
      type(point_or_error_t), intent(in) :: expected

      ! TODO implement a custom check that verifies `actual` is equal to `expected`
   end subroutine check_point_or_error

   subroutine test_translate_point(error)
      type(error_type), allocatable, intent(out) :: error

      class(abstract_point_t), allocatable :: original
      class(abstract_point_t), allocatable :: translated

      original = create_point(10.0_dp, 5.0_dp)

      translated = translate(original, 2.0_dp, -2.0_dp)

      call check(error, translated%get_x(), 12.0_dp)
      if (allocated(error)) then
         return
      end if

      call check(error, translated%get_y(), 3.0_dp)
      if (allocated(error)) then
         return
      end if
   end subroutine test_translate_point

end module test_geometry
