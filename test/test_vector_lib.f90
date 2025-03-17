module test_vector_lib
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
   use m_vector, only: t_3d_vector
   use iso_fortran_env, only: wp => real64

   implicit none(type, external)

   private

   public :: collect_tests

contains

   subroutine collect_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_3d_vector_distance", test_3d_vector_distance), &
                  new_unittest("test_3d_vector_add", test_3d_vector_add), &
                  new_unittest("test_3d_vector_export", test_3d_vector_export) &
                  ]

   end subroutine collect_tests

   subroutine test_3d_vector_distance(error)
      type(error_type), allocatable, intent(out) :: error

      type(t_3d_vector) :: vector1
      type(t_3d_vector) :: vector2

      ! sqrt( (4-1)**2 + (6-2)**2 + (8-3)**2 )
      ! = sqrt( 9 + 16 + 25)
      ! = sqrt(50)
      vector1 = t_3d_vector(4.0_wp, 6.0_wp, 8.0_wp)
      vector2 = t_3d_vector(1.0_wp, 2.0_wp, 3.0_wp)

      call check(error, vector1%distance_to(vector2), sqrt(50.0_wp))
   end subroutine test_3d_vector_distance

   subroutine test_3d_vector_add(error)
      type(error_type), allocatable, intent(out) :: error

      type(t_3d_vector) :: vector1
      type(t_3d_vector) :: vector2
      type(t_3d_vector) :: expected
      type(t_3d_vector) :: actual

      vector1 = t_3d_vector(4.0_wp, 6.0_wp, 8.0_wp)
      vector2 = t_3d_vector(1.0_wp, 2.0_wp, 3.0_wp)

      expected = t_3d_vector(5.0_wp, 8.0_wp, 11.0_wp)
      actual = vector1%add_to(vector2)

      call check(error, expected%v1, actual%v1)
      call check(error, expected%v2, actual%v2)
      call check(error, expected%v3, actual%v3)
   end subroutine test_3d_vector_add

   subroutine test_3d_vector_export(error)
      type(error_type), allocatable, intent(out) :: error

      type(t_3d_vector) :: vector

      vector = t_3d_vector(4.0_wp, 6.0_wp, 8.0_wp)

      call check(error, vector%export_values(), 'v1=4.00000000000000v2=6.00000000000000v3=8.00000000000000')
   end subroutine test_3d_vector_export

end module test_vector_lib
