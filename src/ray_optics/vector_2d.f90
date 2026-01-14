module ray_optics_vector_2d
   use common_precision, only: dp

   implicit none(type, external)

   private
   public :: point_2d_t
   public :: vector_2d_t
   public :: dot, cross, norm

   type :: point_2d_t
      real(dp) :: x
      real(dp) :: y
   end type point_2d_t

   type :: vector_2d_t
      real(dp) :: dx
      real(dp) :: dy
   contains
      procedure :: cross
   end type vector_2d_t
contains

   pure function dot(a, b) result(d)
      type(vector_2d_t), intent(in) :: a
      type(vector_2d_t), intent(in) :: b
      real(dp) :: d
      d = a%dx*b%dx + a%dy*b%dy
   end function dot

   pure function cross(a, b) result(c)
      ! 2D "scalar cross product": a x b = ax*by - ay*bx
      class(vector_2d_t), intent(in) :: a
      type(vector_2d_t), intent(in) :: b
      real(dp) :: c
      c = a%dx*b%dy - a%dy*b%dx
   end function cross

   pure function norm(a) result(n)
      type(vector_2d_t), intent(in) :: a
      real(dp) :: n
      n = sqrt(dot(a, a))
   end function norm

end module ray_optics_vector_2d
