module ray_optics_vector_2d
   use common_precision, only: dp

   implicit none(type, external)

   private
   public :: point_2d_t
   public :: vector_2d_t

   type :: point_2d_t
      real(dp) :: x
      real(dp) :: y
   end type point_2d_t

   type :: vector_2d_t
      real(dp) :: dx
      real(dp) :: dy
   end type vector_2d_t
contains

end module ray_optics_vector_2d
