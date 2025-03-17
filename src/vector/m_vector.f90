module m_vector
   use iso_fortran_env, only: wp => real64

   implicit none(type, external)

   private
   public :: t_3d_vector

   type :: t_3d_vector
      real(kind=wp) :: v1
      real(kind=wp) :: v2
      real(kind=wp) :: v3
   contains
      procedure :: distance_to => vector_distance_to
      procedure :: add_to => vector_add_to
   end type t_3d_vector

contains

   pure function vector_add_to(self, other) result(sum_vector)
      class(t_3d_vector), intent(in) :: self
      class(t_3d_vector), intent(in) :: other
      type(t_3d_vector) :: sum_vector

      sum_vector%v1 = self%v1 + other%v1
      sum_vector%v2 = self%v2 + other%v2
      sum_vector%v3 = self%v3 + other%v3
   end function vector_add_to

   pure function vector_distance_to(self, other) result(distance)
      class(t_3d_vector), intent(in) :: self
      class(t_3d_vector), intent(in) :: other

      real(kind=wp) :: distance

      distance = sqrt((self%v1-other%v1)**2 + (self%v2-other%v2)**2 + (self%v3-other%v3)**2)
   end function vector_distance_to

end module m_vector
