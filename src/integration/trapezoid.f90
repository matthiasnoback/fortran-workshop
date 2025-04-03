module integration_trapezoid
   use iso_fortran_env, only: wp => real64

   implicit none(type, external)

   private
   public :: integrate_trapezoid

   interface
      function function_to_integrate(x) result(y)
         import wp

         implicit none(type, external)

         real(kind=wp), intent(in) :: x
         real(kind=wp):: y
      end function function_to_integrate
   end interface

contains

   function integrate_trapezoid(the_function, x_min, x_max, steps) result(integral)
      procedure(function_to_integrate), pointer, intent(in) :: the_function
      real(kind=wp), intent(in) :: x_min
      real(kind=wp), intent(in) :: x_max
      integer, intent(in) :: steps
      real(kind=wp) :: integral
      integer :: step
      real(kind=wp) :: x
      real(kind=wp) :: delta_x

      if (steps <= 0) then
         integral = 0.0_wp
         return
      end if

      delta_x = (x_max - x_min)/steps

      integral = the_function(x_min) + the_function(x_max)/2.0_wp

      do step = 1, steps - 1
         x = x_min + step*delta_x
         integral = integral + the_function(x)
      end do

      integral = integral*delta_x
   end function integrate_trapezoid

end module integration_trapezoid
