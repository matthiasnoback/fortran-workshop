module damped_oscillator
   use iso_fortran_env, only: wp => real64

   implicit none(type, external)

   private
   public :: a, b, damped_oscillatory_function

   real(kind=wp) :: a
   real(kind=wp) :: b

contains

   function damped_oscillatory_function(x) result(y)
      real(kind=wp), intent(in) :: x
      real(kind=wp):: y

      y = exp(-a*x)*cos(b*x)
   end function damped_oscillatory_function

end module damped_oscillator
