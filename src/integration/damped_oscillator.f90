module integration_damped_oscillator
   use iso_fortran_env, only: wp => real64
   use integration_trapezoid, only: user_function_t

   implicit none(type, external)

   private
   public :: damped_oscillator_t

   type, extends(user_function_t) :: damped_oscillator_t
      real(kind=wp) :: a
      real(kind=wp) :: b
   contains
      procedure :: evaluate => damped_oscillator_evaluate
   end type damped_oscillator_t

contains

   function damped_oscillator_evaluate(self, x) result(y)
      class(damped_oscillator_t), intent(in) :: self
      real(kind=wp), intent(in) :: x
      real(kind=wp):: y

      y = exp(-self%a*x)*cos(self%b*x)
   end function damped_oscillator_evaluate

end module integration_damped_oscillator
