
program integration_demo
   use iso_fortran_env, only: wp => real64
   use integration_trapezoid, only: integrate_trapezoid
   use integration_damped_oscillator, only: damped_oscillatory_function, a, b

   implicit none(type, external)

   real(kind=wp) :: integral

   a = 1.0_wp
   b = 2.0_wp

   integral = integrate_trapezoid(damped_oscillatory_function, 0.0_wp, 10.0_wp, 100)

   print *, 'Integral', integral

end program integration_demo
