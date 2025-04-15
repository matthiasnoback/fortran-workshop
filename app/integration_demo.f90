
program integration_demo
   use iso_fortran_env, only: wp => real64
   use integration_trapezoid, only: integrate_trapezoid
   use integration_damped_oscillator, only: damped_oscillator_t

   implicit none(type, external)

   real(kind=wp) :: integral

   integral = integrate_trapezoid(damped_oscillator_t(1.0_wp, 2.0_wp), 0.0_wp, 10.0_wp, 100)

   print *, 'Integral', integral

end program integration_demo
