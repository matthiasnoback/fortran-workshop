module hydrology_simulation_state
   use iso_fortran_env, only: real64

   implicit none(type, external)

   private
   public :: simulation_state_t
   public :: forcings_t

   type :: simulation_state_t
      real(real64) :: PET, AET ! potential and actual evapotranspiration (mm/day)
      real(real64) :: Q ! quick runoff (mm/day)
      real(real64) :: S ! soil water storage (mm)
      ! TODO provide getters
   end type simulation_state_t

   type :: forcings_t
      character(len=:), allocatable :: date
      real(real64) :: P, T ! precipitation (mm/day), temperature (°C)
   end type forcings_t

end module hydrology_simulation_state
