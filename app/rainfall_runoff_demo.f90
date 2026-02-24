!===============================================================
! Ultra-minimal Hydrology Model (single soil bucket)
! --------------------------------------------------------------
! Purpose:
!   Daily water balance for a single soil bucket:
!     - Precipitation (P) either infiltrates (I) or becomes quick runoff (Q)
!     - Atmosphere demands water via Potential ET (PET)
!     - Actual ET (AET) is limited by soil water availability
!     - Soil storage S is updated each day
!
! Inputs CSV (with header):  date,P,T
!   - date : string (passed through)
!   - P    : precipitation, mm/day
!   - T    : mean air temperature, °C
!
! Output CSV: date,P,T,PET,AET,Q,S
!   - PET : potential evapotranspiration, mm/day
!   - AET : actual evapotranspiration, mm/day
!   - Q   : quick runoff (excess rain that didn’t infiltrate), mm/day
!   - S   : soil water storage after update, mm
!===============================================================
program rainfall_runoff_demo
   use iso_fortran_env, only: real64
   use hydrology_rainfall_runoff, only: run

   implicit none(type, external)

   call run()

end program rainfall_runoff_demo
