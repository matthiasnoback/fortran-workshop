module hydraulic_structures_pump
   use common_error_handling, only: error_t
   use iso_fortran_env, only: real64

   implicit none(type, external)

   private
   public :: pump_specification_t
   public :: pump_specification_or_error_t
   public :: calculate_pump_discharge

   type :: pump_state_t
      logical :: is_running
      real(kind=real64) :: discharge
   end type pump_state_t

   type pump_specification_t
      real(kind=real64) :: capacity
      real(kind=real64) :: start_level
      real(kind=real64) :: stop_level
   end type pump_specification_t

   type :: pump_specification_or_error_t
      type(pump_specification_t), allocatable :: pump
      type(error_t), allocatable :: error
   end type pump_specification_or_error_t

contains

   pure function calculate_pump_discharge(specification, &
                                          actual_level) result(discharge)
      type(pump_specification_t), intent(in) :: specification
      real(kind=real64), intent(in) :: actual_level

      real(kind=real64) :: discharge

      if (actual_level > specification%start_level) then
         discharge = specification%capacity
         return
      end if

      ! if (actual_level < stop_level) then
      !    discharge = 0.0_real64
      !    return
      ! end if

      discharge = 0.0_real64
   end function calculate_pump_discharge

end module hydraulic_structures_pump
