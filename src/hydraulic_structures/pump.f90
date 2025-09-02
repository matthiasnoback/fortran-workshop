module hydraulic_structures_pump
   use common_error_handling, only: error_t
   use common_precision, only: dp

   implicit none(type, external)

   private
   public :: pump_specification_t
   public :: pump_specification_or_error_t
   public :: calculate_pump_discharge

   type pump_specification_t
      real(kind=dp) :: capacity
      real(kind=dp) :: starting_water_level
      real(kind=dp) :: stopping_water_level
   end type pump_specification_t

   type :: pump_specification_or_error_t
      type(pump_specification_t), allocatable :: pump
      type(error_t), allocatable :: error
   end type pump_specification_or_error_t

contains

   pure function calculate_pump_discharge(pump_specification, &
                                          water_level_suction_side &
                                          ) result(discharge)
      type(pump_specification_t), intent(in) :: pump_specification
      real(kind=dp), intent(in) :: water_level_suction_side

      real(kind=dp) :: discharge

      if (water_level_suction_side >= pump_specification%starting_water_level) then
         discharge = pump_specification%capacity
      else
         discharge = 0.0_dp
      end if
   end function calculate_pump_discharge

end module hydraulic_structures_pump
