module hydraulic_structures_pump
   use common_error_handling, only: error_t
   use common_precision, only: dp

   implicit none(type, external)

   private
   public :: pump_specification_t
   public :: pump_specification_or_error_t
   public :: next_pump_state
   public :: pump_state_t

   type pump_state_t
      real(kind=dp) :: discharge
      logical :: is_running
   end type pump_state_t

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

   pure function next_pump_state(pump_specification, &
                                 previous_state, &
                                 water_level_suction_side &
                                 ) result(next_state)
      type(pump_specification_t), intent(in) :: pump_specification
      type(pump_state_t), intent(in) :: previous_state
      real(kind=dp), intent(in) :: water_level_suction_side

      type(pump_state_t) :: next_state

      if (water_level_suction_side >= pump_specification%starting_water_level) then
         next_state%discharge = pump_specification%capacity
         next_state%is_running = .true.
      else
         next_state%discharge = 0.0_dp
         next_state%is_running = .false.
      end if
   end function next_pump_state

end module hydraulic_structures_pump
