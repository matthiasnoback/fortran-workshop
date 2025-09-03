module hydraulic_structures_pump
   use common_error_handling, only: error_t
   use common_precision, only: dp

   implicit none(type, external)

   private
   public :: pump_specification_t
   public :: pump_specification_or_error_t
   public :: next_pump_state
   public :: pump_state_t

   type pump_specification_t
      real(kind=dp) :: capacity
      real(kind=dp) :: start_level
      real(kind=dp) :: stop_level
   end type pump_specification_t

   type :: pump_specification_or_error_t
      type(pump_specification_t), allocatable :: pump
      type(error_t), allocatable :: error
   end type pump_specification_or_error_t

   type :: pump_state_t
      logical :: is_running
      real(kind=dp) :: discharge
   end type pump_state_t

contains

   pure function next_pump_state(pump_specification, previous_state, actual_level) result(next_state)
      type(pump_specification_t), intent(in) :: pump_specification
      type(pump_state_t), intent(in) :: previous_state
      real(kind=dp), intent(in) :: actual_level

      type(pump_state_t) :: next_state

      next_state = previous_state

      if (actual_level > pump_specification%start_level) then
         next_state%is_running = .true.
         next_state%discharge = pump_specification%capacity
      end if

      if (actual_level < pump_specification%stop_level) then
         next_state%is_running = .false.
         next_state%discharge = 0.0_dp
      end if
   end function next_pump_state

end module hydraulic_structures_pump
