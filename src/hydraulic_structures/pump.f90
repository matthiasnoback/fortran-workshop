module hydraulic_structures_pump
   use common_error_handling, only: error_t
   use common_precision, only: dp
   use config_loading, only: configuration_t, config_value_or_error_t, real_or_error_t

   implicit none(type, external)

   private
   public :: pump_specification_t
   public :: pump_specification_or_error_t
   public :: next_pump_state
   public :: pump_state_t
   public :: pump_with_capacity
   public :: load_pump_specification

   type pump_specification_t
      real(kind=dp) :: capacity
      logical :: is_controlled_on_suction_side = .false.
      real(kind=dp) :: suction_side_start_level = 0.0_dp
      real(kind=dp) :: suction_side_stop_level = 0.0_dp
      logical :: is_controlled_on_delivery_side = .false.
      real(kind=dp) :: delivery_side_start_level = 0.0_dp
      real(kind=dp) :: delivery_side_stop_level = 0.0_dp
   contains
      procedure :: control_suction_side => pump_specification_control_suction_side
      procedure :: control_delivery_side => pump_specification_control_delivery_side
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

   function load_pump_specification(configuration) result(res)
      type(configuration_t), intent(in) :: configuration
      type(pump_specification_or_error_t) :: res

      type(config_value_or_error_t) :: capacity_config_value
      type(real_or_error_t) :: capacity_value

      capacity_config_value = configuration%get_config_value('capacity')
      capacity_value = capacity_config_value%config_value%get_real()

      res%pump = pump_with_capacity(capacity_value%value)
   end function load_pump_specification

   pure function next_pump_state(pump_specification, previous_state, actual_level) result(next_state)
      type(pump_specification_t), intent(in) :: pump_specification
      type(pump_state_t), intent(in) :: previous_state
      real(kind=dp), intent(in) :: actual_level

      type(pump_state_t) :: next_state

      if (actual_level > pump_specification%suction_side_start_level) then
         next_state%is_running = .true.
      else if (actual_level < pump_specification%suction_side_stop_level) then
         next_state%is_running = .false.
      else
         next_state%is_running = previous_state%is_running
      end if

      if (next_state%is_running) then
         next_state%discharge = pump_specification%capacity
      else
         next_state%discharge = 0.0_dp
      end if
   end function next_pump_state

   pure function pump_with_capacity(capacity) result(specification)
      real(kind=dp), intent(in) :: capacity

      type(pump_specification_t) :: specification
      specification%capacity = capacity
   end function pump_with_capacity

   subroutine pump_specification_control_suction_side(self, start_level, stop_level)
      class(pump_specification_t), intent(inout) :: self
      real(kind=dp), intent(in) :: start_level
      real(kind=dp), intent(in) :: stop_level

      self%is_controlled_on_suction_side = .true.
      self%suction_side_start_level = start_level
      self%suction_side_stop_level = stop_level
   end subroutine pump_specification_control_suction_side

   subroutine pump_specification_control_delivery_side(self, start_level, stop_level)
      class(pump_specification_t), intent(inout) :: self
      real(kind=dp), intent(in) :: start_level
      real(kind=dp), intent(in) :: stop_level

      self%is_controlled_on_delivery_side = .true.
      self%delivery_side_start_level = start_level
      self%delivery_side_stop_level = stop_level
   end subroutine pump_specification_control_delivery_side

end module hydraulic_structures_pump
