module hydraulic_structures_pump
   use common_error_handling, only: error_t
   use iso_fortran_env, only: real64
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
      real(kind=real64) :: capacity
      real(kind=real64) :: start_level
      real(kind=real64) :: stop_level
   end type pump_specification_t

   type :: pump_specification_or_error_t
      type(pump_specification_t), allocatable :: pump
      type(error_t), allocatable :: error
   end type pump_specification_or_error_t

   type :: pump_state_t
      logical :: is_running
      real(kind=real64) :: discharge
   end type pump_state_t

contains

   pure function load_pump_specification(config) result(pump_or_error)
      type(configuration_t), intent(in) :: config
      type(pump_specification_or_error_t) :: pump_or_error

      type(config_value_or_error_t) :: capacity_config_value
      type(config_value_or_error_t) :: start_level_config_value
      type(config_value_or_error_t) :: stop_level_config_value
      type(real_or_error_t) :: capacity_value
      type(real_or_error_t) :: start_level_value
      type(real_or_error_t) :: stop_level_value

      capacity_config_value = config%get_config_value('capacity')
      if (allocated(capacity_config_value%error)) then
         pump_or_error%error = error_t('"capacity" is required.', &
                                       capacity_config_value%error)
         return
      end if
      capacity_value = capacity_config_value%config_value%get_real()
      if (allocated(capacity_value%error)) then
         pump_or_error%error = error_t('"capacity" should be a real.', &
                                       capacity_value%error)
         return
      end if

      start_level_config_value = config%get_config_value('start_level')
      start_level_value = start_level_config_value%config_value%get_real()

      stop_level_config_value = config%get_config_value('stop_level')
      stop_level_value = stop_level_config_value%config_value%get_real()

      pump_or_error%pump = pump_specification_t(capacity_value%value, &
                                                start_level_value%value, &
                                                stop_level_value%value &
                                                )
   end function load_pump_specification

   pure function next_pump_state(pump_specification, previous_state, actual_level) &
      result(next_state)
      type(pump_specification_t), intent(in) :: pump_specification
      type(pump_state_t), intent(in) :: previous_state
      real(kind=real64), intent(in) :: actual_level

      type(pump_state_t) :: next_state

      next_state = previous_state
   end function next_pump_state

   pure function pump_with_capacity(capacity) result(specification)
      real(kind=real64), intent(in) :: capacity

      type(pump_specification_t) :: specification
      specification%capacity = capacity
   end function pump_with_capacity

end module hydraulic_structures_pump
