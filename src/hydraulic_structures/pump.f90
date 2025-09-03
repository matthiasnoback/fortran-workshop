module hydraulic_structures_pump
   use common_error_handling, only: error_t
   use common_precision, only: dp

   implicit none(type, external)

   private
   public :: pump_specification_t
   public :: pump_specification_or_error_t
   public :: calculate_pump_discharge

   type pump_specification_t
   end type pump_specification_t

   type :: pump_specification_or_error_t
      type(pump_specification_t), allocatable :: pump
      type(error_t), allocatable :: error
   end type pump_specification_or_error_t

contains

   pure function calculate_pump_discharge(capacity) result(discharge)
      real(kind=dp), intent(in) :: capacity

      real(kind=dp) :: discharge

      discharge = capacity
   end function calculate_pump_discharge

end module hydraulic_structures_pump
