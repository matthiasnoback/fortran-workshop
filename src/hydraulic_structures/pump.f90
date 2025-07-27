module hydraulic_structures_pump
   use common_error_handling, only: error_t

   implicit none(type, external)

   private
   public :: pump_specification_t
   public :: pump_specification_or_error_t

   type pump_specification_t
   end type pump_specification_t

   type :: pump_specification_or_error_t
      type(pump_specification_t), allocatable :: pump
      type(error_t), allocatable :: error
   end type pump_specification_or_error_t

contains

end module hydraulic_structures_pump
