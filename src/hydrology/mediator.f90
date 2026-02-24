module hydrology_mediator
   use iso_fortran_env, only: real64

   implicit none(type, external)

   private
   public :: simulation_state_manager_t

   type :: simulation_state_manager_t
      real(kind=real64) :: soil_water_storage
   contains
      procedure :: set_soil_water_storage
      procedure :: get_soil_water_storage
   end type simulation_state_manager_t

contains
   subroutine set_soil_water_storage(self, value)
      class(simulation_state_manager_t), intent(inout) :: self

      real(kind=real64), intent(in) :: value

      self%soil_water_storage = value
   end subroutine set_soil_water_storage

   pure function get_soil_water_storage(self) result(value)
      class(simulation_state_manager_t), intent(in) :: self

      real(kind=real64) :: value

      value = self%soil_water_storage
   end function get_soil_water_storage

end module hydrology_mediator
