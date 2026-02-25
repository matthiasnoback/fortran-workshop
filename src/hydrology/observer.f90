module hydrology_observer
   use hydrology_simulation_state, only: simulation_state_t, forcings_t

   implicit none(type, external)

   private
   public :: observer_t
   public :: observer_reference_t

   type, abstract :: observer_t
   contains
      !> To be invoked at the end of each timestep
      procedure(observe_end_of_timestep_proc), deferred :: end_of_timestep
      procedure(observe_end_of_simulation_proc), deferred :: end_of_simulation
   end type observer_t

   abstract interface
      subroutine observe_end_of_timestep_proc(self, forcings, simulation_state)
         import observer_t, simulation_state_t, forcings_t
         implicit none(type, external)
         class(observer_t), intent(inout) :: self
         class(forcings_t), intent(in) :: forcings
         class(simulation_state_t), intent(in) :: simulation_state
      end subroutine observe_end_of_timestep_proc

      subroutine observe_end_of_simulation_proc(self)
         import observer_t
         implicit none(type, external)
         class(observer_t), intent(inout) :: self
      end subroutine observe_end_of_simulation_proc
   end interface

   !> Wrapper for holding an `observer_t` instance
   !> We need it because we can't make arrays of `class(observer_t)` instances,
   !> but we can make arrays of `type(observer_reference_t)`.
   type :: observer_reference_t
      class(observer_t), allocatable :: observer
   end type observer_reference_t
end module hydrology_observer
