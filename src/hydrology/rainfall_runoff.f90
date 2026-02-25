module hydrology_rainfall_runoff
   use iso_fortran_env, only: real64
   use hydrology_observer, only: observer_reference_t, observer_t
   use hydrology_mediator, only: simulation_state_manager_t

   implicit none(type, external)

   private
   public :: run

   ! --------------------------
   ! Fixed model parameters
   ! --------------------------
   ! Smax: soil water capacity (mm) — max storage in the single bucket
   real(real64), parameter :: Smax = 150.0_real64

   ! a_ET: ET extraction efficiency (1/day) — limits AET to a fraction of current S per day
   real(real64), parameter :: a_ET = 0.5_real64

   ! k_PET: sensitivity of PET to temperature (mm/day/°C)
   real(real64), parameter :: k_PET = 0.8_real64

   ! T_offset: °C offset so cool days still have some PET (>0 when T + T_offset > 0)
   real(real64), parameter :: T_offset = 5.0_real64

   ! --------------------------
   ! Model state (initialization)
   ! --------------------------
   ! S: soil water storage (mm). Start at 60% of capacity as a neutral initial condition.
   real(real64) :: S = 0.6_real64*Smax

   character(len=256) :: inFile, outFile ! file paths
   integer :: uin, ios ! unit numbers + I/O status
   character(len=512) :: line ! line buffer and date string
   character(len=:), allocatable :: date

   real(real64) :: P, T ! precipitation (mm/day), temperature (°C)
   real(real64) :: PET, AET ! potential and actual evapotranspiration (mm/day)
   real(real64) :: I, Q ! infiltration (mm/day), quick runoff (mm/day)

   type, extends(observer_t) :: observer_to_file_t
      private
      integer :: uout
   contains
      procedure :: end_of_timestep => end_of_timestep_to_file
      procedure :: end_of_simulation => end_of_simulation_to_file
   end type observer_to_file_t

contains

   function create_observer_to_file(outFile) result(observer)
      character(len=256), intent(in) :: outFile
      type(observer_to_file_t) :: observer

      integer :: ios

      open (newunit=observer%uout, file=trim(outFile), status='replace', action='write', iostat=ios)
      if (ios /= 0) then
         stop 'Cannot open output CSV.'
      end if

      write (observer%uout, '(A)') 'date,P,T,PET,AET,Q,S'
   end function create_observer_to_file

   subroutine end_of_timestep_to_file(self)
      class(observer_to_file_t), intent(inout) :: self

      ! Write outputs for this step ---
      write (self%uout, '(A,",",F0.3,",",F0.3,",",F0.3,",",F0.3,",",F0.3,",",F0.3)') &
         trim(date), P, T, PET, AET, Q, S
   end subroutine end_of_timestep_to_file

   subroutine end_of_simulation_to_file(self)
      class(observer_to_file_t), intent(inout) :: self

      close (self%uout)
   end subroutine end_of_simulation_to_file

   subroutine run()

      ! TODO in Observer assignment: fill with references to concrete observers:
      !   - terminal_output_observer_t
      !   - output_writing_observer_t
      type(observer_reference_t), dimension(:), allocatable :: observers

      ! TODO in Mediator assignment: pass state_manager to calculate_next_simulation_state
      ! It should set the soil water storage. Before going into the loop we also have to
      ! set the initial value for soil water storage.
      type(simulation_state_manager_t), target :: state_manager

      integer :: o

      call parse_args(inFile, outFile)

      observers = [observer_reference_t(create_observer_to_file(outFile))]

      open (newunit=uin, file=trim(inFile), status='old', action='read', iostat=ios)
      if (ios /= 0) stop 'Cannot open input CSV.'

      read (uin, '(A)', iostat=ios) line

      do
         read (uin, '(A)', iostat=ios) line
         if (ios /= 0) exit

         call parse_row(line, date, P, T)

         call calculate_next_simulation_state()

         do o = 1, size(observers)
            call observers(o)%observer%end_of_timestep()
         end do

         print *, 'Completed calculations for date ', trim(date)
      end do

      close (uin)
      do o = 1, size(observers)
         call observers(o)%observer%end_of_simulation()
      end do
      write (*, *) 'Done. Output -> ', trim(outFile)
   end subroutine run

   subroutine calculate_next_simulation_state()
      ! TODO in Mediator assignment:
      ! - Copy the current soil water storage to a local variable
      ! - Make the calculations
      ! - Pass the new value to the state manager

      ! --- 1) Potential ET (PET) ---
      ! Simple temperature-based demand: PET = max(0, k_PET * (T + T_offset))
      PET = max(0.0_real64, k_PET*(T + T_offset))

      ! --- 2) Actual ET (AET) ---
      ! Limited by soil water: AET <= a_ET * S, and also cannot exceed PET
      AET = min(PET, a_ET*S)

      ! --- 3) Infiltration and quick runoff ---
      ! Infiltration is capped by available storage space; the excess becomes runoff.
      ! I = min(P, Smax - S) but not negative if S is already at/above capacity
      I = min(P, max(0.0_real64, Smax - S))

      ! Quick runoff is any precipitation that could not infiltrate
      Q = max(0.0_real64, P - I)

      ! --- 4) Update soil storage ---
      ! New S = old S + inflow (I) - outflow (AET)
      S = S + I - AET

      ! Enforce physical bounds: 0 <= S <= Smax
      S = min(max(S, 0.0_real64), Smax)
   end subroutine calculate_next_simulation_state

   subroutine parse_args(inf, outf)
      character(len=256), intent(out) :: inf, outf
      integer :: narg

      narg = command_argument_count()
      if (narg < 2) then
         write (*, *) 'Usage: CMD input.csv output.csv'
         stop 1
      end if
      call get_command_argument(1, inf)
      call get_command_argument(2, outf)
   end subroutine parse_args

   subroutine parse_row(line, date, P, T)
      character(len=*), intent(in)  :: line
      character(len=:), allocatable, intent(out) :: date
      real(real64), intent(out) :: P, T
      integer :: i1, i2, n
      character(len=512) :: tmp

      tmp = trim(line)

      i1 = index(tmp, ',')
      n = len_trim(tmp)

      if (i1 == 0) then
         ! No comma: treat entire line as date, default P,T to 0
         date = trim(tmp)
         P = 0.0_real64
         T = 0.0_real64
         return
      end if

      ! Extract date (left of first comma)
      date = adjustl(tmp(1:i1 - 1))

      ! Find second comma in the remainder (for T)
      i2 = index(tmp(i1 + 1:n), ',')

      if (i2 == 0) then
         ! Only P present (no T). Read P, set T=0.
         read (tmp(i1 + 1:n), *, err=10) P
         T = 0.0_real64
         return
      else
         ! Both P and T present
         read (tmp(i1 + 1:i1 + i2 - 1), *, err=10) P
         read (tmp(i1 + i2 + 1:n), *, err=10) T
         return
      end if

10    continue
      ! If parsing fails, default to zeros (fail-safe)
      P = 0.0_real64
      T = 0.0_real64
   end subroutine parse_row

end module hydrology_rainfall_runoff
