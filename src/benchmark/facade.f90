module benchmark_facade
   use iso_fortran_env, only: real64
   use benchmark_diagnostics, only: get_snapshot, &
                                    diagnostics_snapshot_t, &
                                    get_clock_rate
   use common_to_string, only: to_string

   implicit none(type, external)

   private

   public :: start_benchmark
   public :: stop_benchmark
   public :: benchmark_repeated_procedure_calls
   public :: get_benchmarks
   public :: benchmark_ended_t
   public :: benchmark_result_t
   public :: print_benchmark_results
   public :: clear_benchmarks

   type :: benchmark_started_t
      private
      character(len=:), allocatable :: name
      type(diagnostics_snapshot_t) :: start
   end type benchmark_started_t

   type :: benchmark_ended_t
      character(len=:), allocatable :: name
      type(diagnostics_snapshot_t) :: start
      type(diagnostics_snapshot_t) :: end
   contains
      procedure :: result => benchmark_ended_result
   end type benchmark_ended_t

   type :: benchmark_result_t
      real(kind=real64) :: cpu_time_diff
      real(kind=real64) :: wall_clock_time_diff
   end type benchmark_result_t

   type(benchmark_started_t), dimension(:), allocatable :: started_benchmarks
   type(benchmark_ended_t), dimension(:), allocatable, target :: ended_benchmarks

contains

   subroutine start_benchmark(name)
      character(len=*), intent(in) :: name

      type(benchmark_started_t), allocatable :: benchmark_started

      if (.not. allocated(started_benchmarks)) then
         allocate (started_benchmarks(0))
      end if

      benchmark_started = benchmark_started_t(name, get_snapshot())

      started_benchmarks = [started_benchmarks, benchmark_started]
   end subroutine start_benchmark

   subroutine stop_benchmark(name)
      character(len=*), intent(in) :: name

      integer :: i

      if (.not. allocated(started_benchmarks)) then
         allocate (started_benchmarks(0))
      end if
      if (.not. allocated(ended_benchmarks)) then
         allocate (ended_benchmarks(0))
      end if

      do i = 1, size(started_benchmarks)
         if (started_benchmarks(i)%name == name) then
            ended_benchmarks = [ended_benchmarks, &
                                benchmark_ended_t(name, &
                                                  started_benchmarks(i)%start, &
                                                  get_snapshot() &
                                                  )]
         end if
      end do
   end subroutine stop_benchmark

   subroutine benchmark_repeated_procedure_calls(name, repetitions, procedure)
      character(len=*), intent(in) :: name
      integer, intent(in) :: repetitions
      interface
         subroutine procedure()
            implicit none(type, external)
         end subroutine procedure
      end interface

      integer :: i

      call start_benchmark(name)

      do i = 1, repetitions
         call procedure()
      end do

      call stop_benchmark(name)
   end subroutine benchmark_repeated_procedure_calls

   function get_benchmarks() result(benchmarks)
      type(benchmark_ended_t), dimension(:), pointer :: benchmarks
      if (.not. allocated(ended_benchmarks)) then
         allocate (ended_benchmarks(0))
      end if

      benchmarks => ended_benchmarks
   end function get_benchmarks

   function benchmark_ended_result(self) result(benchmark_result)
      class(benchmark_ended_t), intent(in) :: self
      type(benchmark_result_t) :: benchmark_result

      benchmark_result%cpu_time_diff = self%end%cpu_time - self%start%cpu_time
      benchmark_result%wall_clock_time_diff = (self%end%clock_time - self%start%clock_time) &
                                              /get_clock_rate()
   end function benchmark_ended_result

   subroutine print_benchmark_results()
      type(benchmark_ended_t), dimension(:), pointer :: benchmarks
      integer :: i
      integer :: longest_benchmark_name
      character, parameter :: tab = char(9)
      benchmarks => get_benchmarks()

      if (size(benchmarks) == 0) then
         print *, 'There are no benchmark results to print'
         return
      end if

      longest_benchmark_name = 0
      do i = 1, size(benchmarks)
         if (len(benchmarks(i)%name) > longest_benchmark_name) then
            longest_benchmark_name = len(benchmarks(i)%name)
         end if
      end do

      print '(A'//to_string(longest_benchmark_name)//',A,A10,A,A10)', 'Benchmark', tab, 'CPU time', tab, 'Wall clock time'
      do i = 1, size(benchmarks)
         associate (benchmark => benchmarks(i), result => benchmarks(i)%result())
            print '(A'//to_string(longest_benchmark_name)//',A,F10.3,A,F10.3)', &
               benchmark%name, &
               tab, &
               result%cpu_time_diff, &
               tab, &
               result%wall_clock_time_diff
         end associate
      end do
   end subroutine print_benchmark_results

   subroutine clear_benchmarks()
      if (allocated(started_benchmarks)) then
         deallocate (started_benchmarks)
      end if

      if (allocated(ended_benchmarks)) then
         deallocate (ended_benchmarks)
      end if
   end subroutine clear_benchmarks
end module benchmark_facade
