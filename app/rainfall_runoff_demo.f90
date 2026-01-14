program rainfall_runoff_demo
   use iso_fortran_env, only: real64
   use hydrology_rainfall_runoff, only: run

   implicit none(type, external)

   character(len=256) :: inFile, outFile

   call parse_args(inFile, outFile)

   call run(inFile, outFile)

contains
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
end program rainfall_runoff_demo
