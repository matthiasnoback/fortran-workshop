program rainfall_runoff_demo
   use iso_fortran_env, only: real64
   use hydrology_rainfall_runoff, only: run, &
                                        parse_args

   implicit none(type, external)

   character(len=256) :: inFile, outFile

   call parse_args(inFile, outFile)

   call run(inFile, outFile)

end program rainfall_runoff_demo
