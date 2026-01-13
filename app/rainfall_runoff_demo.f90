program rainfall_runoff_demo
   use iso_fortran_env, only: real64
   use hydrology_rainfall_runoff, only: run, &
                                        inFile, &
                                        outFile, &
                                        parse_args

   implicit none(type, external)

   call parse_args(inFile, outFile)

   call run()

end program rainfall_runoff_demo
