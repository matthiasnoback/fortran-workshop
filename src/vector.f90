module vector
   use iso_fortran_env, only: real64

   implicit none(type, external)

   private
   public :: dummy_function

contains

   !> Only here to verify that a unit test can load this module and call one of its functions
   pure function dummy_function() result(res)
      logical :: res

      res = .true.
   end function dummy_function

end module vector
