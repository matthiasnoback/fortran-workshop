module hello_world
   implicit none(type, external)

   private

   public :: hello_world_message

contains

   function hello_world_message() result(res)
      character(len=:), allocatable :: res

      res = 'Hello, world!'
   end function hello_world_message

end module hello_world
