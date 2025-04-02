module test_hello_world
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
   use hello_world, only: hello_world_message

   implicit none(type, external)

   private

   public :: collect_hello_world_tests

contains

   subroutine collect_hello_world_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("hello_world_message", test_hello_world_message) &
                  ]

   end subroutine collect_hello_world_tests

   subroutine test_hello_world_message(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, hello_world_message(), 'Hello, world!')
   end subroutine test_hello_world_message

end module test_hello_world
