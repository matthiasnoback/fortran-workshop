module test_hello_world
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
   use m_hello_world, only: hello_world, t_greeting, t_recipient

   implicit none(type, external)

   private

   public :: collect_hello_world_tests

contains

   subroutine collect_hello_world_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("hello_world", test_hello_world_function), &
                  new_unittest("test_greeting", test_greeting) &
                  ]

   end subroutine collect_hello_world_tests

   subroutine test_hello_world_function(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, hello_world(), 'Hello, world!')
   end subroutine test_hello_world_function

   subroutine test_greeting(error)
      type(error_type), allocatable, intent(out) :: error
      type(t_greeting) :: greeting
      type(t_recipient) :: recipient

      greeting = t_greeting('Hey')
      recipient = t_recipient('mister')

      call check(error, greeting%greet(recipient), 'Hey, mister!')
   end subroutine test_greeting

end module test_hello_world
