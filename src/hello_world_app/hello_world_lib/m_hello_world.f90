module m_hello_world
   implicit none(type, external)

   private

   public :: hello_world, t_greeting, t_recipient

   type :: t_greeting
      character(len=:), allocatable :: greeting
   contains
      procedure :: greet => greeting_greet
   end type t_greeting

   type :: t_recipient
      character(len=:), allocatable :: name
   end type t_recipient

contains

   function greeting_greet(greeting, recipient) result(res)
      class(t_greeting), intent(in) :: greeting
      class(t_recipient), intent(in) :: recipient
      character(len=:), allocatable :: res

      res = greeting%greeting//', '//recipient%name//'!'
   end function greeting_greet

   function hello_world() result(res)
      character(len=:), allocatable :: res

      type(t_greeting) :: greeting
      type(t_recipient) :: recipient

      greeting = t_greeting('Hello')
      recipient = t_recipient('world')

      res = greeting%greet(recipient)
   end function hello_world

end module m_hello_world
