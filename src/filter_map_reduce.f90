!> Module to demonstrate the use of filter, map and reduce functions
module filter_map_reduce
   implicit none(type, external)

   private

   public :: int_list_t
   public :: is_even
   public :: real_list_t
   public :: double
   public :: one_third
   public :: reduce_to_integer
   public :: sum_function
   public :: empty_int_list
   public :: create_int_list

   !> A list of integers
   type :: int_list_t
      integer, dimension(:), allocatable :: values
   contains
      procedure :: average => int_list_average
   end type int_list_t

   !> A list of reals (we'll add filter, map and reduce functions later).
   type :: real_list_t
      real, dimension(:), allocatable :: values
   end type real_list_t

   !> Abstract type for a configurable integer-to-real map function.
   type, abstract :: int_to_real_map_function_t
   contains
      !> The actual map function, to be implemented by a concrete subtype.
      procedure(map_int_to_real_evaluate), deferred :: evaluate
   end type int_to_real_map_function_t

   interface
      !> Interface for `int_to_real_map_function_t%evaluate()` implementations.
      pure function map_int_to_real_evaluate(self, old_value) result(new_value)
         import int_to_real_map_function_t

         implicit none(type, external)

         class(int_to_real_map_function_t), intent(in) :: self
         integer, intent(in) :: old_value
         real :: new_value
      end function map_int_to_real_evaluate
   end interface

contains
   pure function empty_int_list() result(res)
      type(int_list_t) :: res
      allocate (res%values(0))
   end function empty_int_list

   pure function create_int_list(values) result(res)
      integer, dimension(:), intent(in) :: values
      class(int_list_t), allocatable :: res

      allocate (int_list_t :: res)
      res%values = values
   end function create_int_list

   !> Can be used as a filter function to check if an integer is even.
   pure function is_even(value) result(res)
      integer, intent(in) :: value
      logical :: res

      res = mod(value, 2) == 0
   end function is_even

   !> Can be used as a map function to double the provided integer
   pure function double(old_value) result(new_value)
      integer, intent(in) :: old_value
      integer :: new_value

      new_value = 2*old_value
   end function double

   !> Can be used as a map function to calculate one third of an integer.
   pure function one_third(old_value) result(new_value)
      integer, intent(in) :: old_value
      real :: new_value

      new_value = real(old_value)/3.0
   end function one_third

   !> Reduces a list of integers to a single integer using a reduction function.
   pure function reduce_to_integer(numbers, reduction_function, initial_carry) result(reduced)
      integer, dimension(:), intent(in) :: numbers

      interface
         pure function reduction_function(carry, value) result(new_carry)
            implicit none(type, external)
            integer, intent(in) :: carry
            integer, intent(in) :: value
            integer :: new_carry
         end function reduction_function
      end interface

      integer, intent(in) :: initial_carry

      integer :: i
      integer :: reduced

      reduced = initial_carry

      do i = 1, size(numbers)
         reduced = reduction_function(reduced, numbers(i))
      end do
   end function reduce_to_integer

   !> Can be used as a reduction function, summing a list of integers.
   pure function sum_function(carry, value) result(new_carry)
      integer, intent(in) :: carry
      integer, intent(in) :: value
      integer :: new_carry

      new_carry = carry + value
   end function sum_function

   pure function int_list_average(self) result(res)
      class(int_list_t), intent(in) :: self
      real :: res

      res = real(sum(self%values))/size(self%values)
   end function int_list_average

end module filter_map_reduce
