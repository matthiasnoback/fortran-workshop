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
   public :: sum
   public :: divide_by_t
   public :: empty_int_list

   !> A list of integers
   type :: int_list_t
      integer, dimension(:), allocatable :: values
   contains
      procedure :: filter => int_list_filter
      procedure, private :: int_list_map_to_int_list
      procedure, private :: int_list_map_to_real_list
      procedure, private :: int_list_map_to_real_list_using_dt
      generic :: map => int_list_map_to_int_list, int_list_map_to_real_list, int_list_map_to_real_list_using_dt
      procedure, private :: reduce_to_integer
      generic :: reduce => reduce_to_integer
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

   type, extends(int_to_real_map_function_t) :: divide_by_t
      private
      real :: divisor
   contains
      procedure :: evaluate => divide_by_evaluate
   end type divide_by_t

   interface divide_by_t
      module procedure divide_by_constructor
   end interface divide_by_t

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
   pure function int_list_average(self) result(average)
      class(int_list_t), intent(in) :: self
      real :: average
      average = real(self%reduce(sum, 0))/size(self%values)
   end function int_list_average

   pure function divide_by_constructor(divisor) result(res)
      real, intent(in) :: divisor
      type(divide_by_t) :: res
      res%divisor = divisor
   end function divide_by_constructor

   pure function divide_by_evaluate(self, old_value) result(new_value)
      class(divide_by_t), intent(in) :: self
      integer, intent(in) :: old_value
      real :: new_value

      new_value = old_value/self%divisor
   end function divide_by_evaluate

   pure function int_list_filter(self, int_filter_func) result(res)
      class(int_list_t), intent(in) :: self
      type(int_list_t) :: res

      interface
         pure function int_filter_func(value) result(keep)
            implicit none(type, external)

            integer, intent(in) :: value
            logical :: keep
         end function int_filter_func
      end interface

      integer :: i

      res = int_list_t(pack(self%values, &
                            [(int_filter_func(self%values(i)), i=1, size(self%values))]))

   end function int_list_filter

   pure function int_list_map_to_int_list(self, int_to_int_map_func) result(res)
      class(int_list_t), intent(in) :: self
      type(int_list_t) :: res

      interface
         pure function int_to_int_map_func(old_value) result(new_value)
            implicit none(type, external)

            integer, intent(in) :: old_value
            integer :: new_value
         end function int_to_int_map_func
      end interface

      integer :: i

      allocate (res%values(size(self%values)))

      do i = 1, size(self%values)
         res%values(i) = int_to_int_map_func(self%values(i))
      end do
   end function int_list_map_to_int_list

   pure function int_list_map_to_real_list(self, int_to_real_map_func) result(res)
      class(int_list_t), intent(in) :: self
      type(real_list_t) :: res

      interface
         pure function int_to_real_map_func(old_value) result(new_value)
            implicit none(type, external)

            integer, intent(in) :: old_value
            real :: new_value
         end function int_to_real_map_func
      end interface

      integer :: i

      res = real_list_t([(int_to_real_map_func(self%values(i)), i=1, size(self%values))])
   end function int_list_map_to_real_list

   pure function int_list_map_to_real_list_using_dt(self, int_to_real_map_func) result(res)
      class(int_list_t), intent(in) :: self
      type(real_list_t) :: res
      class(int_to_real_map_function_t), intent(in) :: int_to_real_map_func
      integer :: i

      res = real_list_t([(int_to_real_map_func%evaluate(self%values(i)), i=1, size(self%values))])
   end function int_list_map_to_real_list_using_dt

   pure function empty_int_list() result(res)
      type(int_list_t) :: res
      allocate (res%values(0))
   end function empty_int_list

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
   recursive pure function reduce_to_integer( &
      self, reduction_function, initial_carry &
      ) result(reduced)
      class(int_list_t), intent(in) :: self

      type(int_list_t) :: tail

      interface
         pure function reduction_function(carry, value) result(new_carry)
            implicit none(type, external)
            integer, intent(in) :: carry
            integer, intent(in) :: value
            integer :: new_carry
         end function reduction_function
      end interface

      integer, intent(in) :: initial_carry

      integer :: reduced

      if (size(self%values) == 0) then
         reduced = initial_carry
         return
      end if

      tail = int_list_t(self%values(2:))
      reduced = tail%reduce(reduction_function, reduction_function(initial_carry, self%values(1)))
   end function reduce_to_integer

   !> Can be used as a reduction function, summing a list of integers.
   pure function sum(carry, value) result(new_carry)
      integer, intent(in) :: carry
      integer, intent(in) :: value
      integer :: new_carry

      new_carry = carry + value
   end function sum

end module filter_map_reduce
