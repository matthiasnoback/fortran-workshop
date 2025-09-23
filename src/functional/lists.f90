module functional_lists
   use iso_fortran_env, only: wp => real64

   implicit none(type, external)

   public

   type :: int_list_t
      integer, dimension(:), allocatable :: values
   contains
      procedure :: int_list_filter_mutable
      procedure :: int_list_filter_out_argument
      procedure :: filter => int_list_filter
      procedure, private :: int_list_map_to_int
      procedure, private :: int_list_map_to_real
      procedure, private :: int_list_curried_map_to_real
      generic :: map => int_list_map_to_int, int_list_map_to_real, int_list_curried_map_to_real
      procedure, private :: reduce_to_integer
      generic :: reduce => reduce_to_integer
   end type int_list_t

   type :: real_list_t
      real, dimension(:), allocatable :: values
   end type real_list_t

   interface list
      procedure :: list_int
      procedure :: list_real
   end interface

   type, abstract :: int_to_real_map_function_t
   contains
      procedure(map_int_to_real_evaluate), deferred :: evaluate
   end type int_to_real_map_function_t

   type, extends(int_to_real_map_function_t) :: divide_by_t
      real :: divisor
   contains
      procedure :: evaluate => divide_by_evaluate
   end type divide_by_t

   interface
      pure function map_int_to_real_evaluate(self, old_value) result(new_value)
         import int_to_real_map_function_t

         implicit none(type, external)

         class(int_to_real_map_function_t), intent(in) :: self
         integer, intent(in) :: old_value
         real :: new_value
      end function map_int_to_real_evaluate
   end interface

contains

   pure function divide_by_evaluate(self, old_value) result(new_value)
      class(divide_by_t), intent(in) :: self
      integer, intent(in) :: old_value
      real :: new_value

      new_value = real(old_value)/self%divisor
   end function divide_by_evaluate

   pure function list_int(values) result(res)
      integer, dimension(:), intent(in) :: values
      type(int_list_t) :: res

      res%values = values
   end function list_int

   pure function list_real(values) result(res)
      real, dimension(:), intent(in) :: values
      type(real_list_t) :: res

      res%values = values
   end function list_real

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

      res%values = pack(self%values, [(int_filter_func(self%values(i)), i=1, size(self%values))])
   end function int_list_filter

   subroutine int_list_filter_mutable(self, int_filter_func)
      class(int_list_t), intent(inout) :: self

      interface
         pure function int_filter_func(value) result(keep)
            implicit none(type, external)

            integer, intent(in) :: value
            logical :: keep
         end function int_filter_func
      end interface

      integer :: i

      self%values = pack(self%values, [(int_filter_func(self%values(i)), i=1, size(self%values))])
   end subroutine int_list_filter_mutable

   subroutine int_list_filter_out_argument(self, int_filter_func, res)
      class(int_list_t), intent(in) :: self
      type(int_list_t), intent(out) :: res

      interface
         pure function int_filter_func(value) result(keep)
            implicit none(type, external)

            integer, intent(in) :: value
            logical :: keep
         end function int_filter_func
      end interface

      integer :: i

      res%values = pack(self%values, [(int_filter_func(self%values(i)), i=1, size(self%values))])
   end subroutine int_list_filter_out_argument

   pure function int_list_map_to_int(self, int_to_int_map_func) result(res)
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
   end function int_list_map_to_int

   pure function int_list_map_to_real(self, int_to_real_map_func) result(res)
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

      allocate (res%values(size(self%values)))

      do i = 1, size(self%values)
         res%values(i) = int_to_real_map_func(self%values(i))
      end do
   end function int_list_map_to_real

   pure function int_list_curried_map_to_real(self, int_to_real_map_function) result(res)
      class(int_list_t), intent(in) :: self
      class(int_to_real_map_function_t), intent(in) :: int_to_real_map_function
      type(real_list_t) :: res

      integer :: i

      allocate (res%values(size(self%values)))

      do i = 1, size(self%values)
         res%values(i) = int_to_real_map_function%evaluate(self%values(i))
      end do
   end function int_list_curried_map_to_real

   !> Reduces a list of integers to a single integer using a reduction function.
   pure function reduce_to_integer(self, reduction_function, initial_carry) result(reduced)
      class(int_list_t), intent(in) :: self

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

      do i = 1, size(self%values)
         reduced = reduction_function(reduced, self%values(i))
      end do
   end function reduce_to_integer

   !> Can be used as a reduction function, summing a list of integers.
   pure function sum_func(carry, value) result(new_carry)
      integer, intent(in) :: carry
      integer, intent(in) :: value
      integer :: new_carry

      new_carry = carry + value
   end function sum_func

   pure function is_even(value) result(keep)
      integer, intent(in) :: value
      logical :: keep

      keep = mod(value, 2) == 0
   end function is_even

   pure function double(old_value) result(new_value)
      integer, intent(in) :: old_value
      integer :: new_value

      new_value = old_value*2
   end function double

   pure function one_third(old_value) result(new_value)
      integer, intent(in) :: old_value
      real :: new_value

      new_value = real(old_value)/3
   end function one_third
end module functional_lists
