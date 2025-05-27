module test_filter_map_reduce
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
   use filter_map_reduce, only: int_list_t, is_even, real_list_t, one_third, &
                                reduce_to_integer, sum, double, divide_by_t

   implicit none(type, external)

   private

   interface check
      module procedure check_int_list
      module procedure check_real_list
   end interface check

   public :: collect_tests

contains

   subroutine collect_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_int_list_filter_even", &
                               test_int_list_filter_even), &
                  new_unittest("test_int_list_map_double", &
                               test_int_list_map_double), &
                  new_unittest("test_int_list_map_one_third", &
                               test_int_list_map_one_third), &
                  new_unittest("test_divide_by_t", &
                               test_divide_by_t), &
                  new_unittest("test_int_list_map_divide_by", &
                               test_int_list_map_divide_by), &
                  new_unittest("test_reduce_to_integer", &
                               test_reduce_to_integer), &
                  new_unittest("test_average", &
                               test_average), &
                  new_unittest("test_is_even", &
                               test_is_even), &
                  new_unittest("test_one_third", &
                               test_one_third) &
                  ]
   end subroutine collect_tests

   subroutine test_int_list_filter_even(error)
      type(error_type), allocatable, intent(out) :: error

      type(int_list_t), allocatable :: list
      type(int_list_t), allocatable :: even

      list = int_list_t([1, 2, 3, 4])

      ! TODO put only the even numbers in `even`; use `list%filter(is_even)`:
      even = list%filter(is_even)

      call check(error, even, int_list_t([2, 4]))
   end subroutine test_int_list_filter_even

   subroutine test_int_list_map_double(error)
      type(error_type), allocatable, intent(out) :: error

      type(int_list_t), allocatable :: list
      type(int_list_t), allocatable :: doubled

      list = int_list_t([1, 2, 3, 4])

      ! TODO `double` should contain the numbers in `list` doubled; use `list%map(double)`:
      doubled = list%map(double)

      call check(error, doubled, int_list_t([2, 4, 6, 8]))
   end subroutine test_int_list_map_double

   subroutine test_int_list_map_one_third(error)
      type(error_type), allocatable, intent(out) :: error

      type(int_list_t), allocatable :: list
      type(real_list_t), allocatable :: one_thirds

      list = int_list_t([1, 2, 3, 4])

      ! TODO calculate 1/3 using list%map(one_third)
      one_thirds = list%map(one_third)

      call check(error, one_thirds, real_list_t([0.333, 0.6666, 1.000, 1.333]), thr=0.001)
   end subroutine test_int_list_map_one_third

   subroutine test_divide_by_t(error)
      type(error_type), allocatable, intent(out) :: error

      type(divide_by_t) :: divide_by_3
      divide_by_3 = divide_by_t(3.0)

      call check(error, divide_by_3%evaluate(1), 0.333, thr=0.001)
   end subroutine test_divide_by_t

   subroutine test_int_list_map_divide_by(error)
      type(error_type), allocatable, intent(out) :: error

      type(int_list_t), allocatable :: list
      type(real_list_t), allocatable :: result

      list = int_list_t([1, 2, 3, 4])

      ! TODO calculate 1/4 using list%map(divide_by_t(4))
      result = list%map(divide_by_t(4.0))

      call check(error, result, real_list_t([0.25, 0.5, 0.75, 1.0]), thr=0.001)
   end subroutine test_int_list_map_divide_by

   subroutine test_reduce_to_integer(error)
      type(error_type), allocatable, intent(out) :: error

      type(int_list_t), allocatable :: list
      integer :: result

      list = int_list_t([1, 2, 3, 4])

      ! TODO rewrite this to use `list%reduce(sum, 0)`:
      result = reduce_to_integer(list%values, sum, 0)

      call check(error, result, 10)
   end subroutine test_reduce_to_integer

   subroutine test_average(error)
      type(error_type), allocatable, intent(out) :: error

      type(int_list_t), allocatable :: list
      real :: result

      list = int_list_t([1, 4])

      ! TODO rewrite this to use `list%average()`:
      result = 2.5

      call check(error, result, 2.5)
   end subroutine test_average

   subroutine test_is_even(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, is_even(2), .true.)
      if (allocated(error)) return

      call check(error, is_even(3), .false.)
   end subroutine test_is_even

   subroutine test_one_third(error)
      type(error_type), allocatable, intent(out) :: error

      call check(error, one_third(2), 0.666, thr=0.001)
   end subroutine test_one_third

   subroutine check_int_list(error, actual, expected)
      type(error_type), allocatable, intent(out) :: error
      type(int_list_t), intent(in) :: actual, expected

      if (size(actual%values) /= size(expected%values)) then
         call test_failed(error, "List sizes do not match")
         return
      end if

      if (any(actual%values /= expected%values)) then
         call test_failed(error, "List contents do not match")
         return
      end if
   end subroutine check_int_list

   subroutine check_real_list(error, actual, expected, thr)
      type(error_type), allocatable, intent(out) :: error
      type(real_list_t), intent(in) :: actual, expected
      real, intent(in), optional :: thr

      integer :: i

      if (size(actual%values) /= size(expected%values)) then
         call test_failed(error, "List sizes do not match")
         return
      end if

      do i = 1, size(actual%values)
         if (present(thr)) then
            call check(error, actual%values(i), expected%values(i), thr=thr)
         else
            call check(error, actual%values(i), expected%values(i))
         end if
         if (allocated(error)) return
      end do
   end subroutine check_real_list

end module test_filter_map_reduce
