module common_random
   implicit none(type, external)

   private
   public :: initialize_rng
   public :: select_random_integer

contains

   function select_random_integer(integers) result(res)
      integer, intent(in) :: integers(:)
      integer :: res
      real :: r
      integer :: idx

      call random_number(r)
      idx = int(r*size(integers)) + 1
      res = integers(idx)
   end function select_random_integer

   subroutine initialize_rng()
      integer :: n, i
      integer, dimension(:), allocatable :: seed
      integer :: clock_val

      ! Get the required size of the seed array
      call random_seed(size=n)
      allocate (seed(n))

      ! Use system clock to generate a base value
      call system_clock(count=clock_val)

      ! Fill the seed array with variations of the clock value
      do i = 1, n
         seed(i) = clock_val + i*37
      end do

      ! Set the seed
      call random_seed(put=seed)
   end subroutine initialize_rng

end module common_random
