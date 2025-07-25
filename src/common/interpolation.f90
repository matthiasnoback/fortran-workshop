module common_interpolation
   use common_precision, only: dp

   implicit none(type, external)

   private
   public :: interpolate_linearly

contains

   function interpolate_linearly(table, x) result(y)
      real(kind=dp), intent(in), dimension(:, :) :: table
      real(kind=dp), intent(in) :: x
      real(kind=dp) :: y

      integer :: n
      integer :: i

      n = size(table, 1)
      if (n == 0) then
         ! Or should we return an error?
         y = 0.0_dp
         return
      end if

      ! Check if x is out of bounds
      if (x <= table(1, 1)) then
         y = table(1, 2)
         return
      else if (x >= table(n, 1)) then
         y = table(n, 2)
         return
      end if

      ! Find the interval x is in
      do i = 1, n - 1
         if (x >= table(i, 1) .and. x <= table(i + 1, 1)) then
            y = table(i, 2) + (x - table(i, 1))*(table(i + 1, 2) - table(i, 2))/ &
                (table(i + 1, 1) - table(i, 1))
            return
         end if
      end do

      ! Should we return an error?
      ! This will be reached only when the table has no strictly increasing columns
      y = 0.0_dp
   end function interpolate_linearly

end module common_interpolation
