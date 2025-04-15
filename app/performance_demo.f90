! Source: https://zmoon.github.io/FortranTipBrowser/tips/049.html

program column_major
   use iso_fortran_env, only: wp => real64, int64
   use stopwatch_facade, only: stopwatch_start, stopwatch_stop, stopwatch_print_timers

   implicit none(type, external)

   integer, parameter :: m = 10000, n = m
   integer :: array(m, n)
   integer :: i, j, tot
   integer(int64) :: irate, tic, toc

   array = 0

   ! FASTER: Looping consecutively through columns
   call stopwatch_start('Array loop column-first')
   tot = 0
   do j = 1, size(array, dim=2)
      do i = 1, size(array, dim=1)
         ! Do something with array(i, j)
         tot = tot + array(i, j)
      end do
   end do
   call stopwatch_stop('Array loop column-first')

   ! SLOWER: Looping consecutively through rows
   call stopwatch_start('Array loop row-first')
   tot = 0
   do i = 1, size(array, dim=1)
      do j = 1, size(array, dim=2)
         ! Do something with array(i, j)
         tot = tot + array(i, j)
      end do
   end do
   call stopwatch_stop('Array loop row-first')

   call stopwatch_print_timers()
end program column_major
