
module benchmark_memory
   use iso_c_binding

   implicit none(type, external)

   private
   public :: get_memory_usage_kb

   interface
      function get_memory_usage_kb() bind(C)
         import :: c_int

         implicit none(type, external)

         integer(c_int) :: get_memory_usage_kb
      end function get_memory_usage_kb
   end interface
contains
   function current_memory_kb() result(kb)
      integer :: kb
      kb = get_memory_usage_kb()
   end function current_memory_kb
end module benchmark_memory
