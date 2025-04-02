module common_command_line
   implicit none(type, external)

   private

   public :: has_cli_argument
   public :: get_cli_argument

contains

   function has_cli_argument(argument) result(res)
      character(len=*), intent(in) :: argument

      logical :: res
      integer :: index

      do index = 1, command_argument_count()
         if (get_cli_argument(index) == argument) then
            res = .true.
            return
         end if
      end do

      res = .false.
   end function has_cli_argument

   function get_cli_argument(index) result(value)
      integer, intent(in) :: index
      character(len=:), allocatable :: value

      integer :: argument_length
      integer :: status

      call get_command_argument(number=index, length=argument_length)
      allocate (character(len=argument_length) :: value)

      call get_command_argument(index, value, status=status)

      if (status /= 0) then
         error stop "Could not retrieve command-line argument"
      end if
   end function get_cli_argument

end module common_command_line
