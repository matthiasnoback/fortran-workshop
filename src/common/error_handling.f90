module common_error_handling
   use common_to_string, only: to_string_t

   implicit none(type, external)
   private

   public :: error_t
   public :: optional_error_t
   public :: some_error_t
   public :: no_error_t

   type, extends(to_string_t) :: error_t
      character(len=:), allocatable :: message
      class(error_t), allocatable :: previous
   contains
      procedure, public :: to_string => error_to_string
   end type error_t

   type, abstract :: optional_error_t
   end type optional_error_t

   type, extends(optional_error_t) :: some_error_t
      class(error_t), allocatable :: error
   end type some_error_t

   type, extends(optional_error_t) :: no_error_t
   end type no_error_t

contains

   recursive pure function error_to_string(self) result(res)
      class(error_t), intent(in) :: self
      character(len=:), allocatable :: res

      if (allocated(self%previous)) then
         res = self%message//' '//self%previous%to_string()
      else
         res = self%message
      end if
   end function error_to_string

end module common_error_handling
