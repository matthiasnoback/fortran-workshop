module geometry_point
   use common_precision, only: dp
   use common_error_handling, only: error_t

   implicit none(type, external)

   private
   public :: point_t
   public :: create_point
   public :: point_or_error_t
   public :: point_wrapper_t
   public :: translate

   !> Represents a point in 2D space
   type, abstract :: point_t
   contains
      !> Calculate the distince to another point
      procedure :: distance_to => point_distance_to
      procedure(get_x_interface), deferred :: get_x
      procedure(get_y_interface), deferred :: get_y
   end type point_t

   abstract interface
      pure function get_x_interface(self) result(x_value)
         import :: point_t, dp
         implicit none(type, external)
         class(point_t), intent(in) :: self

         real(kind=dp) :: x_value
      end function get_x_interface

      pure function get_y_interface(self) result(y_value)
         import :: point_t, dp
         implicit none(type, external)
         class(point_t), intent(in) :: self

         real(kind=dp) :: y_value
      end function get_y_interface
   end interface

   type, extends(point_t) :: concrete_point_t
      private
      real(kind=dp) :: x
      real(kind=dp) :: y
   contains
      procedure :: get_x => concrete_point_get_x
      procedure :: get_y => concrete_point_get_y
   end type concrete_point_t

   type, extends(point_t) :: translated_point_t
      private
      class(point_t), allocatable :: original_point
      real(kind=dp) :: dx
      real(kind=dp) :: dy
   contains
      procedure :: get_x => translated_point_get_x
      procedure :: get_y => translated_point_get_y
   end type translated_point_t

   type :: point_or_error_t
      type(error_t), allocatable :: error
      class(point_t), allocatable :: point
   end type point_or_error_t

   type :: point_wrapper_t
      class(point_t), allocatable :: point
   end type point_wrapper_t

contains

   pure function create_point(x, y) result(point)
      real(kind=dp), intent(in) :: x
      real(kind=dp), intent(in) :: y

      class(point_t), allocatable :: point

      point = concrete_point_t(x, y)
   end function create_point

   pure function translate(original_point, dx, dy) result(point)
      class(point_t), intent(in) :: original_point
      real(kind=dp), intent(in) :: dx
      real(kind=dp), intent(in) :: dy

      class(point_t), allocatable :: point

      point = translated_point_t(original_point, dx, dy)
   end function translate

   pure function concrete_point_get_x(self) result(x_value)
      class(concrete_point_t), intent(in) :: self

      real(kind=dp) :: x_value
      x_value = self%x
   end function concrete_point_get_x

   pure function concrete_point_get_y(self) result(y_value)
      class(concrete_point_t), intent(in) :: self

      real(kind=dp) :: y_value
      y_value = self%y
   end function concrete_point_get_y

   pure function translated_point_get_x(self) result(x_value)
      class(translated_point_t), intent(in) :: self

      real(kind=dp) :: x_value
      x_value = self%original_point%get_x() + self%dx
   end function translated_point_get_x

   pure function translated_point_get_y(self) result(y_value)
      class(translated_point_t), intent(in) :: self

      real(kind=dp) :: y_value
      y_value = self%original_point%get_y() + self%dy
   end function translated_point_get_y

   pure function point_distance_to(self, other) result(res)
      class(point_t), intent(in) :: self
      class(point_t), intent(in) :: other
      real(kind=dp) :: res

      res = sqrt((self%get_x() - other%get_x())**2 + (self%get_y() - other%get_y())**2)
   end function point_distance_to

end module geometry_point
