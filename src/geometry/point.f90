module geometry_point
   use common_precision, only: dp
   use common_error_handling, only: error_t

   implicit none(type, external)

   private
   public :: point_t
   public :: create_point
   public :: point_or_error_t
   public :: translate
   public :: abstract_point_t

   !> Represents a point in 2D space
   type :: point_t
      real(kind=dp) :: x
      real(kind=dp) :: y
   end type point_t

   type, abstract :: abstract_point_t
   contains
      procedure(point_get_x_proc), deferred :: get_x
      procedure(point_get_y_proc), deferred :: get_y
      procedure :: distance_to => point_distance_to
   end type abstract_point_t

   abstract interface
      pure function point_get_x_proc(self) result(x)
         import :: dp, abstract_point_t
         implicit none(type, external)
         class(abstract_point_t), intent(in) :: self
         real(kind=dp) :: x
      end function point_get_x_proc

      pure function point_get_y_proc(self) result(y)
         import :: dp, abstract_point_t
         implicit none(type, external)
         class(abstract_point_t), intent(in) :: self
         real(kind=dp) :: y
      end function point_get_y_proc
   end interface

   type, extends(abstract_point_t) :: concrete_point_t
      real(kind=dp) :: x
      real(kind=dp) :: y
   contains
      procedure :: get_x => concrete_point_get_x
      procedure :: get_y => concrete_point_get_y
   end type concrete_point_t

   type, extends(abstract_point_t) :: translated_point_t
      class(abstract_point_t), allocatable :: original_point
      real(kind=dp) :: dx
      real(kind=dp) :: dy
   contains
      procedure :: get_x => translated_point_get_x
      procedure :: get_y => translated_point_get_y
   end type translated_point_t

   type :: point_or_error_t
      type(error_t), allocatable :: error
      class(abstract_point_t), allocatable :: point
   end type point_or_error_t

contains

   pure function translate(original, dx, dy) result(point)
      class(abstract_point_t), intent(in) :: original
      real(kind=dp), intent(in) :: dx
      real(kind=dp), intent(in) :: dy

      class(abstract_point_t), allocatable :: point

      point = translated_point_t(original, dx, dy)
   end function translate

   pure function create_point(x, y) result(point)
      real(kind=dp), intent(in) :: x
      real(kind=dp), intent(in) :: y

      class(abstract_point_t), allocatable :: point

      point = concrete_point_t(x, y)
   end function create_point

   pure function concrete_point_get_x(self) result(x)
      class(concrete_point_t), intent(in) :: self
      real(kind=dp) :: x
      x = self%x
   end function concrete_point_get_x

   pure function concrete_point_get_y(self) result(y)
      class(concrete_point_t), intent(in) :: self
      real(kind=dp) :: y
      y = self%y
   end function concrete_point_get_y

   pure function translated_point_get_x(self) result(x)
      class(translated_point_t), intent(in) :: self
      real(kind=dp) :: x
      x = self%original_point%get_x() + self%dx
   end function translated_point_get_x

   pure function translated_point_get_y(self) result(y)
      class(translated_point_t), intent(in) :: self
      real(kind=dp) :: y
      y = self%original_point%get_y() + self%dy
   end function translated_point_get_y

   pure function point_distance_to(self, other) result(res)
      class(abstract_point_t), intent(in) :: self
      class(abstract_point_t), intent(in) :: other
      real(kind=dp) :: res

      res = sqrt((self%get_x() - other%get_x())**2 + (self%get_y() - other%get_y())**2)
   end function point_distance_to

end module geometry_point
