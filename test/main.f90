program tester
   use, intrinsic :: iso_fortran_env, only: output_unit
   use testdrive, only: run_testsuite, new_testsuite, testsuite_type, &
           & select_suite, run_selected, get_argument
   use test_hello_world, only: collect_hello_world_tests
   use test_vector, only: collect_vector_tests => collect_tests
   use test_integration, only: collect_integration_tests => collect_tests

   implicit none(type, external)

   integer :: stat, is
   character(len=:), allocatable :: suite_name, test_name
   type(testsuite_type), allocatable :: testsuites(:)
   character(len=*), parameter :: fmt = '("#", *(1x, a))'

   stat = 0

   testsuites = [ &
                new_testsuite("hello_world", collect_hello_world_tests), &
                new_testsuite("vector", collect_vector_tests), &
                new_testsuite("vector", collect_integration_tests) &
                ]

   call get_argument(1, suite_name)
   call get_argument(2, test_name)

   if (allocated(suite_name)) then
      is = select_suite(testsuites, suite_name)
      if (is > 0 .and. is <= size(testsuites)) then
         if (allocated(test_name)) then
            write (output_unit, fmt) "Suite:", testsuites(is)%name
            call run_selected(testsuites(is)%collect, test_name, output_unit, stat)
            if (stat < 0) then
               error stop 1
            end if
         else
            write (output_unit, fmt) "Testing:", testsuites(is)%name
            call run_testsuite(testsuites(is)%collect, output_unit, stat)
         end if
      else
         write (output_unit, fmt) "Available testsuites"
         do is = 1, size(testsuites)
            write (output_unit, fmt) "-", testsuites(is)%name
         end do
         error stop 1
      end if
   else
      do is = 1, size(testsuites)
         write (output_unit, fmt) "Testing:", testsuites(is)%name
         call run_testsuite(testsuites(is)%collect, output_unit, stat)
      end do
   end if

   if (stat > 0) then
      write (output_unit, '(i0, 1x, a)') stat, "test(s) failed!"
      error stop 1
   end if

end program tester
