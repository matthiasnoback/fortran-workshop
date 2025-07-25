program tester
   use, intrinsic :: iso_fortran_env, only: output_unit
   use testdrive, only: run_testsuite, new_testsuite, testsuite_type, &
           & select_suite, run_selected, get_argument
   use test_config, only: collect_config_tests => collect_tests
   use test_interpolation, only: collect_interpolation_tests => collect_tests
   use test_error_handling, only: collect_error_handling_tests => collect_tests
   use test_filter_map_reduce, only: collect_filter_map_reduce_tests => collect_tests
   use test_geometry, only: collect_geometry_tests => collect_tests
   use test_hello_world, only: collect_hello_world_tests
   use test_integration, only: collect_integration_tests => collect_tests
   use test_random, only: collect_random_tests => collect_tests
   use test_strings, only: collect_strings_tests => collect_tests
   use test_to_string, only: collect_to_string_tests => collect_tests
   use test_vector, only: collect_vector_tests => collect_tests
   use test_benchmark, only: collect_benchmark_tests => collect_tests

   implicit none(type, external)

   integer :: stat, is
   character(len=:), allocatable :: suite_name, test_name
   type(testsuite_type), allocatable :: testsuites(:)
   character(len=*), parameter :: fmt = '("#", *(1x, a))'

   stat = 0

   testsuites = [ &
                new_testsuite("config", collect_config_tests), &
                new_testsuite("interpolation", collect_interpolation_tests), &
                new_testsuite("error_handling", collect_error_handling_tests), &
                new_testsuite("filter_map_reduce", collect_filter_map_reduce_tests), &
                new_testsuite("geometry", collect_geometry_tests), &
                new_testsuite("hello_world", collect_hello_world_tests), &
                new_testsuite("integration", collect_vector_tests), &
                new_testsuite("strings", collect_strings_tests), &
                new_testsuite("random", collect_random_tests), &
                new_testsuite("to_string", collect_to_string_tests), &
                new_testsuite("benchmark", collect_benchmark_tests), &
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
