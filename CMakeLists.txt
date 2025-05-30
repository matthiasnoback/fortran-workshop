cmake_minimum_required(VERSION 3.28)

project(fortran-workshop VERSION 0.1
    LANGUAGES Fortran
    DESCRIPTION "Fortran workshop")

include(cmake/compiler_flags.cmake)
include(cmake/python_dependencies.cmake)

add_subdirectory(app)
add_subdirectory(src)

enable_testing()
include(CTest)

# Disable deprecation warnings for the test-drive package
set(CMAKE_WARN_DEPRECATED OFF CACHE BOOL "" FORCE)
# Disable the tests of test-drive itself
set(TEST_DRIVE_BUILD_TESTING OFF CACHE BOOL "Enable testing for the test-drive project")

include(cmake/find-test-drive)
if(NOT TARGET "test-drive::test-drive")
    find_package("test-drive" REQUIRED)
endif()

# Unit testing
set(
    tests
    "benchmark"
    "error_handling"
    "filter_map_reduce"
    "geometry"
    "hello_world"
    "integration"
    "strings"
    "to_string"
    "vector"
)
set(
    test-srcs
    "test/main.f90"
)
foreach(t IN LISTS tests)
    string(MAKE_C_IDENTIFIER ${t} t)
    list(APPEND test-srcs "test/test_${t}.f90")
endforeach()

add_executable(
    "fortran-workshop-tester"
    "${test-srcs}"
)
target_link_libraries(
    "fortran-workshop-tester"
    PRIVATE
    "fortran-workshop-lib"
    "test-drive::test-drive"
)

target_include_directories("fortran-workshop-tester" INTERFACE ${CMAKE_CURRENT_BINARY_DIR})

foreach(t IN LISTS tests)
    add_test("${t}" "fortran-workshop-tester" "${t}")
endforeach()
