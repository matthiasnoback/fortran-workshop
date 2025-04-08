set(library_name fortran-workshop-lib)

# Run `find -name "*.f90"` in Git Bash and copy the list below
add_library(${library_name} STATIC
    ./common/command_line.f90
    ./common/precision.f90
    ./common/time.f90
    ./hello_world.f90
    ./integration/damped_oscillator.f90
    ./integration/trapezoid.f90
    ./logging/abstract.f90
    ./logging/everything.f90
    ./logging/facade_implementation.f90
    ./logging/facade.f90
    ./logging/file.f90
    ./logging/stdout.f90
    ./stopwatch/facade.f90
    ./stopwatch/timer_linked_list.f90
    ./stopwatch/timer_list.f90
    ./stopwatch/timer.f90
    ./vector.f90
)

# Allows main project to find .mod files for used modules
target_include_directories(${library_name} INTERFACE ${CMAKE_CURRENT_BINARY_DIR})

target_link_libraries(${library_name} PRIVATE
    all_compiler_warnings
    compiler_warnings_as_errors
)
