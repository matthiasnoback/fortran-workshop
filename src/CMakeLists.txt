set(library_name fortran-workshop-lib)

# Run `find -name "*.f90"` in Git Bash and copy the list below
add_library(${library_name} STATIC
    ./benchmark/diagnostics.f90
    ./benchmark/facade.f90
    ./common/command_line.f90
    ./common/error_handling.f90
    ./common/interpolation.f90
    ./common/precision.f90
    ./common/random.f90
    ./common/strings.f90
    ./common/time.f90
    ./common/to_string.f90
    ./config/loading.f90
    ./file/operations.f90
    ./functional/lists.f90
    ./geometry/point.f90
    ./geometry/polygon.f90
    ./geometry/polyline.f90
    ./hydraulic_structures/pump.f90
    ./integration/damped_oscillator.f90
    ./integration/trapezoid.f90
    ./logging/abstract.f90
    ./logging/aggregation.f90
    ./logging/everything.f90
    ./logging/facade_implementation.f90
    ./logging/facade.f90
    ./logging/file.f90
    ./logging/stdout.f90
    ./logging/timestamp.f90
    ./stopwatch/facade_implementation.f90
    ./stopwatch/facade.f90
    ./stopwatch/timer_linked_list.f90
    ./stopwatch/timer_list.f90
    ./stopwatch/timer.f90
    ./filter_map_reduce.f90
    ./hello_world.f90
    ./vector.f90
)

# Allows main project to find .mod files for used modules
target_include_directories(${library_name} INTERFACE ${CMAKE_CURRENT_BINARY_DIR})

target_link_libraries(${library_name} PRIVATE
    all_compiler_warnings
    compiler_warnings_as_errors
)
