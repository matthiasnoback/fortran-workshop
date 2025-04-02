set(library_name fortran-workshop-lib)

add_library(${library_name} STATIC
    common/command_line.f90
    common/precision.f90
    hello_world.f90
    logging.f90
    stopwatch.f90
    vector.f90
)

# Allows main project to find .mod files for used modules
target_include_directories(${library_name} INTERFACE ${CMAKE_CURRENT_BINARY_DIR})

target_link_libraries(${library_name} PRIVATE
    all_compiler_warnings
    compiler_warnings_as_errors
)
