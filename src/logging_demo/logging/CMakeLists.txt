set(library_name logging-lib)

add_library(${library_name} STATIC
    m_logging.f90
)

# Allows main project to find .mod files for used modules
target_include_directories(${library_name} INTERFACE ${CMAKE_CURRENT_BINARY_DIR})

target_link_libraries(${library_name} PRIVATE
    common-lib
    all_compiler_warnings
    compiler_warnings_as_errors
)
