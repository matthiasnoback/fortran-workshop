set(PROGRAM_FILES
    ./hello_world_demo.f90
    ./integration_demo.f90
    ./logging_demo.f90
    ./performance_demo.f90
    ./timer_list_demo.f90
    ./timer_linked_list_demo.f90
)

foreach(PROGRAM_FILE IN LISTS PROGRAM_FILES)
    get_filename_component(PROGRAM_NAME ${PROGRAM_FILE} NAME_WE)
    add_executable(${PROGRAM_NAME} ${PROGRAM_FILE})
    target_link_libraries(${PROGRAM_NAME} PRIVATE fortran-workshop-lib)
endforeach()
