add_executable(hello_world_demo hello_world_demo.f90)
target_link_libraries(hello_world_demo PRIVATE fortran-workshop-lib)

add_executable(logging_demo logging_demo.f90)
target_link_libraries(logging_demo PRIVATE fortran-workshop-lib)

add_executable(performance_demo performance_demo.f90)
target_link_libraries(performance_demo PRIVATE fortran-workshop-lib)
