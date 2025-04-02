program hello_world_demo
   use hello_world, only: hello_world_message

   implicit none(type, external)

   print *, hello_world_message()

end program hello_world_demo
