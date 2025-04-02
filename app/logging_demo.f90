program logging_demo
   use logging_facade, only: get_logger
   use logging_abstract, only: t_abstract_logger

   implicit none(type, external)

   class(t_abstract_logger), pointer :: logger

   logger => get_logger(); 
   call logger%log('A message')

   call logger%log('Another message')

   ! Only to demonstrate that the final procedures will be invoked
   deallocate (logger)

end program logging_demo
