program example
   use m_logging, only: get_logger

   implicit none(type, external)

   call get_logger()%log('A message')

   call get_logger()%log('Another message')
end program example
