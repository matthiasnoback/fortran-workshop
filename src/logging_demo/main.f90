program example
   use m_logging, only: t_abstract_logger, get_logger

   implicit none(type, external)

   ! Depend on the abstract logger
   class(t_abstract_logger), allocatable :: logger

   logger = get_logger()

   call logger%log('[config] Loaded')

end program example
