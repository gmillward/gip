!
#include "defs.h"
      
      module params_module
!
! Magnetic grid:
      integer,parameter ::
     |  nmlat = NMLAT,         ! number of magnetic latitudes
     |  nmlon = NMLON,         ! number of magnetic longitudes
     |  nmlonp1=nmlon+1,
     |  nmlatp1=nmlat+1,
     |  nmlath=(nmlat+1)/2     ! index to magnetic equator
!
      end module params_module
