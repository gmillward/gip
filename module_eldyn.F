      module MD_Eldyn
      
      use cons_module,only: init_cons  ! iyr,iday,secs

      use magfield_module,only: sunloc , sunlons

      use dynamo_module,only: dynamo, zigm11,zigm22,zigmc,zigm2,rim

      use heelis_module,only: highlat,init_heelis  ! use_rcm_bnd,get_vdrop

      use GIP_NMLT_module

      use params_module,only: nmlonp1, nmlat

      implicit none

      integer,parameter,public:: array_size_time=2  !former "time": array size in time
      integer,public :: eldyn_iyr
      integer,public :: eldyn_iday
      real,public :: eldyn_secs1           !time-dependent time in [sec]
      real :: vdrop     ! potential drop [kV] 

!---
      PRIVATE
      PUBLIC :: Eldyn_Init, Eldyn_Subs, Eldyn_end

      contains

!-----------------------------------------------------------------------
!(1) initialization of dynamo solver
      SUBROUTINE  Eldyn_Init ()
      IMPLICIT NONE

! calculate different fields
      call init_cons               ! done only once
      call init_heelis             ! done only once

      end SUBROUTINE  Eldyn_Init

!----------------------------------------------------------------------

      SUBROUTINE Eldyn_Subs ( n_time, n_start )

      use zigm_gip_module,only:get_zigm_from_gip

      implicit none

      integer,intent(in) :: n_time
      integer,intent(in) :: n_start
      real :: secs1_min                   !time-dependent time in [min]

      secs1_min = eldyn_secs1/60.0 !convert sec --> min

      call sunloc(eldyn_iyr,eldyn_iday,eldyn_secs1)   ! every timestep

      call get_zigm_from_gip ( )

      call highlat   !nm021507:( vdrop )                           ! every timestep

      call dynamo                   ! every timestep

      end SUBROUTINE Eldyn_Subs

!----------------------------------------------------------------------
!(3) finalize the electrodynamic solver
      SUBROUTINE Eldyn_end ( )

      end SUBROUTINE Eldyn_end
!----------------------------------------------------------------------
      end  module MD_Eldyn
