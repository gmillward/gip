#include "defs.h"
!
      module cons_module
      use params_module,only: nmlat,nmlon,nmlonp1
       
      implicit none
      
      real ::
     |  pi,             ! set with 4*atan(1)    C(110)
     |  dtr,            ! degrees-to-radians (pi/180.)
     |  rtd             ! radians-to-degrees (180./pi)
     
      real,parameter ::
     |  pi_dyn=3.14159265358979312 ! pi used in dynamo calculations
      real,parameter :: 
     |  re   = 6.37122e8,       ! earth radius (cm)                
     |  h0 =9.0e6, r00 =re+h0	! use mean earth radius - variable renamed r00 here so as not to interfere with r0 variable in GIP
      real :: 
     |  dlatm, dlonm,    ! grid spacing
     |  ylatm(nmlat),    ! magnetic latitudes (radians)
     |  ylonm(nmlonp1),  ! magnetic longitudes (radians)
     |  ylatm_deg(nmlat),    ! magnetic latitudes (degree)
     |  ylonm_deg(nmlonp1),  ! magnetic longitudes (degree)
     |  rcos0s(nmlat),   ! cos(theta0)/cos(thetas)
     |  dt1dts(nmlat)    ! dt0dts/abs(sinim) (non-zero at equator)
!
! Critical colatitude limits for use of Heelis potential in dynamo:
      real,parameter :: 
!     |  crit(2) = (/0.261799387, 0.523598775/) ! original values
     |  crit(2) = (/0.523598775, 0.61086524/)  ! plasmasphere has zero  
!nm031407:     |  crit(2) = (/0.523598775, 0.525298775/)  !nm041106: test
        ! conductances aboce |lam|>60 deg therefore I set the pcb to 60deg
        ! and the auroral zone equator boundary to 55 deg
        ! now above |lam|>60 set the conductances to some value which shouldn't
        ! matter and below it's an interpolation between high and low latitude 
        ! potential
!nm040607:     |  crit(2) = (/0.5314827561378479, 0.61086524/)  !crit(1) corresponds to max lat gip(83)=59.54828119329674  !nm040607:

!      integer, parameter :: iyr = IYR,
!     |   iday = IDAY
!      real, parameter :: secs = SECS
     
      contains
!-----------------------------------------------------------------------
      subroutine init_cons
      
! Local:
      integer :: j,i
      real,parameter :: r1=1.06e7, alfa=1.668
      real :: 
     |  tanth0(nmlat),
     |  tanths(nmlat),
     |  theta0(nmlat),
     |  hamh0(nmlat)
      real :: tanths2
!
      pi = 4.*atan(1.)                ! C(110)
      dtr = pi/180.                   ! degrees to radians
      rtd = 180./pi                   ! radians to degrees
      dlatm = pi_dyn/float(nmlat-1) ! note use of pi_dyn
      dlonm = 2.*pi_dyn/float(nmlon) 

! Set magnetic latitudes ylatm and magnetic longitudes ylonm:
!
! ylatm is equally spaced in theta0, but holds corresponding value
!   of thetas.
      do j=1,nmlat
        theta0(j) = -pi_dyn/2.+float(j-1)*dlatm ! note use of pi_dyn
      enddo ! j=1,nmlat
      do j=2,nmlat-1
        tanth0(j) = abs(tan(theta0(j)))
        hamh0(j) = r1*tanth0(j)+r00*tanth0(j)**(2.+2.*alfa)/
     |    (1.+tanth0(j)**2)**alfa
        tanths(j) = sqrt(hamh0(j)/r00)
        ylatm(j) = sign(atan(tanths(j)),theta0(j))
        ylatm_deg(j) = ylatm(j)*rtd
        rcos0s(j) = sqrt((1.+tanths(j)**2)/(1.+tanth0(j)**2))
        tanths2  = tanths(j)**2
        dt1dts(j) = 
     |    (r00*sqrt(1.+4.*tanths2)*(1.+tanths2))/
     |    (r1*(1.+tanth0(j)**2)+2.*r00*tanth0(j)**(2.*alfa+1.)*
     |    (1.+alfa+tanth0(j)**2)/(1.+tanth0(j)**2)**alfa)
        
      enddo ! j=2,nmlat-1
      
! Magnetic poles:
      ylatm(1)     = theta0(1)
      ylatm(nmlat) = theta0(nmlat)
      ylatm_deg(1)     = ylatm(1)*rtd    
      ylatm_deg(nmlat) = ylatm(nmlat)*rtd 
      rcos0s(1)    = 1.
      rcos0s(nmlat)= 1.
      
! Magnetic longitudes:
      do i=1,nmlonp1
        ylonm(i) = -pi_dyn+float(i-1)*dlonm
        ylonm_deg(i) = ylonm(i)*rtd
      enddo ! i=1,nmlonp1



!
      
      end subroutine init_cons
!-----------------------------------------------------------------------
      end module cons_module
