!
! Utility subprograms for tgcm:
!
!-------------------------------------------------------------------
      real function sddot(n,x,y)
      implicit none
!
! Call sdot (single precision) if on Cray, or ddot (double precision) 
!   if on SGI. (ddot must be called even if -r8 on sgi compiler command 
!   line). Ddot is from -lblas on the sgi.
! On IBM AIX use dot_product()
!
! 2/10/00: removing incx,incy args (i.e., incx=incy=1 on unicos
!   and irix -- IBM dot_product does not use increment args --
!   this function must be called with stride-1 vectors 
!   (see bndry.f, bndry2.f, bndrya.f, threed.f, transf.f)
!
      integer,intent(in) :: n
      real,intent(in) :: x(n),y(n)
!
!nm122007:#elif AIX
      sddot = dot_product(x,y)
!nm122007:#else
!nm122007:      write(6,"('>>> WARNING sddot: unresolved OS pre-processor',
!NM122007:     |  ' directive.')")
!nm122007:#endif
!      print *, "!DBG! FUNCTION SDDOT FINISHED!"
      end function sddot
