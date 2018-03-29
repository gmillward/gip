      module dynamo_module
!----------------------------------------------------------------------- 
! BOP
! !MODULE: dynamo_module
! !DESCRIPTION: 
!
! Module for electrodynamic dynamo (the "e" in tiegcm). 
!
! 5/02 btf: This is the "new" dynamo code, adapted by Astrid Maute
! and Art Richmond from the original ("old") dynamo in tgcm15,
! written by E. Cicely Ridley completed in about 1992. 
!
! Sub prep_dynamo is called from advance. Prep_dynamo prepares dynamo 
! input fields and gathers them to the root task (mp_dynamo_gather).
! Next, advance calls sub dynamo (the dynamo driver), which controls
! the dynamo subroutine calls. Prep_dynamo and the dynamo driver are
! called once per timestep, after sub dynamics.
!
! Since the dynamo is serial (as of 4/02), the master task must receive
! subdomain data from other tasks for input fields to the dynamo 
! (on the geographic grid). These fields are exchanged via sub 
! mp_dynamo_gather which is called from prep_dynamo below.
!
! !USES:
!
      use params_module,only: 
     |  nmlon,  ! number of geomagnetic grid longitudes = 80
     |  nmlonp1,! nmlon+1                               = 81
     |  nmlat,  ! number of geomagnetic grid latitudes  = 97
     |  nmlatp1,! nmlat+1                               = 98
     |  nmlath  ! (nmlat+1)/2 (index to magnetic equator) = 49

!
! Dimensions of the 5 grid resolutions for the multi-grid PDE:
      integer,parameter ::
     |  nmlon0=nmlon+1,
     |  nmlat0=(nmlat+1)/2,
     |  nmlon1=(nmlon0+1)/2,
     |  nmlat1=(nmlat0+1)/2,
     |  nmlon2=(nmlon1+1)/2,
     |  nmlat2=(nmlat1+1)/2,
     |  nmlon3=(nmlon2+1)/2,
     |  nmlat3=(nmlat2+1)/2,
     |  nmlon4=(nmlon3+1)/2,
     |  nmlat4=(nmlat3+1)/2
!
! Space needed for descretized coefficients of dynamo pde at all 
!   5 levels of resolution:
!
      integer,parameter ::
     |  ncee=10*nmlon0*nmlat0+9*(nmlon1*nmlat1+nmlon2*nmlat2+nmlon3*
     |    nmlat3+nmlon4*nmlat4)
!
! Coefficients are stored in 1-d array cee(ncee)
! cee transmits descretized dynamo PDE coefficients to the multi-grid 
!   mudpack solver. (cee was formerly in ceee.h)
! The common block /cee_com/ is retained from earlier versions because
!   of the equivalencing below of coefficient arrays c0, c1, etc.
!
      real :: cee(ncee) 
      common/cee_com/ cee
!
! The following parameters nc0,nc1,... are pointers to the beginning of 
!   the coefficients for each level of resolution.
!
      integer,parameter ::
     | nc0=1,
     | nc1=nc0+10*nmlon0*nmlat0,
     | nc2=nc1+9 *nmlon1*nmlat1,
     | nc3=nc2+9 *nmlon2*nmlat2,
     | nc4=nc3+9 *nmlon3*nmlat3
!
! nc(1:6) are pointers to beginning of coefficient blocks at each of 
!   5 levels of resolution: 
! nc(1) = nc0, pointer to coefficients for highest resolution.
! nc(2) = nc1, pointer to coefficients at half the resolution of nc0, 
!   and so on for nc(3), nc(4), nc(5), etc.
! nc(6) = ncee, the dimension of the entire cee array, containing
!   coefficients for all 5 levels of resolution.
! 
      integer :: nc(6)
!
      real :: 
     |  c0(nmlon0,nmlat0,10),
     |  c1(nmlon1,nmlat1,9),
     |  c2(nmlon2,nmlat2,9),
     |  c3(nmlon3,nmlat3,9),
     |  c4(nmlon4,nmlat4,9)
      equivalence 
     |  (cee,c0), 
     |  (cee(nc1),c1),
     |  (cee(nc2),c2),
     |  (cee(nc3),c3),
     |  (cee(nc4),c4)
!
! Unmodified coefficients for using modified mudpack:
      real,dimension(nmlon0,nmlat0,9) :: cofum
!
! phim: Single level dynamo potential in geomagnetic coordinates, as
!   output by PDE solver mud (formerly in dynphi.h):
! The dynamo is symmetric about the magnetic equator, but the high latitude
!  is anti-symmetric in both hemispheres.  However, since Mudpack uses the
!  NH potential pattern, then the SH potential pattern must be added
!  back into the 2-D phim before the call threed, and before it is
!  transformed to geographic coordinates.
! jn index for NH part of potential (nmlat down to ~nmlat0)
! jp index for NH pfrac (nmlat0 down to 1), the fraction of the dynamo
!
      real,dimension(nmlonp1,nmlat),public :: phim
      integer :: jn,jp
!      
! electric field
      real,dimension(nmlonp1,nmlat),public :: ed1,ed2
!
! Coefficients and RHS terms for PDE on geomagnetic grid:
! (formerly in coefm.h)
!
      real,dimension(nmlonp1,nmlat),public ::
     |  zigm11,  ! sigma11*cos(theta0)
     |  zigmc,   ! sigmac
     |  zigm2,   ! sigma2
     |  zigm22   ! sigma22/cos(theta0)
!
! rim(1)=id(1), rim(2)=id(2)/cos(theta0)
      real,dimension(nmlonp1,nmlat,2),public :: rim 
      real,dimension(nmlonp1,nmlath)  :: rhs ! right-hand side 
!
! isolve = 0 -> original mud version 5.
! isolve = 1 -> muh hybrid solver (only as direct solver -- slow)
! isolve = 2 -> modified mudpack solver (mudmod)
! Default is isolve=2 for DYNAMO=2 (new dynamo)
! For dot products:
      integer,parameter :: isolve = 2
      real,parameter :: unitvm(nmlon)=1.
!
!nm031607: the switch was moved to a namelist module
!nm031607:! am 10/04 define potential model : NONE or HEELIS - first test NONE
!nm031607:!       character(len=*),parameter :: potential_model='NONE'
!nm031607:       character(len=*),parameter :: potential_model='HEELIS'
!
! Electric potential from heelis or weimer:
      real,dimension(nmlonp1,nmlat0) :: pfrac  ! NH fraction of potential
      real,dimension(nmlonp1,nmlat)  :: phihm  ! potential in magnetic
!      
! Work array (private to this module)
      real :: wkarray(-15:nmlon0+16,nmlat0)
!
! !REVISION HISTORY:
! 24.06.05  Astrid Maute: merge TIEGCM version1.8 into coupling with CTIP
!           for George Millward
! 05.02.04  Astrid Maute: include headers
! 
! comments from the original coupled code with CTIP
! am 10/04: adapte the electrodynamo from tiegcm1.6 for the
! coupling with a plasmasphere model
! -electro-dynamo is serial
! -high latitude potential is still in the code but shouldn't be 
! specified (set crit(1) and crit(2) to zero or small value
! crit(1), crit(2) colatitude for boundary of specified high lat. 
! potential (lat_m < crit(1) ) and transitions zone (crit(1)< lat_m < crit(2)
! - output eletric potential in magnetic coordinates
!          electric field in magnetic coordinates
!
! Module for electrodynamic dynamo (the "e" in tiegcm). 
!
! 5/02 btf: This is the "new" dynamo code, adapted by Astrid Maute
! and Art Richmond from the original ("old") dynamo in tgcm15,
! written by E. Cicely Ridley completed in about 1992. 
! 
! EOP
!
      PRIVATE :: 
     &   transf,rhspde,clearcee,stencil,stencmd,htrpex
     &,  cnm,cnmmod,divide,edges,stenmod,stenmd,threed
      PUBLIC :: dynamo,ceee

      contains
!-----------------------------------------------------------------------
!
! BOP
! !IROUTINE: dynamo 
! !INTERFACE:
      subroutine dynamo(zigm11_in, zigm22_in, zigmc_in, zigm2_in,
     &                  rim_in, ed1_out, ed2_out)
! !USES:
      use cons_module,only: dlatm,dlonm,pi_dyn,ylatm,rtd      
!
! !DESCRIPTION:
! Transform needed fields to geomagnetic coordinates
! Perform field-line integrations
! Evaluate PDE coefficients and RHS
! The PDE is divided by 1/ DT0DTS (in dyncal divided by 1/cos(theta_0)
! Sigma_(phi phi) = zigm11/ rcos0s * dt0dts
! Sigma_(lam lam) = zigm22 * rcos0s / dt0dts
! Sigma_(phi lam) = +-(zigm2-zigmc)
! Sigma_(lam phi) = -+(zigm2+zigmc)
! K_(m phi)^D     =   rim(1) * dt0dts
! K_(m lam)^D     = +-rim(2) * rcos0s
!
! !RETURN VALUE:
! !PARAMETERS: 
! !REVISION HISTORY:
! 05.02.04  <Astrid Maute> <include header> 
! 
! EOP
! 
! Local:
      integer :: i,j,jj,jjj,j0,jntl,k,n,ncc,nmaglat,nmaglon,ier
      real :: sym
      real :: array(-15:nmlon0+16,nmlat0),cs(nmlat0)
      character :: fname*10,labl*56,units*12

      real,dimension(nmlonp1,nmlat) ::
     |  zigm11_in,  ! sigma11*cos(theta0)
     |  zigmc_in,   ! sigmac
     |  zigm2_in,   ! sigma2
     |  zigm22_in   ! sigma22/cos(theta0)
      real,dimension(nmlonp1,nmlat,2) :: rim_in
      real,dimension(nmlonp1,nmlat) :: ed1_out,ed2_out

      zigm11 = zigm11_in
      zigmc = zigmc_in
      zigm2 = zigm2_in
      zigm22 = zigm22_in
      rim = rim_in

      call transf

!
! Fold southern hemisphere over on to northern  (was in transf.F version tgcm15)
! -Value at the equator is also folded therefore at the equatorial boundary
!  factor of 1/2 introduced
! -added values in array index 49-97 from equator to poles
! -SH array (1-49) has original SH values
! -reverse sign of zigmc (to be compatible with Cicely's) 
! -sign of K_(m lam)^D in southern hemisphere is reversed, therefore 
!     K_(m lam)^N - (- K_(m lam)^S)
!
! zigm11 = Sigma_(phi phi)(0)^T
! zigm22 = Sigma_(lam lam)(0)^T
! zigmc  =-Sigma_c(0)^T 
! zigm2  = Sigma_h(0)^T 
! rim(1) = K_(m phi)^D(0)^T
! rim(2) = K_(m lam)^D(0)^T
!




      do j=1,nmlath
        do i=1,nmlonp1
          zigm11(i,nmlatp1-j) = (zigm11(i,nmlatp1-j)+zigm11(i,j))
          zigmc(i,nmlatp1-j)  =  zigmc(i,nmlatp1-j)+zigmc(i,j)
          zigmc(i,nmlat+1-j)  = -zigmc(i,nmlat+1-j)
          zigm2(i,nmlat+1-j)  =  zigm2(i,nmlat+1-J)+zigm2(i,j)
          zigm22(i,nmlat+1-j) = (zigm22(i,nmlat+1-J)+zigm22(i,j))
  
          rim(i,nmlatp1-j,1)  = (rim(i,nmlat+1-j,1)+rim(i,j,1))
          rim(i,nmlatp1-j,2)  = (rim(i,nmlat+1-j,2)+rim(i,j,2))  
!         rim(i,nmlatp1-j,2)  = (rim(i,nmlat+1-j,2)-rim(i,j,2))  
        enddo ! i=1,nmlonp1
      enddo ! j=1,nmlath
!
! Calculate RHS of PDE from rim(1) and rim(2) (was in transf.F version tgcm15)
!  [( d K_(m phi)^D / d phi /(cos(theta_m)?) +
!  (d [ K_(m lam)^D * cos(lam_m)]/ d lam_m ) /cos ( lam_m) ] * R / (RCOS0S*DT0DTS)
! ~ J_(Mr)*r^2*cos(theta_m)/cos(theta_0)/DT0DTS
!
      call rhspde
!
! Set index array nc and magnetic latitude cosine array:
! nc pointes to the start of the coefficient array for each level
      nc(1) = nc0
      nc(2) = nc1
      nc(3) = nc2
      nc(4) = nc3
      nc(5) = nc4
      nc(6) = ncee
!
! Use pi_dyn from cons module rather than 4*atan(1) to avoid small
! differences generated by the atan in -lmass lib (-lmass was not
! used in earlier versions).
!
      do j=1,nmlat0
        cs(j) = cos(pi_dyn/2.-(nmlat0-j)*dlatm)
      enddo ! j=1,nmlat0
!
! Set up difference coefficients. 
! mixed terms: factor 4 from 5-point diff. stencil
! "NH" array index 49:97 equator to pole
! zigmc  = Sigma_(phi lam)^T(0)/( 4*d lam_0* d lon )
! zigm2  = Sigma_(phi lam)^T(0)/( 4*d lam_0* d lon )
! zigm22 = Sigma_(lam lam)^T(0)*cos(lam_0)/d lam_0^2
! zigm11 = Sigma_(phi phi)^T(0)/ cos(lam_0) / d lon^2 )
!
! "SH" array index 49:1 equator to pole
! zigmc  = -Sigma_(phi lam)^T(0)/( 4*d lam_0* d lon )
! zigm2  = -Sigma_(phi lam)^T(0)/( 4*d lam_0* d lon )
! zigm22 = Sigma_(lam lam)^T(0)*cos(lam_0)/d lam_0^2
! zigm11 = Sigma_(phi phi)^T(0)/ cos(lam_0) / d lon^2 )
! 
!
      j0 = nmlat0-nmlath
      do j=1,nmlath       !  1,49 (assuming nmlat=97)
        jj = nmlath+j-1   ! 49,97 added values ()^T 
        jjj = nmlath-j+1  ! 49,1
!
        do i=1,nmlonp1
          zigmc(i,jj)   = (zigmc(i,jj) +zigm2(i,jj))/(4.*dlatm*dlonm)
          zigm2(i,jj)   = zigmc(i,jj)-2.*zigm2(i,jj)/(4.*dlatm*dlonm)
          zigm22(i,jj)  = zigm22(i,jj)*cs(j0+j)/dlatm**2
          zigmc(i,jjj)  = -zigmc(i,jj)
          zigm2(i,jjj)  = -zigm2(i,jj)
          zigm22(i,jjj) = zigm22(i,jj)
        enddo ! i=1,nmlonp1
        if (j /= nmlath) then
          do i = 1,nmlonp1
            zigm11(i,jj) = zigm11(i,jj)/(cs(j0+j)*dlonm**2)
            zigm11(i,jjj) = zigm11(i,jj)
          enddo
        endif
      enddo ! j=1,nmlath
!
! Set zigm11 to zero at megnetic poles to avoid floating exception 
! (values at poles are not used):
!
      do i = 1,nmlonp1
        zigm11(i,1)     = 0.0
        zigm11(i,nmlat) = 0.0
      enddo
!
! Clear array for difference stencils at all levels:
      call clearcee(cee,nmlon0,nmlat0)
!
! Calculate contribution to stencils from each PDE coefficient
! isolve = 0 -> original mud version 5.
! isolve = 1 -> muh hybrid solver (only as direct solver -- slow)
! isolve = 2 -> modified mudpack solver (modified and unmodified coefficients)
!
      if (isolve==2) then
        cofum(:,:,:) = 0. ! init
!
! Sigma_(phi phi)(0)/( cos(lam_0)*(d lon)^2 )
        call stencmd(zigm11(1,nmlat0),nmlon0,nmlat0,cee,1)
!
! Sigma_(lam lam)(0)*cos(lam_0)/(d lam_0)^2
        call stencmd(zigm22(1,nmlat0),nmlon0,nmlat0,cee,4)
!
! Sigma_(phi lam)(0)/( 4*d lam_0* d lon )
	zigmc(:,nmlat0) = -zigmc(:,nmlat0)
        call stencmd(zigmc(1,nmlat0),nmlon0,nmlat0,cee,2)
	zigmc(:,nmlat0) = -zigmc(:,nmlat0)
!
! Sigma_(lam phi)(0)/( 4*d lam_0* d lon )
	zigm2(:,nmlat0) = -zigm2(:,nmlat0)
        call stencmd(zigm2(1,nmlat0),nmlon0,nmlat0,cee,3)
	zigm2(:,nmlat0) = -zigm2(:,nmlat0)
!
! isolve /= 2: original or hybrid solver (only modified stencil).
      else
!
! Sigma_(phi phi)(0)/( cos(lam_0)*(d lon)^2 )
        call stencil(zigm11(1,nmlat0),nmlon0,nmlat0,cee,1)
!
! Sigma_(lam lam)(0)*cos(lam_0)*/(d lam_0)^2
        call stencil(zigm22(1,nmlat0),nmlon0,nmlat0,cee,4)
!
! Sigma_(phi lam)(0)/( 4*d lam_0* d lon )
	zigmc(:,nmlat0) = -zigmc(:,nmlat0)
        call stencil(zigmc(1,nmlat0),nmlon0,nmlat0,cee,2)
	zigmc(:,nmlat0) = -zigmc(:,nmlat0)
!
! Sigma_(lam phi)(0)/( 4*d lam_0* d lon )
	zigm2(:,nmlat0) = -zigm2(:,nmlat0)
        call stencil(zigm2(1,nmlat0),nmlon0,nmlat0,cee,3)
	zigm2(:,nmlat0) = -zigm2(:,nmlat0)
!
      endif ! isolve
!
! Insert RHS in finest stencil (formerly sub rths):
      do j = 1,nmlat0
        jj = nmlath-nmlat0+j
        do i = 1,nmlon0
          c0(i,j,10) = rhs(i,jj)
        enddo ! i = 1,nmlon0
      enddo ! j = 1,nmlat0
!
! Set boundary condition at the pole:
      call edges(c0,nmlon0,nmlat0)
      call edges(c1,nmlon1,nmlat1)
      call edges(c2,nmlon2,nmlat2)
      call edges(c3,nmlon3,nmlat3)
      call edges(c4,nmlon4,nmlat4)
      if (isolve==2) 
     |  call edges(cofum,nmlon0,nmlat0)
!
! Divide stencils by cos(lam_0) (not rhs):
      call divide(c0,nmlon0,nmlat0,nmlon0,nmlat0,cs,1)
      call divide(c1,nmlon1,nmlat1,nmlon0,nmlat0,cs,1)
      call divide(c2,nmlon2,nmlat2,nmlon0,nmlat0,cs,1)
      call divide(c3,nmlon3,nmlat3,nmlon0,nmlat0,cs,1)
      call divide(c4,nmlon4,nmlat4,nmlon0,nmlat0,cs,1)
      if (isolve==2) 
     |  call divide(cofum,nmlon0,nmlat0,nmlon0,nmlat0,cs,0)
!
! Set value of solution to 1. at pole:
      do i=1,nmlon0
        c0(i,nmlat0,10) = 1.
      enddo
!
! Modify stencils and RHS so that the NH high lat potential is inserted at
!  high latitude.  The SH high lat potential will be added back later.
!  pfrac = fraction of dynamo in solution in the NH. = 1 low lat, = 0 hi lat
!    cons_module: crit(1)=15, crit(2)=30 deg colats, or hi-lat > 75 deg,
!      dynamo < 60 deg, and combination between 60-75 mag lat.
! The dynamo is symmetric about the magnetic equator, but the high latitude
!  is anti-symmetric in both hemispheres.  However, since Mudpack uses the
!  NH potential pattern, then the SH potential pattern must be added
!  back into the 2-D phim before the call threed, and before it is
!  transformed to geographic coordinates.
!
      ncc = 1
      nmaglon = nmlon0
      nmaglat = nmlat0
      do n=1,5
        if (isolve==2) then
          call stenmd(nmaglon,nmaglat,cee(ncc),phihm(1,nmlat0),pfrac)
        else
          call stenmod(nmaglon,nmaglat,cee(ncc),phihm(1,nmlat0),pfrac)
        endif
        ncc = ncc+9*nmaglon*nmaglat
        if (n==1) ncc = ncc+nmaglon*nmaglat ! rhs is in 10th slot
        nmaglon = (nmaglon+1)/2
        nmaglat = (nmaglat+1)/2
      enddo ! n=1,5
!
      jntl = 0
!
      ier = 0 
      if(isolve==0) then
        call mud(rim,jntl,isolve,ier)	 ! solver in mud.F
        if(ier < 0 ) then 	! not converged
	  write(6,*) 'muh: use direct solver'
	  call muh(rim,jntl) 		! solver in mud.F
	endif
      elseif (isolve==1) then
        call muh(rim,jntl)        	! solver in muh2cr.F
      elseif (isolve==2) then
        call mudmod(rim,jntl,isolve,ier)! solver in mudmod.F
        if(ier < 0 ) then 	! not converged
	  write(6,*) 'muh: use direct solver'
	  call muh(rim,jntl) 		! solver in mud.F
	endif
      else
        write(6,*) 'dynamo: solver type ',isolve,' not implemented.'
        stop 'isolve'
      endif
!
! Copy output potential from rim to phim(nmlonp1,nmlat):
!  Correct the SH potential for the anti-symmetric imposed NH high lat poten
!  pfrac = fraction of dynamo in solution in the NH. = 1 low lat, = 0 hi lat
!    cons_module: crit(1)=15, crit(2)=30 deg colats, or hi-lat > 75 deg,
!      dynamo < 60 deg, and combination between 60-75 mag lat.
!  ie, need (1.-pfrac(i,nmlat0)) at phim(i,1), etc
! jn index for NH part of potential (nmlat down to ~nmlat0)
! jp index for NH pfrac (nmlat0 down to 1)
!
      do j=1,nmlat0
        jn = nmlat - j + 1
        jp = nmlat0 - j + 1
        do i=1,nmlonp1
          phim(i,j)=rim(i,j,1)+(1.-pfrac(i,jp))*(phihm(i,j)-phihm(i,jn))
!	  write(6,'(2i4,3(x,f12.5))') i,j,rim(i,j,1),pfrac(i,jp),
!     |          phihm(i,j)
        enddo ! i=1,nmlonp1
      enddo ! j=1,nmlat0
      do j=nmlat0+1,nmlat
        jp = j - nmlat0
        do i=1,nmlonp1
          phim(i,j) = rim(i,j,1)
        enddo ! i=1,nmlonp1
      enddo ! j=1,nmlat 
!
! Call threed to calculate 2-d electric potential array in geomagnetic coordinates
!   from 2-d solver output phim, corrected for the SH potential
!
      call threed
!    

      ed1_out = ed1
      ed2_out = ed2

      end subroutine dynamo
!-----------------------------------------------------------------------
! BOP
! !IROUTINE: transf
! !INTERFACE:
      subroutine transf
!
! !USES:
      use cons_module,only: dt1dts,rcos0s
!
! !DESCRIPTION: 
!       - include the boundary condition at the equator
!       - transformation from lam_m to lam_0 (regular grid)
!
! !REVISION HISTORY:
! 05.02.04  <Astrid Maute> <include header> 
! 
! EOP
!
! Local:
      integer :: i,ii,k,kk,j,jj,lat,n
!
!
! External:
      real,external :: sddot ! in util.F
!
! From sub fieldline_integrals:
!   zigm11 is int[sig_p*d_1^2/D] ds,   i.e. Sigma_(phi phi)/abs(sin Im)
!   zigm22 is int[sig_p*d_2^2/D] ds,   i.e. Sigma_(lam lam)*abs(sin Im)
!   zigmc  is int[sig_p*d_1*d_2/D] ds, i.e. Sigma_c
!   zigm2  is int[sigma_h] ds,         i.e. Sigma_h
!
!   rim1 is int[(sigma_h-sigma_p*d_1*d_2/D)u_e1 + sigma_p*d_1^2/D u_e2] *A(h_r)*
!                B_e3 ds, i.e.  K_(m phi)^D/abs(sin Im)
!   rim2 is int[(sigma_h+sigma_p*d_1*d_2/D)u_e2 - sigma_p*d_2^2/D u_e1] *A(h_r)*
!                B_e3 ds, K_(m lam)^D ( minus in northern hemisphere
!   Change sign of RIM(2) in S. hemisphere to be compatible with transf
! At this point, rim2 is +-K_(m lam)^D
!
! am 10/04 leave this out and check what difference it makes
! but make sure that field-line integral at equator is definite
!
! Equatorial values:
! Assume that quantities primarily dependent on Pedersen conductivity
!   have field-line integrals 1/4 as large as the averages for next-higher
!   field lines; quantities primarily dependent on Hall conductivity
!   have field-line integrals 0.12 as large as the averages for next-higher
!   field lines.  Exact values chosen should not be important for potential
!   calculation, as long as they are physically reasonable and not too
!   different from adjacent values.
! add factor 1/2 for the equatorial values since both hemispheres are
! added together - two physical points but equator is only one point
!
!      j = nmlat/2+1
!      do i = 1,nmlon
!        zigm11(i,j)= .125*(zigm11(i,j-1)+ zigm11(i,j+1))
!        zigm22(i,j)= .125*(zigm22(i,j-1)+ zigm22(i,j+1))
!        zigmc(i,j) = .125*(zigmc(i,j-1) + zigmc(i,j+1))
!        zigm2(i,j) = .06 *(zigm2(i,j-1) + zigm2(i,j+1))
!        rim(i,j,1) = .06 *(rim(i,j-1,1) + rim(i,j+1,1))
!        rim(i,j,2) = .06 *(rim(i,j-1,2) + rim(i,j+1,2))
!      enddo ! i = 1,nmlon
!
! Include the boundary condition at the equator eq.(5.30) in
! Richmond (1995) Ionospheric Electrodynamics use. Mag. Apex Coord. 
!   
! Sigma_(phi phi)/|sin Im| = 0.5*Sigma_cowling/|sin Im|
!        = 0.5/ |sin Im|*[Simag_(phi phi) -
!                        Sigma_(phi lam)*Sigma_(lam phi)/Sigma_(lam lam)]
!        = 0.5/|sin Im|*[Simag_(phi phi) + 
!                       (Sigma_h-Sigma_c)*(Sigma_h + Sigma_c)/Sigma_(lam lam)]
!  K_(m phi)^D / |sin I_m| =  1/2/|sin Im|*(K_(m phi) - 
!                       Sigma_(phi lam)/Sigma_(lam lam)*K_(m lam)^D)
! factor 1/2 is taken care of above when the conductances at the
! equator are set
!
      j = nmlath      ! nmlath = (nmlat+1)/2
      do i = 1,nmlon
        zigm11(i,j) = zigm11(i,j)+ (zigm2(i,j)-zigmc(i,j))*
     |                (zigm2(i,j)+zigmc(i,j))/zigm22(i,j)
        rim(i,j,1)  = rim(i,j,1) - (zigm2(i,j)-zigmc(i,j))/
     |                zigm22(i,j)*rim(i,j,2)
!modified by TW 110409
!       zigm11(i,j) = 0.5*zigm11(i,j)
!       rim(i,j,1)  = 0.5*rim(i,j,1)
!mod end
        zigm11(i,j) = zigm11(i,j)
        rim(i,j,1)  = rim(i,j,1)
      enddo

!        
! Using notation of Richmond (1995) on right-hand side below:
! Sigma_(phi phi)/ |sin I_m|  = zigm11
! Sigma_(lam lam) * |sin I_m| = zigm22
! Sigma_(phi lam)             = +/-(zigm2-zigmc)
! Sigma_(lam phi)             = -/+(zigm2+zigmc)
! K_(m phi)^D / |sin I_m|     = rim(1)
! K_(m lam)^D                 = +/-rim(2)
!
! Transforming PDE from original apex (lam_m) to new apex grid (lam_0)
!     lam_m is irregular spaced in latitude
!     lam_0 is regular spaced in latitude (used for derivatives)
! the whole PDE is divided by d lam_m /d lam_0
! 
! sign of K_(m lam)^D in southern hemisphere remains reversed.
! for the mixed terms the transformation factors cancel out (zigmc, zigm2)
! DT1DTS : d lam_0/ d lam_m / |sin I_m|
! RCOS0S : cos(lam_0)/ cos(lam_m)
!
! corfac: |sin I_m|*d lam_m/d lam_0 * cos(lam_0)/ cos(lam_m)
! zigm11: |sin I_m|*d lam_m/d lam_0 * cos(lam_0)/ cos(lam_m)
! zigm22: 1/|sin I_m|*d lam_0/d lam_m * cos(lam_m)/ cos(lam_0)
! rim(1): |sin I_m|*d lam_m/d lam_0
! rim(2): cos(lam_m)/ cos(lam_0)        
!
      do j=2,nmlat-1
        corfac = rcos0s(j)/dt1dts(j)
        do i=1,nmlon
          zigm11(i,j) = zigm11(i,j)*corfac
          zigm22(i,j) = zigm22(i,j)/corfac
          rim(i,j,1)  = rim(i,j,1)/dt1dts(j) 
          rim(i,j,2)  = rim(i,j,2)/rcos0s(j) 
        enddo ! i,nmlon
      enddo ! j=2,nmlat-1
!
! Periodic points:
      do j=1,nmlat
        zigm11(nmlonp1,j) = zigm11(1,j)
        zigmc (nmlonp1,j) = zigmc (1,j)
        zigm2 (nmlonp1,j) = zigm2 (1,j)
        zigm22(nmlonp1,j) = zigm22(1,j)
        rim(nmlonp1,j,:)  = rim(1,j,:)
      enddo ! j=1,nmlat
!
! zigm11 = Sigma_(phi phi)(0)= Sigma_(phi phi)(m) * cos lam_0 /cos lam_m * d lam_m /d lam_0
! zigm22 = Sigma_(lam lam)(0)= Sigma_(lam lam)(m) * cos lam_m /cos lam_0 * d lam_0 /d lam_m
! +-(zigm2-zigmc)= Sigma_(phi lam)(0) = Sigma_(phi lam)(m)
! -+(zigm2+zigmc)= Sigma_(lam phi)(0) = Sigma_(lam phi)(m)
! rim(1) = K_(m phi)^D(0)    = K_(m phi)^D(m) * d lam_m /d lam_0   
! rim(2) = +/-K_(m lam)^D(0) = +/-K_(m lam)^D(m) * cos lam_m /cos lam_0 
!  
! Compute polar values for the conductances, 4th order interpolation:
! 
      zigm11(1,    1) = (4.*sddot(nmlon,unitvm,zigm11(1,      2))-
     |  sddot(nmlon,unitvm,zigm11(1,      3)))/(3.*float(nmlon))
      zigm11(1,nmlat) = (4.*sddot(nmlon,unitvm,zigm11(1,nmlat-1))-
     |  sddot(nmlon,unitvm,zigm11(1,nmlat-2)))/(3.*float(nmlon))
      zigmc(1,    1) = (4.*sddot(nmlon,unitvm,zigmc(1,      2))-
     |  sddot(nmlon,unitvm,zigmc(1,      3)))/(3.*float(nmlon))
      zigmc(1,nmlat) = (4.*sddot(nmlon,unitvm,zigmc(1,nmlat-1))-
     |  sddot(nmlon,unitvm,zigmc(1,nmlat-2)))/(3.*float(nmlon))
      zigm2(1,    1) = (4.*sddot(nmlon,unitvm,zigm2(1,      2))-
     |  sddot(nmlon,unitvm,zigm2(1,      3)))/(3.*float(nmlon))
      zigm2(1,nmlat) = (4.*sddot(nmlon,unitvm,zigm2(1,nmlat-1))-
     |  sddot(nmlon,unitvm,zigm2(1,nmlat-2)))/(3.*float(nmlon))
      zigm22(1,    1) = (4.*sddot(nmlon,unitvm,zigm22(1,      2))-
     |  sddot(nmlon,unitvm,zigm22(1,      3)))/(3.*float(nmlon))
      zigm22(1,nmlat) = (4.*sddot(nmlon,unitvm,zigm22(1,nmlat-1))-
     |  sddot(nmlon,unitvm,zigm22(1,nmlat-2)))/(3.*float(nmlon))
! 
! Extend over longitude                                        
      do i = 2,nmlon
        zigm11(i,    1)  = zigm11(1,    1)
        zigm11(i,nmlat)  = zigm11(1,nmlat)
        zigmc(i,     1)  = zigmc(1,     1)
        zigmc(i, nmlat)  = zigmc(1, nmlat)
        zigm2(i,     1)  = zigm2(1,     1)
        zigm2(i, nmlat)  = zigm2(1, nmlat)
        zigm22(i,     1) = zigm22(1,    1)
        zigm22(i, nmlat) = zigm22(1,nmlat)
      enddo ! i = 2,nmlon
!
! RHS vector (I_1,I_2): average over poles:
      do i = 1,nmlon
        rim(i,1,1) = .5*(rim(i,2,1)-rim(1+mod(i-1+nmlon/2,nmlon),2,1))
        rim(i,nmlat,1) = .5*(rim(i,nmlat-1,1)-
     |    rim(1+mod(i-1+nmlon/2,nmlon),nmlat-1,1))
        rim(i,1,2) = .5*(rim(i,2,2)-rim(1+mod(i-1+nmlon/2,nmlon),2,2))
        rim(i,nmlat,2) = .5*(rim(i,nmlat-1,2)-
     |    rim(1+mod(i-1+nmlon/2,nmlon),nmlat-1,2))
      enddo ! i = 1,nmlon
! 
! Periodic points:
      do j=1,nmlat
        zigm11(nmlonp1,j) = zigm11(1,j)
        zigmc (nmlonp1,j) = zigmc (1,j)
        zigm2 (nmlonp1,j) = zigm2 (1,j)
        zigm22(nmlonp1,j) = zigm22(1,j)
        rim(nmlonp1,j,:)  = rim(1,j,:)
      enddo ! j=1,nmlat
!
! Save to secondary history:
!     real,dimension(nmlonp1,nmlat) ::
!    |  zigm11,  ! sigma11*cos(theta0)
!    |  zigmc,   ! sigmac
!    |  zigm2,   ! sigma2
!    |  zigm22   ! sigma22/cos(theta0)
!
!     do j=1,nmlat
!       do i=1,nmlonp1
!         zigm11_plt(i,:) = zigm11(i,j)
!         zigmc_plt (i,:) = zigmc (i,j)
!         zigm2_plt (i,:) = zigm2 (i,j)
!         zigm22_plt(i,:) = zigm22(i,j)
!         rim1_plt  (i,:) = rim   (i,j,1)
!         rim2_plt  (i,:) = rim   (i,j,2)
!       enddo ! i=1,nmlonp1
! 
! Folding south on to northern hemisphere and calculation of RHS has 
!   been moved to subroutine dynamo.
!
      end subroutine transf
!-----------------------------------------------------------------------
! BOP
! !IROUTINE: rhspde
! !INTERFACE:
      subroutine rhspde
! !USES:
      use cons_module,only: pi_dyn,dlatm,dlonm,r00
! !DESCRIPTION:
!  differentiate RHS 
!  R_0 * d lam_m/d lam_0 * 1/cos lam_0 [ d K_(m phi)^DT(0) / d phi  +
!  (d [ K_(m lam)^DT(0) * cos(lam_0)]/ d lam_0  ] 
!
! !REVISION HISTORY:
! 05.02.16  <Astrid Maute> <include header> 
! 
! EOP
!
! Local:
      integer :: i,j,jj
      real :: pi
      real,dimension(nmlat) :: tint1,tint2,tint3
      real,dimension(-1:nmlonp1+1) :: tint33
!
! External:
      real,external :: sddot ! in util.F
!
      pi = pi_dyn
!
! cos lam_0:
      do j=1,nmlat
        tint1(j) = cos(-pi/2.+(j-1)*dlatm)
      enddo ! j=1,nmlat
!
! Perform differentiation (1/ cos lam0 ) * d K_(m phi)^DT(0)/ d phi
! central differencing
! rhs(i,j) = 1/ cos lam0(j) *  [K_(m phi)^DT(0)(i+1,j)-K_(m phi)^DT(0)(i-1,j)]/ (2 d lon)
! rim(1) =  K_(m phi)^DT(0)
! copy rim(1) into tint33 and set up wrap around points
!
      do j=2,nmlath-1   !  2,48                            
        jj = j+nmlath-1 ! 49,96
        do i=1,nmlon
          tint33(i) = rim(i,jj,1)         ! tint33(1:nmlon)
        enddo ! i=1,nmlon
        do i=1,2                          ! wrap around points
          tint33(i-2) = tint33(i-2+nmlon) ! -1:0 <= nmlon-1:nmlon
          tint33(i+nmlon) = tint33(i)     ! nmlon+1:nmlon+2 <= 1,2
        enddo ! i=1,2
        do i=1,nmlon                                         
          rhs(i,j) = 1./(dlonm*tint1(nmlath+j-1))*
     |      .5*(tint33(i+1)-tint33(i-1))  ! tint33 2:nmlon+1 and 0:nmlon-1
        enddo ! i=1,nmlon
      enddo ! j=2,nmlath-1
!
! Perform differentiation (1/ cos lam0 ) * d [K_(m lam)^DT(0) cos lam_0]/ d lam_0:
! central differencing
! rhs(i,j) = rhs(i,j) + 1/ cos lam0(j) *  
!  [K_(m lam)^DT(0)(i,j+1)cos lam0(j+1)-K_(m lam)^DT(0)(i,j-1)cos lam0(j-1)]/ (2 d lat)
!
      do j=nmlath+1,nmlat-1 ! 50,96                          
        jj = j-nmlath+1     !  2,48
        do i=1,nmlon
          rhs(i,jj) = rhs(i,jj)+1./(dlatm*tint1(j))*
     |      .5*(rim(i,j+1,2)*tint1(j+1)-rim(i,j-1,2)*tint1(j-1))
        enddo ! i=1,nmlon
      enddo ! j=nmlath+1,nmlat-1
!
! polar value:
      rhs(1,nmlath) = -2./float(nmlon)*
     |  sddot(nmlon,unitvm,rim(1,nmlat-1,2))/tint1(nmlat-1)
!
! equatorial value:
! [K_(m phi)^DT(0)(i+1,j)-K_(m phi)^DT(0)(i-1,j)]/ (2 d lon) +
! [K_(m lam)^DT(0)(i,j+1/2)cos lam0(j+1/2)-0]/ ( d lat/2)
!   with   -  K_(m lam)^DT(0)(i,j_eq) = 0
!          -  cos lam0(j_eq) =1
!
      i = 1
      rhs(i,1) = 0.5/dlonm*(rim(i+1,nmlath,1)-rim(nmlon,nmlath,1))
      rhs(i,1) = rhs(i,1)+1./dlatm*(tint1(nmlath)*rim(i,nmlath,2)+
     |                            tint1(nmlath+1)*rim(i,nmlath+1,2))
      do i = 2,nmlon-1
        rhs(i,1) = 0.5/dlonm*(rim(i+1,nmlath,1)-rim(i-1,nmlath,1))
        rhs(i,1) = rhs(i,1)+1./dlatm*(tint1(nmlath)*rim(i,nmlath,2)+
     |                              tint1(nmlath+1)*rim(i,nmlath+1,2))
      enddo ! i = 2,nmlon-1
      i = nmlon
      rhs(i,1) = 0.5/dlonm*(rim(1,nmlath,1)-rim(i-1,nmlath,1))
      rhs(i,1) = rhs(i,1)+1./dlatm*(tint1(nmlath)*rim(i,nmlath,2)+
     |                            tint1(nmlath+1)*rim(i,nmlath+1,2))
!
! Extend over longitude:
      do i=2,nmlon
        rhs(i,nmlath) = rhs(1,nmlath)
      enddo ! i=2,nmlon
!
! Periodic points:
      do j=1,nmlath
        rhs(nmlonp1,j) = rhs(1,j)
      enddo ! j=1,nmlath
!
! multiply by earth radius + h_0 in meter  = R0*1.E-2 
!  R_0 * d lam_m/d lam_0 * 1/cos lam_0 [ d K_(m phi)^DT(0) / d phi  +
!  (d [ K_(m lam)^DT(0) * cos(lam_0)]/ d lam_0  ] 
!
      do j=1,nmlath
        do i=1,nmlonp1
          rhs(i,j) = rhs(i,j)*r00*1.e-2
        enddo ! i=1,nmlonp1
      enddo ! j=1,nmlath
!
! Save rhs to secondary histories (redundant in vertical and
!  zero in north hem):
!     real,dimension(nmlonp1,nmlath)  :: rhs ! right-hand side 
!     real,dimension(nmlonp1,-2:nlev) :: rhs_plt ! diag
!
!     rhs_plt = 0.
!     do j=1,nmlat
!       if (j <= nmlath) then
!         do i=1,nmlonp1
!           rhs_plt(i,:) = rhs(i,j)
!         enddo
!       endif
!       call addfsech_ik('RHS',' ',' ',rhs_plt,
!    |    1,nmlonp1,nmlev,nmlev-1,j)
!     enddo ! j=1,nmlat
!
      end subroutine rhspde
!-----------------------------------------------------------------------
! BOP
! !IROUTINE: clearce
! !INTERFACE:
      subroutine clearcee(cee,nlon0,nlat0)
!
! !DESCRIPTION:
! Zero C arrays for stencil coefficients.
! Cee will contain:
!   c0(nmlon0,nmlat0,10), c1(nmlon1,nmlat1,9), c2(nmlon2,nmlat2,9),
!   c3(nmlon3,nmlat3,9),  c4(nmlon4,nmlat4,9)
!
! !PARAMETERS: 
      integer,intent(in) :: nlon0,nlat0
! !RETURN VALUE:
      real,intent(out) :: cee(*)
!
! !REVISION HISTORY:
! 05.03.8  <Astrid Maute> <include header> 
! 
! EOP
!
! Local:
      integer :: nlon,nlat,n,m,i
!
! Compute total size of cee
      nlon = nlon0
      nlat = nlat0
      n = 0
      do m=1,5 ! 5 resolution levels
        n = n+nlon*nlat
        nlon = (nlon+1)/2
        nlat = (nlat+1)/2
      enddo ! m=1,5 ! 5 resolution levels
      n = 9*n+nlon0*nlat0
!
! Clear cee:
      do i=1,n
        cee(i) = 0.
      enddo
      end subroutine clearcee
!-----------------------------------------------------------------------
! BOP
! !IROUTINE: stencil
! !INTERFACE:
      subroutine stencil(zigm,nlon0,nlat0,cee,ncoef)
!
! !DESCRIPTION:
! Calculate contribution fo 3 by 3 stencil from coefficient zigm
! at each grid point and level.
!
! !PARAMETERS: 
      integer,intent(in) :: 
     |  nlon0, ! longitude dimension of finest grid level
     |  nlat0, ! latitude dimension of finest grid level
     |  ncoef   ! integer identifier of coefficient
      real,intent(in) :: 
     |  zigm(nlon0,nlat0)  ! coefficients (nlon0+1/2,(nlat0+1)/2) 
!     
! !RETURN VALUE:
      real,intent(inout) :: 
     |  cee(*)  ! output stencil array consisting of c0,c1,c2,c3,c4
!
! Local:
      integer :: nc,nlon,nlat,n
!
! Perform half-way interpolation and extend zigm in array:
!
      call htrpex(zigm,nlon0,nlat0)
!
! Calculate contribution to stencil for each grid point and level:
      nc = 1
      nlon = nlon0
      nlat = nlat0
      do n=1,5 ! 5 levels of resolution
        call cnm(nlon0,nlat0,nlon,nlat,cee(nc),ncoef)
        nc = nc+9*nlon*nlat
        if (n==1) nc = nc+nlon*nlat
        nlon = (nlon+1)/2
        nlat = (nlat+1)/2
      enddo ! n=1,5
      end subroutine stencil
!-----------------------------------------------------------------------
! BOP
! !IROUTINE: stencmd
! !INTERFACE:
      subroutine stencmd(zigm,nlon0,nlat0,cee,ncoef)
!
! !DESCRIPTION: 
! Calculate contribution fo 3 by 3 stencil from coefficient zigm
! at each grid point and level.
!
! !RETURN VALUE:
      integer,intent(in) :: 
     |  nlon0, ! longitude dimension of finest grid level
     |  nlat0, ! latitude dimension of finest grid level
     |  ncoef  ! integer identifier of coefficient
      real,intent(in) :: 
     |  zigm(nlon0,nlat0)  ! coefficients (nlon0+1/2,(nlat0+1)/2) 
!     
! !RETURN VALUE:
      real,intent(inout) :: 
     |  cee(*)  ! output stencil array consisting of c0,c1,c2,c3,c4
!       
! !REVISION HISTORY:
! 18.02.05  <Astrid Maute> <include header> 
! 
! EOP
! Local:
      integer :: nc,nlon,nlat,n
!
! Perform half-way interpolation and extend zigm in array:
!
      call htrpex(zigm,nlon0,nlat0)
!
! Calculate contribution to stencil for each grid point and level:
!
      nc = 1
      nlon = nlon0
      nlat = nlat0
!
! Calculate modified and unmodified stencil on finest grid
!
      call cnmmod(nlon0,nlat0,nlon,nlat,cee(nc),ncoef)
!
! Stencils on other grid levels remain the same.
      nc = nc+10*nlon*nlat 
      nlon = (nlon+1)/2
      nlat = (nlat+1)/2
!
      do n=2,5
        call cnm(nlon0,nlat0,nlon,nlat,cee(nc),ncoef)
        nc = nc+9*nlon*nlat
        if (n==1) nc = nc+nlon*nlat
        nlon = (nlon+1)/2
        nlat = (nlat+1)/2
      enddo ! n=1,5
      end subroutine stencmd
!-----------------------------------------------------------------------
! BOP
! !IROUTINE: htrpex
! !INTERFACE:
      subroutine htrpex(coeff,nmlon0,nmlat0)
!
! !DESCRIPTION: 
! copy coefficients into array and
! extend array over 16 grid points for the wrap around points
! on the 5 different grid levels. 
! Result returned in array(1:nmlath) - equator to pole.
!
! !ARGUMENTS:
      integer,intent(in) :: nmlon0,nmlat0        ! nmlon, nmlath
      real,intent(in) :: coeff(nmlon0,nmlat0)    ! Sigma(SH) pole to equator
!        
! !REVISION HISTORY:
! 18.02.05  <Astrid Maute> <include header> 
! 
! EOP
!
! Local:
      integer :: i,j,jj
!
! Copy coeff into positions in array:
! MODIFY: NO SYM. USE NH VALUES
!
      do j=1,nmlat0      ! 1:nmlath
        do i=1,nmlon0
          wkarray(i,j) = coeff(i,j)
        enddo ! i=1,nmlon0
      enddo ! j=1,nmlat0
!
! Extend over 32 grid spaces to allow for wrap around points for
! a total of 5 grid levels:
!
      do i=1,16
        do j=1,nmlat0
          wkarray(1-i,j)      = wkarray(nmlon0-i,j) 
          wkarray(nmlon0+i,j) = wkarray(1+i,j)
        enddo ! j=1,nmlat0
      enddo ! i=1,16
!
      end subroutine htrpex
!-----------------------------------------------------------------------
! BOP
! !IROUTINE: cnm
! !INTERFACE:
      subroutine cnm(nlon0,nlat0,nlon,nlat,c,ncoef)
!
! !DESCRIPTION: 
! Compute contribution to stencil from zigm(ncoef) on grid nlon by nlat,
! Finest grid is nlon0 by nlat0.
! uses upwinding method if diagonal dominance is not preserved
!
! ncoef: integer id of Sigma coefficient:
! ncoef = 1 for zigm11 Sigma_(phi phi)
! ncoef = 2 for zigmc  Sigma_(phi lam)
! ncoef = 3 for zigm2  Sigma_(lam phi)
! ncoef = 4 for zigm22 Sigma_(lam lam)
!
! !ARGUMENTS:
      integer,intent(in) :: 
     |  nlon0,nlat0, ! finest grid dimensions
     |  nlon,nlat    ! output grid dimensions
      integer,intent(in) :: ncoef

! !RETURN VALUE:
      real,intent(inout) :: 
     |  c(nlon,nlat,*)    ! output array for grid point stencils at
                          ! resolution nlon x nlat
!
! !REVISION HISTORY:
! 18.02.05  <Astrid Maute> <include header> 
! 
! EOP
! Local:
      integer :: i,j,nint,i0,j0
      real,parameter :: pi=3.141592654
      real :: wk(nlon0,3)
!
! Compute separation of grid points of resolution nlon x nlat within
! grid of resolution nlon0,nlat0. Evaluate dlon and dlat, grid spacing 
! of nlon x nlat.
!
      nint = (nlon0-1)/(nlon-1)
!
! Scan wkarray nlon x nlat calculating and adding contributions to stencil
! from zigm(ncoef)
      i0 = 1-nint
      j0 = 1-nint
!
! zigm11 Sigma_(phi phi)
      if (ncoef==1) then
        do j = 1,nlat-1
          do i = 1,nlon
            c(i,j,1) = c(i,j,1)+.5*(wkarray(i0+i*nint,j0+j*nint)+
     |        wkarray(i0+(i+1)*nint,j0+j*nint))
            c(i,j,5) = c(i,j,5)+.5*(wkarray(i0+i*nint,j0+j*nint)+
     |        wkarray(i0+(i-1)*nint,j0+j*nint))
            c(i,j,9) = c(i,j,9)-.5*(wkarray(i0+(i+1)*nint,j0+j*nint)+
     |        2.*wkarray(i0+i*nint,j0+j*nint)+
     |        wkarray(i0+(i-1)*nint,j0+j*nint))
          enddo ! i = 1,nlon
        enddo ! j = 2,nlat-1
!
! zigmc Sigma_(phi lam)
      elseif (ncoef==2) then
        do j = 2,nlat-1
          do i = 1,nlon
            c(i,j,2) = c(i,j,2)+.5*(wkarray(i0+i*nint,j0+j*nint)+
     |        wkarray(i0+(i+1)*nint,j0+j*nint))
            c(i,j,4) = c(i,j,4)-.5*(wkarray(i0+i*nint,j0+j*nint)+
     |        wkarray(i0+(i-1)*nint,j0+j*nint))
            c(i,j,6) = c(i,j,6)+.5*(wkarray(i0+i*nint,j0+j*nint)+
     |        wkarray(i0+(i-1)*nint,j0+j*nint))
            c(i,j,8) = c(i,j,8)-.5*(wkarray(i0+i*nint,j0+j*nint)+
     |        wkarray(i0+(i+1)*nint,j0+j*nint))
            wk(i,1) = .5*(wkarray(i0+(i+1)*nint,j0+j*nint)-
     |        wkarray(i0+(i-1)*nint,j0+j*nint))
            wk(i,2) = (c(i,j,3)+wk(i,1))*(c(i,j,7)-wk(i,1))
            wk(i,3) = sign(wk(i,1),c(i,j,3)+c(i,j,7))
            if (wk(i,2) >= 0.) wk(i,3) = 0.
            c(i,j,3) = c(i,j,3)+wk(i,1)+wk(i,3)
            c(i,j,7) = c(i,j,7)-wk(i,1)+wk(i,3)
            c(i,j,9) = c(i,j,9)-2.*wk(i,3)
          enddo ! i = 1,nlon
        enddo ! j = 2,nlat-1
!
! zigm2  Sigma_(lam phi)
      elseif (ncoef==3) then
        do j = 2,nlat-1
          do i = 1,nlon
            c(i,j,2) = c(i,j,2)+.5*(wkarray(i0+i*nint,j0+j*nint)+
     |        wkarray(i0+i*nint,j0+(j+1)*nint))
            c(i,j,4) = c(i,j,4)-.5*(wkarray(i0+i*nint,j0+j*nint)+
     |        wkarray(i0+i*nint,j0+(j+1)*nint))
            c(i,j,6) = c(i,j,6)+.5*(wkarray(i0+i*nint,j0+j*nint)+
     |        wkarray(i0+i*nint,j0+(j-1)*nint))
            c(i,j,8) = c(i,j,8)-.5*(wkarray(i0+i*nint,j0+j*nint)+
     |        wkarray(i0+i*nint,j0+(j-1)*nint))
            wk(i,1) = .5*(wkarray(i0+i*nint,j0+(j+1)*nint)-
     |        wkarray(i0+i*nint,j0+(j-1)*nint))
            wk(i,2) = (c(i,j,1)+wk(i,1))*(c(i,j,5)-wk(i,1))
            wk(i,3) = sign(wk(i,1),c(i,j,1)+c(i,j,5))
            if (wk(i,2) >= 0.) wk(i,3) = 0.
            c(i,j,1) = c(i,j,1)+wk(i,1)+wk(i,3)
            c(i,j,5) = c(i,j,5)-wk(i,1)+wk(i,3)
            c(i,j,9) = c(i,j,9)-2.*wk(i,3)
          enddo ! i = 1,nlon
        enddo ! j = 2,nlat-1
!
! Low latitude boundary condition:
        j = 1
        do i=1,nlon
          c(i,j,2) = c(i,j,2)+.5*(wkarray(i0+i*nint,j0+j*nint)+
     |      wkarray(i0+i*nint,j0+(j+1)*nint))
          c(i,j,4) = c(i,j,4)-.5*(wkarray(i0+i*nint,j0+j*nint)+
     |      wkarray(i0+i*nint,j0+(j+1)*nint))
          wk(i,1) = .5*(wkarray(i0+i*nint,j0+j*nint)+
     |      wkarray(i0+i*nint,j0+(j+1)*nint))
          wk(i,2) = (c(i,j,1)+wk(i,1))*(c(i,j,5)-wk(i,1))
          wk(i,3) = sign(wk(i,1),c(i,j,1)+c(i,j,5))
          if (wk(i,2) >= 0.) wk(i,3) = 0.
          c(i,j,1) = c(i,j,1)+wk(i,1)+wk(i,3)
          c(i,j,5) = c(i,j,5)-wk(i,1)+wk(i,3)
          c(i,j,9) = c(i,j,9)-2.*wk(i,3)
        enddo ! i=1,nlon
!
! zigm22 Sigma_(lam lam)
      elseif (ncoef==4) then
        do j = 2,nlat-1
          do i = 1,nlon
            c(i,j,3) = c(i,j,3)+.5*(wkarray(i0+i*nint,j0+j*nint)
     |        +wkarray(i0+i*nint,j0+(j+1)*nint))
            c(i,j,7) = c(i,j,7)+.5*(wkarray(i0+i*nint,j0+j*nint)
     |        +wkarray(i0+i*nint,j0+(j-1)*nint))
            c(i,j,9) = c(i,j,9)-.5*(wkarray(i0+i*nint,j0+(j-1)*nint)
     |        +2.*wkarray(i0+i*nint,j0+j*nint)
     |        +wkarray(i0+i*nint,j0+(j+1)*nint))
          enddo ! i = 1,nlon
        enddo ! j = 2,nlat-1
!
! Low latitude boundary condition:
        j = 1
        do i=1,nlon
          c(i,j,3) = c(i,j,3)+.5*(wkarray(i0+i*nint,j0+j*nint)
     |      +wkarray(i0+i*nint,j0+(j+1)*nint))
          c(i,j,9) = c(i,j,9)-.5*(wkarray(i0+i*nint,j0+j*nint)+
     |                            wkarray(i0+i*nint,j0+(j+1)*nint))
        enddo ! i=1,nlon
      endif ! ncoef
!      
      end subroutine cnm
!-----------------------------------------------------------------------
! BOP
! !IROUTINE: cnmmod
! !INTERFACE:
      subroutine cnmmod(nlon0,nlat0,nlon,nlat,c,ncoef)
!
! !DESCRIPTION: 
! Compute contribution to stencil from zigm(ncoef) on finest grid nlon0 by nlat0.
! calculates the stencil with (c) and without (cofum)
! the use of upwinding method if diagonal dominance is not preserved
! output: c     ( upwinding)
!         cofum (no upwinding)
!
! ncoef: integer id of Sigma coefficient:
! ncoef = 1 for zigm11 Sigma_(phi phi)
! ncoef = 2 for zigmc  Sigma_(phi lam)
! ncoef = 3 for zigm2  Sigma_(lam phi)
! ncoef = 4 for zigm22 Sigma_(lam lam)
!
! !ARGUMENTS:
      integer,intent(in) :: 
     |  nlon0,nlat0, ! finest grid dimensions
     |  nlon,nlat    ! output grid dimensions
      integer,intent(in) :: ncoef
! !RETURN VALUE:
      real,intent(inout) :: 
     |  c(nlon,nlat,*)    ! output array for grid point stencils at
                          ! resolution nlon x nlat
!        
! !REVISION HISTORY:
! 18.02.05  <Astrid Maute> <include header> 
! 
! EOP
!
! Local:
      integer :: i,j,nint,i0,j0
      real,parameter :: pi=3.141592654
      real :: wk(nlon0,3)
!
! Compute separation of grid points of resolution nlon x nlat within
! grid of resolution nlon0,nlat0. Evaluate dlon and dlat, grid spacing 
! of nlon x nlat.
!
      nint = (nlon0-1)/(nlon-1)
!
! Scan wkarray nlon x nlat calculating and adding contributions to stencil
! from zigm(ncoef)
      i0 = 1-nint
      j0 = 1-nint
!
! zigm11: Sigma_(phi phi)
      if (ncoef==1) then
        do j = 1,nlat-1
          do i = 1,nlon
            c(i,j,1) = c(i,j,1)+.5*(wkarray(i0+i*nint,j0+j*nint)+
     |        wkarray(i0+(i+1)*nint,j0+j*nint))
            c(i,j,5) = c(i,j,5)+.5*(wkarray(i0+i*nint,j0+j*nint)+
     |        wkarray(i0+(i-1)*nint,j0+j*nint))
            c(i,j,9) = c(i,j,9)-.5*(wkarray(i0+(i+1)*nint,j0+j*nint)+
     |        2.*wkarray(i0+i*nint,j0+j*nint)+
     |        wkarray(i0+(i-1)*nint,j0+j*nint))
!
! Unmodified:
            cofum(i,j,1)=cofum(i,j,1)+.5*(wkarray(i0+i*nint,j0+j*nint)+
     |        wkarray(i0+(i+1)*nint,j0+j*nint))
            cofum(i,j,5)=cofum(i,j,5)+.5*(wkarray(i0+i*nint,j0+j*nint)+
     |        wkarray(i0+(i-1)*nint,j0+j*nint))
            cofum(i,j,9) = cofum(i,j,9)-
     |        .5*(wkarray(i0+(i+1)*nint,j0+j*nint)+
     |         2.*wkarray(i0+i*nint,j0+j*nint)+
     |        wkarray(i0+(i-1)*nint,j0+j*nint))
          enddo ! i = 1,nlon
        enddo ! j = 2,nlat-1
!
! zigmc Sigma_(phi lam)
      elseif (ncoef==2) then
        do j = 2,nlat-1
          do i = 1,nlon
            c(i,j,2) = c(i,j,2)+.5*(wkarray(i0+i*nint,j0+j*nint)+
     |        wkarray(i0+(i+1)*nint,j0+j*nint))
            c(i,j,4) = c(i,j,4)-.5*(wkarray(i0+i*nint,j0+j*nint)+
     |        wkarray(i0+(i-1)*nint,j0+j*nint))
            c(i,j,6) = c(i,j,6)+.5*(wkarray(i0+i*nint,j0+j*nint)+
     |        wkarray(i0+(i-1)*nint,j0+j*nint))
            c(i,j,8) = c(i,j,8)-.5*(wkarray(i0+i*nint,j0+j*nint)+
     |        wkarray(i0+(i+1)*nint,j0+j*nint))
            wk(i,1) = .5*(wkarray(i0+(i+1)*nint,j0+j*nint)-
     |        wkarray(i0+(i-1)*nint,j0+j*nint))
!
! Unmodified:
            cofum(i,j,2) = c(i,j,2)
            cofum(i,j,4) = c(i,j,4)
            cofum(i,j,6) = c(i,j,6)
            cofum(i,j,8) = c(i,j,8)
            cofum(i,j,3) = cofum(i,j,3)+wk(i,1)
            cofum(i,j,7) = cofum(i,j,7)-wk(i,1)
! test for upwinding
            wk(i,2) = (c(i,j,3)+wk(i,1))*(c(i,j,7)-wk(i,1))
            wk(i,3) = sign(wk(i,1),c(i,j,3)+c(i,j,7))
            if (wk(i,2) >= 0.) wk(i,3) = 0.
            c(i,j,3) = c(i,j,3)+wk(i,1)+wk(i,3)
            c(i,j,7) = c(i,j,7)-wk(i,1)+wk(i,3)
            c(i,j,9) = c(i,j,9)-2.*wk(i,3)
          enddo ! i = 1,nlon
        enddo ! j = 2,nlat-1
!
! zigm2 Sigma_(lam phi)
      elseif (ncoef==3) then
        do j = 2,nlat-1
          do i = 1,nlon
            c(i,j,2) = c(i,j,2)+.5*(wkarray(i0+i*nint,j0+j*nint)+
     |        wkarray(i0+i*nint,j0+(j+1)*nint))
            c(i,j,4) = c(i,j,4)-.5*(wkarray(i0+i*nint,j0+j*nint)+
     |        wkarray(i0+i*nint,j0+(j+1)*nint))
            c(i,j,6) = c(i,j,6)+.5*(wkarray(i0+i*nint,j0+j*nint)+
     |        wkarray(i0+i*nint,j0+(j-1)*nint))
            c(i,j,8) = c(i,j,8)-.5*(wkarray(i0+i*nint,j0+j*nint)+
     |        wkarray(i0+i*nint,j0+(j-1)*nint))
            wk(i,1) = .5*(wkarray(i0+i*nint,j0+(j+1)*nint)-
     |        wkarray(i0+i*nint,j0+(j-1)*nint))
!
! Unmodified:
            cofum(i,j,2) = c(i,j,2)
            cofum(i,j,4) = c(i,j,4)
            cofum(i,j,6) = c(i,j,6)
            cofum(i,j,8) = c(i,j,8)
            cofum(i,j,1) = cofum(i,j,1)+wk(i,1)
            cofum(i,j,5) = cofum(i,j,5)-wk(i,1)
! test for upwinding
            wk(i,2) = (c(i,j,1)+wk(i,1))*(c(i,j,5)-wk(i,1))
            wk(i,3) = sign(wk(i,1),c(i,j,1)+c(i,j,5))
            if (wk(i,2) >= 0.) wk(i,3) = 0.
            c(i,j,1) = c(i,j,1)+wk(i,1)+wk(i,3)
            c(i,j,5) = c(i,j,5)-wk(i,1)+wk(i,3)
            c(i,j,9) = c(i,j,9)-2.*wk(i,3)
          enddo ! i = 1,nlon
        enddo ! j = 2,nlat-1
!
! Low latitude boundary condition:
        j = 1
        do i=1,nlon
          c(i,j,2) = c(i,j,2)+.5*(wkarray(i0+i*nint,j0+j*nint)+
     |      wkarray(i0+i*nint,j0+(j+1)*nint))
          c(i,j,4) = c(i,j,4)-.5*(wkarray(i0+i*nint,j0+j*nint)+
     |      wkarray(i0+i*nint,j0+(j+1)*nint))
          wk(i,1) = .5*(wkarray(i0+i*nint,j0+j*nint)+
     |      wkarray(i0+i*nint,j0+(j+1)*nint))

          cofum(i,j,2) = c(i,j,2)
          cofum(i,j,4) = c(i,j,4)
          cofum(i,j,1) = cofum(i,j,1)+wk(i,1)
          cofum(i,j,5) = cofum(i,j,5)-wk(i,1)

          wk(i,2) = (c(i,j,1)+wk(i,1))*(c(i,j,5)-wk(i,1))
          wk(i,3) = sign(wk(i,1),c(i,j,1)+c(i,j,5))
          if (wk(i,2) >= 0.) wk(i,3) = 0.
          c(i,j,1) = c(i,j,1)+wk(i,1)+wk(i,3)
          c(i,j,5) = c(i,j,5)-wk(i,1)+wk(i,3)
          c(i,j,9) = c(i,j,9)-2.*wk(i,3)
        enddo ! i=1,nlon
!
! zigm22: Sigma_(lam lam)
      elseif (ncoef==4) then
        do j = 2,nlat-1
          do i = 1,nlon
            c(i,j,3) = c(i,j,3)+.5*(wkarray(i0+i*nint,j0+j*nint)
     |        +wkarray(i0+i*nint,j0+(j+1)*nint))
            c(i,j,7) = c(i,j,7)+.5*(wkarray(i0+i*nint,j0+j*nint)
     |        +wkarray(i0+i*nint,j0+(j-1)*nint))
            c(i,j,9) = c(i,j,9)-.5*(wkarray(i0+i*nint,j0+(j-1)*nint)
     |        +2.*wkarray(i0+i*nint,j0+j*nint)
     |        +wkarray(i0+i*nint,j0+(j+1)*nint))
!
! Unmodified:
            cofum(i,j,3) = cofum(i,j,3)+.5*(wkarray(i0+i*nint,j0+j*nint)
     |        +wkarray(i0+i*nint,j0+(j+1)*nint))
            cofum(i,j,7) = cofum(i,j,7)+.5*(wkarray(i0+i*nint,j0+j*nint)
     |        +wkarray(i0+i*nint,j0+(j-1)*nint))
            cofum(i,j,9) = cofum(i,j,9)-.5*(wkarray(i0+i*nint,j0+(j-1)*
     |        nint)+2.*wkarray(i0+i*nint,j0+j*nint)+
     |        wkarray(i0+i*nint,j0+(j+1)*nint))

          enddo ! i = 1,nlon
        enddo ! j = 2,nlat-1
!
! Low latitude boundary condition:
        j = 1
        do i=1,nlon
          c(i,j,3) = c(i,j,3)+.5*(wkarray(i0+i*nint,j0+j*nint)
     |      +wkarray(i0+i*nint,j0+(j+1)*nint))
          c(i,j,9) = c(i,j,9)-.5*(wkarray(i0+i*nint,j0+j*nint)+
     |                            wkarray(i0+i*nint,j0+(j+1)*nint))
          cofum(i,j,3) = cofum(i,j,3)+.5*(wkarray(i0+i*nint,j0+j*nint)+
     |                                 wkarray(i0+i*nint,j0+(j+1)*nint))
          cofum(i,j,9) = cofum(i,j,9)-.5*(wkarray(i0+i*nint,j0+j*nint)+
     |                                 wkarray(i0+i*nint,j0+(j+1)*nint))

        enddo ! i=1,nlon
      endif ! ncoef
!
      end subroutine cnmmod
!-----------------------------------------------------------------------
! BOP
! !IROUTINE: divide
! !INTERFACE:
      subroutine divide(c,nlon,nlat,nlon0,nlat0,cs,igrid)
!
! !USES:
! !DESCRIPTION: 
! Divide stencil C by cos(theta(i,j))
!
! !ARGUMENTS:
      integer,intent(in) :: nlon,nlat,   ! dim. of finest grid
     |    nlon0,nlat0,                   ! dim. of actual grid
     |    igrid                          ! flag for divide by cos lam_0
      real,intent(in) :: cs(*)           ! cos lam_0
! !RETURN VALUE:
      real,intent(out) :: c(nlon,nlat,*) ! coefficient array for grid level
!        
! !REVISION HISTORY:
! 21.02.05  <Astrid Maute> <include header> 
!                          <remove 1/cos of RHS at the equator> 
! 
! EOP
!
! Local:
      integer :: nint,j0,n,j,i
!
      nint = (nlon0-1)/(nlon-1)
      j0 = 1-nint
      do n = 1,9
        do j = 1,nlat-1
          do i = 1,nlon
            c(i,j,n) = c(i,j,n)/(cs(j0+j*nint)*nint**2)
          enddo ! i = 1,nlon
        enddo ! j = 1,nlat-1
      enddo ! n = 1,9
!
      end subroutine divide
!-----------------------------------------------------------------------
! BOP
! !IROUTINE: edges
! !INTERFACE:
      subroutine edges(c,nlon,nlat)
!
! !USES:
! !DESCRIPTION: 
! Insert polar boundary conditions in stencil c(nlon,nlat,9)
!    c(:,nlat,1:8) = 0 & diagonal term c(:,nlat,9) = 1.
!
! !ARGUMENTS:
      integer,intent(in) :: nlon,nlat    ! dimension of coeff. array
! !RETURN VALUE:
      real,intent(out) :: c(nlon,nlat,*) ! coefficient array
!        
! !REVISION HISTORY:
! 21.02.05  <Astrid Maute> <include header> 
! 
! EOP
!
! Local:
      integer :: n,i
!
      do n=1,8
        do i=1,nlon
          c(i,nlat,n) = 0.
        enddo
      enddo
      do i=1,nlon
        c(i,nlat,9) = 1.
      enddo
      end subroutine edges
!-----------------------------------------------------------------------
! BOP
! !IROUTINE: stenmod
! !INTERFACE:
      subroutine stenmod(inlon,inlat,c,phihm,pfrac)
!
! !USES:
      use cons_module,only: dlatm,dtr
!
! !DESCRIPTION:
! Modify stencil c and cofum to set potential to heelis value within auroral circle.
!    c     is the stencil w   upwinding
! pfrac set up in heelis module: fractional presence of dynamo equation
!        pfrac = 1  for |colam_m| < crit(2)
!        pfrac = 0  for |colam_m| > crit(1)
!        pfrac = (colat_m-crit(1))/(crit(2)-crit(1))  
!                                for crit(2) < |lam_m| < crit(1)
! to ensure diagonal dominance only the diogonal element c(9) is used
! factor of (dlatm/(10.*dtr))**2 to eliminate dependence on grid spacing
! for c(9)
!
! !PARAMETERS: 
      integer,intent(in) :: inlon,inlat
      real,dimension(nmlon0,nmlat0),intent(in) :: 
     |  phihm,  ! heelis potential (from subs potm, flwv32)
     |  pfrac   ! fractional presence of dynamo (from sub colath)
! !RETURN VALUE:
      real,intent(inout) :: c(inlon,inlat,*)
! !REVISION HISTORY:
! 05.03.10  <Astrid Maute> <include header> 
! 
! EOP
!
! Local:
      integer :: nint,i0,j0,i,j,n,jj
!
! Compute separation of grid points for this resolution:
      nint = (nmlon0-1)/(inlon-1)
      i0 = 1-nint
      j0 = 1-nint
!
! If nint==1, then we are at the highest resolution.
! Correct RHS, which is in c(10)
!
      if (nint==1) then
        do j=1,inlat
          do i=1,inlon
            c(i,j,10) = pfrac(i,j)*c(i,j,10)+(1.-pfrac(i,j))*c(i,j,9)*
     |        (dlatm/(10.*dtr))**2*phihm(i,j)
          enddo ! i=1,inlon
        enddo ! j=1,inlat
      endif
!
! Modify stencil, c(i,j,n),n=1,9:
!
      do j=1,inlat
        jj = j0+j*nint
        do n = 1,8
          do i = 1,inlon
            c(i,j,n) = c(i,j,n)*pfrac(i0+i*nint,jj)
          enddo ! i = 1,inlon
        enddo ! n = 1,8
        do i = 1,inlon
          c(i,j,9) = c(i,j,9)*pfrac(i0+i*nint,jj)+
     |      (1.-pfrac(i0+i*nint,jj))*c(i,j,9)*
     |      (dlatm*float(nint)/(10.*dtr))**2
        enddo ! i = 1,inlon
      enddo ! j=1,inlat
!
      end subroutine stenmod
!-----------------------------------------------------------------------
! BOP
! !IROUTINE: stenmd
! !INTERFACE:
      subroutine stenmd(inlon,inlat,c,phihm,pfrac)
! !USES:
      use cons_module,only: dlatm,dtr
!
! !DESCRIPTION:
! Modify stencil c and cofum to set potential to heelis value within auroral circle.
!    c     is the stencil w   upwinding
!    cofum is the stencil w/o upwinding
! pfrac set up in heelis module: fractional presence of dynamo equation
!        pfrac = 1  for |colam_m| < crit(2)
!        pfrac = 0  for |colam_m| > crit(1)
!        pfrac = (colat_m-crit(1))/(crit(2)-crit(1))  
!                                for crit(2) < |lam_m| < crit(1)
! to ensure diagonal dominance only the diogonal element c(9) is used
! factor of (dlatm/(10.*dtr))**2 to eliminate dependence on grid spacing
! for c(9)
!
! !PARAMETERS: 
      integer,intent(in) :: inlon,inlat
      real,dimension(nmlon0,nmlat0),intent(in) :: 
     |  phihm,  ! heelis potential (from subs potm, flwv32)
     |  pfrac   ! fractional presence of dynamo (from sub colath)
! !RETURN VALUE:
      real,intent(inout) :: c(inlon,inlat,*)
! !REVISION HISTORY:
! 05.03.8  <Astrid Maute> <include header> 
! 
! EOP
!
! Local:
      integer :: nint,i0,j0,i,j,n,jj
!
! Compute separation of grid points for this resolution:
      nint = (nmlon0-1)/(inlon-1)
      i0 = 1-nint
      j0 = 1-nint
!
! If nint==1, then we are at the highest resolution.
! Correct RHS, which is in c(10)
!
      if (nint==1) then
        do j=1,inlat
          do i=1,inlon
            c(i,j,10) = pfrac(i,j)*c(i,j,10)+(1.-pfrac(i,j))*c(i,j,9)*
     |        (dlatm/(10.*dtr))**2*phihm(i,j)
          enddo ! i=1,inlon
        enddo ! j=1,inlat
      endif
!
! Modify stencil, c(i,j,n),n=1,9:
!
      if (nint==1) then
        do j=1,inlat
          jj = j0+j*nint
          do n = 1,8
            do i = 1,inlon
              c(i,j,n) = c(i,j,n)*pfrac(i0+i*nint,jj)
              cofum(i,j,n) = cofum(i,j,n)*pfrac(i0+i*nint,jj)
            enddo ! i = 1,inlon
          enddo ! n = 1,8
          do i = 1,inlon
            c(i,j,9) = c(i,j,9)*pfrac(i0+i*nint,jj)+
     |        (1.-pfrac(i0+i*nint,jj))*c(i,j,9)*
     |        (dlatm*float(nint)/(10.*dtr))**2
            cofum(i,j,9) =cofum(i,j,9)*pfrac(i0+i*nint,jj)+
     |        (1.-pfrac(i0+i*nint,jj))*cofum(i,j,9)*
     |        (dlatm*float(nint)/(10.*dtr))**2
          enddo ! i = 1,inlon
        enddo ! j=1,inlat
      else ! nint /= 1
        do j=1,inlat
          jj = j0+j*nint
          do n = 1,8
            do i = 1,inlon
              c(i,j,n) = c(i,j,n)*pfrac(i0+i*nint,jj)
            enddo ! i = 1,inlon
          enddo ! n = 1,8
          do i = 1,inlon
            c(i,j,9) = c(i,j,9)*pfrac(i0+i*nint,jj)+
     |        (1.-pfrac(i0+i*nint,jj))*c(i,j,9)*
     |        (dlatm*float(nint)/(10.*dtr))**2
          enddo ! i = 1,inlon
        enddo ! j=1,inlat
      endif ! nint
!      
      end subroutine stenmd
!-----------------------------------------------------------------------
! BOP
! !IROUTINE: ceee
! !INTERFACE:
      subroutine ceee(cee,nx,ny,cf)
! !USES:
!
! !DESCRIPTION:
! Called from mudpack solvers to transfer coefficients.
!
      save
! !PARAMETERS: 
      integer,intent(in) :: nx,ny
      real,intent(in) :: cee(nx,ny,9)
! !RETURN VALUE:
      real,intent(out) :: cf(nx,ny,9)
!
! !REVISION HISTORY:
! 05.02.16  <Astrid Maute> <include header> 
! 
! EOP
!
      integer :: i,j,n
      do n = 1,9
        do j = 1,ny
          do i = 1,nx
            cf(i,j,n) = cee(i,j,n)
          enddo
        enddo
      enddo
      end subroutine ceee
!-----------------------------------------------------------------------
! BOP
! !IROUTINE: threed
! !INTERFACE:
      subroutine threed
!
! !USES:
      use cons_module,only: dlatm,dlonm,rcos0s,r00,pi_dyn,dt1dts
!
! !DESCRIPTION: 
! calculates electric field 
!      E_d1 = -1/(R cos lam_m) d Phi/ d phi_m
!      E_d2 = 1/ (R |sinI_m|)  d Phi/ d lam_m
! calculate 3D electric potential and electric field assuming
! that the fieldlines are dipolar and with constant electric potential
! along the field line
!
! !REVISION HISTORY:
! 07.03.05  <Astrid Maute> <include header> 
! 
! EOP
!
! Local:
      integer :: i,j,ip1f,ip2f,ip3f
!nm101306: 
      character :: fname*10,labl*56,units*12

!
! Externals:
      real,external :: sddot ! in util.F
!
      pi = pi_dyn
!
! Calculate E_d1, E_d2 components of electric field:
! E_d1 = -1/(R cos lam_0)* (cos lam_0/cos lam_m) d Phi/ d phi_m
      write(6,*) 'NMLONP1 NMLAT ', nmlonp1, nmlat
      do j=2,nmlat-1
        csth0 = cos(-pi/2.+(j-1)*dlatm)
        do i=2,nmlon
          ed1(i,j) = -(phim(i+1,j)-phim(i-1,j))/(2.*dlonm*csth0)*
     |      rcos0s(j)/(r00*1.e-2)
        enddo ! i=2,nmlon
        ed1(1,j) = -(phim(2,j)-phim(nmlon,j))/(2.*dlonm*csth0)*
     |      rcos0s(j)/(r00*1.e-2)
        ed1(nmlonp1,j) = ed1(1,j)
      enddo ! j=2,nmlat-1
!
! E_d2 = +/- 1/(R |sinI_m|) d Phi/d lam_0* (d lam_0/ (d lam_m*|sin I_m|))
! Southern hemisphere:
      do j=2,nmlath-1
        do i=1,nmlonp1
          ed2(i,j) = -(phim(i,j+1)-phim(i,j-1))/(2.*dlatm)*dt1dts(j)/
     |      (r00*1.e-2)
        enddo ! i=1,nmlonp1
      enddo ! j=2,nmlath-1
!
! Northern hemisphere:
      do j=nmlath+1,nmlat-1
        do i=1,nmlonp1
          ed2(i,j) = (phim(i,j+1)-phim(i,j-1))/(2.*dlatm)*dt1dts(j)/
     |      (r00*1.e-2)
        enddo ! i=1,nmlonp1
      enddo ! j=nmlath+1,nmlat-1
!
! Poles:
      do i = 1,nmlonp1
        ip1f = i + nmlon/4
        if (ip1f > nmlonp1) ip1f = ip1f - nmlon
        ip2f = i + nmlon/2
        if (ip2f > nmlonp1) ip2f = ip2f - nmlon
        ip3f = i + 3*nmlon/4
        if (ip3f > nmlonp1) ip3f = ip3f - nmlon
        ed1(i,1) = .25*(ed1(i,2) - ed1(ip2f,2) +
     |                  ed2(ip1f,2) - ed2(ip3f,2))
        ed1(i,nmlat) = .25*(ed1(i,nmlat-1) - ed1(ip2f,nmlat-1) +
     |                      ed2(ip1f,nmlat-1) - ed2(ip3f,nmlat-1))
        ed2(i,1) = .25*(ed2(i,2) - ed2(ip2f,2) -
     |                  ed1(ip1f,2) + ed1(ip3f,2))
        ed2(i,nmlat) = .25*(ed2(i,nmlat-1) - ed2(ip2f,nmlat-1) -
     |                      ed1(ip1f,nmlat-1) + ed1(ip3f,nmlat-1))
!
! Equator
       ed2(i,nmlath) = (4.*phim(i,nmlath+1)-phim(i,nmlath+2)
     |    -3.*phim(i,nmlath))/(2.*dlatm)/(R00*1.e-2)
       ed2(i,nmlath+1) = (4.*phim(i,nmlath+2)-phim(i,nmlath+3)
     |    -3.*phim(i,nmlath+1))/(2.*dlatm)/(R00*1.e-2)
       ed2(i,nmlath-1) = (4.*phim(i,nmlath-2)-phim(i,nmlath-3)
     |    -3.*phim(i,nmlath-1))/(2.*dlatm)/(R00*1.e-2)
      enddo ! i = 1,nmlonp1
!
!
      end subroutine threed
!-----------------------------------------------------------------------
      end module dynamo_module
!-----------------------------------------------------------------------
