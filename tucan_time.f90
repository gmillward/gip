!************************************************************************
!                                                                       *
!  *******************************************************************  *
!  *                                                                 *  *
!  *                                                                 *  *
!  *                   PROGRAM TUCAN_TIME                            *  *
!  *                                                                 *  *
!  *                                                                 *  *
!  *******************************************************************  *
!                                                                       *
!************************************************************************
 
      PROGRAM TUCAN_TIME
 
      use IONOSPHERE_PLASMASPHERE
      use THERMOSPHERE
      use RUN_PARAMETERS
  
      IMPLICIT NONE

      CHARACTER*3 switch
      REAL*8 dcol , dlon 
      REAL*8 Foster_potential(23,20,7) 
      INTEGER Foster_level
      REAL*8 Foster_power
      REAL*8 Foster_Efield_amplification
      REAL*8 emaps(21,20,7) , cmaps(21,20,7)
      REAL*8 profile(15,21)

      REAL*8 B_magnitude_apex_nT(91,20)
      REAL*8 B_dip_angle_apex_degrees(91,20)
      REAL*8 B_declination_apex_degrees(91,20)
      REAL*8 Magnetic_latitude_degrees(91,20)
      REAL*8 Magnetic_longitude_degrees(91,20)

      REAL*8 plvu(49) , zonal(49)
      REAL*8 solar_declination_angle_radians
      REAL*8 Universal_Time_hours
      REAL*8 Universal_Time_seconds
      REAL*8  Start_time_UT_hours,Stop_time_UT_hours
      INTEGER i , j , l , m , n , i_no_day
      INTEGER nn , nnloop , &
              nn_composition_counter , nn_smoothing_counter
      INTEGER number_of_GT_time_steps_in_24_hours
      INTEGER(kind=8) :: t0_loop , t0_gt , t0_gip , t1 , clock_rate

      REAL*8    Ne_density_FOR_GT(15,91,20) 
      REAL*8    Oplus_density_FOR_GT(15,91,20)
      REAL*8    Hplus_density_FOR_GT(15,91,20)
      REAL*8    NOplus_density_FOR_GT(15,91,20)
      REAL*8    O2plus_density_FOR_GT(15,91,20)
      REAL*8    Te_FOR_GT(15,91,20)
      REAL*8    Ti_Oplus_FOR_GT(15,91,20)
      REAL*8    Ti_Hplus_FOR_GT(15,91,20)

      REAL*8    elx(91,20) , ely(91,20)
      REAL*8    exns(2,45,20) , eyns(2,45,20) , ezns(2,45,20)
      REAL*8    qion3d(15,91,20)

      INTEGER idump_gip , idump_gt
      INTEGER i_gip
      INTEGER GT_ht_dim
      INTEGER GT_lat_dim
      INTEGER GT_lon_dim

      CHARACTER*10 thermospheric_model_name
      logical GIP_switches(20)
      character*100 Plot_data_file

      REAL(kind=8) wind_southwards_ms1_FROM_GT(15,91,20)
      REAL(kind=8) wind_eastwards_ms1_FROM_GT(15,91,20)
      REAL(kind=8) wvz_FROM_GT(15,91,20)
      REAL(kind=8) rmt_FROM_GT(15,91,20)
      REAL(kind=8) Temperature_K_FROM_GT(15,91,20)
      REAL(kind=8) ht_FROM_GT(15,91,20)
      REAL(kind=8) O_density_FROM_GT(15,91,20)
      REAL(kind=8) O2_density_FROM_GT(15,91,20)
      REAL(kind=8) N2_density_FROM_GT(15,91,20)

      real*8 O_density_m3_FOR_GIP(15,20,91), &
          O2_density_m3_FOR_GIP(15,20,91), &
          N2_density_m3_FOR_GIP(15,20,91), &
          therm_model_HYD_density_m3(15,20,91), &
          therm_model_HEL_density_m3(15,20,91), &
          therm_model_NO_density_m3(15,20,91), &
          therm_model_N4S_density_m3(15,20,91), &
          therm_model_N2D_density_m3(15,20,91), &
          Tn_K_FOR_GIP(15,20,91), &
          Vn_Southwards_ms1_FOR_GIP(15,20,91), &
          Vn_Eastwards_ms1_FOR_GIP(15,20,91), &
          Vn_Upwards_ms1_FOR_GIP(15,20,91), &
          Altitude_m_FOR_GIP(15,20,91), &
          qion3d_FOR_GIP(15,20,91), &
          therm_model_qo2p_aurora(15,20,91), &
          therm_model_qop_aurora(15,20,91), &
          therm_model_qn2p_aurora(15,20,91), &
          therm_model_qnp_aurora(15,20,91), &
          therm_model_qtef_aurora(15,20,91), &
          elx_FOR_GIP(15,20,91), &
          ely_FOR_GIP(15,20,91), &
! dummy inputs from tiegcm testing.....
      tiegcm_Op(15,20,91) , tiegcm_NOp(15,20,91) , tiegcm_O2p(15,20,91)
      real*8 therm_model_geo_long_deg(20)
      real*8 therm_model_geo_lat_deg(91)

      REAL*8 potential_field(81,97)
      REAL*8 ed1(81,97)
      REAL*8 ed2(81,97)

      real*8 Ne_density_FROM_GIP_m3(15,20,91), &
          Oplus_density_FROM_GIP_m3(15,20,91), & 
          Hplus_density_FROM_GIP_m3(15,20,91), &
          NOplus_density_FROM_GIP_m3(15,20,91), &
          O2plus_density_FROM_GIP_m3(15,20,91), &
          N2plus_density_FROM_GIP_m3(15,20,91), &
          Nplus_density_FROM_GIP_m3(15,20,91), &
          Te_FROM_GIP_K(15,20,91), &
          Ti_Oplus_FROM_GIP_K(15,20,91), &
          Ti_Hplus_FROM_GIP_K(15,20,91)

      real*8 NmF2(91,90)
      real*8 HmF2_km(91,90)
      real*8 TEC(91,90)
      real*8 this_Ne_profile(183)
      real*8 Te_400(91,90)
      real*8 Ti_400(91,90)
      REAL*8 Upar_apex_at_F2(91,90)
      REAL*8 O_N2_ratio_F2(91,90)
      REAL*8 Vx_at_F2(91,90)
      REAL*8 Vy_at_F2(91,90)
      REAL*8 O_density_at_F2(91,90)
      REAL*8 N2_density_at_F2(91,90)
      REAL*8 upar_apex_300km(91,90)
      REAL*8 Vx_300km(91,90)
      REAL*8 Vy_300km(91,90)
      REAL*8 O_density_300km(91,90)
      REAL*8 N2_density_300km(91,90)
      REAL*8 ne_high_res_fixed(31,91,90)

      REAL*8 dynamo_sigma_phph_dsi(81,97), &
              dynamo_sigma_lmlm_msi(81,97), &
              dynamo_sigma_h(81,97),dynamo_sigma_c(81,97), &
              dynamo_Kdmph_dsi(81,97),dynamo_Kdmlm(81,97)

      REAL*8 hough22(181) , hough23(181) , hough24(181)
      REAL*8 hough11(181) , hough25(181)


      parameter(thermospheric_model_name = 'CMAT2')
      parameter(GT_ht_dim=15)
      parameter(GT_lat_dim=91)
      parameter(GT_lon_dim=20)

      call read_in_run_parameters_from_unit_5

      write(6,*) 'dee location is : ',static_file_location
 
! read in tiros/foster data
 
      OPEN(21,FILE=TRIM(static_file_location)//'ionprof',STATUS='old')
      READ(21,99001) emaps
      READ(21,99001) cmaps
      CLOSE(21)

      OPEN(23,FILE=TRIM(static_file_location)//'prof2',STATUS='old')
      READ(23,99001) profile
      CLOSE(23)

      OPEN(25,FILE=TRIM(static_file_location)//'holt',STATUS='old')
      READ(25,99001) Foster_potential
      CLOSE(25)
 
99001 FORMAT (1x,6E13.6)
 
      OPEN(33,FILE=TRIM(static_file_location)//&
                        &'hough',STATUS='old')
      OPEN(36,FILE=TRIM(static_file_location)//&
                        &'hough11',STATUS='old')
      OPEN(40,FILE=TRIM(static_file_location)//&
                        &'hough25',STATUS='old')
 
      call calculate_magnetic_parameters_using_apex( &
                 B_magnitude_apex_nT,B_dip_angle_apex_degrees,B_declination_apex_degrees, &
                 Magnetic_latitude_degrees,Magnetic_longitude_degrees)


!  Write out all the switches information....

      write(6,*) '  amplitudes(m), Local times  :  1,1    ',ampl11,lt11
      write(6,*) '                                 2,2    ',ampl22,lt22
      write(6,*) '                                 2,3    ',ampl23,lt23
      write(6,*) '                                 2,4    ',ampl24,lt24
      write(6,*) '                                 2,5    ',ampl25,lt25
      write(6,*) ' '
      write(6,*) ' '
      switch='off'
      if(sw_electro) switch='on '
      write(6,*) ' Full Electrodynamics  . . . . . . . . . . .  ',switch
      write(6,*) '  '
      write(6,*) ' '
      Start_time_UT_hours = MOD(12.+(nnstrt-1)/60.,24.)
      Stop_time_UT_hours = MOD(12.+nnstop/60.,24.)
!     WRITE(6,*) 'Running for ',i_total_no_days,' days'
!     WRITE(6,*) 'with graphics output from day ', i_graphics_out_start
!     WRITE(6,*) 'Run from ',nnstrt,' until ',nnstop, &
!     ' (ie,',int(Start_time_UT_hours),':',int((Start_time_UT_hours- &
!      float(int(Start_time_UT_hours)))*60.), &
!      ' UT until',int(Stop_time_UT_hours),':', &
!     nint((Stop_time_UT_hours-float(int(Stop_time_UT_hours)))*60.), &
!          ' UT)'
      write(6,*)' '
!     write(6,*) ' High Res Graphics every ',ipint_high,' minutes'
      write(6,*)' '

      write(6,224) nday
 224  format('       day number . . . . . . . . . . ',i3)
      write(6,225) f107
 225  format('       F10.7 . . . . . . . . . . . . . ',f4.0)
      write(6,*) ' '
 
 


      IF ( .NOT.sw_electro ) THEN
         OPEN (41,FILE=TRIM(static_file_location)//&
                            &'jicamarca_zonal_drifts' ,STATUS='OLD')
         call low_lat_efield (exns,eyns,ezns,plvu,zonal,f107,nday)
      ENDIF

           Universal_Time_seconds = Start_time_UT_hours * 3600.

           call GT_thermosphere_INIT( &
                         GT_input_dataset,  &
                         GT_output_dataset,  &
                         nday, &
                         Universal_Time_seconds, &
                         solar_declination_angle_radians, &
                         hough11 , hough22 , hough23 , hough24 , hough25, &
                         ampl11,ampl22 , ampl23 , ampl24 , ampl25, &
                         lt11 , lt22 , lt23 , lt24 , lt25, &
                         wind_southwards_ms1_FROM_GT, &
                         wind_eastwards_ms1_FROM_GT, &
                         wvz_FROM_GT, &
                         rmt_FROM_GT, &
                         Temperature_K_FROM_GT, &
                         ht_FROM_GT) 





          Plot_data_file = trim(GIP_output_dataset) // '.plot_data'
          OPEN (32,FILE=Plot_data_file,STATUS= 'unknown')

! initialise all of the GIP switches to be .FALSE.

          GIP_switches(:) = .FALSE.

! then set the the ones that are used..


          GIP_switches(5) = sw_External_model_provides_NO_N4S_densities
          GIP_switches(6) = sw_External_model_provides_low_lat_E_fields
          GIP_switches(7) = sw_input_Auroral_production_is_single_rate

      do l = 1 , 20
      therm_model_geo_long_deg(l) = (l - 1) * 18.
      enddo
      do m = 1 , 91
      therm_model_geo_lat_deg(m) = (m - 46) * 2.
      enddo
      do n = 1 , 15
      do l = 1 , 20
      do m = 1 , 91
      Altitude_m_FOR_GIP(n,l,m) = ht_FROM_GT(n,m,l)
      enddo
      enddo
      enddo



      CALL GIP_INIT( &
                    GIP_switches, &
                    nday, &
                    start_time_UT_hours, &
                    f107, &
                    GIP_input_dataset, &
                    GIP_output_dataset, &
                    static_file_location, &
                    GIP_Apex_coords_static_file, &
                    thermospheric_model_name, &
                    GT_ht_dim, &
                    GT_lat_dim, &
                    GT_lon_dim, &
                    therm_model_geo_long_deg, &
                    therm_model_geo_lat_deg, &
                    Altitude_m_FOR_GIP, &
                    Ne_density_FROM_GIP_m3, &
                    Oplus_density_FROM_GIP_m3, &
                    Hplus_density_FROM_GIP_m3, &
                    NOplus_density_FROM_GIP_m3, &
                    O2plus_density_FROM_GIP_m3, &
                    N2plus_density_FROM_GIP_m3, &
                    Nplus_density_FROM_GIP_m3, &
                    Te_FROM_GIP_K, & 
                    Ti_Oplus_FROM_GIP_K, &
                    Ti_Hplus_FROM_GIP_K, &
                    dynamo_sigma_phph_dsi, &
                    dynamo_sigma_lmlm_msi, &
                    dynamo_sigma_h, &
                    dynamo_sigma_c, &
                    dynamo_Kdmph_dsi, &
                    dynamo_Kdmlm)



      do n = 1 , 15
      do m = 1 , 91
      do l = 1 , 20
        Ne_density_FOR_GT(n,m,l)     = Ne_density_FROM_GIP_m3(n,l,m)
        Oplus_density_FOR_GT(n,m,l)  = oplus_density_FROM_GIP_m3(n,l,m)
        NOplus_density_FOR_GT(n,m,l) = noplus_density_FROM_GIP_m3(n,l,m)
        O2plus_density_FOR_GT(n,m,l) = o2plus_density_FROM_GIP_m3(n,l,m)
        Te_FOR_GT(n,m,l)             = Te_FROM_GIP_K(n,l,m)
        Ti_Oplus_FOR_GT(n,m,l)       = Ti_Oplus_FROM_GIP_K(n,l,m)
        Ti_Hplus_FOR_GT(n,m,l)       = Ti_Hplus_FROM_GIP_K(n,l,m)
      enddo
      enddo
      enddo



      write(32,7878) 'CTIP',5,91,90,'gip1','GIP parameters'
 7878 format(A4,'  ',3I4,'   ',A20,'  ',A20)

! Zero some counters....      

      nn_smoothing_counter = 0
      nn_composition_counter = 0
      i_gip = 0


      write(6,*) ' '
      write(6,*) '      **********************************' 
      write(6,*) '      *                                *' 
      write(6,*) '      *           OK here goes ......  *'
      write(6,*) '      *                                *' 
      write(6,*) '      **********************************' 
      write(6,*) ' '
      write(6,*) 'Running from ',nnstrt,'  to ',nnstop
      write(6,*) ' '

      number_of_GT_time_steps_in_24_hours = 1440 * 60 / GT_timestep_in_seconds

!  Put a number of days loop around the time loop....
 
      do 3000 i_no_day = 1 , i_total_no_days
 
      iout_high = 0
 
       write(6,*) 'DAY NUMBER',i_no_day,' OF A TOTAL',i_total_no_days
 
!**********************
!      time loop      *
!**********************
 
      call system_clock(count_rate=clock_rate)

      DO 2000 nnloop = nnstrt , nnstop

      WRITE(6,*) 'nnloop=  ' , nnloop
      call system_clock(t0_loop)

! make sure nn is in range (1 to number_of_GT_time_steps_in_24_hours)

         nn = MOD(nnloop,number_of_GT_time_steps_in_24_hours)
         IF ( nn.EQ.0 ) nn = number_of_GT_time_steps_in_24_hours

! increment counters

         nn_smoothing_counter = nn_smoothing_counter + 1
         nn_composition_counter = nn_composition_counter + 1
         iout_high = iout_high + 1
         i_gip = i_gip + 1




       Universal_Time_hours = MOD(12. + real(nn) * ( real(GT_timestep_in_seconds) / 60.) / 60.,24.)
       Universal_Time_seconds = Universal_Time_hours*3600.

!      write(6,*) 'Universal_Time = ' , Universal_Time_hours, Universal_Time_seconds

       Foster_level = 5
!      Foster_power = 125.  ! Note: this is only used if Foster_level (above) is 10.
!      Foster_power = 160.  ! Note: this is only used if Foster_level (above) is 10.
       Foster_power = 130.  ! Note: this is only used if Foster_level (above) is 10.
       Foster_Efield_amplification = 1.3

! Added for the storm run test on 6 Feb 2008 by George Millward
! Define our storm in terms of Foster _level.....
! Added by MF   7th Feb 2008

       if (i_no_day.eq.1) then

!         if (nnloop.gt.12) Foster_level = 8
!         if (nnloop.gt.24) Foster_level = 10
!         if (nnloop.gt.732) Foster_level = 8
!         if (nnloop.gt.744) Foster_level = 5

! above are for a 1 minute time step.  For 30 seconds e need this:

!         if (nnloop.gt.72) Foster_level = 8
!         if (nnloop.gt.144) Foster_level = 10
!         if (nnloop.gt.264) Foster_level = 8
!         if (nnloop.gt.336) Foster_level = 5
!         if (nnloop.gt.4392) Foster_level = 8
!         if (nnloop.gt.4464) Foster_level = 10
!         if (nnloop.gt.4584) Foster_level = 8
!         if (nnloop.gt.4656) Foster_level = 5
!         if (nnloop.gt.4392) Foster_level = 8
!         if (nnloop.gt.4464) Foster_level = 5

       else

          Foster_level = 5

       endif
! End of storm run test piece of code 7th Feb 2008 MF

!      write(6,*) 'Foster level ',Foster_level

       CALL FOSTER(exns,eyns,Foster_level,Foster_power,Foster_Efield_amplification,Foster_potential)

       call high_lat_elecz(exns,ezns)

!      CALL TIROS

!      CALL SOLAR_EUV

      idump_gt = 0
      if (i_no_day .eq. i_total_no_days .and. nnloop .eq. nnstop) idump_gt = 1


      call system_clock(t0_gt)
      call GT_thermosphere( &
                      GT_input_dataset, &
                      GT_output_dataset, &
                      i_no_day, &
                      i_graphics_out_start, &
                      idump_gt, &
                      iout_high, &
                      ipint_high, &
                      dcol,dlon, &
                      solar_declination_angle_radians, &
                      nn,nnloop, &
                      Universal_Time_seconds, &
                      nn_smoothing_counter, &
                      i_smoothing_frequency, &
                      nn_composition_counter, &
                      i_neutral_composition_calling_frequency, &
                      hough11 , hough22 , hough23 , hough24 , hough25, &
                      ampl11,ampl22 , ampl23 , ampl24 , ampl25, &
                      lt11 , lt22 , lt23 , lt24 , lt25, &
                      Ne_density_FOR_GT, &
                      Oplus_density_FOR_GT, &
                      Hplus_density_FOR_GT, &
                      NOplus_density_FOR_GT, &
                      O2plus_density_FOR_GT, &
                      Te_FOR_GT, &
                      Ti_Oplus_FOR_GT, &
                      Ti_Hplus_FOR_GT, &
                      exns,eyns,ezns, &
                      B_dip_angle_apex_degrees,B_magnitude_apex_nT, &
                      Magnetic_latitude_degrees,Magnetic_longitude_degrees, &
                      Foster_level, &
                      Foster_power, &
                      f107, &
                      emaps,cmaps,profile, &
                      wind_southwards_ms1_FROM_GT, &
                      wind_eastwards_ms1_FROM_GT, &
                      wvz_FROM_GT, &
                      rmt_FROM_GT, &
                      Temperature_K_FROM_GT, &
                      ht_FROM_GT, &
                      O_density_FROM_GT, &
                      O2_density_FROM_GT, &
                      N2_density_FROM_GT, &
                      qion3d)

      call system_clock(t1)
      write(6,'(A,I6,A,F8.3,A)') 'TIMING GT   nnloop=',nnloop, &
        '  wall=',real(t1-t0_gt)/real(clock_rate),' s'

!  NaN check after GT
      if (any(isnan(Temperature_K_FROM_GT))) &
        write(6,*) 'NaN WARNING: Temperature_K_FROM_GT  nnloop=',nnloop
      if (any(isnan(wind_southwards_ms1_FROM_GT))) &
        write(6,*) 'NaN WARNING: wind_southwards_ms1_FROM_GT  nnloop=',nnloop
      if (any(isnan(wind_eastwards_ms1_FROM_GT))) &
        write(6,*) 'NaN WARNING: wind_eastwards_ms1_FROM_GT  nnloop=',nnloop
      if (any(isnan(O_density_FROM_GT))) &
        write(6,*) 'NaN WARNING: O_density_FROM_GT  nnloop=',nnloop

!g  call GIP......

      if (i_gip .eq. GIP_calling_frequency) then

      do i = 1 , 81
      do j = 1 , 97
      potential_field(i,j) = 0.0
      ed1(i,j) = 0.0
      ed2(i,j) = 0.0
      enddo
      enddo


      do l = 1 , 20
      therm_model_geo_long_deg(l) = (l - 1) * 18.
      enddo
      do m = 1 , 91
      therm_model_geo_lat_deg(m) = (m - 46) * 2.
      enddo
 
      do n = 1 , 15
      do l = 1 , 20
      do m = 1 , 91

      therm_model_HYD_density_m3(n,l,m) = 0.0
      therm_model_HEL_density_m3(n,l,m) = 0.0
      therm_model_NO_density_m3(n,l,m) = 0.0
      therm_model_N4S_density_m3(n,l,m) = 0.0
      therm_model_N2D_density_m3(n,l,m) = 0.0
      therm_model_qo2p_aurora(n,l,m) = 0.0
      therm_model_qop_aurora(n,l,m) = 0.0
      therm_model_qn2p_aurora(n,l,m) = 0.0
      therm_model_qnp_aurora(n,l,m) = 0.0
      therm_model_qtef_aurora(n,l,m) = 0.0

      enddo
      enddo
      enddo

      do n = 1 , 15
      do l = 1 , 20
      do m = 1 , 91

      Altitude_m_FOR_GIP(n,l,m) = ht_FROM_GT(n,m,l)
      Vn_Southwards_ms1_FOR_GIP(n,l,m) = wind_southwards_ms1_FROM_GT(n,m,l)
      Vn_Eastwards_ms1_FOR_GIP(n,l,m) = wind_eastwards_ms1_FROM_GT(n,m,l)
      Vn_Upwards_ms1_FOR_GIP(n,l,m) = wvz_FROM_GT(n,m,l)
      Tn_K_FOR_GIP(n,l,m) = temperature_K_FROM_GT(n,m,l)
      O_density_m3_FOR_GIP(n,l,m) = O_density_FROM_GT(n,m,l)
      O2_density_m3_FOR_GIP(n,l,m) = O2_density_FROM_GT(n,m,l)
      N2_density_m3_FOR_GIP(n,l,m) = N2_density_FROM_GT(n,m,l)
      elx_FOR_GIP(n,l,m) = elx(m,l)
      ely_FOR_GIP(n,l,m) = ely(m,l)
      qion3d_FOR_GIP(n,l,m) = qion3d(n,m,l)

      enddo
      enddo
      enddo

          idump_gip = 0
          if (i_no_day .eq. i_total_no_days .and. nnloop .eq. nnstop) idump_gip = 1

          call system_clock(t0_gip)
          CALL GIP_CALCULATION (  &
                               GIP_switches, &
                               GIP_input_dataset, &
                               GIP_output_dataset, &
                               thermospheric_model_name, &
                               GT_ht_dim, &
                               GT_lat_dim, &
                               GT_lon_dim, &
                               therm_model_geo_long_deg, &
                               therm_model_geo_lat_deg, &
                               Altitude_m_FOR_GIP, &
                               O_density_m3_FOR_GIP, &
                               O2_density_m3_FOR_GIP, &
                               N2_density_m3_FOR_GIP, &
                               therm_model_NO_density_m3, &
                               therm_model_N4S_density_m3, &
                               therm_model_N2D_density_m3, &
                               Tn_K_FOR_GIP, &
                               Vn_Southwards_ms1_FOR_GIP, &
                               Vn_Eastwards_ms1_FOR_GIP, &
                               Vn_Upwards_ms1_FOR_GIP, &
                               qion3d_FOR_GIP, &
                               elx_FOR_GIP, &
                               ely_FOR_GIP, &
                               therm_model_qo2p_aurora, &
                               therm_model_qop_aurora, &
                               therm_model_qn2p_aurora, &
                               therm_model_qnp_aurora, &
                               therm_model_qtef_aurora, &
                               potential_field, &
                               ed1, &
                               ed2, &
                               nday, &
                               Universal_Time_hours, &
                               f107, &
                               idump_GIP, & 
                               Ne_density_FROM_GIP_m3, &
                               Oplus_density_FROM_GIP_m3, &
                               Hplus_density_FROM_GIP_m3, &
                               NOplus_density_FROM_GIP_m3, &
                               O2plus_density_FROM_GIP_m3, &
                               N2plus_density_FROM_GIP_m3, &
                               Nplus_density_FROM_GIP_m3, &
                               Te_FROM_GIP_K, &
                               Ti_Oplus_FROM_GIP_K, &
                               Ti_Hplus_FROM_GIP_K, &
                               dynamo_sigma_phph_dsi, &
                               dynamo_sigma_lmlm_msi, &
                               dynamo_sigma_h, &
                               dynamo_sigma_c, &
                               dynamo_Kdmph_dsi, &
                               dynamo_Kdmlm, &
                               ne_high_res_fixed)

      call system_clock(t1)
      write(6,'(A,I6,A,F8.3,A)') 'TIMING GIP  nnloop=',nnloop, &
        '  wall=',real(t1-t0_gip)/real(clock_rate),' s'

!  NaN check after GIP
      if (any(isnan(Ne_density_FROM_GIP_m3))) &
        write(6,*) 'NaN WARNING: Ne_density_FROM_GIP_m3  nnloop=',nnloop
      if (any(isnan(Te_FROM_GIP_K))) &
        write(6,*) 'NaN WARNING: Te_FROM_GIP_K  nnloop=',nnloop
      if (any(isnan(Ti_Oplus_FROM_GIP_K))) &
        write(6,*) 'NaN WARNING: Ti_Oplus_FROM_GIP_K  nnloop=',nnloop

      i_gip = 0

      IF ( i_no_day.ge.i_graphics_out_start ) THEN
          WRITE(32,29021) Universal_Time_hours , Universal_Time_hours
29021 FORMAT (f5.2,2x,f5.2,' UT')
          write(32,3544) ne_high_res_fixed
 3544     format(10e12.4)
 2544     format(20f8.2)

      ENDIF

      do n = 1 , 15
      do m = 1 , 91
      do l = 1 , 20
        Ne_density_FOR_GT(n,m,l)     = Ne_density_FROM_GIP_m3(n,l,m)
        Oplus_density_FOR_GT(n,m,l)  = oplus_density_FROM_GIP_m3(n,l,m)
        NOplus_density_FOR_GT(n,m,l) = noplus_density_FROM_GIP_m3(n,l,m)
        O2plus_density_FOR_GT(n,m,l) = o2plus_density_FROM_GIP_m3(n,l,m)
        Te_FOR_GT(n,m,l)             = Te_FROM_GIP_K(n,l,m)
        Ti_Oplus_FOR_GT(n,m,l)       = Ti_Oplus_FROM_GIP_K(n,l,m)
        Ti_Hplus_FOR_GT(n,m,l)       = Ti_Hplus_FROM_GIP_K(n,l,m)
      enddo
      enddo
      enddo

      endif   !if i_gip eq GIP_calling_frequency


!     CALL ELECTRODYNAMICS
  

      call system_clock(t1)
      write(6,'(A,I6,A,F8.3,A)') 'TIMING LOOP nnloop=',nnloop, &
        '  wall=',real(t1-t0_loop)/real(clock_rate),' s'

 2000 CONTINUE

!  ...and end the no_of days time loop.....

 3000 CONTINUE

      WRITE (6,*) '*************** NORMAL END ***************'

      STOP

      END
