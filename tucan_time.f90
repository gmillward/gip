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
!      use cons_module
!      use dynamo_module
!      use heelis_module
  
      IMPLICIT NONE

      integer istop
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
      INTEGER i , j , l , m , n
      INTEGER nnstop , nnstrt
      INTEGER nn , nnloop , &
              nn_composition_counter , nn_smoothing_counter
      INTEGER number_of_GT_time_steps_in_24_hours
      character*4 hours_mins_timestamp
      integer minutes_counter

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
      character*100 nmf2_data_file

      character*100 low_lat_efields_from_electrodynamics_or_empirical

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
      real*8 maxval_ne_fixed_heights
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
      integer :: UT_hours_part_integer
      integer :: UT_mins_part_integer
      character(2) :: hours_string
      character(2) :: mins_string
      character*256 :: GT_output_dataset2

      logical first_call_of_plasma

      parameter(thermospheric_model_name = 'CMAT2')
      parameter(GT_ht_dim=15)
      parameter(GT_lat_dim=91)
      parameter(GT_lon_dim=20)

      CHARACTER*100 GT_input_dataset , GT_output_dataset
      character*100 GIP_input_dataset , GIP_output_dataset
      character*100 GIP_output_dataset2
      character*100 GIP_Apex_coords_static_file
      character*100 static_file_loc
      logical sw_External_model_provides_NO_N4S_densities
      logical sw_External_model_provides_low_lat_E_fields
      logical sw_input_Auroral_production_is_single_rate
      REAL*8 ampl22 , ampl11 , ampl25
      REAL*8 ampl23 , ampl24
      REAL*8 lt22,lt23,lt24,lt11,lt25
      REAL*8 tmpmin
      REAL*8 windmx
      INTEGER i_smoothing_frequency ,  &
              i_neutral_composition_calling_frequency
      INTEGER nday
      INTEGER GT_timestep_in_seconds
      INTEGER GIP_calling_frequency
      REAL*8  f107

      integer :: start_step
      integer :: start_year
      integer :: start_month
      integer :: start_day
      integer :: start_hour
      integer :: start_minute
      integer :: start_day_number
      integer, dimension(1440) :: istep
      integer, dimension(1440) :: iyear
      integer, dimension(1440) :: imonth
      integer, dimension(1440) :: iday
      integer, dimension(1440) :: ihour
      integer, dimension(1440) :: iminute
      integer, dimension(1440) :: iday_number
      real*8, dimension(1440) ::  solar_declination_angle_degrees_arr
      integer :: ii , nsteps

!      integer :: eldyn_iyr
!      integer :: eldyn_iday
!      real*8 :: eldyn_secs1
!      real*8 :: sunlons(1)
!      character(len=15) :: potential_model
!      parameter(potential_model = 'HEELIS')
!      integer :: ilat_dySH, ilat_dyNH, k, ithis

      character*4 :: year_string
      character*2 :: month_string
      character*2 :: day_string
      character*2 :: hour_string
      character*2 :: minute_string

      character*8 :: date_string
      character*4 :: time_string
      character*13 :: datetime_string
      LOGICAL :: kill_file_exists

!  INTEGER NPTS
      INTEGER NMP
      INTEGER NLP

      low_lat_efields_from_electrodynamics_or_empirical = 'ELECTRODYNAMICS'

!     read in run parameters from unit 5
      READ(5,29001) GT_input_dataset
      READ(5,29001) GT_output_dataset
      READ(5,29001) GIP_input_dataset
      READ(5,29001) GIP_output_dataset
29001     FORMAT (A)
      read(5,*) nsteps
      read(5,*) start_step,start_year,start_month,start_day,start_hour,start_minute,start_day_number
      do ii = 1 , nsteps
        read(5,*) istep(ii),iyear(ii),imonth(ii),iday(ii),ihour(ii),iminute(ii),iday_number(ii), &
                    solar_declination_angle_degrees_arr(ii)
      enddo
      close(5)

static_file_loc = 'static_files/'

!      GIP_Apex_coords_static_file =
!      TRIM(static_file_location)//'GIP_dipole_coords'
GIP_Apex_coords_static_file = TRIM(static_file_loc)//'GIP_COORDS_FILE'

sw_External_model_provides_NO_N4S_densities = .FALSE.
sw_External_model_provides_low_lat_E_fields = .FALSE.
sw_input_Auroral_production_is_single_rate = .TRUE.

f107 = 120.0

GT_timestep_in_seconds = 60
GIP_calling_frequency = 15

i_smoothing_frequency = 5
i_neutral_composition_calling_frequency = 1
windmx = 1000.0               ! GT maximum wind before smoothing
tmpmin = 130.0                ! GT minimum temperature before smoothing

! tidal amplitudes and phases...
ampl11 = 0.0   ; lt11 = 8.0
ampl22 = 300.0 ; lt22 = 1.0
ampl23 = 0.0   ; lt23 = 7.4
ampl24 = 0.0   ; lt24 = 5.5
ampl25 = 0.0   ; lt25 = 10.0


      nday = iday_number(1)

! read in tiros/foster data
 
      OPEN(21,FILE=TRIM(static_file_loc)//'ionprof',STATUS='old')
      READ(21,99001) emaps
      READ(21,99001) cmaps
      CLOSE(21)

      OPEN(23,FILE=TRIM(static_file_loc)//'prof2',STATUS='old')
      READ(23,99001) profile
      CLOSE(23)

      OPEN(25,FILE=TRIM(static_file_loc)//'holt',STATUS='old')
      READ(25,99001) Foster_potential
      CLOSE(25)
 
99001 FORMAT (1x,6E13.6)
 
      OPEN(33,FILE=TRIM(static_file_loc)//&
                        &'hough',STATUS='old')
      OPEN(36,FILE=TRIM(static_file_loc)//&
                        &'hough11',STATUS='old')
      OPEN(40,FILE=TRIM(static_file_loc)//&
                        &'hough25',STATUS='old')
 
      call calculate_magnetic_parameters_using_apex( &
                 B_magnitude_apex_nT,B_dip_angle_apex_degrees,B_declination_apex_degrees, &
                 Magnetic_latitude_degrees,Magnetic_longitude_degrees)


      write(6,225) f107
 225  format('       F10.7 . . . . . . . . . . . . . ',f4.0)
      write(6,*) ' '
 
OPEN (41,FILE=TRIM(static_file_loc)//'jicamarca_zonal_drifts',STATUS='OLD')          
call low_lat_efield (exns,eyns,ezns,plvu,zonal,f107,nday)

      Start_time_UT_hours = dble(start_hour) + dble(start_minute)/60.
      stop_time_UT_hours = dble(ihour(nsteps)) + dble(iminute(nsteps))/60.

      Universal_Time_seconds = Start_time_UT_hours * 3600.

!      call init_cons               ! done only once
!      call init_heelis             ! done only once


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
          nmf2_data_file = trim(GIP_output_dataset) // '.nmf2'
          OPEN (32,FILE=Plot_data_file,STATUS= 'unknown')
          OPEN (132,FILE=nmf2_data_file,STATUS= 'unknown')

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



      first_call_of_plasma = .FALSE.
      CALL GIP_INIT( &
                    first_call_of_plasma, &
                    GIP_switches, &
                    nday, &
                    start_time_UT_hours, &
                    f107, &
                    GIP_input_dataset, &
                    GIP_output_dataset, &
                    static_file_loc, &
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

      istop = 0
      if(istop.eq.1) stop


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

      number_of_GT_time_steps_in_24_hours = 1440 * 60 / GT_timestep_in_seconds

 
!**********************
!      time loop      *
!**********************
 
      DO 2000 nnloop = 1 , nsteps
 
      WRITE(6,*) 'nnloop=  ' , nnloop

      INQUIRE(FILE="GT-GIP.KILL", EXIST=kill_file_exists)
      if (kill_file_exists) then
        write(6,*) ' KILL FILE '
        stop
      endif

      Universal_Time_hours = dble(ihour(nnloop)) + dble(iminute(nnloop))/60.
      Universal_Time_seconds = Universal_Time_hours*3600.

      write(6,*) 'Universal_Time = ' , Universal_Time_hours, Universal_Time_seconds

      write(year_string,"(I4.4)") iyear(nnloop)
      write(month_string,"(I2.2)") imonth(nnloop)
      write(day_string,"(I2.2)") iday(nnloop)
      write(hour_string,"(I2.2)") ihour(nnloop)
      write(minute_string,"(I2.2)") iminute(nnloop)

      date_string = year_string//month_string//day_string
      time_string = hour_string//minute_string
      datetime_string = date_string//"_"//time_string

      GT_output_dataset2 = trim(GT_output_dataset)//'_'//datetime_string
      write(6,*) 'GT_output_dataset2 ',GT_output_dataset2
      GIP_output_dataset2 = trim(GIP_output_dataset)//'_'//datetime_string

! make sure nn is in range (1 to number_of_GT_time_steps_in_24_hours)

         nn = MOD(nnloop,number_of_GT_time_steps_in_24_hours)
         IF ( nn.EQ.0 ) nn = number_of_GT_time_steps_in_24_hours

! increment counters

         nn_smoothing_counter = nn_smoothing_counter + 1
         nn_composition_counter = nn_composition_counter + 1
         i_gip = i_gip + 1


       Foster_level = 5
!      Foster_power = 125.  ! Note: this is only used if Foster_level (above) is 10.
!      Foster_power = 160.  ! Note: this is only used if Foster_level (above) is 10.
       Foster_power = 130.  ! Note: this is only used if Foster_level (above) is 10.
       Foster_Efield_amplification = 1.3

! Added for the storm run test on 6 Feb 2008 by George Millward
! Define our storm in terms of Foster _level.....
! Added by MF   7th Feb 2008

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
          Foster_level = 5
! End of storm run test piece of code 7th Feb 2008 MF

!      write(6,*) 'Foster level ',Foster_level

       CALL FOSTER(exns,eyns,Foster_level,Foster_power,Foster_Efield_amplification,Foster_potential)

       call high_lat_elecz(exns,ezns)

!      CALL TIROS

!      CALL SOLAR_EUV

      idump_gt = 1


      call GT_thermosphere( &
                      GT_input_dataset, &
                      GT_output_dataset2, &
                      idump_gt, &
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



!g  call GIP......

      if (i_gip .eq. GIP_calling_frequency) then

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

          idump_gip = 1
          write(6,*) 'NNLOOP , GIP_CALLING ', nnloop , GIP_calling_frequency
          if (nnloop .eq. GIP_calling_frequency) then
            first_call_of_plasma = .TRUE.
          else
            first_call_of_plasma = .FALSE.
          endif
          write(6,*) 'FIRST CALL ?????', first_call_of_plasma

      UT_hours_part_integer = int(universal_time_seconds/3600.)
      UT_mins_part_integer = nint((((universal_time_seconds/3600.) - real(UT_hours_part_integer)) * 60.))                      

      if ( UT_hours_part_integer < 10 ) then
        write(hours_string,fmt='(i1)') UT_hours_part_integer
        hours_string = '0' // hours_string
      else
        write(hours_string,fmt='(i2)') UT_hours_part_integer
      endif

      if ( UT_mins_part_integer < 10 ) then
        write(mins_string,fmt='(i1)') UT_mins_part_integer
        mins_string = '0' // mins_string
      else
        write(mins_string,fmt='(i2)') UT_mins_part_integer
      endif

      write(32,*) hours_string // ':' // mins_string // ' UTC'            
      write(132,*) hours_string // ':' // mins_string // ' UTC'            

      write(162,*) "TUCAN ",hours_string // ':' // mins_string // ' UTC'            

          CALL GIP_CALCULATION (  &
                               first_call_of_plasma, &
                               hours_string, &
                               mins_string, &
                               GIP_switches, &
                               GIP_input_dataset, &
                               GIP_output_dataset2, &
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
                               ne_high_res_fixed, &
                               nmf2, &
                               tec)

      i_gip = 0






!         WRITE(32,29021) Universal_Time_hours , Universal_Time_hours
!         write(32,*) 'Universal_Time = ' , Universal_Time_hours, Universal_Time_seconds
!         WRITE(132,29021) Universal_Time_hours , Universal_Time_hours
!         write(132,*) 'Universal_Time = ' , Universal_Time_hours, Universal_Time_seconds
29021 FORMAT (f5.2,2x,f5.2,' UT')

          maxval_ne_fixed_heights = maxval(ne_high_res_fixed/1.e12)
          write(6,*) 'MAXVAL ',maxval_ne_fixed_heights

          if ( maxval_ne_fixed_heights < 10. ) then
            write(32,2545) ne_high_res_fixed/1.e12
          else
            write(32,2547) ne_high_res_fixed/1.e12
          endif

          write(132,3544) nmf2
          write(132,3544) tec

 3544     format(10e12.4)
 3545     format(10e10.2)

 2547     format(20f7.2)
 2546     format(20f6.2)
 2545     format(20f5.2)

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

      istop = 0
      if(istop.eq.1.and.nnloop.eq.30) stop
  
      endif   !if i_gip eq GIP_calling_frequency

 2000 CONTINUE

      WRITE (6,*) '*************** NORMAL END ***************'

      STOP

      END

      character*4 function hours_mins_timestamp(minute_counter)
        integer :: hour, minute, minute_counter
        character*2 :: hour_str, minute_str
        hour = minute_counter / 60
        minute = minute_counter - (hour * 60)
        write (hour_str,'(i2.2)') hour
        write (minute_str,'(i2.2)') minute
        hours_mins_timestamp = hour_str//minute_str
      end function hours_mins_timestamp

      integer function minutes_counter(hours_mins_timestamp)
        character*4 :: hours_mins_timestamp
        character*2 :: hour_str, minute_str
        INTEGER :: hour, minute
        hour_str = hours_mins_timestamp(1:2)
        minute_str = hours_mins_timestamp(3:4)
        read(hour_str , *) hour
        read(minute_str , *) minute
        minutes_counter = (hour * 60) + minute
      end function minutes_counter
